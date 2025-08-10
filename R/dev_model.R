# -- helper: upload one asset via GitHub's upload_url (no piggyback) ----
#' @keywords internal
#' @noRd
.upload_release_asset <- function(upload_url, file_path, token, overwrite = TRUE,
                                  owner, repo, release_id) {
  stopifnot(file.exists(file_path))
  # Remove the "{?name,label}" template
  base_url <- sub("\\{.*$", "", upload_url)
  fname <- basename(file_path)

  # Optionally delete an asset with the same name first
  if (isTRUE(overwrite)) {
    assets <- gh("GET /repos/{owner}/{repo}/releases/{rid}/assets",
                     owner = owner, repo = repo, rid = release_id,
                     .token = token)
    if (length(assets)) {
      hits <- vapply(assets, `[[`, character(1), "name") == fname
      if (any(hits)) {
        asset_id <- assets[[which(hits)[1]]]$id
        gh("DELETE /repos/{owner}/{repo}/releases/assets/{asset_id}",
               owner = owner, repo = repo, asset_id = asset_id,
               .token = token)
      }
    }
  }

  # Content-Type by extension
  ctype <- if (grepl("\\.zip$", fname, ignore.case = TRUE)) "application/zip" else "text/csv"

  # Upload with retries
  resp <- RETRY(
    verb  = "POST",
    url   = base_url,
    query = list(name = fname),
    add_headers(
      Authorization = paste("token", token),
      Accept        = "application/vnd.github+json"
    ),
    body  = upload_file(file_path, type = ctype),
    times = 6,           # ~ exponential backoff
    pause_base = 1
  )
  if (status_code(resp) >= 300) {
    stop("Upload failed (", status_code(resp), "): ",
         paste0(capture.output(str(resp)), collapse = "\n"))
  }
  invisible(TRUE)
}

# -- main: publish models robustly --------------------------------------
#' Zip, upload models to a GitHub Release, and refresh the shipped manifest.
#'
#' Developer-only. Not exported.
#'
#' @param tag  Release tag to use. Default = a unique auto tag.
#' @param root Path to the package root (for devload_all(), default ".").
#' @param wait_seconds How long to wait for the release to become visible (API).
#' @keywords internal
#' @noRd
.publish_models <- function(tag = NULL, root = ".", wait_seconds = 180) {
  if (is.null(tag)) tag <- .auto_models_tag(root)
  root <- normalizePath(root, mustWork = TRUE)

  # expected local model dirs
  eir_dir  <- file.path(root, "inst", "extdata", "eir_model")
  case_dir <- file.path(root, "inst", "extdata", "case_model")
  if (!dir.exists(eir_dir) || !dir.exists(case_dir))
    stop("Expected model directories not found under inst/extdata/ (eir_model, case_model).")

  # Work from inst/extdata so zip keeps folder roots correct
  oldwd <- setwd(file.path(root, "inst", "extdata"))
  on.exit(setwd(oldwd), add = TRUE)

  # files to checksum & include
  ext_files <- c(
    list.files("eir_model",  recursive = TRUE, full.names = TRUE),
    list.files("case_model", recursive = TRUE, full.names = TRUE)
  )
  if (!length(ext_files)) stop("No model files found under inst/extdata/.")

  # zip in temp dir
  zip_path <- file.path(tempdir(), paste0(tag, ".zip"))
  zip(zipfile = zip_path, files = c("eir_model", "case_model"))
  if (!file.exists(zip_path)) stop("Zip not created at: ", zip_path)

  # checksums -> manifest
  chks <- md5sum(ext_files)
  manifest <- data.frame(
    path   = sub("^\\./", "", ext_files),
    md5    = unname(chks),
    size_B = file.info(ext_files)$size,
    stringsAsFactors = FALSE
  )

  # write tag + manifest into inst/
  tag_file <- file.path(root, "inst", "models-tag.txt")
  chk_file <- file.path(root, "inst", "models-checksums.csv")
  writeLines(tag, tag_file)
  write.csv(manifest, chk_file, row.names = FALSE)

  # --- robust release ensure + upload (GitHub API) ---
  repo <- .model_repo()
  parts <- strsplit(repo, "/", fixed = TRUE)[[1]]
  owner <- parts[[1]]; repo_name <- parts[[2]]
  token <- gh_token()

  # Ensure release exists (idempotent)
  rel <- tryCatch(
    gh("GET /repos/{owner}/{repo}/releases/tags/{tag}",
           owner = owner, repo = repo_name, tag = tag, .token = token),
    error = function(e) NULL
  )
  if (is.null(rel)) {
    rel <- gh("POST /repos/{owner}/{repo}/releases",
                  owner = owner, repo = repo_name,
                  tag_name = tag, name = tag, .token = token)
  }

  # Poll until the release has an upload_url (eventual consistency)
  t0 <- Sys.time()
  repeat {
    if (!is.null(rel$upload_url) && nzchar(rel$upload_url)) break
    if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > wait_seconds)
      stop("Release exists but API isn't returning upload_url yet; try again shortly.")
    Sys.sleep(2)
    rel <- tryCatch(
      gh("GET /repos/{owner}/{repo}/releases/tags/{tag}",
             owner = owner, repo = repo_name, tag = tag, .token = token),
      error = function(e) NULL
    )
  }

  # Upload ZIP first, then checksums
  .upload_release_asset(rel$upload_url, zip_path, token,
                        overwrite = TRUE, owner = owner, repo = repo_name, release_id = rel$id)
  .upload_release_asset(rel$upload_url, chk_file, token,
                        overwrite = TRUE, owner = owner, repo = repo_name, release_id = rel$id)

  # cleanup local temp zip
  unlink(zip_path, force = TRUE)

  message("Published models as release '", tag, "'.")
  message("Now commit & push the updated inst/models-tag.txt and inst/models-checksums.csv.")
  invisible(tag)
}

#' Create a unique models tag based on content and time.
#'
#' Builds a tag like: "models-20250809-152430-abc123def456" and guarantees
#' uniqueness by appending "-2", "-3", ... if the tag already exists.
#' Developer-only. Not exported.
#'
#' @param root   Package root (default ".")
#' @param prefix Tag prefix (default "models")
#' @param hash_len Number of hex chars from the composite hash to keep
#' @keywords internal
#' @noRd
.auto_models_tag <- function(root = ".", prefix = "models", hash_len = 12) {
  root <- normalizePath(root, mustWork = TRUE)

  ext <- file.path(root, "inst", "extdata")
  eir_dir  <- file.path(ext, "eir_model")
  case_dir <- file.path(ext, "case_model")
  if (!dir.exists(eir_dir) || !dir.exists(case_dir)) {
    stop("Expected model directories not found under inst/extdata/ (eir_model, case_model).")
  }

  files <- sort(c(
    list.files(eir_dir,  recursive = TRUE, full.names = TRUE),
    list.files(case_dir, recursive = TRUE, full.names = TRUE)
  ))
  if (!length(files)) stop("No model files found under inst/extdata.")

  # Per-file md5, then a composite hash of the (path, md5) list
  md5s <- md5sum(files)
  composite_lines <- paste(basename(files), unname(md5s), sep = ":", collapse = "\n")
  tf <- tempfile(fileext = ".txt")
  on.exit(unlink(tf, force = TRUE), add = TRUE)
  writeLines(composite_lines, tf)
  composite_md5 <- as.character(md5sum(tf))
  short <- substr(composite_md5, 1, hash_len)

  stamp <- format(Sys.time(), "%Y%m%d-%H%M%S")  # local time, second resolution
  tag <- sprintf("%s-%s-%s", prefix, stamp, short)

  # Ensure uniqueness vs existing releases (append -2, -3, ...)
  exists_tag <- function(tg) {
    rels <- tryCatch(pb_releases(.model_repo()),
                     error = function(e) data.frame())
    nrow(rels) > 0 && tg %in% rels$tag_name
  }
  if (exists_tag(tag)) {
    for (i in 2:999) {
      tg2 <- sprintf("%s-%d", tag, i)
      if (!exists_tag(tg2)) { tag <- tg2; break }
    }
  }
  tag
}
