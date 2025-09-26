#' @noRd
.model_repo <- function() "CosmoNaught/estiMINT"

#' @noRd
.model_cache_dir <- function() {
  if (!requireNamespace("rappdirs", quietly = TRUE))
    stop("Please install 'rappdirs' (Suggests) for caching.")
  d <- rappdirs::user_cache_dir("estiMINT")
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

#' @noRd
.models_tag <- function() {
  p <- system.file("models-tag.txt", package = "estiMINT")
  if (p == "" && file.exists("inst/models-tag.txt")) p <- "inst/models-tag.txt"
  if (p == "") stop("models-tag.txt missing. Publish a model and ship the tag.")
  trimws(readLines(p, warn = FALSE)[1])
}

#' @noRd
.models_checksums <- function() {
  p <- system.file("models-checksums.csv", package = "estiMINT")
  if (p == "" && file.exists("inst/models-checksums.csv")) p <- "inst/models-checksums.csv"
  if (p == "") return(NULL)
  utils::read.csv(p, stringsAsFactors = FALSE)
}

# Single point of truth for where models live:
#  - ENV ESTIMINT_MODELS_DIR (no network), else
#  - user cache under <tag> (downloaded on first use)
#' @noRd
.model_root <- function(tag = .models_tag()) {
  override <- Sys.getenv("ESTIMINT_MODELS_DIR", "")
  if (nzchar(override)) return(normalizePath(override, mustWork = TRUE))
  file.path(.model_cache_dir(), "models", tag)
}

# Download <tag>.zip to cache (once) and verify checksums if present
#' @noRd
.ensure_models <- function(tag = .models_tag()) {
  root <- .model_root(tag)
  if (file.exists(file.path(root, ".ok"))) return(invisible(root))

  if (!requireNamespace("piggyback", quietly = TRUE))
    stop("Please install 'piggyback' (Suggests) to download published models.")

  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  tmp_zip <- tempfile(fileext = ".zip")

  piggyback::pb_download(
    file = paste0(tag, ".zip"),
    repo = .model_repo(),
    tag  = tag,
    dest = dirname(tmp_zip),
    overwrite = TRUE
  )
  zipfile <- file.path(dirname(tmp_zip), paste0(tag, ".zip"))
  if (!file.exists(zipfile)) stop("Models ZIP not found for tag: ", tag)

  utils::unzip(zipfile, exdir = root)

  chks <- .models_checksums()
  if (!is.null(chks)) {
    if (!requireNamespace("tools", quietly = TRUE))
      stop("Please install 'tools' (base-recommended) for checksum verification.")
    full <- file.path(root, chks$path)
    have <- tools::md5sum(full)
    want <- setNames(chks$md5, chks$path)
    # align by relative path
    have_rel <- gsub(paste0("^", gsub("\\\\","/", root), "/?"), "", gsub("\\\\","/", names(have)))
    bad <- unname(have) != unname(want[have_rel])
    if (any(bad)) {
      unlink(root, recursive = TRUE, force = TRUE)
      stop("Model checksum verification failed.")
    }
  }

  file.create(file.path(root, ".ok"))
  invisible(root)
}

# Find estiMINT_model.rds inside a directory (or accept a direct file)
#' @noRd
.resolve_model_file <- function(dir_or_file) {
  if (file.exists(dir_or_file) && !dir.exists(dir_or_file)) return(dir_or_file)
  d <- normalizePath(dir_or_file, mustWork = TRUE)
  candidates <- c(
    file.path(d, "estiMINT_model.rds"),
    file.path(d, "eir_model", "estiMINT_model.rds")
  )
  for (p in candidates) if (file.exists(p)) return(p)
  hit <- list.files(d, pattern = "\\.rds$", recursive = TRUE, full.names = TRUE)
  hit <- hit[basename(hit) == "estiMINT_model.rds"]
  if (length(hit)) return(hit[[1]])
  stop("Could not find 'estiMINT_model.rds' under: ", d)
}

#' @noRd
save_xgb_model <- function(model_dir,
                           tag = NULL,
                           pkg_root = ".",
                           repo = .model_repo(),
                           overwrite = TRUE,
                           wait_seconds = 90) {
  if (!dir.exists(model_dir)) stop("model_dir does not exist: ", model_dir)
  rds <- .resolve_model_file(model_dir)

  if (!requireNamespace("piggyback", quietly = TRUE))
    stop("Please install 'piggyback' (Suggests) to publish models.")
  if (!requireNamespace("tools", quietly = TRUE))
    stop("Please install 'tools' for md5 hashing.")

  pkg_root <- normalizePath(pkg_root, mustWork = TRUE)

  # tag: models-YYYYmmdd-HHMMSS-<md5_8>
  if (is.null(tag)) {
    md5 <- tools::md5sum(rds)
    short <- substr(unname(md5), 1, 8)
    tag <- paste0("models-", format(Sys.time(), "%Y%m%d-%H%M%S"), "-", short)
  }

  # write inst/ files shipped with the package
  dir.create(file.path(pkg_root, "inst"), showWarnings = FALSE)
  writeLines(tag, file.path(pkg_root, "inst", "models-tag.txt"))

  manifest <- data.frame(
    path   = basename(rds),
    md5    = unname(tools::md5sum(rds)),
    size_B = file.info(rds)$size,
    stringsAsFactors = FALSE
  )
  manifest_path <- file.path(pkg_root, "inst", "models-checksums.csv")
  utils::write.csv(manifest, manifest_path, row.names = FALSE)

  # zip the single RDS as <tag>.zip
  zip_path <- file.path(tempdir(), paste0(tag, ".zip"))
  oldwd <- setwd(dirname(rds)); on.exit(setwd(oldwd), add = TRUE)
  utils::zip(zipfile = zip_path, files = basename(rds))
  if (!file.exists(zip_path)) stop("Failed to create zip at: ", zip_path)

  # Create (idempotent) release, using newer API if available
  if (is.function(piggyback::pb_release_create)) {
    try(piggyback::pb_release_create(repo = repo, tag = tag), silent = TRUE)
  } else if (is.function(piggyback::pb_new_release)) {
    try(piggyback::pb_new_release(repo = repo, tag = tag), silent = TRUE)
  } else {
    stop("Your piggyback version lacks pb_release_create()/pb_new_release(). Please update piggyback.")
  }

  # Poll until the tag appears in pb_releases() (API eventual consistency)
  t0 <- Sys.time()
  repeat {
    rels <- try(piggyback::pb_releases(repo), silent = TRUE)
    if (!inherits(rels, "try-error") && NROW(rels) && (tag %in% rels$tag)) break
    if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > wait_seconds) {
      stop("Release '", tag, "' not visible yet; try again shortly.")
    }
    Sys.sleep(2)
  }

  # Upload zip and manifest (overwrite OK)
  piggyback::pb_upload(zip_path,      repo = repo, tag = tag, overwrite = overwrite)
  piggyback::pb_upload(manifest_path, repo = repo, tag = tag, overwrite = overwrite)

  message("Model published under tag '", tag, "'. ",
          "Commit & reinstall to ship updated 'inst/models-*'.")
  invisible(tag)
}
