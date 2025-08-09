#' Zip, upload models to a GitHub Release, and refresh the shipped manifest.
#'
#' Developer-only. Not exported.
#'
#' @param tag  Release tag to use. Default = "models-YYYY-MM-DD".
#' @param root Path to the package root (for devtools::load_all(), default ".").
#' @keywords internal
#' @noRd
.publish_models <- function(tag = NULL, root = ".") {
  if (is.null(tag)) tag <- paste0("models-", Sys.Date())
  root <- normalizePath(root, mustWork = TRUE)  # absolute path to repo root

  # expected local model dirs
  eir_dir  <- file.path(root, "inst", "extdata", "eir_model")
  case_dir <- file.path(root, "inst", "extdata", "case_model")
  if (!dir.exists(eir_dir) || !dir.exists(case_dir)) {
    stop("Expected model directories not found under inst/extdata/ (eir_model, case_model).")
  }

  # Work from inst/extdata so paths inside the zip are "eir_model/..." and "case_model/..."
  oldwd <- setwd(file.path(root, "inst", "extdata"))
  on.exit(setwd(oldwd), add = TRUE)

  # files to checksum & include
  ext_files <- c(
    list.files("eir_model",  recursive = TRUE, full.names = TRUE),
    list.files("case_model", recursive = TRUE, full.names = TRUE)
  )
  if (!length(ext_files)) stop("No model files found under inst/extdata/.")

  # create the zip in a temp dir (no leftovers in the repo)
  zip_path <- file.path(tempdir(), paste0(tag, ".zip"))
  utils::zip(zipfile = zip_path, files = c("eir_model", "case_model"))
  if (!file.exists(zip_path)) stop("Zip not created at: ", zip_path)

  # checksums (md5) -> manifest that ships with the package
  chks <- tools::md5sum(ext_files)
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
  utils::write.csv(manifest, chk_file, row.names = FALSE)

  # ensure the GitHub release exists (idempotent), then wait until visible
  repo <- .model_repo()
  try(piggyback::pb_release_create(repo, tag), silent = TRUE)

  start <- Sys.time(); ok <- FALSE
  repeat {
    rels <- tryCatch(piggyback::pb_releases(repo), error = function(e) data.frame())
    if (nrow(rels) && tag %in% rels$tag_name) { ok <- TRUE; break }
    if (as.numeric(difftime(Sys.time(), start, units = "secs")) > 120) break
    Sys.sleep(2)
  }
  if (!ok) stop("Release exists but not yet visible via API; try again shortly.")

  # upload ZIP first (payload), then checksums (small file)
  piggyback::pb_upload(zip_path, repo = repo, tag = tag)
  piggyback::pb_upload(chk_file, repo = repo, tag = tag, overwrite = TRUE)

  # cleanup
  unlink(zip_path, force = TRUE)

  message("Published models as release '", tag, "'.")
  message("Now commit & push the updated inst/models-tag.txt and inst/models-checksums.csv.")
  invisible(tag)
}
