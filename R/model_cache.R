#' @keywords internal
#' @noRd
.model_repo <- function() "CosmoNaught/estiMINT"

#' @keywords internal
#' @noRd
.model_cache_dir <- function() {
  d <- user_cache_dir("estiMINT")
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

#' @keywords internal
#' @noRd
.models_tag <- function() {
  # prefer installed file; fall back to source tree for devtools::load_all()
  p <- system.file("models-tag.txt", package = "estiMINT")
  if (p == "" && file.exists("inst/models-tag.txt")) p <- "inst/models-tag.txt"
  if (p == "") stop("models-tag.txt missing in the package. Ask maintainer to publish models.")
  trimws(readLines(p, warn = FALSE)[1])
}

#' @keywords internal
#' @noRd
.models_checksums <- function() {
  p <- system.file("models-checksums.csv", package = "estiMINT")
  if (p == "" && file.exists("inst/models-checksums.csv")) p <- "inst/models-checksums.csv"
  if (p == "") return(NULL)
  read.csv(p, stringsAsFactors = FALSE)
}

#' Ensure models for the current tag are present in cache; download if needed.
#' @keywords internal
#' @noRd
.ensure_models <- function() {
  tag  <- .models_tag()
  dest <- file.path(.model_cache_dir(), "models", tag)
  marker <- file.path(dest, ".ok")
  if (file.exists(marker)) return(invisible(dest))

  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  zipfile <- tempfile(fileext = ".zip")

  pb_download(
    file = paste0(tag, ".zip"),
    repo = .model_repo(),
    tag  = tag,
    dest = dirname(zipfile)
  )
  zipfile <- file.path(dirname(zipfile), paste0(tag, ".zip"))
  unzip(zipfile, exdir = dest)

  # verify checksums if a manifest shipped with the package
  chks <- .models_checksums()
  if (!is.null(chks)) {
    full <- file.path(dest, chks$path)
    have <- tools::md5sum(full)
    bad  <- chks$md5[match(names(have), chks$path)] != unname(have)
    if (any(bad, na.rm = TRUE)) {
      unlink(dest, recursive = TRUE, force = TRUE)
      stop("Model checksum verification failed. Try again or contact maintainer.")
    }
  }

  file.create(marker)
  invisible(dest)
}

#' Join a relative model path under the cache (ensuring presence).
#' @keywords internal
#' @noRd
.model_path <- function(rel) {
  .ensure_models()
  file.path(.model_cache_dir(), "models", .models_tag(), rel)
}

#' Clear cached models (current tag or all).
#' @keywords internal
#' @noRd
.purge_models_cache <- function(all = FALSE) {
  root <- file.path(.model_cache_dir(), "models")
  if (!dir.exists(root)) return(invisible(TRUE))
  if (all) unlink(root, recursive = TRUE, force = TRUE)
  else     unlink(file.path(root, .models_tag()), recursive = TRUE, force = TRUE)
  invisible(TRUE)
}
