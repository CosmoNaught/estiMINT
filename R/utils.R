#' Create default bin edges for covariates
#'
#' @return List of bin edges for each covariate
#' @export
create_default_bin_edges <- function() {
  dn0_min_non_zero <- 0.1
  dn0_max          <- 1
  dn0_range        <- sort(runif(10, dn0_min_non_zero, dn0_max))
  
  list(
    dn0_use        = dn0_range,
    dn0_future     = dn0_range,
    Q0             = seq(0.6, 1, length.out = 6),
    phi_bednets    = seq(0.4, 0.95, length.out = 6),
    seasonal       = c(0, 0.5, 1),
    routine        = c(-0.1, 0.5, 1.1),
    itn_use        = seq(0, 1, by = 0.2),
    irs_use        = seq(0, 1, by = 0.2),
    itn_future     = seq(0, 1, length.out = 6),
    irs_future     = seq(0, 1, length.out = 6),
    lsm            = seq(0, 1, length.out = 6),
    year           = 0:6,
    eir            = c(0.1, 1, 5, 10, 50, 100, 500, 1000)
  )
}

#' Bin a continuous variable
#'
#' @param vec Numeric vector to bin
#' @param edges Numeric vector of bin edges
#' @param prefix Character prefix for bin labels (default: "B")
#' @return Factor of binned values
#' @export
bin_variable <- function(vec, edges, prefix = "B") {
  if (length(edges) < 3) {
    factor(vec)
  } else {
    cut(vec,
        breaks = edges,
        include.lowest = TRUE,
        labels = paste0(prefix, seq_len(length(edges) - 1)))
  }
}

#' Load the pretrained EIR models that ship with *estiMINT*
#' @export
load_pretrained_eir_models <- function() {

  model_dir <- system.file("extdata/eir_model", package = "estiMINT")
  if (model_dir == "")
    stop("Pre-trained model files were not found in this installation of estiMINT.")

  paths <- c(
    xgboost      = file.path(model_dir, "xgboost_model.rds"),
    rf_model     = file.path(model_dir, "rf_model.rds"),
    feature_cols = file.path(model_dir, "feature_columns.rds")
  )

  missing <- paths[!base::file.exists(unname(paths))]
  if (length(missing))
    stop(
      "The following pretrained model files are missing: ",
      paste(basename(missing), collapse = ", ")
    )

  xgb   <- readRDS(paths["xgboost"])
  rf    <- readRDS(paths["rf_model"])
  feats <- readRDS(paths["feature_cols"])

  list(
    # direct handles
    xgboost      = xgb,
    rf_model     = rf,
    feature_cols = feats,
    # backwards-compatible structure used by build_eir_models()
    models       = list(
      xgboost  = xgb,
      rf_model = rf
    )
  )
}

#' Load the pretrained case prediction models that ship with *estiMINT*
#' @export
load_pretrained_case_models <- function() {

  model_dir <- system.file("extdata/case_model", package = "estiMINT")
  if (model_dir == "")
    stop("Pre-trained model files were not found in this installation of estiMINT.")

  paths <- c(
    xgboost_cases = file.path(model_dir, "xgb_cases_model.rds"),
    rf_cases      = file.path(model_dir, "rf_cases_model.rds"),
    feature_cols  = file.path(model_dir, "case_feature_columns.rds")
  )

  missing <- paths[!base::file.exists(unname(paths))]
  if (length(missing)) {
    # Try alternative naming convention
    alt_paths <- c(
      xgboost_cases = file.path(model_dir, "xgboost_cases_model.rds"),
      rf_cases      = file.path(model_dir, "rf_cases_model.rds"),
      feature_cols  = file.path(model_dir, "case_feature_columns.rds")
    )
    
    alt_missing <- alt_paths[!file.exists(alt_paths)]
    if (length(alt_missing)) {
      stop(
        "The following pretrained case model files are missing: ",
        paste(basename(missing), collapse = ", ")
      )
    } else {
      paths <- alt_paths
    }
  }

  xgb   <- readRDS(paths["xgboost_cases"])
  rf    <- readRDS(paths["rf_cases"])
  feats <- readRDS(paths["feature_cols"])

  list(
    # direct handles
    xgboost_cases = xgb,
    rf_cases      = rf,
    feature_cols  = feats,
    # backwards-compatible structure used by build_case_models()
    models        = list(
      xgboost_cases = xgb,
      rf_cases      = rf
    )
  )
}

