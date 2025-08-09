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

#' Load the pretrained EIR models
#' @export
load_pretrained_eir_models <- function() {
  paths <- c(
    xgboost      = .model_path("eir_model/xgboost_model.rds"),
    rf_model     = .model_path("eir_model/rf_model.rds"),
    feature_cols = .model_path("eir_model/feature_columns.rds")
  )
  missing <- paths[!file.exists(unname(paths))]
  if (length(missing)) stop("Missing EIR model files: ", paste(basename(missing), collapse = ", "))
  xgb   <- readRDS(paths["xgboost"])
  rf    <- readRDS(paths["rf_model"])
  feats <- readRDS(paths["feature_cols"])
  list(
    xgboost      = xgb,
    rf_model     = rf,
    feature_cols = feats,
    models       = list(xgboost = xgb, rf_model = rf)
  )
}

#' Load the pretrained case models
#' @export
load_pretrained_case_models <- function() {
  # support either naming convention
  cand <- list(
    xgb1 = "case_model/xgb_cases_model.rds",
    xgb2 = "case_model/xgboost_cases_model.rds"
  )
  xgb_path <- .model_path(if (file.exists(.model_path(cand$xgb1))) cand$xgb1 else cand$xgb2)

  paths <- c(
    xgboost_cases = xgb_path,
    rf_cases      = .model_path("case_model/rf_cases_model.rds"),
    feature_cols  = .model_path("case_model/case_feature_columns.rds")
  )
  missing <- paths[!file.exists(unname(paths))]
  if (length(missing)) stop("Missing case model files: ", paste(basename(missing), collapse = ", "))
  xgb   <- readRDS(paths["xgboost_cases"])
  rf    <- readRDS(paths["rf_cases"])
  feats <- readRDS(paths["feature_cols"])
  list(
    xgboost_cases = xgb,
    rf_cases      = rf,
    feature_cols  = feats,
    models        = list(xgboost_cases = xgb, rf_cases = rf)
  )
}


#' Get user max threads for paralellisation
#'
#' @return Max number of user threads
#' @export
get_threads <- function() {
  max(1L, parallel::detectCores() - 4L)
}

#' Build metric string for Tweedie loss
#'
#' @param rho Tweedie variance power (1–2)
#' @return Character scalar like "tweedie-nloglik@1.50"
#' @export
make_metric <- function(rho) {
  sprintf("tweedie-nloglik@%.2f", rho)
}
