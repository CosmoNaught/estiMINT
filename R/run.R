#' Load model onto memory for usage
#' 
#' @param path Path to .rds file or to a models directory containing estiMINT_model.rds
#' @return An 'estiMINT_model' object
#' @export
load_xgb_model <- function(path) {
  stopifnot(length(path) == 1)
  if (dir.exists(path)) {
    cand <- file.path(path, "estiMINT_model.rds")
    if (file.exists(cand)) path <- cand
  }
  if (!file.exists(path)) stop("Model file not found at: ", path)
  obj <- readRDS(path)
  if (!inherits(obj, "estiMINT_model")) warning("Loaded object does not inherit 'estiMINT_model'")
  obj
}

#' Run xgb model with initial conditons
#' 
#' @param new_data Data frame with columns: prevalence (or prev_y9), dn0_use, Q0, phi_bednets,
#'   seasonal, itn_use, irs_use
#' @param model An 'estiMINT_model' object; if NULL, tries global 'estiMINT_model'
#' @return Numeric vector of calibrated EIR predictions
#' @export
run_xgb_model <- function(new_data, model = NULL) {
  if (is.null(model)) {
    if (exists("estiMINT_model", envir = .GlobalEnv, inherits = FALSE)) {
      model <- get("estiMINT_model", envir = .GlobalEnv, inherits = FALSE)
    } else {
      stop("No model provided and 'estiMINT_model' not found in the global environment.")
    }
  }
  req <- model$features
  nd <- data.table::as.data.table(new_data)

  if ("prevalence" %in% names(nd) && !("prev_y9" %in% names(nd))) {
    nd[, prev_y9 := prevalence]
  }

  missing <- setdiff(req, names(nd))
  if (length(missing)) stop("Missing required columns: ", paste(missing, collapse = ", "))

  X <- as.matrix(nd[, ..req])

  dnew <- xgboost::xgb.DMatrix(X)
  pred_log10 <- predict(model$booster, dnew)
  pred_raw   <- 10^pred_log10
  pred_cal   <- predict_qmap_w(pred_raw, model$calibrator$qmap)
  pred_final <- pmax(0, model$calibrator$scale * pred_cal)
  as.numeric(pred_final)
}
