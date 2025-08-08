#' Predict initial EIR using trained models
#'
#' @param model_obj Model object from train_xgboost or train_random_forest
#' @param new_data Data frame with new observations
#' @param feature_cols Character vector of feature column names
#' @return Numeric vector of predictions
#' @export
predict_initial_eir <- function(model_obj, new_data, feature_cols) {
  X_new <- clean_features(new_data, feature_cols)
  is_xgb <- "xgb.Booster" %in% class(model_obj$model)
  if (is_xgb) {
    pred_log <- predict(model_obj$model,
                        xgb.DMatrix(data = X_new))
  } else {
    pred_log <- predict(model_obj$model,
                        data.frame(X_new))$predictions
  }
  pmax(model_obj$inverse_transform(pred_log), 0)
}

#' Predict annual clinical cases per 1000
#'
#' @inheritParams predict_initial_eir
#' @return numeric vector of predictions
#' @export
predict_annual_cases <- function(model_obj, new_data, feature_cols) {
  X_new   <- clean_features(new_data, feature_cols)
  is_xgb  <- inherits(model_obj$model, "xgb.Booster")
  pred_log <- if (is_xgb)
    predict(model_obj$model, xgb.DMatrix(data = X_new))
  else
    predict(model_obj$model, data.frame(X_new))$predictions

  pmax(model_obj$inverse_transform(pred_log), 0)
}