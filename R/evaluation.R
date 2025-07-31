#' Evaluate model performance
#'
#' @param model_obj Model object from train_xgboost or train_random_forest
#' @param X_test Numeric matrix of test features
#' @param y_test Numeric vector of test targets
#' @param model_label Character label for the model
#' @return List with predictions and metrics data frame
#' @export
evaluate_model <- function(model_obj, X_test, y_test, model_label = "Model") {

  is_xgb <- "xgb.Booster" %in% class(model_obj$model)

  if (is_xgb) {
    dtest    <- xgb.DMatrix(data = X_test)
    pred_log <- predict(model_obj$model, dtest)
  } else {
    pred_log <- predict(model_obj$model,
                        data.frame(X_test))$predictions
  }

  pred <- pmax(model_obj$inverse_transform(pred_log), 0)

  rmse <- sqrt(mean((y_test - pred)^2))
  mae  <- mean(abs(y_test - pred))
  r2   <- 1 - sum((y_test - pred)^2) /
               sum((y_test - mean(y_test))^2)
  mape <- mean(abs((y_test - pred)/(y_test + 0.1))) * 100
  cor_val <- cor(y_test, pred)

  list(
    predictions = pred,
    metrics = data.frame(
      Model = model_label, RMSE = rmse, MAE = mae,
      R2 = r2, MAPE = mape, Correlation = cor_val
    )
  )
}