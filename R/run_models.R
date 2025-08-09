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

#' Predict annual clinical cases per 1000 with enhanced models
#'
#' @param model_obj Model object from train_xgboost or train_random_forest
#' @param new_data Data frame with new observations
#' @param feature_cols Character vector of feature column names
#' @param return_intervals Logical whether to return prediction intervals (default: FALSE)
#' @param interval_level Numeric confidence level for intervals (default: 0.95)
#' @return Numeric vector of predictions (or data frame with intervals if requested)
#' @export
predict_annual_cases <- function(model_obj, new_data, feature_cols,
                                return_intervals = FALSE,
                                interval_level = 0.95) {
  
  X_new <- clean_features(new_data, feature_cols)
  is_xgb <- inherits(model_obj$model, "xgb.Booster")
  
  if (is_xgb) {
    # Check if this is a Tweedie model
    if (!is.null(model_obj$params$objective) && 
        model_obj$params$objective == "reg:tweedie") {
      # Direct prediction for Tweedie
      pred_raw <- predict(model_obj$model, xgb.DMatrix(data = X_new))
      pred <- model_obj$inverse_transform(pred_raw)
    } else {
      # Log-transformed model
      pred_log <- predict(model_obj$model, xgb.DMatrix(data = X_new))
      pred <- model_obj$inverse_transform(pred_log)
    }
    
    if (return_intervals) {
      # For XGBoost, we can use the model to get approximate intervals
      # This is a simplified approach - for production, consider quantile regression
      # or conformal prediction
      
      # Get predictions from multiple trees (approximation)
      n_trees <- model_obj$nrounds
      tree_preds <- matrix(0, nrow = nrow(X_new), ncol = min(100, n_trees))
      
      for (i in seq_len(ncol(tree_preds))) {
        iter_sample <- sample(n_trees, 1)
        pred_iter <- predict(model_obj$model, 
                           xgb.DMatrix(data = X_new),
                           iterationrange = c(1, iter_sample))
        tree_preds[, i] <- model_obj$inverse_transform(pred_iter)
      }
      
      # Calculate intervals from tree predictions
      alpha <- 1 - interval_level
      intervals <- t(apply(tree_preds, 1, function(x) {
        quantile(x, probs = c(alpha/2, 1 - alpha/2))
      }))
      
      return(data.frame(
        prediction = pred,
        lower = intervals[, 1],
        upper = intervals[, 2]
      ))
    }
    
  } else {
    # Random Forest
    pred_log <- predict(model_obj$model, data.frame(X_new))$predictions
    pred <- pmax(model_obj$inverse_transform(pred_log), 0)
    
    if (return_intervals) {
      # For RF, we can use the predict.all option if available
      # This gives predictions from all trees
      if ("ranger" %in% class(model_obj$model)) {
        # Get all tree predictions
        all_preds <- predict(model_obj$model, 
                           data.frame(X_new),
                           predict.all = TRUE)$predictions
        
        # Transform all predictions
        all_preds_transformed <- apply(all_preds, 2, model_obj$inverse_transform)
        
        # Calculate intervals
        alpha <- 1 - interval_level
        intervals <- t(apply(all_preds_transformed, 1, function(x) {
          quantile(x, probs = c(alpha/2, 1 - alpha/2))
        }))
        
        return(data.frame(
          prediction = pred,
          lower = pmax(intervals[, 1], 0),
          upper = pmax(intervals[, 2], 0)
        ))
      }
    }
  }
  
  return(pred)
}