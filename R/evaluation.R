#' Evaluate model performance with stratified metrics
#'
#' @param model_obj Model object from train_xgboost or train_random_forest
#' @param X_test Numeric matrix of test features
#' @param y_test Numeric vector of test targets
#' @param model_label Character label for the model
#' @param stratified_eval Logical whether to compute stratified metrics (default: TRUE)
#' @return List with predictions and metrics data frame
#' @export
evaluate_model <- function(model_obj, X_test, y_test, model_label = "Model",
                          stratified_eval = TRUE) {
  
  # Get predictions
  is_xgb <- "xgb.Booster" %in% class(model_obj$model)
  if (is_xgb) {
    # For Tweedie models, apply the same shift
    test_shifted <- model_obj$transform(y_test)
    dtest <- xgb.DMatrix(data = X_test)
    pred_raw <- predict(model_obj$model, dtest)
    pred <- model_obj$inverse_transform(pred_raw)
  } else {
    pred_log <- predict(model_obj$model, data.frame(X_test))$predictions
    pred <- pmax(model_obj$inverse_transform(pred_log), 0)
  }
  
  # Overall metrics
  rmse <- sqrt(mean((y_test - pred)^2))
  mae <- mean(abs(y_test - pred))
  r2 <- 1 - sum((y_test - pred)^2) / sum((y_test - mean(y_test))^2)
  mape <- mean(abs((y_test - pred)/(y_test + 0.1))) * 100
  cor_val <- cor(y_test, pred)
  
  # Base metrics data frame
  metrics_df <- data.frame(
    Model = model_label,
    RMSE = rmse,
    MAE = mae,
    R2 = r2,
    MAPE = mape,
    Correlation = cor_val,
    stringsAsFactors = FALSE
  )
  
  # Add stratified metrics if requested
  if (stratified_eval) {
    # Define dynamic quantile ranges based on test data distribution
    quantiles <- quantile(y_test, probs = c(0, 0.5, 0.75, 0.9, 0.95, 1))
    q_labels <- c("0-50%", "50-75%", "75-90%", "90-95%", "95-100%")
    
    stratified_metrics <- list()
    
    for (i in 1:(length(quantiles)-1)) {
      idx <- y_test >= quantiles[i] & y_test < quantiles[i+1]
      if (i == length(quantiles)-1) {  # Include max value in last bin
        idx <- y_test >= quantiles[i] & y_test <= quantiles[i+1]
      }
      
      if (sum(idx) > 0) {
        y_subset <- y_test[idx]
        pred_subset <- pred[idx]
        
        rmse_q <- sqrt(mean((y_subset - pred_subset)^2))
        mae_q <- mean(abs(y_subset - pred_subset))
        
        # Store with descriptive names
        metrics_df[[paste0("RMSE_", q_labels[i])]] <- rmse_q
        metrics_df[[paste0("MAE_", q_labels[i])]] <- mae_q
        metrics_df[[paste0("N_", q_labels[i])]] <- sum(idx)
        
        # Also store the range for reference
        metrics_df[[paste0("Range_", q_labels[i])]] <- 
          sprintf("[%.2f, %.2f]", quantiles[i], quantiles[i+1])
      }
    }
    
    # Add high-case specific metrics (top 5% of cases)
    high_case_threshold <- quantile(y_test, 0.95)
    high_idx <- y_test >= high_case_threshold
    
    if (sum(high_idx) > 0) {
      high_y <- y_test[high_idx]
      high_pred <- pred[high_idx]
      
      metrics_df$RMSE_High <- sqrt(mean((high_y - high_pred)^2))
      metrics_df$MAE_High <- mean(abs(high_y - high_pred))
      metrics_df$R2_High <- 1 - sum((high_y - high_pred)^2) / 
                                 sum((high_y - mean(high_y))^2)
      metrics_df$N_High <- sum(high_idx)
      
      # Directional bias for high cases (negative = underestimation)
      metrics_df$Bias_High <- mean(high_pred - high_y)
    }
    
    # Print stratified summary
    message(sprintf("\n%s Stratified Performance:", model_label))
    message(sprintf("  Overall: RMSE=%.3f, MAE=%.3f, R²=%.3f", rmse, mae, r2))
    
    for (i in 1:length(q_labels)) {
      col_rmse <- paste0("RMSE_", q_labels[i])
      col_n <- paste0("N_", q_labels[i])
      if (col_rmse %in% names(metrics_df)) {
        message(sprintf("  %s (n=%d): RMSE=%.3f", 
                       q_labels[i], metrics_df[[col_n]], metrics_df[[col_rmse]]))
      }
    }
    
    if ("Bias_High" %in% names(metrics_df)) {
      message(sprintf("  High-case bias: %.3f (negative=underestimation)", 
                     metrics_df$Bias_High))
    }
  }
  
  list(
    predictions = pred,
    metrics = metrics_df
  )
}

#' Compare model predictions across case ranges
#'
#' @param models List of model objects
#' @param new_data Data frame with new observations  
#' @param feature_cols Character vector of feature column names
#' @param true_values Optional numeric vector of true values for comparison
#' @return Data frame with predictions and comparison metrics
#' @export
compare_case_predictions <- function(models, new_data, feature_cols, 
                                    true_values = NULL) {
  
  results <- data.frame(row_id = seq_len(nrow(new_data)))
  
  # Get predictions from each model
  for (model_name in names(models)) {
    model_obj <- models[[model_name]]
    pred <- predict_annual_cases(model_obj, new_data, feature_cols)
    results[[paste0(model_name, "_pred")]] <- pred
  }
  
  # Add true values if provided
  if (!is.null(true_values)) {
    results$true_value <- true_values
    
    # Calculate errors for each model
    for (model_name in names(models)) {
      pred_col <- paste0(model_name, "_pred")
      results[[paste0(model_name, "_error")]] <- 
        results[[pred_col]] - results$true_value
      results[[paste0(model_name, "_abs_error")]] <- 
        abs(results[[paste0(model_name, "_error")]])
      results[[paste0(model_name, "_pct_error")]] <- 
        100 * results[[paste0(model_name, "_error")]] / (results$true_value + 0.1)
    }
    
    # Add case range categories
    results$case_range <- cut(results$true_value,
                              breaks = c(-Inf, 1, 2, 5, 10, Inf),
                              labels = c("0-1", "1-2", "2-5", "5-10", "10+"))
    
    # Summary by case range
    message("\nModel performance by case range:")
    for (range in levels(results$case_range)) {
      range_data <- results[results$case_range == range, ]
      if (nrow(range_data) > 0) {
        message(sprintf("\n%s cases/1000 (n=%d):", range, nrow(range_data)))
        for (model_name in names(models)) {
          mae <- mean(range_data[[paste0(model_name, "_abs_error")]])
          rmse <- sqrt(mean(range_data[[paste0(model_name, "_error")]]^2))
          bias <- mean(range_data[[paste0(model_name, "_error")]])
          message(sprintf("  %s: MAE=%.3f, RMSE=%.3f, Bias=%.3f",
                         model_name, mae, rmse, bias))
        }
      }
    }
  }
  
  return(results)
}