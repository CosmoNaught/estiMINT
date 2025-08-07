#' Build EIR prediction models
#'
#' @param db_path Character path to DuckDB database
#' @param model_dir Character path to save model artifacts (default: "model_parameters")
#' @param plot_dir Character path to save plots (default: "training_plots")
#' @param plotting Logical whether to generate plots (default: TRUE)
#' @param param_limit Integer limit on parameter index (default: NULL)
#' @param sim_limit Integer limit on simulations per parameter (default: NULL)
#' @param tune_hyperparams Logical whether to tune hyperparameters (default: TRUE)
#' @param data_dir Directory to store training data
#' @param export_data Bool to export data
#' @return List containing models, metrics, and feature columns
#' @export
build_eir_models <- function(db_path,
                             data_dir  = "eir_data",
                             model_dir = "model_parameters",
                             plot_dir = "training_plots",
                             plotting = TRUE,
                             param_limit = NULL,
                             sim_limit = NULL,
                             tune_hyperparams = TRUE,
                             export_data = TRUE) {
  
  if (!dir.exists(data_dir)) {
    dir_created <- dir.create(data_dir, showWarnings = TRUE, recursive = TRUE)
    if (!dir_created) {
      full_path <- file.path(getwd(), data_dir)
      dir_created <- dir.create(full_path, showWarnings = TRUE, recursive = TRUE)
      if (!dir_created) {
        stop(sprintf("Failed to create data directory: %s", data_dir))
      }
      data_dir <- full_path
    }
    message(sprintf("Created data directory: %s", data_dir))
  }

  if (!dir.exists(model_dir)) {
    dir_created <- dir.create(model_dir, showWarnings = TRUE, recursive = TRUE)
    if (!dir_created) {
      full_path <- file.path(getwd(), model_dir)
      dir_created <- dir.create(full_path, showWarnings = TRUE, recursive = TRUE)
      if (!dir_created) {
        stop(sprintf("Failed to create model directory: %s", model_dir))
      }
      model_dir <- full_path
    }
    message(sprintf("Created model directory: %s", model_dir))
  }
  
  if (plotting && !dir.exists(plot_dir)) {
    dir_created <- dir.create(plot_dir, showWarnings = TRUE, recursive = TRUE)
    if (!dir_created) {
      full_path <- file.path(getwd(), plot_dir)
      dir_created <- dir.create(full_path, showWarnings = TRUE, recursive = TRUE)
      if (!dir_created) {
        stop(sprintf("Failed to create plot directory: %s", plot_dir))
      }
      plot_dir <- full_path
    }
    message(sprintf("Created plot directory: %s", plot_dir))
  }
  
  if (!dir.exists(model_dir)) {
    stop(sprintf("Model directory does not exist: %s", model_dir))
  }
  
  test_file <- file.path(model_dir, ".test_write")
  tryCatch({
    writeLines("test", test_file)
    unlink(test_file)
  }, error = function(e) {
    stop(sprintf("Cannot write to model directory %s: %s", model_dir, e$message))
  })
  
  con <- dbConnect(duckdb::duckdb(), db_path, read_only = TRUE)
  on.exit(dbDisconnect(con))

  table_name <- "simulation_results"
  ts <- get_timestep_window(con, table_name)

  message("Loading data...")
  dat <- load_sim_data(con, table_name, ts,
                       param_limit, sim_limit, seed = 42)

  if (export_data) {
    message("Writing data to disk...")
    saveRDS(dat, paste0(data_dir, "/eir_data.RDS"))
  }

  message(sprintf("Loaded %d rows (%d train, %d test)",
                  nrow(dat), sum(!dat$is_test), sum(dat$is_test)))

  feature_cols <- c(
    "prevalence",
    "dn0_use",
    "Q0",
    "phi_bednets",
    "seasonal",
    "routine",
    "itn_use",
    "irs_use"
  )

  train_data <- dat[!dat$is_test, ]
  test_data  <- dat[ dat$is_test, ]

  set.seed(42)
  idx       <- sample(nrow(train_data), 0.8 * nrow(train_data))
  val_data  <- train_data[-idx, ]
  train_data<- train_data[ idx, ]

  train_indices <- rownames(train_data)
  val_indices   <- rownames(val_data)
  test_indices  <- rownames(test_data)
  
  message(sprintf("Data split sizes - Train: %d, Val: %d, Test: %d", 
                  length(train_indices), length(val_indices), length(test_indices)))
  message(sprintf("Train EIR range: [%.2f, %.2f], mean: %.2f", 
                  min(train_data$eir_init), max(train_data$eir_init), mean(train_data$eir_init)))
  message(sprintf("Test EIR range: [%.2f, %.2f], mean: %.2f", 
                  min(test_data$eir_init), max(test_data$eir_init), mean(test_data$eir_init)))

  X_train <- clean_features(train_data, feature_cols)
  X_val   <- clean_features(val_data,   feature_cols)
  X_test  <- clean_features(test_data,  feature_cols)

  y_train <- train_data$eir_init
  y_val   <- val_data$eir_init
  y_test  <- test_data$eir_init

  rownames(X_train) <- train_indices
  rownames(X_val)   <- val_indices
  rownames(X_test)  <- test_indices

  validate_data(X_train, y_train, "Train")
  validate_data(X_val,   y_val,   "Validation")
  validate_data(X_test,  y_test,  "Test")

  message(sprintf("Training data check - First 5 y values: %.2f, %.2f, %.2f, %.2f, %.2f",
                  y_train[1], y_train[2], y_train[3], y_train[4], y_train[5]))
  message(sprintf("Test data check - First 5 y values: %.2f, %.2f, %.2f, %.2f, %.2f",
                  y_test[1], y_test[2], y_test[3], y_test[4], y_test[5]))

  message("Training XGBoost...")
  xgb_model <- train_xgboost(X_train, y_train,
                             X_val, y_val,
                             tune_params = tune_hyperparams)

  message("Training Random Forest...")
  rf_model  <- train_random_forest(X_train, y_train,
                                   X_val, y_val,
                                   tune_params = tune_hyperparams)

  message("Evaluating (test)...")
  xgb_eval <- evaluate_model(xgb_model, X_test, y_test, "XGBoost")
  rf_eval  <- evaluate_model(rf_model,  X_test, y_test, "RandomForest")
  
  message(sprintf("Test set size for predictions: XGB=%d, RF=%d", 
                  length(xgb_eval$predictions), length(rf_eval$predictions)))
  message(sprintf("First 5 test observations: %.2f, %.2f, %.2f, %.2f, %.2f",
                  y_test[1], y_test[2], y_test[3], y_test[4], y_test[5]))
  message(sprintf("XGB predictions for first 5: %.2f, %.2f, %.2f, %.2f, %.2f",
                  xgb_eval$predictions[1], xgb_eval$predictions[2], 
                  xgb_eval$predictions[3], xgb_eval$predictions[4], xgb_eval$predictions[5]))
  message(sprintf("RF predictions for first 5: %.2f, %.2f, %.2f, %.2f, %.2f",
                  rf_eval$predictions[1], rf_eval$predictions[2], 
                  rf_eval$predictions[3], rf_eval$predictions[4], rf_eval$predictions[5]))

  metrics_df <- rbind(xgb_eval$metrics, rf_eval$metrics)
  print(metrics_df)
  
  csv_path <- file.path(model_dir, "model_metrics.csv")
  tryCatch({
    write.csv(metrics_df, csv_path, row.names = FALSE)
    message(sprintf("Wrote metrics to: %s", csv_path))
  }, error = function(e) {
    warning(sprintf("Could not write metrics CSV: %s", e$message))
  })

  if (plotting) {
    predictions_list <- list(
      XGBoost       = xgb_eval$predictions,
      RandomForest  = rf_eval$predictions
    )
    
    message("Final plotting data verification:")
    message(sprintf("Number of test points: %d", length(y_test)))
    message(sprintf("XGBoost predictions length: %d", length(predictions_list$XGBoost)))
    message(sprintf("RandomForest predictions length: %d", length(predictions_list$RandomForest)))
    
    plot_predictions_combined(y_test, predictions_list, plot_dir)
    
    plot_data <- data.frame(
      y_true = y_test,
      xgb_pred = predictions_list$XGBoost,
      rf_pred = predictions_list$RandomForest
    )
    write.csv(plot_data, file.path(plot_dir, "plot_data_verification.csv"), row.names = FALSE)
    
    importance_list <- list(
      XGBoost      = xgb_model$importance,
      RandomForest = rf_model$importance
    )
    plot_feature_importance_combined(importance_list, plot_dir)
    
    prev_rank <- ave(test_data$prevalence,
                     test_data$parameter_index,
                     FUN = function(x) rank(x, ties.method = "first"))
    
    plot_predictions_by_prev_rank(
      y_true            = y_test,
      predictions_list  = predictions_list,
      prev_rank         = prev_rank,
      output_dir        = plot_dir
    )

    covariates_to_plot <- c(
      "dn0_use",
      "Q0",
      "phi_bednets",
      "seasonal",
      "routine",
      "itn_use",
      "irs_use")

    bin_edges <- create_default_bin_edges()

    batch_cov_bin_plots(
      y_true       = y_test,
      predictions_list = predictions_list,
      df           = test_data,
      covariates   = covariates_to_plot,
      bin_edges    = bin_edges,
      output_dir   = plot_dir
    )
  }

  saveRDS(xgb_model,  file.path(model_dir, "xgboost_model.rds"))
  saveRDS(rf_model,   file.path(model_dir, "rf_model.rds"))
  saveRDS(feature_cols,
          file.path(model_dir, "feature_columns.rds"))

  out <- list(
    models       = list(xgboost = xgb_model, rf = rf_model),
    metrics      = metrics_df,
    feature_cols = feature_cols,
    model_dir    = model_dir,
    plot_dir     = plot_dir
  )

  invisible(out)
}
