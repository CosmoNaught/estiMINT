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
  plan(multisession, workers = get_threads())
  
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

#' Build annual-case prediction models with enhanced stratification and weighting
#'
#' @inheritParams build_eir_models
#' @param y_keep Integer vector of simulation years to keep when training
#'               (default 0:6).  During prediction you must pass a column
#'               called **`year`** with a value 0–5 indicating the horizon
#'               for which you want cases/1000.
#' @param use_case_weights Logical whether to use dynamic case-based weights (default: TRUE)
#' @param weight_power Numeric power for weight calculation (default: 0.5)
#' @param stratified_eval Logical whether to compute stratified metrics (default: TRUE)
#' @return List: models, metrics, feature_cols, model_dir, plot_dir
#' @export
build_case_models <- function(db_path,
                              data_dir  = "case_data",
                              model_dir = "model_parameters_cases",
                              plot_dir  = "training_plots_cases",
                              plotting  = TRUE,
                              y_keep    = 0:6,
                              param_limit = NULL,
                              sim_limit   = NULL,
                              tune_hyperparams = TRUE,
                              export_data = TRUE,
                              use_case_weights = TRUE,
                              weight_power = 0.75,
                              stratified_eval = TRUE) {

  plan(multisession, workers = get_threads()) 

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

  ## ------------------------------------------------------------------------
  con <- dbConnect(duckdb::duckdb(), db_path, read_only = TRUE)
  on.exit(dbDisconnect(con))

  ts  <- get_timestep_window(con, "simulation_results")
  message("Loading case data...")
  dat <- load_case_data(con, "simulation_results", ts,
                        param_limit, sim_limit,
                        seed = 42)

  if (export_data) {
    message("Writing data to disk...")
    saveRDS(dat, paste0(data_dir, "/case_data.RDS"))
  }

  dat <- subset(dat, year %in% y_keep)
  message(sprintf("Loaded %d rows for years %s", 
                  nrow(dat), paste(range(y_keep), collapse="-")))

  # Print distribution summary for awareness
  case_summary <- summary(dat$cases_per_1000)
  message("\nCase distribution summary:")
  message(sprintf("  Min: %.2f, Q1: %.2f, Median: %.2f, Mean: %.2f, Q3: %.2f, Max: %.2f",
                  case_summary[1], case_summary[2], case_summary[3], 
                  case_summary[4], case_summary[5], case_summary[6]))
  
  # Show distribution by quantiles
  q_vals <- quantile(dat$cases_per_1000, probs = c(0.5, 0.75, 0.9, 0.95, 0.99, 1))
  message("\nPercentile thresholds:")
  for (i in seq_along(q_vals)) {
    message(sprintf("  %s%%: %.2f cases/1000", 
                   names(q_vals)[i], q_vals[i]))
  }

  feature_cols <- c(
    "year",  
    "eir",
    "dn0_use",    "dn0_future",
    "Q0",         "phi_bednets",
    "seasonal",   "routine",
    "itn_use",    "irs_use",
    "itn_future", "irs_future",
    "lsm"
  )

  set.seed(42)
  train_idx <- dat$is_test == FALSE
  test_idx  <- dat$is_test == TRUE

  train_data <- dat[train_idx, ]
  test_data  <- dat[test_idx,  ]

  set.seed(42)
  val_flag         <- sample(nrow(train_data)) < 0.2 * nrow(train_data)
  val_data         <- train_data[val_flag, ]
  train_data       <- train_data[!val_flag, ]

  message(sprintf("Data split - Train: %d, Val: %d, Test: %d", 
                  nrow(train_data), nrow(val_data), nrow(test_data)))
  message(sprintf("Cases/1000 range - Train: [%.2f, %.2f], Test: [%.2f, %.2f]", 
                  min(train_data$cases_per_1000), max(train_data$cases_per_1000),
                  min(test_data$cases_per_1000), max(test_data$cases_per_1000)))

  X_train <- clean_features(train_data, feature_cols)
  X_val   <- clean_features(val_data,   feature_cols)
  X_test  <- clean_features(test_data,  feature_cols)

  y_train <- train_data$cases_per_1000
  y_val   <- val_data$cases_per_1000
  y_test  <- test_data$cases_per_1000

  validate_data(X_train, y_train, "Train")
  validate_data(X_val,   y_val,   "Validation")
  validate_data(X_test,  y_test,  "Test")

  message("\nTraining XGBoost (cases) with enhanced weighting...")
  if (use_case_weights) {
    message("  Using dynamic case-based weights with Tweedie distribution")
  }
  
  xgb_model <- train_xgboost(X_train, y_train,
                             X_val, y_val,
                             tune_params = tune_hyperparams,
                             use_case_weights = use_case_weights,
                             weight_power = weight_power)

  message("\nTraining Random Forest (cases) with enhanced weighting...")
  if (use_case_weights) {
    message("  Using dynamic case-based weights")
  }
  
  rf_model  <- train_random_forest(X_train, y_train,
                                   X_val, y_val,
                                   tune_params = tune_hyperparams,
                                   use_case_weights = use_case_weights,
                                   weight_power = weight_power)

  message("\nEvaluating models on test set with stratified metrics...")
  xgb_eval <- evaluate_model(xgb_model, X_test, y_test, 
                            "XGBoost-Cases",
                            stratified_eval = stratified_eval)
  rf_eval  <- evaluate_model(rf_model,  X_test, y_test, 
                            "RandomForest-Cases",
                            stratified_eval = stratified_eval)

  # Combine metrics
  metrics_df <- rbind(xgb_eval$metrics, rf_eval$metrics)
  
  # Print condensed metrics comparison
  message("\n=== Overall Performance Comparison ===")
  print(metrics_df[, c("Model", "RMSE", "MAE", "R2", "Correlation")])
  
  if (stratified_eval) {
    message("\n=== High-Case Performance (top 5%) ===")
    if (all(c("RMSE_High", "MAE_High", "Bias_High") %in% names(metrics_df))) {
      print(metrics_df[, c("Model", "RMSE_High", "MAE_High", "Bias_High", "N_High")])
    }
  }
  
  csv_path <- file.path(model_dir, "case_model_metrics_enhanced.csv")
  tryCatch({
    write.csv(metrics_df, csv_path, row.names = FALSE)
    message(sprintf("\nWrote enhanced metrics to: %s", csv_path))
  }, error = function(e) {
    warning(sprintf("Could not write metrics CSV: %s", e$message))
  })

  if (plotting) {
    predictions_list <- list(
      `XGBoost (cases)`      = xgb_eval$predictions,
      `RandomForest (cases)` = rf_eval$predictions
    )
    
    message("Creating case prediction plots...")
    
    # Use the case-specific plotting function
    plot_case_predictions_combined(y_test, predictions_list, plot_dir)
    
    # Plot by year
    plot_case_predictions_by_year(
      y_true = y_test,
      predictions_list = predictions_list,
      years = test_data$year,
      output_dir = plot_dir
    )
    
    # Feature importance
    importance_list <- list(
      `XGBoost (cases)`      = xgb_model$importance,
      `RandomForest (cases)` = rf_model$importance
    )
    plot_feature_importance_combined(importance_list, plot_dir)
    
    # Additional stratified performance plot
    if (stratified_eval) {
      plot_stratified_performance(metrics_df, plot_dir)
    }
  }

  # Save models with metadata about training configuration
  model_metadata <- list(
    use_case_weights = use_case_weights,
    weight_power = weight_power,
    stratified_eval = stratified_eval,
    training_date = Sys.Date(),
    data_summary = summary(dat$cases_per_1000)
  )
  
  saveRDS(list(model = xgb_model, metadata = model_metadata),  
          file.path(model_dir, "xgb_cases_model_enhanced.rds"))
  saveRDS(list(model = rf_model, metadata = model_metadata),   
          file.path(model_dir, "rf_cases_model_enhanced.rds"))
  saveRDS(feature_cols,
          file.path(model_dir, "case_feature_columns.rds"))

  out <- list(
    models       = list(xgboost_cases = xgb_model, rf_cases = rf_model),
    metrics      = metrics_df,
    feature_cols = feature_cols,
    model_dir    = model_dir,
    plot_dir     = plot_dir,
    metadata     = model_metadata
  )

  invisible(out)
}
