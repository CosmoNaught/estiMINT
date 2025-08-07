#' containing the *target* `cases_per_1000` and **only** the 12 static
#' covariates you specified, **plus** the calendar year column.  
#' Only loads data from timesteps 2190-4380 (which we map to years 2-7).
#'
#' @param con           A live `DBI` connection (e.g. `duckdb::duckdb()`).
#' @param table_name    Character. Name of the table holding simulation output.
#' @param ts            A list as returned by `get_timestep_window()` with
#'                      `data_start`, `start`, `end` (only `data_start` is used).
#' @param param_limit   Optional integer.  Keep rows with
#'                      `parameter_index < param_limit`.
#' @param sim_limit     Optional integer.  Randomly sample up to `sim_limit`
#'                      simulations **per** `parameter_index`.
#' @param test_fraction Fraction of *parameter indices* to allocate to the
#'                      test set (default `0.2`).
#' @param seed          Integer random-seed for reproducible splits (default `42`).
#' @export
load_case_data <- function(con, table_name, ts,
                           param_limit     = NULL,
                           sim_limit       = NULL,
                           test_fraction   = 0.2,
                           seed            = 42) {

  param_filter <- if (!is.null(param_limit))
    sprintf("WHERE parameter_index < %d", param_limit) else ""

  ## Down-sample simulations per parameter
  sim_sampling <- if (!is.null(sim_limit)) sprintf(
    "SELECT parameter_index, simulation_index
       FROM ( SELECT DISTINCT parameter_index, simulation_index,
                     ROW_NUMBER() OVER (PARTITION BY parameter_index
                                        ORDER BY RANDOM()) AS rn
              FROM %s %s)
       WHERE rn <= %d",
    table_name, param_filter, sim_limit) else
    sprintf("SELECT DISTINCT parameter_index, simulation_index
             FROM %s %s", table_name, param_filter)

  # Map timesteps to year indices 0-5:
  sql <- sprintf("
    WITH sel AS (%s)
    SELECT
      s.parameter_index,
      s.simulation_index,

      /* Map timesteps to year indices 0-5 */
      CASE 
        WHEN t.timesteps >= 2190 AND t.timesteps < 2555 THEN 0
        WHEN t.timesteps >= 2555 AND t.timesteps < 2920 THEN 1
        WHEN t.timesteps >= 2920 AND t.timesteps < 3285 THEN 2
        WHEN t.timesteps >= 3285 AND t.timesteps < 3650 THEN 3
        WHEN t.timesteps >= 3650 AND t.timesteps < 4015 THEN 4
        WHEN t.timesteps >= 4015 AND t.timesteps <= 4380 THEN 5
        ELSE NULL
      END AS year,

      /* ---------- TARGET: annual clinical cases per 1000 ---------- */
      1000.0 * SUM(t.n_inc_clinical_0_36500)
              / NULLIF(SUM(t.n_age_0_36500), 0)  AS cases_per_1000,

      /* ---------- STATIC COVARIATES (mean or max as appropriate) ---------- */
      MAX(t.eir)            AS eir,
      MAX(t.dn0_use)        AS dn0_use,
      MAX(t.dn0_future)     AS dn0_future,
      MAX(t.Q0)             AS Q0,
      MAX(t.phi_bednets)    AS phi_bednets,
      MAX(t.seasonal)       AS seasonal,
      MAX(t.routine)        AS routine,
      MAX(t.itn_use)        AS itn_use,
      MAX(t.irs_use)        AS irs_use,
      MAX(t.itn_future)     AS itn_future,
      MAX(t.irs_future)     AS irs_future,
      MAX(t.lsm)            AS lsm

    FROM %s t
    JOIN sel s USING (parameter_index, simulation_index)
    WHERE t.timesteps >= 2190 AND t.timesteps <= 4380  -- Use ALL data from years 6-12
    GROUP BY 1, 2, 3
    HAVING cases_per_1000 IS NOT NULL AND year IS NOT NULL",
    sim_sampling, table_name)

  dat <- DBI::dbGetQuery(con, sql)
  dat <- dat[order(dat$parameter_index,
                   dat$simulation_index,
                   dat$year), ]

  message(sprintf("Loaded %d rows for timesteps 2190-4380", nrow(dat)))
  message(sprintf("Year mapping: 0=timesteps 2190-2555, 1=2555-2920, 2=2920-3285, 3=3285-3650, 4=3650-4015, 5=4015-4380"))
  message(sprintf("Unique years in data: %s", paste(sort(unique(dat$year)), collapse=", ")))

  set.seed(seed)
  unique_params <- unique(dat$parameter_index)
  n_test        <- ceiling(length(unique_params) * test_fraction)
  test_params   <- sample(unique_params, n_test)

  dat$is_test   <- dat$parameter_index %in% test_params
  dat
}


#' Build annual-case prediction models (cases/1000)
#'
#' @inheritParams build_eir_models
#' @param y_keep Integer vector of simulation years to keep when training
#'               (default 0:6).  During prediction you must pass a column
#'               called **`year`** with a value 0–5 indicating the horizon
#'               for which you want cases/1000.
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

  message("Training XGBoost (cases)...")
  xgb_model <- train_xgboost(X_train, y_train,
                             X_val, y_val,
                             tune_params = tune_hyperparams)

  message("Training Random Forest (cases)...")
  rf_model  <- train_random_forest(X_train, y_train,
                                   X_val, y_val,
                                   tune_params = tune_hyperparams)

  message("Evaluating models on test set...")
  xgb_eval <- evaluate_model(xgb_model, X_test, y_test, "XGBoost-Cases")
  rf_eval  <- evaluate_model(rf_model,  X_test, y_test, "RandomForest-Cases")

  metrics_df <- rbind(xgb_eval$metrics, rf_eval$metrics)
  print(metrics_df)
  
  csv_path <- file.path(model_dir, "case_model_metrics.csv")
  tryCatch({
    write.csv(metrics_df, csv_path, row.names = FALSE)
    message(sprintf("Wrote metrics to: %s", csv_path))
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

  }

  # Save models with consistent naming
  saveRDS(xgb_model,  file.path(model_dir, "xgb_cases_model.rds"))
  saveRDS(rf_model,   file.path(model_dir, "rf_cases_model.rds"))
  saveRDS(feature_cols,
          file.path(model_dir, "case_feature_columns.rds"))

  out <- list(
    models       = list(xgboost_cases = xgb_model, rf_cases = rf_model),
    metrics      = metrics_df,
    feature_cols = feature_cols,
    model_dir    = model_dir,
    plot_dir     = plot_dir
  )

  invisible(out)
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