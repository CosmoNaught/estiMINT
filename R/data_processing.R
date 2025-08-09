#' Get timestep window for year 2-3 analysis
#'
#' @param con DBI database connection
#' @param table_name Character string naming the table
#' @param y0 Numeric start year (default: 2)
#' @param y1 Numeric end year (default: 3)
#' @return List with start, end, and data_start timesteps
#' @export
get_timestep_window <- function(con, table_name, y0 = 2, y1 = 3) {
  # To match case model's relative years within 2190-4380 window
  # Year 2 = timesteps 2920-3285, Year 3 = timesteps 3285-3650
  if (y0 == 2 && y1 == 3) {
    # Use the same relative mapping as case models
    list(start = 2920,
         end   = 3650,
         data_start = 2190)  # Start of the intervention period
  } else {
    # Original behavior for other uses
    min_ts <- dbGetQuery(con, sprintf(
      "SELECT MIN(timesteps) AS min_ts FROM %s", table_name))[["min_ts"]]
    list(start = min_ts + y0*365,
         end   = min_ts + y1*365,
         data_start = min_ts)
  }
}


#' Load simulation data with train/test split
#'
#' @param con DBI database connection
#' @param table_name Character string naming the table
#' @param ts List with timestep window information
#' @param param_limit Integer limit on parameter index (default: NULL)
#' @param sim_limit Integer limit on simulations per parameter (default: NULL)
#' @param test_fraction Numeric fraction for test set (default: 0.2)
#' @param seed Integer random seed (default: 42)
#' @return Data frame with simulation results and is_test indicator
#' @export
load_sim_data <- function(con, table_name, ts,
                          param_limit = NULL, sim_limit = NULL,
                          test_fraction = 0.2, seed = 42) {

  param_filter <- if (!is.null(param_limit))
    sprintf("WHERE parameter_index < %d", param_limit) else ""

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

  sql <- sprintf("
    WITH sel AS (%s)
    SELECT
      s.parameter_index, s.simulation_index,
      MIN_BY(t.eir, t.timesteps) AS eir_init,

      MAX(t.dn0_use)    AS dn0_use,
      MAX(t.dn0_future) AS dn0_future,
      MAX(t.Q0)         AS Q0,
      MAX(t.phi_bednets)AS phi_bednets,
      MAX(t.seasonal)   AS seasonal,
      MAX(t.routine)    AS routine,
      MAX(t.itn_use)    AS itn_use,
      MAX(t.irs_use)    AS irs_use,
      MAX(t.itn_future) AS itn_future,
      MAX(t.irs_future) AS irs_future,
      MAX(t.lsm)        AS lsm,

      AVG(CASE WHEN t.timesteps >= %d AND t.timesteps < %d
               AND t.n_age_0_1825 > 0
               THEN CAST(t.n_detect_lm_0_1825 AS DOUBLE)/t.n_age_0_1825 END)
                 AS prevalence
    FROM %s t JOIN sel s USING (parameter_index, simulation_index)
    GROUP BY s.parameter_index, s.simulation_index
    HAVING prevalence IS NOT NULL AND prevalence > 0",
    sim_sampling, ts$start, ts$end, table_name)

  dat <- dbGetQuery(con, sql)
  dat <- dat[order(dat$parameter_index, dat$simulation_index), ]

  set.seed(seed)
  unique_params <- unique(dat$parameter_index)
  n_test  <- ceiling(length(unique_params) * test_fraction)
  test_params <- sample(unique_params, n_test)
  dat$is_test <- dat$parameter_index %in% test_params
  dat
}

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
  zero_threshold  <- 0.01
    # Apply threshold to target variable
  dat$cases_per_1000_raw <- dat$cases_per_1000  # Keep original
  dat$cases_per_1000 <- ifelse(
    dat$cases_per_1000 < zero_threshold,
    0,
    dat$cases_per_1000
  )
  
  message(sprintf("Applied zero threshold (%.4f): %d values set to 0", 
                  zero_threshold, 
                  sum(dat$cases_per_1000 == 0 & dat$cases_per_1000_raw > 0)))
  
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


#' Clean and prepare feature matrix
#'
#' @param df Data frame containing features
#' @param feature_cols Character vector of feature column names
#' @return Numeric matrix of features
#' @export
clean_features <- function(df, feature_cols) {
  missing_cols <- setdiff(feature_cols, names(df))
  if (length(missing_cols)) df[missing_cols] <- 0
  df_subset <- df[, feature_cols, drop = FALSE]
  df_subset[] <- lapply(df_subset, function(col) {
    if (inherits(col, "integer64") || is.factor(col)) as.numeric(col) else col
  })
  data.matrix(df_subset)
}

#' Validate data dimensions and check for NAs
#'
#' @param X Feature matrix
#' @param y Target vector
#' @param data_name Character label for the dataset
#' @export
validate_data <- function(X, y, data_name) {
  message(sprintf(
    "%s: [%d x %d] target=%d  NA-feat=%s NA-target=%s  range=[%.2f, %.2f]",
    data_name, nrow(X), ncol(X), length(y),
    any(is.na(X)), any(is.na(y)), min(y), max(y)))
}