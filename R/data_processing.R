#' @noRd
load_and_filter <- function(in_parquet, thr_lo = 0.02, thr_hi = 0.95) {
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, "PRAGMA threads=8; PRAGMA memory_limit='16GB';")

  qry <- sprintf("
    WITH base AS (SELECT * FROM read_parquet('%s')),
    avg_prev AS (
      SELECT parameter_index,
             AVG(CASE WHEN year BETWEEN 1 AND 8 THEN prevalence_annual_mean END) AS prev_avg_1_8
      FROM base GROUP BY parameter_index
    ),
    y9 AS (
      SELECT b.parameter_index,
             b.dn0_use, b.Q0, b.phi_bednets, b.seasonal, b.itn_use, b.irs_use,
             b.prevalence_annual_mean AS prev_y9,
             b.eir
      FROM base b WHERE b.year = 9
    )
    SELECT y9.*, avg_prev.prev_avg_1_8
    FROM y9 JOIN avg_prev USING (parameter_index);
  ", in_parquet)

  df_all <- DBI::dbGetQuery(con, qry)
  DT0 <- data.table::as.data.table(df_all)
  DT0 <- DT0[complete.cases(DT0), ]

  DT_excluded <- DT0[
    (prev_avg_1_8 < thr_lo) | (prev_avg_1_8 > thr_hi) |
    (prev_y9       < thr_lo) | (prev_y9       > thr_hi)
  ]
  DT <- DT0[
    (prev_avg_1_8 >= thr_lo) & (prev_avg_1_8 <= thr_hi) &
    (prev_y9       >= thr_lo) & (prev_y9       <= thr_hi)
  ]
  list(DT = DT, DT_excluded = DT_excluded)
}

#' @noRd
make_value_weights <- function(eir_raw, digits = 3) {
  key <- round(eir_raw, digits)
  freq <- table(key)
  w <- 1 / as.numeric(freq[as.character(key)])
  w / mean(w)
}

#' @noRd
strata_and_split <- function(DT, k_strata = 16, seed = 42) {
  set.seed(seed)
  km <- stats::kmeans(DT$eir_log10, centers = k_strata, nstart = 50, iter.max = 5000)
  ord <- order(km$centers[, 1]); id_map <- setNames(seq_len(k_strata), ord)
  DT[, strat_bin := id_map[km$cluster]]

  DT[, split := NA_character_]
  for (b in sort(unique(DT$strat_bin))) {
    idx  <- which(DT$strat_bin == b)
    n_b  <- length(idx)
    n_tr <- floor(0.70 * n_b)
    n_val <- floor(0.15 * n_b)
    tr  <- if (n_tr  > 0) sample(idx, n_tr)  else integer(0)
    rem <- setdiff(idx, tr)
    val <- if (n_val > 0) sample(rem, n_val) else integer(0)
    te  <- setdiff(rem, val)
    DT$split[tr]  <- "train"; DT$split[val] <- "val"; DT$split[te]  <- "test"
  }
  DT[is.na(split), split := "train"]
  DT[]
}
