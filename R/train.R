#' Train XGBoost with K-fold CV, QMAP+scale calibration, and optional artifacts
#' 
#' @param in_parquet Path to input parquet
#' @param out_dir Base output directory (models/plots/metrics/predictions will be created)
#' @param thr_lo Lower prevalence filter (inclusive)
#' @param thr_hi Upper prevalence filter (inclusive)
#' @param k_strata Number of strata for k-means on log10(EIR)
#' @param K Number of CV folds
#' @param seed Random seed for reproducibility
#' @param xgb_params List of xgboost parameters
#' @param nrounds_max Max rounds per fold for early-stopped training
#' @param early_stopping_rounds Early stopping patience
#' @param save_rds Save an RDS bundle with model, calibrator, metadata
#' @param export_onnx Attempt ONNX export (will stop with message in R-only workflow)
#' @param save_plots Save diagnostic plots
#' @param save_artifacts Save CSV metrics and fold stats
#' @return A list of class 'estiMINT_model' with booster, calibrator, features, metadata
#' @export
train_xgb_model <- function(
  in_parquet,
  out_dir,
  thr_lo = 0.02,
  thr_hi = 0.95,
  k_strata = 16,
  K = 10,
  seed = 42,
  xgb_params = list(
    objective = "reg:squarederror",
    eval_metric = "rmse",
    tree_method = "hist",
    max_depth = 6,
    eta = 0.05,
    subsample = 0.8,
    colsample_bytree = 0.8,
    min_child_weight = 1.0,
    lambda = 1.0
  ),
  nrounds_max = 5000,
  early_stopping_rounds = 100,
  save_rds = TRUE,
  export_onnx = FALSE,
  save_plots = TRUE,
  save_artifacts = TRUE
) {
  stopifnot(length(in_parquet) == 1, length(out_dir) == 1)
  dir_models <- file.path(out_dir, "models")
  dir_plots  <- file.path(out_dir, "plots")
  dir_metric <- file.path(out_dir, "metrics")
  dir_pred   <- file.path(out_dir, "predictions")
  dir.create(dir_models, TRUE, FALSE)
  dir.create(dir_plots,  TRUE, FALSE)
  dir.create(dir_metric, TRUE, FALSE)
  dir.create(dir_pred,   TRUE, FALSE)

  ts("Reading parquet & applying prevalence filters ...")
  lf <- load_and_filter(in_parquet, thr_lo = thr_lo, thr_hi = thr_hi)
  DT  <- lf$DT
  DT_excluded <- lf$DT_excluded

  data.table::fwrite(DT_excluded, file.path(dir_metric,
    sprintf("excluded_prev_outside_0p%02d_0p%02d.csv", round(thr_lo*100), round(thr_hi*100))))
  data.table::fwrite(DT, file.path(dir_metric,
    sprintf("kept_after_prev_filters_0p%02d_0p%02d.csv", round(thr_lo*100), round(thr_hi*100))))

  features <- c("dn0_use","Q0","phi_bednets","seasonal","itn_use","irs_use","prev_y9")
  DT[, eir_log10 := log10(eir)]
  stopifnot(nrow(DT) > 0)

  set.seed(seed)
  ts("Creating %d strata on log10(EIR) and 70/15/15 split ...", k_strata)
  DT <- strata_and_split(DT, k_strata = k_strata, seed = seed)

  # Hold-out test
  X_test  <- as.matrix(DT[split=="test",  ..features])
  y_test  <- DT[split=="test",  eir_log10]
  obs_eir_test <- 10^y_test
  stopifnot(all(is.finite(X_test)), all(is.finite(y_test)))

  # CV folds on train+val
  ts("Assigning %d-fold CV within TRAIN+VAL strata ...", K)
  DTcv <- data.table::copy(DT[split != "test"])
  set.seed(seed + 1L)
  DTcv[, fold := {
    ids <- sample(.N)
    rep(1:K, length.out = .N)[order(ids)]
  }, by = strat_bin]

  ts("Running %d-fold CV with early stopping ...", K)
  oof_pred_raw <- rep(NA_real_, nrow(DTcv))
  best_iters   <- integer(K)

  for (k in seq_len(K)) {
    ts(" Fold %d / %d", k, K)
    idx_val <- which(DTcv$fold == k)
    idx_tr  <- which(DTcv$fold != k)

    X_tr <- as.matrix(DTcv[idx_tr, ..features]); y_tr <- DTcv[idx_tr, eir_log10]
    X_va <- as.matrix(DTcv[idx_val, ..features]); y_va <- DTcv[idx_val, eir_log10]

    w_tr <- make_value_weights(10^y_tr, digits = 3)
    w_va <- make_value_weights(10^y_va, digits = 3)

    dtr <- xgboost::xgb.DMatrix(X_tr, label = y_tr, weight = w_tr)
    dva <- xgboost::xgb.DMatrix(X_va, label = y_va, weight = w_va)

    mdl <- xgboost::xgb.train(
      params = xgb_params, data = dtr,
      nrounds = nrounds_max, watchlist = list(train = dtr, val = dva),
      early_stopping_rounds = early_stopping_rounds, verbose = 0
    )
    best_iters[k] <- mdl$best_iteration

    pred_log10_va <- predict(mdl, dva)
    oof_pred_raw[idx_val] <- 10^pred_log10_va
  }

  stopifnot(all(is.finite(oof_pred_raw)))
  obs_cv_raw <- 10^DTcv$eir_log10

  if (save_artifacts) {
    data.table::fwrite(data.table::data.table(fold = seq_len(K), best_iteration = best_iters),
                       file.path(dir_metric, sprintf("cv_fold_best_iterations_K%d.csv", K)))
  }

  ts("Fitting final calibrator (QMAP + positive scale) on OOF ...")
  cal_oof <- fit_qmap_w(oof_pred_raw, obs_cv_raw, ngrid = 1024, round_digits = 8)
  oof_pred_cal <- predict_qmap_w(oof_pred_raw, cal_oof)
  a_oof <- scale_pos(obs_cv_raw, oof_pred_cal)
  oof_pred_final <- pmax(0, a_oof * oof_pred_cal)

  if (save_artifacts) {
    oof_metrics <- data.table::data.table(
      set=c("OOF_uncalibrated","OOF_calibrated"),
      R2   = c(r2(obs_cv_raw, oof_pred_raw),    r2(obs_cv_raw, oof_pred_final)),
      bias = c(mean(oof_pred_raw-obs_cv_raw),   mean(oof_pred_final-obs_cv_raw)),
      MSE  = c(mse(obs_cv_raw,oof_pred_raw),    mse(obs_cv_raw,oof_pred_final)),
      RMSE = c(rmse(obs_cv_raw,oof_pred_raw),   rmse(obs_cv_raw,oof_pred_final)),
      MAE  = c(mae(obs_cv_raw,oof_pred_raw),    mae(obs_cv_raw,oof_pred_final)),
      MedianAE = c(median_ae(obs_cv_raw,oof_pred_raw), median_ae(obs_cv_raw,oof_pred_final)),
      MAE_rel  = c(mae_rel(obs_cv_raw,oof_pred_raw),   mae_rel(obs_cv_raw,oof_pred_final)),
      RMSLE    = c(rmsle(obs_cv_raw,oof_pred_raw),     rmsle(obs_cv_raw,oof_pred_final)),
      NRMSE_mean = c(safe_div(rmse(obs_cv_raw,oof_pred_raw), mean(obs_cv_raw)),
                     safe_div(rmse(obs_cv_raw,oof_pred_final), mean(obs_cv_raw))),
      RelRMSE_p1 = c(
        sqrt(mean(safe_div(oof_pred_raw-obs_cv_raw, pmax(1, obs_cv_raw))^2)),
        sqrt(mean(safe_div(oof_pred_final-obs_cv_raw, pmax(1, obs_cv_raw))^2))
      ),
      sMAPE = c(smape(obs_cv_raw,oof_pred_raw), smape(obs_cv_raw,oof_pred_final))
    )
    data.table::fwrite(oof_metrics, file.path(dir_metric, sprintf("eir_OOF_metrics_K%dCV.csv", K)))
  }

  ts("Training final model on TRAIN+VAL with nrounds = median(best_iteration) ...")
  best_nrounds <- as.integer(round(stats::median(best_iters)))

  X_trcv <- as.matrix(DT[split!="test", ..features])
  y_trcv <- DT[split!="test", eir_log10]
  w_trcv <- make_value_weights(10^y_trcv, digits=3)
  dtrcv  <- xgboost::xgb.DMatrix(X_trcv, label = y_trcv, weight = w_trcv)

  xgb_cvfit <- xgboost::xgb.train(params = xgb_params, data = dtrcv, nrounds = best_nrounds, verbose = 0)
  xgboost::xgb.save(xgb_cvfit, file.path(dir_models, "eir_xgb_KCV.model"))

  # Predict on TEST then calibrate via OOF calibrator
  dtest <- xgboost::xgb.DMatrix(X_test, label = y_test)
  pred_log10_test_raw <- predict(xgb_cvfit, dtest)
  pred_raw_test <- 10^pred_log10_test_raw
  pred_eir_test <- predict_qmap_w(pred_raw_test, cal_oof)
  pred_eir_test <- pmax(0, a_oof * pred_eir_test)

  data.table::fwrite(data.table::data.table(obs = obs_eir_test, pred_xgb = pred_eir_test),
                     file.path(dir_pred, "eir_test_predictions_xgb_QMAP_SCALE.csv"))

  if (save_plots) {
    plot_obs_pred(
      obs_eir_test, pred_eir_test,
      sprintf("EIR — Observed vs Predicted (XGBoost, K=%d CV, QMAP+Scale, test)", K),
      file.path(dir_plots, "eir_obs_vs_pred_xgb_QMAP_SCALE_test.png"),
      xlab = "Observed EIR", ylab = "Predicted EIR"
    )
    plot_obs_pred(
      y_test, log10(pmax(1e-12, pred_eir_test)),
      sprintf("EIR (log10) — Observed vs Predicted (XGBoost, K=%d CV after QMAP+Scale, test)", K),
      file.path(dir_plots, "eir_log10_obs_vs_pred_xgb_after_QMAP_SCALE_test.png"),
      xlab = "Observed log10(EIR)", ylab = "Predicted log10(EIR)"
    )
  }

  # Range metrics
  cuts <- cut(obs_eir_test, breaks = c(0,10,50,100,200,Inf), include.lowest = TRUE)
  DTm <- data.table::data.table(
    range = cuts,
    obs   = obs_eir_test,
    pred  = pred_eir_test,
    err   = pred_eir_test - obs_eir_test
  )
  per_range <- DTm[, .(
    N         = .N,
    obs_mean  = mean(obs),
    obs_median= stats::median(obs),
    obs_sd    = stats::sd(obs),
    pred_mean = mean(pred),
    bias      = mean(err),
    MAE       = mae(obs, pred),
    MedianAE  = median_ae(obs, pred),
    RMSE      = rmse(obs, pred),
    RMSLE     = rmsle(obs, pred),
    NRMSE_mean= safe_div(rmse(obs, pred), mean(obs)),
    RelRMSE_p1= sqrt(mean(safe_div(err, pmax(1, obs))^2)),
    sMAPE     = smape(obs, pred)
  ), by = range][order(as.numeric(range))]

  if (save_artifacts) {
    data.table::fwrite(per_range[, .(model = "xgboost_KCV", range = as.character(range), RMSE)],
                       file.path(dir_metric, "eir_RMSE_by_range_test_QMAP_SCALE.csv"))
    data.table::fwrite(per_range[, .(
      model="xgboost_KCV", range=as.character(range), N, obs_mean, obs_median, obs_sd,
      pred_mean, bias, MAE, MedianAE, RMSE, RMSLE, NRMSE_mean, RelRMSE_p1, sMAPE
    )], file.path(dir_metric, "eir_metrics_by_range_test_QMAP_SCALE.csv"))
  }

  # Train on ALL filtered data for deployment
  ts("Training deployment booster on ALL filtered data ...")
  X_all <- as.matrix(DT[, ..features]); y_all <- DT$eir_log10
  dall <- xgboost::xgb.DMatrix(X_all, label = y_all)
  xgb_final <- xgboost::xgb.train(params = xgb_params, data = dall, nrounds = best_nrounds, verbose = 0)
  xgboost::xgb.save(xgb_final, file.path(dir_models, "eir_xgb_FINAL.model"))

  cal_bundle <- list(kind="qmap+scale",
                     qmap=list(xq=cal_oof$xq, yq=cal_oof$yq),
                     scale=a_oof)

  preprocess <- list(
    features = features,
    target   = "eir",
    transform = "log10",
    inverse   = "pow10",
    prevalence_filter = list(min_prev_input = thr_lo, avg_prev_years_1_to_8_ge = thr_lo, year9_prev_le = thr_hi),
    reweighting = list(scheme = "inverse_frequency_by_raw_EIR_value", digits = 3, applied_to = c("train","val","cv_folds")),
    cv = list(K = K, stratify_by = sprintf("strat_bin (k-means on log10(EIR), centers=%d)", k_strata),
              best_iteration_median = best_nrounds),
    calibration = list(final = "QMAP then positive scale", final_pred = "pmax(0, a * QMAP(10^pred_log10))")
  )

  model_bundle <- list(
    class = "estiMINT_model",
    booster = xgb_final,
    calibrator = cal_bundle,
    features = features,
    best_nrounds = best_nrounds,
    preprocess = preprocess,
    artifacts = list(
      dir_models = dir_models,
      dir_plots  = dir_plots,
      dir_metric = dir_metric,
      dir_pred   = dir_pred
    )
  )
  class(model_bundle) <- c("estiMINT_model", class(model_bundle))

  if (save_rds) {
    saveRDS(model_bundle, file.path(dir_models, "estiMINT_model.rds"), compress = "xz")
  }

  if (export_onnx) {
    stop("ONNX export from R xgboost is not natively supported. Convert externally (e.g., Python onnxmltools).")
  }

  ts("Done. Artifacts saved under: %s", out_dir)
  model_bundle
}
