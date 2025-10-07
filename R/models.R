#' Train XGBoost model for EIR prediction
#'
#' @param X_train Numeric matrix of training features
#' @param y_train Numeric vector of training targets
#' @param X_val Numeric matrix of validation features (default: NULL)
#' @param y_val Numeric vector of validation targets (default: NULL)
#' @param tune_params Logical whether to tune hyperparameters (default: TRUE)
#' @return List containing model, parameters, and transformation functions
#' @export
train_eir_xgboost <- function(X_train, y_train, X_val = NULL, y_val = NULL,
                          tune_params = TRUE) {

  y_train_log <- log10(y_train + 1)
  dtrain <- xgb.DMatrix(data = X_train, label = y_train_log)

  watchlist <- list(train = dtrain)
  if (!is.null(X_val) && !is.null(y_val)) {
    dval <- xgb.DMatrix(data = X_val, label = log10(y_val + 1))
    watchlist$eval <- dval
  }

  base_params <- list(
    objective = "reg:squarederror",
    eval_metric = "rmse",
    eta = 0.05,
    max_depth = 4,
    min_child_weight = 5,
    subsample = 0.7,
    colsample_bytree = 0.7,
    gamma = 0.1,
    alpha = 0.1,
    lambda = 1.0
  )

  if (tune_params) {
    best_rmse <- Inf; best_params <- base_params; best_nrounds <- 100
    for (depth in c(3, 4, 5))
      for (eta in c(0.01, 0.05, 0.1))
        for (subsample in c(0.6, 0.7, 0.8)) {
          params <- modifyList(base_params,
                               list(max_depth = depth, eta = eta,
                                    subsample = subsample))
          cv <- xgb.cv(params = params, data = dtrain,
                       nfold = 5, nrounds = 500,
                       early_stopping_rounds = 20, verbose = FALSE,
                       seed = 42)
          cv_rmse <- cv$evaluation_log$test_rmse_mean[cv$best_iteration]
          if (cv_rmse < best_rmse) {
            best_rmse    <- cv_rmse
            best_params  <- params
            best_nrounds <- cv$best_iteration
          }
        }
    params  <- best_params
    nrounds <- best_nrounds
    message(sprintf(
      "Best XGBoost: depth=%d, eta=%.3f, subsample=%.2f, nrounds=%d, CV-RMSE=%.4f",
      params$max_depth, params$eta, params$subsample, nrounds, best_rmse))
  } else {
    cv <- xgb.cv(params = base_params, data = dtrain,
                 nfold = 5, nrounds = 500,
                 early_stopping_rounds = 20, verbose = FALSE,
                 seed = 42)
    params  <- base_params
    nrounds <- cv$best_iteration
  }

  model <- xgb.train(params = params, data = dtrain,
                     nrounds = nrounds, watchlist = watchlist,
                     verbose = 0)

  importance <- xgb.importance(model = model)
  list(
    model = model,
    params = params,
    nrounds = nrounds,
    importance = importance,
    transform          = function(y) log10(y + 1),
    inverse_transform  = function(y) 10^y - 1
  )
}