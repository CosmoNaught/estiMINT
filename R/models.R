#' Train XGBoost model for EIR prediction
#'
#' @param X_train Numeric matrix of training features
#' @param y_train Numeric vector of training targets
#' @param X_val Numeric matrix of validation features (default: NULL)
#' @param y_val Numeric vector of validation targets (default: NULL)
#' @param tune_params Logical whether to tune hyperparameters (default: TRUE)
#' @return List containing model, parameters, and transformation functions
#' @export
train_xgboost <- function(X_train, y_train, X_val = NULL, y_val = NULL,
                          tune_params = TRUE) {
  message("XGB using ", get_threads(), " cores")
  y_train_log <- log10(y_train + 1)
  data_mat    <- X_train  
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
    lambda = 1.0,
    nthread = get_threads()
  )

  if (tune_params) {
    best_rmse <- Inf; best_params <- base_params; best_nrounds <- 100
    grid <- expand.grid(
      max_depth = c(3, 4, 5),
      eta       = c(0.01, 0.05, 0.1),
      subsample = c(0.6, 0.7, 0.8),
      KEEP.OUT.ATTRS = FALSE
    )

    oopt <- options(future.rng.onMisuse = "ignore")

    results <- future_lapply(seq_len(nrow(grid)), function(i) {
      library(xgboost)                       # must load inside worker
      g <- grid[i, ]
      params <- modifyList(base_params,
                          list(max_depth = g$max_depth,
                                eta       = g$eta,
                                subsample = g$subsample,
                                nthread   = 1))   # no nested threads

      cv <- xgb.cv(params = params,
                  data   = data_mat,        # <-- plain matrix
                  label  = y_train_log,     # <-- vector
                  nfold  = 5,
                  nrounds = 500,
                  early_stopping_rounds = 20,
                  verbose = FALSE,
                  seed = 42)

      list(rmse    = cv$evaluation_log$test_rmse_mean[cv$best_iteration],
          params  = params,
          nrounds = cv$best_iteration)
    }, future.seed = TRUE)

    options(oopt)

    best <- results[[which.min(vapply(results, `[[`, numeric(1), "rmse"))]]
    params  <- best$params
    nrounds <- best$nrounds
    params$nthread <- get_threads()
    best_rmse <- best$rmse
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

#' Train Random Forest model for EIR prediction
#'
#' @param X_train Numeric matrix of training features
#' @param y_train Numeric vector of training targets
#' @param X_val Numeric matrix of validation features (default: NULL)
#' @param y_val Numeric vector of validation targets (default: NULL)
#' @param tune_params Logical whether to tune hyperparameters (default: TRUE)
#' @return List containing model, parameters, and transformation functions
#' @export
train_random_forest <- function(X_train, y_train,
                                X_val = NULL, y_val = NULL,
                                tune_params = TRUE) {

  y_train_log <- log10(y_train + 1)
  train_df <- data.frame(y = y_train_log, X_train)

  if (tune_params) {
    best_oob_error <- Inf; best_params <- list()
    grid <- expand.grid(
      num_trees = c(500, 1000, 1500),
      mtry_frac = c(0.3, 0.5, 0.7),
      min_node  = c(5, 10, 20),
      KEEP.OUT.ATTRS = FALSE
    )

    oopt <- options(future.rng.onMisuse = "ignore")
    
    results <- future_lapply(seq_len(nrow(grid)), function(i) {
      library(ranger)
      g <- grid[i, ]
      mtry_val <- max(1, floor(g$mtry_frac * ncol(X_train)))

      rf <- ranger(
        y ~ ., data = train_df,
        num.trees       = g$num_trees,
        mtry            = mtry_val,
        min.node.size   = g$min_node,
        max.depth       = 10,
        sample.fraction = 0.632,
        importance      = "impurity",
        seed            = 42,
        num.threads     = 1
      )

      list(oob   = rf$prediction.error,
          params = list(num.trees     = g$num_trees,
                        mtry          = mtry_val,
                        min.node.size = g$min_node))
    }, future.seed = TRUE)

    options(oopt)

    best <- results[[which.min(vapply(results, `[[`, numeric(1), "oob"))]]
    params          <- best$params
    best_oob_error  <- best$oob

    message(sprintf(
      "Best RF: trees=%d, mtry=%d, min.node=%d, OOB-error=%.4f",
      params$num.trees, params$mtry, params$min.node.size, best_oob_error))
  } else {
    params <- list(num.trees     = 1000,
                   mtry          = floor(sqrt(ncol(X_train))),
                   min.node.size = 10)
  }

  model <- ranger(
    y ~ ., data = train_df,
    num.trees     = params$num.trees,
    mtry          = params$mtry,
    min.node.size = params$min.node.size,
    max.depth     = 10,
    sample.fraction = 0.632,
    importance    = "impurity",
    num.threads   = get_threads(),
    seed          = 42
  )

  importance <- data.frame(
    feature    = names(model$variable.importance),
    importance = model$variable.importance,
    row.names  = NULL
  )[order(-model$variable.importance), ]

  list(
    model = model,
    params = params,
    importance = importance,
    transform          = function(y) log10(y + 1),
    inverse_transform  = function(y) 10^y - 1
  )
}

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