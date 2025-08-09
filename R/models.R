#' Train XGBoost model for case prediction with dynamic weighting
#'
#' @param X_train Numeric matrix of training features
#' @param y_train Numeric vector of training targets
#' @param X_val Numeric matrix of validation features (default: NULL)
#' @param y_val Numeric vector of validation targets (default: NULL)
#' @param tune_params Logical whether to tune hyperparameters (default: TRUE)
#' @param use_case_weights Logical whether to use dynamic case weights (default: TRUE)
#' @param weight_power Numeric power for weight calculation (default: 0.5)
#' @return List containing model, parameters, and transformation functions
#' @export
train_xgboost <- function(X_train, y_train, X_val = NULL, y_val = NULL,
                          tune_params = TRUE, use_case_weights = TRUE, 
                          weight_power = 0.7) {
  message("XGB using ", get_threads(), " cores")
  
  # Enhanced weight calculation with year-awareness and extreme case handling
  calculate_weights <- function(y, X = NULL) {
    # More aggressive weighting for extreme values
    # Use adaptive quantiles based on actual distribution
    
    # First, identify the distribution characteristics
    y_median <- median(y)
    y_q75 <- quantile(y, 0.75)
    y_q90 <- quantile(y, 0.90)
    y_q95 <- quantile(y, 0.95)
    y_q99 <- quantile(y, 0.99)
    
    # Create weights based on rarity and importance
    weights <- rep(1, length(y))
    
    # Progressive weight increases for higher quantiles
    weights[y >= y_median] <- 2
    weights[y >= y_q75] <- 4
    weights[y >= y_q90] <- 8
    weights[y >= y_q95] <- 16
    weights[y >= y_q99] <- 128
    
    # Additional boost for extreme outliers (top 1%)
    extreme_threshold <- quantile(y, 0.99)
    is_extreme <- y >= extreme_threshold
    if (sum(is_extreme) > 0) {
      # Scale weights by how extreme the value is
      extreme_scale <- (y[is_extreme] / extreme_threshold)^weight_power
      weights[is_extreme] <- weights[is_extreme] * extreme_scale
    }
    
    # Year-based adjustment if year column is available
    if (!is.null(X) && "year" %in% colnames(X)) {
      year_col_idx <- which(colnames(X) == "year")
      years <- X[, year_col_idx]
      
      # Give extra weight to later years which have more diverse outcomes
      year_multiplier <- 1 + (years / max(years)) * 0.5
      weights <- weights * year_multiplier
      
      # Extra weight for high cases in early years (rare but important)
      early_high <- (years <= 2) & (y >= y_q75)
      weights[early_high] <- weights[early_high] * 2
    }
    
    # Normalize to mean = 1 for stability
    weights <- weights * length(weights) / sum(weights)
    
    message(sprintf("Weight stats: min=%.2f, median=%.2f, max=%.2f, >10: %d samples", 
                    min(weights), median(weights), max(weights), sum(weights > 10)))
    
    # Log weight distribution for high cases
    high_case_weights <- weights[y >= y_q95]
    if (length(high_case_weights) > 0) {
      message(sprintf("High case (>95%%) weights: min=%.2f, median=%.2f, max=%.2f", 
                      min(high_case_weights), median(high_case_weights), max(high_case_weights)))
    }
    
    return(weights)
  }
  
  # Calculate weights with year awareness
  if (use_case_weights) {
    train_weights <- calculate_weights(y_train, X_train)
  } else {
    train_weights <- rep(1, length(y_train))
  }
  
  # For Tweedie, ensure positive values
  y_train_shifted <- y_train + 0.001
  
  # Create DMatrix with weights
  dtrain <- xgb.DMatrix(data = X_train, 
                        label = y_train_shifted,
                        weight = train_weights)
  
  watchlist <- list(train = dtrain)
  if (!is.null(X_val) && !is.null(y_val)) {
    val_weights <- if (use_case_weights) calculate_weights(y_val, X_val) else rep(1, length(y_val))
    dval <- xgb.DMatrix(data = X_val, 
                        label = y_val + 0.001,
                        weight = val_weights)
    watchlist$eval <- dval
  }
  
  # Adjusted parameters for better handling of extreme values
  base_params <- list(
    objective = "reg:tweedie",
    tweedie_variance_power = 1.15,  # Fine-tuned for count data
    eval_metric = "rmse",
    eta = 0.02,  # Even slower learning
    max_depth = 8,  # Deeper for complex patterns
    min_child_weight = 1,  # Allow smaller leaf nodes for rare cases
    subsample = 0.85,
    colsample_bytree = 0.85,
    gamma = 0.01,  # Very low - allow more splits
    alpha = 0.01,  # Minimal L1
    lambda = 0.1,  # Minimal L2
    max_delta_step = 2,  # Higher for imbalanced data
    nthread = get_threads()
  )

if (tune_params) {
  message("Starting enhanced hyperparameter tuning...")
  best_rmse <- Inf; best_params <- base_params; best_nrounds <- 100
  set.seed(42)
  
  BUDGET <- 16
  L <- lhs::maximinLHS(BUDGET, 6)
  linmap <- function(u, lo, hi) lo + u * (hi - lo)
  
  grid <- data.frame(
    max_depth = as.integer(round(linmap(L[,1], 6, 12))),  # Deeper trees
    eta = linmap(L[,2], 0.01, 0.04),  # Lower learning rates
    min_child_weight = c(1,1,2,3,5)[pmax(1, pmin(5, ceiling(L[,3]*5)))],  # Allow 1
    tweedie_variance_power = linmap(L[,4], 1.05, 1.25),  # Narrower range
    subsample = linmap(L[,5], 0.75, 0.95),  # Higher subsample
    colsample_bytree = linmap(L[,6], 0.75, 0.95)  # Higher colsample
  )
  
  handlers(global = TRUE)
  message(sprintf("Testing %d parameter combinations (LHS)...", nrow(grid)))
  
  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  
  workers <- min(8L, nrow(grid))
  future::plan(future::multisession, workers = workers)  # Changed from multicore
  per_combo_threads <- max(1L, floor(get_threads() / workers))
  
  results <- with_progress({
    p <- progressor(steps = nrow(grid))
    
    future_lapply(seq_len(nrow(grid)), function(i) {
      # Import xgboost in the worker
      library(xgboost)
      
      g <- grid[i, ]
      params <- modifyList(base_params, list(
        max_depth = g$max_depth,
        eta = g$eta,
        min_child_weight = g$min_child_weight,
        tweedie_variance_power = g$tweedie_variance_power,
        subsample = g$subsample,
        colsample_bytree = g$colsample_bytree,
        nthread = per_combo_threads,
        tree_method = "hist"
      ))
      
      # CRITICAL FIX: Create DMatrix inside the worker
      dtrain_cv <- xgb.DMatrix(
        data = X_train, 
        label = y_train_shifted, 
        weight = train_weights
      )
      
      cv <- xgb.cv(
        params = params,
        data = dtrain_cv,  # Use the locally created DMatrix
        nfold = 5,
        nrounds = 1000,
        early_stopping_rounds = 50,
        verbose = FALSE,
        stratified = FALSE,
        seed = 42
      )
      
      # Clean up the DMatrix to avoid memory issues
      rm(dtrain_cv)
      gc(verbose = FALSE)
      
      # tick the bar when this combo finishes
      p(message = sprintf("combo %d/%d done", i, nrow(grid)))
      
      list(
        score = min(cv$evaluation_log$test_rmse_mean),
        params = params,
        nrounds = cv$best_iteration
      )
    }, 
    future.seed = TRUE,
    future.packages = c("xgboost"),  # Ensure xgboost is loaded in workers
    future.globals = list(  # Pass needed objects explicitly
      X_train = X_train,
      y_train_shifted = y_train_shifted,
      train_weights = train_weights,
      base_params = base_params,
      grid = grid
    ))
  })
    
    # Find best result
    best_idx <- which.min(vapply(results, `[[`, numeric(1), "score"))
    best <- results[[best_idx]]
    
    if (!is.null(best$params)) {
      params <- best$params
      nrounds <- min(best$nrounds, 1500)  # Cap at 1500 to prevent overfitting
      message(sprintf(
        "Best XGBoost: depth=%d, eta=%.3f, min_child=%.0f, tweedie=%.2f, nrounds=%d, Score=%.4f",
        params$max_depth, params$eta, params$min_child_weight,
        params$tweedie_variance_power, nrounds, best$score))
    } else {
      params <- base_params
      nrounds = 500
    }
  } else {
    params <- base_params
    cv <- xgb.cv(params = params, 
                 data = dtrain,
                 nfold = 5, 
                 nrounds = 1000,
                 early_stopping_rounds = 50, 
                 verbose = FALSE,
                 stratified = FALSE,
                 seed = 42)
    nrounds <- min(cv$best_iteration, 1500)
  }
  
  # Train final model with more rounds
  model <- xgb.train(params = params, 
                     data = dtrain,
                     nrounds = nrounds, 
                     watchlist = watchlist,
                     verbose = 0)
  
  importance <- xgb.importance(model = model)
  
  list(
    model = model,
    params = params,
    nrounds = nrounds,
    importance = importance,
    transform = function(y) y + 0.001,
    inverse_transform = function(y) pmax(y - 0.001, 0),
    weights_used = use_case_weights,
    weight_power = weight_power
  )
}

train_random_forest <- function(X_train, y_train,
                                X_val = NULL, y_val = NULL,
                                tune_params = TRUE,
                                use_case_weights = TRUE,
                                weight_power = 0.8) {
  
  # Enhanced weight calculation (same as XGBoost for consistency)
  calculate_weights <- function(y, X = NULL) {
    y_median <- median(y)
    y_q75 <- quantile(y, 0.75)
    y_q90 <- quantile(y, 0.90)
    y_q95 <- quantile(y, 0.95)
    y_q99 <- quantile(y, 0.99)
    
    weights <- rep(1, length(y))
    weights[y >= y_median] <- 2
    weights[y >= y_q75] <- 4
    weights[y >= y_q90] <- 8
    weights[y >= y_q95] <- 16
    weights[y >= y_q99] <- 128
    
    extreme_threshold <- quantile(y, 0.99)
    is_extreme <- y >= extreme_threshold
    if (sum(is_extreme) > 0) {
      extreme_scale <- (y[is_extreme] / extreme_threshold)^weight_power
      weights[is_extreme] <- weights[is_extreme] * extreme_scale
    }
    
    if (!is.null(X) && "year" %in% colnames(X)) {
      year_col_idx <- which(colnames(X) == "year")
      years <- X[, year_col_idx]
      year_multiplier <- 1 + (years / max(years)) * 0.5
      weights <- weights * year_multiplier
      early_high <- (years <= 2) & (y >= y_q75)
      weights[early_high] <- weights[early_high] * 2
    }
    
    weights <- weights * length(weights) / sum(weights)
    return(weights)
  }
  
  # Use square root transform instead of log for better handling of near-zero values
  y_train_transformed <- sqrt(y_train)
  train_df <- data.frame(y = y_train_transformed, X_train)
  
  # Calculate case weights
  if (use_case_weights) {
    case_weights <- calculate_weights(y_train, X_train)
    message(sprintf("RF weight range: [%.3f, %.3f], median: %.3f", 
                    min(case_weights), max(case_weights), median(case_weights)))
  } else {
    case_weights <- NULL
  }
  
  if (tune_params) {
    message("Tuning Random Forest with enhanced parameters...")
    best_oob_error <- Inf
    best_params <- list()
    
    set.seed(42)
    
    # REDUCED BUDGET for stability
    BUDGET <- 16  # Reduced from 16
    L <- lhs::maximinLHS(BUDGET, 4)
    
    linmap <- function(u, lo, hi) lo + u * (hi - lo)
    
    grid <- data.frame(
      num_trees = as.integer(round(linmap(L[,1], 1000, 2000) / 100) * 100),  # Reduced from 3000
      mtry_frac = linmap(L[,2], 0.4, 0.7),
      min_node  = as.integer(round(linmap(L[,3], 1, 8))),
      max_depth = as.integer(round(linmap(L[,4], 16, 25)))  # Reduced from 30
    )
    
    # Setup progress handlers with error handling
    handlers(global = TRUE)
    message(sprintf("Testing %d parameter combinations (LHS)...", nrow(grid)))
    
    # Save and restore future plan
    old_plan <- future::plan()
    on.exit(future::plan(old_plan), add = TRUE)
    
    # Use fewer workers for stability
    workers <- min(4L, nrow(grid))  # Reduced from 8
    future::plan(
      future::multisession, 
      workers = workers,
      gc = TRUE  # Enable garbage collection
    )
    per_combo_threads <- max(1L, floor(get_threads() / workers))
    
    # Suppress RNG warnings for ranger
    oopt <- options(future.rng.onMisuse = "ignore")
    on.exit(options(oopt), add = TRUE, after = FALSE)
    
    # Run with better error handling
    results <- tryCatch({
      with_progress({
        p <- progressor(steps = nrow(grid))
        
        future_lapply(seq_len(nrow(grid)), function(i) {
          # Load ranger in worker
          library(ranger)
          
          # Add error handling within worker
          result <- tryCatch({
            g <- grid[i, ]
            mtry_val <- max(1, floor(g$mtry_frac * ncol(X_train)))
            
            # Add timeout protection (if needed)
            rf_fit <- withTimeout({
              ranger(
                y ~ .,
                data             = train_df,
                num.trees        = g$num_trees,
                mtry             = mtry_val,
                min.node.size    = g$min_node,
                max.depth        = g$max_depth,
                sample.fraction  = 0.632,  # Reduced from 0.8 for memory
                replace          = TRUE,
                case.weights     = case_weights,
                importance       = "none",
                verbose          = FALSE,
                seed             = 42,
                num.threads      = per_combo_threads
              )
            }, timeout = 300, onTimeout = "error")  # 5 minute timeout per model
            
            oob_error <- rf_fit$prediction.error
            
            # Aggressive cleanup
            rm(rf_fit)
            gc(verbose = FALSE, full = TRUE)
            
            list(
              oob = oob_error,
              params = list(
                num.trees     = g$num_trees,
                mtry          = mtry_val,
                min.node.size = g$min_node,
                max.depth     = g$max_depth
              ),
              error = NULL
            )
            
          }, error = function(e) {
            # Return NA result on error
            list(
              oob = NA,
              params = list(
                num.trees     = g$num_trees,
                mtry          = mtry_val,
                min.node.size = g$min_node,
                max.depth     = g$max_depth
              ),
              error = as.character(e)
            )
          })
          
          # Update progress
          if (!is.null(result$error)) {
            p(message = sprintf("[%d/%d] Failed: %s", i, nrow(grid), result$error))
          } else {
            p(message = sprintf(
              "[%d/%d] trees=%d, mtry=%d, OOB=%.4f",
              i, nrow(grid), g$num_trees, mtry_val, result$oob
            ))
          }
          
          return(result)
        },
        future.seed = TRUE,
        future.packages = c("ranger", "R.utils"),
        future.globals = list(
          train_df = train_df,
          case_weights = case_weights,
          X_train = X_train,
          grid = grid,
          per_combo_threads = per_combo_threads
        ),
        future.chunk.size = 1  # Process one at a time for stability
        )
      })
    }, error = function(e) {
      message("Parallel tuning failed, falling back to sequential...")
      
      # Fallback to sequential processing
      results_seq <- list()
      for (i in seq_len(nrow(grid))) {
        g <- grid[i, ]
        mtry_val <- max(1, floor(g$mtry_frac * ncol(X_train)))
        
        rf_fit <- tryCatch({
          ranger(
            y ~ .,
            data = train_df,
            num.trees = g$num_trees,
            mtry = mtry_val,
            min.node.size = g$min_node,
            max.depth = g$max_depth,
            sample.fraction = 0.632,
            replace = TRUE,
            case.weights = case_weights,
            importance = "none",
            verbose = FALSE,
            seed = 42,
            num.threads = get_threads()
          )
        }, error = function(e) NULL)
        
        if (!is.null(rf_fit)) {
          results_seq[[i]] <- list(
            oob = rf_fit$prediction.error,
            params = list(
              num.trees = g$num_trees,
              mtry = mtry_val,
              min.node.size = g$min_node,
              max.depth = g$max_depth
            )
          )
          message(sprintf("[%d/%d] OOB: %.4f", i, nrow(grid), rf_fit$prediction.error))
        } else {
          results_seq[[i]] <- list(oob = NA, params = list())
        }
        
        rm(rf_fit); gc(verbose = FALSE)
      }
      results_seq
    })
    
    # Filter out failed results
    valid_results <- results[!sapply(results, function(x) is.na(x$oob))]
    
    if (length(valid_results) == 0) {
      message("All tuning attempts failed, using default parameters")
      params <- list(
        num.trees = 1000,
        mtry = floor(0.5 * ncol(X_train)),
        min.node.size = 5,
        max.depth = 20
      )
    } else {
      # Find best valid result
      best_idx <- which.min(vapply(valid_results, `[[`, numeric(1), "oob"))
      best <- valid_results[[best_idx]]
      params <- best$params
      best_oob_error <- best$oob
      
      message(sprintf(
        "Best RF: trees=%d, mtry=%d, min.node=%d, max.depth=%d, OOB-error=%.4f",
        params$num.trees, params$mtry, params$min.node.size, params$max.depth, best_oob_error))
    }
    
  } else {
    params <- list(
      num.trees = 1000,
      mtry = floor(0.5 * ncol(X_train)),
      min.node.size = 5,
      max.depth = 20
    )
  }
  
  # Train final model with all threads and importance calculation
  message("Training final Random Forest model...")
  model <- ranger(
    y ~ ., 
    data = train_df,
    num.trees = params$num.trees,
    mtry = params$mtry,
    min.node.size = params$min.node.size,
    max.depth = params$max.depth,
    sample.fraction = 0.632,
    replace = TRUE,
    case.weights = case_weights,
    importance = "impurity",
    num.threads = get_threads(),
    seed = 42
  )
  
  importance <- data.frame(
    feature = names(model$variable.importance),
    importance = model$variable.importance,
    row.names = NULL
  )[order(-model$variable.importance), ]
  
  list(
    model = model,
    params = params,
    importance = importance,
    transform = function(y) sqrt(y),
    inverse_transform = function(y) pmax(y^2, 0),
    weights_used = use_case_weights,
    weight_power = weight_power
  )
}