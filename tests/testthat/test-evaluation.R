test_that("evaluate_model returns correct structure", {
  # Create mock model object
  mock_model <- list(
    model = structure(list(), class = "mock_model"),  # Add class for detection
    inverse_transform = function(y) 10^y - 1
  )
  
  # Using with_mocked_bindings
  with_mocked_bindings(
    {
      X_test <- matrix(rnorm(25), nrow = 5, ncol = 5)
      y_test <- 1:5
      
      result <- evaluate_model(mock_model, X_test, y_test, "TestModel")
      
      expect_type(result, "list")
      expect_true("predictions" %in% names(result))
      expect_true("metrics" %in% names(result))
      expect_s3_class(result$metrics, "data.frame")
      expect_equal(nrow(result$metrics), 1)
      expect_true(all(c("Model", "RMSE", "MAE", "R2", "MAPE", "Correlation") %in% names(result$metrics)))
    },
    predict = function(object, newdata) {
      # Check what type of input we have
      if (inherits(newdata, "xgb.DMatrix")) {
        return(log10(1:5 + 1))  # Return vector for xgboost
      } else {
        # Return list with predictions element for ranger
        return(list(predictions = log10(1:5 + 1)))
      }
    },
    .package = "stats"  # Specify we're mocking stats::predict
  )
})

test_that("evaluate_model handles zero predictions correctly", {
  # Create mock model that returns negative log values
  mock_model <- list(
    model = structure(list(), class = "ranger"),  # Mock ranger model
    inverse_transform = function(y) pmax(10^y - 2, -1)  # Can return negative
  )
  
  with_mocked_bindings(
    {
      X_test <- matrix(rnorm(25), nrow = 5, ncol = 5)
      y_test <- runif(5, 0, 1)
      
      result <- evaluate_model(mock_model, X_test, y_test, "TestModel")
      
      # Check that predictions are non-negative (clamped to 0)
      expect_true(all(result$predictions >= 0))
    },
    predict = function(object, newdata) {
      # Return appropriate format based on model type
      if (inherits(object, "xgb.Booster")) {
        return(rep(log10(0.5), 5))
      } else {
        # For ranger or other models
        return(list(predictions = rep(log10(0.5), 5)))
      }
    },
    .package = "stats"
  )
})