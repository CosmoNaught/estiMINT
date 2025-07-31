test_that("evaluate_model returns correct structure", {
  # Create mock model object
  mock_model <- list(
    model = list(class = "mock_model"),
    inverse_transform = function(y) 10^y - 1
  )
  
  # Mock predict function for our mock model
  with_mock(
    predict = function(object, newdata) {
      if ("xgb.DMatrix" %in% class(newdata)) {
        log10(1:5 + 1)  # Return log-transformed predictions
      } else {
        list(predictions = log10(1:5 + 1))
      }
    },
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
    }
  )
})

test_that("evaluate_model handles zero predictions correctly", {
  # Create mock model that returns negative log values (leading to negative predictions)
  mock_model <- list(
    model = list(),
    inverse_transform = function(y) pmax(10^y - 2, -1)  # Can return negative
  )
  
  with_mock(
    predict = function(object, newdata) {
      rep(log10(0.5), 5)  # Will give negative after inverse transform
    },
    {
      X_test <- matrix(rnorm(25), nrow = 5, ncol = 5)
      y_test <- runif(5, 0, 1)
      
      result <- evaluate_model(mock_model, X_test, y_test, "TestModel")
      
      # Check that predictions are non-negative (clamped to 0)
      expect_true(all(result$predictions >= 0))
    }
  )
})
