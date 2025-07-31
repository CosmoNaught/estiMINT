test_that("train_xgboost returns correct structure", {
  # Create simple test data
  set.seed(42)
  X_train <- matrix(rnorm(100), nrow = 20, ncol = 5)
  y_train <- runif(20, 0, 10)
  
  # Train model without tuning for speed
  result <- train_xgboost(X_train, y_train, tune_params = FALSE)
  
  expect_type(result, "list")
  expect_s3_class(result$model, "xgb.Booster")
  expect_true("params" %in% names(result))
  expect_true("nrounds" %in% names(result))
  expect_true("importance" %in% names(result))
  expect_true(is.function(result$transform))
  expect_true(is.function(result$inverse_transform))
})

test_that("train_random_forest returns correct structure", {
  # Create simple test data
  set.seed(42)
  X_train <- matrix(rnorm(100), nrow = 20, ncol = 5)
  y_train <- runif(20, 0, 10)
  
  # Train model without tuning for speed
  result <- train_random_forest(X_train, y_train, tune_params = FALSE)
  
  expect_type(result, "list")
  expect_s3_class(result$model, "ranger")
  expect_true("params" %in% names(result))
  expect_true("importance" %in% names(result))
  expect_s3_class(result$importance, "data.frame")
  expect_true(is.function(result$transform))
  expect_true(is.function(result$inverse_transform))
})

test_that("predict_initial_eir works with xgboost model", {
  # Create simple model
  set.seed(42)
  X_train <- matrix(rnorm(100), nrow = 20, ncol = 5)
  y_train <- runif(20, 0, 10)
  colnames(X_train) <- paste0("feat", 1:5)
  
  model_obj <- train_xgboost(X_train, y_train, tune_params = FALSE)
  
  # Create new data
  new_data <- data.frame(
    feat1 = rnorm(3),
    feat2 = rnorm(3),
    feat3 = rnorm(3),
    feat4 = rnorm(3),
    feat5 = rnorm(3)
  )
  
  predictions <- predict_initial_eir(model_obj, new_data, colnames(X_train))
  
  expect_type(predictions, "double")
  expect_length(predictions, 3)
  expect_true(all(predictions >= 0))
})

test_that("transform and inverse_transform are inverses", {
  set.seed(42)
  X_train <- matrix(rnorm(50), nrow = 10, ncol = 5)
  y_train <- runif(10, 0, 10)
  
  model_obj <- train_xgboost(X_train, y_train, tune_params = FALSE)
  
  # Test values
  test_vals <- c(0, 1, 10, 100)
  transformed <- model_obj$transform(test_vals)
  back_transformed <- model_obj$inverse_transform(transformed)
  
  expect_equal(back_transformed, test_vals, tolerance = 1e-10)
})
