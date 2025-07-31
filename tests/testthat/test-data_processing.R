test_that("get_timestep_window returns correct structure", {
  # Mock connection and query result
  mock_con <- list()
  mock_table <- "test_table"
  
  # Create a mock dbGetQuery function
  with_mock(
    dbGetQuery = function(con, query) {
      list(min_ts = 100)
    },
    {
      result <- get_timestep_window(mock_con, mock_table, y0 = 2, y1 = 3)
      
      expect_type(result, "list")
      expect_equal(names(result), c("start", "end", "data_start"))
      expect_equal(result$start, 100 + 2*365)
      expect_equal(result$end, 100 + 3*365)
      expect_equal(result$data_start, 100)
    }
  )
})

test_that("clean_features handles missing columns", {
  # Create test data
  df <- data.frame(
    col1 = 1:5,
    col2 = 6:10
  )
  feature_cols <- c("col1", "col2", "col3")
  
  result <- clean_features(df, feature_cols)
  
  expect_s3_class(result, "matrix")
  expect_equal(ncol(result), 3)
  expect_equal(colnames(result), feature_cols)
  expect_true(all(result[, "col3"] == 0))
})

test_that("clean_features converts integer64 to numeric", {
  df <- data.frame(
    col1 = bit64::as.integer64(1:5),
    col2 = factor(c("a", "b", "c", "d", "e"))
  )
  
  result <- clean_features(df, c("col1", "col2"))
  
  expect_type(result, "double")
  expect_true(is.numeric(result[, "col1"]))
  expect_true(is.numeric(result[, "col2"]))
})

test_that("validate_data prints correct message", {
  X <- matrix(1:20, nrow = 5, ncol = 4)
  y <- 1:5
  
  expect_message(
    validate_data(X, y, "Test"),
    regexp = "Test: \\[5 x 4\\] target=5.*range=\\[1.00, 5.00\\]"
  )
})
