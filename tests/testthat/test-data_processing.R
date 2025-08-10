test_that("get_timestep_window uses fixed 2–3 mapping", {
  # No DB call on purpose in this branch
  res <- get_timestep_window(con = NULL, table_name = "ignored", y0 = 2, y1 = 3)
  expect_type(res, "list")
  expect_equal(names(res), c("start", "end", "data_start"))
  expect_equal(res$start, 2920)
  expect_equal(res$end, 3650)
  expect_equal(res$data_start, 2190)
})

test_that("get_timestep_window uses DB-based mapping for other ranges", {
  with_mocked_bindings(
    {
      res <- get_timestep_window(mock_con, "test_table", y0 = 1, y1 = 2)
      expect_type(res, "list")
      expect_equal(names(res), c("start", "end", "data_start"))
      expect_equal(res$start, 100 + 1*365)
      expect_equal(res$end,   100 + 2*365)
      expect_equal(res$data_start, 100)
    },
    # If your implementation calls dbGetQuery unqualified, leave as is:
    dbGetQuery = function(con, query) list(min_ts = 100)
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
  
  expect_true(is.matrix(result))
  expect_equal(ncol(result), 3)
  expect_equal(colnames(result), feature_cols)
  expect_true(all(result[, "col3"] == 0))
})

test_that("clean_features converts integer64 to numeric", {
  skip_if_not_installed("bit64")
  
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