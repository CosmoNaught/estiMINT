test_that("bin_variable creates correct number of bins", {
  vec <- runif(100, 0, 1)
  edges <- seq(0, 1, by = 0.2)
  result <- bin_variable(vec, edges)
  
  expect_s3_class(result, "factor")
  expect_equal(nlevels(result), length(edges) - 1)
  expect_equal(levels(result), paste0("B", 1:(length(edges)-1)))
})

test_that("bin_variable handles edge cases", {
  # Test with sufficient edges (3 or more)
  vec <- 1:10
  edges <- c(0, 5, 11)  # This creates 2 bins
  result <- bin_variable(vec, edges)
  
  expect_s3_class(result, "factor")
  expect_equal(nlevels(result), 2)  # Should have 2 levels
  
  # Test with too few edges (< 3) - should return factor of original values
  vec_char <- c("a", "b", "c", "a", "b")
  edges_few <- c(0, 1)  # Only 2 edges
  result_few <- bin_variable(vec_char, edges_few)
  
  expect_s3_class(result_few, "factor")
  # When edges < 3, it should factor the original values
  expect_equal(nlevels(result_few), 3)  # a, b, c
})

test_that("create_default_bin_edges returns correct structure", {
  edges <- create_default_bin_edges()
  
  expect_type(edges, "list")
  expect_true(all(c("dn0_use", "Q0", "phi_bednets", "seasonal", 
                    "routine", "itn_use", "irs_use", "itn_future", 
                    "irs_future", "lsm") %in% names(edges)))
  
  # Check that all elements are numeric
  expect_true(all(sapply(edges, is.numeric)))
  
  # Check specific ranges
  expect_true(all(edges$Q0 >= 0.6 & edges$Q0 <= 1))
  expect_true(all(edges$phi_bednets >= 0.4 & edges$phi_bednets <= 0.95))
  expect_equal(edges$seasonal, c(0, 0.5, 1))
})

test_that("bin_variable preserves data integrity", {
  set.seed(42)
  vec <- runif(50, 0, 10)
  edges <- seq(0, 10, by = 2)
  
  result <- bin_variable(vec, edges, prefix = "Group")
  
  # Check all values are binned
  expect_false(any(is.na(result)))
  expect_equal(length(result), length(vec))
  
  # Check custom prefix
  expect_true(all(grepl("^Group", levels(result))))
})
