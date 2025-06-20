test_that("maxmin scales values between 0 and 1", {
  x <- c(10, 20, 30, 40, 50)
  result <- maxmin(x)
  
  expect_equal(min(result), 0)
  expect_equal(max(result), 1)
  expect_equal(result, c(0, 0.25, 0.5, 0.75, 1))
})

test_that("maxmin handles single values", {
  result <- maxmin(5)
  expect_true(is.nan(result) || result == 0) # Single value should result in NaN or 0
})

test_that("maxmin handles identical values", {
  x <- c(5, 5, 5, 5)
  result <- maxmin(x)
  expect_true(all(is.nan(result)) || all(result == 0)) # All identical should be NaN or 0
})

test_that("maxmin handles negative values", {
  x <- c(-10, -5, 0, 5, 10)
  result <- maxmin(x)
  
  expect_equal(min(result), 0)
  expect_equal(max(result), 1)
  expect_equal(length(result), 5)
})

test_that("maxmin handles NA values with warning", {
  x <- c(1, 2, NA, 4, 5)
  
  expect_warning(result <- maxmin(x), "vector contains missing values")
  
  # Should still scale non-NA values correctly
  expect_equal(result[1], 0)  # min value -> 0
  expect_equal(result[5], 1)  # max value -> 1
  expect_true(is.na(result[3]))  # NA should remain NA
})

test_that("maxmin returns numeric vector", {
  x <- c(1L, 2L, 3L, 4L, 5L)  # Integer input
  result <- maxmin(x)
  
  expect_true(is.numeric(result))
  expect_false(is.integer(result))
})

test_that("maxmin example from documentation works", {
  # Test with normal distribution
  set.seed(123)
  rand <- rnorm(100, mean = 0, sd = 1)
  result <- maxmin(rand)
  
  expect_equal(min(result, na.rm = TRUE), 0)
  expect_equal(max(result, na.rm = TRUE), 1)
  expect_length(result, 100)
})

test_that("maxmin handles decimal values correctly", {
  x <- c(0.1, 0.5, 0.9)
  result <- maxmin(x)
  
  expect_equal(result[1], 0)    # 0.1 -> 0
  expect_equal(result[2], 0.5)  # 0.5 -> 0.5
  expect_equal(result[3], 1)    # 0.9 -> 1
})
