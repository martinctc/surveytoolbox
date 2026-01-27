# Tests for remove_na_only function
test_that("remove_na_only removes NA-only columns", {
  df <- data.frame(
    a = c(1, 2, 3),
    b = c(NA, NA, NA),
    c = c("x", "y", "z"),
    stringsAsFactors = FALSE
  )
  
  result <- remove_na_only(df)
  
  expect_equal(ncol(result), 2)
  expect_true("a" %in% names(result))
  expect_true("c" %in% names(result))
  expect_false("b" %in% names(result))
})

test_that("remove_na_only keeps columns with some values", {
  df <- data.frame(
    a = c(1, NA, 3),
    b = c(NA, NA, NA),
    c = c(NA, 2, NA)
  )
  
  result <- remove_na_only(df)
  
  expect_equal(ncol(result), 2)
  expect_true("a" %in% names(result))
  expect_true("c" %in% names(result))
})

test_that("remove_na_only handles all valid columns", {
  df <- data.frame(a = 1:3, b = 4:6)
  
  result <- remove_na_only(df)
  
  expect_equal(ncol(result), 2)
  expect_equal(names(result), names(df))
})

# Tests for remove_zero_only function
test_that("remove_zero_only removes zero-only columns", {
  df <- data.frame(
    a = c(1, 2, 3),
    b = c(0, 0, 0),
    c = c(0, 1, 0)
  )
  
  result <- remove_zero_only(df)
  
  expect_equal(ncol(result), 2)
  expect_true("a" %in% names(result))
  expect_true("c" %in% names(result))
  expect_false("b" %in% names(result))
})

test_that("remove_zero_only keeps columns with non-zero values", {
  df <- data.frame(
    a = c(0, 0, 1),
    b = c(0, 0, 0)
  )
  
  result <- remove_zero_only(df)
  
  expect_equal(ncol(result), 1)
  expect_true("a" %in% names(result))
})

# Tests for replace_na_range function
test_that("replace_na_range replaces NAs with values from range", {
  set.seed(123)
  x <- c(1, NA, 2, 3, NA, 2)
  range_vals <- c(1, 2, 3)
  
  result <- replace_na_range(x, range_vals)
  
  expect_false(any(is.na(result)))
  expect_true(all(result %in% range_vals | result %in% c(1, 2, 3)))
})

test_that("replace_na_range preserves non-NA values", {
  x <- c(1, NA, 3)
  result <- replace_na_range(x, c(5, 6, 7))
  
  expect_equal(result[1], 1)
  expect_equal(result[3], 3)
})

test_that("replace_na_range handles all NAs", {
  set.seed(123)
  x <- c(NA, NA, NA)
  result <- replace_na_range(x, c(1, 2, 3))
  
  expect_false(any(is.na(result)))
  expect_true(all(result %in% c(1, 2, 3)))
})

test_that("replace_na_range handles no NAs", {
  x <- c(1, 2, 3)
  result <- replace_na_range(x, c(7, 8, 9))
  
  expect_equal(result, x)
})

# Tests for labelled_quantile function
test_that("labelled_quantile returns character labels", {
  x <- c(1, 1, 1, 1, 2, 3, 5, 5, 6)
  result <- labelled_quantile(x)
  
  expect_type(result, "character")
  expect_length(result, length(x))
})

test_that("labelled_quantile creates meaningful labels", {
  x <- 1:100
  result <- labelled_quantile(x)
  
  # Labels should contain "_TO_" pattern
  expect_true(any(grepl("_TO_", result)))
})

# Tests for timed_fn function
test_that("timed_fn creates timestamped filename", {
  result <- timed_fn("test", ".csv")
  
  expect_type(result, "character")
  expect_true(grepl("test", result))
  expect_true(grepl("\\.csv", result))
})

test_that("timed_fn includes timestamp", {
  result1 <- timed_fn("file", ".txt")
  Sys.sleep(1)  # Small delay
  result2 <- timed_fn("file", ".txt")
  
  # Both should contain the main name
  expect_true(grepl("file", result1))
  expect_true(grepl("file", result2))
  
  # Results should be different due to timestamp
  expect_false(result1 == result2)
})

test_that("timed_fn handles different extensions", {
  csv_result <- timed_fn("data", ".csv")
  xlsx_result <- timed_fn("data", ".xlsx")
  
  expect_true(grepl("\\.csv", csv_result))
  expect_true(grepl("\\.xlsx", xlsx_result))
})
