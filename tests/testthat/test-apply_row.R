test_that("apply_row works with basic functions", {
  # Create test data
  df <- data.frame(
    a = c(1, 2, 3),
    b = c(4, 5, 6),
    c = c(7, 8, 9)
  )
  
  # Test sum function
  result <- apply_row(df, everything(), sum)
  expected <- c(12, 15, 18)  # 1+4+7, 2+5+8, 3+6+9
  expect_equal(result, expected)
})

test_that("apply_row works with column selection", {
  df <- data.frame(
    a = c(1, 2, 3),
    b = c(4, 5, 6),
    c = c(7, 8, 9)
  )
  
  # Test with specific columns
  result <- apply_row(df, c("a", "b"), sum)
  expected <- c(5, 7, 9)  # 1+4, 2+5, 3+6
  expect_equal(result, expected)
})

test_that("apply_row handles NA values correctly", {
  df <- data.frame(
    a = c(1, NA, 3),
    b = c(4, 5, 6)
  )
  
  # Without na.rm
  result1 <- apply_row(df, everything(), sum)
  expect_equal(result1[1], 5)
  expect_true(is.na(result1[2]))
  expect_equal(result1[3], 9)
  
  # With na.rm
  result2 <- apply_row(df, everything(), sum, na.rm = TRUE)
  expect_equal(result2, c(5, 5, 9))
})

test_that("apply_row works with different functions", {
  df <- data.frame(
    a = c(1, 2, 3),
    b = c(4, 5, 6),
    c = c(7, 8, 9)
  )
  
  # Test mean
  result_mean <- apply_row(df, everything(), mean)
  expected_mean <- c(4, 5, 6)
  expect_equal(result_mean, expected_mean)
  
  # Test max
  result_max <- apply_row(df, everything(), max)
  expected_max <- c(7, 8, 9)
  expect_equal(result_max, expected_max)
  
  # Test min
  result_min <- apply_row(df, everything(), min)
  expected_min <- c(1, 2, 3)
  expect_equal(result_min, expected_min)
})

test_that("apply_row works with custom functions", {
  df <- data.frame(
    a = c(1, 2, 3),
    b = c(4, 5, 6)
  )
  
  # Custom function to check if any value > 3
  result <- apply_row(df, everything(), function(x) any(x > 3))
  expected <- c(TRUE, TRUE, TRUE)  # All rows have values > 3
  expect_equal(result, expected)
    # Custom function to count values > 2
  result2 <- apply_row(df, everything(), function(x) sum(x > 2))
  expected2 <- c(1, 1, 2)  # Row 1: only 4>2 (1 value), Row 2: only 5>2 (1 value), Row 3: both 3,6>2 (2 values)
  expect_equal(result2, expected2)
})
