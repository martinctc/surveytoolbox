test_that("CAGR calculates correctly for basic cases", {
  # Simple case: doubling over 1 period
  expect_equal(CAGR(100, 200, 1), 1.0)
  
  # No growth case
  expect_equal(CAGR(100, 100, 5), 0.0)
  
  # 10% annual growth over 5 years
  result <- CAGR(100, 161.051, 5)
  expect_equal(round(result, 3), 0.100, tolerance = 0.001)
})

test_that("CAGR handles different time periods", {
  # Same growth rate, different periods
  value_begin <- 1000
  cagr_rate <- 0.05  # 5% annual growth
  
  # 2 years
  value_end_2y <- value_begin * (1 + cagr_rate)^2
  expect_equal(CAGR(value_begin, value_end_2y, 2), cagr_rate, tolerance = 0.0001)
  
  # 10 years
  value_end_10y <- value_begin * (1 + cagr_rate)^10
  expect_equal(CAGR(value_begin, value_end_10y, 10), cagr_rate, tolerance = 0.0001)
})

test_that("CAGR handles negative growth", {
  # 50% decline over 2 years
  result <- CAGR(100, 50, 2)
  expected <- (0.5)^(1/2) - 1  # Approximately -0.293
  expect_equal(result, expected, tolerance = 0.001)
})

test_that("CAGR handles edge cases", {
  # Single period with growth
  expect_equal(CAGR(100, 150, 1), 0.5)
  
  # Single period with decline
  expect_equal(CAGR(100, 80, 1), -0.2)
})

test_that("CAGR handles fractional periods", {
  # 6 months (0.5 years) with 10% growth
  result <- CAGR(100, 110, 0.5)
  expected <- (1.1)^(1/0.5) - 1  # Should be about 21%
  expect_equal(result, expected, tolerance = 0.001)
})

test_that("CAGR input validation", {
  # Test with zero beginning value (should give Inf or error)
  expect_true(is.infinite(CAGR(0, 100, 1)))
})
