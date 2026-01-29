test_that("as_percent converts numbers to percentage strings", {
  expect_equal(as_percent(0.5), "50%")
  expect_equal(as_percent(0.25), "25%")
  expect_equal(as_percent(1), "100%")
  expect_equal(as_percent(0), "0%")
})

test_that("as_percent handles rounding correctly", {
  expect_equal(as_percent(0.867, rounding = 0), "87%")
  expect_equal(as_percent(0.867, rounding = 1), "86.7%")
  expect_equal(as_percent(0.867, rounding = 2), "86.7%")  # R doesn't add trailing zeros by default
})

test_that("as_percent works with vectors", {
  result <- as_percent(c(0.1, 0.5, 0.9))
  expected <- c("10%", "50%", "90%")
  expect_equal(result, expected)
})

test_that("as_percent handles edge cases", {
  expect_equal(as_percent(1.5), "150%")  # Greater than 100%
  expect_equal(as_percent(-0.1), "-10%")  # Negative values
})

test_that("as_percent handles NA values", {
  result <- as_percent(c(0.5, NA, 0.7))
  expect_equal(result[1], "50%")
  expect_equal(result[2], "NA%")  # as_percent converts NA to "NA%"
  expect_equal(result[3], "70%")
})
