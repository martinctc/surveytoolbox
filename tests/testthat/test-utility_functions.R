test_that("likert_reverse reverses scale correctly", {
  # Test basic reversal of 5-point scale
  x <- c(1, 2, 3, 4, 5)
  result <- likert_reverse(x, 5, 1)
  expected <- c(5, 4, 3, 2, 1)
  expect_equal(result, expected)
})

test_that("likert_reverse handles different scales", {
  # Test 7-point scale
  x <- c(1, 4, 7)
  result <- likert_reverse(x, 7, 1)
  expected <- c(7, 4, 1)  # Midpoint stays same, extremes flip
  expect_equal(result, expected)
  
  # Test 10-point scale (0-10)
  x <- c(0, 5, 10)
  result <- likert_reverse(x, 10, 0)
  expected <- c(10, 5, 0)
  expect_equal(result, expected)
})

test_that("likert_reverse handles NA values", {
  x <- c(1, NA, 5)
  result <- likert_reverse(x, 5, 1)
  expect_equal(result[1], 5)
  expect_true(is.na(result[2]))
  expect_equal(result[3], 1)
})

test_that("squish returns single value when all identical", {
  x <- c(1, 1, 1, 1)
  result <- squish(x)
  expect_equal(result, 1)
  expect_length(result, 1)
})

test_that("squish throws error when values differ", {
  x <- c(1, 2, 3)
  expect_error(squish(x), "More than one unique value")
})

test_that("squish works with character vectors", {
  x <- c("A", "A", "A")
  result <- squish(x)
  expect_equal(result, "A")
  expect_length(result, 1)
})
