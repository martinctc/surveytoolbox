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

test_that("squish limits values correctly", {
  x <- c(-5, 0, 5, 10, 15)
  result <- squish(x, range = c(0, 10))
  expected <- c(0, 0, 5, 10, 10)  # Values outside range are squished to limits
  expect_equal(result, expected)
})

test_that("squish handles different ranges", {
  x <- c(1, 2, 3, 4, 5)
  result <- squish(x, range = c(2, 4))
  expected <- c(2, 2, 3, 4, 4)  # 1->2, 5->4, others unchanged
  expect_equal(result, expected)
})

test_that("squish preserves values within range", {
  x <- c(2, 3, 4)
  result <- squish(x, range = c(1, 5))
  expect_equal(result, x)  # All values within range, no change
})
