test_that("likert_convert scales 5-point to 10-point correctly", {
  # 5-point scale (1-5) to 10-point scale (1-10)
  expect_equal(likert_convert(1, 5, 1, 10, 1), 1)   # Min to min
  expect_equal(likert_convert(5, 5, 1, 10, 1), 10)  # Max to max
  expect_equal(likert_convert(3, 5, 1, 10, 1), 5.5) # Mid to mid
})

test_that("likert_convert scales 10-point to 5-point correctly", {
  # 10-point scale (1-10) to 5-point scale (1-5)
  expect_equal(likert_convert(1, 10, 1, 5, 1), 1)   # Min to min
  expect_equal(likert_convert(10, 10, 1, 5, 1), 5)  # Max to max
  expect_equal(likert_convert(5.5, 10, 1, 5, 1), 3) # Mid to mid
})

test_that("likert_convert handles different scale ranges", {
  # 0-10 scale to 1-100 scale
  expect_equal(likert_convert(0, 10, 0, 100, 0), 0)
  expect_equal(likert_convert(10, 10, 0, 100, 0), 100)
  expect_equal(likert_convert(5, 10, 0, 100, 0), 50)
})

test_that("likert_convert works with vectors", {
  input <- c(1, 3, 5)
  result <- likert_convert(input, 5, 1, 10, 1)
  expected <- c(1, 5.5, 10)
  expect_equal(result, expected)
})

test_that("likert_convert handles edge values", {
  # Test values at the boundaries
  expect_equal(likert_convert(1, 5, 1, 10, 1), 1)
  expect_equal(likert_convert(5, 5, 1, 10, 1), 10)
  
  # Test intermediate values
  expect_equal(likert_convert(2, 5, 1, 10, 1), 3.25)
  expect_equal(likert_convert(4, 5, 1, 10, 1), 7.75)
})

test_that("likert_convert preserves NA values", {
  input <- c(1, NA, 5)
  result <- likert_convert(input, 5, 1, 10, 1)
  expect_equal(result[1], 1)
  expect_true(is.na(result[2]))
  expect_equal(result[3], 10)
})
