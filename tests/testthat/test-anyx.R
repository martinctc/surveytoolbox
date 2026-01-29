test_that("any_x returns TRUE when value exists", {
  expect_true(any_x(c(1, 0, 1), 1))
  expect_true(any_x(c(1, NA, 1), 1))
  expect_true(any_x(c(0, 1, 2, 3), 1))
})

test_that("any_x returns FALSE when value doesn't exist", {
  expect_false(any_x(c(0, 0, NA), 1))
  expect_false(any_x(c(2, 3, 4), 1))
  expect_false(any_x(c(0, 0, 0), 1))
})

test_that("any_x returns NA when all values are NA", {
  expect_identical(any_x(c(NA, NA, NA), 1), NA)
  expect_identical(any_x(c(NA), 1), NA)
})

test_that("any_x handles multiple values to search for", {
  expect_true(any_x(c(1, 2, 3), c(1, 5)))
  expect_true(any_x(c(1, 2, 3), c(2, 5)))
  expect_false(any_x(c(1, 2, 3), c(4, 5)))
})

test_that("any_x handles edge cases", {
  expect_true(is.na(any_x(c(), 1)))  # Empty vector returns NA
  expect_true(any_x(c(1), 1))
  expect_false(any_x(c(1), 2))
})

