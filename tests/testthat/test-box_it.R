test_that("box_it returns a labelled double object", {
  # Generate some test data
  x <- sample(1:10, 100, replace = TRUE)
  
  # Call the function
  result <- box_it(x)
  
  # Check if the result is a labelled double object
  expect_true(haven::is.labelled(result))
  expect_true(is.double(result))
})

test_that("box_it correctly identifies top values", {
  x <- c(1, 2, 3, 8, 9, 10)
  result <- box_it(x, which = "top", number = 2)
  
  # Values 9 and 10 should be 1 (selected), others should be 0
  expected_values <- c(0, 0, 0, 0, 1, 1)
  expect_equal(as.numeric(result), expected_values)
})

test_that("box_it correctly identifies bottom values", {
  x <- c(1, 2, 3, 8, 9, 10)
  result <- box_it(x, which = "bottom", number = 2)
  
  # Values 1 and 2 should be 1 (selected), others should be 0
  expected_values <- c(1, 1, 0, 0, 0, 0)
  expect_equal(as.numeric(result), expected_values)
})

test_that("box_it handles NA values correctly", {
  x <- c(1, 2, NA, 9, 10)

  # Missing values use na_val when it is supplied (the default is 99)
  result <- box_it(x, which = "top", number = 2, na_val = 99)
  expect_equal(as.numeric(result[3]), 99)

  # Missing values remain NA when na_val is NULL
  result_na <- box_it(x, which = "top", number = 2, na_val = NULL)
  expect_true(is.na(result_na[3]))
})

test_that("box_it replaces specified values with NA", {
  x <- c(1, 2, 99, 9, 10)
  result <- box_it(
    x,
    which = "top",
    number = 2,
    replace_na = 99,
    na_val = NULL
  )

  # The 99 should be treated as missing
  expect_true(is.na(result[3]))
})
