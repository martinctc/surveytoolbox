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
  result <- box_it(x, which = "top", number = 2, na_val = 99)
  
  # Check that NA is preserved appropriately
  expect_true(is.na(result[3]) || result[3] == 99)
})

test_that("box_it replaces specified values with NA", {
  x <- c(1, 2, 99, 9, 10)
  result <- box_it(x, which = "top", number = 2, replace_na = 99)
  
  # The 99 should be treated as NA
  expect_true(is.na(result[3]))
})