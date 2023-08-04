library(testthat)
library(surveytoolbox)
library(haven)

test_that("box_it returns a labelled double object", {
  # Generate some test data
  x <- sample(1:10, 100, replace = TRUE)
  
  # Call the function
  result <- box_it(x)
  
  # Check if the result is a labelled double object
  expect_true(is.labelled(result))
  expect_true(is.double(result))
})