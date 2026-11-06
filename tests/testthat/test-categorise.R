test_that("categorise creates categories correctly", {
  # Test basic categorization
  x <- 1:10
  result <- categorise(x, breaks = c(0, 3, 7, 10))
  
  expect_type(result, "character")
  expect_length(result, 10)
  
  # First 3 values should be in [0,3]
  expect_true(all(result[1:3] == "[0,3]"))
  
  # Values 4-7 should be in (3,7]
  expect_true(all(result[4:7] == "(3,7]"))
  
  # Values 8-10 should be in (7,10]
  expect_true(all(result[8:10] == "(7,10]"))
})

test_that("categorise handles edge cases", {
  # Single category
  x <- c(1, 2, 3)
  result <- categorise(x, breaks = c(0, 10))
  expect_true(all(result == "[0,10]"))
  
  # Values at exact breakpoints
  x <- c(0, 5, 10)
  result <- categorise(x, breaks = c(0, 5, 10))
  expect_equal(result[1], "[0,5]")  # include.lowest = TRUE
  expect_equal(result[2], "[0,5]")  # right = TRUE, so 5 goes into [0,5]
  expect_equal(result[3], "(5,10]")
})

test_that("categorise handles NA values", {
  x <- c(1, NA, 5, NA, 10)
  result <- categorise(x, breaks = c(0, 5, 10))
  
  expect_true(is.na(result[2]))
  expect_true(is.na(result[4]))
  expect_false(is.na(result[1]))
  expect_false(is.na(result[3]))
})

test_that("categorise works with decimal breaks", {
  x <- c(0.5, 1.5, 2.5, 3.5)
  result <- categorise(x, breaks = c(0, 1, 2, 3, 4))
  
  expect_type(result, "character")
  expect_length(result, 4)
})

test_that("categorise returns character vector as documented", {
  x <- seq(10)
  result <- categorise(x, breaks = c(0, 3, 10))
  
  # Documentation says it returns character vector
  expect_type(result, "character")
  expect_false(is.factor(result))
})
