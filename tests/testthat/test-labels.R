test_that("set_varl sets variable labels correctly", {
  x <- c(1, 2, 3)
  result <- set_varl(x, "Test variable")
  
  expect_equal(attr(result, "label"), "Test variable")
  expect_equal(as.numeric(result), c(1, 2, 3))
})

test_that("set_varl preserves data", {
  x <- c(1, 2, NA, 4)
  result <- set_varl(x, "Variable with NA")
  
  # Check that the underlying data is preserved (ignoring attributes)
  expect_equal(as.numeric(result), as.numeric(x))
  expect_equal(attr(result, "label"), "Variable with NA")
})

test_that("set_varl works with different data types", {
  # Numeric
  num_var <- set_varl(c(1.5, 2.5), "Numeric variable")
  expect_equal(attr(num_var, "label"), "Numeric variable")
  
  # Character
  char_var <- set_varl(c("a", "b"), "Character variable")
  expect_equal(attr(char_var, "label"), "Character variable")
  
  # Logical
  log_var <- set_varl(c(TRUE, FALSE), "Logical variable")
  expect_equal(attr(log_var, "label"), "Logical variable")
})

test_that("set_vall sets value labels correctly", {
  x <- c(1, 2, 1, 2)
  labels <- c("No" = 1, "Yes" = 2)
  result <- set_vall(x, labels)
  
  expect_true(haven::is.labelled(result))
  result_labels <- attr(result, "labels")
  expect_equal(result_labels, labels)
})

test_that("set_vall preserves variable labels", {
  x <- c(1, 2, 1, 2)
  x <- set_varl(x, "Original variable label")
  
  value_labels <- c("No" = 1, "Yes" = 2)
  result <- set_vall(x, value_labels)
  
  expect_equal(attr(result, "label"), "Original variable label")
  expect_true(haven::is.labelled(result))
})

test_that("set_vall works without existing variable label", {
  x <- c(0, 1, 0, 1)
  value_labels <- c("No" = 0, "Yes" = 1)
  result <- set_vall(x, value_labels)
  
  expect_true(haven::is.labelled(result))
  result_labels <- attr(result, "labels")
  expect_equal(result_labels, value_labels)
})

test_that("set_vall handles complex scenarios", {
  # Create a variable with both variable and value labels
  x <- c(1, 2, 3, 1, 2)
  x <- set_varl(x, "Satisfaction Level")
  
  value_labels <- c("Dissatisfied" = 1, "Neutral" = 2, "Satisfied" = 3)
  result <- set_vall(x, value_labels)
  
  expect_equal(attr(result, "label"), "Satisfaction Level")
  expect_true(haven::is.labelled(result))
  expect_equal(attr(result, "labels"), value_labels)
})
