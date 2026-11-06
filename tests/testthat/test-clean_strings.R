test_that("clean_strings removes special characters", {
  expect_equal(clean_strings("Q23. Brand Awareness"), "q23_brand_awareness")
  expect_equal(clean_strings("Respondent ID"), "respondent_id")
})

test_that("clean_strings handles quotes and percent signs", {
  expect_equal(clean_strings("'quoted'"), "quoted")
  expect_equal(clean_strings('"double quoted"'), "double_quoted")
  expect_equal(clean_strings("50% satisfaction"), "x50percent_satisfaction")  # make.names adds X prefix
})

test_that("clean_strings removes leading/trailing spaces and dots", {
  expect_equal(clean_strings("  spaced  "), "spaced")
  expect_equal(clean_strings("dotted.variable."), "dotted_variable")
  expect_equal(clean_strings("multiple...dots"), "multiple_dots")
})

test_that("clean_strings handles duplicates when treat_dups=TRUE", {
  input <- c("Variable", "Variable", "Variable")
  result <- clean_strings(input, treat_dups = TRUE)
  expected <- c("variable", "variable_2", "variable_3")
  expect_equal(result, expected)
})

test_that("clean_strings preserves duplicates when treat_dups=FALSE", {
  input <- c("Variable", "Variable", "Variable")
  result <- clean_strings(input, treat_dups = FALSE)
  expected <- c("variable", "variable", "variable")
  expect_equal(result, expected)
})

test_that("clean_strings handles mixed case and numbers", {
  expect_equal(clean_strings("Q1_Brand123"), "q1_brand123")
  expect_equal(clean_strings("CamelCase"), "camelcase")
})

test_that("clean_strings handles empty strings", {
  expect_equal(clean_strings(""), "x")  # make.names converts empty string to "X"
  expect_equal(clean_strings(c("test", "", "another")), c("test", "x", "another"))
})
