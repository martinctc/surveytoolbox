# Tests for superspread function
test_that("superspread creates dummy variables correctly", {
  df <- data.frame(
    id = 1:5,
    cat1 = c("a", "b", "a", "c", "b"),
    stringsAsFactors = FALSE
  )
  
  result <- superspread(df, "cat1")
  
  expect_true(tibble::is_tibble(result))
  # Should have original columns plus dummy columns
  expect_true(all(c("id", "cat1", "a", "b", "c") %in% names(result)))
  
  # Check dummy values are correct (TRUE/FALSE or 1/0)
  expect_equal(sum(result$a), 2)  # "a" appears twice
  expect_equal(sum(result$b), 2)  # "b" appears twice
  expect_equal(sum(result$c), 1)  # "c" appears once
})

test_that("superspread handles multiple categorical columns", {
  df <- data.frame(
    id = 1:4,
    cat1 = c("a", "b", "a", "b"),
    cat2 = c("x", "x", "y", "y"),
    stringsAsFactors = FALSE
  )
  
  result <- superspread(df, c("cat1", "cat2"))
  
  expect_true(tibble::is_tibble(result))
  # Should have dummies for all unique values across both columns
  expect_true(all(c("a", "b", "x", "y") %in% names(result)))
})

test_that("superspread works with iris Species", {
  result <- superspread(iris, "Species")
  
  expect_true(tibble::is_tibble(result))
  expect_true(all(c("setosa", "versicolor", "virginica") %in% names(result)))
  
  # Each dummy should sum to 50 (50 of each species)
  expect_equal(sum(result$setosa), 50)
  expect_equal(sum(result$versicolor), 50)
  expect_equal(sum(result$virginica), 50)
})

# Tests for superspread_count function
test_that("superspread_count counts occurrences correctly", {
  df <- data.frame(
    id = 1:3,
    cat1 = c("a", "b", "a"),
    cat2 = c("a", "a", "b"),
    stringsAsFactors = FALSE
  )
  
  result <- superspread_count(df, c("cat1", "cat2"))
  
  expect_true(tibble::is_tibble(result))
  
  # Row 1: cat1="a", cat2="a" -> a should be 2
  expect_equal(result$a[1], 2)
  expect_equal(result$b[1], 0)
  
  # Row 2: cat1="b", cat2="a" -> a=1, b=1
  expect_equal(result$a[2], 1)
  expect_equal(result$b[2], 1)
  
  # Row 3: cat1="a", cat2="b" -> a=1, b=1
  expect_equal(result$a[3], 1)
  expect_equal(result$b[3], 1)
})

test_that("superspread_count preserves original columns", {
  df <- data.frame(
    id = 1:4,
    value = c(10, 20, 30, 40),
    cat = c("a", "b", "a", "b"),
    stringsAsFactors = FALSE
  )
  
  result <- superspread_count(df, "cat")
  
  expect_true("id" %in% names(result))
  expect_true("value" %in% names(result))
  expect_equal(result$id, df$id)
  expect_equal(result$value, df$value)
})

# Tests for superspread_fill function
test_that("superspread_fill fills values correctly", {
  df <- data.frame(
    a = c(1, 2, 4, 5, 6, 7, 8),
    b = c(244, 333, 434, 453, 123, 123, 435)
  )
  
  result <- superspread_fill(df, 8, "a", "b")
  
  # Should create 8 columns numbered 1-8
  expect_true(all(as.character(1:8) %in% names(result)))
  
  # Column 1 should have value from row where a=1 (which is b=244)
  expect_equal(result$`1`[1], 244)
  
  # Column 2 should have value from row where a=2
  expect_equal(result$`2`[2], 333)
})

test_that("superspread_fill handles missing values", {
  df <- data.frame(
    a = c(1, 3),  # Missing 2
    b = c(100, 300)
  )
  
  result <- superspread_fill(df, 4, "a", "b")
  
  # Should still have 4 columns
  expect_true(all(as.character(1:4) %in% names(result)))
})
