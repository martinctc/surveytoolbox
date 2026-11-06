# Tests for data_dict function
test_that("data_dict creates dictionary from labelled data", {
  # Create test data with labels
  df <- tibble::tibble(
    var1 = haven::labelled(c(1, 2, 1, 2), c("No" = 1, "Yes" = 2)),
    var2 = haven::labelled(c(1, 2, 3, 1), c("Low" = 1, "Medium" = 2, "High" = 3))
  )
  attr(df$var1, "label") <- "Question 1"
  attr(df$var2, "label") <- "Question 2"
  
  result <- data_dict(df)
  
  expect_equal(nrow(result), 2)
  expect_true(all(c("var", "label_var", "label_val", "value") %in% names(result)))
  expect_equal(result$var[1], "var1")
  expect_equal(result$label_var[1], "Question 1")
})

test_that("data_dict handles missing labels", {
  # Data without labels
  df <- tibble::tibble(
    var1 = c(1, 2, 3),
    var2 = c("a", "b", "c")
  )
  
  result <- data_dict(df)
  
  expect_equal(nrow(result), 2)
  # Should still work without errors
  expect_true(all(c("var", "label_var") %in% names(result)))
})

# Tests for cor_to_df function
test_that("cor_to_df converts correlation matrix to tibble", {
  # Create correlation matrix
  cor_m <- cor(mtcars[, 1:4])
  
  result <- cor_to_df(cor_m)
  
  expect_true(tibble::is_tibble(result))
  expect_true("cor_matrix" %in% names(result))
  expect_equal(nrow(result), 4)
})

test_that("cor_to_df handles label_table parameter", {
  cor_m <- cor(mtcars[, 1:3])
  
  label_table <- tibble::tibble(
    var_name = c("mpg", "cyl", "disp"),
    label = c("Miles per gallon", "Cylinders", "Displacement")
  )
  
  result <- cor_to_df(cor_m, label_table = label_table, id = "var_name")
  
  expect_true(tibble::is_tibble(result))
  expect_true("label" %in% names(result))
})

test_that("cor_to_df errors on wrong label_table format", {
  cor_m <- cor(mtcars[, 1:3])
  
  # Label table with wrong number of columns
  label_table <- tibble::tibble(
    var_name = c("mpg", "cyl", "disp"),
    label = c("A", "B", "C"),
    extra = c(1, 2, 3)
  )
  
  expect_error(cor_to_df(cor_m, label_table = label_table, id = "var_name"))
})

# Tests for create_freq_dist function
test_that("create_freq_dist creates frequency distribution", {
  result <- create_freq_dist(iris, "Sepal.Length")
  
  expect_true(is.data.frame(result))
  expect_true(all(c("metric", "bin_range", "counts") %in% names(result)))
  expect_equal(result$metric[1], "Sepal.Length")
  expect_true(sum(result$counts) == nrow(iris))
})

test_that("create_freq_dist works with different metrics", {
  result <- create_freq_dist(mtcars, "mpg")
  
  expect_true(is.data.frame(result))
  expect_equal(result$metric[1], "mpg")
})

# Tests for varl_tb function
test_that("varl_tb extracts variable labels", {
  df <- tibble::tibble(
    var1 = c(1, 2, 3),
    var2 = c("a", "b", "c")
  )
  attr(df$var1, "label") <- "Variable 1 Label"
  attr(df$var2, "label") <- "Variable 2 Label"
  
  result <- varl_tb(df)
  
  expect_true(tibble::is_tibble(result))
  expect_equal(nrow(result), 2)
  expect_true(all(c("var", "var_label") %in% names(result)))
  expect_equal(result$var_label[result$var == "var1"], "Variable 1 Label")
})

test_that("varl_tb handles missing labels with 'No label'", {
  df <- tibble::tibble(
    var1 = c(1, 2, 3),
    var2 = c("a", "b", "c")
  )
  # No labels set
  
  result <- varl_tb(df)
  
  expect_true(all(result$var_label == "No label"))
})

# Tests for extract_vallab function
test_that("extract_vallab extracts value labels", {
  df <- tibble::tibble(
    var1 = haven::labelled(c(1, 2, 1), c("No" = 1, "Yes" = 2))
  )
  
  result <- extract_vallab(df, "var1")
  
  expect_true(tibble::is_tibble(result))
  expect_true(all(c("id", "var1") %in% names(result)))
  expect_equal(nrow(result), 2)
})

test_that("extract_vallab returns NULL when no labels", {
  df <- tibble::tibble(
    var1 = c(1, 2, 3)
  )
  
  result <- extract_vallab(df, "var1")
  
  expect_null(result)
})
