# Tests for test_chisq function
test_that("test_chisq returns correct structure", {
  result <- test_chisq(mtcars, "cyl", "vs")
  
  expect_true(tibble::is_tibble(result))
  expect_true(all(c("col_x", "col_y", "p") %in% names(result)))
  expect_equal(result$col_x, "cyl")
  expect_equal(result$col_y, "vs")
})

test_that("test_chisq handles NA filtering", {
  df <- mtcars
  df$cyl[1:3] <- NA
  
  result <- test_chisq(df, "cyl", "vs", na_x = NA)
  
  expect_true(tibble::is_tibble(result))
  expect_true(!is.na(result$p))
})

test_that("test_chisq returns significant result for related variables", {
  # cyl and vs are clearly related in mtcars
  result <- test_chisq(mtcars, "cyl", "vs")
  
  # Should be highly significant
  expect_true(result$p < 0.05)
})

# Tests for calc_pc_loglin function
test_that("calc_pc_loglin calculates percentage impact", {
  # Create simple linear model with log transform
  df <- data.frame(
    y = exp(1 + 0.5 * 1:10 + rnorm(10, 0, 0.1)),
    x = 1:10
  )
  model <- lm(log(y) ~ x, data = df)
  
  result <- calc_pc_loglin(model)
  
  expect_true(tibble::is_tibble(result))
  expect_true(all(c("var", "coef", "pc_impact") %in% names(result)))
  expect_equal(nrow(result), 2)  # Intercept + x
})

test_that("calc_pc_loglin percentage impact formula is correct", {
  # Create model with known coefficient
  df <- data.frame(y = 1:10, x = 1:10)
  model <- lm(log(y) ~ x, data = df)
  
  result <- calc_pc_loglin(model)
  
  # pc_impact should be exp(coef) - 1
  expect_equal(
    result$pc_impact,
    exp(result$coef) - 1,
    tolerance = 0.0001
  )
})

# Tests for run_hclust function
test_that("run_hclust returns hclust object", {
  df <- iris[, 1:4]
  result <- run_hclust(df)
  
  expect_s3_class(result, "hclust")
})
  
test_that("run_hclust respects method parameter", {
  df <- iris[1:20, 1:4]
  
  result_complete <- run_hclust(df, method = "complete")
  result_single <- run_hclust(df, method = "single")
  
  expect_s3_class(result_complete, "hclust")
  expect_s3_class(result_single, "hclust")
  
  # Different methods should give different results
  expect_false(identical(result_complete$height, result_single$height))
})

test_that("run_hclust respects dmeth parameter", {
  df <- iris[1:20, 1:4]
  
  result_euclidean <- run_hclust(df, dmeth = "euclidean")
  result_manhattan <- run_hclust(df, dmeth = "manhattan")
  
  expect_s3_class(result_euclidean, "hclust")
  expect_s3_class(result_manhattan, "hclust")
})

# Tests for ttest_nps function
test_that("ttest_nps returns margin of error", {
  set.seed(123)
  x <- sample(c(-100, 0, 100), 100, replace = TRUE)
  
  # Capture the message and result
  result <- expect_message(ttest_nps(x))
  
  expect_type(result, "double")
  expect_true(result > 0)  # Margin of error should be positive
})

test_that("ttest_nps respects confidence level", {
  set.seed(123)
  x <- sample(c(-100, 0, 100), 100, replace = TRUE)
  
  result_95 <- suppressMessages(ttest_nps(x, conf_level = 0.95))
  result_99 <- suppressMessages(ttest_nps(x, conf_level = 0.99))
  
  # Higher confidence level = wider interval
  expect_true(result_99 > result_95)
})

# Tests for split_tt function
test_that("split_tt splits data correctly", {
  df <- data.frame(x = 1:100, y = 101:200)
  
  result <- split_tt(df, 0.7)
  
  expect_type(result, "list")
  expect_true(all(c("train", "test") %in% names(result)))
  
  # Check proportions are roughly correct
  expect_equal(nrow(result$train), 70)
  expect_equal(nrow(result$test), 30)
  
  # No overlap between train and test
  train_rows <- rownames(result$train)
  test_rows <- rownames(result$test)
  expect_equal(length(intersect(train_rows, test_rows)), 0)
})

test_that("split_tt handles different proportions", {
  df <- data.frame(x = 1:100)
  
  result_80 <- split_tt(df, 0.8)
  result_50 <- split_tt(df, 0.5)
  
  expect_equal(nrow(result_80$train), 80)
  expect_equal(nrow(result_50$train), 50)
})

test_that("split_tt preserves all columns", {
  df <- data.frame(a = 1:10, b = 11:20, c = letters[1:10])
  
  result <- split_tt(df, 0.6)
  
  expect_equal(names(result$train), names(df))
  expect_equal(names(result$test), names(df))
})
