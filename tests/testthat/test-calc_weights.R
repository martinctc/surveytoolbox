test_that("calc_weights calculates weights correctly", {
  # Create test data with known proportions
  set.seed(123)
  df <- tibble::tibble(cat = rep(c("A", "B", "C"), c(50, 30, 20)))
  
  # Target equal weights (1/3 each)
  target <- c("A" = 1/3, "B" = 1/3, "C" = 1/3)
  
  result <- calc_weights(df, "cat", target)
  
  # Check that weight column was added

  expect_true("weight" %in% names(result))
  
  # Check weights are calculated correctly
  # A: actual 50%, target 33.3% -> weight = 0.333/0.5 = 0.667
  # B: actual 30%, target 33.3% -> weight = 0.333/0.3 = 1.111
  # C: actual 20%, target 33.3% -> weight = 0.333/0.2 = 1.667
  a_weight <- result$weight[result$cat == "A"][1]
  b_weight <- result$weight[result$cat == "B"][1]
  c_weight <- result$weight[result$cat == "C"][1]
  
  expect_equal(a_weight, (1/3) / 0.5, tolerance = 0.01)
  expect_equal(b_weight, (1/3) / 0.3, tolerance = 0.01)
  expect_equal(c_weight, (1/3) / 0.2, tolerance = 0.01)
})

test_that("calc_weights errors on NA values in weight_var", {
  df <- tibble::tibble(cat = c("A", "B", NA, "C"))
  target <- c("A" = 0.25, "B" = 0.25, "C" = 0.5)
  
  expect_error(
    calc_weights(df, "cat", target),
    "contains missing values"
  )
})

test_that("calc_weights respects custom weight_label", {
  df <- tibble::tibble(cat = rep(c("A", "B"), c(50, 50)))
  target <- c("A" = 0.5, "B" = 0.5)
  
  result <- calc_weights(df, "cat", target, weight_label = "custom_weight")
  
  expect_true("custom_weight" %in% names(result))
  expect_false("weight" %in% names(result))
})

test_that("calc_weights preserves original data", {
  df <- tibble::tibble(
    id = 1:100,
    cat = rep(c("A", "B"), 50),
    value = rnorm(100)
  )
  target <- c("A" = 0.5, "B" = 0.5)
  
  result <- calc_weights(df, "cat", target)
  
  # Check original columns preserved
  expect_true(all(c("id", "cat", "value") %in% names(result)))
  expect_equal(result$id, df$id)
  expect_equal(result$value, df$value)
})
