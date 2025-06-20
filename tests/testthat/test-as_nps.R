test_that("as_nps converts scores correctly", {
  # Test detractors (0-6)
  detractors <- 0:6
  result_det <- as_nps(detractors)
  expect_true(all(as.numeric(result_det) == -100))
  
  # Test passives (7-8)
  passives <- 7:8
  result_pas <- as_nps(passives)
  expect_true(all(as.numeric(result_pas) == 0))
  
  # Test promoters (9-10)
  promoters <- 9:10
  result_pro <- as_nps(promoters)
  expect_true(all(as.numeric(result_pro) == 100))
})

test_that("as_nps returns labelled vector", {
  scores <- c(0, 7, 9, 10)
  result <- as_nps(scores)
  
  expect_true(haven::is.labelled(result))
  
  # Check labels exist
  labels <- attr(result, "labels")
  expect_true("Detractor" %in% names(labels))
  expect_true("Passive" %in% names(labels))
  expect_true("Promoter" %in% names(labels))
})

test_that("as_nps handles NA values", {
  scores <- c(0, NA, 7, 9)
  result <- as_nps(scores)
  
  expect_true(is.na(result[2]))
  expect_equal(as.numeric(result[1]), -100) # Detractor
  expect_equal(as.numeric(result[3]), 0)    # Passive
  expect_equal(as.numeric(result[4]), 100)  # Promoter
})

test_that("as_nps validates input range", {
  # Should error for values outside 0-10
  expect_error(as_nps(c(0, 5, 11)), "Values out of bounds")
  expect_error(as_nps(c(-1, 5, 10)), "Values out of bounds")
  expect_error(as_nps(c(0.5, 5.5, 10.5)), "Values out of bounds")
})

test_that("as_nps NPS calculation works", {
  # Example: 2 detractors, 1 passive, 2 promoters = (2-2)/5 * 100 = 0
  scores <- c(0, 6, 7, 9, 10)  # 2 detractors, 1 passive, 2 promoters
  result <- as_nps(scores)
  nps_score <- mean(result, na.rm = TRUE)
  expect_equal(nps_score, 0) # (200 + 0 - 200) / 5 = 0
})

test_that("as_nps edge cases", {
  # All detractors
  all_det <- as_nps(c(0, 1, 2))
  expect_equal(mean(all_det), -100)
  
  # All promoters  
  all_pro <- as_nps(c(9, 10, 10))
  expect_equal(mean(all_pro), 100)
  
  # All passives
  all_pas <- as_nps(c(7, 8, 7))
  expect_equal(mean(all_pas), 0)
})
