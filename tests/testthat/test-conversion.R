# Tests for as_nps_cat function
test_that("as_nps_cat categorizes scores correctly", {
  x <- 0:10
  result <- as_nps_cat(x)
  
  # Detractors (0-6) should be 1

  expect_true(all(as.numeric(result[1:7]) == 1))
  
  # Passives (7-8) should be 2
  expect_true(all(as.numeric(result[8:9]) == 2))
  
  # Promoters (9-10) should be 3
  expect_true(all(as.numeric(result[10:11]) == 3))
})

test_that("as_nps_cat returns labelled vector", {
  x <- c(0, 7, 9)
  result <- as_nps_cat(x)
  
  expect_true(haven::is.labelled(result))
  
  labels <- attr(result, "labels")
  expect_true("Detractor" %in% names(labels))
  expect_true("Passive" %in% names(labels))
  expect_true("Promoter" %in% names(labels))
})

test_that("as_nps_cat respects custom code parameters", {
  x <- c(0, 7, 9)
  result <- as_nps_cat(x, det = 10, pas = 20, pro = 30)
  
  expect_equal(as.numeric(result[1]), 10)  # Detractor
  expect_equal(as.numeric(result[2]), 20)  # Passive
  expect_equal(as.numeric(result[3]), 30)  # Promoter
})

test_that("as_nps_cat returns NA for out of range values", {
  x <- c(-1, 5, 11)
  result <- as_nps_cat(x)
  
  expect_true(is.na(result[1]))
  expect_false(is.na(result[2]))
  expect_true(is.na(result[3]))
})

test_that("as_nps_cat errors on non-numeric parameters", {
  x <- c(0, 5, 10)
  expect_error(as_nps_cat(x, det = "a"))
})

# Tests for char_to_lab function
test_that("char_to_lab converts character to labelled", {
  x <- c("Yes", "No", "Yes", "No")
  result <- char_to_lab(x)
  
  expect_true(haven::is.labelled(result))
  expect_length(result, 4)
})

test_that("char_to_lab handles NA values", {
  x <- c("Yes", NA, "No")
  result <- char_to_lab(x)
  
  expect_true(is.na(result[2]))
  expect_false(is.na(result[1]))
  expect_false(is.na(result[3]))
})

test_that("char_to_lab creates unique codes for unique values", {
  x <- c("A", "B", "C", "A", "B")
  result <- char_to_lab(x)
  
  # Should have 3 unique labels
  labels <- attr(result, "labels")
  expect_equal(length(labels), 3)
})

# Tests for lab_to_char function
test_that("lab_to_char converts labelled to character", {
  x <- haven::labelled(c(1, 2, 1, 2), c("No" = 1, "Yes" = 2))
  result <- lab_to_char(x)
  
  expect_type(result, "character")
  expect_equal(result, c("No", "Yes", "No", "Yes"))
})

test_that("lab_to_char errors on non-labelled input", {
  x <- c(1, 2, 3)
  expect_error(lab_to_char(x), "not a labelled double")
})

# Tests for recode_vallab function
test_that("recode_vallab changes value labels", {
  x <- haven::labelled(c(1, 2, 3, 2, 1), c("a" = 1, "b" = 2, "d" = 3))
  result <- recode_vallab(x, code = 2, new_label = "Surprise!")
  
  labels <- attr(result, "labels")
  expect_equal(names(labels)[labels == 2], "Surprise!")
  
  # Other labels unchanged
  expect_equal(names(labels)[labels == 1], "a")
  expect_equal(names(labels)[labels == 3], "d")
})

test_that("recode_vallab preserves data values", {
  x <- haven::labelled(c(1, 2, 3), c("a" = 1, "b" = 2, "c" = 3))
  result <- recode_vallab(x, code = 1, new_label = "new_a")
  
  expect_equal(as.numeric(result), c(1, 2, 3))
})

test_that("recode_vallab handles multiple recodes", {
  x <- haven::labelled(c(1, 2, 3), c("a" = 1, "b" = 2, "c" = 3))
  
  result <- recode_vallab(x, code = 1, new_label = "first")
  result <- recode_vallab(result, code = 3, new_label = "third")
  
  labels <- attr(result, "labels")
  expect_equal(names(labels)[labels == 1], "first")
  expect_equal(names(labels)[labels == 3], "third")
})
