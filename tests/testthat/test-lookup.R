test_that("look_up basic functionality works", {
  # Create test lookup table
  lookup_table <- data.frame(
    var = c(1, 2, 3),
    return = c("one", "two", "three"),
    stringsAsFactors = FALSE
  )
  
  expect_equal(
    look_up(c(1, 2, 3), lookup_table),
    c("one", "two", "three")
  )
})

test_that("look_up handles missing values", {
  lookup_table <- data.frame(
    var = c(1, 2, 3),
    return = c("one", "two", "three"),
    stringsAsFactors = FALSE
  )
  
  result <- look_up(c(1, NA, 4), lookup_table)
  expect_equal(result[1], "one")
  expect_true(is.na(result[2]))
  expect_equal(result[3], "4") # Should return original value as character when no match
})

test_that("look_up works with tibbles", {
  lookup_table <- tibble::tibble(
    var = c(1, 2, 3),
    return = c("one", "two", "three")
  )
  
  expect_equal(
    look_up(c(1, 2, 3), lookup_table),
    c("one", "two", "three")
  )
})

test_that("look_up handles different column specifications", {
  lookup_table <- data.frame(
    id = c(1, 2, 3),
    value = c("one", "two", "three"),
    stringsAsFactors = FALSE
  )
  
  expect_equal(
    look_up(c(1, 2, 3), lookup_table, index = "id", column = "value"),
    c("one", "two", "three")
  )
})


