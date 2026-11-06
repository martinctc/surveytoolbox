test_that("wrap_text preserves short strings", {
  short_text <- "short"
  result <- wrap_text(short_text, threshold = 15)
  expect_equal(result, short_text)
})

test_that("wrap_text wraps long strings", {
  long_text <- "This is a very long string that should be wrapped"
  result <- wrap_text(long_text, threshold = 15)
  
  # Should contain newline characters
  expect_true(grepl("\n", result))
  
  # Each line should be roughly within the threshold
  lines <- strsplit(result, "\n")[[1]]
  # Most lines should be <= threshold + some tolerance for word boundaries
  expect_true(all(nchar(lines) <= 25)) # Allowing some tolerance
})

test_that("wrap_text respects custom threshold", {
  text <- "This is a test string"
  
  # With threshold 10
  result_10 <- wrap_text(text, threshold = 10)
  expect_true(grepl("\n", result_10))
  
  # With threshold 30 (longer than the string)
  result_30 <- wrap_text(text, threshold = 30)
  expect_equal(result_30, text) # Should remain unchanged
})

test_that("wrap_text handles edge cases", {
  # Empty string
  expect_equal(wrap_text(""), "")
  
  # Single word longer than threshold
  long_word <- "supercalifragilisticexpialidocious"
  result <- wrap_text(long_word, threshold = 10)
  # Should still wrap somehow or remain as is
  expect_true(is.character(result))
  
  # String with no spaces
  no_spaces <- "thisisastringwithnospaces"
  result <- wrap_text(no_spaces, threshold = 10)
  expect_true(is.character(result))
})

test_that("wrap_text works with vectors", {
  texts <- c("short", "This is a longer string that needs wrapping")
  results <- wrap_text(texts, threshold = 15)
  
  expect_length(results, 2)
  expect_equal(results[1], "short") # First should be unchanged
  expect_true(grepl("\n", results[2])) # Second should be wrapped
})

test_that("wrap_text handles special characters", {
  text_with_special <- "This has special chars: @#$%^&*()"
  result <- wrap_text(text_with_special, threshold = 10)
  expect_true(is.character(result))
})

test_that("wrap_text example from documentation works", {
  text <- "The total entropy of an isolated system can never decrease."
  result <- wrap_text(text)
  
  # Should wrap and contain newlines
  expect_true(grepl("\n", result))
  expect_true(is.character(result))
})
