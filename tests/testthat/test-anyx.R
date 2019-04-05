context("any-x")

testthat::test_that("any_x returns NA when it should",{
  testthat::expect_identical(
    any_x(c(NA,NA,NA),value = 1),
    expected = NA
  )
})

