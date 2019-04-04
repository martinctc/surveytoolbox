context("lookup")

testthat::test_that("lookup returns the right vector",{
  testthat::expect_identical(
    look_up(1:3,data.frame(var=5,return="one")),
    expected = c("integer(0)","integer(0)","integer(0)")
  )
  
  testthat::expect_identical(
    look_up(1:3,tibble(var=5,return="one")),
    expected = c(1,2,3)
  )
})


