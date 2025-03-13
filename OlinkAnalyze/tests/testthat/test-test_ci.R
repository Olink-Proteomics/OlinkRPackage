testthat::test_that("Skip Message",{
  x = 3
  skip_if(x != 2, "Test is skipped.")
  testthat::expect_equal(2,2)
})

