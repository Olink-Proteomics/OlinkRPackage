test_that("capturing print works", {
  expect_equal(
    print_and_capture(c("test")),
    "[1] \"test\"")
})
