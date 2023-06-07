test_that("Olink color palette", {
  expect_equal(olink_pal()(n = 5), c("#00C7E1FF", "#FE1F04FF", "#00559EFF", "#FFC700FF", "#077183FF"))
  expect_equal(
    olink_pal(coloroption = c("teal", "pink"))(n = 2),
    structure(c("#077183FF", "#FF51B8FF"), .Dim = 2:1)
  )

  expect_equal(
    olink_pal(alpha = 0.5)(n = 2),
    c("#00C7E17F", "#FE1F047F")
  )

  expect_error(olink_pal(alpha = 2)(n = 2))
})
