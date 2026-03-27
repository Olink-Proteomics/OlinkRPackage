test_that(
  "Olink color palette",
  {
    expect_equal(
      object = olink_pal()(n = 5L),
      expected = c("#00C7E1FF", "#FE1F04FF", "#00559EFF",
                   "#FFC700FF", "#077183FF")
    )

    expect_equal(
      object = olink_pal(
        coloroption = c("teal", "pink")
      )(n = 2L),
      expected = structure(
        c("#077183FF", "#FF51B8FF"),
        .Dim = 2:1
      )
    )

    expect_equal(
      object = olink_pal(alpha = 0.5)(n = 2L),
      expected = c("#00C7E17F", "#FE1F047F")
    )

    expect_error(
      object = olink_pal(alpha = 2L)(n = 2L),
      regexp = "alpha in wrong range"
    )
  }
)
