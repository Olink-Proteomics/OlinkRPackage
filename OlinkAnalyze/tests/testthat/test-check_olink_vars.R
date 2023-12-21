# Test check_olink_platform ----

test_that(
  "check_olink_platform - unexpected platform",
  {
    expect_no_condition(
      object = check_olink_platform(
        x = "Target 48",
        broader_platform = NULL
      )
    )

    expect_no_condition(
      object = check_olink_platform(
        x = "Explore HT",
        broader_platform = NULL
      )
    )

    expect_no_condition(
      object = check_olink_platform(
        x = "Target 48",
        broader_platform = "qPCR"
      )
    )

    expect_no_condition(
      object = check_olink_platform(
        x = "Explore HT",
        broader_platform = "NGS"
      )
    )
  }
)

test_that(
  "check_olink_platform - unexpected platform",
  {
    # random platform name
    expect_error(
      object = check_olink_platform(
        x = "Not_An_Olink_Platform",
        broader_platform = NULL
      ),
      regexp = "Unexpected Olink platform"
    )

    # qPCR platform in NGS broader platform
    expect_error(
      object = check_olink_platform(
        x = "Target 48",
        broader_platform = "NGS"
      ),
      regexp = "Unexpected Olink platform"
    )

    # qPCR platform in NGS broader platform
    expect_error(
      object = check_olink_platform(
        x = "Explore HT",
        broader_platform = "qPCR"
      ),
      regexp = "Unexpected Olink platform"
    )
  }
)

test_that(
  "check_olink_platform - unexpected broader platform",
  {
    # random platform name
    expect_error(
      object = check_olink_platform(
        x = "Target 48",
        broader_platform = "Not_An_Olink_Platform"
      ),
      regexp = "Unexpected Olink broader platform"
    )
  }
)
