# Test check_olink_platform ----

test_that(
  "check_olink_platform - works",
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

# Test check_olink_data_type ----

test_that(
  "check_olink_data_type - works",
  {
    expect_no_condition(
      object = check_olink_data_type(x = "Ct",
                                     broader_platform = "qPCR")
    )

    expect_no_condition(
      object = check_olink_data_type(x = "NPX",
                                     broader_platform = "qPCR")
    )

    expect_no_condition(
      object = check_olink_data_type(x = "Quantified",
                                     broader_platform = "qPCR")
    )

    expect_no_condition(
      object = check_olink_data_type(x = "NPX",
                                     broader_platform = "NGS")
    )
  }
)

test_that(
  "check_olink_data_type - unexpected data_type",
  {
    expect_error(
      object = check_olink_data_type(x = "Wrong_Data_Type",
                                     broader_platform = NULL),
      regexp = "Unexpected Olink data type"
    )

    expect_error(
      object = check_olink_data_type(x = "Wrong_Data_Type",
                                     broader_platform = "qPCR"),
      regexp = "Unexpected Olink data type"
    )

    expect_error(
      object = check_olink_data_type(x = "Wrong_Data_Type",
                                     broader_platform = "NGS"),
      regexp = "Unexpected Olink data type"
    )

    expect_error(
      object = check_olink_data_type(x = "Ct",
                                     broader_platform = "NGS"),
      regexp = "Unexpected Olink data type"
    )

    expect_error(
      object = check_olink_data_type(x = "Quantified",
                                     broader_platform = "NGS"),
      regexp = "Unexpected Olink data type"
    )
  }
)

# Test check_olink_broader_platform ----

test_that(
  "check_olink_broader_platform - works",
  {
    expect_no_condition(
      object = check_olink_broader_platform(
        x = "qPCR"
      )
    )

    expect_no_condition(
      object = check_olink_broader_platform(
        x = "NGS"
      )
    )
  }
)

test_that(
  "check_olink_broader_platform - unexpected broader platform",
  {
    # random platform name
    expect_error(
      object = check_olink_broader_platform(
        x = "Not_An_Olink_Platform"
      ),
      regexp = "Unexpected Olink broader platform"
    )
  }
)
