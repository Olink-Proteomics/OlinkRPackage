# Test get_olink_platforms ----

test_that(
  "get_olink_platforms - works",
  {
    uniq_platforms <- accepted_olink_platforms$name |>
      unique() |>
      sort()

    expect_true(
      object = identical(uniq_platforms, get_olink_platforms())
    )
  }
)

# Test get_olink_qpcr_platforms ----

test_that(
  "get_olink_qpcr_platforms - works",
  {
    expect_true(
      object = accepted_olink_platforms |>
        dplyr::filter(
          .data[["broader_platform"]] == "qPCR"
        ) |>
        dplyr::pull(
          .data[["name"]]
        ) |>
        sort() |>
        (\(.) . %in% get_olink_qpcr_platforms())() |>
        all()
    )
  }
)

# Test get_olink_ngs_platforms ----

test_that(
  "get_olink_ngs_platforms - works",
  {
    expect_true(
      object = accepted_olink_platforms |>
        dplyr::filter(
          .data[["broader_platform"]] == "NGS"
        ) |>
        dplyr::pull(
          .data[["name"]]
        ) |>
        sort() |>
        (\(.) . %in% get_olink_ngs_platforms())() |>
        all()
    )
  }
)

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

# Test get_olink_broader_platforms ----

test_that(
  "get_olink_broader_platforms - works",
  {
    broad_plat <- get_olink_broader_platforms()

    expect_true(
      object = all(accepted_olink_platforms$broader_platform %in% broad_plat)
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

# Test get_olink_data_types ----

test_that(
  "get_olink_data_types - works",
  {
    uniq_quant_methods <- accepted_olink_platforms$quant_method |>
      unlist() |>
      unique() |>
      sort()

    expect_true(
      object = identical(uniq_quant_methods, get_olink_data_types())
    )
  }
)

# Test get_olink_qpcr_data_types ----

test_that(
  "get_olink_qpcr_data_types - works",
  {
    expect_true(
      object = accepted_olink_platforms |>
        dplyr::filter(
          .data[["broader_platform"]] == "qPCR"
        ) |>
        dplyr::pull(
          .data[["quant_method"]]
        ) |>
        unlist() |>
        unique() |>
        sort() |>
        (\(.) . %in% get_olink_qpcr_data_types())() |>
        all()
    )
  }
)

# Test get_olink_ngs_data_types ----

test_that(
  "get_olink_ngs_data_types - works",
  {
    expect_true(
      object = accepted_olink_platforms |>
        dplyr::filter(
          .data[["broader_platform"]] == "NGS"
        ) |>
        dplyr::pull(
          .data[["quant_method"]]
        ) |>
        unlist() |>
        unique() |>
        sort() |>
        (\(.) . %in% get_olink_ngs_data_types())() |>
        all()
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
