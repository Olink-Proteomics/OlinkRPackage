# Test check_olink_vars ----

## Test check_olink_platform ----

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

## Test check_olink_data_type ----

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

## Test check_olink_broader_platform ----

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

# Test remove_all_na_cols ----

test_that(
  "remove_all_na_cols - works - one NA col",
  {
    ## tibble ----

    df <- dplyr::tibble(
      a = c(1L, 2L),
      b = c("a", "b"),
      c = rep(x = NA_character_, times = 2L)
    )

    expect_no_condition(
      object = df_no_na <- remove_all_na_cols(df = df)
    )

    expect_identical(
      object = df_no_na,
      expected = dplyr::select(df, -dplyr::all_of(c("c")))
    )

    ## arrow ----

    df_arrow <- arrow::as_arrow_table(df)

    expect_no_condition(
      object = df_no_na_arrow <- remove_all_na_cols(df = df_arrow)
    )

    expect_identical(
      object = df_no_na_arrow |>
        dplyr::collect(),
      expected = df_arrow |>
        dplyr::select(-dplyr::all_of(c("c"))) |>
        dplyr::collect()
    )

    expect_identical(
      object = df_no_na_arrow |>
        dplyr::collect(),
      expected = df_no_na
    )
  }
)

test_that(
  "remove_all_na_cols - works - multiple NA cols",
  {
    ## tibble ----

    df <- dplyr::tibble(
      a = c(1L, 2L),
      b = c("a", "b"),
      c = rep(x = NA_character_, times = 2L),
      d = rep(x = NA_character_, times = 2L),
      e = rep(x = NA_character_, times = 2L)
    )

    expect_no_condition(
      object = df_no_na <- remove_all_na_cols(df = df)
    )

    expect_identical(
      object = df_no_na,
      expected = dplyr::select(df, -dplyr::all_of(c("c", "d", "e")))
    )

    ## arrow ----

    df_arrow <- arrow::as_arrow_table(df)

    expect_no_condition(
      object = df_no_na_arrow <- remove_all_na_cols(df = df_arrow)
    )

    expect_identical(
      object = df_no_na_arrow |>
        dplyr::collect(),
      expected = df_arrow |>
        dplyr::select(-dplyr::all_of(c("c", "d", "e"))) |>
        dplyr::collect()
    )

    expect_identical(
      object = df_no_na_arrow |>
        dplyr::collect(),
      expected = df_no_na
    )
  }
)

test_that(
  "remove_all_na_cols - works - no NA cols",
  {
    ## tibble ----

    df <- dplyr::tibble(
      a = c(1L, 2L),
      b = c("a", "b")
    )

    expect_no_condition(
      object = df_no_na <- remove_all_na_cols(df = df)
    )

    expect_identical(
      object = df_no_na,
      expected = df
    )

    ## arrow ----

    df_arrow <- arrow::as_arrow_table(df)

    expect_no_condition(
      object = df_no_na_arrow <- remove_all_na_cols(df = df_arrow)
    )

    expect_identical(
      object = df_no_na_arrow |>
        dplyr::collect(),
      expected = df_arrow |>
        dplyr::collect()
    )

    expect_identical(
      object = df_no_na_arrow |>
        dplyr::collect(),
      expected = df_no_na
    )

  }
)
