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

# Test ansi_collapse_quot ----

test_that(
  "ansi_collapse_quot - works",
  {
    expect_equal(
      object = ansi_collapse_quot(
        x = c("A", "B"),
        sep = "and"
      ),
      expected = "\"A\" and \"B\""
    )

    expect_equal(
      object = ansi_collapse_quot(
        x = c("A", "B"),
        sep = "or"
      ),
      expected = "\"A\" or \"B\""
    )

    expect_equal(
      object = ansi_collapse_quot(
        x = c("A", "B", "C")
      ),
      expected = "\"A\", \"B\", and \"C\""
    )

    expect_equal(
      object = ansi_collapse_quot(
        x = c("A", "B", "C"),
        sep = "and"
      ),
      expected = "\"A\", \"B\", and \"C\""
    )

    expect_equal(
      object = ansi_collapse_quot(
        x = c("A", "B", "C"),
        sep = "something"
      ),
      expected = "\"A\", \"B\", and \"C\""
    )

    expect_equal(
      object = ansi_collapse_quot(
        x = c("A", "B", "C"),
        sep = "or"
      ),
      expected = "\"A\", \"B\", or \"C\""
    )
  }
)

# Test check_osi ----

test_that(
  "check_osi - works",
  {
    osi_data <- get_example_data("example_osi_data.rds")

    osi_check_log <- check_npx(osi_data) |>
      suppressWarnings() |>
      suppressMessages()

    # OSISUmmary ----

    expect_no_condition(
      object = osi_summary <- check_osi(
        df = osi_data,
        check_log = osi_check_log,
        osi_score = "OSISummary"
      )
    )

    expect_equal(
      object = osi_summary,
      expected = osi_data
    )

    # OSIPreparationTemperature ----

    expect_no_condition(
      object = osi_prep_temp <- check_osi(
        df = osi_data,
        check_log = osi_check_log,
        osi_score = "OSIPreparationTemperature"
      )
    )

    expect_equal(
      object = osi_prep_temp,
      expected = osi_data
    )

    # OSITimeToCentrifugation ----

    expect_no_condition(
      object = osi_time <- check_osi(
        df = osi_data,
        check_log = osi_check_log,
        osi_score = "OSITimeToCentrifugation"
      )
    )

    expect_equal(
      object = osi_time,
      expected = osi_data
    )

    # OSICategory ----

    expect_no_condition(
      object = osi_cat <- check_osi(
        df = osi_data,
        check_log = osi_check_log,
        osi_score = "OSICategory"
      )
    )

    expect_equal(
      object = osi_cat |>
        dplyr::mutate(
          OSICategory = as.character(OSICategory) |> as.numeric()
        ),
      expected = osi_data
    )
  }
)

test_that(
  "check_osi - errors",
  {
    osi_data <- get_example_data("example_osi_data.rds")

    osi_check_log <- check_npx(osi_data) |>
      suppressWarnings() |>
      suppressMessages()

    # check inputs ----

    expect_error(
      object = check_osi(),
      regexp = "Missing required argument `df`!",
      fixed = TRUE
    )

    expect_error(
      object = check_osi(osi_score = "OSISummary"),
      regexp = "Missing required argument `df`!",
      fixed = TRUE
    )

    # missing osi_score argument ----

    expect_error(
      object = check_osi(
        df = osi_data,
        check_log = osi_check_log
      ),
      regexp = "`osi_score` must be a scalar character!"
    )

    # invalid osi_score argument ----

    expect_error(
      object = check_osi(
        df = osi_data,
        check_log = osi_check_log,
        osi_score = "not a real score"
      ),
      regexp = "Invalid value for `osi_score` = \"not a real score\"!"
    )

    # missing OSI column ----

    expect_error(
      object = check_osi(
        df = osi_data |>
          dplyr::select(
            -dplyr::all_of("OSISummary")
          ),
        check_log = osi_check_log,
        osi_score = "OSISummary"
      ),
      regexp = "\`df` is missing the required columns: \"OSISummary\"!"
    )

    # All values in OSI column are NA ----

    expect_error(
      object = check_osi(
        df = osi_data |>
          dplyr::mutate(
            OSIPreparationTemperature = NA
          ),
        check_log = osi_check_log,
        osi_score = "OSIPreparationTemperature"
      ),
      regexp = paste("All values are 'NA' in the column",
                     "\"OSIPreparationTemperature\""),
      fixed = TRUE
    )

    # invalid value is OSICategory ----

    expect_error(
      object = check_osi(
        df = osi_data |>
          dplyr::mutate(
            OSICategory = dplyr::if_else(
              .data[["SampleID"]] == "A1",
              -1,
              .data[["OSICategory"]]
            )
          ),
        check_log = osi_check_log,
        osi_score = "OSICategory"
      ),
      regexp = "Invalid values detected in column \"OSICategory\" of `df`!"
    )

    # invalid value is OSISummary - character ----

    expect_error(
      object = check_osi(
        df = osi_data |>
          dplyr::mutate(
            OSISummary = dplyr::if_else(
              .data[["SampleID"]] == "A1",
              "Invalid",
              as.character(.data[["OSISummary"]])
            )
          ),
        check_log = osi_check_log,
        osi_score = "OSISummary"
      ),
      regexp = "Non-numeric values detected in column \"OSISummary\" of `df`!"
    )

    # invalid value is OSISummary - out of range ----

    expect_error(
      object = check_osi(
        df = osi_data |>
          dplyr::mutate(
            OSISummary = dplyr::if_else(
              .data[["SampleID"]] == "A1",
              .data[["OSISummary"]] + 2L,
              .data[["OSISummary"]]
            )
          ),
        check_log = osi_check_log,
        osi_score = "OSISummary"
      ),
      regexp = "Out of range values detected in column \"OSISummary\" of `df`!"
    )
  }
)
