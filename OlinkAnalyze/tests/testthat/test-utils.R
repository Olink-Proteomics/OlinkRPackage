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
