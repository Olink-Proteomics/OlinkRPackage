# Test remove_all_na_cols ----

test_that(
  "remove_all_na_cols - works - one NA col",
  {
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
  }
)

test_that(
  "remove_all_na_cols - works - multiple NA cols",
  {
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
  }
)

test_that(
  "remove_all_na_cols - works - no NA cols",
  {
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
  }
)
