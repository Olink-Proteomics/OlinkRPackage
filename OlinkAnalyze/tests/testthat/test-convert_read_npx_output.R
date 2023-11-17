test_that(
  "check convert read npx output - df: arrow; out: tibble",
  {
    expect_true(
      object = convert_read_npx_output(
        df = dplyr::tibble(
          "A" = c(1, 2.2, 3.14),
          "B" = c("a", "b", "c"),
          "C" = c(TRUE, TRUE, FALSE),
          "D" = c("NA", "B", NA_character_),
          "E" = c(1L, 2L, 3L)
        ) |>
          arrow::as_arrow_table(df),
        out_df = "tibble"
      ) |>
        inherits(what = "tbl_df")
    )

  }
)

test_that(
  "check convert read npx output - df: arrow; out: arrow",
  {
    expect_true(
      object = convert_read_npx_output(
        df = dplyr::tibble(
          "A" = c(1, 2.2, 3.14),
          "B" = c("a", "b", "c"),
          "C" = c(TRUE, TRUE, FALSE),
          "D" = c("NA", "B", NA_character_),
          "E" = c(1L, 2L, 3L)
        ) |>
          arrow::as_arrow_table(df),
        out_df = "arrow"
      ) |>
        inherits(what = "ArrowObject")
    )

  }
)

test_that(
  "check convert read npx output - df: tibble; out: arrow",
  {
    expect_true(
      object = convert_read_npx_output(
        df = dplyr::tibble(
          "A" = c(1, 2.2, 3.14),
          "B" = c("a", "b", "c"),
          "C" = c(TRUE, TRUE, FALSE),
          "D" = c("NA", "B", NA_character_),
          "E" = c(1L, 2L, 3L)
        ),
        out_df = "arrow"
      ) |>
        inherits(what = "ArrowObject")
    )

  }
)

test_that(
  "check convert read npx output - df: tibble; out: tibble",
  {
    expect_true(
      object = convert_read_npx_output(
        df = dplyr::tibble(
          "A" = c(1, 2.2, 3.14),
          "B" = c("a", "b", "c"),
          "C" = c(TRUE, TRUE, FALSE),
          "D" = c("NA", "B", NA_character_),
          "E" = c(1L, 2L, 3L)
        ),
        out_df = "tibble"
      ) |>
        inherits(what = "tbl_df")
    )

  }
)

test_that(
  "check convert read npx output - df: data.frame; out: arrow",
  {
    expect_true(
      object = convert_read_npx_output(
        df = data.frame(
          "A" = c(1, 2.2, 3.14),
          "B" = c("a", "b", "c"),
          "C" = c(TRUE, TRUE, FALSE),
          "D" = c("NA", "B", NA_character_),
          "E" = c(1L, 2L, 3L)
        ),
        out_df = "arrow"
      ) |>
        inherits(what = "ArrowObject")
    )

  }
)

test_that(
  "check convert read npx output - df: data.frame; out: tibble",
  {
    expect_true(
      object = convert_read_npx_output(
        df = data.frame(
          "A" = c(1, 2.2, 3.14),
          "B" = c("a", "b", "c"),
          "C" = c(TRUE, TRUE, FALSE),
          "D" = c("NA", "B", NA_character_),
          "E" = c(1L, 2L, 3L)
        ),
        out_df = "tibble"
      ) |>
        inherits(what = "tbl_df")
    )

  }
)

test_that(
  "check convert read npx output - ERROR",
  {

    expect_error(
      object = convert_read_npx_output(df = c("I_Shall_Not_Pass",
                                              NA_character_),
                                       out_df = "tibble"),
      regexp = "Unexpected input data frame"
    )

    expect_error(
      object = convert_read_npx_output(df = NA_character_,
                                       out_df = "tibble"),
      regexp = "Unexpected input data frame"
    )

    expect_error(
      object = convert_read_npx_output(df = NULL,
                                       out_df = "tibble"),
      regexp = "Unexpected input data frame"
    )

    expect_error(
      object = convert_read_npx_output(df = 1,
                                       out_df = "tibble"),
      regexp = "Unexpected input data frame"
    )

    expect_error(
      object = convert_read_npx_output(df = 1L,
                                       out_df = "tibble"),
      regexp = "Unexpected input data frame"
    )

    expect_error(
      object = convert_read_npx_output(df = TRUE,
                                       out_df = "tibble"),
      regexp = "Unexpected input data frame"
    )

  }
)
