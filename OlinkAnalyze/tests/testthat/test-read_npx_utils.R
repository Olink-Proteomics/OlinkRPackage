# Test check_out_df_arg ----

test_that(
  "check out df arg works - TRUE",
  {
    expect_no_condition(
      object = check_out_df_arg(out_df = "tibble")
    )

    expect_no_condition(
      object = check_out_df_arg(out_df = "arrow")
    )
  }
)

test_that(
  "check out df arg works - ERROR",
  {
    expect_error(
      object = check_out_df_arg(out_df = "Tibble"),
      regexp = "Unknown output argument"
    )

    expect_error(
      object = check_out_df_arg(out_df = "TIBBLE"),
      regexp = "Unknown output argument"
    )

    expect_error(
      object = check_out_df_arg(out_df = "I_Shall_Not_Pass"),
      regexp = "Unknown output argument"
    )
  }
)

# Test convert_read_npx_output ----

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
  "check convert read npx output - ERROR",
  {
    expect_error(
      object = convert_read_npx_output(df = c("I_Shall_Not_Pass",
                                              NA_character_),
                                       out_df = "tibble"),
      regexp = "Unexpected input dataset"
    )

    expect_error(
      object = convert_read_npx_output(df = NA_character_,
                                       out_df = "tibble"),
      regexp = "Unexpected input dataset"
    )

    expect_error(
      object = convert_read_npx_output(df = NULL,
                                       out_df = "tibble"),
      regexp = "Unexpected input dataset"
    )

    expect_error(
      object = convert_read_npx_output(df = 1,
                                       out_df = "tibble"),
      regexp = "Unexpected input dataset"
    )

    expect_error(
      object = convert_read_npx_output(df = 1L,
                                       out_df = "tibble"),
      regexp = "Unexpected input dataset"
    )

    expect_error(
      object = convert_read_npx_output(df = TRUE,
                                       out_df = "tibble"),
      regexp = "Unexpected input dataset"
    )

    expect_error(
      object = convert_read_npx_output(
        df = data.frame(
          "A" = c(1, 2.2, 3.14),
          "B" = c("a", "b", "c"),
          "C" = c(TRUE, TRUE, FALSE),
          "D" = c("NA", "B", NA_character_),
          "E" = c(1L, 2L, 3L)
        ),
        out_df = "tibble"
      ),
      regexp = "Unexpected input dataset"
    )
  }
)

# Test read_npx_format_colnames ----

test_that(
  "read_npx_format_colnames - error - long read as wide",
  {
    withr::with_tempfile(
      new = "cdfile_test",
      pattern = "delim-file-test",
      fileext = ".txt",
      code = {

        # write the coma-delimited file from a random data frame
        dplyr::tibble(
          "A" = c(1, 2.2, 3.14),
          "B" = c("a", "b", "c"),
          "C" = c(TRUE, TRUE, FALSE),
          "D" = c("NA", "B", NA_character_),
          "E" = c(1L, 2L, 3L)
        ) |>
          utils::write.table(
            file = cdfile_test,
            append = FALSE,
            quote = FALSE,
            sep = ";",
            eol = "\n",
            na = "",
            dec = ".",
            row.names = TRUE,
            col.names = TRUE
          )

        # check that the comma delimited file exists
        expect_true(object = file.exists(cdfile_test))

        df <- utils::read.delim(
          file = cdfile_test,
          header = FALSE,
          sep = ";",
          blank.lines.skip = FALSE,
          na.strings = c("", "NA")
        )
        colnames(df) <- paste0("V", seq_len(ncol(df)))
        df <- dplyr::as_tibble(df)

        expect_error(
          object = read_npx_format_colnames(df = df,
                                            file = cdfile_test),
          regexp = "Detected file in wide format"
        )

        expect_error(
          object = read_npx_format_colnames(df = arrow::as_arrow_table(df),
                                            file = cdfile_test),
          regexp = "Detected file in wide format"
        )
      }
    )
  }
)

test_that(
  "read_npx_format_colnames - error - long read as wide delim",
  {
    withr::with_tempfile(
      new = "cdfile_test",
      pattern = "delim-file-test",
      fileext = ".txt",
      code = {
        # write the coma-delimited file from a random data frame
        dplyr::tibble(
          "A" = c("", 1, 2.2, 3.14),
          "B" = c("B", "a", "b", "c"),
          "C" = c("C", TRUE, TRUE, FALSE),
          "D" = c("D", "NA", "B", NA_character_),
          "E" = c("E", 1L, 2L, 3L)
        ) |>
          utils::write.table(
            file = cdfile_test,
            append = FALSE,
            quote = TRUE,
            sep = ";",
            eol = "\n",
            na = "",
            dec = ".",
            row.names = FALSE,
            col.names = FALSE
          )

        # check that the comma delimited file exists
        expect_true(object = file.exists(cdfile_test))

        df <- arrow::open_delim_dataset(
          sources = cdfile_test,
          delim = ";",
          col_names = TRUE,
          quoted_na = TRUE,
          na = c("", "NA")
        )

        # chech that reading the file works
        expect_error(
          object = read_npx_format_colnames(df = df,
                                            file = cdfile_test),
          regexp = "The dataset contains column names that are"
        )

        # crashes when converting ArrowTable to tibble
      }
    )
  }
)
