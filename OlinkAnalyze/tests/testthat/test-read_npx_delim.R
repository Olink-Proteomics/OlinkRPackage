# Test read_npx_delim ----

test_that(
  "read_npx_delim - works - long format - output df matches input df",
  {
    ## tibble ----

    withr::with_tempfile(
      new = "scdfile_test",
      pattern = "delim-file-test",
      fileext = ".csv",
      code = {
        # random data frame
        df <- dplyr::tibble(
          "A" = c(1, 2.2, NA_real_),
          "B" = c("a", NA_character_, "c"),
          "C" = c(NA, TRUE, FALSE),
          "D" = c("NA", "B", NA_character_),
          "E" = c(1L, 2L, NA_integer_)
        )

        # write the coma-delimited file
        utils::write.table(
          x = df,
          file = scdfile_test,
          append = FALSE,
          quote = FALSE,
          sep = ";",
          eol = "\n",
          na = "",
          dec = ".",
          row.names = FALSE,
          col.names = TRUE
        )

        # check that the comma delimited file exists
        expect_true(object = file.exists(scdfile_test))

        # chech that reading the file works
        expect_no_condition(
          object = df_out <- read_npx_delim(file = scdfile_test,
                                            out_df = "tibble")
        )

        # check that variable exists
        expect_true(object = exists("df_out"))

        expect_true(inherits(x = df_out, what = "tbl_df"))

        # convert "NA" in df to NA so that the test below works
        # this is to simulate how to arrow reader is epected to work
        df <- df |>
          dplyr::mutate(
            dplyr::across(
              .cols = dplyr::everything(),
              .fns = ~ dplyr::if_else(.x == "NA", NA, .x)
            )
          )

        # check that the two dataframes are identical
        expect_equal(
          object = df_out,
          expected = df
        )
      }
    )

    ## arrow ----

    withr::with_tempfile(
      new = "scdfile_test",
      pattern = "delim-file-test",
      fileext = ".csv",
      code = {
        # random data frame
        df <- dplyr::tibble(
          "A" = c(1, 2.2, NA_real_),
          "B" = c("a", NA_character_, "c"),
          "C" = c(NA, TRUE, FALSE),
          "D" = c("NA", "B", NA_character_),
          "E" = c(1L, 2L, NA_integer_)
        )

        # write the coma-delimited file
        utils::write.table(
          x = df,
          file = scdfile_test,
          append = FALSE,
          quote = FALSE,
          sep = ";",
          eol = "\n",
          na = "",
          dec = ".",
          row.names = FALSE,
          col.names = TRUE
        )

        # check that the comma delimited file exists
        expect_true(object = file.exists(scdfile_test))

        # chech that reading the file works
        expect_no_condition(
          object = df_out <- read_npx_delim(file = scdfile_test,
                                            out_df = "arrow")
        )

        # check that variable exists
        expect_true(object = exists("df_out"))

        expect_true(inherits(x = df_out, what = "ArrowObject"))

        # convert "NA" in df to NA so that the test below works
        # this is to simulate how to arrow reader is epected to work
        df <- df |>
          dplyr::mutate(
            dplyr::across(
              .cols = dplyr::everything(),
              .fns = ~ dplyr::if_else(.x == "NA", NA, .x)
            )
          )

        # check that the two dataframes are identical
        expect_equal(
          object = dplyr::as_tibble(df_out),
          expected = df
        )
      }
    )
  }
)

test_that(
  "read_npx_delim - works - wide format - output df matches input df",
  {
    # get synthetic data, or skip if not available
    df_rand <- get_wide_synthetic_data(
      olink_platform = "Target 96",
      data_type = "NPX",
      n_panels = 3L,
      n_assays = 92L,
      n_samples = 99L,
      show_dev_int_ctrl = TRUE,
      show_int_ctrl = TRUE,
      version = 1L
    )

    ## tibble ----

    withr::with_tempfile(
      new = "scdfile_test",
      pattern = "delim-file-test",
      fileext = ".csv",
      code = {
        # write the coma-delimited file
        utils::write.table(
          x = df_rand$list_df_wide$df_wide,
          file = scdfile_test,
          append = FALSE,
          quote = FALSE,
          sep = ";",
          eol = "\n",
          na = "",
          dec = ".",
          row.names = FALSE,
          col.names = FALSE
        )

        # check that the comma delimited file exists
        expect_true(object = file.exists(scdfile_test))

        # chech that reading the file works
        expect_no_condition(
          object = df_out <- read_npx_delim(file = scdfile_test,
                                            out_df = "tibble")
        )

        # check that variable exists
        expect_true(object = exists("df_out"))

        expect_true(inherits(x = df_out, what = "tbl_df"))

        # check that the two dataframes are identical
        expect_equal(
          object = df_out,
          expected = df_rand$list_df_wide$df_wide
        )
      }
    )

    ## arrow ----

    withr::with_tempfile(
      new = "scdfile_test",
      pattern = "delim-file-test",
      fileext = ".csv",
      code = {
        # write the coma-delimited file
        utils::write.table(
          x = df_rand$list_df_wide$df_wide,
          file = scdfile_test,
          append = FALSE,
          quote = FALSE,
          sep = ";",
          eol = "\n",
          na = "",
          dec = ".",
          row.names = FALSE,
          col.names = FALSE
        )

        # check that the comma delimited file exists
        expect_true(object = file.exists(scdfile_test))

        # chech that reading the file works
        expect_no_condition(
          object = df_out <- read_npx_delim(file = scdfile_test,
                                            out_df = "arrow")
        )

        # check that variable exists
        expect_true(object = exists("df_out"))

        expect_true(inherits(x = df_out, what = "ArrowObject"))

        # check that the two dataframes are identical
        expect_equal(
          object = dplyr::as_tibble(df_out),
          expected = df_rand$list_df_wide$df_wide
        )
      }
    )
  }
)

test_that(
  "read_npx_delim - works - long format - file contains hashtags \"#\"",
  {
    withr::with_tempfile(
      new = "cdfile_test",
      pattern = "delim-file-test",
      fileext = ".csv",
      code = {
        # random data frame
        df <- dplyr::tibble(
          "B" = c("a#1", "b", "c"),
          "A" = c(1, 2.2, 3.14),
          "C" = c(TRUE, TRUE, FALSE),
          "D" = c("A", "B", NA_character_),
          "E" = c(1L, 2L, 3L)
        )

        # write the coma-delimited file
        utils::write.table(
          x = df,
          file = cdfile_test,
          append = FALSE,
          quote = FALSE,
          sep = ",",
          eol = "\n",
          na = "",
          dec = ".",
          row.names = FALSE,
          col.names = TRUE
        )

        # check that the comma delimited file exists
        expect_true(object = file.exists(cdfile_test))

        # chech that reading the file works
        expect_no_condition(
          object = df_out <- read_npx_delim(file = cdfile_test,
                                            out_df = "tibble")
        )

        # check that variable exists
        expect_true(object = exists("df_out"))

        # check that the two dataframes are identical
        expect_equal(
          object = df_out,
          expected = df
        )
      }
    )
  }
)

test_that(
  "read_npx_delim - works - long format - quoted numbered rownames",
  {
    withr::with_tempfile(
      new = "cdfile_test",
      pattern = "delim-file-test",
      fileext = ".csv",
      code = {
        # random data frame
        df <- dplyr::tibble(
          "B" = c("a#1", "b", "c"),
          "A" = c(1, 2.2, 3.14),
          "C" = c(TRUE, TRUE, FALSE),
          "D" = c("A", "B", NA_character_),
          "E" = c(1L, 2L, 3L)
        )

        out_column <- df |>
          dplyr::mutate(
            dplyr::across(
              where(is.character),
              ~ dplyr::if_else(is.na(.x), .x, paste0("\"", .x, "\""))
            )
          ) |>
          dplyr::mutate(
            out_col = paste0(
              "\"", dplyr::row_number(), "\";",
              .data[["B"]], ";", .data[["A"]], ";", .data[["C"]],
              ";", .data[["D"]], ";", .data[["E"]]
            )
          ) |>
          dplyr::pull(
            .data[["out_col"]]
          )
        out_column <- c(
          paste0(
            "\"\";\"",
            cli::ansi_collapse(x = names(df), sep = "\";\"", last = "\";\""),
            "\""
          ),
          out_column
        )
        writeLines(
          text = out_column,
          con = cdfile_test
        )

        # check that the comma delimited file exists
        expect_true(object = file.exists(cdfile_test))

        # chech that reading the file works
        expect_no_condition(
          object = df_out <- read_npx_delim(file = cdfile_test,
                                            out_df = "tibble")
        )

        # check that variable exists
        expect_true(object = exists("df_out"))

        # check that the two dataframes are identical
        expect_equal(
          object = df_out |>
            dplyr::select(
              -dplyr::all_of("V1")
            ),
          expected = df
        )
      }
    )
  }
)

test_that(
  "read_npx_delim - works - long format - file contains quotations",
  {
    withr::with_tempfile(
      new = "cdfile_test",
      pattern = "delim-file-test",
      fileext = ".csv",
      code = {
        # random data frame
        df <- dplyr::tibble(
          "B" = c("a#1", "b", "c"),
          "A" = c(1, 2.2, 3.14),
          "C" = c(TRUE, TRUE, FALSE),
          "D" = c("A", "B", NA_character_),
          "E" = c(1L, 2L, 3L)
        )

        # write the coma-delimited file
        utils::write.table(
          x = df,
          file = cdfile_test,
          append = FALSE,
          quote = TRUE,
          sep = ";",
          eol = "\n",
          na = "",
          dec = ".",
          row.names = FALSE,
          col.names = TRUE
        )

        # check that the comma delimited file exists
        expect_true(object = file.exists(cdfile_test))

        # chech that reading the file works
        expect_no_condition(
          object = df_out <- read_npx_delim(file = cdfile_test,
                                            out_df = "tibble")
        )

        # check that variable exists
        expect_true(object = exists("df_out"))

        # check that the two dataframes are identical
        expect_equal(
          object = df_out,
          expected = df
        )
      }
    )
  }
)

test_that(
  "read_npx_delim - error - long format - file not delimited",
  {
    withr::with_tempfile(
      new = "pfile_test",
      pattern = "parquet-file_test",
      fileext = ".parquet",
      code = {
        # write the parquet file from a random data frame
        dplyr::tibble(
          "A" = c(1, 2.2, 3.14),
          "B" = c("a", "b", "c"),
          "C" = c(TRUE, TRUE, FALSE),
          "D" = c("NA", "B", NA_character_),
          "E" = c(1L, 2L, 3L)
        ) |>
          arrow::write_parquet(
            sink = pfile_test,
            compression = "gzip"
          )

        # check that the semicolon delimited file exists
        expect_true(object = file.exists(pfile_test))

        # check that relevant error is thrown
        expect_error(
          object = read_npx_delim(file = pfile_test),
          regexp = "Unable to open delimited file:"
        )
      }
    )
  }
)

test_that(
  "read_npx_delim - error - wide format - file not delimited",
  {
    # get synthetic data, or skip if not available
    df_rand <- get_wide_synthetic_data(
      olink_platform = "Target 96",
      data_type = "NPX",
      n_panels = 3L,
      n_assays = 92L,
      n_samples = 99L,
      show_dev_int_ctrl = TRUE,
      show_int_ctrl = TRUE,
      version = 1L
    )

    withr::with_tempfile(
      new = "pfile_test",
      pattern = "parquet-file_test",
      fileext = ".parquet",
      code = {
        arrow::write_parquet(
          x = df_rand$list_df_wide$df_wide,
          sink = pfile_test,
          compression = "gzip"
        )

        # check that the semicolon delimited file exists
        expect_true(object = file.exists(pfile_test))

        # check that relevant error is thrown
        expect_error(
          object = read_npx_delim(file = pfile_test) |>
            suppressWarnings(),
          regexp = "Unable to open delimited file:"
        )
      }
    )
  }
)

test_that(
  "read_npx_delim - error - df has one column only",
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
            sep = "ABC",
            eol = "\n",
            na = "",
            dec = ".",
            row.names = FALSE,
            col.names = TRUE
          )

        # check that the comma delimited file exists
        expect_true(object = file.exists(cdfile_test))

        # check that reading the file works
        expect_warning(
          object = expect_warning(
            object = read_npx_delim(file = cdfile_test),
            regexp = "has only one column"
          )
        )
      }
    )
  }
)
