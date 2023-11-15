# Test that relevant error is thrown when sep is not a string
test_that(
  "read NPX delim error - sep not a string",
  {

    withr::with_tempfile(
      new = "cfile_test",
      pattern = "csv_temp_file",
      fileext = ".csv",
      code = {

        writeLines("foo", cfile_test)
        expect_true(object = file.exists(cfile_test))

        expect_error(
          object = read_npx_delim(file = cfile_test,
                                  sep = c(",", ";")),
          regexp = "\"sep\" should be a string!"
        )

        expect_error(
          object = read_npx_delim(file = cfile_test,
                                  sep = character()),
          regexp = "\"sep\" should be a string!"
        )

        expect_error(
          object = read_npx_delim(file = cfile_test,
                                  sep = 1),
          regexp = "\"sep\" should be a string!"
        )


        expect_error(
          object = read_npx_delim(file = cfile_test,
                                  sep = TRUE),
          regexp = "\"sep\" should be a string!"
        )

      }
    )

  }
)

# Test that relevant error is thrown when sep is not a string
test_that(
  "read NPX delim error - sep not accepted",
  {

    withr::with_tempfile(
      new = "cfile_test",
      pattern = "csv_temp_file",
      fileext = ".csv",
      code = {

        # write a random delimited file
        dplyr::tibble("A" = c(1, 2.2, 3.14),
                      "B" = c("a", "b", "c"),
                      "C" = c(TRUE, TRUE, FALSE),
                      "D" = c("NA", "B", NA_character_),
                      "E" = c(1L, 2L, 3L)) |>
          utils::write.table(file = cfile_test,
                             append = FALSE,
                             quote = FALSE,
                             sep = ";",
                             eol = "\n",
                             na = "",
                             dec = ".",
                             row.names = FALSE,
                             col.names = TRUE)

        expect_error(
          object = read_npx_delim(file = cfile_test,
                                  sep = "I_Am_Unaccepted"),
          regexp = "Unexpected separator:"
        )

        expect_error(
          object = read_npx_delim(file = cfile_test,
                                  sep = "A"),
          regexp = "Unexpected separator:"
        )

        expect_error(
          object = read_npx_delim(file = cfile_test,
                                  sep = "#"),
          regexp = "Unexpected separator:"
        )


        expect_error(
          object = read_npx_delim(file = cfile_test,
                                  sep = "|"),
          regexp = "Unexpected separator:"
        )

      }
    )

  }
)

# Test read delim works when sep is NULL - comma-delimited
test_that(
  "read NPX delim works - sep is NULL",
  {

    withr::with_tempfile(
      new = c("cdfile_test", "scdfile_test"),
      pattern = "delim-file-test",
      fileext = ".txt",
      code = {

        # random data frame
        df <- dplyr::tibble(
          "A" = c(1, 2.2, 3.14),
          "B" = c("a", "b", "c"),
          "C" = c(TRUE, TRUE, FALSE),
          "D" = c("NA", "B", NA_character_),
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
          object = coma_df <- read_npx_delim(file = cdfile_test,
                                             sep = NULL)
        )

        # check that variable exists
        expect_true(object = exists("coma_df"))

        # write the semicolon-delimited file
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

        # check that the semicolon delimited file exists
        expect_true(object = file.exists(scdfile_test))

        # chech that reading the file works
        expect_no_condition(
          object = semicolon_df <- read_npx_delim(file = scdfile_test,
                                                  sep = NULL)
        )

        # check that variable exists
        expect_true(object = exists("semicolon_df"))

        # check that the two dataframes are identical
        expect_equal(
          object = dplyr::as_tibble(coma_df),
          expected = dplyr::as_tibble(semicolon_df)
        )

      }
    )

  }
)

# Test read delim throws an error when file is not delimited
test_that(
  "read NPX delim error - file not delimited",
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
          object = read_npx_delim(file = pfile_test,
                                  sep = ","),
          regexp = "Unable to open delimited file:"
        )

      }
    )

  }
)

# Test read delim throws warning when wrong separator
test_that(
  "read NPX delim works - wrong sep",
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
        expect_warning(
          object = read_npx_delim(file = cdfile_test,
                                  sep = ";"),
          regexp = "Wrong input sep \";\"?"
        )

      }
    )

  }
)
