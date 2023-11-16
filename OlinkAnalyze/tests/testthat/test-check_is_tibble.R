# Test that relevant errors are thrown when non-tibble objects are checked
test_that(
  "check is tibble - ERROR",
  {
    expect_error(
      object = check_is_tibble(df = c("I_Shall_Not_Pass",
                                      NA_character_),
                               error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "is not a tibble data frame!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_tibble(df = NA_character_,
                               error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "is not a tibble data frame!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_tibble(df = NULL,
                               error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "is not a tibble data frame!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_tibble(df = 1,
                               error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "is not a tibble data frame!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_tibble(df = 1L,
                               error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "is not a tibble data frame!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_tibble(df = TRUE,
                               error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "is not a tibble data frame!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_tibble(df = data.frame(a = c(1, 2),
                                               b = c("a", "b"),
                                               c = c(TRUE, FALSE)),
                               error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "is not a tibble data frame!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_tibble(df = data.frame(a = c(1, 2),
                                                   b = c("a", "b"),
                                                   c = c(TRUE, FALSE)) |>
                                     arrow::as_arrow_table(),
                                   error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "is not a tibble data frame!",
                    fixed = TRUE)
    )

  }
)

# Test that check_is_tibble returns FALSE when non-tibble objects are checked
test_that(
  "check is tibble - FALSE",
  {

    expect_false(
      object = check_is_tibble(df = c("I_Shall_Not_Pass",
                                      NA_character_),
                               error = FALSE)
    )

    expect_false(
      object = check_is_tibble(df = NA_character_,
                               error = FALSE)
    )

    expect_false(
      object = check_is_tibble(df = NULL,
                               error = FALSE)
    )

    expect_false(
      object = check_is_tibble(df = 1,
                               error = FALSE)
    )

    expect_false(
      object = check_is_tibble(df = 1L,
                               error = FALSE)
    )

    expect_false(
      object = check_is_tibble(df = TRUE,
                               error = FALSE)
    )

    expect_false(
      object = check_is_tibble(df = data.frame(a = c(1, 2),
                                               b = c("a", "b"),
                                               c = c(TRUE, FALSE)),
                               error = FALSE)
    )

    expect_error(
      object = check_is_tibble(df = data.frame(a = c(1, 2),
                                               b = c("a", "b"),
                                               c = c(TRUE, FALSE)) |>
                                 arrow::as_arrow_table(),
                               error = FALSE)
    )

  }
)

# Test that no errors are thrown when csv file is read and converted to tibble
test_that(
  "check is tibble - csv - TRUE",
  {

    withr::with_tempfile(
      new = "dfile_test",
      pattern = "delim-file-test",
      fileext = ".csv",
      code = {

        # random data frame
        df <- dplyr::tibble(
          "A" = c(1, 2.2, 3.14),
          "B" = c("a", "b", "c"),
          "C" = c(TRUE, TRUE, FALSE),
          "D" = c("NA", "B", NA_character_),
          "E" = c(1L, 2L, 3L)
        )

        sep_arrow <- ","

        # write the coma-delimited file
        utils::write.table(
          x = df,
          file = dfile_test,
          append = FALSE,
          quote = FALSE,
          sep = sep_arrow,
          eol = "\n",
          na = "",
          dec = ".",
          row.names = FALSE,
          col.names = TRUE
        )

        # check that the comma delimited file exists
        expect_true(object = file.exists(dfile_test))

        # check that reading the file works
        expect_no_condition(
          object = df_read <- utils::read.delim(
            file = dfile_test,
            header = TRUE,
            sep = sep_arrow,
            na.strings = c("NA", ""),
            stringsAsFactors = FALSE
          ) |>
            dplyr::as_tibble()
        )

        # check that variable exists
        expect_true(object = exists("df_read"))

        # check if return from check_is_tibble is TRUE
        expect_true(
          object = check_is_tibble(df = df_read,
                                   error = FALSE)
        )

        # check if return from check_is_tibble is TRUE
        expect_true(
          object = check_is_tibble(df = df_read,
                                   error = TRUE)
        )

      }
    )

  }
)

# Test that no errors are thrown when a parquet file is read and converted to
# tibble
test_that(
  "check is tibble - parquet - TRUE",
  {

    withr::with_tempfile(
      new = "pfile_test",
      pattern = "delim-file-test",
      fileext = ".parquet",
      code = {

        # random data frame
        df <- dplyr::tibble(
          "A" = c(1, 2.2, 3.14),
          "B" = c("a", "b", "c"),
          "C" = c(TRUE, TRUE, FALSE),
          "D" = c("NA", "B", NA_character_),
          "E" = c(1L, 2L, 3L)
        )

        # write the parquet file
        arrow::write_parquet(
          x = df,
          sink = pfile_test,
          compression = "gzip"
        )

        # check that the semicolon delimited file exists
        expect_true(object = file.exists(pfile_test))

        # check that reading the file works
        expect_no_condition(
          object = df_read <- arrow::open_dataset(
            sources = pfile_test
          ) |>
            dplyr::collect()
        )

        # check that variable exists
        expect_true(object = exists("df_read"))

        # check if check_is_tibble returns TRUE
        expect_true(
          object = check_is_tibble(df = df_read,
                                   error = FALSE)
        )

        # check if check_is_tibble returns TRUE
        expect_true(
          object = check_is_tibble(df = df_read,
                                   error = TRUE)
        )

      }
    )

  }
)

# Test that no errors are thrown when a data.frame is converted to tibble
test_that(
  "check is tibble - data.frame - TRUE",
  {

    # random data frame
    df <- dplyr::tibble(
      "A" = c(1, 2.2, 3.14),
      "B" = c("a", "b", "c"),
      "C" = c(TRUE, TRUE, FALSE),
      "D" = c("NA", "B", NA_character_),
      "E" = c(1L, 2L, 3L)
    )

    # check that variable exists
    expect_true(object = exists("df"))

    # check if check_is_tibble returns TRUE
    expect_true(
      object = check_is_tibble(df = df,
                               error = FALSE)
    )

    # check if check_is_tibble returns TRUE
    expect_true(
      object = check_is_tibble(df = df,
                               error = TRUE)
    )

  }
)
