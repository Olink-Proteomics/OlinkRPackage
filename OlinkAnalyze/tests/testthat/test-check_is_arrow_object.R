# Test that relevant errors are thrown when non-ArrowObjects are checked
test_that(
  "check is arrow object - ERROR",
  {

    expect_error(
      object = check_is_arrow_object(df = c("I_Shall_Pass",
                                            NA_character_),
                                     error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "is not an R6 ArrowObject!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_arrow_object(df = NA_character_,
                                     error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "is not an R6 ArrowObject!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_arrow_object(df = NULL,
                                     error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "is not an R6 ArrowObject!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_arrow_object(df = 1,
                                     error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "is not an R6 ArrowObject!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_arrow_object(df = 1L,
                                     error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "is not an R6 ArrowObject!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_arrow_object(df = TRUE,
                                     error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "is not an R6 ArrowObject!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_arrow_object(df = data.frame(a = c(1, 2),
                                                     b = c("a", "b"),
                                                     c = c(TRUE, FALSE)),
                                     error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "is not an R6 ArrowObject!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_arrow_object(df = data.frame(a = c(1, 2),
                                                     b = c("a", "b"),
                                                     c = c(TRUE, FALSE)) |>
                                       dplyr::as_tibble(),
                                     error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "is not an R6 ArrowObject!",
                    fixed = TRUE)
    )

  }
)

# Test that FALSE is returned when a non-ArrowObject
test_that(
  "check is arrow object - FALSE",
  {

    expect_false(
      object = check_is_arrow_object(df = c("I_Shall_Pass",
                                            NA_character_),
                                     error = FALSE)
    )

    expect_false(
      object = check_is_arrow_object(df = NA_character_,
                                     error = FALSE)
    )

    expect_false(
      object = check_is_arrow_object(df = NULL,
                                     error = FALSE)
    )

    expect_false(
      object = check_is_arrow_object(df = 1,
                                     error = FALSE)
    )

    expect_false(
      object = check_is_arrow_object(df = 1L,
                                     error = FALSE)
    )

    expect_false(
      object = check_is_arrow_object(df = TRUE,
                                     error = FALSE)
    )

    expect_false(
      object = check_is_arrow_object(df = data.frame(a = c(1, 2),
                                                     b = c("a", "b"),
                                                     c = c(TRUE, FALSE)),
                                     error = FALSE)
    )

    expect_false(
      object = check_is_arrow_object(df = data.frame(a = c(1, 2),
                                                     b = c("a", "b"),
                                                     c = c(TRUE, FALSE)) |>
                                       dplyr::as_tibble(),
                                     error = FALSE)
    )

  }
)

# Test that no errors are thrown when csv file is read by arrow
test_that(
  "check is arrow object - csv - TRUE",
  {

    withr::with_tempfile(
      new = "dfile_test",
      pattern = "delim-file-test",
      fileext = ".csv",
      code = {

        sep_arrow <- ","

        # write the coma-delimited file from random data frame
        dplyr::tibble(
          "A" = c(1, 2.2, 3.14),
          "B" = c("a", "b", "c"),
          "C" = c(TRUE, TRUE, FALSE),
          "D" = c("NA", "B", NA_character_),
          "E" = c(1L, 2L, 3L)
        ) |>
          utils::write.table(
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
          object = df_arrow <- arrow::open_delim_dataset(
            sources = dfile_test,
            delim = sep_arrow,
            col_names = TRUE,
            quoted_na = TRUE,
            na = c("", "NA")
          )
        )

        # check that variable exists
        expect_true(object = exists("df_arrow"))

        # check if return from check_is_arrow_object is TRUE
        expect_true(
          object = check_is_arrow_object(df = df_arrow,
                                         error = FALSE)
        )

        # check if return from check_is_arrow_object is TRUE
        expect_true(
          object = check_is_arrow_object(df = df_arrow,
                                         error = TRUE)
        )

      }
    )

  }
)

# Test that no errors are thrown when csv file is read by arrow
test_that(
  "check is arrow object - parquet - TRUE",
  {

    withr::with_tempfile(
      new = "pfile_test",
      pattern = "delim-file-test",
      fileext = ".parquet",
      code = {

        # write the parquet file from random data frame
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

        # check that reading the file works
        expect_no_condition(
          object = df_arrow <- arrow::open_dataset(
            sources = pfile_test
          )
        )

        # check that variable exists
        expect_true(object = exists("df_arrow"))

        # check if check_is_arrow_object returns TRUE
        expect_true(
          object = check_is_arrow_object(df = df_arrow,
                                         error = FALSE)
        )

        # check if check_is_arrow_object returns TRUE
        expect_true(
          object = check_is_arrow_object(df = df_arrow,
                                         error = TRUE)
        )

      }
    )

  }
)

# Test that no errors are thrown when a data.frame is converted to an arrow
# table
test_that(
  "check is arrow object - data.frame - TRUE",
  {

    # random data frame
    df <- dplyr::tibble(
      "A" = c(1, 2.2, 3.14),
      "B" = c("a", "b", "c"),
      "C" = c(TRUE, TRUE, FALSE),
      "D" = c("NA", "B", NA_character_),
      "E" = c(1L, 2L, 3L)
    ) |>
      arrow::as_arrow_table()

    # check if object exists
    expect_true(object = exists("df"))

    # check if check_is_arrow_object returns TRUE
    expect_true(
      object = check_is_arrow_object(df = df,
                                     error = FALSE)
    )

    # check if check_is_arrow_object returns TRUE
    expect_true(
      object = check_is_arrow_object(df = df,
                                     error = TRUE)
    )

  }
)
