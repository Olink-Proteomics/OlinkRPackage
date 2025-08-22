# Test check_is_arrow_object ----

# Test that relevant errors are thrown when non-ArrowObjects are checked
test_that(
  "check is arrow object - ERROR",
  {
    expect_error(
      object = check_is_arrow_object(x = c("I_Shall_Pass",
                                           NA_character_),
                                     error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "is not an R6 ArrowObject!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_arrow_object(x = NA_character_,
                                     error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "is not an R6 ArrowObject!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_arrow_object(x = NULL,
                                     error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "is not an R6 ArrowObject!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_arrow_object(x = 1,
                                     error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "is not an R6 ArrowObject!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_arrow_object(x = 1L,
                                     error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "is not an R6 ArrowObject!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_arrow_object(x = TRUE,
                                     error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "is not an R6 ArrowObject!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_arrow_object(x = data.frame(a = c(1, 2),
                                                    b = c("a", "b"),
                                                    c = c(TRUE, FALSE)),
                                     error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "is not an R6 ArrowObject!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_arrow_object(x = data.frame(a = c(1, 2),
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
      object = check_is_arrow_object(x = c("I_Shall_Pass",
                                           NA_character_),
                                     error = FALSE)
    )

    expect_false(
      object = check_is_arrow_object(x = NA_character_,
                                     error = FALSE)
    )

    expect_false(
      object = check_is_arrow_object(x = NULL,
                                     error = FALSE)
    )

    expect_false(
      object = check_is_arrow_object(x = 1,
                                     error = FALSE)
    )

    expect_false(
      object = check_is_arrow_object(x = 1L,
                                     error = FALSE)
    )

    expect_false(
      object = check_is_arrow_object(x = TRUE,
                                     error = FALSE)
    )

    expect_false(
      object = check_is_arrow_object(x = data.frame(a = c(1, 2),
                                                    b = c("a", "b"),
                                                    c = c(TRUE, FALSE)),
                                     error = FALSE)
    )

    expect_false(
      object = check_is_arrow_object(x = data.frame(a = c(1, 2),
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
            sep = ",",
            eol = "\n",
            na = "",
            dec = ".",
            row.names = FALSE,
            col.names = TRUE
          )

        # check that the comma delimited file exists
        expect_true(object = file.exists(dfile_test))

        # read the file
        df_arrow <- arrow::open_delim_dataset(
          sources = dfile_test,
          delim = ",",
          col_names = TRUE,
          quoted_na = TRUE,
          na = c("", "NA")
        )

        # check that variable exists
        expect_true(object = exists("df_arrow"))

        # check if return from check_is_arrow_object is TRUE
        expect_true(
          object = check_is_arrow_object(x = df_arrow,
                                         error = FALSE)
        )

        # check if return from check_is_arrow_object is TRUE
        expect_true(
          object = check_is_arrow_object(x = df_arrow,
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
          object = check_is_arrow_object(x = df_arrow,
                                         error = FALSE)
        )

        # check if check_is_arrow_object returns TRUE
        expect_true(
          object = check_is_arrow_object(x = df_arrow,
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
      object = check_is_arrow_object(x = df,
                                     error = FALSE)
    )

    # check if check_is_arrow_object returns TRUE
    expect_true(
      object = check_is_arrow_object(x = df,
                                     error = TRUE)
    )
  }
)

# Test check_is_tibble ----

# Test that relevant errors are thrown when non-tibble objects are checked
test_that(
  "check is tibble - ERROR",
  {
    expect_error(
      object = check_is_tibble(x = c("I_Shall_Not_Pass",
                                     NA_character_),
                               error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "is not a tibble dataset!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_tibble(x = NA_character_,
                               error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "is not a tibble dataset!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_tibble(x = NULL,
                               error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "is not a tibble dataset!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_tibble(x = 1,
                               error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "is not a tibble dataset!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_tibble(x = 1L,
                               error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "is not a tibble dataset!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_tibble(x = TRUE,
                               error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "is not a tibble dataset!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_tibble(x = data.frame(a = c(1, 2),
                                              b = c("a", "b"),
                                              c = c(TRUE, FALSE)),
                               error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "is not a tibble dataset!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_tibble(x = data.frame(a = c(1, 2),
                                              b = c("a", "b"),
                                              c = c(TRUE, FALSE)) |>
                                 arrow::as_arrow_table(),
                               error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "is not a tibble dataset!",
                    fixed = TRUE)
    )
  }
)

# Test that check_is_tibble returns FALSE when non-tibble objects are checked
test_that(
  "check is tibble - FALSE",
  {
    expect_false(
      object = check_is_tibble(x = c("I_Shall_Not_Pass",
                                     NA_character_),
                               error = FALSE)
    )

    expect_false(
      object = check_is_tibble(x = NA_character_,
                               error = FALSE)
    )

    expect_false(
      object = check_is_tibble(x = NULL,
                               error = FALSE)
    )

    expect_false(
      object = check_is_tibble(x = 1,
                               error = FALSE)
    )

    expect_false(
      object = check_is_tibble(x = 1L,
                               error = FALSE)
    )

    expect_false(
      object = check_is_tibble(x = TRUE,
                               error = FALSE)
    )

    expect_false(
      object = check_is_tibble(x = data.frame(a = c(1, 2),
                                              b = c("a", "b"),
                                              c = c(TRUE, FALSE)),
                               error = FALSE)
    )

    expect_false(
      object = check_is_tibble(x = data.frame(a = c(1, 2),
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
        sep_arrow <- ","

        # write the coma-delimited file from a random data frame
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
          object = check_is_tibble(x = df_read,
                                   error = FALSE)
        )

        # check if return from check_is_tibble is TRUE
        expect_true(
          object = check_is_tibble(x = df_read,
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
        # write the parquet file from a random data frame
        df <- dplyr::tibble(
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
          object = df_read <- arrow::open_dataset(
            sources = pfile_test
          ) |>
            dplyr::collect()
        )

        # check that variable exists
        expect_true(object = exists("df_read"))

        # check if check_is_tibble returns TRUE
        expect_true(
          object = check_is_tibble(x = df_read,
                                   error = FALSE)
        )

        # check if check_is_tibble returns TRUE
        expect_true(
          object = check_is_tibble(x = df_read,
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
      object = check_is_tibble(x = df,
                               error = FALSE)
    )

    # check if check_is_tibble returns TRUE
    expect_true(
      object = check_is_tibble(x = df,
                               error = TRUE)
    )
  }
)

# Test check_is_dataset ----

test_that(
  "check_is_dataset - error",
  {
    expect_error(
      object = check_is_dataset(x = c("I_Shall_Not_Pass",
                                      NA_character_),
                                error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "is not a tibble or an ArrowObject dataset!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_dataset(x = NA_character_,
                                error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "is not a tibble or an ArrowObject dataset!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_dataset(x = NULL,
                                error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "is not a tibble or an ArrowObject dataset!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_dataset(x = 1,
                                error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "is not a tibble or an ArrowObject dataset!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_dataset(x = 1L,
                                error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "is not a tibble or an ArrowObject dataset!",
                    fixed = TRUE)
    )

    expect_error(
      object = check_is_dataset(x = TRUE,
                                error = TRUE),
      regexp = gsub(pattern = " ",
                    replacement = "([[:space:]].*|\\n.*)?",
                    x = "is not a tibble or an ArrowObject dataset!",
                    fixed = TRUE)
    )
  }
)

test_that(
  "check_is_dataset - FALSE",
  {
    expect_false(
      object = check_is_dataset(x = c("I_Shall_Not_Pass",
                                      NA_character_),
                                error = FALSE)
    )

    expect_false(
      object = check_is_dataset(x = NA_character_,
                                error = FALSE)
    )

    expect_false(
      object = check_is_dataset(x = NULL,
                                error = FALSE)
    )

    expect_false(
      object = check_is_dataset(x = 1,
                                error = FALSE)
    )

    expect_false(
      object = check_is_dataset(x = 1L,
                                error = FALSE)
    )

    expect_false(
      object = check_is_dataset(x = TRUE,
                                error = FALSE)
    )
  }
)

test_that(
  "check_is_dataset - works - csv to tibble or arrow",
  {
    ## tibble ----

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
          )
        )

        # check that variable exists
        expect_true(object = exists("df_read"))

        # read.delim return a data.frame
        expect_false(
          object = check_is_dataset(x = df_read,
                                    error = FALSE)
        )

        # check if return from check_is_data_frame is TRUE
        expect_true(
          object = check_is_dataset(x = dplyr::as_tibble(df_read),
                                    error = TRUE)
        )
      }
    )

    ## arrow ----

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
          object = df_read <- arrow::open_delim_dataset(
            sources = dfile_test,
            delim = sep_arrow,
            col_names = TRUE,
            quoted_na = TRUE,
            na = c("", "NA")
          )
        )

        # check that variable exists
        expect_true(object = exists("df_read"))

        # arrow::open_delim_dataset returns an arrow object
        expect_true(
          object = check_is_dataset(x = df_read,
                                    error = TRUE)
        )
      }
    )
  }
)

test_that(
  "check_is_dataset - works - parquet to arrow or tibble",
  {
    ## arrow ----

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
          object = df_read <- arrow::open_dataset(
            sources = pfile_test
          )
        )

        # check that variable exists
        expect_true(object = exists("df_read"))

        # check if check_is_dataset returns TRUE
        expect_true(
          object = check_is_dataset(x = df_read,
                                    error = TRUE)
        )

        # check that reading the file as data_frame works
        expect_no_condition(
          object = df_parquet <- arrow::read_parquet(
            file = pfile_test
          )
        )

        # check that variable exists
        expect_true(object = exists("df_parquet"))

        # check if check_is_dataset returns TRUE
        expect_true(
          object = check_is_dataset(x = df_parquet,
                                    error = TRUE)
        )
      }
    )

    ## tibble ----

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
          object = df_read <- arrow::open_dataset(
            sources = pfile_test
          ) |>
            dplyr::collect()
        )

        # check that variable exists
        expect_true(object = exists("df_read"))

        # check if check_is_dataset returns TRUE
        expect_true(
          object = check_is_dataset(x = df_read,
                                    error = TRUE)
        )

        # check that reading the file as data_frame works
        expect_no_condition(
          object = df_parquet <- arrow::read_parquet(
            file = pfile_test
          ) |>
            dplyr::collect()
        )

        # check that variable exists
        expect_true(object = exists("df_parquet"))

        # check if check_is_dataset returns TRUE
        expect_true(
          object = check_is_dataset(x = df_parquet,
                                    error = TRUE)
        )
      }
    )
  }
)

test_that(
  "check_is_dataset - works - data.frame to arrow or tibble",
  {
    # random data frame
    df <- data.frame(
      "A" = c(1, 2.2, 3.14),
      "B" = c("a", "b", "c"),
      "C" = c(TRUE, TRUE, FALSE),
      "D" = c("NA", "B", NA_character_),
      "E" = c(1L, 2L, 3L)
    )

    # check that variable exists
    expect_true(object = exists("df"))

    # check if check_is_dataset returns FALSE
    expect_false(
      object = check_is_dataset(x = df,
                                error = FALSE)
    )

    # check if check_is_dataset returns TRUE
    expect_true(
      object = check_is_dataset(x = dplyr::as_tibble(df),
                                error = TRUE)
    )

    # check if check_is_dataset returns TRUE
    expect_true(
      object = check_is_dataset(x = arrow::as_arrow_table(df),
                                error = FALSE)
    )
  }
)
