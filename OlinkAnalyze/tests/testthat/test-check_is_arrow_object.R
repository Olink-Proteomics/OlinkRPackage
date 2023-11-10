# Test that relevant errors are thrown when non-ArrowObjects are checked
test_that(
  "check is arrow object - relevant errors",
  {
    expect_error(
      check_is_arrow_object(var = c("I_Shall_Pass",
                                    NA_character_),
                            error = TRUE),
      regexp = "is not an R6 ArrowObject!"
    )

    expect_error(
      check_is_arrow_object(var = NA_character_,
                            error = TRUE),
      regexp = "is not an R6 ArrowObject!"
    )

    expect_error(
      check_is_arrow_object(var = NULL,
                            error = TRUE),
      regexp = "is not an R6 ArrowObject!"
    )

    expect_error(
      check_is_arrow_object(var = 1,
                            error = TRUE),
      regexp = "is not an R6 ArrowObject!"
    )

    expect_error(
      check_is_arrow_object(var = 1L,
                            error = TRUE),
      regexp = "is not an R6 ArrowObject!"
    )

    expect_error(
      check_is_arrow_object(var = TRUE,
                            error = TRUE),
      regexp = "is not an R6 ArrowObject!"
    )

  }
)

# Test that FALSE is returned when a non-ArrowObject
test_that(
  "check is arrow object - return FALSE",
  {
    expect_false(
      check_is_arrow_object(var = c("I_Shall_Pass",
                                    NA_character_),
                            error = FALSE)
    )

    expect_false(
      check_is_arrow_object(var = NA_character_,
                            error = FALSE)
    )

    expect_false(
      check_is_arrow_object(var = NULL,
                            error = FALSE)
    )

    expect_false(
      check_is_arrow_object(var = 1,
                            error = FALSE)
    )

    expect_false(
      check_is_arrow_object(var = 1L,
                            error = FALSE)
    )

    expect_false(
      check_is_arrow_object(var = TRUE,
                            error = FALSE)
    )

    # random data frame
    df <- dplyr::tibble("A" = c(1, 2.2, 3.14),
                        "B" = c("a", "b", "c"),
                        "C" = c(TRUE, TRUE, FALSE),
                        "D" = c("NA", "B", NA_character_),
                        "E" = c(1L, 2L, 3L))
    expect_false(
      check_is_arrow_object(var = df,
                            error = FALSE)
    )

  }
)

# Test that no errors are thrown when csv file is read by arrow
test_that(
  "check is arrow object - no errors CSV",
  {

    withr::with_tempfile(
      new = "dfile_test",
      pattern = "delim-file-test",
      fileext = ".csv",
      code = {

        # random data frame
        df <- dplyr::tibble("A" = c(1, 2.2, 3.14),
                            "B" = c("a", "b", "c"),
                            "C" = c(TRUE, TRUE, FALSE),
                            "D" = c("NA", "B", NA_character_),
                            "E" = c(1L, 2L, 3L))

        sep_arrow <- ","

        # write the coma-delimited file
        utils::write.table(x = df,
                           file = dfile_test,
                           append = FALSE,
                           quote = FALSE,
                           sep = sep_arrow,
                           eol = "\n",
                           na = "",
                           dec = ".",
                           row.names = FALSE,
                           col.names = TRUE)

        # check that the comma delimited file exists
        expect_true(file.exists(dfile_test))

        # check that reading the file works
        expect_no_condition(
          df_arrow <- arrow::open_delim_dataset(
            sources = dfile_test,
            delim = sep_arrow,
            quote = "\"",
            col_names = TRUE,
            na = c("", "NA")
          )
        )

        # check that variable exists
        expect_true(exists("df_arrow"))

        # check if return from check_is_arrow_object is TRUE
        expect_true(
          check_is_arrow_object(var = df_arrow,
                                error = FALSE)
        )

        # check if return from check_is_arrow_object is TRUE
        expect_true(
          check_is_arrow_object(var = df_arrow,
                                error = TRUE)
        )


      }
    )

  }
)

# Test that no errors are thrown when csv file is read by arrow
test_that(
  "check is arrow object - no errors Parquet",
  {

    withr::with_tempfile(
      new = "pfile_test",
      pattern = "delim-file-test",
      fileext = ".parquet",
      code = {

        # random data frame
        df <- dplyr::tibble("A" = c(1, 2.2, 3.14),
                            "B" = c("a", "b", "c"),
                            "C" = c(TRUE, TRUE, FALSE),
                            "D" = c("NA", "B", NA_character_),
                            "E" = c(1L, 2L, 3L)) |>
          arrow::as_arrow_table()

        # modify metadata ----
        df$metadata$FileVersion <- "NA"
        df$metadata$ProjectName <- "NA"
        df$metadata$SampleMatrix <- "NA"
        df$metadata$DataFileType <- "NPX File"
        df$metadata$Product <- "ExploreHT"

        # write the parquet file
        arrow::write_parquet(x = df,
                             sink = pfile_test,
                             compression = "gzip")

        # check that the semicolon delimited file exists
        expect_true(file.exists(pfile_test))

        # check that reading the file works
        expect_no_condition(
          df_arrow <- arrow::open_dataset(
            sources = pfile_test
          )
        )

        # check that variable exists
        expect_true(exists("df_arrow"))

        # check if check_is_arrow_object returns TRUE
        expect_true(
          check_is_arrow_object(var = df_arrow,
                                error = FALSE)
        )

        # check if check_is_arrow_object returns TRUE
        expect_true(
          check_is_arrow_object(var = df_arrow,
                                error = TRUE)
        )

      }
    )

  }
)
