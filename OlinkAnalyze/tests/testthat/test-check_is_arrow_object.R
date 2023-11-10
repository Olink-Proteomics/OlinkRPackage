# Test that relevant errors are thrown when non-ArrowObjects are checked
test_that(
  "check is arrow object - relevant errors",
  {
    expect_error(
      check_is_arrow_object(var = c("I_Shall_Pass",
                                    NA_character_)),
      regexp = "is not an R6 ArrowObject!"
    )

    expect_error(
      check_is_arrow_object(var = NA_character_),
      regexp = "is not an R6 ArrowObject!"
    )

    expect_error(
      check_is_arrow_object(var = NULL),
      regexp = "is not an R6 ArrowObject!"
    )

    expect_error(
      check_is_arrow_object(var = 1),
      regexp = "is not an R6 ArrowObject!"
    )

    expect_error(
      check_is_arrow_object(var = 1L),
      regexp = "is not an R6 ArrowObject!"
    )

    expect_error(
      check_is_arrow_object(var = TRUE),
      regexp = "is not an R6 ArrowObject!"
    )

  }
)

# Test that no errors are thrown when csv file is read by arrow
test_that(
  "check is arrow object - no errors CSV",
  {

    delimfile_test <- character()

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

        # write the coma-delimited file
        utils::write.table(x = df,
                           file = dfile_test,
                           append = FALSE,
                           quote = FALSE,
                           sep = ",",
                           eol = "\n",
                           na = "",
                           dec = ".",
                           row.names = FALSE,
                           col.names = TRUE)

        # check that the comma delimited file exists
        expect_true(file.exists(dfile_test))

        # chech that reading the file works
        expect_no_condition(
          delim_df <- read_npx_delim(file = dfile_test,
                                     sep = NULL)
        )

        # check that variable exists
        expect_true(exists("delim_df"))

        # check if return from read_npx_delim is R6 ArrowObject
        expect_no_condition(
          check_is_arrow_object(var = delim_df)
        )

        delimfile_test <<- dfile_test

      }
    )

    expect_false(file.exists(delimfile_test))

  }
)

# Test that no errors are thrown when csv file is read by arrow
test_that(
  "check is arrow object - no errors Parquet",
  {

    parquetfile_test <- character()

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

        # chech that reading the file works
        expect_no_condition(
          parquet_df <- read_npx_parquet(file = pfile_test)
        )

        # check that variable exists
        expect_true(exists("parquet_df"))

        # check if return from read_npx_parquet is R6 ArrowObject
        expect_no_condition(
          check_is_arrow_object(var = parquet_df)
        )

        parquetfile_test <<- pfile_test

      }
    )

    expect_false(file.exists(parquetfile_test))

  }
)
