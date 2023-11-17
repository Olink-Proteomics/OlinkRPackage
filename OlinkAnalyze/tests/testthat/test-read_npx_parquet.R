# Test that a relevant error is thrown when a txt file was provided as parquet.
test_that(
  "Non-parquet input is handled - random file",
  {

    withr::with_tempfile(
      new = "txtfile_p",
      pattern = "txt-file_as_parquet-file",
      fileext = ".txt",
      code = {

        # write some text in a txt file
        writeLines("foo", txtfile_p)

        # check that the parquet file was created
        expect_true(object = file.exists(txtfile_p))

        # check that relevant error is thrown
        expect_error(
          object = read_npx_parquet(file = txtfile_p),
          regexp = "Unable to read parquet file: "
        )

      }
    )

  }
)

# Test that a relevant error is thrown when a text file with the extension
# parquet was provided as input.
test_that(
  "Non-parquet input is handled - corrupt parquet file",
  {

    withr::with_tempfile(
      new = "txtfile_pcorrupt",
      pattern = "txt-file_as_corrupt-parquet-file",
      fileext = ".parquet",
      code = {

        # write some text in a txt file
        writeLines("foo", txtfile_pcorrupt)

        # check that the parquet file was created
        expect_true(object = file.exists(txtfile_pcorrupt))

        # check that relevant error is thrown
        expect_error(
          object = read_npx_parquet(file = txtfile_pcorrupt),
          regexp = "Unable to read parquet file: "
        )

      }
    )

  }
)

# Test that a relevant error is thrown when required metadata fieleds are
# missing.
test_that(
  "Required metadata fields are missing",
  {

    withr::with_tempfile(
      new = "pfile_metadata",
      pattern = "parquet-file_alt-product-type",
      fileext = ".parquet",
      code = {

        # random data frame
        df <- dplyr::tibble(
          "A" = c(1, 2.2, 3.14),
          "B" = c("a", "b", "c"),
          "C" = c(TRUE, TRUE, FALSE),
          "D" = c("NA", "B", NA_character_),
          "E" = c(1L, 2L, 3L)
        ) |>
          arrow::as_arrow_table(df)

        # modify metadata
        df$metadata$FileVersion <- "NA"
        df$metadata$ProjectName <- "NA"
        df$metadata$SampleMatrix <- "NA"

        # write parquet
        arrow::write_parquet(
          x = df,
          sink = pfile_metadata,
          compression = "gzip"
        )

        # check that the parquet file was created
        expect_true(object = file.exists(pfile_metadata))

        # check that relevant error is thrown
        expect_error(
          object = read_npx_parquet(file = pfile_metadata),
          regexp = "Missing required fields in metadata: "
        )

      }
    )

  }
)

# Test that a relevant error is thrown when the required metadata filed Product
# contains unexpected entries.
test_that(
  "Product field is incorrect",
  {

    withr::with_tempfile(
      new = "pfile_product",
      pattern = "parquet-file_alt-product-type",
      fileext = ".parquet",
      code = {

        tmp_product_name <- "Unknown_Product"

        # random data frame
        df <- dplyr::tibble(
          "A" = c(1, 2.2, 3.14),
          "B" = c("a", "b", "c"),
          "C" = c(TRUE, TRUE, FALSE),
          "D" = c("NA", "B", NA_character_),
          "E" = c(1L, 2L, 3L)
        ) |>
          arrow::as_arrow_table()

        # modify metadata
        df$metadata$FileVersion <- "NA"
        df$metadata$ProjectName <- "NA"
        df$metadata$SampleMatrix <- "NA"
        df$metadata$DataFileType <- "NPX File"
        df$metadata$Product <- tmp_product_name

        # write parquet
        arrow::write_parquet(
          x = df,
          sink = pfile_product,
          compression = "gzip"
        )

        # check that the parquet file was created
        expect_true(object = file.exists(pfile_product))

        # check that relevant error is thrown
        expect_error(
          object = read_npx_parquet(file = pfile_product),
          regexp = paste0("Unsupported platform: \"", tmp_product_name, "\"")
        )

      }
    )

  }
)

# Test that a relevant error is thrown when the required metadata filed
# DataFileType contains unexpected entries.
test_that(
  "DataFileType field is incorrect",
  {
    withr::with_tempfile(
      new = "pfile_datafiletype",
      pattern = "parquet-file_alt-product-type",
      fileext = ".parquet",
      code = {

        # random product name for testing
        tmp_datafiletype_name <- "Unknown File"

        # random data frame
        df <- dplyr::tibble(
          "A" = c(1, 2.2, 3.14),
          "B" = c("a", "b", "c"),
          "C" = c(TRUE, TRUE, FALSE),
          "D" = c("NA", "B", NA_character_),
          "E" = c(1L, 2L, 3L)
        ) |>
          arrow::as_arrow_table()

        # modify metadata
        df$metadata$FileVersion <- "NA"
        df$metadata$ProjectName <- "NA"
        df$metadata$SampleMatrix <- "NA"
        df$metadata$DataFileType <- tmp_datafiletype_name
        df$metadata$Product <- "ExploreHT"

        # write parquet
        arrow::write_parquet(
          x = df,
          sink = pfile_datafiletype,
          compression = "gzip"
        )

        # check that the parquet file was created
        expect_true(object = file.exists(pfile_datafiletype))

        # check that relevant error is thrown
        expect_error(
          object = read_npx_parquet(file = pfile_datafiletype),
          regexp = paste0("Unsupported file: \"", tmp_datafiletype_name, "\"")
        )

      }
    )

  }
)

# Test that a the function works if DataFileType is one of the accepted values.
test_that(
  "Function returns arrowobject - all DataFileType",
  {

    # DataFileType == "NPX File" ----

    withr::with_tempfile(
      new = "pfile_test",
      pattern = "parquet-file_test",
      fileext = ".parquet",
      code = {

        # random data frame
        df <- dplyr::tibble(
          "A" = c(1, 2.2, 3.14),
          "B" = c("a", "b", "c"),
          "C" = c(TRUE, TRUE, FALSE),
          "D" = c("NA", "B", NA_character_),
          "E" = c(1L, 2L, 3L)
        ) |>
          arrow::as_arrow_table()

        # modify metadata ----
        df$metadata$FileVersion <- "NA"
        df$metadata$ProjectName <- "NA"
        df$metadata$SampleMatrix <- "NA"
        df$metadata$DataFileType <- "NPX File"
        df$metadata$Product <- "ExploreHT"

        # write the parquet file
        arrow::write_parquet(
          x = df,
          sink = pfile_test,
          compression = "gzip"
        )

        # check that the semicolon delimited file exists
        expect_true(object = file.exists(pfile_test))

        # check that relevant error is thrown
        expect_no_condition(
          object = df_out <- read_npx_parquet(file = pfile_test,
                                              out_df = "arrow")
        )

        expect_true(object = exists("df_out"))

        expect_true(inherits(x = df_out, what = "ArrowObject"))

        expect_true(
          object = read_npx_parquet(file = pfile_test,
                                    out_df = "tibble") |>
            inherits(what = "tbl_df")
        )

      }
    )

    # DataFileType == "Extended NPX File" ----

    withr::with_tempfile(
      new = "pfile_test",
      pattern = "parquet-file_test",
      fileext = ".parquet",
      code = {

        # random data frame
        df <- dplyr::tibble(
          "A" = c(1, 2.2, 3.14),
          "B" = c("a", "b", "c"),
          "C" = c(TRUE, TRUE, FALSE),
          "D" = c("NA", "B", NA_character_),
          "E" = c(1L, 2L, 3L)
        ) |>
          arrow::as_arrow_table()

        # modify metadata ----
        df$metadata$FileVersion <- "NA"
        df$metadata$ProjectName <- "NA"
        df$metadata$SampleMatrix <- "NA"
        df$metadata$DataFileType <- "Extended NPX File"
        df$metadata$Product <- "ExploreHT"

        # write the parquet file
        arrow::write_parquet(
          x = df,
          sink = pfile_test,
          compression = "gzip"
        )

        # check that the semicolon delimited file exists
        expect_true(object = file.exists(pfile_test))

        # check that relevant error is thrown
        expect_no_condition(
          object = read_npx_parquet(file = pfile_test)
        )

      }
    )

    # DataFileType == "CLI Data Export File" ----

    withr::with_tempfile(
      new = "pfile_test",
      pattern = "parquet-file_test",
      fileext = ".parquet",
      code = {

        # random data frame
        df <- dplyr::tibble(
          "A" = c(1, 2.2, 3.14),
          "B" = c("a", "b", "c"),
          "C" = c(TRUE, TRUE, FALSE),
          "D" = c("NA", "B", NA_character_),
          "E" = c(1L, 2L, 3L)
        ) |>
          arrow::as_arrow_table()

        # modify metadata ----
        df$metadata$FileVersion <- "NA"
        df$metadata$ProjectName <- "NA"
        df$metadata$SampleMatrix <- "NA"
        df$metadata$DataFileType <- "CLI Data Export File"
        df$metadata$Product <- "ExploreHT"

        # write the parquet file
        arrow::write_parquet(
          x = df,
          sink = pfile_test,
          compression = "gzip"
        )

        # check that the semicolon delimited file exists
        expect_true(object = file.exists(pfile_test))

        # check that relevant error is thrown
        expect_no_condition(
          object = read_npx_parquet(file = pfile_test)
        )

      }
    )

    # DataFileType == "Internal CLI Data Export File" ----

    withr::with_tempfile(
      new = "pfile_test",
      pattern = "parquet-file_test",
      fileext = ".parquet",
      code = {

        # random data frame
        df <- dplyr::tibble(
          "A" = c(1, 2.2, 3.14),
          "B" = c("a", "b", "c"),
          "C" = c(TRUE, TRUE, FALSE),
          "D" = c("NA", "B", NA_character_),
          "E" = c(1L, 2L, 3L)
        ) |>
          arrow::as_arrow_table()

        # modify metadata ----
        df$metadata$FileVersion <- "NA"
        df$metadata$ProjectName <- "NA"
        df$metadata$SampleMatrix <- "NA"
        df$metadata$DataFileType <- "Internal CLI Data Export File"
        df$metadata$Product <- "ExploreHT"

        # write the parquet file
        arrow::write_parquet(
          x = df,
          sink = pfile_test,
          compression = "gzip"
        )

        # check that the semicolon delimited file exists
        expect_true(object = file.exists(pfile_test))

        # check that relevant error is thrown
        expect_no_condition(
          object = read_npx_parquet(file = pfile_test)
        )

      }
    )

    # DataFileType == "R Package Export File" ----

    withr::with_tempfile(
      new = "pfile_test",
      pattern = "parquet-file_test",
      fileext = ".parquet",
      code = {

        # random data frame
        df <- dplyr::tibble(
          "A" = c(1, 2.2, 3.14),
          "B" = c("a", "b", "c"),
          "C" = c(TRUE, TRUE, FALSE),
          "D" = c("NA", "B", NA_character_),
          "E" = c(1L, 2L, 3L)
        ) |>
          arrow::as_arrow_table()

        # modify metadata ----
        df$metadata$FileVersion <- "NA"
        df$metadata$ProjectName <- "NA"
        df$metadata$SampleMatrix <- "NA"
        df$metadata$DataFileType <- "R Package Export File"
        df$metadata$Product <- "ExploreHT"

        # write the parquet file
        arrow::write_parquet(
          x = df,
          sink = pfile_test,
          compression = "gzip"
        )

        # check that the semicolon delimited file exists
        expect_true(object = file.exists(pfile_test))

        # check that relevant error is thrown
        expect_no_condition(
          object = read_npx_parquet(file = pfile_test)
        )

      }
    )

  }
)

# Test that a the function returns the same df as its input.
test_that(
  "Function returns tibble - matches input",
  {

    withr::with_tempfile(
      new = "pfile_test",
      pattern = "parquet-file_test",
      fileext = ".parquet",
      code = {

        # random data frame
        df <- dplyr::tibble(
          "A" = c(1, 2.2, NA_real_),
          "B" = c("a", NA_character_, "c"),
          "C" = c(NA, TRUE, FALSE),
          "D" = c("A#1", "B", NA_character_),
          "E" = c(1L, 2L, NA_integer_)
        ) |>
          arrow::as_arrow_table()

        # modify metadata ----
        df$metadata$FileVersion <- "NA"
        df$metadata$ProjectName <- "NA"
        df$metadata$SampleMatrix <- "NA"
        df$metadata$DataFileType <- "NPX File"
        df$metadata$Product <- "ExploreHT"

        # write the parquet file
        arrow::write_parquet(
          x = df,
          sink = pfile_test,
          compression = "gzip"
        )

        # check that the semicolon delimited file exists
        expect_true(object = file.exists(pfile_test))

        # check that relevant error is thrown
        expect_no_condition(
          object = df_out <- read_npx_parquet(file = pfile_test,
                                              out_df = "tibble")
        )

        expect_true(object = exists("df_out"))

        # convert df which is an ArrowObject to a tibble
        # so that the test below works
        df <- df |>
          dplyr::as_tibble()

        # check that the two dataframes are identical
        expect_equal(
          object = df_out,
          expected = df
        )

      }
    )

  }
)
