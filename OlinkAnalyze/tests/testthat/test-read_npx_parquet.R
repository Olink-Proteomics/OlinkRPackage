# this function is used to write NPX parquet files for testing purposes
write_npx_parquet <-
  function(df,
           npx_parquet_file,
           file_version = "NA",
           project_name = "NA",
           sample_matrix = "NA",
           data_file_type = "NPX File",
           product = "ExploreHT",
           remove_fields = NA) {
    # convert to arrow table ----
    df <- arrow::as_arrow_table(df)

    # modify metadata ----
    df$metadata$FileVersion <- file_version
    df$metadata$ProjectName <- project_name
    df$metadata$SampleMatrix <- sample_matrix
    df$metadata$DataFileType <- data_file_type
    df$metadata$Product <- product

    if (length(remove_fields) > 0) {
      df$metadata[remove_fields] <- NULL
    }

    # write file ----
    arrow::write_parquet(
      x = df,
      sink = npx_parquet_file,
      compression = "gzip"
    )
  }

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
        expect_true(file.exists(txtfile_p))

        # check that relevant error is thrown
        expect_error(
          read_npx_parquet(file = txtfile_p),
          regexp = "Unable to read parquet file: "
        )

      }
    )

  }
)

# Test that a relevant error is thrown when a text file with th extsnsion
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
        expect_true(file.exists(txtfile_pcorrupt))

        # check that relevant error is thrown
        expect_error(
          read_npx_parquet(file = txtfile_pcorrupt),
          regexp = "Unable to read parquet file: "
        )

      }
    )

  }
)

# Test that a relevant error is thrown when required metadata fieleds are
# missing.
test_that(
  "All metadata fields are in place",
  {

    withr::with_tempfile(
      new = "pfile_metadata",
      pattern = "parquet-file_alt-product-type",
      fileext = ".parquet",
      code = {

        # random data frame
        df <- dplyr::tibble("A" = c(1, 2.2, 3.14),
                            "B" = c("a", "b", "c"),
                            "C" = c(TRUE, TRUE, FALSE),
                            "D" = c("NA", "B", NA_character_),
                            "E" = c(1L, 2L, 3L))

        # random product name for testing
        tmp_metadata_remove <- c("Product", "DataFileType")

        write_npx_parquet(
          df = df,
          npx_parquet_file = pfile_metadata,
          remove_fields = tmp_metadata_remove
        )

        # check that the parquet file was created
        expect_true(file.exists(pfile_metadata))

        # check that relevant error is thrown
        expect_error(
          read_npx_parquet(file = pfile_metadata),
          regexp = "Missing required fields in metadata: "
        )

      }
    )

  }
)

# Test that a relevant error is thrown when the required metadata filed Product
# contains unexpected entries.
test_that(
  "Product field is correct",
  {

    withr::with_tempfile(
      new = "pfile_product",
      pattern = "parquet-file_alt-product-type",
      fileext = ".parquet",
      code = {

        # random data frame
        df <- dplyr::tibble("A" = c(1, 2.2, 3.14),
                            "B" = c("a", "b", "c"),
                            "C" = c(TRUE, TRUE, FALSE),
                            "D" = c("NA", "B", NA_character_),
                            "E" = c(1L, 2L, 3L))

        # random product name for testing
        tmp_product_name <- "Unknown_Product"

        write_npx_parquet(
          df = df,
          npx_parquet_file = pfile_product,
          product = tmp_product_name
        )

        # check that the parquet file was created
        expect_true(file.exists(pfile_product))

        # check that relevant error is thrown
        expect_error(
          read_npx_parquet(file = pfile_product),
          regexp = paste0("Unsupported platform: \"", tmp_product_name, "\"")
        )

      }
    )

  }
)

# Test that a relevant error is thrown when the required metadata filed
# DataFileType contains unexpected entries.
test_that(
  "DataFileType field is correct",
  {
    withr::with_tempfile(
      new = "pfile_datafiletype",
      pattern = "parquet-file_alt-product-type",
      fileext = ".parquet",
      code = {

        # random data frame
        df <- dplyr::tibble("A" = c(1, 2.2, 3.14),
                            "B" = c("a", "b", "c"),
                            "C" = c(TRUE, TRUE, FALSE),
                            "D" = c("NA", "B", NA_character_),
                            "E" = c(1L, 2L, 3L))

        # random product name for testing
        tmp_datafiletype_name <- "Unknown File"

        write_npx_parquet(
          df = df,
          npx_parquet_file = pfile_datafiletype,
          data_file_type = tmp_datafiletype_name
        )

        # check that the parquet file was created
        expect_true(file.exists(pfile_datafiletype))

        # check that relevant error is thrown
        expect_error(
          read_npx_parquet(file = pfile_datafiletype),
          regexp = paste0("Unsupported file: \"", tmp_datafiletype_name, "\"")
        )

      }
    )

  }
)

# Test that a relevant error is thrown when the required metadata filed
# DataFileType contains unexpected entries.
test_that(
  "Function returns arrowobject",
  {
    withr::with_tempfile(
      new = "pfile_test",
      pattern = "parquet-file_test",
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

        # check that relevant error is thrown
        expect_no_condition(
          read_npx_parquet(file = pfile_test)
        )

      }
    )

  }
)
