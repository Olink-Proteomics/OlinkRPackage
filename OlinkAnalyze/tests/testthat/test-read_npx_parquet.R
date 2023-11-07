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

test_that(
  "Non-parquet input is handled - random file",
  {
    textfile_parquet <- character()

    withr::with_tempfile(
      new = "txtfile_p",
      pattern = "txt-file_as_parquet-file",
      fileext = ".txt",
      code = {

        # write some text in a txt file
        writeLines("foo", txtfile_p)

        # check that the parquet file was created
        expect(
          file.exists(txtfile_p),
          failure_message = "Text file was not created!"
        )

        # check that relevant error is thrown
        expect_error(
          read_npx_parquet(file = txtfile_p),
          regexp = paste0("Unable to read parquet file: ")
        )

        textfile_parquet <<- txtfile_p
      }
    )

    expect_false(file.exists(textfile_parquet))
  }
)

test_that(
  "Non-parquet input is handled - corrupt parquet file",
  {
    txtfile_parquetcorrupt <- character()

    withr::with_tempfile(
      new = "txtfile_pcorrupt",
      pattern = "txt-file_as_corrupt-parquet-file",
      fileext = ".parquet",
      code = {

        # write some text in a txt file
        writeLines("foo", txtfile_pcorrupt)

        # check that the parquet file was created
        expect(
          file.exists(txtfile_pcorrupt),
          failure_message = "Corrupt parquet file was not created!"
        )

        # check that relevant error is thrown
        expect_error(
          read_npx_parquet(file = txtfile_pcorrupt),
          regexp = paste0("Unable to read parquet file: ")
        )

        txtfile_parquetcorrupt <<- txtfile_pcorrupt
      }
    )

    expect_false(file.exists(txtfile_parquetcorrupt))
  }
)

test_that(
  "All metadata fields are in place",
  {
    parquetfile_metadata <- character()

    withr::with_tempfile(
      new = "pfile_metadata",
      pattern = "parquet-file_alt-product-type",
      fileext = ".parquet",
      code = {
        # random product name for testing
        tmp_metadata_remove <- c("Product", "DataFileType")

        write_npx_parquet(
          df = system.file("extdata", "npx_data_ext.parquet",
                           package = "OlinkAnalyze",
                           mustWork = TRUE) |>
            read_npx_parquet() |>
            dplyr::as_tibble() |>
            dplyr::collect(),
          npx_parquet_file = pfile_metadata,
          remove_fields = tmp_metadata_remove
        )

        # check that the parquet file was created
        expect(
          file.exists(pfile_metadata),
          failure_message = "Parquet file was not created!"
        )

        # check that relevant error is thrown
        expect_error(
          read_npx_parquet(file = pfile_metadata),
          regexp = paste0("Missing required field s in metadata: ")
        )

        parquetfile_metadata <<- pfile_metadata
      }
    )

    expect_false(file.exists(parquetfile_metadata))
  }
)

test_that(
  "Product field is correct",
  {
    parquetfile_product <- character()

    withr::with_tempfile(
      new = "pfile_product",
      pattern = "parquet-file_alt-product-type",
      fileext = ".parquet",
      code = {
        # random product name for testing
        tmp_product_name <- "Unknown_Product"

        write_npx_parquet(
          df = system.file("extdata", "npx_data_ext.parquet",
                           package = "OlinkAnalyze",
                           mustWork = TRUE) |>
            read_npx_parquet() |>
            dplyr::as_tibble() |>
            dplyr::collect(),
          npx_parquet_file = pfile_product,
          product = tmp_product_name
        )

        # check that the parquet file was created
        expect(
          file.exists(pfile_product),
          failure_message = "Parquet file was not created!"
        )

        # check that relevant error is thrown
        expect_error(
          read_npx_parquet(file = pfile_product),
          regexp = paste0("Unsupported platform: \"", tmp_product_name, "\"")
        )

        parquetfile_product <<- pfile_product
      }
    )

    expect_false(file.exists(parquetfile_product))
  }
)

test_that(
  "DataFileType field is correct",
  {
    parquetfile_datafiletype <- character()

    withr::with_tempfile(
      new = "pfile_datafiletype",
      pattern = "parquet-file_alt-product-type",
      fileext = ".parquet",
      code = {
        # random product name for testing
        tmp_datafiletype_name <- "Unknown File"

        write_npx_parquet(
          df = system.file("extdata", "npx_data_ext.parquet",
                           package = "OlinkAnalyze",
                           mustWork = TRUE) |>
            read_npx_parquet() |>
            dplyr::as_tibble() |>
            dplyr::collect(),
          npx_parquet_file = pfile_datafiletype,
          data_file_type = tmp_datafiletype_name
        )

        # check that the parquet file was created
        expect(
          file.exists(pfile_datafiletype),
          failure_message = "Parquet file was not created!"
        )

        # check that relevant error is thrown
        expect_error(
          read_npx_parquet(file = pfile_datafiletype),
          regexp = paste0("Unsupported file: \"", tmp_datafiletype_name, "\"")
        )

        parquetfile_datafiletype <<- pfile_datafiletype
      }
    )

    expect_false(file.exists(parquetfile_datafiletype))
  }
)
