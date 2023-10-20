# original raw parquet file
parquet_file <- system.file("extdata", "npx_data_ext.parquet",
                            package = "OlinkAnalyze", mustWork = TRUE)

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
  "All metadata fields are in place",
  {
    # temp file to store parquet file
    parquetfile_metadata <- tempfile(
      pattern = "parquet-file_alt-product-type",
      tmpdir = tempdir(),
      fileext = ".parquet"
    )

    # random product name for testing
    tmp_metadata_remove <- c("Product", "DataFileType")


    write_npx_parquet(
      df = read_NPX(parquet_file) |>
        dplyr::as_tibble() |>
        dplyr::collect(),
      npx_parquet_file = parquetfile_metadata,
      remove_fields = tmp_metadata_remove
    )

    # check that the parquet file was created
    expect(
      file.exists(parquetfile_metadata),
      failure_message = "Parquet file was not created!"
    )

    # check that relevant error is thrown
    expect_error(
      read_NPX(parquetfile_metadata),
      regexp = paste0("Missing required field s in metadata: ")
    )

    # remove temporary parquet file
    expect_true(
      file.remove(parquetfile_metadata)
    )
  }
)

test_that(
  "Product field is correct",
  {
    # temp file to store parquet file
    parquetfile_product <- tempfile(
      pattern = "parquet-file_alt-product-type",
      tmpdir = tempdir(),
      fileext = ".parquet"
    )

    # random product name for testing
    tmp_product_name <- "Unknown_Product"

    write_npx_parquet(
      df = read_NPX(parquet_file) |>
        dplyr::as_tibble() |>
        dplyr::collect(),
      npx_parquet_file = parquetfile_product,
      product = tmp_product_name
    )

    # check that the parquet file was created
    expect(
      file.exists(parquetfile_product),
      failure_message = "Parquet file was not created!"
    )

    # check that relevant error is thrown
    expect_error(
      read_NPX(parquetfile_product),
      regexp = paste0("Unsupported platform: \"", tmp_product_name, "\"")
    )

    # remove temporary parquet file
    expect_true(
      file.remove(parquetfile_product)
    )
  }
)

test_that(
  "DataFileType field is correct",
  {
    # temp file to store parquet file
    parquetfile_datafiletype <- tempfile(
      pattern = "parquet-file_alt-product-type",
      tmpdir = tempdir(),
      fileext = ".parquet"
    )

    # random product name for testing
    tmp_datafiletype_name <- "Unknown File"

    write_npx_parquet(
      df = read_NPX(parquet_file) |>
        dplyr::as_tibble() |>
        dplyr::collect(),
      npx_parquet_file = parquetfile_datafiletype,
      data_file_type = tmp_datafiletype_name
    )

    # check that the parquet file was created
    expect(
      file.exists(parquetfile_datafiletype),
      failure_message = "Parquet file was not created!"
    )

    # check that relevant error is thrown
    expect_error(
      read_NPX(parquetfile_datafiletype),
      regexp = paste0("Unsupported file: \"", tmp_datafiletype_name, "\"")
    )

    # remove temporary parquet file
    expect_true(
      file.remove(parquetfile_datafiletype)
    )
  }
)
