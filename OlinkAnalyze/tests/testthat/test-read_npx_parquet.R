# Test that a relevant error is thrown when a txt file was provided as parquet.
test_that(
  "read_npx_parquet - error - random non-parquet file",
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
  "read_npx_parquet - error - corrupt parquet file",
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

# Test that a relevant error is thrown when required metadata fields are
# missing.
test_that(
  "read_npx_parquet - error - required metadata fields are missing",
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
        df_metadata_list <- rep(x = "NA", times = 3L) |>
          as.list()
        names(df_metadata_list) <- olink_parquet_spec$parquet_metadata[1L:3L] |>
          unname()
        df$metadata <- df_metadata_list

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
  "read_npx_parquet - error - Product field is incorrect",
  {

    withr::with_tempfile(
      new = "pfile_product",
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
          arrow::as_arrow_table()

        # modify metadata
        df_metadata_list <- c(rep(x = "NA", times = 3L),
                              "Unknown_Product",
                              "NPX File") |>
          as.list()
        names(df_metadata_list) <- olink_parquet_spec$parquet_metadata[1L:5L] |>
          unname()
        df$metadata <- df_metadata_list

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
          regexp = "Unsupported product"
        )

      }
    )

  }
)

# Test that a relevant error is thrown when the required metadata filed
# DataFileType contains unexpected entries.
test_that(
  "read_npx_parquet - error - DataFileType field is incorrect",
  {
    withr::with_tempfile(
      new = "pfile_datafiletype",
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
          arrow::as_arrow_table()

        # modify metadata
        df_metadata_list <- c(rep(x = "NA", times = 3L),
                              "ExploreHT",
                              "Unknown File") |>
          as.list()
        names(df_metadata_list) <- olink_parquet_spec$parquet_metadata[1L:5L] |>
          unname()
        df$metadata <- df_metadata_list

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
          regexp = "Unsupported file"
        )

      }
    )

  }
)

# Test that the function works if DataFileType is one of the accepted values.
test_that(
  "read_npx_parquet - works - all DataFileType",
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

    # modify metadata
    df_metadata_list <- c(rep(x = "NA", times = 3L),
                          "ExploreHT",
                          "NA") |>
      as.list()
    names(df_metadata_list) <- olink_parquet_spec$parquet_metadata[1L:5L] |>
      unname()
    df$metadata <- df_metadata_list

    # DataFileType = "NPX File" ----

    withr::with_tempfile(
      new = "pfile_test",
      pattern = "parquet-file_test",
      fileext = ".parquet",
      code = {

        # modify metadata
        df$metadata[olink_parquet_spec$parquet_metadata[5L]] <- "NPX File"

        # write the parquet file
        arrow::write_parquet(
          x = df,
          sink = pfile_test,
          compression = "gzip"
        )

        # check that the file exists
        expect_true(object = file.exists(pfile_test))

        # check that function works
        expect_true(
          object = read_npx_parquet(file = pfile_test,
                                    out_df = "arrow") |>
            inherits(what = "ArrowObject")
        )

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

        # modify metadata ----
        df$metadata[olink_parquet_spec$parquet_metadata[5L]] <-
          "Extended NPX File"

        # write the parquet file
        arrow::write_parquet(
          x = df,
          sink = pfile_test,
          compression = "gzip"
        )

        # check that the file exists
        expect_true(object = file.exists(pfile_test))

        # check that function works
        expect_true(
          object = read_npx_parquet(file = pfile_test,
                                    out_df = "arrow") |>
            inherits(what = "ArrowObject")
        )

        expect_true(
          object = read_npx_parquet(file = pfile_test,
                                    out_df = "tibble") |>
            inherits(what = "tbl_df")
        )

      }
    )

    # DataFileType == "CLI Data Export File" ----

    withr::with_tempfile(
      new = "pfile_test",
      pattern = "parquet-file_test",
      fileext = ".parquet",
      code = {

        # modify metadata ----
        df$metadata[olink_parquet_spec$parquet_metadata[5L]] <-
          "CLI Data Export File"

        # write the parquet file
        arrow::write_parquet(
          x = df,
          sink = pfile_test,
          compression = "gzip"
        )

        # check that the file exists
        expect_true(object = file.exists(pfile_test))

        # check that function works
        expect_true(
          object = read_npx_parquet(file = pfile_test,
                                    out_df = "arrow") |>
            inherits(what = "ArrowObject")
        )

        expect_true(
          object = read_npx_parquet(file = pfile_test,
                                    out_df = "tibble") |>
            inherits(what = "tbl_df")
        )

      }
    )

    # DataFileType == "Internal CLI Data Export File" ----

    withr::with_tempfile(
      new = "pfile_test",
      pattern = "parquet-file_test",
      fileext = ".parquet",
      code = {

        # modify metadata ----
        df$metadata[olink_parquet_spec$parquet_metadata[5L]] <-
          "Internal CLI Data Export File"

        # write the parquet file
        arrow::write_parquet(
          x = df,
          sink = pfile_test,
          compression = "gzip"
        )

        # check that the file exists
        expect_true(object = file.exists(pfile_test))

        # check that function works
        expect_true(
          object = read_npx_parquet(file = pfile_test,
                                    out_df = "arrow") |>
            inherits(what = "ArrowObject")
        )

        expect_true(
          object = read_npx_parquet(file = pfile_test,
                                    out_df = "tibble") |>
            inherits(what = "tbl_df")
        )

      }
    )

    # DataFileType == "R Package Export File" ----

    withr::with_tempfile(
      new = "pfile_test",
      pattern = "parquet-file_test",
      fileext = ".parquet",
      code = {

        # modify metadata ----
        df$metadata[olink_parquet_spec$parquet_metadata[5L]] <-
          "R Package Export File"

        # write the parquet file
        arrow::write_parquet(
          x = df,
          sink = pfile_test,
          compression = "gzip"
        )

        # check that the file exists
        expect_true(object = file.exists(pfile_test))

        # check that function works
        expect_true(
          object = read_npx_parquet(file = pfile_test,
                                    out_df = "arrow") |>
            inherits(what = "ArrowObject")
        )

        expect_true(
          object = read_npx_parquet(file = pfile_test,
                                    out_df = "tibble") |>
            inherits(what = "tbl_df")
        )

      }
    )

  }
)

# Test that the function works if Product is one of the accepted values.
test_that(
  "read_npx_parquet - works - all Product",
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

    # modify metadata
    df_metadata_list <- c(rep(x = "NA", times = 3L),
                          "NA",
                          "NPX File") |>
      as.list()
    names(df_metadata_list) <- olink_parquet_spec$parquet_metadata[1L:5L] |>
      unname()
    df$metadata <- df_metadata_list

    ## Product is "ExploreHT"

    withr::with_tempfile(
      new = "pfile_test",
      pattern = "parquet-file_test",
      fileext = ".parquet",
      code = {

        # modify metadata
        df$metadata[olink_parquet_spec$parquet_metadata[4L]] <- "ExploreHT"

        # write the parquet file
        arrow::write_parquet(
          x = df,
          sink = pfile_test,
          compression = "gzip"
        )

        # check that the file exists
        expect_true(object = file.exists(pfile_test))

        # check that function works
        expect_true(
          object = read_npx_parquet(file = pfile_test,
                                    out_df = "arrow") |>
            inherits(what = "ArrowObject")
        )

        expect_true(
          object = read_npx_parquet(file = pfile_test,
                                    out_df = "tibble") |>
            inherits(what = "tbl_df")
        )

      }
    )

    ## Product is "Explore3072"

    withr::with_tempfile(
      new = "pfile_test",
      pattern = "parquet-file_test",
      fileext = ".parquet",
      code = {

        # modify metadata
        df$metadata[olink_parquet_spec$parquet_metadata[4L]] <- "Explore3072"

        # write the parquet file
        arrow::write_parquet(
          x = df,
          sink = pfile_test,
          compression = "gzip"
        )

        # check that the file exists
        expect_true(object = file.exists(pfile_test))

        # check that function works
        expect_true(
          object = read_npx_parquet(file = pfile_test,
                                    out_df = "arrow") |>
            inherits(what = "ArrowObject")
        )

        expect_true(
          object = read_npx_parquet(file = pfile_test,
                                    out_df = "tibble") |>
            inherits(what = "tbl_df")
        )

      }
    )

  }
)

# Test that the function return correct data frame
test_that(
  "read_npx_parquet - works",
  {
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

        # modify metadata
        df_metadata_list <- c(rep(x = "NA", times = 3L),
                              "Explore3072",
                              "NPX File") |>
          as.list()
        names(df_metadata_list) <- olink_parquet_spec$parquet_metadata[1L:5L] |>
          unname()
        df$metadata <- df_metadata_list

        # write the parquet file
        arrow::write_parquet(
          x = df,
          sink = pfile_test,
          compression = "gzip"
        )

        # check that the file exists
        expect_true(object = file.exists(pfile_test))

        # check that function works
        expect_no_condition(
          object = df_arrow <- read_npx_parquet(file = pfile_test,
                                                out_df = "arrow")
        )

        expect_no_condition(
          object = df_tibble <- read_npx_parquet(file = pfile_test,
                                                 out_df = "tibble")
        )

        # check df
        expect_identical(object = df_tibble,
                         expected = dplyr::as_tibble(df))

        expect_identical(object = dplyr::as_tibble(df_arrow),
                         expected = dplyr::as_tibble(df))

      }
    )

  }
)
