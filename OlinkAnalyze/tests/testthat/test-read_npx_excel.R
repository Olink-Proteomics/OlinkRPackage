test_that(
  "read_npx_excel - works - wide format",
  {
    # file path
    file_synthetic <- file_wide_synthetic_data(
      olink_platform = "Target 96",
      data_type = "NPX",
      n_panels = 3L,
      n_assays = 92L,
      n_samples = 99L,
      show_dev_int_ctrl = TRUE,
      show_int_ctrl = TRUE,
      version = 1L
    )

    skip_if_not(file.exists(file_synthetic))
    skip_if_not_installed(pkg = "readxl")
    skip_if_not_installed(pkg = "writexl")

    # get wide synthetic data
    df_synthetic <- readRDS(file = file_synthetic)

    ## tibble ----

    withr::with_tempfile(
      new = "excel_file",
      pattern = "excel-file-test",
      fileext = ".xlsx",
      code = {
        # write excel file
        writexl::write_xlsx(x = df_synthetic$list_df_wide$df_wide,
                            path = excel_file,
                            col_names = FALSE,
                            format_headers = FALSE)

        # check that the comma delimited file exists
        expect_true(object = file.exists(excel_file))

        # chech that reading the file works
        expect_no_condition(
          object = df_out <- read_npx_excel(file = excel_file,
                                            out_df = "tibble")
        )

        # check that variable exists
        expect_true(object = exists("df_out"))

        expect_true(inherits(x = df_out, what = "tbl_df"))

        # check that the two dataframes are identical
        expect_equal(
          object = df_out,
          expected = df_synthetic$list_df_wide$df_wide
        )
      }
    )

    ## arrow ----

    withr::with_tempfile(
      new = "excel_file",
      pattern = "excel-file-test",
      fileext = ".xlsx",
      code = {
        # write excel file
        writexl::write_xlsx(x = df_synthetic$list_df_wide$df_wide,
                            path = excel_file,
                            col_names = FALSE,
                            format_headers = FALSE)

        # check that the comma delimited file exists
        expect_true(object = file.exists(excel_file))

        # chech that reading the file works
        expect_no_condition(
          object = df_out <- read_npx_excel(file = excel_file,
                                            out_df = "arrow")
        )

        # check that variable exists
        expect_true(object = exists("df_out"))

        expect_true(inherits(x = df_out, what = "ArrowObject"))

        # check that the two dataframes are identical
        expect_equal(
          object = dplyr::as_tibble(df_out),
          expected = df_synthetic$list_df_wide$df_wide
        )
      }
    )
  }
)

test_that(
  "read_npx_excel - works - long format",
  {
    # file path
    file_synthetic <- file_wide_synthetic_data(
      olink_platform = "Target 96",
      data_type = "NPX",
      n_panels = 3L,
      n_assays = 92L,
      n_samples = 99L,
      show_dev_int_ctrl = TRUE,
      show_int_ctrl = TRUE,
      version = 1L
    )

    skip_if_not(file.exists(file_synthetic))
    skip_if_not_installed(pkg = "readxl")
    skip_if_not_installed(pkg = "writexl")

    # get wide synthetic data
    df_synthetic <- readRDS(file = file_synthetic)

    ## tibble ----

    withr::with_tempfile(
      new = "excel_file",
      pattern = "excel-file-test",
      fileext = ".csv",
      code = {
        # write excel file
        writexl::write_xlsx(x = df_synthetic$list_df_long$df_long,
                            path = excel_file,
                            col_names = TRUE,
                            format_headers = FALSE)

        # check that the comma delimited file exists
        expect_true(object = file.exists(excel_file))

        # chech that reading the file works
        expect_no_condition(
          object = df_out <- read_npx_excel(file = excel_file,
                                            out_df = "tibble")
        )

        # check that variable exists
        expect_true(object = exists("df_out"))

        expect_true(inherits(x = df_out, what = "tbl_df"))

        # check that the two dataframes are identical
        expect_equal(
          object = df_out,
          expected = df_synthetic$list_df_long$df_long
        )
      }
    )

    ## arrow ----

    withr::with_tempfile(
      new = "excel_file",
      pattern = "excel-file-test",
      fileext = ".xlsx",
      code = {
        # write excel file
        writexl::write_xlsx(x = df_synthetic$list_df_long$df_long,
                            path = excel_file,
                            col_names = TRUE,
                            format_headers = FALSE)

        # check that the comma delimited file exists
        expect_true(object = file.exists(excel_file))

        # chech that reading the file works
        expect_no_condition(
          object = df_out <- read_npx_excel(file = excel_file,
                                            out_df = "arrow")
        )

        # check that variable exists
        expect_true(object = exists("df_out"))

        expect_true(inherits(x = df_out, what = "ArrowObject"))

        # check that the two dataframes are identical
        expect_equal(
          object = dplyr::as_tibble(df_out),
          expected = df_synthetic$list_df_long$df_long
        )
      }
    )
  }
)

test_that(
  "read_npx_excel - error - file not excel",
  {
    skip_if_not_installed(pkg = "readxl")

    withr::with_tempfile(
      new = "pfile_test",
      pattern = "parquet-file_test",
      fileext = ".parquet",
      code = {

        # write the parquet file from a random data frame
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

        # check that relevant error is thrown
        expect_error(
          object = read_npx_excel(file = pfile_test),
          regexp = "Unable to open excel file:"
        )
      }
    )
  }
)

test_that(
  "read_npx_excel - error - df has one column only",
  {
    skip_if_not_installed(pkg = "readxl")
    skip_if_not_installed(pkg = "writexl")

    withr::with_tempfile(
      new = "excel_file",
      pattern = "excel-file-test",
      fileext = ".xlsx",
      code = {
        # write the excel file from a random data frame
        dplyr::tibble(
          "A" = c(1, 2.2, 3.14)
        ) |>
          writexl::write_xlsx(path = excel_file,
                              col_names = TRUE,
                              format_headers = FALSE)

        # check that the comma delimited file exists
        expect_true(object = file.exists(excel_file))

        # chech that reading the file works
        expect_warning(
          object = read_npx_excel(file = excel_file),
          regexp = "has only one column"
        )
      }
    )
  }
)
