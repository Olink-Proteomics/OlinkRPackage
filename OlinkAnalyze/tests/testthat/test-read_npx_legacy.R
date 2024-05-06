# Test read_npx_legacy_help ----

test_that(
  "read_npx_legacy_help - works",
  {
    # Target 48 NPX ----

    withr::with_tempfile(
      new = "file_wide",
      pattern = "test_long",
      fileext = c(".csv", ".xlsx"),
      code = {

        # files
        file_wide_csv <- file_wide[1L]
        file_wide_xlsx <- file_wide[2L]

        # get wide synthetic data
        df_synthetic <- get_wide_synthetic_data(olink_platform = "Target 48",
                                                data_type = "NPX",
                                                n_panels = 1L,
                                                n_assays = 45L,
                                                n_samples = 88L,
                                                show_dev_int_ctrl = FALSE,
                                                show_int_ctrl = FALSE,
                                                version = 1L)

        # write in csv
        write.table(x = df_synthetic$list_df_wide$df_wide,
                    file = file_wide_csv,
                    append = FALSE,
                    sep = ";",
                    quote = FALSE,
                    eol = "\n",
                    na = "",
                    dec = ".",
                    row.names = FALSE,
                    col.names = FALSE)

        # write in excel
        writexl::write_xlsx(x = df_synthetic$list_df_wide$df_wide,
                            path = file_wide_xlsx,
                            col_names = FALSE,
                            format_headers = FALSE)

        #check that files exist
        expect_true(object = file.exists(file_wide_csv))
        expect_true(object = file.exists(file_wide_xlsx))

        # check that read_npx_format works
        expect_no_error(
          expect_no_warning(
            object = list_npx_legacy_csv <- read_npx_legacy_help(
              file = file_wide_csv,
              out_df = "tibble",
              olink_platform = NULL,
              data_type = NULL
            )
          )
        )

        expect_no_error(
          expect_no_warning(
            object = list_npx_legacy_xlsx <- read_npx_legacy_help(
              file = file_wide_xlsx,
              out_df = "tibble",
              olink_platform = NULL,
              data_type = NULL
            )
          )
        )

        # check that the correct values are returned
        expect_identical(
          object = list_npx_legacy_csv$olink_platform,
          expected = "Target 48"
        )
        expect_identical(
          object = list_npx_legacy_xlsx$olink_platform,
          expected = "Target 48"
        )

        expect_identical(
          object = list_npx_legacy_csv$long_format,
          expected = FALSE
        )
        expect_identical(
          object = list_npx_legacy_xlsx$long_format,
          expected = FALSE
        )

        expect_identical(
          object = list_npx_legacy_csv$data_type,
          expected = "NPX"
        )
        expect_identical(
          object = list_npx_legacy_xlsx$data_type,
          expected = "NPX"
        )

        expect_identical(
          object = list_npx_legacy_csv$npxs_v,
          expected = "Test"
        )
        expect_identical(
          object = list_npx_legacy_xlsx$npxs_v,
          expected = "Test"
        )

        expect_identical(
          object = list_npx_legacy_csv$df_split$df_top,
          expected = df_synthetic$list_df_wide$df_top_wide
        )
        expect_identical(
          object = list_npx_legacy_xlsx$df_split$df_top,
          expected = df_synthetic$list_df_wide$df_top_wide
        )

        expect_identical(
          object = list_npx_legacy_csv$df_split$df_data,
          expected = df_synthetic$list_df_wide$df_middle_wide |>
            dplyr::bind_rows(
              df_synthetic$list_df_wide$df_na_wide
            ) |>
            dplyr::bind_rows(
              df_synthetic$list_df_wide$df_bottom_wide
            )
        )
        expect_identical(
          object = list_npx_legacy_xlsx$df_split$df_data,
          expected = df_synthetic$list_df_wide$df_middle_wide |>
            dplyr::bind_rows(
              df_synthetic$list_df_wide$df_na_wide
            ) |>
            dplyr::bind_rows(
              df_synthetic$list_df_wide$df_bottom_wide
            )
        )
      }
    )

    # Target 48 Quantified ----

    withr::with_tempfile(
      new = "file_wide",
      pattern = "test_long",
      fileext = c(".csv", ".xlsx"),
      code = {

        # files
        file_wide_csv <- file_wide[1L]
        file_wide_xlsx <- file_wide[2L]

        # get wide synthetic data
        df_synthetic <- get_wide_synthetic_data(olink_platform = "Target 48",
                                                data_type = "Quantified",
                                                n_panels = 1L,
                                                n_assays = 45L,
                                                n_samples = 88L,
                                                show_dev_int_ctrl = FALSE,
                                                show_int_ctrl = FALSE,
                                                version = 0L)

        # write in csv
        write.table(x = df_synthetic$list_df_wide$df_wide,
                    file = file_wide_csv,
                    append = FALSE,
                    sep = ";",
                    quote = FALSE,
                    eol = "\n",
                    na = "",
                    dec = ".",
                    row.names = FALSE,
                    col.names = FALSE)

        # write in excel
        writexl::write_xlsx(x = df_synthetic$list_df_wide$df_wide,
                            path = file_wide_xlsx,
                            col_names = FALSE,
                            format_headers = FALSE)

        #check that files exist
        expect_true(object = file.exists(file_wide_csv))
        expect_true(object = file.exists(file_wide_xlsx))

        # check that read_npx_format works
        expect_no_error(
          expect_no_warning(
            object = list_npx_legacy_csv <- read_npx_legacy_help(
              file = file_wide_csv,
              out_df = "tibble",
              olink_platform = NULL,
              data_type = NULL
            )
          )
        )

        expect_no_error(
          expect_no_warning(
            object = list_npx_legacy_xlsx <- read_npx_legacy_help(
              file = file_wide_xlsx,
              out_df = "tibble",
              olink_platform = NULL,
              data_type = NULL
            )
          )
        )

        # check that the correct values are returned
        expect_identical(
          object = list_npx_legacy_csv$olink_platform,
          expected = "Target 48"
        )
        expect_identical(
          object = list_npx_legacy_xlsx$olink_platform,
          expected = "Target 48"
        )

        expect_identical(
          object = list_npx_legacy_csv$long_format,
          expected = FALSE
        )
        expect_identical(
          object = list_npx_legacy_xlsx$long_format,
          expected = FALSE
        )

        expect_identical(
          object = list_npx_legacy_csv$data_type,
          expected = "Quantified"
        )
        expect_identical(
          object = list_npx_legacy_xlsx$data_type,
          expected = "Quantified"
        )

        expect_identical(
          object = list_npx_legacy_csv$npxs_v,
          expected = "Test"
        )
        expect_identical(
          object = list_npx_legacy_xlsx$npxs_v,
          expected = "Test"
        )

        expect_identical(
          object = list_npx_legacy_csv$df_split$df_top,
          expected = df_synthetic$list_df_wide$df_top_wide
        )
        expect_identical(
          object = list_npx_legacy_xlsx$df_split$df_top,
          expected = df_synthetic$list_df_wide$df_top_wide
        )

        expect_identical(
          object = list_npx_legacy_csv$df_split$df_data,
          expected = df_synthetic$list_df_wide$df_middle_wide |>
            dplyr::bind_rows(
              df_synthetic$list_df_wide$df_na_wide
            ) |>
            dplyr::bind_rows(
              df_synthetic$list_df_wide$df_bottom_wide
            )
        )
        expect_identical(
          object = list_npx_legacy_xlsx$df_split$df_data,
          expected = df_synthetic$list_df_wide$df_middle_wide |>
            dplyr::bind_rows(
              df_synthetic$list_df_wide$df_na_wide
            ) |>
            dplyr::bind_rows(
              df_synthetic$list_df_wide$df_bottom_wide
            )
        )
      }
    )

    # Target 96 NPX v1 ----

    withr::with_tempfile(
      new = "file_wide",
      pattern = "test_long",
      fileext = c(".csv", ".xlsx"),
      code = {

        # files
        file_wide_csv <- file_wide[1L]
        file_wide_xlsx <- file_wide[2L]

        # get wide synthetic data
        df_synthetic <- get_wide_synthetic_data(olink_platform = "Target 96",
                                                data_type = "NPX",
                                                n_panels = 1L,
                                                n_assays = 92L,
                                                n_samples = 88L,
                                                show_dev_int_ctrl = FALSE,
                                                show_int_ctrl = FALSE,
                                                version = 1L)

        # write in csv
        write.table(x = df_synthetic$list_df_wide$df_wide,
                    file = file_wide_csv,
                    append = FALSE,
                    sep = ";",
                    quote = FALSE,
                    eol = "\n",
                    na = "",
                    dec = ".",
                    row.names = FALSE,
                    col.names = FALSE)

        # write in excel
        writexl::write_xlsx(x = df_synthetic$list_df_wide$df_wide,
                            path = file_wide_xlsx,
                            col_names = FALSE,
                            format_headers = FALSE)

        #check that files exist
        expect_true(object = file.exists(file_wide_csv))
        expect_true(object = file.exists(file_wide_xlsx))

        # check that read_npx_format works
        expect_no_error(
          expect_no_warning(
            object = list_npx_legacy_csv <- read_npx_legacy_help(
              file = file_wide_csv,
              out_df = "tibble",
              olink_platform = NULL,
              data_type = NULL
            )
          )
        )

        expect_no_error(
          expect_no_warning(
            object = list_npx_legacy_xlsx <- read_npx_legacy_help(
              file = file_wide_xlsx,
              out_df = "tibble",
              olink_platform = NULL,
              data_type = NULL
            )
          )
        )

        # check that the correct values are returned
        expect_identical(
          object = list_npx_legacy_csv$olink_platform,
          expected = "Target 96"
        )
        expect_identical(
          object = list_npx_legacy_xlsx$olink_platform,
          expected = "Target 96"
        )

        expect_identical(
          object = list_npx_legacy_csv$long_format,
          expected = FALSE
        )
        expect_identical(
          object = list_npx_legacy_xlsx$long_format,
          expected = FALSE
        )

        expect_identical(
          object = list_npx_legacy_csv$data_type,
          expected = "NPX"
        )
        expect_identical(
          object = list_npx_legacy_xlsx$data_type,
          expected = "NPX"
        )

        expect_identical(
          object = list_npx_legacy_csv$npxs_v,
          expected = "Test"
        )
        expect_identical(
          object = list_npx_legacy_xlsx$npxs_v,
          expected = "Test"
        )

        expect_identical(
          object = list_npx_legacy_csv$df_split$df_top,
          expected = df_synthetic$list_df_wide$df_top_wide
        )
        expect_identical(
          object = list_npx_legacy_xlsx$df_split$df_top,
          expected = df_synthetic$list_df_wide$df_top_wide
        )

        expect_identical(
          object = list_npx_legacy_csv$df_split$df_data,
          expected = df_synthetic$list_df_wide$df_middle_wide |>
            dplyr::bind_rows(
              df_synthetic$list_df_wide$df_na_wide
            ) |>
            dplyr::bind_rows(
              df_synthetic$list_df_wide$df_bottom_wide
            )
        )
        expect_identical(
          object = list_npx_legacy_xlsx$df_split$df_data,
          expected = df_synthetic$list_df_wide$df_middle_wide |>
            dplyr::bind_rows(
              df_synthetic$list_df_wide$df_na_wide
            ) |>
            dplyr::bind_rows(
              df_synthetic$list_df_wide$df_bottom_wide
            )
        )
      }
    )

    # Target 96 NPX v2 ----

    withr::with_tempfile(
      new = "file_wide",
      pattern = "test_long",
      fileext = c(".csv", ".xlsx"),
      code = {

        # files
        file_wide_csv <- file_wide[1L]
        file_wide_xlsx <- file_wide[2L]

        # get wide synthetic data
        df_synthetic <- get_wide_synthetic_data(olink_platform = "Target 96",
                                                data_type = "NPX",
                                                n_panels = 1L,
                                                n_assays = 92L,
                                                n_samples = 88L,
                                                show_dev_int_ctrl = FALSE,
                                                show_int_ctrl = FALSE,
                                                version = 2L)

        # write in csv
        write.table(x = df_synthetic$list_df_wide$df_wide,
                    file = file_wide_csv,
                    append = FALSE,
                    sep = ";",
                    quote = FALSE,
                    eol = "\n",
                    na = "",
                    dec = ".",
                    row.names = FALSE,
                    col.names = FALSE)

        # write in excel
        writexl::write_xlsx(x = df_synthetic$list_df_wide$df_wide,
                            path = file_wide_xlsx,
                            col_names = FALSE,
                            format_headers = FALSE)

        #check that files exist
        expect_true(object = file.exists(file_wide_csv))
        expect_true(object = file.exists(file_wide_xlsx))

        # check that read_npx_format works
        expect_no_error(
          expect_no_warning(
            object = list_npx_legacy_csv <- read_npx_legacy_help(
              file = file_wide_csv,
              out_df = "tibble",
              olink_platform = NULL,
              data_type = NULL
            )
          )
        )

        expect_no_error(
          expect_no_warning(
            object = list_npx_legacy_xlsx <- read_npx_legacy_help(
              file = file_wide_xlsx,
              out_df = "tibble",
              olink_platform = NULL,
              data_type = NULL
            )
          )
        )

        # check that the correct values are returned
        expect_identical(
          object = list_npx_legacy_csv$olink_platform,
          expected = "Target 96"
        )
        expect_identical(
          object = list_npx_legacy_xlsx$olink_platform,
          expected = "Target 96"
        )

        expect_identical(
          object = list_npx_legacy_csv$long_format,
          expected = FALSE
        )
        expect_identical(
          object = list_npx_legacy_xlsx$long_format,
          expected = FALSE
        )

        expect_identical(
          object = list_npx_legacy_csv$data_type,
          expected = "NPX"
        )
        expect_identical(
          object = list_npx_legacy_xlsx$data_type,
          expected = "NPX"
        )

        expect_identical(
          object = list_npx_legacy_csv$npxs_v,
          expected = "Test"
        )
        expect_identical(
          object = list_npx_legacy_xlsx$npxs_v,
          expected = "Test"
        )

        expect_identical(
          object = list_npx_legacy_csv$df_split$df_top,
          expected = df_synthetic$list_df_wide$df_top_wide
        )
        expect_identical(
          object = list_npx_legacy_xlsx$df_split$df_top,
          expected = df_synthetic$list_df_wide$df_top_wide
        )

        expect_identical(
          object = list_npx_legacy_csv$df_split$df_data,
          expected = df_synthetic$list_df_wide$df_middle_wide |>
            dplyr::bind_rows(
              df_synthetic$list_df_wide$df_na_wide
            ) |>
            dplyr::bind_rows(
              df_synthetic$list_df_wide$df_bottom_wide
            )
        )
        expect_identical(
          object = list_npx_legacy_xlsx$df_split$df_data,
          expected = df_synthetic$list_df_wide$df_middle_wide |>
            dplyr::bind_rows(
              df_synthetic$list_df_wide$df_na_wide
            ) |>
            dplyr::bind_rows(
              df_synthetic$list_df_wide$df_bottom_wide
            )
        )
      }
    )
  }
)

test_that(
  "read_npx_legacy_help - error - long df",
  {
    withr::with_tempfile(
      new = "csv_long",
      pattern = "test_long",
      fileext = ".csv",
      code = {

        # get wide synthetic data
        df_synthetic <- get_wide_synthetic_data(olink_platform = "Target 48",
                                                data_type = "NPX",
                                                n_panels = 1L,
                                                n_assays = 45L,
                                                n_samples = 88L,
                                                show_dev_int_ctrl = FALSE,
                                                show_int_ctrl = FALSE,
                                                version = 2L)

        # write in csv
        write.table(x = df_synthetic$list_df_long$df_long,
                    file = csv_long,
                    append = FALSE,
                    sep = ";",
                    quote = FALSE,
                    eol = "\n",
                    na = "",
                    dec = ".",
                    row.names = FALSE,
                    col.names = TRUE)

        #check that file exists
        expect_true(object = file.exists(csv_long))

        # check that read_npx_format works
        expect_error(
          object = read_npx_legacy_help(
            file = csv_long,
            out_df = "tibble",
            olink_platform = NULL,
            data_type = NULL
          ),
          regexp = "accepts only wide format files!"
        )

      }
    )
  }
)

test_that(
  "read_npx_legacy_help - error - not T48, T96, Flex",
  {
    withr::with_tempfile(
      new = "csv_long",
      pattern = "test_long",
      fileext = ".csv",
      code = {

        # get wide synthetic data
        df_synthetic <- get_wide_synthetic_data(olink_platform = "Focus",
                                                data_type = "NPX",
                                                n_panels = 1L,
                                                n_assays = 33L,
                                                n_samples = 88L,
                                                show_dev_int_ctrl = FALSE,
                                                show_int_ctrl = FALSE,
                                                version = 0L)

        # write in csv
        write.table(x = df_synthetic$list_df_wide$df_wide,
                    file = csv_long,
                    append = FALSE,
                    sep = ";",
                    quote = FALSE,
                    eol = "\n",
                    na = "",
                    dec = ".",
                    row.names = FALSE,
                    col.names = FALSE)

        #check that file exists
        expect_true(object = file.exists(csv_long))

        # check that read_npx_format works
        expect_warning(
          object = expect_error(
            object = read_npx_legacy_help(
              file = csv_long,
              out_df = "tibble",
              olink_platform = "Focus",
              data_type = "NPX"
            ),
            regexp = "accepts only data from \"Target"
          ),
          regexp = "Unable to recognize the Olink platform from the input file"
        )

      }
    )
  }
)

test_that(
  "read_npx_legacy_help - error - not NPX or Quantified",
  {
    withr::with_tempfile(
      new = "csv_long",
      pattern = "test_long",
      fileext = ".csv",
      code = {

        # get wide synthetic data
        df_synthetic <- get_wide_synthetic_data(olink_platform = "Target 48",
                                                data_type = "Ct",
                                                n_panels = 1L,
                                                n_assays = 45L,
                                                n_samples = 88L,
                                                show_dev_int_ctrl = FALSE,
                                                show_int_ctrl = FALSE,
                                                version = 0L)

        # write in csv
        write.table(x = df_synthetic$list_df_wide$df_wide,
                    file = csv_long,
                    append = FALSE,
                    sep = ";",
                    quote = FALSE,
                    eol = "\n",
                    na = "",
                    dec = ".",
                    row.names = FALSE,
                    col.names = FALSE)

        #check that file exists
        expect_true(object = file.exists(csv_long))

        # check that read_npx_format works
        expect_error(
          object = read_npx_legacy_help(
            file = csv_long,
            out_df = "tibble",
            olink_platform = NULL,
            data_type = NULL
          ),
          regexp = "accepts only \"NPX\" and \"Quantified\" data ran on"
        )

      }
    )
  }
)

# Test read_npx_legacy ----

test_that(
  "read_npx_legacy - works - T48 - single panel",
  {
    # Target 48 NPX, no dev int ctrl, no int ctrl, 1 plate, v1 ----

    withr::with_tempfile(
      new = "file_wide",
      pattern = "test_long",
      fileext = c(".csv", ".xlsx"),
      code = {

        # vars
        olink_platform <- "Target 48"

        # files
        file_wide_csv <- file_wide[1L]
        file_wide_xlsx <- file_wide[2L]

        # get wide synthetic data
        df_synthetic <- get_wide_synthetic_data(olink_platform = olink_platform,
                                                data_type = "NPX",
                                                n_panels = 1L,
                                                n_assays = 45L,
                                                n_samples = 88L,
                                                show_dev_int_ctrl = FALSE,
                                                show_int_ctrl = FALSE,
                                                version = 1L)

        # write in csv
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          write.table(
            file = file_wide_csv,
            append = FALSE,
            sep = ";",
            quote = FALSE,
            eol = "\n",
            na = "",
            dec = ".",
            row.names = FALSE,
            col.names = FALSE
          )

        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that files exist
        expect_true(object = file.exists(file_wide_csv))
        expect_true(object = file.exists(file_wide_xlsx))

        # check that function works
        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_csv <- read_npx_legacy(
                file = file_wide_csv,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that excel and csv are identical
        expect_identical(
          object = npx_legacy_xlsx,
          expected = npx_legacy_csv
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_synthetic$list_df_long$df_long,
          long_legacy = npx_legacy_csv,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    # Target 48 NPX, no dev int ctrl, w int ctrl, 1 plate, v1 ----

    withr::with_tempfile(
      new = "file_wide",
      pattern = "test_long",
      fileext = c(".csv", ".xlsx"),
      code = {

        # vars
        olink_platform <- "Target 48"

        # files
        file_wide_csv <- file_wide[1L]
        file_wide_xlsx <- file_wide[2L]

        # get wide synthetic data
        df_synthetic <- get_wide_synthetic_data(olink_platform = olink_platform,
                                                data_type = "NPX",
                                                n_panels = 1L,
                                                n_assays = 45L,
                                                n_samples = 88L,
                                                show_dev_int_ctrl = FALSE,
                                                show_int_ctrl = TRUE,
                                                version = 1L)

        # write in csv
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          write.table(
            file = file_wide_csv,
            append = FALSE,
            sep = ";",
            quote = FALSE,
            eol = "\n",
            na = "",
            dec = ".",
            row.names = FALSE,
            col.names = FALSE
          )

        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that files exist
        expect_true(object = file.exists(file_wide_csv))
        expect_true(object = file.exists(file_wide_xlsx))

        # check that function works
        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_csv <- read_npx_legacy(
                file = file_wide_csv,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that excel and csv are identical
        expect_identical(
          object = npx_legacy_xlsx,
          expected = npx_legacy_csv
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_synthetic$list_df_long$df_long,
          long_legacy = npx_legacy_csv,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    # Target 48 NPX, w dev int ctrl, w int ctrl, 1 plate, v1 ----

    withr::with_tempfile(
      new = "file_wide",
      pattern = "test_long",
      fileext = c(".csv", ".xlsx"),
      code = {

        # vars
        olink_platform <- "Target 48"

        # files
        file_wide_csv <- file_wide[1L]
        file_wide_xlsx <- file_wide[2L]

        # get wide synthetic data
        df_synthetic <- get_wide_synthetic_data(olink_platform = olink_platform,
                                                data_type = "NPX",
                                                n_panels = 1L,
                                                n_assays = 45L,
                                                n_samples = 88L,
                                                show_dev_int_ctrl = TRUE,
                                                show_int_ctrl = TRUE,
                                                version = 1L)

        # write in csv
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          write.table(
            file = file_wide_csv,
            append = FALSE,
            sep = ";",
            quote = FALSE,
            eol = "\n",
            na = "",
            dec = ".",
            row.names = FALSE,
            col.names = FALSE
          )

        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that files exist
        expect_true(object = file.exists(file_wide_csv))
        expect_true(object = file.exists(file_wide_xlsx))

        # check that function works
        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_csv <- read_npx_legacy(
                file = file_wide_csv,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that excel and csv are identical
        expect_identical(
          object = npx_legacy_xlsx,
          expected = npx_legacy_csv
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_synthetic$list_df_long$df_long,
          long_legacy = npx_legacy_csv,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    # Target 48 NPX, no dev int ctrl, no int ctrl, 2 plates, v1 ----

    withr::with_tempfile(
      new = "file_wide",
      pattern = "test_long",
      fileext = c(".csv", ".xlsx"),
      code = {

        # vars
        olink_platform <- "Target 48"

        # files
        file_wide_csv <- file_wide[1L]
        file_wide_xlsx <- file_wide[2L]

        # get wide synthetic data
        df_synthetic <- get_wide_synthetic_data(olink_platform = olink_platform,
                                                data_type = "NPX",
                                                n_panels = 1L,
                                                n_assays = 45L,
                                                n_samples = 99L,
                                                show_dev_int_ctrl = FALSE,
                                                show_int_ctrl = FALSE,
                                                version = 1L)

        # write in csv
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          write.table(
            file = file_wide_csv,
            append = FALSE,
            sep = ";",
            quote = FALSE,
            eol = "\n",
            na = "",
            dec = ".",
            row.names = FALSE,
            col.names = FALSE
          )

        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that files exist
        expect_true(object = file.exists(file_wide_csv))
        expect_true(object = file.exists(file_wide_xlsx))

        # check that function works
        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_csv <- read_npx_legacy(
                file = file_wide_csv,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that excel and csv are identical
        expect_identical(
          object = npx_legacy_xlsx,
          expected = npx_legacy_csv
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_synthetic$list_df_long$df_long,
          long_legacy = npx_legacy_csv,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    # Target 48 NPX, no dev int ctrl, w int ctrl, 2 plates, v1 ----

    withr::with_tempfile(
      new = "file_wide",
      pattern = "test_long",
      fileext = c(".csv", ".xlsx"),
      code = {

        # vars
        olink_platform <- "Target 48"

        # files
        file_wide_csv <- file_wide[1L]
        file_wide_xlsx <- file_wide[2L]

        # get wide synthetic data
        df_synthetic <- get_wide_synthetic_data(olink_platform = olink_platform,
                                                data_type = "NPX",
                                                n_panels = 1L,
                                                n_assays = 45L,
                                                n_samples = 99L,
                                                show_dev_int_ctrl = FALSE,
                                                show_int_ctrl = TRUE,
                                                version = 1L)

        # write in csv
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          write.table(
            file = file_wide_csv,
            append = FALSE,
            sep = ";",
            quote = FALSE,
            eol = "\n",
            na = "",
            dec = ".",
            row.names = FALSE,
            col.names = FALSE
          )

        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that files exist
        expect_true(object = file.exists(file_wide_csv))
        expect_true(object = file.exists(file_wide_xlsx))

        # check that function works
        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_csv <- read_npx_legacy(
                file = file_wide_csv,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that excel and csv are identical
        expect_identical(
          object = npx_legacy_xlsx,
          expected = npx_legacy_csv
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_synthetic$list_df_long$df_long,
          long_legacy = npx_legacy_csv,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    # Target 48 NPX, w dev int ctrl, w int ctrl, 2 plates, v1 ----

    withr::with_tempfile(
      new = "file_wide",
      pattern = "test_long",
      fileext = c(".csv", ".xlsx"),
      code = {

        # vars
        olink_platform <- "Target 48"

        # files
        file_wide_csv <- file_wide[1L]
        file_wide_xlsx <- file_wide[2L]

        # get wide synthetic data
        df_synthetic <- get_wide_synthetic_data(olink_platform = olink_platform,
                                                data_type = "NPX",
                                                n_panels = 1L,
                                                n_assays = 45L,
                                                n_samples = 99L,
                                                show_dev_int_ctrl = TRUE,
                                                show_int_ctrl = TRUE,
                                                version = 1L)

        # write in csv
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          write.table(
            file = file_wide_csv,
            append = FALSE,
            sep = ";",
            quote = FALSE,
            eol = "\n",
            na = "",
            dec = ".",
            row.names = FALSE,
            col.names = FALSE
          )

        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that files exist
        expect_true(object = file.exists(file_wide_csv))
        expect_true(object = file.exists(file_wide_xlsx))

        # check that function works
        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_csv <- read_npx_legacy(
                file = file_wide_csv,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that excel and csv are identical
        expect_identical(
          object = npx_legacy_xlsx,
          expected = npx_legacy_csv
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_synthetic$list_df_long$df_long,
          long_legacy = npx_legacy_csv,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    # Target 48 Quantified, no dev int ctrl, no int ctrl, 1 plate ----

    withr::with_tempfile(
      new = "file_wide",
      pattern = "test_long",
      fileext = c(".csv", ".xlsx"),
      code = {

        # vars
        olink_platform <- "Target 48"

        # files
        file_wide_csv <- file_wide[1L]
        file_wide_xlsx <- file_wide[2L]

        # get wide synthetic data
        df_synthetic <- get_wide_synthetic_data(olink_platform = olink_platform,
                                                data_type = "Quantified",
                                                n_panels = 1L,
                                                n_assays = 45L,
                                                n_samples = 88L,
                                                show_dev_int_ctrl = FALSE,
                                                show_int_ctrl = FALSE,
                                                version = 0L)

        # write in csv
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          write.table(
            file = file_wide_csv,
            append = FALSE,
            sep = ";",
            quote = FALSE,
            eol = "\n",
            na = "",
            dec = ".",
            row.names = FALSE,
            col.names = FALSE
          )

        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that files exist
        expect_true(object = file.exists(file_wide_csv))
        expect_true(object = file.exists(file_wide_xlsx))

        # check that function works
        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_csv <- read_npx_legacy(
                file = file_wide_csv,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that excel and csv are identical
        expect_identical(
          object = npx_legacy_xlsx,
          expected = npx_legacy_csv
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_synthetic$list_df_long$df_long,
          long_legacy = npx_legacy_csv,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    # Target 48 Quantified, no dev int ctrl, w int ctrl, 1 plate ----

    withr::with_tempfile(
      new = "file_wide",
      pattern = "test_long",
      fileext = c(".csv", ".xlsx"),
      code = {

        # vars
        olink_platform <- "Target 48"

        # files
        file_wide_csv <- file_wide[1L]
        file_wide_xlsx <- file_wide[2L]

        # get wide synthetic data
        df_synthetic <- get_wide_synthetic_data(olink_platform = olink_platform,
                                                data_type = "Quantified",
                                                n_panels = 1L,
                                                n_assays = 45L,
                                                n_samples = 88L,
                                                show_dev_int_ctrl = FALSE,
                                                show_int_ctrl = TRUE,
                                                version = 0L)

        # write in csv
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          write.table(
            file = file_wide_csv,
            append = FALSE,
            sep = ";",
            quote = FALSE,
            eol = "\n",
            na = "",
            dec = ".",
            row.names = FALSE,
            col.names = FALSE
          )

        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that files exist
        expect_true(object = file.exists(file_wide_csv))
        expect_true(object = file.exists(file_wide_xlsx))

        # check that function works
        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_csv <- read_npx_legacy(
                file = file_wide_csv,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that excel and csv are identical
        expect_identical(
          object = npx_legacy_xlsx,
          expected = npx_legacy_csv
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_synthetic$list_df_long$df_long,
          long_legacy = npx_legacy_csv,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    # Target 48 Quantified, w dev int ctrl, w int ctrl, 1 plate ----

    withr::with_tempfile(
      new = "file_wide",
      pattern = "test_long",
      fileext = c(".csv", ".xlsx"),
      code = {

        # vars
        olink_platform <- "Target 48"

        # files
        file_wide_csv <- file_wide[1L]
        file_wide_xlsx <- file_wide[2L]

        # get wide synthetic data
        df_synthetic <- get_wide_synthetic_data(olink_platform = olink_platform,
                                                data_type = "Quantified",
                                                n_panels = 1L,
                                                n_assays = 45L,
                                                n_samples = 88L,
                                                show_dev_int_ctrl = TRUE,
                                                show_int_ctrl = TRUE,
                                                version = 0L)

        # write in csv
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          write.table(
            file = file_wide_csv,
            append = FALSE,
            sep = ";",
            quote = FALSE,
            eol = "\n",
            na = "",
            dec = ".",
            row.names = FALSE,
            col.names = FALSE
          )

        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that files exist
        expect_true(object = file.exists(file_wide_csv))
        expect_true(object = file.exists(file_wide_xlsx))

        # check that function works
        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_csv <- read_npx_legacy(
                file = file_wide_csv,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that excel and csv are identical
        expect_identical(
          object = npx_legacy_xlsx,
          expected = npx_legacy_csv
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_synthetic$list_df_long$df_long,
          long_legacy = npx_legacy_csv,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    # Target 48 Quantified, no dev int ctrl, no int ctrl, 2 plates ----

    withr::with_tempfile(
      new = "file_wide",
      pattern = "test_long",
      fileext = c(".csv", ".xlsx"),
      code = {

        # vars
        olink_platform <- "Target 48"

        # files
        file_wide_csv <- file_wide[1L]
        file_wide_xlsx <- file_wide[2L]

        # get wide synthetic data
        df_synthetic <- get_wide_synthetic_data(olink_platform = olink_platform,
                                                data_type = "Quantified",
                                                n_panels = 1L,
                                                n_assays = 45L,
                                                n_samples = 99L,
                                                show_dev_int_ctrl = FALSE,
                                                show_int_ctrl = FALSE,
                                                version = 0L)

        # write in csv
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          write.table(
            file = file_wide_csv,
            append = FALSE,
            sep = ";",
            quote = FALSE,
            eol = "\n",
            na = "",
            dec = ".",
            row.names = FALSE,
            col.names = FALSE
          )

        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that files exist
        expect_true(object = file.exists(file_wide_csv))
        expect_true(object = file.exists(file_wide_xlsx))

        # check that function works
        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_csv <- read_npx_legacy(
                file = file_wide_csv,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that excel and csv are identical
        expect_identical(
          object = npx_legacy_xlsx,
          expected = npx_legacy_csv
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_synthetic$list_df_long$df_long,
          long_legacy = npx_legacy_csv,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    # Target 48 Quantified, no dev int ctrl, w int ctrl, 2 plates ----

    withr::with_tempfile(
      new = "file_wide",
      pattern = "test_long",
      fileext = c(".csv", ".xlsx"),
      code = {

        # vars
        olink_platform <- "Target 48"

        # files
        file_wide_csv <- file_wide[1L]
        file_wide_xlsx <- file_wide[2L]

        # get wide synthetic data
        df_synthetic <- get_wide_synthetic_data(olink_platform = olink_platform,
                                                data_type = "Quantified",
                                                n_panels = 1L,
                                                n_assays = 45L,
                                                n_samples = 99L,
                                                show_dev_int_ctrl = FALSE,
                                                show_int_ctrl = TRUE,
                                                version = 0L)

        # write in csv
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          write.table(
            file = file_wide_csv,
            append = FALSE,
            sep = ";",
            quote = FALSE,
            eol = "\n",
            na = "",
            dec = ".",
            row.names = FALSE,
            col.names = FALSE
          )

        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that files exist
        expect_true(object = file.exists(file_wide_csv))
        expect_true(object = file.exists(file_wide_xlsx))

        # check that function works
        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_csv <- read_npx_legacy(
                file = file_wide_csv,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that excel and csv are identical
        expect_identical(
          object = npx_legacy_xlsx,
          expected = npx_legacy_csv
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_synthetic$list_df_long$df_long,
          long_legacy = npx_legacy_csv,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    # Target 48 Quantified, w dev int ctrl, w int ctrl, 2 plates ----

    withr::with_tempfile(
      new = "file_wide",
      pattern = "test_long",
      fileext = c(".csv", ".xlsx"),
      code = {

        # vars
        olink_platform <- "Target 48"

        # files
        file_wide_csv <- file_wide[1L]
        file_wide_xlsx <- file_wide[2L]

        # get wide synthetic data
        df_synthetic <- get_wide_synthetic_data(olink_platform = olink_platform,
                                                data_type = "Quantified",
                                                n_panels = 1L,
                                                n_assays = 45L,
                                                n_samples = 99L,
                                                show_dev_int_ctrl = TRUE,
                                                show_int_ctrl = TRUE,
                                                version = 0L)

        # write in csv
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          write.table(
            file = file_wide_csv,
            append = FALSE,
            sep = ";",
            quote = FALSE,
            eol = "\n",
            na = "",
            dec = ".",
            row.names = FALSE,
            col.names = FALSE
          )

        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that files exist
        expect_true(object = file.exists(file_wide_csv))
        expect_true(object = file.exists(file_wide_xlsx))

        # check that function works
        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_csv <- read_npx_legacy(
                file = file_wide_csv,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that excel and csv are identical
        expect_identical(
          object = npx_legacy_xlsx,
          expected = npx_legacy_csv
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_synthetic$list_df_long$df_long,
          long_legacy = npx_legacy_csv,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )
  }
)

test_that(
  "read_npx_legacy - works - T48 - multiple panels",
  {
    # Target 48 NPX, no dev int ctrl, no int ctrl, 1 plate, v1 ----

    withr::with_tempfile(
      new = "file_wide",
      pattern = "test_long",
      fileext = c(".csv", ".xlsx"),
      code = {

        # vars
        olink_platform <- "Target 48"

        # files
        file_wide_csv <- file_wide[1L]
        file_wide_xlsx <- file_wide[2L]

        # get wide synthetic data
        df_synthetic <- get_wide_synthetic_data(olink_platform = olink_platform,
                                                data_type = "NPX",
                                                n_panels = 3L,
                                                n_assays = 45L,
                                                n_samples = 88L,
                                                show_dev_int_ctrl = FALSE,
                                                show_int_ctrl = FALSE,
                                                version = 1L)

        # write in csv
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          write.table(
            file = file_wide_csv,
            append = FALSE,
            sep = ";",
            quote = FALSE,
            eol = "\n",
            na = "",
            dec = ".",
            row.names = FALSE,
            col.names = FALSE
          )

        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that files exist
        expect_true(object = file.exists(file_wide_csv))
        expect_true(object = file.exists(file_wide_xlsx))

        # check that function works
        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_csv <- read_npx_legacy(
                file = file_wide_csv,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that excel and csv are identical
        expect_identical(
          object = npx_legacy_xlsx,
          expected = npx_legacy_csv
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_synthetic$list_df_long$df_long,
          long_legacy = npx_legacy_csv,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    # Target 48 NPX, no dev int ctrl, w int ctrl, 1 plate, v1 ----

    withr::with_tempfile(
      new = "file_wide",
      pattern = "test_long",
      fileext = c(".csv", ".xlsx"),
      code = {

        # vars
        olink_platform <- "Target 48"

        # files
        file_wide_csv <- file_wide[1L]
        file_wide_xlsx <- file_wide[2L]

        # get wide synthetic data
        df_synthetic <- get_wide_synthetic_data(olink_platform = olink_platform,
                                                data_type = "NPX",
                                                n_panels = 3L,
                                                n_assays = 45L,
                                                n_samples = 88L,
                                                show_dev_int_ctrl = FALSE,
                                                show_int_ctrl = TRUE,
                                                version = 1L)

        # write in csv
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          write.table(
            file = file_wide_csv,
            append = FALSE,
            sep = ";",
            quote = FALSE,
            eol = "\n",
            na = "",
            dec = ".",
            row.names = FALSE,
            col.names = FALSE
          )

        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that files exist
        expect_true(object = file.exists(file_wide_csv))
        expect_true(object = file.exists(file_wide_xlsx))

        # check that function works
        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_csv <- read_npx_legacy(
                file = file_wide_csv,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that excel and csv are identical
        expect_identical(
          object = npx_legacy_xlsx,
          expected = npx_legacy_csv
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_synthetic$list_df_long$df_long,
          long_legacy = npx_legacy_csv,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    # Target 48 NPX, w dev int ctrl, w int ctrl, 1 plate, v1 ----

    withr::with_tempfile(
      new = "file_wide",
      pattern = "test_long",
      fileext = c(".csv", ".xlsx"),
      code = {

        # vars
        olink_platform <- "Target 48"

        # files
        file_wide_csv <- file_wide[1L]
        file_wide_xlsx <- file_wide[2L]

        # get wide synthetic data
        df_synthetic <- get_wide_synthetic_data(olink_platform = olink_platform,
                                                data_type = "NPX",
                                                n_panels = 3L,
                                                n_assays = 45L,
                                                n_samples = 88L,
                                                show_dev_int_ctrl = TRUE,
                                                show_int_ctrl = TRUE,
                                                version = 1L)

        # write in csv
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          write.table(
            file = file_wide_csv,
            append = FALSE,
            sep = ";",
            quote = FALSE,
            eol = "\n",
            na = "",
            dec = ".",
            row.names = FALSE,
            col.names = FALSE
          )

        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that files exist
        expect_true(object = file.exists(file_wide_csv))
        expect_true(object = file.exists(file_wide_xlsx))

        # check that function works
        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_csv <- read_npx_legacy(
                file = file_wide_csv,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that excel and csv are identical
        expect_identical(
          object = npx_legacy_xlsx,
          expected = npx_legacy_csv
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_synthetic$list_df_long$df_long,
          long_legacy = npx_legacy_csv,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    # Target 48 NPX, no dev int ctrl, no int ctrl, 2 plates, v1 ----

    withr::with_tempfile(
      new = "file_wide",
      pattern = "test_long",
      fileext = c(".csv", ".xlsx"),
      code = {

        # vars
        olink_platform <- "Target 48"

        # files
        file_wide_csv <- file_wide[1L]
        file_wide_xlsx <- file_wide[2L]

        # get wide synthetic data
        df_synthetic <- get_wide_synthetic_data(olink_platform = olink_platform,
                                                data_type = "NPX",
                                                n_panels = 3L,
                                                n_assays = 45L,
                                                n_samples = 99L,
                                                show_dev_int_ctrl = FALSE,
                                                show_int_ctrl = FALSE,
                                                version = 1L)

        # write in csv
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          write.table(
            file = file_wide_csv,
            append = FALSE,
            sep = ";",
            quote = FALSE,
            eol = "\n",
            na = "",
            dec = ".",
            row.names = FALSE,
            col.names = FALSE
          )

        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that files exist
        expect_true(object = file.exists(file_wide_csv))
        expect_true(object = file.exists(file_wide_xlsx))

        # check that function works
        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_csv <- read_npx_legacy(
                file = file_wide_csv,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that excel and csv are identical
        expect_identical(
          object = npx_legacy_xlsx,
          expected = npx_legacy_csv
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_synthetic$list_df_long$df_long,
          long_legacy = npx_legacy_csv,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    # Target 48 NPX, no dev int ctrl, w int ctrl, 2 plates, v1 ----

    withr::with_tempfile(
      new = "file_wide",
      pattern = "test_long",
      fileext = c(".csv", ".xlsx"),
      code = {

        # vars
        olink_platform <- "Target 48"

        # files
        file_wide_csv <- file_wide[1L]
        file_wide_xlsx <- file_wide[2L]

        # get wide synthetic data
        df_synthetic <- get_wide_synthetic_data(olink_platform = olink_platform,
                                                data_type = "NPX",
                                                n_panels = 3L,
                                                n_assays = 45L,
                                                n_samples = 99L,
                                                show_dev_int_ctrl = FALSE,
                                                show_int_ctrl = TRUE,
                                                version = 1L)

        # write in csv
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          write.table(
            file = file_wide_csv,
            append = FALSE,
            sep = ";",
            quote = FALSE,
            eol = "\n",
            na = "",
            dec = ".",
            row.names = FALSE,
            col.names = FALSE
          )

        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that files exist
        expect_true(object = file.exists(file_wide_csv))
        expect_true(object = file.exists(file_wide_xlsx))

        # check that function works
        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_csv <- read_npx_legacy(
                file = file_wide_csv,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that excel and csv are identical
        expect_identical(
          object = npx_legacy_xlsx,
          expected = npx_legacy_csv
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_synthetic$list_df_long$df_long,
          long_legacy = npx_legacy_csv,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    # Target 48 NPX, w dev int ctrl, w int ctrl, 2 plate, v1 ----

    withr::with_tempfile(
      new = "file_wide",
      pattern = "test_long",
      fileext = c(".csv", ".xlsx"),
      code = {

        # vars
        olink_platform <- "Target 48"

        # files
        file_wide_csv <- file_wide[1L]
        file_wide_xlsx <- file_wide[2L]

        # get wide synthetic data
        df_synthetic <- get_wide_synthetic_data(olink_platform = olink_platform,
                                                data_type = "NPX",
                                                n_panels = 3L,
                                                n_assays = 45L,
                                                n_samples = 99L,
                                                show_dev_int_ctrl = TRUE,
                                                show_int_ctrl = TRUE,
                                                version = 1L)

        # write in csv
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          write.table(
            file = file_wide_csv,
            append = FALSE,
            sep = ";",
            quote = FALSE,
            eol = "\n",
            na = "",
            dec = ".",
            row.names = FALSE,
            col.names = FALSE
          )

        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that files exist
        expect_true(object = file.exists(file_wide_csv))
        expect_true(object = file.exists(file_wide_xlsx))

        # check that function works
        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_csv <- read_npx_legacy(
                file = file_wide_csv,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that excel and csv are identical
        expect_identical(
          object = npx_legacy_xlsx,
          expected = npx_legacy_csv
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_synthetic$list_df_long$df_long,
          long_legacy = npx_legacy_csv,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    # Target 48 Quantified, no dev int ctrl, no int ctrl, 1 plate ----

    withr::with_tempfile(
      new = "file_wide",
      pattern = "test_long",
      fileext = c(".csv", ".xlsx"),
      code = {

        # vars
        olink_platform <- "Target 48"

        # files
        file_wide_csv <- file_wide[1L]
        file_wide_xlsx <- file_wide[2L]

        # get wide synthetic data
        df_synthetic <- get_wide_synthetic_data(olink_platform = olink_platform,
                                                data_type = "Quantified",
                                                n_panels = 3L,
                                                n_assays = 45L,
                                                n_samples = 88L,
                                                show_dev_int_ctrl = FALSE,
                                                show_int_ctrl = FALSE,
                                                version = 0L)

        # write in csv
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          write.table(
            file = file_wide_csv,
            append = FALSE,
            sep = ";",
            quote = FALSE,
            eol = "\n",
            na = "",
            dec = ".",
            row.names = FALSE,
            col.names = FALSE
          )

        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that files exist
        expect_true(object = file.exists(file_wide_csv))
        expect_true(object = file.exists(file_wide_xlsx))

        # check that function works
        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_csv <- read_npx_legacy(
                file = file_wide_csv,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that excel and csv are identical
        expect_identical(
          object = npx_legacy_xlsx,
          expected = npx_legacy_csv
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_synthetic$list_df_long$df_long,
          long_legacy = npx_legacy_csv,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    # Target 48 Quantified, no dev int ctrl, w int ctrl, 1 plate ----

    withr::with_tempfile(
      new = "file_wide",
      pattern = "test_long",
      fileext = c(".csv", ".xlsx"),
      code = {

        # vars
        olink_platform <- "Target 48"

        # files
        file_wide_csv <- file_wide[1L]
        file_wide_xlsx <- file_wide[2L]

        # get wide synthetic data
        df_synthetic <- get_wide_synthetic_data(olink_platform = olink_platform,
                                                data_type = "Quantified",
                                                n_panels = 3L,
                                                n_assays = 45L,
                                                n_samples = 88L,
                                                show_dev_int_ctrl = FALSE,
                                                show_int_ctrl = TRUE,
                                                version = 0L)

        # write in csv
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          write.table(
            file = file_wide_csv,
            append = FALSE,
            sep = ";",
            quote = FALSE,
            eol = "\n",
            na = "",
            dec = ".",
            row.names = FALSE,
            col.names = FALSE
          )

        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that files exist
        expect_true(object = file.exists(file_wide_csv))
        expect_true(object = file.exists(file_wide_xlsx))

        # check that function works
        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_csv <- read_npx_legacy(
                file = file_wide_csv,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that excel and csv are identical
        expect_identical(
          object = npx_legacy_xlsx,
          expected = npx_legacy_csv
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_synthetic$list_df_long$df_long,
          long_legacy = npx_legacy_csv,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    # Target 48 Quantified, w dev int ctrl, w int ctrl, 1 plate ----

    withr::with_tempfile(
      new = "file_wide",
      pattern = "test_long",
      fileext = c(".csv", ".xlsx"),
      code = {

        # vars
        olink_platform <- "Target 48"

        # files
        file_wide_csv <- file_wide[1L]
        file_wide_xlsx <- file_wide[2L]

        # get wide synthetic data
        df_synthetic <- get_wide_synthetic_data(olink_platform = olink_platform,
                                                data_type = "Quantified",
                                                n_panels = 3L,
                                                n_assays = 45L,
                                                n_samples = 88L,
                                                show_dev_int_ctrl = TRUE,
                                                show_int_ctrl = TRUE,
                                                version = 0L)

        # write in csv
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          write.table(
            file = file_wide_csv,
            append = FALSE,
            sep = ";",
            quote = FALSE,
            eol = "\n",
            na = "",
            dec = ".",
            row.names = FALSE,
            col.names = FALSE
          )

        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that files exist
        expect_true(object = file.exists(file_wide_csv))
        expect_true(object = file.exists(file_wide_xlsx))

        # check that function works
        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_csv <- read_npx_legacy(
                file = file_wide_csv,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that excel and csv are identical
        expect_identical(
          object = npx_legacy_xlsx,
          expected = npx_legacy_csv
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_synthetic$list_df_long$df_long,
          long_legacy = npx_legacy_csv,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    # Target 48 Quantified, no dev int ctrl, no int ctrl, 2 plates ----

    withr::with_tempfile(
      new = "file_wide",
      pattern = "test_long",
      fileext = c(".csv", ".xlsx"),
      code = {

        # vars
        olink_platform <- "Target 48"

        # files
        file_wide_csv <- file_wide[1L]
        file_wide_xlsx <- file_wide[2L]

        # get wide synthetic data
        df_synthetic <- get_wide_synthetic_data(olink_platform = olink_platform,
                                                data_type = "Quantified",
                                                n_panels = 3L,
                                                n_assays = 45L,
                                                n_samples = 99L,
                                                show_dev_int_ctrl = FALSE,
                                                show_int_ctrl = FALSE,
                                                version = 0L)

        # write in csv
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          write.table(
            file = file_wide_csv,
            append = FALSE,
            sep = ";",
            quote = FALSE,
            eol = "\n",
            na = "",
            dec = ".",
            row.names = FALSE,
            col.names = FALSE
          )

        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that files exist
        expect_true(object = file.exists(file_wide_csv))
        expect_true(object = file.exists(file_wide_xlsx))

        # check that function works
        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_csv <- read_npx_legacy(
                file = file_wide_csv,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that excel and csv are identical
        expect_identical(
          object = npx_legacy_xlsx,
          expected = npx_legacy_csv
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_synthetic$list_df_long$df_long,
          long_legacy = npx_legacy_csv,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    # Target 48 Quantified, no dev int ctrl, w int ctrl, 2 plates ----

    withr::with_tempfile(
      new = "file_wide",
      pattern = "test_long",
      fileext = c(".csv", ".xlsx"),
      code = {

        # vars
        olink_platform <- "Target 48"

        # files
        file_wide_csv <- file_wide[1L]
        file_wide_xlsx <- file_wide[2L]

        # get wide synthetic data
        df_synthetic <- get_wide_synthetic_data(olink_platform = olink_platform,
                                                data_type = "Quantified",
                                                n_panels = 3L,
                                                n_assays = 45L,
                                                n_samples = 99L,
                                                show_dev_int_ctrl = FALSE,
                                                show_int_ctrl = TRUE,
                                                version = 0L)

        # write in csv
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          write.table(
            file = file_wide_csv,
            append = FALSE,
            sep = ";",
            quote = FALSE,
            eol = "\n",
            na = "",
            dec = ".",
            row.names = FALSE,
            col.names = FALSE
          )

        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that files exist
        expect_true(object = file.exists(file_wide_csv))
        expect_true(object = file.exists(file_wide_xlsx))

        # check that function works
        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_csv <- read_npx_legacy(
                file = file_wide_csv,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that excel and csv are identical
        expect_identical(
          object = npx_legacy_xlsx,
          expected = npx_legacy_csv
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_synthetic$list_df_long$df_long,
          long_legacy = npx_legacy_csv,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    # Target 48 Quantified, w dev int ctrl, w int ctrl, 2 plates ----

    withr::with_tempfile(
      new = "file_wide",
      pattern = "test_long",
      fileext = c(".csv", ".xlsx"),
      code = {

        # vars
        olink_platform <- "Target 48"

        # files
        file_wide_csv <- file_wide[1L]
        file_wide_xlsx <- file_wide[2L]

        # get wide synthetic data
        df_synthetic <- get_wide_synthetic_data(olink_platform = olink_platform,
                                                data_type = "Quantified",
                                                n_panels = 3L,
                                                n_assays = 45L,
                                                n_samples = 99L,
                                                show_dev_int_ctrl = TRUE,
                                                show_int_ctrl = TRUE,
                                                version = 0L)

        # write in csv
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          write.table(
            file = file_wide_csv,
            append = FALSE,
            sep = ";",
            quote = FALSE,
            eol = "\n",
            na = "",
            dec = ".",
            row.names = FALSE,
            col.names = FALSE
          )

        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that files exist
        expect_true(object = file.exists(file_wide_csv))
        expect_true(object = file.exists(file_wide_xlsx))

        # check that function works
        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_csv <- read_npx_legacy(
                file = file_wide_csv,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that excel and csv are identical
        expect_identical(
          object = npx_legacy_xlsx,
          expected = npx_legacy_csv
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_synthetic$list_df_long$df_long,
          long_legacy = npx_legacy_csv,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )
  }
)

test_that(
  "read_npx_legacy - works - T96 - single panel",
  {
    # Target 96 NPX, no dev int ctrl, no int ctrl, 1 plate, v1 ----

    withr::with_tempfile(
      new = "file_wide",
      pattern = "test_long",
      fileext = c(".csv", ".xlsx"),
      code = {

        # vars
        olink_platform <- "Target 96"

        # files
        file_wide_csv <- file_wide[1L]
        file_wide_xlsx <- file_wide[2L]

        # get wide synthetic data
        df_synthetic <- get_wide_synthetic_data(olink_platform = olink_platform,
                                                data_type = "NPX",
                                                n_panels = 1L,
                                                n_assays = 92L,
                                                n_samples = 88L,
                                                show_dev_int_ctrl = FALSE,
                                                show_int_ctrl = FALSE,
                                                version = 1L)

        # write in csv
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          write.table(
            file = file_wide_csv,
            append = FALSE,
            sep = ";",
            quote = FALSE,
            eol = "\n",
            na = "",
            dec = ".",
            row.names = FALSE,
            col.names = FALSE
          )

        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that files exist
        expect_true(object = file.exists(file_wide_csv))
        expect_true(object = file.exists(file_wide_xlsx))

        # check that function works
        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_csv <- read_npx_legacy(
                file = file_wide_csv,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that excel and csv are identical
        expect_identical(
          object = npx_legacy_xlsx,
          expected = npx_legacy_csv
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_synthetic$list_df_long$df_long,
          long_legacy = npx_legacy_csv,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    # Target 96 NPX, no dev int ctrl, w int ctrl, 1 plate, v1 ----

    withr::with_tempfile(
      new = "file_wide",
      pattern = "test_long",
      fileext = c(".csv", ".xlsx"),
      code = {

        # vars
        olink_platform <- "Target 96"

        # files
        file_wide_csv <- file_wide[1L]
        file_wide_xlsx <- file_wide[2L]

        # get wide synthetic data
        df_synthetic <- get_wide_synthetic_data(olink_platform = olink_platform,
                                                data_type = "NPX",
                                                n_panels = 1L,
                                                n_assays = 92L,
                                                n_samples = 88L,
                                                show_dev_int_ctrl = FALSE,
                                                show_int_ctrl = TRUE,
                                                version = 1L)

        # write in csv
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          write.table(
            file = file_wide_csv,
            append = FALSE,
            sep = ";",
            quote = FALSE,
            eol = "\n",
            na = "",
            dec = ".",
            row.names = FALSE,
            col.names = FALSE
          )

        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that files exist
        expect_true(object = file.exists(file_wide_csv))
        expect_true(object = file.exists(file_wide_xlsx))

        # check that function works
        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_csv <- read_npx_legacy(
                file = file_wide_csv,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that excel and csv are identical
        expect_identical(
          object = npx_legacy_xlsx,
          expected = npx_legacy_csv
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_synthetic$list_df_long$df_long,
          long_legacy = npx_legacy_csv,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    # Target 96 NPX, w dev int ctrl, w int ctrl, 1 plate, v1 ----

    withr::with_tempfile(
      new = "file_wide",
      pattern = "test_long",
      fileext = c(".csv", ".xlsx"),
      code = {

        # vars
        olink_platform <- "Target 96"

        # files
        file_wide_csv <- file_wide[1L]
        file_wide_xlsx <- file_wide[2L]

        # get wide synthetic data
        df_synthetic <- get_wide_synthetic_data(olink_platform = olink_platform,
                                                data_type = "NPX",
                                                n_panels = 1L,
                                                n_assays = 92L,
                                                n_samples = 88L,
                                                show_dev_int_ctrl = TRUE,
                                                show_int_ctrl = TRUE,
                                                version = 1L)

        # write in csv
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          write.table(
            file = file_wide_csv,
            append = FALSE,
            sep = ";",
            quote = FALSE,
            eol = "\n",
            na = "",
            dec = ".",
            row.names = FALSE,
            col.names = FALSE
          )

        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that files exist
        expect_true(object = file.exists(file_wide_csv))
        expect_true(object = file.exists(file_wide_xlsx))

        # check that function works
        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_csv <- read_npx_legacy(
                file = file_wide_csv,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that excel and csv are identical
        expect_identical(
          object = npx_legacy_xlsx,
          expected = npx_legacy_csv
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_synthetic$list_df_long$df_long,
          long_legacy = npx_legacy_csv,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    # Target 96 NPX, no dev int ctrl, no int ctrl, 2 plates, v1 ----

    withr::with_tempfile(
      new = "file_wide",
      pattern = "test_long",
      fileext = c(".csv", ".xlsx"),
      code = {

        # vars
        olink_platform <- "Target 96"

        # files
        file_wide_csv <- file_wide[1L]
        file_wide_xlsx <- file_wide[2L]

        # get wide synthetic data
        df_synthetic <- get_wide_synthetic_data(olink_platform = olink_platform,
                                                data_type = "NPX",
                                                n_panels = 1L,
                                                n_assays = 92L,
                                                n_samples = 99L,
                                                show_dev_int_ctrl = FALSE,
                                                show_int_ctrl = FALSE,
                                                version = 1L)

        # write in csv
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          write.table(
            file = file_wide_csv,
            append = FALSE,
            sep = ";",
            quote = FALSE,
            eol = "\n",
            na = "",
            dec = ".",
            row.names = FALSE,
            col.names = FALSE
          )

        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that files exist
        expect_true(object = file.exists(file_wide_csv))
        expect_true(object = file.exists(file_wide_xlsx))

        # check that function works
        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_csv <- read_npx_legacy(
                file = file_wide_csv,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that excel and csv are identical
        expect_identical(
          object = npx_legacy_xlsx,
          expected = npx_legacy_csv
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_synthetic$list_df_long$df_long,
          long_legacy = npx_legacy_csv,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    # Target 96 NPX, no dev int ctrl, w int ctrl, 2 plates, v1 ----

    withr::with_tempfile(
      new = "file_wide",
      pattern = "test_long",
      fileext = c(".csv", ".xlsx"),
      code = {

        # vars
        olink_platform <- "Target 96"

        # files
        file_wide_csv <- file_wide[1L]
        file_wide_xlsx <- file_wide[2L]

        # get wide synthetic data
        df_synthetic <- get_wide_synthetic_data(olink_platform = olink_platform,
                                                data_type = "NPX",
                                                n_panels = 1L,
                                                n_assays = 92L,
                                                n_samples = 99L,
                                                show_dev_int_ctrl = FALSE,
                                                show_int_ctrl = TRUE,
                                                version = 1L)

        # write in csv
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          write.table(
            file = file_wide_csv,
            append = FALSE,
            sep = ";",
            quote = FALSE,
            eol = "\n",
            na = "",
            dec = ".",
            row.names = FALSE,
            col.names = FALSE
          )

        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that files exist
        expect_true(object = file.exists(file_wide_csv))
        expect_true(object = file.exists(file_wide_xlsx))

        # check that function works
        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_csv <- read_npx_legacy(
                file = file_wide_csv,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that excel and csv are identical
        expect_identical(
          object = npx_legacy_xlsx,
          expected = npx_legacy_csv
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_synthetic$list_df_long$df_long,
          long_legacy = npx_legacy_csv,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    # Target 96 NPX, w dev int ctrl, w int ctrl, 2 plates, v1 ----

    withr::with_tempfile(
      new = "file_wide",
      pattern = "test_long",
      fileext = c(".csv", ".xlsx"),
      code = {

        # vars
        olink_platform <- "Target 96"

        # files
        file_wide_csv <- file_wide[1L]
        file_wide_xlsx <- file_wide[2L]

        # get wide synthetic data
        df_synthetic <- get_wide_synthetic_data(olink_platform = olink_platform,
                                                data_type = "NPX",
                                                n_panels = 1L,
                                                n_assays = 92L,
                                                n_samples = 99L,
                                                show_dev_int_ctrl = TRUE,
                                                show_int_ctrl = TRUE,
                                                version = 1L)

        # write in csv
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          write.table(
            file = file_wide_csv,
            append = FALSE,
            sep = ";",
            quote = FALSE,
            eol = "\n",
            na = "",
            dec = ".",
            row.names = FALSE,
            col.names = FALSE
          )

        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that files exist
        expect_true(object = file.exists(file_wide_csv))
        expect_true(object = file.exists(file_wide_xlsx))

        # check that function works
        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_csv <- read_npx_legacy(
                file = file_wide_csv,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that excel and csv are identical
        expect_identical(
          object = npx_legacy_xlsx,
          expected = npx_legacy_csv
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_synthetic$list_df_long$df_long,
          long_legacy = npx_legacy_csv,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    # Target 96 NPX, no dev int ctrl, no int ctrl, 1 plate, v2 ----

    withr::with_tempfile(
      new = "file_wide",
      pattern = "test_long",
      fileext = c(".csv", ".xlsx"),
      code = {

        # vars
        olink_platform <- "Target 96"

        # files
        file_wide_csv <- file_wide[1L]
        file_wide_xlsx <- file_wide[2L]

        # get wide synthetic data
        df_synthetic <- get_wide_synthetic_data(olink_platform = olink_platform,
                                                data_type = "NPX",
                                                n_panels = 1L,
                                                n_assays = 92L,
                                                n_samples = 88L,
                                                show_dev_int_ctrl = FALSE,
                                                show_int_ctrl = FALSE,
                                                version = 2L)

        # write in csv
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          write.table(
            file = file_wide_csv,
            append = FALSE,
            sep = ";",
            quote = FALSE,
            eol = "\n",
            na = "",
            dec = ".",
            row.names = FALSE,
            col.names = FALSE
          )

        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that files exist
        expect_true(object = file.exists(file_wide_csv))
        expect_true(object = file.exists(file_wide_xlsx))

        # check that function works
        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_csv <- read_npx_legacy(
                file = file_wide_csv,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that excel and csv are identical
        expect_identical(
          object = npx_legacy_xlsx,
          expected = npx_legacy_csv
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_synthetic$list_df_long$df_long,
          long_legacy = npx_legacy_csv,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    # Target 96 NPX, no dev int ctrl, w int ctrl, 1 plate, v2 ----

    withr::with_tempfile(
      new = "file_wide",
      pattern = "test_long",
      fileext = c(".csv", ".xlsx"),
      code = {

        # vars
        olink_platform <- "Target 96"

        # files
        file_wide_csv <- file_wide[1L]
        file_wide_xlsx <- file_wide[2L]

        # get wide synthetic data
        df_synthetic <- get_wide_synthetic_data(olink_platform = olink_platform,
                                                data_type = "NPX",
                                                n_panels = 1L,
                                                n_assays = 92L,
                                                n_samples = 88L,
                                                show_dev_int_ctrl = FALSE,
                                                show_int_ctrl = TRUE,
                                                version = 2L)

        # write in csv
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          write.table(
            file = file_wide_csv,
            append = FALSE,
            sep = ";",
            quote = FALSE,
            eol = "\n",
            na = "",
            dec = ".",
            row.names = FALSE,
            col.names = FALSE
          )

        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that files exist
        expect_true(object = file.exists(file_wide_csv))
        expect_true(object = file.exists(file_wide_xlsx))

        # check that function works
        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_csv <- read_npx_legacy(
                file = file_wide_csv,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that excel and csv are identical
        expect_identical(
          object = npx_legacy_xlsx,
          expected = npx_legacy_csv
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_synthetic$list_df_long$df_long,
          long_legacy = npx_legacy_csv,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    # Target 96 NPX, w dev int ctrl, w int ctrl, 1 plate, v2 ----

    withr::with_tempfile(
      new = "file_wide",
      pattern = "test_long",
      fileext = c(".csv", ".xlsx"),
      code = {

        # vars
        olink_platform <- "Target 96"

        # files
        file_wide_csv <- file_wide[1L]
        file_wide_xlsx <- file_wide[2L]

        # get wide synthetic data
        df_synthetic <- get_wide_synthetic_data(olink_platform = olink_platform,
                                                data_type = "NPX",
                                                n_panels = 1L,
                                                n_assays = 92L,
                                                n_samples = 88L,
                                                show_dev_int_ctrl = TRUE,
                                                show_int_ctrl = TRUE,
                                                version = 2L)

        # write in csv
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          write.table(
            file = file_wide_csv,
            append = FALSE,
            sep = ";",
            quote = FALSE,
            eol = "\n",
            na = "",
            dec = ".",
            row.names = FALSE,
            col.names = FALSE
          )

        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that files exist
        expect_true(object = file.exists(file_wide_csv))
        expect_true(object = file.exists(file_wide_xlsx))

        # check that function works
        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_csv <- read_npx_legacy(
                file = file_wide_csv,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that excel and csv are identical
        expect_identical(
          object = npx_legacy_xlsx,
          expected = npx_legacy_csv
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_synthetic$list_df_long$df_long,
          long_legacy = npx_legacy_csv,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    # Target 96 NPX, no dev int ctrl, no int ctrl, 2 plates, v2 ----

    withr::with_tempfile(
      new = "file_wide",
      pattern = "test_long",
      fileext = c(".csv", ".xlsx"),
      code = {

        # vars
        olink_platform <- "Target 96"

        # files
        file_wide_csv <- file_wide[1L]
        file_wide_xlsx <- file_wide[2L]

        # get wide synthetic data
        df_synthetic <- get_wide_synthetic_data(olink_platform = olink_platform,
                                                data_type = "NPX",
                                                n_panels = 1L,
                                                n_assays = 92L,
                                                n_samples = 99L,
                                                show_dev_int_ctrl = FALSE,
                                                show_int_ctrl = FALSE,
                                                version = 2L)

        # write in csv
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          write.table(
            file = file_wide_csv,
            append = FALSE,
            sep = ";",
            quote = FALSE,
            eol = "\n",
            na = "",
            dec = ".",
            row.names = FALSE,
            col.names = FALSE
          )

        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that files exist
        expect_true(object = file.exists(file_wide_csv))
        expect_true(object = file.exists(file_wide_xlsx))

        # check that function works
        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_csv <- read_npx_legacy(
                file = file_wide_csv,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that excel and csv are identical
        expect_identical(
          object = npx_legacy_xlsx,
          expected = npx_legacy_csv
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_synthetic$list_df_long$df_long,
          long_legacy = npx_legacy_csv,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    # Target 96 NPX, no dev int ctrl, w int ctrl, 2 plates, v2 ----

    withr::with_tempfile(
      new = "file_wide",
      pattern = "test_long",
      fileext = c(".csv", ".xlsx"),
      code = {

        # vars
        olink_platform <- "Target 96"

        # files
        file_wide_csv <- file_wide[1L]
        file_wide_xlsx <- file_wide[2L]

        # get wide synthetic data
        df_synthetic <- get_wide_synthetic_data(olink_platform = olink_platform,
                                                data_type = "NPX",
                                                n_panels = 1L,
                                                n_assays = 92L,
                                                n_samples = 99L,
                                                show_dev_int_ctrl = FALSE,
                                                show_int_ctrl = TRUE,
                                                version = 2L)

        # write in csv
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          write.table(
            file = file_wide_csv,
            append = FALSE,
            sep = ";",
            quote = FALSE,
            eol = "\n",
            na = "",
            dec = ".",
            row.names = FALSE,
            col.names = FALSE
          )

        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that files exist
        expect_true(object = file.exists(file_wide_csv))
        expect_true(object = file.exists(file_wide_xlsx))

        # check that function works
        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_csv <- read_npx_legacy(
                file = file_wide_csv,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that excel and csv are identical
        expect_identical(
          object = npx_legacy_xlsx,
          expected = npx_legacy_csv
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_synthetic$list_df_long$df_long,
          long_legacy = npx_legacy_csv,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    # Target 96 NPX, w dev int ctrl, w int ctrl, 2 plates, v2 ----

    withr::with_tempfile(
      new = "file_wide",
      pattern = "test_long",
      fileext = c(".csv", ".xlsx"),
      code = {

        # vars
        olink_platform <- "Target 96"

        # files
        file_wide_csv <- file_wide[1L]
        file_wide_xlsx <- file_wide[2L]

        # get wide synthetic data
        df_synthetic <- get_wide_synthetic_data(olink_platform = olink_platform,
                                                data_type = "NPX",
                                                n_panels = 1L,
                                                n_assays = 92L,
                                                n_samples = 99L,
                                                show_dev_int_ctrl = TRUE,
                                                show_int_ctrl = TRUE,
                                                version = 2L)

        # write in csv
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          write.table(
            file = file_wide_csv,
            append = FALSE,
            sep = ";",
            quote = FALSE,
            eol = "\n",
            na = "",
            dec = ".",
            row.names = FALSE,
            col.names = FALSE
          )

        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that files exist
        expect_true(object = file.exists(file_wide_csv))
        expect_true(object = file.exists(file_wide_xlsx))

        # check that function works
        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_csv <- read_npx_legacy(
                file = file_wide_csv,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that excel and csv are identical
        expect_identical(
          object = npx_legacy_xlsx,
          expected = npx_legacy_csv
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_synthetic$list_df_long$df_long,
          long_legacy = npx_legacy_csv,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )
  }
)

test_that(
  "read_npx_legacy - works - T96 - multiple panels",
  {
    # Target 96 NPX, no dev int ctrl, no int ctrl, 1 plate, v1 ----

    withr::with_tempfile(
      new = "file_wide",
      pattern = "test_long",
      fileext = c(".csv", ".xlsx"),
      code = {

        # vars
        olink_platform <- "Target 96"

        # files
        file_wide_csv <- file_wide[1L]
        file_wide_xlsx <- file_wide[2L]

        # get wide synthetic data
        df_synthetic <- get_wide_synthetic_data(olink_platform = olink_platform,
                                                data_type = "NPX",
                                                n_panels = 3L,
                                                n_assays = 92L,
                                                n_samples = 88L,
                                                show_dev_int_ctrl = FALSE,
                                                show_int_ctrl = FALSE,
                                                version = 1L)

        # write in csv
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          write.table(
            file = file_wide_csv,
            append = FALSE,
            sep = ";",
            quote = FALSE,
            eol = "\n",
            na = "",
            dec = ".",
            row.names = FALSE,
            col.names = FALSE
          )

        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that files exist
        expect_true(object = file.exists(file_wide_csv))
        expect_true(object = file.exists(file_wide_xlsx))

        # check that function works
        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_csv <- read_npx_legacy(
                file = file_wide_csv,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that excel and csv are identical
        expect_identical(
          object = npx_legacy_xlsx,
          expected = npx_legacy_csv
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_synthetic$list_df_long$df_long,
          long_legacy = npx_legacy_csv,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    # Target 96 NPX, no dev int ctrl, w int ctrl, 1 plate, v1 ----

    withr::with_tempfile(
      new = "file_wide",
      pattern = "test_long",
      fileext = c(".csv", ".xlsx"),
      code = {

        # vars
        olink_platform <- "Target 96"

        # files
        file_wide_csv <- file_wide[1L]
        file_wide_xlsx <- file_wide[2L]

        # get wide synthetic data
        df_synthetic <- get_wide_synthetic_data(olink_platform = olink_platform,
                                                data_type = "NPX",
                                                n_panels = 3L,
                                                n_assays = 92L,
                                                n_samples = 88L,
                                                show_dev_int_ctrl = FALSE,
                                                show_int_ctrl = TRUE,
                                                version = 1L)

        # write in csv
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          write.table(
            file = file_wide_csv,
            append = FALSE,
            sep = ";",
            quote = FALSE,
            eol = "\n",
            na = "",
            dec = ".",
            row.names = FALSE,
            col.names = FALSE
          )

        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that files exist
        expect_true(object = file.exists(file_wide_csv))
        expect_true(object = file.exists(file_wide_xlsx))

        # check that function works
        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_csv <- read_npx_legacy(
                file = file_wide_csv,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that excel and csv are identical
        expect_identical(
          object = npx_legacy_xlsx,
          expected = npx_legacy_csv
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_synthetic$list_df_long$df_long,
          long_legacy = npx_legacy_csv,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    # Target 96 NPX, w dev int ctrl, w int ctrl, 1 plate, v1 ----

    withr::with_tempfile(
      new = "file_wide",
      pattern = "test_long",
      fileext = c(".csv", ".xlsx"),
      code = {

        # vars
        olink_platform <- "Target 96"

        # files
        file_wide_csv <- file_wide[1L]
        file_wide_xlsx <- file_wide[2L]

        # get wide synthetic data
        df_synthetic <- get_wide_synthetic_data(olink_platform = olink_platform,
                                                data_type = "NPX",
                                                n_panels = 3L,
                                                n_assays = 92L,
                                                n_samples = 88L,
                                                show_dev_int_ctrl = TRUE,
                                                show_int_ctrl = TRUE,
                                                version = 1L)

        # write in csv
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          write.table(
            file = file_wide_csv,
            append = FALSE,
            sep = ";",
            quote = FALSE,
            eol = "\n",
            na = "",
            dec = ".",
            row.names = FALSE,
            col.names = FALSE
          )

        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that files exist
        expect_true(object = file.exists(file_wide_csv))
        expect_true(object = file.exists(file_wide_xlsx))

        # check that function works
        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_csv <- read_npx_legacy(
                file = file_wide_csv,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that excel and csv are identical
        expect_identical(
          object = npx_legacy_xlsx,
          expected = npx_legacy_csv
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_synthetic$list_df_long$df_long,
          long_legacy = npx_legacy_csv,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    # Target 96 NPX, no dev int ctrl, no int ctrl, 2 plates, v1 ----

    withr::with_tempfile(
      new = "file_wide",
      pattern = "test_long",
      fileext = c(".csv", ".xlsx"),
      code = {

        # vars
        olink_platform <- "Target 96"

        # files
        file_wide_csv <- file_wide[1L]
        file_wide_xlsx <- file_wide[2L]

        # get wide synthetic data
        df_synthetic <- get_wide_synthetic_data(olink_platform = olink_platform,
                                                data_type = "NPX",
                                                n_panels = 3L,
                                                n_assays = 92L,
                                                n_samples = 99L,
                                                show_dev_int_ctrl = FALSE,
                                                show_int_ctrl = FALSE,
                                                version = 1L)

        # write in csv
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          write.table(
            file = file_wide_csv,
            append = FALSE,
            sep = ";",
            quote = FALSE,
            eol = "\n",
            na = "",
            dec = ".",
            row.names = FALSE,
            col.names = FALSE
          )

        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that files exist
        expect_true(object = file.exists(file_wide_csv))
        expect_true(object = file.exists(file_wide_xlsx))

        # check that function works
        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_csv <- read_npx_legacy(
                file = file_wide_csv,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that excel and csv are identical
        expect_identical(
          object = npx_legacy_xlsx,
          expected = npx_legacy_csv
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_synthetic$list_df_long$df_long,
          long_legacy = npx_legacy_csv,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    # Target 96 NPX, no dev int ctrl, w int ctrl, 2 plates, v1 ----

    withr::with_tempfile(
      new = "file_wide",
      pattern = "test_long",
      fileext = c(".csv", ".xlsx"),
      code = {

        # vars
        olink_platform <- "Target 96"

        # files
        file_wide_csv <- file_wide[1L]
        file_wide_xlsx <- file_wide[2L]

        # get wide synthetic data
        df_synthetic <- get_wide_synthetic_data(olink_platform = olink_platform,
                                                data_type = "NPX",
                                                n_panels = 3L,
                                                n_assays = 92L,
                                                n_samples = 99L,
                                                show_dev_int_ctrl = FALSE,
                                                show_int_ctrl = TRUE,
                                                version = 1L)

        # write in csv
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          write.table(
            file = file_wide_csv,
            append = FALSE,
            sep = ";",
            quote = FALSE,
            eol = "\n",
            na = "",
            dec = ".",
            row.names = FALSE,
            col.names = FALSE
          )

        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that files exist
        expect_true(object = file.exists(file_wide_csv))
        expect_true(object = file.exists(file_wide_xlsx))

        # check that function works
        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_csv <- read_npx_legacy(
                file = file_wide_csv,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that excel and csv are identical
        expect_identical(
          object = npx_legacy_xlsx,
          expected = npx_legacy_csv
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_synthetic$list_df_long$df_long,
          long_legacy = npx_legacy_csv,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    # Target 96 NPX, w dev int ctrl, w int ctrl, 2 plates, v1 ----

    withr::with_tempfile(
      new = "file_wide",
      pattern = "test_long",
      fileext = c(".csv", ".xlsx"),
      code = {

        # vars
        olink_platform <- "Target 96"

        # files
        file_wide_csv <- file_wide[1L]
        file_wide_xlsx <- file_wide[2L]

        # get wide synthetic data
        df_synthetic <- get_wide_synthetic_data(olink_platform = olink_platform,
                                                data_type = "NPX",
                                                n_panels = 3L,
                                                n_assays = 92L,
                                                n_samples = 99L,
                                                show_dev_int_ctrl = TRUE,
                                                show_int_ctrl = TRUE,
                                                version = 1L)

        # write in csv
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          write.table(
            file = file_wide_csv,
            append = FALSE,
            sep = ";",
            quote = FALSE,
            eol = "\n",
            na = "",
            dec = ".",
            row.names = FALSE,
            col.names = FALSE
          )

        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that files exist
        expect_true(object = file.exists(file_wide_csv))
        expect_true(object = file.exists(file_wide_xlsx))

        # check that function works
        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_csv <- read_npx_legacy(
                file = file_wide_csv,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that excel and csv are identical
        expect_identical(
          object = npx_legacy_xlsx,
          expected = npx_legacy_csv
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_synthetic$list_df_long$df_long,
          long_legacy = npx_legacy_csv,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    # Target 96 NPX, no dev int ctrl, no int ctrl, 1 plate, v2 ----

    withr::with_tempfile(
      new = "file_wide",
      pattern = "test_long",
      fileext = c(".csv", ".xlsx"),
      code = {

        # vars
        olink_platform <- "Target 96"

        # files
        file_wide_csv <- file_wide[1L]
        file_wide_xlsx <- file_wide[2L]

        # get wide synthetic data
        df_synthetic <- get_wide_synthetic_data(olink_platform = olink_platform,
                                                data_type = "NPX",
                                                n_panels = 3L,
                                                n_assays = 92L,
                                                n_samples = 88L,
                                                show_dev_int_ctrl = FALSE,
                                                show_int_ctrl = FALSE,
                                                version = 2L)

        # write in csv
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          write.table(
            file = file_wide_csv,
            append = FALSE,
            sep = ";",
            quote = FALSE,
            eol = "\n",
            na = "",
            dec = ".",
            row.names = FALSE,
            col.names = FALSE
          )

        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that files exist
        expect_true(object = file.exists(file_wide_csv))
        expect_true(object = file.exists(file_wide_xlsx))

        # check that function works
        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_csv <- read_npx_legacy(
                file = file_wide_csv,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that excel and csv are identical
        expect_identical(
          object = npx_legacy_xlsx,
          expected = npx_legacy_csv
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_synthetic$list_df_long$df_long,
          long_legacy = npx_legacy_csv,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    # Target 96 NPX, no dev int ctrl, w int ctrl, 1 plate, v2 ----

    withr::with_tempfile(
      new = "file_wide",
      pattern = "test_long",
      fileext = c(".csv", ".xlsx"),
      code = {

        # vars
        olink_platform <- "Target 96"

        # files
        file_wide_csv <- file_wide[1L]
        file_wide_xlsx <- file_wide[2L]

        # get wide synthetic data
        df_synthetic <- get_wide_synthetic_data(olink_platform = olink_platform,
                                                data_type = "NPX",
                                                n_panels = 3L,
                                                n_assays = 92L,
                                                n_samples = 88L,
                                                show_dev_int_ctrl = FALSE,
                                                show_int_ctrl = TRUE,
                                                version = 2L)

        # write in csv
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          write.table(
            file = file_wide_csv,
            append = FALSE,
            sep = ";",
            quote = FALSE,
            eol = "\n",
            na = "",
            dec = ".",
            row.names = FALSE,
            col.names = FALSE
          )

        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that files exist
        expect_true(object = file.exists(file_wide_csv))
        expect_true(object = file.exists(file_wide_xlsx))

        # check that function works
        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_csv <- read_npx_legacy(
                file = file_wide_csv,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that excel and csv are identical
        expect_identical(
          object = npx_legacy_xlsx,
          expected = npx_legacy_csv
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_synthetic$list_df_long$df_long,
          long_legacy = npx_legacy_csv,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    # Target 96 NPX, w dev int ctrl, w int ctrl, 1 plate, v2 ----

    withr::with_tempfile(
      new = "file_wide",
      pattern = "test_long",
      fileext = c(".csv", ".xlsx"),
      code = {

        # vars
        olink_platform <- "Target 96"

        # files
        file_wide_csv <- file_wide[1L]
        file_wide_xlsx <- file_wide[2L]

        # get wide synthetic data
        df_synthetic <- get_wide_synthetic_data(olink_platform = olink_platform,
                                                data_type = "NPX",
                                                n_panels = 3L,
                                                n_assays = 92L,
                                                n_samples = 88L,
                                                show_dev_int_ctrl = TRUE,
                                                show_int_ctrl = TRUE,
                                                version = 2L)

        # write in csv
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          write.table(
            file = file_wide_csv,
            append = FALSE,
            sep = ";",
            quote = FALSE,
            eol = "\n",
            na = "",
            dec = ".",
            row.names = FALSE,
            col.names = FALSE
          )

        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that files exist
        expect_true(object = file.exists(file_wide_csv))
        expect_true(object = file.exists(file_wide_xlsx))

        # check that function works
        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_csv <- read_npx_legacy(
                file = file_wide_csv,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that excel and csv are identical
        expect_identical(
          object = npx_legacy_xlsx,
          expected = npx_legacy_csv
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_synthetic$list_df_long$df_long,
          long_legacy = npx_legacy_csv,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    # Target 96 NPX, no dev int ctrl, no int ctrl, 2 plates, v2 ----

    withr::with_tempfile(
      new = "file_wide",
      pattern = "test_long",
      fileext = c(".csv", ".xlsx"),
      code = {

        # vars
        olink_platform <- "Target 96"

        # files
        file_wide_csv <- file_wide[1L]
        file_wide_xlsx <- file_wide[2L]

        # get wide synthetic data
        df_synthetic <- get_wide_synthetic_data(olink_platform = olink_platform,
                                                data_type = "NPX",
                                                n_panels = 3L,
                                                n_assays = 92L,
                                                n_samples = 99L,
                                                show_dev_int_ctrl = FALSE,
                                                show_int_ctrl = FALSE,
                                                version = 2L)

        # write in csv
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          write.table(
            file = file_wide_csv,
            append = FALSE,
            sep = ";",
            quote = FALSE,
            eol = "\n",
            na = "",
            dec = ".",
            row.names = FALSE,
            col.names = FALSE
          )

        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that files exist
        expect_true(object = file.exists(file_wide_csv))
        expect_true(object = file.exists(file_wide_xlsx))

        # check that function works
        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_csv <- read_npx_legacy(
                file = file_wide_csv,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that excel and csv are identical
        expect_identical(
          object = npx_legacy_xlsx,
          expected = npx_legacy_csv
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_synthetic$list_df_long$df_long,
          long_legacy = npx_legacy_csv,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    # Target 96 NPX, no dev int ctrl, w int ctrl, 2 plates, v2 ----

    withr::with_tempfile(
      new = "file_wide",
      pattern = "test_long",
      fileext = c(".csv", ".xlsx"),
      code = {

        # vars
        olink_platform <- "Target 96"

        # files
        file_wide_csv <- file_wide[1L]
        file_wide_xlsx <- file_wide[2L]

        # get wide synthetic data
        df_synthetic <- get_wide_synthetic_data(olink_platform = olink_platform,
                                                data_type = "NPX",
                                                n_panels = 3L,
                                                n_assays = 92L,
                                                n_samples = 99L,
                                                show_dev_int_ctrl = FALSE,
                                                show_int_ctrl = TRUE,
                                                version = 2L)

        # write in csv
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          write.table(
            file = file_wide_csv,
            append = FALSE,
            sep = ";",
            quote = FALSE,
            eol = "\n",
            na = "",
            dec = ".",
            row.names = FALSE,
            col.names = FALSE
          )

        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that files exist
        expect_true(object = file.exists(file_wide_csv))
        expect_true(object = file.exists(file_wide_xlsx))

        # check that function works
        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_csv <- read_npx_legacy(
                file = file_wide_csv,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that excel and csv are identical
        expect_identical(
          object = npx_legacy_xlsx,
          expected = npx_legacy_csv
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_synthetic$list_df_long$df_long,
          long_legacy = npx_legacy_csv,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    # Target 96 NPX, w dev int ctrl, w int ctrl, 2 plates, v2 ----

    withr::with_tempfile(
      new = "file_wide",
      pattern = "test_long",
      fileext = c(".csv", ".xlsx"),
      code = {

        # vars
        olink_platform <- "Target 96"

        # files
        file_wide_csv <- file_wide[1L]
        file_wide_xlsx <- file_wide[2L]

        # get wide synthetic data
        df_synthetic <- get_wide_synthetic_data(olink_platform = olink_platform,
                                                data_type = "NPX",
                                                n_panels = 3L,
                                                n_assays = 92L,
                                                n_samples = 99L,
                                                show_dev_int_ctrl = TRUE,
                                                show_int_ctrl = TRUE,
                                                version = 2L)

        # write in csv
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          write.table(
            file = file_wide_csv,
            append = FALSE,
            sep = ";",
            quote = FALSE,
            eol = "\n",
            na = "",
            dec = ".",
            row.names = FALSE,
            col.names = FALSE
          )

        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_synthetic$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        #check that files exist
        expect_true(object = file.exists(file_wide_csv))
        expect_true(object = file.exists(file_wide_xlsx))

        # check that function works
        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_csv <- read_npx_legacy(
                file = file_wide_csv,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            )
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that excel and csv are identical
        expect_identical(
          object = npx_legacy_xlsx,
          expected = npx_legacy_csv
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_synthetic$list_df_long$df_long,
          long_legacy = npx_legacy_csv,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )
  }
)
