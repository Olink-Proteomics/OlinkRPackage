# Test read_npx_legacy_help ----

test_that(
  "read_npx_legacy_help - works",
  {
    skip_if_not_installed(pkg = "readxl")
    skip_if_not_installed(pkg = "writexl")

    # Target 48 NPX ----

    # get synthetic data, or skip if not available
    df_rand <- get_wide_synthetic_data(
      olink_platform = "Target 48",
      data_type = "NPX",
      n_panels = 1L,
      n_assays = 45L,
      n_samples = 88L,
      show_dev_int_ctrl = FALSE,
      show_int_ctrl = FALSE,
      version = 1L
    )

    withr::with_tempfile(
      new = "file_wide",
      pattern = "test_long",
      fileext = c(".csv", ".xlsx"),
      code = {

        # files
        file_wide_csv <- file_wide[1L]
        file_wide_xlsx <- file_wide[2L]

        # write in csv
        write.table(x = df_rand$list_df_wide$df_wide,
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
        writexl::write_xlsx(x = df_rand$list_df_wide$df_wide,
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
          expected = df_rand$list_df_wide$df_top_wide
        )
        expect_identical(
          object = list_npx_legacy_xlsx$df_split$df_top,
          expected = df_rand$list_df_wide$df_top_wide
        )

        expect_identical(
          object = list_npx_legacy_csv$df_split$df_data,
          expected = df_rand$list_df_wide$df_middle_wide |>
            dplyr::bind_rows(
              df_rand$list_df_wide$df_na_wide
            ) |>
            dplyr::bind_rows(
              df_rand$list_df_wide$df_bottom_wide
            )
        )
        expect_identical(
          object = list_npx_legacy_xlsx$df_split$df_data,
          expected = df_rand$list_df_wide$df_middle_wide |>
            dplyr::bind_rows(
              df_rand$list_df_wide$df_na_wide
            ) |>
            dplyr::bind_rows(
              df_rand$list_df_wide$df_bottom_wide
            )
        )
      }
    )

    # Target 48 Quantified ----

    # get synthetic data, or skip if not available
    df_rand <- get_wide_synthetic_data(
      olink_platform = "Target 48",
      data_type = "Quantified",
      n_panels = 1L,
      n_assays = 45L,
      n_samples = 88L,
      show_dev_int_ctrl = FALSE,
      show_int_ctrl = FALSE,
      version = 0L
    )

    withr::with_tempfile(
      new = "file_wide",
      pattern = "test_long",
      fileext = c(".csv", ".xlsx"),
      code = {

        # files
        file_wide_csv <- file_wide[1L]
        file_wide_xlsx <- file_wide[2L]

        # write in csv
        write.table(x = df_rand$list_df_wide$df_wide,
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
        writexl::write_xlsx(x = df_rand$list_df_wide$df_wide,
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
          expected = df_rand$list_df_wide$df_top_wide
        )
        expect_identical(
          object = list_npx_legacy_xlsx$df_split$df_top,
          expected = df_rand$list_df_wide$df_top_wide
        )

        expect_identical(
          object = list_npx_legacy_csv$df_split$df_data,
          expected = df_rand$list_df_wide$df_middle_wide |>
            dplyr::bind_rows(
              df_rand$list_df_wide$df_na_wide
            ) |>
            dplyr::bind_rows(
              df_rand$list_df_wide$df_bottom_wide
            )
        )
        expect_identical(
          object = list_npx_legacy_xlsx$df_split$df_data,
          expected = df_rand$list_df_wide$df_middle_wide |>
            dplyr::bind_rows(
              df_rand$list_df_wide$df_na_wide
            ) |>
            dplyr::bind_rows(
              df_rand$list_df_wide$df_bottom_wide
            )
        )
      }
    )

    # Target 96 NPX v1 ----

    # get synthetic data, or skip if not available
    df_rand <- get_wide_synthetic_data(
      olink_platform = "Target 96",
      data_type = "NPX",
      n_panels = 1L,
      n_assays = 92L,
      n_samples = 88L,
      show_dev_int_ctrl = FALSE,
      show_int_ctrl = FALSE,
      version = 1L
    )

    withr::with_tempfile(
      new = "file_wide",
      pattern = "test_long",
      fileext = c(".csv", ".xlsx"),
      code = {

        # files
        file_wide_csv <- file_wide[1L]
        file_wide_xlsx <- file_wide[2L]

        # write in csv
        write.table(x = df_rand$list_df_wide$df_wide,
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
        writexl::write_xlsx(x = df_rand$list_df_wide$df_wide,
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
          expected = df_rand$list_df_wide$df_top_wide
        )
        expect_identical(
          object = list_npx_legacy_xlsx$df_split$df_top,
          expected = df_rand$list_df_wide$df_top_wide
        )

        expect_identical(
          object = list_npx_legacy_csv$df_split$df_data,
          expected = df_rand$list_df_wide$df_middle_wide |>
            dplyr::bind_rows(
              df_rand$list_df_wide$df_na_wide
            ) |>
            dplyr::bind_rows(
              df_rand$list_df_wide$df_bottom_wide
            )
        )
        expect_identical(
          object = list_npx_legacy_xlsx$df_split$df_data,
          expected = df_rand$list_df_wide$df_middle_wide |>
            dplyr::bind_rows(
              df_rand$list_df_wide$df_na_wide
            ) |>
            dplyr::bind_rows(
              df_rand$list_df_wide$df_bottom_wide
            )
        )
      }
    )

    # Target 96 NPX v2 ----

    # get synthetic data, or skip if not available
    df_rand <- get_wide_synthetic_data(
      olink_platform = "Target 96",
      data_type = "NPX",
      n_panels = 1L,
      n_assays = 92L,
      n_samples = 88L,
      show_dev_int_ctrl = FALSE,
      show_int_ctrl = FALSE,
      version = 2L
    )

    withr::with_tempfile(
      new = "file_wide",
      pattern = "test_long",
      fileext = c(".csv", ".xlsx"),
      code = {

        # files
        file_wide_csv <- file_wide[1L]
        file_wide_xlsx <- file_wide[2L]

        # write in csv
        write.table(x = df_rand$list_df_wide$df_wide,
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
        writexl::write_xlsx(x = df_rand$list_df_wide$df_wide,
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
          expected = df_rand$list_df_wide$df_top_wide
        )
        expect_identical(
          object = list_npx_legacy_xlsx$df_split$df_top,
          expected = df_rand$list_df_wide$df_top_wide
        )

        expect_identical(
          object = list_npx_legacy_csv$df_split$df_data,
          expected = df_rand$list_df_wide$df_middle_wide |>
            dplyr::bind_rows(
              df_rand$list_df_wide$df_na_wide
            ) |>
            dplyr::bind_rows(
              df_rand$list_df_wide$df_bottom_wide
            )
        )
        expect_identical(
          object = list_npx_legacy_xlsx$df_split$df_data,
          expected = df_rand$list_df_wide$df_middle_wide |>
            dplyr::bind_rows(
              df_rand$list_df_wide$df_na_wide
            ) |>
            dplyr::bind_rows(
              df_rand$list_df_wide$df_bottom_wide
            )
        )
      }
    )
  }
)

test_that(
  "read_npx_legacy_help - error - long df",
  {
    # get synthetic data, or skip if not available
    df_rand <- get_wide_synthetic_data(
      olink_platform = "Target 48",
      data_type = "NPX",
      n_panels = 1L,
      n_assays = 45L,
      n_samples = 88L,
      show_dev_int_ctrl = FALSE,
      show_int_ctrl = FALSE,
      version = 2L
    )

    withr::with_tempfile(
      new = "csv_long",
      pattern = "test_long",
      fileext = ".csv",
      code = {
        # write in csv
        write.table(x = df_rand$list_df_long$df_long,
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
    # get synthetic data, or skip if not available
    df_rand <- get_wide_synthetic_data(
      olink_platform = "Focus",
      data_type = "NPX",
      n_panels = 1L,
      n_assays = 33L,
      n_samples = 88L,
      show_dev_int_ctrl = FALSE,
      show_int_ctrl = FALSE,
      version = 0L
    )

    withr::with_tempfile(
      new = "csv_long",
      pattern = "test_long",
      fileext = ".csv",
      code = {
        # write in csv
        write.table(x = df_rand$list_df_wide$df_wide,
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
    # get synthetic data, or skip if not available
    df_rand <- get_wide_synthetic_data(
      olink_platform = "Target 48",
      data_type = "Ct",
      n_panels = 1L,
      n_assays = 45L,
      n_samples = 88L,
      show_dev_int_ctrl = FALSE,
      show_int_ctrl = FALSE,
      version = 0L
    )

    withr::with_tempfile(
      new = "csv_long",
      pattern = "test_long",
      fileext = ".csv",
      code = {
        # write in csv
        write.table(x = df_rand$list_df_wide$df_wide,
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
    skip_if_not_installed(pkg = "readxl")
    skip_if_not_installed(pkg = "writexl")

    olink_platform <- "Target 48"
    data_type <- "NPX"
    version <- 1L

    # vars NPX 88 samples version 1 ----

    n_samples <- 88L

    ## Target 48 NPX, no dev int ctrl, no int ctrl, 1 plate, v1 ----

    # get synthetic data, or skip if not available
    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = 1L,
      n_assays = 45L,
      n_samples = n_samples,
      show_dev_int_ctrl = FALSE,
      show_int_ctrl = FALSE,
      version = version
    )

    withr::with_tempfile(
      new = "file_wide_xlsx",
      pattern = "test_long",
      fileext = ".xlsx",
      code = {
        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_rand$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        # check that function works
        expect_message(
          object = expect_warning(
            object = expect_no_error(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            ),
            regexp = "You are using the function read_npx_legacy"
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that excel and csv are identical
        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_rand$list_df_long$df_long,
          long_legacy = npx_legacy_xlsx,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    ## Target 48 NPX, no dev int ctrl, w int ctrl, 1 plate, v1 ----

    # get synthetic data, or skip if not available
    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = 1L,
      n_assays = 45L,
      n_samples = n_samples,
      show_dev_int_ctrl = FALSE,
      show_int_ctrl = TRUE,
      version = version
    )

    withr::with_tempfile(
      new = "file_wide_xlsx",
      pattern = "test_long",
      fileext = ".xlsx",
      code = {
        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_rand$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        # check that function works
        expect_message(
          object = expect_warning(
            object = expect_no_error(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            ),
            regexp = "You are using the function read_npx_legacy"
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that excel and csv are identical
        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_rand$list_df_long$df_long,
          long_legacy = npx_legacy_xlsx,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    ## Target 48 NPX, w dev int ctrl, w int ctrl, 1 plate, v1 ----

    # get synthetic data, or skip if not available
    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = 1L,
      n_assays = 45L,
      n_samples = n_samples,
      show_dev_int_ctrl = TRUE,
      show_int_ctrl = TRUE,
      version = version
    )

    withr::with_tempfile(
      new = "file_wide_xlsx",
      pattern = "test_long",
      fileext = ".xlsx",
      code = {
        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_rand$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        # check that function works
        expect_message(
          object = expect_warning(
            object = expect_no_error(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            ),
            regexp = "You are using the function read_npx_legacy"
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_rand$list_df_long$df_long,
          long_legacy = npx_legacy_xlsx,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    # vars 99 samples ----

    n_samples <- 99L

    ## Target 48 NPX, no dev int ctrl, no int ctrl, 2 plates, v1 ----

    # get synthetic data, or skip if not available
    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = 1L,
      n_assays = 45L,
      n_samples = n_samples,
      show_dev_int_ctrl = FALSE,
      show_int_ctrl = FALSE,
      version = version
    )

    withr::with_tempfile(
      new = "file_wide_xlsx",
      pattern = "test_long",
      fileext =  ".xlsx",
      code = {
        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_rand$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        # check that function works
        expect_message(
          object = expect_warning(
            object = expect_no_error(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            ),
            regexp = "You are using the function read_npx_legacy"
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_rand$list_df_long$df_long,
          long_legacy = npx_legacy_xlsx,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    ## Target 48 NPX, no dev int ctrl, w int ctrl, 2 plates, v1 ----

    # get synthetic data, or skip if not available
    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = 1L,
      n_assays = 45L,
      n_samples = n_samples,
      show_dev_int_ctrl = FALSE,
      show_int_ctrl = TRUE,
      version = version
    )

    withr::with_tempfile(
      new = "file_wide_xlsx",
      pattern = "test_long",
      fileext = ".xlsx",
      code = {
        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_rand$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        # check that function works
        expect_message(
          object = expect_warning(
            object = expect_no_error(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            ),
            regexp = "You are using the function read_npx_legacy"
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_rand$list_df_long$df_long,
          long_legacy = npx_legacy_xlsx,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    ## Target 48 NPX, w dev int ctrl, w int ctrl, 2 plates, v1 ----

    # get synthetic data, or skip if not available
    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = 1L,
      n_assays = 45L,
      n_samples = n_samples,
      show_dev_int_ctrl = TRUE,
      show_int_ctrl = TRUE,
      version = version
    )

    withr::with_tempfile(
      new = "file_wide_xlsx",
      pattern = "test_long",
      fileext = ".xlsx",
      code = {
        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_rand$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        # check that function works
        expect_message(
          object = expect_warning(
            object = expect_no_error(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            ),
            regexp = "You are using the function read_npx_legacy"
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_rand$list_df_long$df_long,
          long_legacy = npx_legacy_xlsx,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    # vars Quantified 88 samples version 0 ----

    data_type <- "Quantified"
    n_samples <- 88L
    version <- 0L

    ## Target 48 Quantified, no dev int ctrl, no int ctrl, 1 plate ----

    # get synthetic data, or skip if not available
    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = 1L,
      n_assays = 45L,
      n_samples = n_samples,
      show_dev_int_ctrl = FALSE,
      show_int_ctrl = FALSE,
      version = version
    )

    withr::with_tempfile(
      new = "file_wide_xlsx",
      pattern = "test_long",
      fileext = ".xlsx",
      code = {
        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_rand$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        # check that function works
        expect_message(
          object = expect_warning(
            object = expect_no_error(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            ),
            regexp = "You are using the function read_npx_legacy"
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_rand$list_df_long$df_long,
          long_legacy = npx_legacy_xlsx,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    ## Target 48 Quantified, no dev int ctrl, w int ctrl, 1 plate ----

    # get synthetic data, or skip if not available
    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = 1L,
      n_assays = 45L,
      n_samples = n_samples,
      show_dev_int_ctrl = FALSE,
      show_int_ctrl = TRUE,
      version = version
    )

    withr::with_tempfile(
      new = "file_wide_xlsx",
      pattern = "test_long",
      fileext = ".xlsx",
      code = {
        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_rand$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        # check that function works
        expect_message(
          object = expect_warning(
            object = expect_no_error(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            ),
            regexp = "You are using the function read_npx_legacy"
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_rand$list_df_long$df_long,
          long_legacy = npx_legacy_xlsx,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    ## Target 48 Quantified, w dev int ctrl, w int ctrl, 1 plate ----

    # get synthetic data, or skip if not available
    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = 1L,
      n_assays = 45L,
      n_samples = n_samples,
      show_dev_int_ctrl = TRUE,
      show_int_ctrl = TRUE,
      version = version
    )

    withr::with_tempfile(
      new = "file_wide_xlsx",
      pattern = "test_long",
      fileext = ".xlsx",
      code = {
        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_rand$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        # check that function works
        expect_message(
          object = expect_warning(
            object = expect_no_error(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            ),
            regexp = "You are using the function read_npx_legacy"
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_rand$list_df_long$df_long,
          long_legacy = npx_legacy_xlsx,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    # vars 99 samples ----

    n_samples <- 99L

    ## Target 48 Quantified, no dev int ctrl, no int ctrl, 2 plates ----

    # get synthetic data, or skip if not available
    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = 1L,
      n_assays = 45L,
      n_samples = n_samples,
      show_dev_int_ctrl = FALSE,
      show_int_ctrl = FALSE,
      version = version
    )

    withr::with_tempfile(
      new = "file_wide_xlsx",
      pattern = "test_long",
      fileext = ".xlsx",
      code = {
        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_rand$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        # check that function works
        expect_message(
          object = expect_warning(
            object = expect_no_error(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            ),
            regexp = "You are using the function read_npx_legacy"
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_rand$list_df_long$df_long,
          long_legacy = npx_legacy_xlsx,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    ## Target 48 Quantified, no dev int ctrl, w int ctrl, 2 plates ----

    # get synthetic data, or skip if not available
    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = 1L,
      n_assays = 45L,
      n_samples = n_samples,
      show_dev_int_ctrl = FALSE,
      show_int_ctrl = TRUE,
      version = version
    )

    withr::with_tempfile(
      new = "file_wide_xlsx",
      pattern = "test_long",
      fileext = ".xlsx",
      code = {
        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_rand$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        # check that function works
        expect_message(
          object = expect_warning(
            object = expect_no_error(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            ),
            regexp = "You are using the function read_npx_legacy"
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_rand$list_df_long$df_long,
          long_legacy = npx_legacy_xlsx,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    ## Target 48 Quantified, w dev int ctrl, w int ctrl, 2 plates ----

    # get synthetic data, or skip if not available
    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = 1L,
      n_assays = 45L,
      n_samples = n_samples,
      show_dev_int_ctrl = TRUE,
      show_int_ctrl = TRUE,
      version = version
    )

    withr::with_tempfile(
      new = "file_wide_xlsx",
      pattern = "test_long",
      fileext = ".xlsx",
      code = {
        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_rand$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        # check that function works
        expect_message(
          object = expect_warning(
            object = expect_no_error(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            ),
            regexp = "You are using the function read_npx_legacy"
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_rand$list_df_long$df_long,
          long_legacy = npx_legacy_xlsx,
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
    skip_if_not_installed(pkg = "readxl")
    skip_if_not_installed(pkg = "writexl")

    olink_platform <- "Target 48"
    data_type <- "NPX"
    version <- 1L

    # vars NPX 88 samples version 1 ----

    n_samples <- 88L

    ## Target 48 NPX, no dev int ctrl, no int ctrl, 1 plate, v1 ----

    # get synthetic data, or skip if not available
    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = 3L,
      n_assays = 45L,
      n_samples = n_samples,
      show_dev_int_ctrl = FALSE,
      show_int_ctrl = FALSE,
      version = version
    )

    withr::with_tempfile(
      new = "file_wide_xlsx",
      pattern = "test_long",
      fileext = ".xlsx",
      code = {
        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_rand$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        # check that function works
        expect_message(
          object = expect_warning(
            object = expect_no_error(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            ),
            regexp = "You are using the function read_npx_legacy"
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that excel and csv are identical
        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_rand$list_df_long$df_long,
          long_legacy = npx_legacy_xlsx,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    ## Target 48 NPX, no dev int ctrl, w int ctrl, 1 plate, v1 ----

    # get synthetic data, or skip if not available
    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = 3L,
      n_assays = 45L,
      n_samples = n_samples,
      show_dev_int_ctrl = FALSE,
      show_int_ctrl = TRUE,
      version = version
    )

    withr::with_tempfile(
      new = "file_wide_xlsx",
      pattern = "test_long",
      fileext = ".xlsx",
      code = {
        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_rand$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        # check that function works
        expect_message(
          object = expect_warning(
            object = expect_no_error(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            ),
            regexp = "You are using the function read_npx_legacy"
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that excel and csv are identical
        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_rand$list_df_long$df_long,
          long_legacy = npx_legacy_xlsx,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    ## Target 48 NPX, w dev int ctrl, w int ctrl, 1 plate, v1 ----

    # get synthetic data, or skip if not available
    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = 3L,
      n_assays = 45L,
      n_samples = n_samples,
      show_dev_int_ctrl = TRUE,
      show_int_ctrl = TRUE,
      version = version
    )

    withr::with_tempfile(
      new = "file_wide_xlsx",
      pattern = "test_long",
      fileext = ".xlsx",
      code = {
        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_rand$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        # check that function works
        expect_message(
          object = expect_warning(
            object = expect_no_error(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            ),
            regexp = "You are using the function read_npx_legacy"
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_rand$list_df_long$df_long,
          long_legacy = npx_legacy_xlsx,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    # vars 99 samples ----

    n_samples <- 99L

    ## Target 48 NPX, no dev int ctrl, no int ctrl, 2 plates, v1 ----

    # get synthetic data, or skip if not available
    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = 3L,
      n_assays = 45L,
      n_samples = n_samples,
      show_dev_int_ctrl = FALSE,
      show_int_ctrl = FALSE,
      version = version
    )

    withr::with_tempfile(
      new = "file_wide_xlsx",
      pattern = "test_long",
      fileext =  ".xlsx",
      code = {
        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_rand$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        # check that function works
        expect_message(
          object = expect_warning(
            object = expect_no_error(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            ),
            regexp = "You are using the function read_npx_legacy"
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_rand$list_df_long$df_long,
          long_legacy = npx_legacy_xlsx,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    ## Target 48 NPX, no dev int ctrl, w int ctrl, 2 plates, v1 ----

    # get synthetic data, or skip if not available
    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = 3L,
      n_assays = 45L,
      n_samples = n_samples,
      show_dev_int_ctrl = FALSE,
      show_int_ctrl = TRUE,
      version = version
    )

    withr::with_tempfile(
      new = "file_wide_xlsx",
      pattern = "test_long",
      fileext = ".xlsx",
      code = {
        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_rand$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        # check that function works
        expect_message(
          object = expect_warning(
            object = expect_no_error(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            ),
            regexp = "You are using the function read_npx_legacy"
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_rand$list_df_long$df_long,
          long_legacy = npx_legacy_xlsx,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    ## Target 48 NPX, w dev int ctrl, w int ctrl, 2 plates, v1 ----

    # get synthetic data, or skip if not available
    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = 3L,
      n_assays = 45L,
      n_samples = n_samples,
      show_dev_int_ctrl = TRUE,
      show_int_ctrl = TRUE,
      version = version
    )

    withr::with_tempfile(
      new = "file_wide_xlsx",
      pattern = "test_long",
      fileext = ".xlsx",
      code = {
        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_rand$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        # check that function works
        expect_message(
          object = expect_warning(
            object = expect_no_error(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            ),
            regexp = "You are using the function read_npx_legacy"
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_rand$list_df_long$df_long,
          long_legacy = npx_legacy_xlsx,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    # vars Quantified 88 samples version 0 ----

    data_type <- "Quantified"
    n_samples <- 88L
    version <- 0L

    ## Target 48 Quantified, no dev int ctrl, no int ctrl, 1 plate ----

    # get synthetic data, or skip if not available
    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = 3L,
      n_assays = 45L,
      n_samples = n_samples,
      show_dev_int_ctrl = FALSE,
      show_int_ctrl = FALSE,
      version = version
    )

    withr::with_tempfile(
      new = "file_wide_xlsx",
      pattern = "test_long",
      fileext = ".xlsx",
      code = {
        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_rand$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        # check that function works
        expect_message(
          object = expect_warning(
            object = expect_no_error(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            ),
            regexp = "You are using the function read_npx_legacy"
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_rand$list_df_long$df_long,
          long_legacy = npx_legacy_xlsx,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    ## Target 48 Quantified, no dev int ctrl, w int ctrl, 1 plate ----

    # get synthetic data, or skip if not available
    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = 3L,
      n_assays = 45L,
      n_samples = n_samples,
      show_dev_int_ctrl = FALSE,
      show_int_ctrl = TRUE,
      version = version
    )

    withr::with_tempfile(
      new = "file_wide_xlsx",
      pattern = "test_long",
      fileext = ".xlsx",
      code = {
        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_rand$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        # check that function works
        expect_message(
          object = expect_warning(
            object = expect_no_error(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            ),
            regexp = "You are using the function read_npx_legacy"
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_rand$list_df_long$df_long,
          long_legacy = npx_legacy_xlsx,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    ## Target 48 Quantified, w dev int ctrl, w int ctrl, 1 plate ----

    # get synthetic data, or skip if not available
    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = 3L,
      n_assays = 45L,
      n_samples = n_samples,
      show_dev_int_ctrl = TRUE,
      show_int_ctrl = TRUE,
      version = version
    )

    withr::with_tempfile(
      new = "file_wide_xlsx",
      pattern = "test_long",
      fileext = ".xlsx",
      code = {
        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_rand$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        # check that function works
        expect_message(
          object = expect_warning(
            object = expect_no_error(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            ),
            regexp = "You are using the function read_npx_legacy"
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_rand$list_df_long$df_long,
          long_legacy = npx_legacy_xlsx,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    # vars 99 samples ----

    n_samples <- 99L

    ## Target 48 Quantified, no dev int ctrl, no int ctrl, 2 plates ----

    # get synthetic data, or skip if not available
    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = 3L,
      n_assays = 45L,
      n_samples = n_samples,
      show_dev_int_ctrl = FALSE,
      show_int_ctrl = FALSE,
      version = version
    )

    withr::with_tempfile(
      new = "file_wide_xlsx",
      pattern = "test_long",
      fileext = ".xlsx",
      code = {
        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_rand$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        # check that function works
        expect_message(
          object = expect_warning(
            object = expect_no_error(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            ),
            regexp = "You are using the function read_npx_legacy"
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_rand$list_df_long$df_long,
          long_legacy = npx_legacy_xlsx,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    ## Target 48 Quantified, no dev int ctrl, w int ctrl, 2 plates ----

    # get synthetic data, or skip if not available
    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = 3L,
      n_assays = 45L,
      n_samples = n_samples,
      show_dev_int_ctrl = FALSE,
      show_int_ctrl = TRUE,
      version = version
    )

    withr::with_tempfile(
      new = "file_wide_xlsx",
      pattern = "test_long",
      fileext = ".xlsx",
      code = {
        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_rand$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        # check that function works
        expect_message(
          object = expect_warning(
            object = expect_no_error(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            ),
            regexp = "You are using the function read_npx_legacy"
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_rand$list_df_long$df_long,
          long_legacy = npx_legacy_xlsx,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    ## Target 48 Quantified, w dev int ctrl, w int ctrl, 2 plates ----

    # get synthetic data, or skip if not available
    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = 3L,
      n_assays = 45L,
      n_samples = n_samples,
      show_dev_int_ctrl = TRUE,
      show_int_ctrl = TRUE,
      version = version
    )

    withr::with_tempfile(
      new = "file_wide_xlsx",
      pattern = "test_long",
      fileext = ".xlsx",
      code = {
        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_rand$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        # check that function works
        expect_message(
          object = expect_warning(
            object = expect_no_error(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            ),
            regexp = "You are using the function read_npx_legacy"
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_rand$list_df_long$df_long,
          long_legacy = npx_legacy_xlsx,
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
    skip_if_not_installed(pkg = "readxl")
    skip_if_not_installed(pkg = "writexl")

    olink_platform <- "Target 96"
    data_type <- "NPX"
    version <- 1L

    # vars 88 samples version 1 ----

    n_samples <- 88L

    ## Target 96 NPX, no dev int ctrl, no int ctrl, 1 plate, v1 ----

    # get synthetic data, or skip if not available
    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = 1L,
      n_assays = 92L,
      n_samples = n_samples,
      show_dev_int_ctrl = FALSE,
      show_int_ctrl = FALSE,
      version = version
    )

    withr::with_tempfile(
      new = "file_wide_xlsx",
      pattern = "test_long",
      fileext = ".xlsx",
      code = {
        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_rand$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        # check that function works
        expect_message(
          expect_warning(
            object = expect_no_error(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            ),
            regexp = "You are using the function read_npx_legacy"
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that excel and csv are identical
        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_rand$list_df_long$df_long,
          long_legacy = npx_legacy_xlsx,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    ## Target 96 NPX, no dev int ctrl, w int ctrl, 1 plate, v1 ----

    # get synthetic data, or skip if not available
    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = 1L,
      n_assays = 92L,
      n_samples = n_samples,
      show_dev_int_ctrl = FALSE,
      show_int_ctrl = TRUE,
      version = version
    )

    withr::with_tempfile(
      new = "file_wide_xlsx",
      pattern = "test_long",
      fileext = ".xlsx",
      code = {
        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_rand$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        # check that function works
        expect_message(
          expect_warning(
            object = expect_no_error(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            ),
            regexp = "You are using the function read_npx_legacy"
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that excel and csv are identical
        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_rand$list_df_long$df_long,
          long_legacy = npx_legacy_xlsx,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    ## Target 96 NPX, w dev int ctrl, w int ctrl, 1 plate, v1 ----

    # get synthetic data, or skip if not available
    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = 1L,
      n_assays = 92L,
      n_samples = n_samples,
      show_dev_int_ctrl = TRUE,
      show_int_ctrl = TRUE,
      version = version
    )

    withr::with_tempfile(
      new = "file_wide_xlsx",
      pattern = "test_long",
      fileext = ".xlsx",
      code = {
        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_rand$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        # check that function works
        expect_message(
          expect_warning(
            object = expect_no_error(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            ),
            regexp = "You are using the function read_npx_legacy"
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_rand$list_df_long$df_long,
          long_legacy = npx_legacy_xlsx,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    # vars 99 samples ----

    n_samples <- 99L

    ## Target 96 NPX, no dev int ctrl, no int ctrl, 2 plates, v1 ----

    # get synthetic data, or skip if not available
    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = 1L,
      n_assays = 92L,
      n_samples = n_samples,
      show_dev_int_ctrl = FALSE,
      show_int_ctrl = FALSE,
      version = version
    )

    withr::with_tempfile(
      new = "file_wide_xlsx",
      pattern = "test_long",
      fileext =  ".xlsx",
      code = {
        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_rand$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        # check that function works
        expect_message(
          expect_warning(
            object = expect_no_error(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            ),
            regexp = "You are using the function read_npx_legacy"
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_rand$list_df_long$df_long,
          long_legacy = npx_legacy_xlsx,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    ## Target 96 NPX, no dev int ctrl, w int ctrl, 2 plates, v1 ----

    # get synthetic data, or skip if not available
    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = 1L,
      n_assays = 92L,
      n_samples = n_samples,
      show_dev_int_ctrl = FALSE,
      show_int_ctrl = TRUE,
      version = version
    )

    withr::with_tempfile(
      new = "file_wide_xlsx",
      pattern = "test_long",
      fileext = ".xlsx",
      code = {
        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_rand$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        # check that function works
        expect_message(
          expect_warning(
            object = expect_no_error(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            ),
            regexp = "You are using the function read_npx_legacy"
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_rand$list_df_long$df_long,
          long_legacy = npx_legacy_xlsx,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    ## Target 96 NPX, w dev int ctrl, w int ctrl, 2 plates, v1 ----

    # get synthetic data, or skip if not available
    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = 1L,
      n_assays = 92L,
      n_samples = n_samples,
      show_dev_int_ctrl = TRUE,
      show_int_ctrl = TRUE,
      version = version
    )

    withr::with_tempfile(
      new = "file_wide_xlsx",
      pattern = "test_long",
      fileext = ".xlsx",
      code = {
        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_rand$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        # check that function works
        expect_message(
          expect_warning(
            object = expect_no_error(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            ),
            regexp = "You are using the function read_npx_legacy"
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_rand$list_df_long$df_long,
          long_legacy = npx_legacy_xlsx,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    # vars 88 samples version 2 ----

    version <- 2L

    ## Target 96 NPX, no dev int ctrl, no int ctrl, 1 plate ----

    # get synthetic data, or skip if not available
    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = 1L,
      n_assays = 92L,
      n_samples = n_samples,
      show_dev_int_ctrl = FALSE,
      show_int_ctrl = FALSE,
      version = version
    )

    withr::with_tempfile(
      new = "file_wide_xlsx",
      pattern = "test_long",
      fileext = ".xlsx",
      code = {
        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_rand$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        # check that function works
        expect_message(
          expect_warning(
            object = expect_no_error(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            ),
            regexp = "You are using the function read_npx_legacy"
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_rand$list_df_long$df_long,
          long_legacy = npx_legacy_xlsx,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    ## Target 96 NPX, no dev int ctrl, w int ctrl, 1 plate ----

    # get synthetic data, or skip if not available
    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = 1L,
      n_assays = 92L,
      n_samples = n_samples,
      show_dev_int_ctrl = FALSE,
      show_int_ctrl = TRUE,
      version = version
    )

    withr::with_tempfile(
      new = "file_wide_xlsx",
      pattern = "test_long",
      fileext = ".xlsx",
      code = {
        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_rand$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        # check that function works
        expect_message(
          expect_warning(
            object = expect_no_error(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            ),
            regexp = "You are using the function read_npx_legacy"
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_rand$list_df_long$df_long,
          long_legacy = npx_legacy_xlsx,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    ## Target 96 NPX, w dev int ctrl, w int ctrl, 1 plate ----

    # get synthetic data, or skip if not available
    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = 1L,
      n_assays = 92L,
      n_samples = n_samples,
      show_dev_int_ctrl = TRUE,
      show_int_ctrl = TRUE,
      version = version
    )

    withr::with_tempfile(
      new = "file_wide_xlsx",
      pattern = "test_long",
      fileext = ".xlsx",
      code = {# write in excel
        olink_wide_order_cols(
          list_df_wide = df_rand$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        # check that function works
        expect_message(
          expect_warning(
            object = expect_no_error(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            ),
            regexp = "You are using the function read_npx_legacy"
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_rand$list_df_long$df_long,
          long_legacy = npx_legacy_xlsx,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    # vars 99 samples ----

    n_samples <- 99L

    ## Target 96 NPX, no dev int ctrl, no int ctrl, 2 plates ----

    # get synthetic data, or skip if not available
    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = 1L,
      n_assays = 92L,
      n_samples = n_samples,
      show_dev_int_ctrl = FALSE,
      show_int_ctrl = FALSE,
      version = version
    )

    withr::with_tempfile(
      new = "file_wide_xlsx",
      pattern = "test_long",
      fileext = ".xlsx",
      code = {
        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_rand$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        # check that function works
        expect_message(
          expect_warning(
            object = expect_no_error(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            ),
            regexp = "You are using the function read_npx_legacy"
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_rand$list_df_long$df_long,
          long_legacy = npx_legacy_xlsx,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    ## Target 96 NPX, no dev int ctrl, w int ctrl, 2 plates ----

    # get synthetic data, or skip if not available
    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = 1L,
      n_assays = 92L,
      n_samples = n_samples,
      show_dev_int_ctrl = FALSE,
      show_int_ctrl = TRUE,
      version = version
    )

    withr::with_tempfile(
      new = "file_wide_xlsx",
      pattern = "test_long",
      fileext = ".xlsx",
      code = {
        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_rand$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        # check that function works
        expect_message(
          expect_warning(
            object = expect_no_error(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            ),
            regexp = "You are using the function read_npx_legacy"
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_rand$list_df_long$df_long,
          long_legacy = npx_legacy_xlsx,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    ## Target 96 NPX, w dev int ctrl, w int ctrl, 2 plates ----

    # get synthetic data, or skip if not available
    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = 1L,
      n_assays = 92L,
      n_samples = n_samples,
      show_dev_int_ctrl = TRUE,
      show_int_ctrl = TRUE,
      version = version
    )

    withr::with_tempfile(
      new = "file_wide_xlsx",
      pattern = "test_long",
      fileext = ".xlsx",
      code = {
        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_rand$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        # check that function works
        expect_message(
          expect_warning(
            object = expect_no_error(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            ),
            regexp = "You are using the function read_npx_legacy"
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_rand$list_df_long$df_long,
          long_legacy = npx_legacy_xlsx,
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
    skip_if_not_installed(pkg = "readxl")
    skip_if_not_installed(pkg = "writexl")

    olink_platform <- "Target 96"
    data_type <- "NPX"
    version <- 1L

    # vars 88 samples version 1 ----

    n_samples <- 88L

    ## Target 96 NPX, no dev int ctrl, no int ctrl, 1 plate, v1 ----

    # get synthetic data, or skip if not available
    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = 3L,
      n_assays = 92L,
      n_samples = n_samples,
      show_dev_int_ctrl = FALSE,
      show_int_ctrl = FALSE,
      version = version
    )

    withr::with_tempfile(
      new = "file_wide_xlsx",
      pattern = "test_long",
      fileext = ".xlsx",
      code = {
        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_rand$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        # check that function works
        expect_message(
          expect_warning(
            object = expect_no_error(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            ),
            regexp = "You are using the function read_npx_legacy"
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that excel and csv are identical
        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_rand$list_df_long$df_long,
          long_legacy = npx_legacy_xlsx,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    ## Target 96 NPX, no dev int ctrl, w int ctrl, 1 plate, v1 ----

    # get synthetic data, or skip if not available
    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = 3L,
      n_assays = 92L,
      n_samples = n_samples,
      show_dev_int_ctrl = FALSE,
      show_int_ctrl = TRUE,
      version = version
    )

    withr::with_tempfile(
      new = "file_wide_xlsx",
      pattern = "test_long",
      fileext = ".xlsx",
      code = {
        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_rand$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        # check that function works
        expect_message(
          expect_warning(
            object = expect_no_error(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            ),
            regexp = "You are using the function read_npx_legacy"
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that excel and csv are identical
        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_rand$list_df_long$df_long,
          long_legacy = npx_legacy_xlsx,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    ## Target 96 NPX, w dev int ctrl, w int ctrl, 1 plate, v1 ----

    # get synthetic data, or skip if not available
    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = 3L,
      n_assays = 92L,
      n_samples = n_samples,
      show_dev_int_ctrl = TRUE,
      show_int_ctrl = TRUE,
      version = version
    )

    withr::with_tempfile(
      new = "file_wide_xlsx",
      pattern = "test_long",
      fileext = ".xlsx",
      code = {
        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_rand$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        # check that function works
        expect_message(
          expect_warning(
            object = expect_no_error(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            ),
            regexp = "You are using the function read_npx_legacy"
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_rand$list_df_long$df_long,
          long_legacy = npx_legacy_xlsx,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    # vars 99 samples ----

    n_samples <- 99L

    ## Target 96 NPX, no dev int ctrl, no int ctrl, 2 plates, v1 ----

    # get synthetic data, or skip if not available
    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = 3L,
      n_assays = 92L,
      n_samples = n_samples,
      show_dev_int_ctrl = FALSE,
      show_int_ctrl = FALSE,
      version = version
    )

    withr::with_tempfile(
      new = "file_wide_xlsx",
      pattern = "test_long",
      fileext =  ".xlsx",
      code = {
        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_rand$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        # check that function works
        expect_message(
          expect_warning(
            object = expect_no_error(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            ),
            regexp = "You are using the function read_npx_legacy"
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_rand$list_df_long$df_long,
          long_legacy = npx_legacy_xlsx,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    ## Target 96 NPX, no dev int ctrl, w int ctrl, 2 plates, v1 ----

    # get synthetic data, or skip if not available
    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = 3L,
      n_assays = 92L,
      n_samples = n_samples,
      show_dev_int_ctrl = FALSE,
      show_int_ctrl = TRUE,
      version = version
    )

    withr::with_tempfile(
      new = "file_wide_xlsx",
      pattern = "test_long",
      fileext = ".xlsx",
      code = {
        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_rand$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        # check that function works
        expect_message(
          expect_warning(
            object = expect_no_error(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            ),
            regexp = "You are using the function read_npx_legacy"
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_rand$list_df_long$df_long,
          long_legacy = npx_legacy_xlsx,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    ## Target 96 NPX, w dev int ctrl, w int ctrl, 2 plates, v1 ----

    # get synthetic data, or skip if not available
    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = 3L,
      n_assays = 92L,
      n_samples = n_samples,
      show_dev_int_ctrl = TRUE,
      show_int_ctrl = TRUE,
      version = version
    )

    withr::with_tempfile(
      new = "file_wide_xlsx",
      pattern = "test_long",
      fileext = ".xlsx",
      code = {
        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_rand$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        # check that function works
        expect_message(
          expect_warning(
            object = expect_no_error(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            ),
            regexp = "You are using the function read_npx_legacy"
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_rand$list_df_long$df_long,
          long_legacy = npx_legacy_xlsx,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    # vars 88 samples version 2 ----

    version <- 2L

    ## Target 96 NPX, no dev int ctrl, no int ctrl, 1 plate ----

    # get synthetic data, or skip if not available
    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = 3L,
      n_assays = 92L,
      n_samples = n_samples,
      show_dev_int_ctrl = FALSE,
      show_int_ctrl = FALSE,
      version = version
    )

    withr::with_tempfile(
      new = "file_wide_xlsx",
      pattern = "test_long",
      fileext = ".xlsx",
      code = {
        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_rand$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        # check that function works
        expect_message(
          expect_warning(
            object = expect_no_error(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            ),
            regexp = "You are using the function read_npx_legacy"
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_rand$list_df_long$df_long,
          long_legacy = npx_legacy_xlsx,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    ## Target 96 NPX, no dev int ctrl, w int ctrl, 1 plate ----

    # get synthetic data, or skip if not available
    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = 3L,
      n_assays = 92L,
      n_samples = n_samples,
      show_dev_int_ctrl = FALSE,
      show_int_ctrl = TRUE,
      version = version
    )

    withr::with_tempfile(
      new = "file_wide_xlsx",
      pattern = "test_long",
      fileext = ".xlsx",
      code = {
        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_rand$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        # check that function works
        expect_message(
          expect_warning(
            object = expect_no_error(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            ),
            regexp = "You are using the function read_npx_legacy"
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_rand$list_df_long$df_long,
          long_legacy = npx_legacy_xlsx,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    ## Target 96 NPX, w dev int ctrl, w int ctrl, 1 plate ----

    # get synthetic data, or skip if not available
    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = 3L,
      n_assays = 92L,
      n_samples = n_samples,
      show_dev_int_ctrl = TRUE,
      show_int_ctrl = TRUE,
      version = version
    )

    withr::with_tempfile(
      new = "file_wide_xlsx",
      pattern = "test_long",
      fileext = ".xlsx",
      code = {# write in excel
        olink_wide_order_cols(
          list_df_wide = df_rand$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        # check that function works
        expect_message(
          expect_warning(
            object = expect_no_error(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            ),
            regexp = "You are using the function read_npx_legacy"
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_rand$list_df_long$df_long,
          long_legacy = npx_legacy_xlsx,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    # vars 99 samples ----

    n_samples <- 99L

    ## Target 96 NPX, no dev int ctrl, no int ctrl, 2 plates ----

    # get synthetic data, or skip if not available
    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = 3L,
      n_assays = 92L,
      n_samples = n_samples,
      show_dev_int_ctrl = FALSE,
      show_int_ctrl = FALSE,
      version = version
    )

    withr::with_tempfile(
      new = "file_wide_xlsx",
      pattern = "test_long",
      fileext = ".xlsx",
      code = {
        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_rand$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        # check that function works
        expect_message(
          expect_warning(
            object = expect_no_error(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            ),
            regexp = "You are using the function read_npx_legacy"
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_rand$list_df_long$df_long,
          long_legacy = npx_legacy_xlsx,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    ## Target 96 NPX, no dev int ctrl, w int ctrl, 2 plates ----

    # get synthetic data, or skip if not available
    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = 3L,
      n_assays = 92L,
      n_samples = n_samples,
      show_dev_int_ctrl = FALSE,
      show_int_ctrl = TRUE,
      version = version
    )

    withr::with_tempfile(
      new = "file_wide_xlsx",
      pattern = "test_long",
      fileext = ".xlsx",
      code = {
        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_rand$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        # check that function works
        expect_message(
          expect_warning(
            object = expect_no_error(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            ),
            regexp = "You are using the function read_npx_legacy"
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_rand$list_df_long$df_long,
          long_legacy = npx_legacy_xlsx,
          olink_platform = olink_platform
        )

        expect_equal(
          object = lst_df$df_legacy,
          expected = lst_df$df_expected,
          tolerance = 1e-4
        )
      }
    )

    ## Target 96 NPX, w dev int ctrl, w int ctrl, 2 plates ----

    # get synthetic data, or skip if not available
    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = 3L,
      n_assays = 92L,
      n_samples = n_samples,
      show_dev_int_ctrl = TRUE,
      show_int_ctrl = TRUE,
      version = version
    )

    withr::with_tempfile(
      new = "file_wide_xlsx",
      pattern = "test_long",
      fileext = ".xlsx",
      code = {
        # write in excel
        olink_wide_order_cols(
          list_df_wide = df_rand$list_df_wide
        ) |>
          writexl::write_xlsx(
            path = file_wide_xlsx,
            col_names = FALSE,
            format_headers = FALSE
          )

        # check that function works
        expect_message(
          expect_warning(
            object = expect_no_error(
              object = npx_legacy_xlsx <- read_npx_legacy(
                file = file_wide_xlsx,
                out_df = "tibble",
                olink_platform = NULL,
                data_type = NULL,
                quiet = FALSE
              )
            ),
            regexp = "You are using the function read_npx_legacy"
          ),
          regexp = paste(olink_platform, "data in wide form detected")
        )

        # check that the correct values are returned
        lst_df <- expected_vs_legacy_df_prep(
          long_expected = df_rand$list_df_long$df_long,
          long_legacy = npx_legacy_xlsx,
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
  "read_npx_legacy - error - wrong order of cols",
  {
    skip_if_not_installed(pkg = "readxl")
    skip_if_not_installed(pkg = "writexl")

    ## Target 48 NPX, w dev int ctrl, w int ctrl, 1 plate, v1 ----

    # get synthetic data, or skip if not available
    df_rand <- get_wide_synthetic_data(
      olink_platform = "Target 48",
      data_type = "NPX",
      n_panels = 3L,
      n_assays = 45L,
      n_samples = 88L,
      show_dev_int_ctrl = TRUE,
      show_int_ctrl = TRUE,
      version = 1L
    )

    withr::with_tempfile(
      new = "file_wide_xlsx",
      pattern = "test_long",
      fileext = ".xlsx",
      code = {
        # write in excel
        writexl::write_xlsx(
          x = df_rand$list_df_wide$df_wide,
          path = file_wide_xlsx,
          col_names = FALSE,
          format_headers = FALSE
        )

        # check that function works
        expect_error(
          object = expect_warning(
            object = read_npx_legacy(
              file = file_wide_xlsx,
              out_df = "tibble",
              olink_platform = NULL,
              data_type = NULL,
              quiet = FALSE
            ),
            regexp = "You are using the function read_npx_legacy"
          ),
          regexp = "should be sorted!"
        )
      }
    )

    ## Target 48 NPX, no dev int ctrl, w int ctrl, 1 plate, v1 ----

    # get synthetic data, or skip if not available
    df_rand <- get_wide_synthetic_data(
      olink_platform = "Target 48",
      data_type = "NPX",
      n_panels = 3L,
      n_assays = 45L,
      n_samples = 88L,
      show_dev_int_ctrl = FALSE,
      show_int_ctrl = TRUE,
      version = 1L
    )

    withr::with_tempfile(
      new = "file_wide_xlsx",
      pattern = "test_long",
      fileext = ".xlsx",
      code = {
        # write in excel
        writexl::write_xlsx(
          x = df_rand$list_df_wide$df_wide,
          path = file_wide_xlsx,
          col_names = FALSE,
          format_headers = FALSE
        )

        # check that function works
        expect_error(
          object = expect_warning(
            object = read_npx_legacy(
              file = file_wide_xlsx,
              out_df = "tibble",
              olink_platform = NULL,
              data_type = NULL,
              quiet = FALSE
            ),
            regexp = "You are using the function read_npx_legacy"
          ),
          regexp = "should be sorted!"
        )
      }
    )
  }
)

test_that(
  "read_npx_legacy - error - dev int ctrl but no int ctrl",
  {
    skip_if_not_installed(pkg = "readxl")
    skip_if_not_installed(pkg = "writexl")

    # get synthetic data, or skip if not available
    df_rand <- get_wide_synthetic_data(
      olink_platform = "Target 48",
      data_type = "NPX",
      n_panels = 1L,
      n_assays = 45L,
      n_samples = 88L,
      show_dev_int_ctrl = TRUE,
      show_int_ctrl = FALSE,
      version = 1L
    )

    withr::with_tempfile(
      new = "file_wide_xlsx",
      pattern = "test_long",
      fileext = ".xlsx",
      code = {
        # write in excel
        writexl::write_xlsx(
          x = df_rand$list_df_wide$df_wide,
          path = file_wide_xlsx,
          col_names = FALSE,
          format_headers = FALSE
        )

        # check that function works
        expect_error(
          object = expect_warning(
            object = read_npx_legacy(
              file = file_wide_xlsx,
              out_df = "tibble",
              olink_platform = NULL,
              data_type = NULL,
              quiet = FALSE
            ),
            regexp = "You are using the function read_npx_legacy"
          ),
          regexp = "but lacks"
        )
      }
    )
  }
)

test_that(
  "read_npx_legacy - error - bottom matrix unsupported version",
  {
    skip_if_not_installed(pkg = "readxl")
    skip_if_not_installed(pkg = "writexl")

    # T48 NPX v2 ----

    # get synthetic data, or skip if not available
    df_rand <- get_wide_synthetic_data(
      olink_platform = "Target 48",
      data_type = "NPX",
      n_panels = 1L,
      n_assays = 45L,
      n_samples = 88L,
      show_dev_int_ctrl = FALSE,
      show_int_ctrl = FALSE,
      version = 2L
    )

    withr::with_tempfile(
      new = "file_wide_xlsx",
      pattern = "test_long",
      fileext = ".xlsx",
      code = {
        # write in excel
        writexl::write_xlsx(
          x = df_rand$list_df_wide$df_wide,
          path = file_wide_xlsx,
          col_names = FALSE,
          format_headers = FALSE
        )

        # check that function works
        expect_error(
          object = expect_warning(
            object = read_npx_legacy(
              file = file_wide_xlsx,
              out_df = "tibble",
              olink_platform = NULL,
              data_type = NULL,
              quiet = FALSE
            ),
            regexp = "You are using the function read_npx_legacy"
          ),
          regexp = "contains bottom matrix with unsupported labels"
        )
      }
    )

    # T96 NPX v3 ----

    # get synthetic data, or skip if not available
    df_rand <- get_wide_synthetic_data(
      olink_platform = "Target 96",
      data_type = "NPX",
      n_panels = 1L,
      n_assays = 92L,
      n_samples = 88L,
      show_dev_int_ctrl = FALSE,
      show_int_ctrl = FALSE,
      version = 3L
    )

    withr::with_tempfile(
      new = "file_wide_xlsx",
      pattern = "test_long",
      fileext = ".xlsx",
      code = {
        # write in excel
        writexl::write_xlsx(
          x = df_rand$list_df_wide$df_wide,
          path = file_wide_xlsx,
          col_names = FALSE,
          format_headers = FALSE
        )

        # check that function works
        expect_error(
          object = expect_warning(
            object = read_npx_legacy(
              file = file_wide_xlsx,
              out_df = "tibble",
              olink_platform = NULL,
              data_type = NULL,
              quiet = FALSE
            ),
            regexp = "You are using the function read_npx_legacy"
          ),
          regexp = "contains bottom matrix with unsupported labels"
        )
      }
    )
  }
)
