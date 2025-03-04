# Test read_npx_format ----

test_that(
  "read_npx_format - works - long",
  {
    skip_if_not_installed("writexl")

    # get wide synthetic data
    df_synthetic <- get_wide_synthetic_data(olink_platform = "Target 48",
                                            data_type = "NPX",
                                            n_panels = 3L,
                                            n_assays = 45L,
                                            n_samples = 99L,
                                            show_dev_int_ctrl = TRUE,
                                            show_int_ctrl = TRUE,
                                            version = 2L)

    ## csv semicolon ----

    withr::with_tempfile(
      new = "csv_long",
      pattern = "test_long",
      fileext = ".csv",
      code = {

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
        expect_message(
          object = df_out_v1 <- read_npx_format(
            file = csv_long,
            out_df = "tibble",
            sep = NULL,
            long_format = NULL,
            olink_platform = NULL,
            data_type = NULL,
            quiet = FALSE
          ),
          regexp = "Detected data in long format"
        )

        # check that read_npx_format works olink_platform T48
        expect_message(
          object = df_out_v2 <- read_npx_format(
            file = csv_long,
            out_df = "tibble",
            sep = NULL,
            long_format = NULL,
            olink_platform = "Target 48",
            data_type = NULL,
            quiet = FALSE
          ),
          regexp = "Detected data in long format"
        )

        # check that read_npx_format works long_format TRUE
        expect_message(
          object = df_out_v3 <- read_npx_format(
            file = csv_long,
            out_df = "tibble",
            sep = NULL,
            long_format = TRUE,
            olink_platform = "Target 48",
            data_type = NULL,
            quiet = FALSE
          ),
          regexp = "Detected data in long format"
        )

        # check that read_npx_format works data_type NPX
        expect_message(
          object = df_out_v4 <- read_npx_format(
            file = csv_long,
            out_df = "tibble",
            sep = NULL,
            long_format = TRUE,
            olink_platform = "Target 48",
            data_type = "NPX",
            quiet = FALSE
          ),
          regexp = "Detected data in long format"
        )

        # check if df identical
        expect_identical(
          object = df_out_v1 |>
            dplyr::mutate(
              dplyr::across(
                dplyr::everything(),
                ~ as.character(.x)
              )
            ),
          expected = df_synthetic$list_df_long$df_long
        )

        expect_identical(
          object = df_out_v2 |>
            dplyr::mutate(
              dplyr::across(
                dplyr::everything(),
                ~ as.character(.x)
              )
            ),
          expected = df_synthetic$list_df_long$df_long
        )

        expect_identical(
          object = df_out_v3 |>
            dplyr::mutate(
              dplyr::across(
                dplyr::everything(),
                ~ as.character(.x)
              )
            ),
          expected = df_synthetic$list_df_long$df_long
        )

        expect_identical(
          object = df_out_v4 |>
            dplyr::mutate(
              dplyr::across(
                dplyr::everything(),
                ~ as.character(.x)
              )
            ),
          expected = df_synthetic$list_df_long$df_long
        )

      }
    )

    ## csv comma ----

    withr::with_tempfile(
      new = "csv_long",
      pattern = "test_long",
      fileext = ".csv",
      code = {

        # write in csv
        write.table(x = df_synthetic$list_df_long$df_long,
                    file = csv_long,
                    append = FALSE,
                    sep = ",",
                    quote = FALSE,
                    eol = "\n",
                    na = "",
                    dec = ".",
                    row.names = FALSE,
                    col.names = TRUE)

        #check that file exists
        expect_true(object = file.exists(csv_long))

        # check that read_npx_format works
        expect_message(
          object = df_out_v1 <- read_npx_format(
            file = csv_long,
            out_df = "tibble",
            sep = NULL,
            long_format = NULL,
            olink_platform = NULL,
            data_type = NULL,
            quiet = FALSE
          ),
          regexp = "Detected data in long format"
        )

        # check that read_npx_format works olink_platform T48
        expect_message(
          object = df_out_v2 <- read_npx_format(
            file = csv_long,
            out_df = "tibble",
            sep = NULL,
            long_format = NULL,
            olink_platform = "Target 48",
            data_type = NULL,
            quiet = FALSE
          ),
          regexp = "Detected data in long format"
        )

        # check that read_npx_format works long_format TRUE
        expect_message(
          object = df_out_v3 <- read_npx_format(
            file = csv_long,
            out_df = "tibble",
            sep = NULL,
            long_format = TRUE,
            olink_platform = "Target 48",
            data_type = NULL,
            quiet = FALSE
          ),
          regexp = "Detected data in long format"
        )

        # check that read_npx_format works data_type NPX
        expect_message(
          object = df_out_v4 <- read_npx_format(
            file = csv_long,
            out_df = "tibble",
            sep = NULL,
            long_format = TRUE,
            olink_platform = "Target 48",
            data_type = "NPX",
            quiet = FALSE
          ),
          regexp = "Detected data in long format"
        )

        # check if df identical
        expect_identical(
          object = df_out_v1 |>
            dplyr::mutate(
              dplyr::across(
                dplyr::everything(),
                ~ as.character(.x)
              )
            ),
          expected = df_synthetic$list_df_long$df_long
        )

        expect_identical(
          object = df_out_v2 |>
            dplyr::mutate(
              dplyr::across(
                dplyr::everything(),
                ~ as.character(.x)
              )
            ),
          expected = df_synthetic$list_df_long$df_long
        )

        expect_identical(
          object = df_out_v3 |>
            dplyr::mutate(
              dplyr::across(
                dplyr::everything(),
                ~ as.character(.x)
              )
            ),
          expected = df_synthetic$list_df_long$df_long
        )

        expect_identical(
          object = df_out_v4 |>
            dplyr::mutate(
              dplyr::across(
                dplyr::everything(),
                ~ as.character(.x)
              )
            ),
          expected = df_synthetic$list_df_long$df_long
        )

      }
    )

    ## excel ----

    withr::with_tempfile(
      new = "excel_long",
      pattern = "test_long",
      fileext = ".xlsx",
      code = {

        # write in csv
        writexl::write_xlsx(x = df_synthetic$list_df_long$df_long,
                            path = excel_long,
                            col_names = TRUE,
                            format_headers = FALSE)

        #check that file exists
        expect_true(object = file.exists(excel_long))

        # check that read_npx_format works
        expect_message(
          object = df_out_v1 <- read_npx_format(
            file = excel_long,
            out_df = "tibble",
            sep = NULL,
            long_format = NULL,
            olink_platform = NULL,
            data_type = NULL,
            quiet = FALSE
          ),
          regexp = "Detected data in long format"
        )

        # check that read_npx_format works olink_platform T48
        expect_message(
          object = df_out_v2 <- read_npx_format(
            file = excel_long,
            out_df = "tibble",
            sep = NULL,
            long_format = NULL,
            olink_platform = "Target 48",
            data_type = NULL,
            quiet = FALSE
          ),
          regexp = "Detected data in long format"
        )

        # check that read_npx_format works long_format TRUE
        expect_message(
          object = df_out_v3 <- read_npx_format(
            file = excel_long,
            out_df = "tibble",
            sep = NULL,
            long_format = TRUE,
            olink_platform = "Target 48",
            data_type = NULL,
            quiet = FALSE
          ),
          regexp = "Detected data in long format"
        )

        # check that read_npx_format works data_type NPX
        expect_message(
          object = df_out_v4 <- read_npx_format(
            file = excel_long,
            out_df = "tibble",
            sep = NULL,
            long_format = TRUE,
            olink_platform = "Target 48",
            data_type = "NPX",
            quiet = FALSE
          ),
          regexp = "Detected data in long format"
        )

        # check if df identical
        expect_identical(
          object = df_out_v1 |>
            dplyr::mutate(
              dplyr::across(
                dplyr::everything(),
                ~ as.character(.x)
              )
            ),
          expected = df_synthetic$list_df_long$df_long
        )

        expect_identical(
          object = df_out_v2 |>
            dplyr::mutate(
              dplyr::across(
                dplyr::everything(),
                ~ as.character(.x)
              )
            ),
          expected = df_synthetic$list_df_long$df_long
        )

        expect_identical(
          object = df_out_v3 |>
            dplyr::mutate(
              dplyr::across(
                dplyr::everything(),
                ~ as.character(.x)
              )
            ),
          expected = df_synthetic$list_df_long$df_long
        )

        expect_identical(
          object = df_out_v4 |>
            dplyr::mutate(
              dplyr::across(
                dplyr::everything(),
                ~ as.character(.x)
              )
            ),
          expected = df_synthetic$list_df_long$df_long
        )

      }
    )

    ## legacy ----

    ### T48 NPX ----

    withr::with_tempfile(
      new = "excel_long",
      pattern = "test_long",
      fileext = ".xlsx",
      code = {

        data_type <- "NPX"
        olink_platform <- "Target 48"

        # df_synthetic_legacy
        df_synthetic_legacy <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = 3L,
          n_assays = 45L,
          n_samples = 99L,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = FALSE,
          version = 2L
        )

        # write in csv
        writexl::write_xlsx(x = df_synthetic_legacy$list_df_long$df_long,
                            path = excel_long,
                            col_names = TRUE,
                            format_headers = FALSE)

        #check that file exists
        expect_true(object = file.exists(excel_long))

        # check that read_npx_format works
        expect_message(
          object = list_out_v1 <- read_npx_format(
            file = excel_long,
            out_df = "tibble",
            sep = NULL,
            long_format = NULL,
            olink_platform = NULL,
            data_type = NULL,
            quiet = FALSE,
            legacy = TRUE
          ),
          regexp = "Detected data in long format"
        )

        # modify list_out_v1
        list_out_v1$df <- list_out_v1$df |>
          dplyr::mutate(
            dplyr::across(
              dplyr::everything(),
              ~ as.character(.x)
            )
          )

        # check if df identical
        expect_identical(
          object = list_out_v1,
          expected = list(
            olink_platform = NULL,
            long_format = TRUE,
            data_type = NULL,
            df = df_synthetic_legacy$list_df_long$df_long
          )
        )

      }
    )

    ### T48 Quantified ----

    withr::with_tempfile(
      new = "excel_long",
      pattern = "test_long",
      fileext = ".xlsx",
      code = {

        data_type <- "Quantified"
        olink_platform <- "Target 48"

        # df_synthetic_legacy
        df_synthetic_legacy <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = 3L,
          n_assays = 45L,
          n_samples = 99L,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = FALSE,
          version = 0L
        )

        # write in csv
        writexl::write_xlsx(x = df_synthetic_legacy$list_df_long$df_long,
                            path = excel_long,
                            col_names = TRUE,
                            format_headers = FALSE)

        #check that file exists
        expect_true(object = file.exists(excel_long))

        # check that read_npx_format works
        expect_message(
          object = list_out_v1 <- read_npx_format(
            file = excel_long,
            out_df = "tibble",
            sep = NULL,
            long_format = NULL,
            olink_platform = NULL,
            data_type = NULL,
            quiet = FALSE,
            legacy = TRUE
          ),
          regexp = "Detected data in long format"
        )

        # modify list_out_v1
        list_out_v1$df <- list_out_v1$df |>
          dplyr::mutate(
            dplyr::across(
              dplyr::everything(),
              ~ as.character(.x)
            )
          )

        # check if df identical
        expect_identical(
          object = list_out_v1,
          expected = list(
            olink_platform = NULL,
            long_format = TRUE,
            data_type = NULL,
            df = df_synthetic_legacy$list_df_long$df_long
          )
        )

      }
    )

    ### T48 Ct ----

    withr::with_tempfile(
      new = "excel_long",
      pattern = "test_long",
      fileext = ".xlsx",
      code = {

        data_type <- "Ct"
        olink_platform <- "Target 48"

        # df_synthetic_legacy
        df_synthetic_legacy <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = 3L,
          n_assays = 45L,
          n_samples = 99L,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = FALSE,
          version = 0L
        )

        # write in csv
        writexl::write_xlsx(x = df_synthetic_legacy$list_df_long$df_long,
                            path = excel_long,
                            col_names = TRUE,
                            format_headers = FALSE)

        #check that file exists
        expect_true(object = file.exists(excel_long))

        # check that read_npx_format works
        expect_message(
          object = list_out_v1 <- read_npx_format(
            file = excel_long,
            out_df = "tibble",
            sep = NULL,
            long_format = NULL,
            olink_platform = NULL,
            data_type = NULL,
            quiet = FALSE,
            legacy = TRUE
          ),
          regexp = "Detected data in long format"
        )

        # modify list_out_v1
        list_out_v1$df <- list_out_v1$df |>
          dplyr::mutate(
            dplyr::across(
              dplyr::everything(),
              ~ as.character(.x)
            )
          )

        # check if df identical
        expect_identical(
          object = list_out_v1,
          expected = list(
            olink_platform = NULL,
            long_format = TRUE,
            data_type = NULL,
            df = df_synthetic_legacy$list_df_long$df_long
          )
        )

      }
    )

    ### T96 NPX ----

    withr::with_tempfile(
      new = "excel_long",
      pattern = "test_long",
      fileext = ".xlsx",
      code = {

        data_type <- "NPX"
        olink_platform <- "Target 96"

        # df_synthetic_legacy
        df_synthetic_legacy <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = 3L,
          n_assays = 92L,
          n_samples = 99L,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = FALSE,
          version = 2L
        )

        # write in csv
        writexl::write_xlsx(x = df_synthetic_legacy$list_df_long$df_long,
                            path = excel_long,
                            col_names = TRUE,
                            format_headers = FALSE)

        #check that file exists
        expect_true(object = file.exists(excel_long))

        # check that read_npx_format works
        expect_message(
          object = list_out_v1 <- read_npx_format(
            file = excel_long,
            out_df = "tibble",
            sep = NULL,
            long_format = NULL,
            olink_platform = NULL,
            data_type = NULL,
            quiet = FALSE,
            legacy = TRUE
          ),
          regexp = "Detected data in long format"
        )

        # modify list_out_v1
        list_out_v1$df <- list_out_v1$df |>
          dplyr::mutate(
            dplyr::across(
              dplyr::everything(),
              ~ as.character(.x)
            )
          )

        # check if df identical
        expect_identical(
          object = list_out_v1,
          expected = list(
            olink_platform = NULL,
            long_format = TRUE,
            data_type = NULL,
            df = df_synthetic_legacy$list_df_long$df_long
          )
        )

      }
    )

    ### T96 Ct ----

    withr::with_tempfile(
      new = "excel_long",
      pattern = "test_long",
      fileext = ".xlsx",
      code = {

        data_type <- "Ct"
        olink_platform <- "Target 96"

        # df_synthetic_legacy
        df_synthetic_legacy <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = 3L,
          n_assays = 92L,
          n_samples = 99L,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = FALSE,
          version = 0L
        )

        # write in csv
        writexl::write_xlsx(x = df_synthetic_legacy$list_df_long$df_long,
                            path = excel_long,
                            col_names = TRUE,
                            format_headers = FALSE)

        #check that file exists
        expect_true(object = file.exists(excel_long))

        # check that read_npx_format works
        expect_message(
          object = list_out_v1 <- read_npx_format(
            file = excel_long,
            out_df = "tibble",
            sep = NULL,
            long_format = NULL,
            olink_platform = NULL,
            data_type = NULL,
            quiet = FALSE,
            legacy = TRUE
          ),
          regexp = "Detected data in long format"
        )

        # modify list_out_v1
        list_out_v1$df <- list_out_v1$df |>
          dplyr::mutate(
            dplyr::across(
              dplyr::everything(),
              ~ as.character(.x)
            )
          )

        # check if df identical
        expect_identical(
          object = list_out_v1,
          expected = list(
            olink_platform = NULL,
            long_format = TRUE,
            data_type = NULL,
            df = df_synthetic_legacy$list_df_long$df_long
          )
        )

      }
    )

  }
)

test_that(
  "read_npx_format - works - wide",
  {
    skip_if_not_installed("writexl")

    # get wide synthetic data
    df_synthetic <- get_wide_synthetic_data(olink_platform = "Target 48",
                                            data_type = "NPX",
                                            n_panels = 3L,
                                            n_assays = 45L,
                                            n_samples = 99L,
                                            show_dev_int_ctrl = TRUE,
                                            show_int_ctrl = TRUE,
                                            version = 2L)

    ## csv semicolon ----

    withr::with_tempfile(
      new = "csv_wide",
      pattern = "test_wide",
      fileext = ".csv",
      code = {

        # write in csv
        write.table(x = df_synthetic$list_df_wide$df_wide,
                    file = csv_wide,
                    append = FALSE,
                    sep = ";",
                    quote = FALSE,
                    eol = "\n",
                    na = "",
                    dec = ".",
                    row.names = FALSE,
                    col.names = FALSE)

        #check that file exists
        expect_true(object = file.exists(csv_wide))

        # check that read_npx_format works
        expect_message(
          object = df_out_v1 <- read_npx_format(
            file = csv_wide,
            out_df = "tibble",
            sep = NULL,
            long_format = NULL,
            olink_platform = NULL,
            data_type = NULL,
            quiet = FALSE
          ),
          regexp = "Detected \"NPX\" data from \"Olink Target 48\" in wide for"
        )

        # check that read_npx_format works olink_platform T48
        expect_message(
          object = df_out_v2 <- read_npx_format(
            file = csv_wide,
            out_df = "tibble",
            sep = NULL,
            long_format = NULL,
            olink_platform = "Target 48",
            data_type = NULL,
            quiet = FALSE
          ),
          regexp = "Detected \"NPX\" data from \"Olink Target 48\" in wide for"
        )

        # check that read_npx_format works long_format FALSE
        expect_message(
          object = df_out_v3 <- read_npx_format(
            file = csv_wide,
            out_df = "tibble",
            sep = NULL,
            long_format = FALSE,
            olink_platform = "Target 48",
            data_type = NULL,
            quiet = FALSE
          ),
          regexp = "Detected \"NPX\" data from \"Olink Target 48\" in wide for"
        )

        # check that read_npx_format works data_type NPX
        expect_message(
          object = df_out_v4 <- read_npx_format(
            file = csv_wide,
            out_df = "tibble",
            sep = NULL,
            long_format = FALSE,
            olink_platform = "Target 48",
            data_type = "NPX",
            quiet = FALSE
          ),
          regexp = "Detected \"NPX\" data from \"Olink Target 48\" in wide for"
        )

        # check if df identical
        expect_identical(
          object = df_out_v1 |>
            dplyr::mutate(
              dplyr::across(
                dplyr::everything(),
                ~ as.character(.x)
              )
            ) |>
            dplyr::select(
              dplyr::all_of(names(df_synthetic$list_df_long$df_long))
            ),
          expected = df_synthetic$list_df_long$df_long
        )

        expect_identical(
          object = df_out_v2 |>
            dplyr::mutate(
              dplyr::across(
                dplyr::everything(),
                ~ as.character(.x)
              )
            ) |>
            dplyr::select(
              dplyr::all_of(names(df_synthetic$list_df_long$df_long))
            ),
          expected = df_synthetic$list_df_long$df_long
        )

        expect_identical(
          object = df_out_v3 |>
            dplyr::mutate(
              dplyr::across(
                dplyr::everything(),
                ~ as.character(.x)
              )
            ) |>
            dplyr::select(
              dplyr::all_of(names(df_synthetic$list_df_long$df_long))
            ),
          expected = df_synthetic$list_df_long$df_long
        )

        expect_identical(
          object = df_out_v4 |>
            dplyr::mutate(
              dplyr::across(
                dplyr::everything(),
                ~ as.character(.x)
              )
            ) |>
            dplyr::select(
              dplyr::all_of(names(df_synthetic$list_df_long$df_long))
            ),
          expected = df_synthetic$list_df_long$df_long
        )

      }
    )

    ## csv comma ----

    withr::with_tempfile(
      new = "csv_wide",
      pattern = "test_wide",
      fileext = ".csv",
      code = {

        # write in csv
        write.table(x = df_synthetic$list_df_wide$df_wide,
                    file = csv_wide,
                    append = FALSE,
                    sep = ",",
                    quote = FALSE,
                    eol = "\n",
                    na = "",
                    dec = ".",
                    row.names = FALSE,
                    col.names = FALSE)

        #check that file exists
        expect_true(object = file.exists(csv_wide))

        # check that read_npx_format works
        expect_message(
          object = df_out_v1 <- read_npx_format(
            file = csv_wide,
            out_df = "tibble",
            sep = NULL,
            long_format = NULL,
            olink_platform = NULL,
            data_type = NULL,
            quiet = FALSE
          ),
          regexp = "Detected \"NPX\" data from \"Olink Target 48\" in wide for"
        )

        # check that read_npx_format works olink_platform T48
        expect_message(
          object = df_out_v2 <- read_npx_format(
            file = csv_wide,
            out_df = "tibble",
            sep = NULL,
            long_format = NULL,
            olink_platform = "Target 48",
            data_type = NULL,
            quiet = FALSE
          ),
          regexp = "Detected \"NPX\" data from \"Olink Target 48\" in wide for"
        )

        # check that read_npx_format works long_format FALSE
        expect_message(
          object = df_out_v3 <- read_npx_format(
            file = csv_wide,
            out_df = "tibble",
            sep = NULL,
            long_format = FALSE,
            olink_platform = "Target 48",
            data_type = NULL,
            quiet = FALSE
          ),
          regexp = "Detected \"NPX\" data from \"Olink Target 48\" in wide for"
        )

        # check that read_npx_format works data_type NPX
        expect_message(
          object = df_out_v4 <- read_npx_format(
            file = csv_wide,
            out_df = "tibble",
            sep = NULL,
            long_format = FALSE,
            olink_platform = "Target 48",
            data_type = "NPX",
            quiet = FALSE
          ),
          regexp = "Detected \"NPX\" data from \"Olink Target 48\" in wide for"
        )

        # check if df identical
        expect_identical(
          object = df_out_v1 |>
            dplyr::mutate(
              dplyr::across(
                dplyr::everything(),
                ~ as.character(.x)
              )
            ) |>
            dplyr::select(
              dplyr::all_of(names(df_synthetic$list_df_long$df_long))
            ),
          expected = df_synthetic$list_df_long$df_long
        )

        expect_identical(
          object = df_out_v2 |>
            dplyr::mutate(
              dplyr::across(
                dplyr::everything(),
                ~ as.character(.x)
              )
            ) |>
            dplyr::select(
              dplyr::all_of(names(df_synthetic$list_df_long$df_long))
            ),
          expected = df_synthetic$list_df_long$df_long
        )

        expect_identical(
          object = df_out_v3 |>
            dplyr::mutate(
              dplyr::across(
                dplyr::everything(),
                ~ as.character(.x)
              )
            ) |>
            dplyr::select(
              dplyr::all_of(names(df_synthetic$list_df_long$df_long))
            ),
          expected = df_synthetic$list_df_long$df_long
        )

        expect_identical(
          object = df_out_v4 |>
            dplyr::mutate(
              dplyr::across(
                dplyr::everything(),
                ~ as.character(.x)
              )
            ) |>
            dplyr::select(
              dplyr::all_of(names(df_synthetic$list_df_long$df_long))
            ),
          expected = df_synthetic$list_df_long$df_long
        )

      }
    )

    ## excel ----

    withr::with_tempfile(
      new = "excel_wide",
      pattern = "test_wide",
      fileext = ".xlsx",
      code = {

        # write in csv
        writexl::write_xlsx(x = df_synthetic$list_df_wide$df_wide,
                            path = excel_wide,
                            col_names = FALSE,
                            format_headers = FALSE)

        #check that file exists
        expect_true(object = file.exists(excel_wide))

        # check that read_npx_format works
        expect_message(
          object = df_out_v1 <- read_npx_format(
            file = excel_wide,
            out_df = "tibble",
            sep = NULL,
            long_format = NULL,
            olink_platform = NULL,
            data_type = NULL,
            quiet = FALSE
          ),
          regexp = "Detected \"NPX\" data from \"Olink Target 48\" in wide for"
        )

        # check that read_npx_format works olink_platform T48
        expect_message(
          object = df_out_v2 <- read_npx_format(
            file = excel_wide,
            out_df = "tibble",
            sep = NULL,
            long_format = NULL,
            olink_platform = "Target 48",
            data_type = NULL,
            quiet = FALSE
          ),
          regexp = "Detected \"NPX\" data from \"Olink Target 48\" in wide for"
        )

        # check that read_npx_format works long_format FALSE
        expect_message(
          object = df_out_v3 <- read_npx_format(
            file = excel_wide,
            out_df = "tibble",
            sep = NULL,
            long_format = FALSE,
            olink_platform = "Target 48",
            data_type = NULL,
            quiet = FALSE
          ),
          regexp = "Detected \"NPX\" data from \"Olink Target 48\" in wide for"
        )

        # check that read_npx_format works data_type NPX
        expect_message(
          object = df_out_v4 <- read_npx_format(
            file = excel_wide,
            out_df = "tibble",
            sep = NULL,
            long_format = FALSE,
            olink_platform = "Target 48",
            data_type = "NPX",
            quiet = FALSE
          ),
          regexp = "Detected \"NPX\" data from \"Olink Target 48\" in wide for"
        )

        # check if df identical
        expect_identical(
          object = df_out_v1 |>
            dplyr::mutate(
              dplyr::across(
                dplyr::everything(),
                ~ as.character(.x)
              )
            ) |>
            dplyr::select(
              dplyr::all_of(names(df_synthetic$list_df_long$df_long))
            ),
          expected = df_synthetic$list_df_long$df_long
        )

        expect_identical(
          object = df_out_v2 |>
            dplyr::mutate(
              dplyr::across(
                dplyr::everything(),
                ~ as.character(.x)
              )
            ) |>
            dplyr::select(
              dplyr::all_of(names(df_synthetic$list_df_long$df_long))
            ),
          expected = df_synthetic$list_df_long$df_long
        )

        expect_identical(
          object = df_out_v3 |>
            dplyr::mutate(
              dplyr::across(
                dplyr::everything(),
                ~ as.character(.x)
              )
            ) |>
            dplyr::select(
              dplyr::all_of(names(df_synthetic$list_df_long$df_long))
            ),
          expected = df_synthetic$list_df_long$df_long
        )

        expect_identical(
          object = df_out_v4 |>
            dplyr::mutate(
              dplyr::across(
                dplyr::everything(),
                ~ as.character(.x)
              )
            ) |>
            dplyr::select(
              dplyr::all_of(names(df_synthetic$list_df_long$df_long))
            ),
          expected = df_synthetic$list_df_long$df_long
        )

      }
    )

    ## legacy ----

    ### T48 NPX ----

    withr::with_tempfile(
      new = "excel_wide",
      pattern = "test_wide",
      fileext = ".xlsx",
      code = {

        data_type <- "NPX"
        olink_platform <- "Target 48"

        # df_synthetic_legacy
        df_synthetic_legacy <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = 3L,
          n_assays = 45L,
          n_samples = 99L,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = FALSE,
          version = 2L
        )

        # write in csv
        writexl::write_xlsx(x = df_synthetic_legacy$list_df_wide$df_wide,
                            path = excel_wide,
                            col_names = FALSE,
                            format_headers = FALSE)

        #check that file exists
        expect_true(object = file.exists(excel_wide))

        # check that read_npx_format works
        expect_message(
          object = list_out_v1 <- read_npx_format(
            file = excel_wide,
            out_df = "tibble",
            sep = NULL,
            long_format = NULL,
            olink_platform = NULL,
            data_type = NULL,
            quiet = FALSE,
            legacy = TRUE
          ),
          regexp = paste0("Detected \"", data_type, "\" data from \"Olink ",
                          olink_platform, "\" in wide format")
        )

        # modify list_out_v1
        list_out_v1$df <- list_out_v1$df |>
          dplyr::mutate(
            dplyr::across(
              dplyr::everything(),
              ~ as.character(.x)
            )
          )

        # check if df identical
        expect_identical(
          object = list_out_v1,
          expected = list(
            olink_platform = olink_platform,
            long_format = FALSE,
            data_type = data_type,
            df = df_synthetic_legacy$list_df_wide$df_wide
          )
        )

      }
    )

    ### T48 Quantified ----

    withr::with_tempfile(
      new = "excel_wide",
      pattern = "test_wide",
      fileext = ".xlsx",
      code = {

        data_type <- "Quantified"
        olink_platform <- "Target 48"

        # df_synthetic_legacy
        df_synthetic_legacy <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = 3L,
          n_assays = 45L,
          n_samples = 99L,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = FALSE,
          version = 0L
        )

        # write in csv
        writexl::write_xlsx(x = df_synthetic_legacy$list_df_wide$df_wide,
                            path = excel_wide,
                            col_names = FALSE,
                            format_headers = FALSE)

        #check that file exists
        expect_true(object = file.exists(excel_wide))

        # check that read_npx_format works
        expect_message(
          object = list_out_v1 <- read_npx_format(
            file = excel_wide,
            out_df = "tibble",
            sep = NULL,
            long_format = NULL,
            olink_platform = NULL,
            data_type = NULL,
            quiet = FALSE,
            legacy = TRUE
          ),
          regexp = paste0("Detected \"", data_type, "\" data from \"Olink ",
                          olink_platform, "\" in wide format")
        )

        # modify list_out_v1
        list_out_v1$df <- list_out_v1$df |>
          dplyr::mutate(
            dplyr::across(
              dplyr::everything(),
              ~ as.character(.x)
            )
          )

        # check if df identical
        expect_identical(
          object = list_out_v1,
          expected = list(
            olink_platform = olink_platform,
            long_format = FALSE,
            data_type = data_type,
            df = df_synthetic_legacy$list_df_wide$df_wide
          )
        )

      }
    )

    ### T48 Ct ----

    withr::with_tempfile(
      new = "excel_wide",
      pattern = "test_wide",
      fileext = ".xlsx",
      code = {

        data_type <- "Ct"
        olink_platform <- "Target 48"

        # df_synthetic_legacy
        df_synthetic_legacy <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = 3L,
          n_assays = 45L,
          n_samples = 99L,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = FALSE,
          version = 0L
        )

        # write in csv
        writexl::write_xlsx(x = df_synthetic_legacy$list_df_wide$df_wide,
                            path = excel_wide,
                            col_names = FALSE,
                            format_headers = FALSE)

        #check that file exists
        expect_true(object = file.exists(excel_wide))

        # check that read_npx_format works
        expect_message(
          object = list_out_v1 <- read_npx_format(
            file = excel_wide,
            out_df = "tibble",
            sep = NULL,
            long_format = NULL,
            olink_platform = NULL,
            data_type = NULL,
            quiet = FALSE,
            legacy = TRUE
          ),
          regexp = paste0("Detected \"", data_type, "\" data from \"Olink ",
                          olink_platform, "\" in wide format")
        )

        # modify list_out_v1
        list_out_v1$df <- list_out_v1$df |>
          dplyr::mutate(
            dplyr::across(
              dplyr::everything(),
              ~ as.character(.x)
            )
          )

        # check if df identical
        expect_identical(
          object = list_out_v1,
          expected = list(
            olink_platform = olink_platform,
            long_format = FALSE,
            data_type = data_type,
            df = df_synthetic_legacy$list_df_wide$df_wide
          )
        )

      }
    )

    ### T96 NPX ----

    withr::with_tempfile(
      new = "excel_wide",
      pattern = "test_wide",
      fileext = ".xlsx",
      code = {

        data_type <- "NPX"
        olink_platform <- "Target 96"

        # df_synthetic_legacy
        df_synthetic_legacy <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = 3L,
          n_assays = 92L,
          n_samples = 99L,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = FALSE,
          version = 2L
        )

        # write in csv
        writexl::write_xlsx(x = df_synthetic_legacy$list_df_wide$df_wide,
                            path = excel_wide,
                            col_names = FALSE,
                            format_headers = FALSE)

        #check that file exists
        expect_true(object = file.exists(excel_wide))

        # check that read_npx_format works
        expect_message(
          object = list_out_v1 <- read_npx_format(
            file = excel_wide,
            out_df = "tibble",
            sep = NULL,
            long_format = NULL,
            olink_platform = NULL,
            data_type = NULL,
            quiet = FALSE,
            legacy = TRUE
          ),
          regexp = paste0("Detected \"", data_type, "\" data from \"Olink ",
                          olink_platform, "\" in wide format")
        )

        # modify list_out_v1
        list_out_v1$df <- list_out_v1$df |>
          dplyr::mutate(
            dplyr::across(
              dplyr::everything(),
              ~ as.character(.x)
            )
          )

        # check if df identical
        expect_identical(
          object = list_out_v1,
          expected = list(
            olink_platform = olink_platform,
            long_format = FALSE,
            data_type = data_type,
            df = df_synthetic_legacy$list_df_wide$df_wide
          )
        )

      }
    )

    ### T96 Ct ----

    withr::with_tempfile(
      new = "excel_wide",
      pattern = "test_wide",
      fileext = ".xlsx",
      code = {

        data_type <- "Ct"
        olink_platform <- "Target 96"

        # df_synthetic_legacy
        df_synthetic_legacy <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = 3L,
          n_assays = 92L,
          n_samples = 99L,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = FALSE,
          version = 0L
        )

        # write in csv
        writexl::write_xlsx(x = df_synthetic_legacy$list_df_wide$df_wide,
                            path = excel_wide,
                            col_names = FALSE,
                            format_headers = FALSE)

        #check that file exists
        expect_true(object = file.exists(excel_wide))

        # check that read_npx_format works
        expect_message(
          object = list_out_v1 <- read_npx_format(
            file = excel_wide,
            out_df = "tibble",
            sep = NULL,
            long_format = NULL,
            olink_platform = NULL,
            data_type = NULL,
            quiet = FALSE,
            legacy = TRUE
          ),
          regexp = paste0("Detected \"", data_type, "\" data from \"Olink ",
                          olink_platform, "\" in wide format")
        )

        # modify list_out_v1
        list_out_v1$df <- list_out_v1$df |>
          dplyr::mutate(
            dplyr::across(
              dplyr::everything(),
              ~ as.character(.x)
            )
          )

        # check if df identical
        expect_identical(
          object = list_out_v1,
          expected = list(
            olink_platform = olink_platform,
            long_format = FALSE,
            data_type = data_type,
            df = df_synthetic_legacy$list_df_wide$df_wide
          )
        )

      }
    )

  }
)

# Test read_npx_format_read ----

test_that(
  "read_npx_format_read - works",
  {
    skip_if_not_installed("writexl")
    skip_on_cran()

    # get wide synthetic data
    df_synthetic <- get_wide_synthetic_data(olink_platform = "Target 48",
                                            data_type = "Quantified",
                                            n_panels = 3L,
                                            n_assays = 45L,
                                            n_samples = 88L,
                                            show_dev_int_ctrl = TRUE,
                                            show_int_ctrl = TRUE,
                                            version = 0L)

    # top n rows
    top_n_row <- 3L

    ## excel - wide ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # write wide df
        writexl::write_xlsx(
          x = df_synthetic$list_df_wide$df_wide,
          path = olink_wide_format,
          col_names = FALSE,
          format_headers = FALSE
        )

        # check that function runs
        expect_no_condition(
          object = df_read_out <- read_npx_format_read(
            file = olink_wide_format,
            sep = NULL,
            read_n = top_n_row
          )
        )

        # check that read_npx_format_read works
        expect_identical(
          object = df_read_out$df_top_n,
          expected = df_synthetic$list_df_wide$df_wide |>
            dplyr::slice_head(n = top_n_row) |>
            remove_all_na_cols()
        )

        expect_identical(
          object = dplyr::as_tibble(df_read_out$df),
          expected = df_synthetic$list_df_wide$df_wide
        )

      }
    )

    ## csv - wide ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".csv",
      code = {

        sep <- ";"

        # write wide df
        utils::write.table(x = df_synthetic$list_df_wide$df_wide,
                           file = olink_wide_format,
                           append = FALSE,
                           quote = FALSE,
                           sep = sep,
                           eol = "\n",
                           na = "",
                           dec = ".",
                           row.names = FALSE,
                           col.names = FALSE)

        # check that function runs
        expect_no_condition(
          object = df_read_out <- read_npx_format_read(
            file = olink_wide_format,
            sep = sep,
            read_n = top_n_row
          )
        )

        # check that read_npx_format_top_n works
        expect_identical(
          object = df_read_out$df_top_n,
          expected = df_synthetic$list_df_wide$df_wide |>
            dplyr::slice_head(n = top_n_row) |>
            remove_all_na_cols()
        )

        expect_identical(
          object = dplyr::as_tibble(df_read_out$df),
          expected = df_synthetic$list_df_wide$df_wide
        )

      }
    )

    ## txt - wide ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".txt",
      code = {

        sep <- ";"

        # write wide df
        utils::write.table(x = df_synthetic$list_df_wide$df_wide,
                           file = olink_wide_format,
                           append = FALSE,
                           quote = FALSE,
                           sep = sep,
                           eol = "\n",
                           na = "",
                           dec = ".",
                           row.names = FALSE,
                           col.names = FALSE)

        # check that function runs
        expect_no_condition(
          object = df_read_out <- read_npx_format_read(
            file = olink_wide_format,
            sep = sep,
            read_n = top_n_row
          )
        )

        # check that read_npx_format_read works
        expect_identical(
          object = df_read_out$df_top_n,
          expected = df_synthetic$list_df_wide$df_wide |>
            dplyr::slice_head(n = top_n_row) |>
            remove_all_na_cols()
        )

        expect_identical(
          object = dplyr::as_tibble(df_read_out$df),
          expected = df_synthetic$list_df_wide$df_wide
        )

      }
    )

    ## excel - long ----

    withr::with_tempfile(
      new = "olink_long_format",
      pattern = "test-olink-long",
      fileext = ".xlsx",
      code = {

        # write wide df
        writexl::write_xlsx(
          x = df_synthetic$list_df_long$df_long,
          path = olink_long_format,
          col_names = TRUE
        )

        # check that function runs
        expect_no_condition(
          object = df_read_out <- read_npx_format_read(
            file = olink_long_format,
            sep = NULL,
            read_n = top_n_row
          )
        )

        df_exp_r <- df_synthetic$list_df_long$df_long |>
          dplyr::slice_head(n = top_n_row - 1L)
        colnames(df_exp_r) <- paste0("V", seq_len(ncol(df_exp_r)))

        df_exp <- colnames(df_synthetic$list_df_long$df_long) |>
          dplyr::tibble() |>
          t()
        rownames(df_exp) <- NULL
        colnames(df_exp) <- paste0("V", seq_len(ncol(df_exp)))
        df_exp <- df_exp |>
          dplyr::as_tibble() |>
          dplyr::bind_rows(
            df_exp_r
          )

        # check that read_npx_format_top_n works
        expect_identical(
          object = df_read_out$df_top_n,
          expected = df_exp
        )

        expect_identical(
          object = dplyr::as_tibble(df_read_out$df) |>
            dplyr::mutate(
              dplyr::across(
                dplyr::everything(),
                ~ as.character(.x)
              )
            ),
          expected = df_synthetic$list_df_long$df_long
        )

      }
    )

    ## csv - long ----

    withr::with_tempfile(
      new = "olink_long_format",
      pattern = "test-olink-long",
      fileext = ".csv",
      code = {

        sep <- ";"

        # write wide df
        write.table(x = df_synthetic$list_df_long$df_long,
                    file = olink_long_format,
                    append = FALSE,
                    quote = FALSE,
                    sep = sep,
                    eol = "\n",
                    na = "",
                    dec = ".",
                    row.names = FALSE,
                    col.names = TRUE)

        # check that function runs
        expect_no_condition(
          object = df_read_out <- read_npx_format_read(
            file = olink_long_format,
            sep = sep,
            read_n = top_n_row
          )
        )

        df_exp_r <- df_synthetic$list_df_long$df_long |>
          dplyr::slice_head(n = top_n_row - 1L)
        colnames(df_exp_r) <- paste0("V", seq_len(ncol(df_exp_r)))

        df_exp <- colnames(df_synthetic$list_df_long$df_long) |>
          dplyr::tibble() |>
          t()
        rownames(df_exp) <- NULL
        colnames(df_exp) <- paste0("V", seq_len(ncol(df_exp)))
        df_exp <- df_exp |>
          dplyr::as_tibble() |>
          dplyr::bind_rows(
            df_exp_r
          )

        # check that read_npx_format_top_n works
        expect_identical(
          object = df_read_out$df_top_n,
          expected = df_exp
        )

        expect_identical(
          object = dplyr::as_tibble(df_read_out$df) |>
            dplyr::mutate(
              dplyr::across(
                dplyr::everything(),
                ~ as.character(.x)
              )
            ),
          expected = df_synthetic$list_df_long$df_long
        )

      }
    )

    ## txt - long ----

    withr::with_tempfile(
      new = "olink_long_format",
      pattern = "test-olink-long",
      fileext = ".txt",
      code = {

        sep <- ","

        # write wide df
        write.table(x = df_synthetic$list_df_long$df_long,
                    file = olink_long_format,
                    append = FALSE,
                    quote = FALSE,
                    sep = sep,
                    eol = "\n",
                    na = "",
                    dec = ".",
                    row.names = FALSE,
                    col.names = TRUE)

        # check that function runs
        expect_no_condition(
          object = df_read_out <- read_npx_format_read(
            file = olink_long_format,
            sep = sep,
            read_n = top_n_row
          )
        )

        df_exp_r <- df_synthetic$list_df_long$df_long |>
          dplyr::slice_head(n = top_n_row - 1L)
        colnames(df_exp_r) <- paste0("V", seq_len(ncol(df_exp_r)))

        df_exp <- colnames(df_synthetic$list_df_long$df_long) |>
          dplyr::tibble() |>
          t()
        rownames(df_exp) <- NULL
        colnames(df_exp) <- paste0("V", seq_len(ncol(df_exp)))
        df_exp <- df_exp |>
          dplyr::as_tibble() |>
          dplyr::bind_rows(
            df_exp_r
          )

        # check that read_npx_format_top_n works
        expect_identical(
          object = df_read_out$df_top_n,
          expected = df_exp
        )

        expect_identical(
          object = dplyr::as_tibble(df_read_out$df) |>
            dplyr::mutate(
              dplyr::across(
                dplyr::everything(),
                ~ as.character(.x)
              )
            ),
          expected = df_synthetic$list_df_long$df_long
        )

      }
    )

  }
)

test_that(
  "read_npx_format_read - error - unknown file type",
  {
    skip_on_cran()

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".parquet",
      code = {

        # get wide synthetic data
        df_synthetic <- get_wide_synthetic_data(olink_platform = "Target 48",
                                                data_type = "Quantified",
                                                n_panels = 3L,
                                                n_assays = 45L,
                                                n_samples = 88L,
                                                show_dev_int_ctrl = TRUE,
                                                show_int_ctrl = TRUE,
                                                version = 0L)

        sep <- NULL

        # write wide df
        arrow::write_parquet(
          x = df_synthetic$list_df_long$df_oid_long,
          sink = olink_wide_format,
          compression = "gzip"
        )

        # check that function runs
        expect_error(
          object = read_npx_format_read(
            file = olink_wide_format,
            sep = sep,
            read_n = 3L
          ),
          regexp = "Unable to recognize format from file extension of"
        )

      }
    )
  }
)

# Test read_npx_format_get_format (wide/long) ----

test_that(
  "read_npx_format_get_format - works - wide",
  {
    skip_on_cran()

    withr::with_tempfile(
      new = "excel_wide",
      pattern = "test_excel_npx_wide",
      fileext = ".xlsx",
      code = {

        # get wide synthetic data
        df_synthetic <- get_wide_synthetic_data(olink_platform = "Target 48",
                                                data_type = "NPX",
                                                n_panels = 3L,
                                                n_assays = 45L,
                                                n_samples = 88L,
                                                show_dev_int_ctrl = FALSE,
                                                show_int_ctrl = TRUE,
                                                version = 1L)

        # modify df wide
        df <- df_synthetic$list_df_wide$df_wide |>
          dplyr::slice_head(
            n = 2L
          )

        # write a dummy file
        writeLines("foo", excel_wide)

        #check that file exists
        expect_true(object = file.exists(excel_wide))

        # check that read_npx_format_get_format works with long_format = NULL
        expect_no_condition(
          object = df_npx_null <- read_npx_format_get_format(
            df = df,
            file = excel_wide,
            long_format = NULL
          )
        )

        # check that object exists
        expect_true(object = exists("df_npx_null"))

        # check that it contains the correct elements
        expect_equal(
          object = names(df_npx_null),
          expected = c("is_long_format", "data_cells")
        )

        # check that it is the correct format
        expect_equal(
          object = df_npx_null$is_long_format,
          expected = FALSE
        )

        # check that the output string is correct
        expect_equal(
          object = df_npx_null$data_cells,
          expected = c("NPX")
        )

        # check that read_npx_format_get_format works with long_format = FALSE
        expect_no_condition(
          object = df_npx_false <- read_npx_format_get_format(
            df = df,
            file = excel_wide,
            long_format = FALSE
          )
        )

        # check that object exists
        expect_true(object = exists("df_npx_false"))

        # check that the two runs of read_npx_format_get_format return the same
        expect_identical(
          object = df_npx_false,
          expected = df_npx_null
        )

      }
    )

  }
)

test_that(
  "read_npx_format_get_format - works - long",
  {
    skip_on_cran()

    withr::with_tempfile(
      new = "excel_long",
      pattern = "test_excel_npx_long",
      fileext = ".xlsx",
      code = {

        # get wide synthetic data
        df_synthetic <- get_wide_synthetic_data(olink_platform = "Target 48",
                                                data_type = "NPX",
                                                n_panels = 3L,
                                                n_assays = 45L,
                                                n_samples = 88L,
                                                show_dev_int_ctrl = FALSE,
                                                show_int_ctrl = TRUE,
                                                version = 1L)

        # modify df long
        df_exp_r1 <- df_synthetic$list_df_long$df_long |>
          dplyr::slice_head(
            n = 1L
          )
        colnames(df_exp_r1) <- paste0("V", seq_len(ncol(df_exp_r1)))

        df <- colnames(df_synthetic$list_df_long$df_long) |>
          dplyr::tibble() |>
          t()
        rownames(df) <- NULL
        colnames(df) <- paste0("V", seq_len(ncol(df)))
        df <- df |>
          dplyr::as_tibble() |>
          dplyr::bind_rows(
            df_exp_r1
          )

        # write a dummy file
        writeLines("foo", excel_long)

        #check that file exists
        expect_true(object = file.exists(excel_long))

        # check that read_npx_format_get_format works with long_format = NULL
        expect_no_condition(
          object = df_npx_null <- read_npx_format_get_format(
            df = df,
            file = excel_long,
            long_format = NULL
          )
        )

        # check that object exists
        expect_true(object = exists("df_npx_null"))

        # check that it contains the correct elements
        expect_equal(
          object = names(df_npx_null),
          expected = c("is_long_format", "data_cells")
        )

        # check that it is the correct format
        expect_equal(
          object = df_npx_null$is_long_format,
          expected = TRUE
        )

        # check that the output string is correct
        expect_equal(
          object = df_npx_null$data_cells,
          expected = colnames(df_synthetic$list_df_long$df_long) |>
            (\(.x) {
              .x[!grepl(pattern = "Version",
                        x = .x,
                        ignore.case = TRUE)]
            })()
        )

        # check that read_npx_format_get_format works with long_format = TRUE
        expect_no_condition(
          object = df_npx_true <- read_npx_format_get_format(
            df = df,
            file = excel_long,
            long_format = TRUE
          )
        )

        # check that object exists
        expect_true(object = exists("df_npx_true"))

        # check that the two runs of read_npx_format_get_format return the same
        expect_identical(
          object = df_npx_true,
          expected = df_npx_null
        )

      }
    )

  }
)

test_that(
  "read_npx_format_get_format - warning - diff autodetection vs user input",
  {
    skip_on_cran()

    # get wide synthetic data
    df_synthetic <- get_wide_synthetic_data(olink_platform = "Target 48",
                                            data_type = "NPX",
                                            n_panels = 3L,
                                            n_assays = 45L,
                                            n_samples = 88L,
                                            show_dev_int_ctrl = FALSE,
                                            show_int_ctrl = TRUE,
                                            version = 1L)

    ## long input ----

    withr::with_tempfile(
      new = "excel_long",
      pattern = "test_excel_long",
      fileext = ".xlsx",
      code = {

        # modify df long
        df_exp_r1 <- df_synthetic$list_df_long$df_long |>
          dplyr::slice_head(
            n = 1L
          )
        colnames(df_exp_r1) <- paste0("V", seq_len(ncol(df_exp_r1)))

        df <- colnames(df_synthetic$list_df_long$df_long) |>
          dplyr::tibble() |>
          t()
        rownames(df) <- NULL
        colnames(df) <- paste0("V", seq_len(ncol(df)))
        df <- df |>
          dplyr::as_tibble() |>
          dplyr::bind_rows(
            df_exp_r1
          )

        # write a dummy file
        writeLines("foo", excel_long)

        #check that file exists
        expect_true(object = file.exists(excel_long))

        # read_npx_format_get_format throws warn with long_format = FALSE
        expect_warning(
          object = df_npx_false <- read_npx_format_get_format(
            df = df,
            file = excel_long,
            long_format = FALSE
          ),
          regexp = "Based on `long_format` we were expecting \"wide\" format"
        )

        # check that object exists
        expect_true(object = exists("df_npx_false"))

        # check that it contains the correct elements
        expect_equal(
          object = names(df_npx_false),
          expected = c("is_long_format", "data_cells")
        )

        # check that it is the correct format
        expect_equal(
          object = df_npx_false$is_long_format,
          expected = FALSE
        )

        # check that the output string is conatins data from df_in
        expect_equal(
          object = df_npx_false$data_cells,
          expected = df |>
            dplyr::select(1L) |>
            dplyr::slice(2L) |>
            as.character()
        )

      }
    )

    ## wide input ----

    withr::with_tempfile(
      new = "excel_wide",
      pattern = "test_excel_wide",
      fileext = ".xlsx",
      code = {

        # modify df wide
        df <- df_synthetic$list_df_wide$df_wide |>
          dplyr::slice_head(
            n = 2L
          )

        # write a dummy file
        writeLines("foo", excel_wide)

        #check that file exists
        expect_true(object = file.exists(excel_wide))

        # read_npx_format_get_format throws warn with long_format = TRUE
        expect_error(
          object = expect_warning(
            object = read_npx_format_get_format(
              df = df,
              file = excel_wide,
              long_format = TRUE
            ),
            regexp = "Based on `long_format` we were expecting \"long\" format"
          ),
          regexp = "`NA` column names in long format file"
        )

      }
    )

  }
)

test_that(
  "read_npx_format_get_format - warning - diff autodetection vs user input V2",
  {
    skip_on_cran()

    # get wide synthetic data
    df_synthetic <- get_wide_synthetic_data(olink_platform = "Target 48",
                                            data_type = "NPX",
                                            n_panels = 3L,
                                            n_assays = 45L,
                                            n_samples = 88L,
                                            show_dev_int_ctrl = FALSE,
                                            show_int_ctrl = TRUE,
                                            version = 1L)

    ## long input ----

    withr::with_tempfile(
      new = "excel_long",
      pattern = "test_excel_long",
      fileext = ".xlsx",
      code = {

        # modify df long
        df_exp_r1 <- df_synthetic$list_df_long$df_long |>
          dplyr::slice_head(
            n = 1L
          )
        colnames(df_exp_r1) <- paste0("V", seq_len(ncol(df_exp_r1)))

        df <- colnames(df_synthetic$list_df_long$df_long) |>
          dplyr::tibble() |>
          t()
        df[grepl("NPX", df)] <- "Wrong_Name"
        rownames(df) <- NULL
        colnames(df) <- paste0("V", seq_len(ncol(df)))
        df <- df |>
          dplyr::as_tibble() |>
          dplyr::bind_rows(
            df_exp_r1
          )

        # write a dummy file
        writeLines("foo", excel_long)

        #check that file exists
        expect_true(object = file.exists(excel_long))

        # read_npx_format_get_format throws warn with long_format = FALSE
        expect_warning(
          object = df_npx_false <- read_npx_format_get_format(
            df = df,
            file = excel_long,
            long_format = TRUE
          ),
          regexp = "Unable to confirm the \"long\" format from the input file:"
        )

        # check that object exists
        expect_true(object = exists("df_npx_false"))

        # check that it contains the correct elements
        expect_equal(
          object = names(df_npx_false),
          expected = c("is_long_format", "data_cells")
        )

        # check that it is the correct format
        expect_equal(
          object = df_npx_false$is_long_format,
          expected = TRUE
        )

        # check that the output string is conatins data from df_in
        expect_equal(
          object = df_npx_false$data_cells,
          expected = df |>
            dplyr::slice_head(n = 1L) |>
            as.character() |>
            (\(.x) {
              .x[!grepl(pattern = "Version",
                        x = .x,
                        ignore.case = TRUE)]
            })()
        )

      }
    )

    ## wide input ----

    withr::with_tempfile(
      new = "excel_wide",
      pattern = "test_excel_wide",
      fileext = ".xlsx",
      code = {

        # modify df wide
        df <- df_synthetic$list_df_wide$df_wide |>
          dplyr::slice_head(
            n = 2L
          ) |>
          dplyr::mutate(
            V1 = dplyr::if_else(.data[["V1"]] == "NPX",
                                "Wrong_Name",
                                .data[["V1"]])
          )

        # write a dummy file
        writeLines("foo", excel_wide)

        #check that file exists
        expect_true(object = file.exists(excel_wide))

        # read_npx_format_get_format throws warn with long_format = TRUE
        expect_warning(
          object = df_npx_true <- read_npx_format_get_format(
            df = df,
            file = excel_wide,
            long_format = FALSE
          ),
          regexp = "Unable to confirm the \"wide\" format from the input file:"
        )

        # check that object exists
        expect_true(object = exists("df_npx_true"))

        # check that it contains the correct elements
        expect_equal(
          object = names(df_npx_true),
          expected = c("is_long_format", "data_cells")
        )

        # check that it is the correct format
        expect_equal(
          object = df_npx_true$is_long_format,
          expected = FALSE
        )

        # check that the output string is NULL
        expect_equal(
          object = df_npx_true$data_cells,
          expected = df |>
            dplyr::select(1L) |>
            dplyr::slice(2L) |>
            as.character()
        )

      }
    )

  }
)

test_that(
  "read_npx_format_get_format - error - unable to detect format",
  {
    skip_on_cran()

    # get wide synthetic data
    df_synthetic <- get_wide_synthetic_data(olink_platform = "Target 48",
                                            data_type = "NPX",
                                            n_panels = 3L,
                                            n_assays = 45L,
                                            n_samples = 88L,
                                            show_dev_int_ctrl = FALSE,
                                            show_int_ctrl = TRUE,
                                            version = 1L)

    ## long input ----

    withr::with_tempfile(
      new = "excel_long",
      pattern = "test_excel_long",
      fileext = ".xlsx",
      code = {

        # modify df long
        df_exp_r1 <- df_synthetic$list_df_long$df_long |>
          dplyr::slice_head(
            n = 1L
          )
        colnames(df_exp_r1) <- paste0("V", seq_len(ncol(df_exp_r1)))

        df <- colnames(df_synthetic$list_df_long$df_long) |>
          dplyr::tibble() |>
          t()
        df[grepl("NPX", df)] <- "Wrong_Name"
        rownames(df) <- NULL
        colnames(df) <- paste0("V", seq_len(ncol(df)))
        df <- df |>
          dplyr::as_tibble() |>
          dplyr::bind_rows(
            df_exp_r1
          )

        # write a dummy file
        writeLines("foo", excel_long)

        #check that file exists
        expect_true(object = file.exists(excel_long))

        # read_npx_format_get_format throws warn with long_format = FALSE
        expect_error(
          object = df_npx_false <- read_npx_format_get_format(
            df = df,
            file = excel_long,
            long_format = NULL
          ),
          regexp = "Unable to recognize the format of the input file"
        )

      }
    )

    ## wide input ----

    withr::with_tempfile(
      new = "excel_wide",
      pattern = "test_excel_wide",
      fileext = ".xlsx",
      code = {

        # modify df wide
        df <- df_synthetic$list_df_wide$df_wide |>
          dplyr::slice_head(
            n = 2L
          ) |>
          dplyr::mutate(
            V1 = dplyr::if_else(.data[["V1"]] == "NPX",
                                "Wrong_Name",
                                .data[["V1"]])
          )

        # write a dummy file
        writeLines("foo", excel_wide)

        #check that file exists
        expect_true(object = file.exists(excel_wide))

        # read_npx_format_get_format throws warn with long_format = TRUE
        expect_error(
          object = df_npx_true <- read_npx_format_get_format(
            df = df,
            file = excel_wide,
            long_format = NULL
          ),
          regexp = "Unable to recognize the format of the input file"
        )

      }
    )

  }
)

test_that(
  "read_npx_format_get_format - error - long format with NA colnames",
  {
    skip_on_cran()

    # get wide synthetic data
    df_synthetic <- get_wide_synthetic_data(olink_platform = "Target 48",
                                            data_type = "NPX",
                                            n_panels = 1L,
                                            n_assays = 45L,
                                            n_samples = 88L,
                                            show_dev_int_ctrl = FALSE,
                                            show_int_ctrl = FALSE,
                                            version = 1L)

    ## long input ----

    withr::with_tempfile(
      new = "excel_long",
      pattern = "test_excel_long",
      fileext = ".xlsx",
      code = {

        # modify df long
        df_exp_r2 <- df_synthetic$list_df_long$df_long |>
          dplyr::slice_head(
            n = 2L
          )
        colnames(df_exp_r2) <- paste0("V", seq_len(ncol(df_exp_r2)))

        df_exp_r1 <- dplyr::tibble(
          "A" = colnames(df_synthetic$list_df_long$df_long),
          "B" = colnames(df_exp_r2)
        ) |>
          dplyr::bind_rows(
            dplyr::tibble("A" = NA_character_,
                          "B" = paste0("V", (ncol(df_exp_r2) + 1L)))
          ) |>
          t()
        colnames(df_exp_r1) <- df_exp_r1[2L, ]
        df_exp_r1 <- df_exp_r1 |>
          dplyr::as_tibble() |>
          dplyr::slice_head(n = 1L)

        df <- df_exp_r1 |>
          dplyr::bind_rows(df_exp_r2)

        # write a dummy file
        writeLines("foo", excel_long)

        #check that file exists
        expect_true(object = file.exists(excel_long))

        # read_npx_format_get_format throws warn with long_format = FALSE
        expect_error(
          object = df_npx_false <- read_npx_format_get_format(
            df = df,
            file = excel_long,
            long_format = NULL
          ),
          regexp = "`NA` column names in long format file"
        )

      }
    )

    ## wide input ----

    withr::with_tempfile(
      new = "excel_wide",
      pattern = "test_excel_wide",
      fileext = ".xlsx",
      code = {

        # modify df wide
        df <- df_synthetic$list_df_wide$df_wide |>
          dplyr::slice_head(
            n = 2L
          ) |>
          dplyr::mutate(
            V1 = dplyr::if_else(.data[["V1"]] == "NPX",
                                "Wrong_Name",
                                .data[["V1"]])
          )

        # write a dummy file
        writeLines("foo", excel_wide)

        #check that file exists
        expect_true(object = file.exists(excel_wide))

        # read_npx_format_get_format throws warn with long_format = TRUE
        expect_error(
          object = df_npx_true <- read_npx_format_get_format(
            df = df,
            file = excel_wide,
            long_format = NULL
          ),
          regexp = "Unable to recognize the format of the input file"
        )

      }
    )

  }
)

# Test read_npx_format_get_platform (T96, T48, Flex, Focus) ----

test_that(
  "read_npx_format_get_platform - works",
  {
    skip_on_cran()

    olink_platforms_wide <- accepted_olink_platforms |>
      dplyr::filter(.data[["broader_platform"]] == "qPCR")

    ## Target 96 ----

    withr::with_tempfile(
      new = "excel_t96",
      pattern = "test_excel_t96",
      fileext = ".xlsx",
      code = {

        # get wide synthetic data
        df_synthetic <- get_wide_synthetic_data(olink_platform = "Target 96",
                                                data_type = "NPX",
                                                n_panels = 1L,
                                                n_assays = 92L,
                                                n_samples = 88L,
                                                show_dev_int_ctrl = TRUE,
                                                show_int_ctrl = TRUE,
                                                version = 2L)

        # modify df wide
        df <- df_synthetic$list_df_wide$df_wide |>
          dplyr::slice_head(
            n = 3L
          )

        # write a dummy file
        writeLines("foo", excel_t96)

        #check that file exists
        expect_true(object = file.exists(excel_t96))

        # read_npx_format_get_platform works for olink_platform = NULL
        expect_no_condition(
          object = df_npx_null <- read_npx_format_get_platform(
            df = df,
            file = excel_t96,
            olink_platform = NULL,
            olink_platforms_wide = olink_platforms_wide
          )
        )

        # check that object exists
        expect_true(object = exists("df_npx_null"))

        # check that it contains the correct elements
        expect_equal(
          object = df_npx_null,
          expected = "Target 96"
        )

        # read_npx_format_get_platform works for olink_platform = "T96"
        expect_no_condition(
          object = df_npx_t96 <- read_npx_format_get_platform(
            df = df,
            file = excel_t96,
            olink_platform = "Target 96",
            olink_platforms_wide = olink_platforms_wide
          )
        )

        # check that object exists
        expect_true(object = exists("df_npx_t96"))

        # check that it contains the correct elements
        expect_equal(
          object = df_npx_t96,
          expected = df_npx_null
        )

      }
    )

    ## Target 48 ----

    withr::with_tempfile(
      new = "excel_t48",
      pattern = "test_excel_t48",
      fileext = ".xlsx",
      code = {

        # get wide synthetic data
        df_synthetic <- get_wide_synthetic_data(olink_platform = "Target 48",
                                                data_type = "NPX",
                                                n_panels = 1L,
                                                n_assays = 45L,
                                                n_samples = 88L,
                                                show_dev_int_ctrl = TRUE,
                                                show_int_ctrl = TRUE,
                                                version = 1L)

        # modify df wide
        df <- df_synthetic$list_df_wide$df_wide |>
          dplyr::slice_head(
            n = 3L
          )

        # write a dummy file
        writeLines("foo", excel_t48)

        # read_npx_format_get_platform works for olink_platform = NULL
        expect_no_condition(
          object = df_npx_null <- read_npx_format_get_platform(
            df = df,
            file = excel_t48,
            olink_platform = NULL,
            olink_platforms_wide = olink_platforms_wide
          )
        )

        # check that object exists
        expect_true(object = exists("df_npx_null"))

        # check that it contains the correct elements
        expect_equal(
          object = df_npx_null,
          expected = "Target 48"
        )

        # read_npx_format_get_platform works for olink_platform = "T48"
        expect_no_condition(
          object = df_npx_t48 <- read_npx_format_get_platform(
            df = df,
            file = excel_t48,
            olink_platform = "Target 48",
            olink_platforms_wide = olink_platforms_wide
          )
        )

        # check that object exists
        expect_true(object = exists("df_npx_t48"))

        # check that it contains the correct elements
        expect_equal(
          object = df_npx_t48,
          expected = df_npx_null
        )

      }
    )

    ## Flex v1 ----

    withr::with_tempfile(
      new = "excel_flex",
      pattern = "test_excel_flex",
      fileext = ".xlsx",
      code = {

        # get wide synthetic data
        df_synthetic <- get_wide_synthetic_data(olink_platform = "Flex",
                                                data_type = "NPX",
                                                n_panels = 1L,
                                                n_assays = 20L,
                                                n_samples = 88L,
                                                show_dev_int_ctrl = TRUE,
                                                show_int_ctrl = TRUE,
                                                version = 1L)

        # modify df wide
        df <- df_synthetic$list_df_wide$df_wide |>
          dplyr::slice_head(
            n = 3L
          )

        # write a dummy file
        writeLines("foo", excel_flex)

        #check that file exists
        expect_true(object = file.exists(excel_flex))

        # read_npx_format_get_platform works for olink_platform = NULL
        expect_no_condition(
          object = df_npx_null <- read_npx_format_get_platform(
            df = df,
            file = excel_flex,
            olink_platform = NULL,
            olink_platforms_wide = olink_platforms_wide
          )
        )

        # check that object exists
        expect_true(object = exists("df_npx_null"))

        # check that it contains the correct elements
        expect_equal(
          object = df_npx_null,
          expected = "Flex"
        )

        # read_npx_format_get_platform works for olink_platform = "Flex"
        expect_no_condition(
          object = df_npx_flex <- read_npx_format_get_platform(
            df = df,
            file = excel_flex,
            olink_platform = "Flex",
            olink_platforms_wide = olink_platforms_wide
          )
        )

        # check that object exists
        expect_true(object = exists("df_npx_flex"))

        # check that it contains the correct elements
        expect_equal(
          object = df_npx_flex,
          expected = df_npx_null
        )

      }
    )

    ## Flex v2 ----

    withr::with_tempfile(
      new = "excel_flex",
      pattern = "test_excel_flex",
      fileext = ".xlsx",
      code = {

        # get wide synthetic data
        df_synthetic <- get_wide_synthetic_data(olink_platform = "Flex",
                                                data_type = "NPX",
                                                n_panels = 1L,
                                                n_assays = 20L,
                                                n_samples = 88L,
                                                show_dev_int_ctrl = TRUE,
                                                show_int_ctrl = TRUE,
                                                version = 1L)

        # modify df wide
        df <- df_synthetic$list_df_wide$df_wide |>
          dplyr::slice_head(
            n = 3L
          ) |>
          dplyr::mutate(
            dplyr::across(
              dplyr::everything(),
              ~ sub(pattern = "Flex",
                    replacement = "ABCD-ABCD",
                    x = .x,
                    fixed = TRUE)
            )
          )

        # write a dummy file
        writeLines("foo", excel_flex)

        #check that file exists
        expect_true(object = file.exists(excel_flex))

        # read_npx_format_get_platform works for olink_platform = NULL
        expect_no_condition(
          object = df_npx_null <- read_npx_format_get_platform(
            df = df,
            file = excel_flex,
            olink_platform = NULL,
            olink_platforms_wide = olink_platforms_wide
          )
        )

        # check that object exists
        expect_true(object = exists("df_npx_null"))

        # check that it contains the correct elements
        expect_equal(
          object = df_npx_null,
          expected = "Flex"
        )

        # read_npx_format_get_platform works for olink_platform = "Flex"
        expect_no_condition(
          object = df_npx_flex <- read_npx_format_get_platform(
            df = df,
            file = excel_flex,
            olink_platform = "Flex",
            olink_platforms_wide = olink_platforms_wide
          )
        )

        # check that object exists
        expect_true(object = exists("df_npx_flex"))

        # check that it contains the correct elements
        expect_equal(
          object = df_npx_flex,
          expected = df_npx_null
        )

      }
    )

  }
)

test_that(
  "read_npx_format_get_platform - error - no match",
  {
    skip_on_cran()

    withr::with_tempfile(
      new = "excel_wide",
      pattern = "test_excel_wide",
      fileext = ".xlsx",
      code = {

        # get wide synthetic data
        df_synthetic <- get_wide_synthetic_data(olink_platform = "Target 96",
                                                data_type = "NPX",
                                                n_panels = 1L,
                                                n_assays = 92L,
                                                n_samples = 88L,
                                                show_dev_int_ctrl = TRUE,
                                                show_int_ctrl = TRUE,
                                                version = 2L)

        # modify df wide
        df <- df_synthetic$list_df_wide$df_wide |>
          dplyr::slice_head(
            n = 3L
          ) |>
          dplyr::mutate(
            dplyr::across(
              dplyr::everything(),
              ~ sub(pattern = "Target 96",
                    replacement = "Unknown",
                    x = .x,
                    fixed = TRUE)
            )
          )

        # write a dummy file
        writeLines("foo", excel_wide)

        #check that file exists
        expect_true(object = file.exists(excel_wide))

        # check that read_npx_format_get_platform throws error
        expect_error(
          object = read_npx_format_get_platform(
            df = df,
            file = excel_wide,
            olink_platform = NULL,
            olink_platforms_wide = accepted_olink_platforms |>
              dplyr::filter(.data[["broader_platform"]] == "qPCR")
          ),
          regexp = "Expected one of"
        )

      }
    )

  }
)

test_that(
  "read_npx_format_get_platform - error - multiple matches",
  {
    skip_on_cran()

    withr::with_tempfile(
      new = "excel_wide",
      pattern = "test_excel_wide",
      fileext = ".xlsx",
      code = {

        # get wide synthetic data
        df_synthetic <- get_wide_synthetic_data(olink_platform = "Target 96",
                                                data_type = "NPX",
                                                n_panels = 1L,
                                                n_assays = 92L,
                                                n_samples = 88L,
                                                show_dev_int_ctrl = TRUE,
                                                show_int_ctrl = TRUE,
                                                version = 2L)

        # modify df wide
        df <- df_synthetic$list_df_wide$df_wide |>
          dplyr::slice_head(
            n = 3L
          ) |>
          dplyr::mutate(
            dplyr::across(
              dplyr::everything(),
              ~ sub(pattern = "Target 96",
                    replacement = "Target 96 Flex",
                    x = .x,
                    fixed = TRUE)
            )
          )

        # write a dummy file
        writeLines("foo", excel_wide)

        #check that file exists
        expect_true(object = file.exists(excel_wide))

        # check that read_npx_format_get_platform throws error
        expect_error(
          object = read_npx_format_get_platform(
            df = df,
            file = excel_wide,
            olink_platform = NULL,
            olink_platforms_wide = accepted_olink_platforms |>
              dplyr::filter(.data[["broader_platform"]] == "qPCR")
          ),
          regexp = "Too many matches from"
        )

      }
    )

  }
)

test_that(
  "read_npx_format_get_platform - warning",
  {
    skip_on_cran()

    # get wide synthetic data
    df_synthetic <- get_wide_synthetic_data(olink_platform = "Target 96",
                                            data_type = "NPX",
                                            n_panels = 1L,
                                            n_assays = 92L,
                                            n_samples = 88L,
                                            show_dev_int_ctrl = TRUE,
                                            show_int_ctrl = TRUE,
                                            version = 2L)

    # modify df wide
    df <- df_synthetic$list_df_wide$df_wide |>
      dplyr::slice_head(
        n = 3L
      )

    olink_platforms_wide <- accepted_olink_platforms |>
      dplyr::filter(.data[["broader_platform"]] == "qPCR")

    ## no match ----

    withr::with_tempfile(
      new = "excel_wide",
      pattern = "test_excel_wide",
      fileext = ".xlsx",
      code = {

        # modify df
        df <- df |>
          dplyr::mutate(
            dplyr::across(
              dplyr::everything(),
              ~ sub(pattern = "Target 96",
                    replacement = "Unknown",
                    x = .x,
                    fixed = TRUE)
            )
          )

        # write a dummy file
        writeLines("foo", excel_wide)

        #check that file exists
        expect_true(object = file.exists(excel_wide))

        # check that read_npx_format_get_platform throws error
        expect_warning(
          object = df_npx <- read_npx_format_get_platform(
            df = df,
            file = excel_wide,
            olink_platform = "Target 96",
            olink_platforms_wide = olink_platforms_wide
          ),
          regexp = "Unable to recognize the Olink platform from the input file:"
        )

        # check that object exists
        expect_true(object = exists("df_npx"))

        # check that it contains the correct elements
        expect_equal(
          object = df_npx,
          expected = "Target 96"
        )

      }
    )

    ## multiple matches ----

    withr::with_tempfile(
      new = "excel_wide",
      pattern = "test_excel_wide",
      fileext = ".xlsx",
      code = {

        # modify df
        df <- df |>
          dplyr::mutate(
            dplyr::across(
              dplyr::everything(),
              ~ sub(pattern = "Target 96",
                    replacement = "Target 96 Flex",
                    x = .x,
                    fixed = TRUE)
            )
          )

        # write a dummy file
        writeLines("foo", excel_wide)

        #check that file exists
        expect_true(object = file.exists(excel_wide))

        # check that read_npx_format_get_platform throws error
        expect_warning(
          object = df_npx <- read_npx_format_get_platform(
            df = df,
            file = excel_wide,
            olink_platform = "Target 96",
            olink_platforms_wide = olink_platforms_wide
          ),
          regexp = "Unable to recognize the Olink platform from the input file:"
        )

        # check that object exists
        expect_true(object = exists("df_npx"))

        # check that it contains the correct elements
        expect_equal(
          object = df_npx,
          expected = "Target 96"
        )

      }
    )

    ## difference in matches ----

    withr::with_tempfile(
      new = "excel_wide",
      pattern = "test_excel_wide",
      fileext = ".xlsx",
      code = {

        # write a dummy file
        writeLines("foo", excel_wide)

        #check that file exists
        expect_true(object = file.exists(excel_wide))

        # check that read_npx_format_get_platform throws error
        expect_warning(
          object = df_npx <- read_npx_format_get_platform(
            df = df,
            file = excel_wide,
            olink_platform = "Target 48",
            olink_platforms_wide = olink_platforms_wide
          ),
          regexp = "Based on `olink_platform` we were expecting Olink"
        )

        # check that object exists
        expect_true(object = exists("df_npx"))

        # check that it contains the correct elements
        expect_equal(
          object = df_npx,
          expected = "Target 48"
        )

      }
    )

  }
)

# Test read_npx_format_get_quant (NPX, Ct, Quantified) ----

test_that(
  "read_npx_format_get_quant - works",
  {
    skip_on_cran()

    ## NPX ----

    withr::with_tempfile(
      new = "excel_wide",
      pattern = "test_excel_npx_wide",
      fileext = ".xslx",
      code = {

        # writing something to the file
        writeLines("foo", excel_wide)

        # check that file exists
        expect_true(object = file.exists(excel_wide))

        # check that read_npx_format_get_quant works for data_type = NULL
        expect_no_condition(
          object = df_npx_null <- read_npx_format_get_quant(
            file = excel_wide,
            data_type = NULL,
            data_cells = "NPX",
            quant_methods_expected = c("NPX", "Ct", "Quantified")
          )
        )

        # check that object exists
        expect_true(object = exists("df_npx_null"))

        # check that it contains the correct quantification method
        expect_equal(
          object = df_npx_null,
          expected = "NPX"
        )

        # check that read_npx_format_get_quant works for data_type = NPX
        expect_no_condition(
          object = df_npx_npx <- read_npx_format_get_quant(
            file = excel_wide,
            data_type = "NPX",
            data_cells = "NPX",
            quant_methods_expected = c("NPX", "Ct", "Quantified")
          )
        )

        # check that object exists
        expect_true(object = exists("df_npx_npx"))

        # check that it contains the correct quantification method
        expect_equal(
          object = df_npx_npx,
          expected = df_npx_null
        )

      }
    )

    ## Ct ----

    withr::with_tempfile(
      new = "excel_wide",
      pattern = "test_excel_ct_wide",
      fileext = ".xslx",
      code = {

        # writing something to the file
        writeLines("foo", excel_wide)

        # check that file exists
        expect_true(object = file.exists(excel_wide))

        # check that read_npx_format_get_quant works for data_type = NULL
        expect_no_condition(
          object = df_npx_null <- read_npx_format_get_quant(
            file = excel_wide,
            data_type = NULL,
            data_cells = "Ct",
            quant_methods_expected = c("NPX", "Ct", "Quantified")
          )
        )

        # check that object exists
        expect_true(object = exists("df_npx_null"))

        # check that it contains the correct quantification method
        expect_equal(
          object = df_npx_null,
          expected = "Ct"
        )

        # check that read_npx_format_get_quant works for data_type = Ct
        expect_no_condition(
          object = df_npx_ct <- read_npx_format_get_quant(
            file = excel_wide,
            data_type = "Ct",
            data_cells = "Ct",
            quant_methods_expected = c("NPX", "Ct", "Quantified")
          )
        )

        # check that object exists
        expect_true(object = exists("df_npx_ct"))

        # check that it contains the correct quantification method
        expect_equal(
          object = df_npx_ct,
          expected = df_npx_null
        )

      }
    )

    ## Quantified ----

    withr::with_tempfile(
      new = "excel_wide",
      pattern = "test_excel_quantified_wide",
      fileext = ".xslx",
      code = {

        # writing something to the file
        writeLines("foo", excel_wide)

        # check that file exists
        expect_true(object = file.exists(excel_wide))

        # check that read_npx_format_get_quant works for data_type = NULL
        expect_no_condition(
          object = df_npx_null <- read_npx_format_get_quant(
            file = excel_wide,
            data_type = NULL,
            data_cells = "Quantified",
            quant_methods_expected = c("NPX", "Ct", "Quantified")
          )
        )

        # check that object exists
        expect_true(object = exists("df_npx_null"))

        # check that it contains the correct quantification method
        expect_equal(
          object = df_npx_null,
          expected = "Quantified"
        )

        # check that read_npx_format_get_quant works for data_type = Quantified
        expect_no_condition(
          object = df_npx_quant <- read_npx_format_get_quant(
            file = excel_wide,
            data_type = "Quantified",
            data_cells = "Quantified",
            quant_methods_expected = c("NPX", "Ct", "Quantified")
          )
        )

        # check that object exists
        expect_true(object = exists("df_npx_quant"))

        # check that it contains the correct quantification method
        expect_equal(
          object = df_npx_quant,
          expected = df_npx_null
        )

      }
    )

  }
)

test_that(
  "read_npx_format_get_quant - warning - difference in detection",
  {
    skip_on_cran()

    withr::with_tempfile(
      new = "excel_wide",
      pattern = "test_excel_wide",
      fileext = ".xlsx",
      code = {

        # writing something to the file
        writeLines("foo", excel_wide)

        # check that file exists
        expect_true(object = file.exists(excel_wide))

        # check that read_npx_format_get_quant works for data_type = Ct
        expect_warning(
          object = df_npx <- read_npx_format_get_quant(
            file = excel_wide,
            data_type = "Ct",
            data_cells = "NPX",
            quant_methods_expected = c("NPX", "Ct", "Quantified")
          ),
          regexp = "Based on `data_type` we were expecting \"Ct\" format data"
        )

        # check that object exists
        expect_true(object = exists("df_npx"))

        # check that it contains the correct elements
        expect_equal(
          object = df_npx,
          expected = "Ct"
        )

      }
    )

  }
)

test_that(
  "read_npx_format_get_quant - warning - no quant method match",
  {
    skip_on_cran()

    withr::with_tempfile(
      new = "excel_wide",
      pattern = "test_excel_wide",
      fileext = ".xlsx",
      code = {

        # writing something to the file
        writeLines("foo", excel_wide)

        # check that file exists
        expect_true(object = file.exists(excel_wide))

        # check that read_npx_format_get_quant works for data_type = NULL
        expect_error(
          object = read_npx_format_get_quant(
            file = excel_wide,
            data_type = NULL,
            data_cells = "Wrong_Name",
            quant_methods_expected = c("NPX", "Ct", "Quantified")
          ),
          regexp = "Expected one of:"
        )

        # check that read_npx_format_get_quant works for data_type = NPX
        expect_warning(
          object = df_npx <- read_npx_format_get_quant(
            file = excel_wide,
            data_type = "NPX",
            data_cells = "Wrong_Name",
            quant_methods_expected = c("NPX", "Ct", "Quantified")
          ),
          regexp = "Unable to recognize the quantification method from the inpu"
        )

        # check that object exists
        expect_true(object = exists("df_npx"))

        # check that it contains the correct elements
        expect_equal(
          object = df_npx,
          expected = "NPX"
        )

      }
    )

  }
)

test_that(
  "read_npx_format_get_quant - warning - multiple quant method matches",
  {
    skip_on_cran()

    withr::with_tempfile(
      new = "excel_wide",
      pattern = "test_excel_wide",
      fileext = ".xlsx",
      code = {

        # writing something to the file
        writeLines("foo", excel_wide)

        # check that file exists
        expect_true(object = file.exists(excel_wide))

        # check that read_npx_format_get_quant works for data_type = "NPX"
        expect_warning(
          object = df_npx <- read_npx_format_get_quant(
            file = excel_wide,
            data_type = "NPX",
            data_cells = "NPX_Ct",
            quant_methods_expected = c("NPX", "Ct", "Quantified")
          ),
          regexp = "Unable to recognize the quantification method from the inpu"
        )

        # check that object exists
        expect_true(object = exists("df_npx"))

        # check that it contains NULL
        expect_equal(
          object = df_npx,
          expected = "NPX"
        )

      }
    )

  }
)

test_that(
  "read_npx_format_get_quant - error - no quant method identified",
  {
    skip_on_cran()

    withr::with_tempfile(
      new = "excel_wide",
      pattern = "test_excel_wide",
      fileext = ".xlsx",
      code = {

        # writing something to the file
        writeLines("foo", excel_wide)

        # check that file exists
        expect_true(object = file.exists(excel_wide))

        # check that read_npx_format_get_quant works for data_type = NULL
        expect_error(
          object = read_npx_format_get_quant(
            file = excel_wide,
            data_type = NULL,
            data_cells = "NPX_Ct",
            quant_methods_expected = c("NPX", "Ct", "Quantified")
          ),
          regexp = "Too many occurrences of:"
        )

      }
    )

  }
)
