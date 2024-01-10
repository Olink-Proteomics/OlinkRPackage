# Test read_npx_wide_split_row ----

test_that(
  "read_npx_wide_split_row - works",
  {
    ## NPX ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_t <- "NPX"

        df_rand <- olink_wide_synthetic(olink_platform = "Target 48",
                                        data_type = data_t,
                                        n_panels = 2L,
                                        n_assays = 45L,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write wide df
        writexl::write_xlsx(
          x = df_rand$wide$df,
          path = wide_excel,
          col_names = FALSE,
          format_headers = FALSE
        )

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_split_row(file = wide_excel,
                                                     data_type = data_t,
                                                     format_spec = format_s)
        )

        # check that df_top works
        expect_identical(
          object = df_out$df_top,
          expected = df_rand$wide$df_top
        )

        # check that df_mid works
        expect_identical(
          object = df_out$df_mid,
          expected = df_rand$wide$df_middle
        )

        # check that df_bottom works
        expect_identical(
          object = remove_all_na_cols(df = df_out$df_bottom),
          expected = df_rand$wide$df_bottom
        )

      }
    )

    ## Ct ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_t <- "Ct"

        df_rand <- olink_wide_synthetic(olink_platform = "Target 48",
                                        data_type = data_t,
                                        n_panels = 2L,
                                        n_assays = 45L,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write wide df
        writexl::write_xlsx(
          x = df_rand$wide$df,
          path = wide_excel,
          col_names = FALSE,
          format_headers = FALSE
        )

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_split_row(file = wide_excel,
                                                     data_type = data_t,
                                                     format_spec = format_s)
        )

        # check that df_top works
        expect_identical(
          object = df_out$df_top,
          expected = df_rand$wide$df_top
        )

        # check that df_mid works
        expect_identical(
          object = df_out$df_mid,
          expected = df_rand$wide$df_middle
        )

        # check that df_bottom works
        expect_true(object = is.null(df_out$df_bottom))

      }
    )

    ## Quantified wo int ctrl ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_t <- "Quantified"

        df_rand <- olink_wide_synthetic(olink_platform = "Target 48",
                                        data_type = data_t,
                                        n_panels = 2L,
                                        n_assays = 45L,
                                        n_samples = 88L,
                                        show_int_ctrl = FALSE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write wide df
        writexl::write_xlsx(
          x = df_rand$wide$df,
          path = wide_excel,
          col_names = FALSE,
          format_headers = FALSE
        )

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_split_row(file = wide_excel,
                                                     data_type = data_t,
                                                     format_spec = format_s)
        )

        # check that df_top works
        expect_identical(
          object = df_out$df_top,
          expected = df_rand$wide$df_top
        )

        # check that df_mid works
        expect_identical(
          object = df_out$df_mid,
          expected = df_rand$wide$df_middle
        )

        # check that df_bottom works
        expect_identical(
          object = remove_all_na_cols(df = df_out$df_bottom),
          expected = df_rand$wide$df_bottom
        )

      }
    )

    ## Quantified w int ctrl ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_t <- "Quantified"

        df_rand <- olink_wide_synthetic(olink_platform = "Target 48",
                                        data_type = data_t,
                                        n_panels = 2L,
                                        n_assays = 45L,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write wide df
        writexl::write_xlsx(
          x = df_rand$wide$df,
          path = wide_excel,
          col_names = FALSE,
          format_headers = FALSE
        )

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_split_row(file = wide_excel,
                                                     data_type = data_t,
                                                     format_spec = format_s)
        )

        # check that df_top works
        expect_identical(
          object = df_out$df_top,
          expected = df_rand$wide$df_top
        )

        # check that df_mid works
        expect_identical(
          object = df_out$df_mid,
          expected = df_rand$wide$df_middle
        )

        # check that df_bottom works
        expect_identical(
          object = remove_all_na_cols(df = df_out$df_bottom),
          expected = df_rand$wide$df_bottom
        )

      }
    )

    ## Quantified w int ctrl in V2 ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_t <- "Quantified"

        df_rand <- olink_wide_synthetic(olink_platform = "Target 48",
                                        data_type = data_t,
                                        n_panels = 2L,
                                        n_assays = 45L,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V2",
                                        shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write wide df
        writexl::write_xlsx(
          x = df_rand$wide$df,
          path = wide_excel,
          col_names = FALSE,
          format_headers = FALSE
        )

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_split_row(file = wide_excel,
                                                     data_type = data_t,
                                                     format_spec = format_s)
        )

        # check that df_top works
        expect_identical(
          object = df_out$df_top,
          expected = df_rand$wide$df_top
        )

        # check that df_mid works
        expect_identical(
          object = df_out$df_mid,
          expected = df_rand$wide$df_middle
        )

        # check that df_bottom works
        expect_identical(
          object = remove_all_na_cols(df = df_out$df_bottom),
          expected = df_rand$wide$df_bottom
        )

      }
    )

    ## Quantified w int ctrl in V2 shuffled ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_t <- "Quantified"

        df_rand <- olink_wide_synthetic(olink_platform = "Target 48",
                                        data_type = data_t,
                                        n_panels = 2L,
                                        n_assays = 45L,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V2",
                                        shuffle_assays = TRUE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write wide df
        writexl::write_xlsx(
          x = df_rand$wide$df,
          path = wide_excel,
          col_names = FALSE,
          format_headers = FALSE
        )

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_split_row(file = wide_excel,
                                                     data_type = data_t,
                                                     format_spec = format_s)
        )

        # check that df_top works
        expect_identical(
          object = df_out$df_top,
          expected = df_rand$wide$df_top
        )

        # check that df_mid works
        expect_identical(
          object = df_out$df_mid,
          expected = df_rand$wide$df_middle
        )

        # check that df_bottom works
        expect_identical(
          object = remove_all_na_cols(df = df_out$df_bottom),
          expected = df_rand$wide$df_bottom
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_split_row - error - no or too many all-NA rows",
  {
    ## No all-NA rows ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_t <- "NPX"

        df_rand <- olink_wide_synthetic(olink_platform = "Target 48",
                                        data_type = data_t,
                                        n_panels = 2L,
                                        n_assays = 45L,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        # modify and write wide df
        df_rand$wide$df |>
          dplyr::filter(
            is.na(.data[["V1"]])
          ) |>
          writexl::write_xlsx(
            path = wide_excel,
            col_names = FALSE,
            format_headers = FALSE
          )

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # check that function runs with error
        expect_error(
          object = read_npx_wide_split_row(file = wide_excel,
                                           data_type = data_t,
                                           format_spec = format_s),
          regexp = "We identified 0 rows with all columns `NA` in file"
        )
      }
    )

    ## Too many all-NA rows ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_t <- "NPX"

        df_rand <- olink_wide_synthetic(olink_platform = "Target 48",
                                        data_type = data_t,
                                        n_panels = 2L,
                                        n_assays = 45L,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        # modify and write wide df
        df_rand$wide$df |>
          dplyr::mutate(
            V1 = dplyr::if_else(.data[["V1"]] == .env[["data_t"]],
                                NA_character_,
                                .data[["V1"]])
          ) |>
          writexl::write_xlsx(
            path = wide_excel,
            col_names = FALSE,
            format_headers = FALSE
          )

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # check that function runs with error
        expect_error(
          object = read_npx_wide_split_row(file = wide_excel,
                                           data_type = data_t,
                                           format_spec = format_s),
          regexp = "We identified 3 rows with all columns `NA` in file"
        )
      }
    )

  }
)

test_that(
  "read_npx_wide_split_row - error - not as many all-NA rows as expected",
  {
    ## NPX ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_t <- "NPX"

        df_rand <- olink_wide_synthetic(olink_platform = "Target 48",
                                        data_type = "Ct",
                                        n_panels = 2L,
                                        n_assays = 45L,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write wide df
        writexl::write_xlsx(
          x = df_rand$wide$df,
          path = wide_excel,
          col_names = FALSE,
          format_headers = FALSE
        )

        # check that function runs with error
        expect_error(
          object = read_npx_wide_split_row(file = wide_excel,
                                           data_type = data_t,
                                           format_spec = format_s),
          regexp = "while we expected 2"
        )

      }
    )

    ## Ct ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_t <- "Ct"

        df_rand <- olink_wide_synthetic(olink_platform = "Target 48",
                                        data_type = "NPX",
                                        n_panels = 2L,
                                        n_assays = 45L,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write wide df
        writexl::write_xlsx(
          x = df_rand$wide$df,
          path = wide_excel,
          col_names = FALSE,
          format_headers = FALSE
        )

        # check that function runs with error
        expect_error(
          object = read_npx_wide_split_row(file = wide_excel,
                                           data_type = data_t,
                                           format_spec = format_s),
          regexp = "while we expected 1"
        )

      }
    )

    ## Quantified ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_t <- "Quantified"

        df_rand <- olink_wide_synthetic(olink_platform = "Target 48",
                                        data_type = "Ct",
                                        n_panels = 2L,
                                        n_assays = 45L,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write wide df
        writexl::write_xlsx(
          x = df_rand$wide$df,
          path = wide_excel,
          col_names = FALSE,
          format_headers = FALSE
        )

        # check that function runs with error
        expect_error(
          object = read_npx_wide_split_row(file = wide_excel,
                                           data_type = data_t,
                                           format_spec = format_s),
          regexp = "while we expected 2"
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_split_row - error - all-NA rows are consecutive",
  {
    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_t <- "NPX"

        df_rand <- olink_wide_synthetic(olink_platform = "Target 48",
                                        data_type = data_t,
                                        n_panels = 2L,
                                        n_assays = 45L,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)


        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        df_rand$wide$df |>
          dplyr::filter(
            !grepl(pattern = "^S", x = .data[["V1"]])
          ) |>
          writexl::write_xlsx(
            path = wide_excel,
            col_names = FALSE,
            format_headers = FALSE
          )

        # check that function runs with error
        expect_error(
          object = read_npx_wide_split_row(file = wide_excel,
                                           data_type = data_t,
                                           format_spec = format_s),
          regexp = "Consecutive rows with all columns NA."
        )

      }
    )

  }
)

# Test read_npx_wide_check_top ----

test_that(
  "read_npx_wide_check_top - works",
  {
    ## NPX ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_t <- "NPX"

        df_rand <- olink_wide_synthetic(olink_platform = "Target 48",
                                        data_type = data_t,
                                        n_panels = 2L,
                                        n_assays = 45L,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write empty-ish file
        writeLines("foo", wide_excel)

        # check that function runs
        expect_no_condition(
          object = read_npx_wide_check_top(df = df_rand$wide$df_top,
                                           file = wide_excel,
                                           format_spec = format_s)
        )

      }
    )

    ## Ct ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_t <- "Ct"

        df_rand <- olink_wide_synthetic(olink_platform = "Target 48",
                                        data_type = data_t,
                                        n_panels = 2L,
                                        n_assays = 45L,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write empty-ish file
        writeLines("foo", wide_excel)

        # check that function runs
        expect_no_condition(
          object = read_npx_wide_check_top(df = df_rand$wide$df_top,
                                           file = wide_excel,
                                           format_spec = format_s)
        )

      }
    )

    ## Quantified wo int ctrl ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_t <- "Quantified"

        df_rand <- olink_wide_synthetic(olink_platform = "Target 48",
                                        data_type = data_t,
                                        n_panels = 2L,
                                        n_assays = 45L,
                                        n_samples = 88L,
                                        show_int_ctrl = FALSE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write empty-ish file
        writeLines("foo", wide_excel)

        # check that function runs
        expect_no_condition(
          object = read_npx_wide_check_top(df = df_rand$wide$df_top,
                                           file = wide_excel,
                                           format_spec = format_s)
        )

      }
    )

    ## Quantified w int ctrl ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_t <- "Quantified"

        df_rand <- olink_wide_synthetic(olink_platform = "Target 48",
                                        data_type = data_t,
                                        n_panels = 2L,
                                        n_assays = 45L,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write empty-ish file
        writeLines("foo", wide_excel)

        # check that function runs
        expect_no_condition(
          object = read_npx_wide_check_top(df = df_rand$wide$df_top,
                                           file = wide_excel,
                                           format_spec = format_s)
        )

      }
    )

    ## Quantified w int ctrl shuffled ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_t <- "Quantified"

        df_rand <- olink_wide_synthetic(olink_platform = "Target 48",
                                        data_type = data_t,
                                        n_panels = 2L,
                                        n_assays = 45L,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = TRUE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write empty-ish file
        writeLines("foo", wide_excel)

        # check that function runs
        expect_no_condition(
          object = read_npx_wide_check_top(df = df_rand$wide$df_top,
                                           file = wide_excel,
                                           format_spec = format_s)
        )

      }
    )

    ## Quantified w int ctrl in V2 ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_t <- "Quantified"

        df_rand <- olink_wide_synthetic(olink_platform = "Target 48",
                                        data_type = data_t,
                                        n_panels = 2L,
                                        n_assays = 45L,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V2",
                                        shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write empty-ish file
        writeLines("foo", wide_excel)

        # check that function runs
        expect_no_condition(
          object = read_npx_wide_check_top(df = df_rand$wide$df_top,
                                           file = wide_excel,
                                           format_spec = format_s)
        )

      }
    )

    ## Quantified w int ctrl shuffled and in V2 ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_t <- "Quantified"

        df_rand <- olink_wide_synthetic(olink_platform = "Target 48",
                                        data_type = data_t,
                                        n_panels = 2L,
                                        n_assays = 45L,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V2",
                                        shuffle_assays = TRUE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write empty-ish file
        writeLines("foo", wide_excel)

        # check that function runs
        expect_no_condition(
          object = read_npx_wide_check_top(df = df_rand$wide$df_top,
                                           file = wide_excel,
                                           format_spec = format_s)
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_check_top - error - missing labels in V1",
  {
    ## NPX or Ct ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_t <- "NPX"

        df_rand <- olink_wide_synthetic(olink_platform = "Target 48",
                                        data_type = data_t,
                                        n_panels = 2L,
                                        n_assays = 45L,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        # modify df_top
        df_rand$wide$df_top <- df_rand$wide$df_top |>
          dplyr::filter(
            .data[["V1"]] != "OlinkID"
          )

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write empty-ish file
        writeLines("foo", wide_excel)

        # check that function runs with error
        expect_error(
          object = read_npx_wide_check_top(df = df_rand$wide$df_top,
                                           file = wide_excel,
                                           format_spec = format_s),
          regexp = "Column 1 of of the top matrix with assay metadata in file"
        )

      }
    )

    ## Quantified ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_t <- "Quantified"
        n_panel <- 2L
        n_assay <- 45L

        df_rand <- olink_wide_synthetic(olink_platform = "Target 48",
                                        data_type = data_t,
                                        n_panels = n_panel,
                                        n_assays = n_assay,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        # modify df_top
        df_top_add_row <- dplyr::tibble(
          "X" = c("Extra_Row",
                  rep(x = "A", times = n_panel * n_assay),
                  rep(x = NA_character_, times = n_panel),
                  rep(x = NA_character_, times = n_panel),
                  rep(x = NA_character_, times = 3L * n_panel))
        ) |>
          t()
        rownames(df_top_add_row) <- NULL
        colnames(df_top_add_row) <- paste0("V",
                                           seq_len(ncol(df_rand$wide$df_top)))
        df_top_add_row <- dplyr::as_tibble(df_top_add_row)

        df_rand$wide$df_top <- df_rand$wide$df_top |>
          dplyr::bind_rows(
            df_top_add_row
          )

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write empty-ish file
        writeLines("foo", wide_excel)

        # check that function runs with error
        expect_error(
          object = read_npx_wide_check_top(df = df_rand$wide$df_top,
                                           file = wide_excel,
                                           format_spec = format_s),
          regexp = "Column 1 of of the top matrix with assay metadata in file"
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_check_top - error - missing labels in rows 2 & 3",
  {
    ## NPX ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_t <- "NPX"
        n_panel <- 2L
        n_assay <- 45L

        df_rand <- olink_wide_synthetic(olink_platform = "Target 48",
                                        data_type = data_t,
                                        n_panels = n_panel,
                                        n_assays = n_assay,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # remove some columns to reproduce error
        keep_cols <- paste0("V", seq_len((n_panel * n_assay) + 1L + n_panel))

        df_rand$wide$df_top <- df_rand$wide$df_top |>
          dplyr::select(
            dplyr::all_of(keep_cols)
          )

        # write empty-ish file
        writeLines("foo", wide_excel)

        # check that function runs with error
        expect_error(
          object = read_npx_wide_check_top(df = df_rand$wide$df_top,
                                           file = wide_excel,
                                           format_spec = format_s),
          regexp = "Columns 2 and 3 of of the top matrix with assay metadata in"
        )

      }
    )

    ## Ct ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_t <- "Ct"
        n_panel <- 2L
        n_assay <- 45L

        df_rand <- olink_wide_synthetic(olink_platform = "Target 48",
                                        data_type = data_t,
                                        n_panels = n_panel,
                                        n_assays = n_assay,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # remove some columns to reproduce error
        keep_cols <- paste0("V", seq_len((n_panel * n_assay) + 1L))

        df_rand$wide$df_top <- df_rand$wide$df_top |>
          dplyr::select(
            dplyr::all_of(keep_cols)
          )

        # write empty-ish file
        writeLines("foo", wide_excel)

        # check that function runs with error
        expect_error(
          object = read_npx_wide_check_top(df = df_rand$wide$df_top,
                                           file = wide_excel,
                                           format_spec = format_s),
          regexp = "Columns 2 and 3 of of the top matrix with assay metadata in"
        )

      }
    )

    ## Quantified ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_t <- "Quantified"
        n_panel <- 2L
        n_assay <- 45L

        df_rand <- olink_wide_synthetic(olink_platform = "Target 48",
                                        data_type = data_t,
                                        n_panels = n_panel,
                                        n_assays = n_assay,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # remove some columns to reproduce error
        keep_cols <- paste0("V", seq_len((n_panel * n_assay) + 1L + n_panel))

        df_rand$wide$df_top <- df_rand$wide$df_top |>
          dplyr::select(
            dplyr::all_of(keep_cols)
          )

        # write empty-ish file
        writeLines("foo", wide_excel)

        # check that function runs with error
        expect_error(
          object = read_npx_wide_check_top(df = df_rand$wide$df_top,
                                           file = wide_excel,
                                           format_spec = format_s),
          regexp = "Columns 2 and 3 of of the top matrix with assay metadata in"
        )

      }
    )

  }
)

# Test read_npx_wide_top ----

test_that(
  "read_npx_wide_top - works - T48 - single panel",
  {
    ## NPX ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        o_platform <- "Target 48"
        data_t <- "NPX"

        df_rand <- olink_wide_synthetic(olink_platform = o_platform,
                                        data_type = data_t,
                                        n_panels = 1L,
                                        n_assays = 45L,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write empty-ish file
        writeLines("foo", wide_excel)

        # check that function runs
        expect_no_condition(
          object = l_obj <- read_npx_wide_top(df = df_rand$wide$df_top,
                                              file = wide_excel,
                                              olink_platform = o_platform,
                                              format_spec = format_s)
        )

        # check that output match
        expect_identical(
          object = l_obj$df_oid,
          expected = df_rand$long$df_top$df_oid
        )

        expect_identical(
          object = l_obj$df_plate,
          expected = df_rand$long$df_top$df_plate
        )

        expect_identical(
          object = l_obj$df_qc_warn,
          expected = df_rand$long$df_top$df_qc_warn
        )

        expect_false(
          object = "df_int_ctrl" %in% names(l_obj)
        )

      }
    )

    ## Ct ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        o_platform <- "Target 48"
        data_t <- "Ct"

        df_rand <- olink_wide_synthetic(olink_platform = o_platform,
                                        data_type = data_t,
                                        n_panels = 1L,
                                        n_assays = 45L,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write empty-ish file
        writeLines("foo", wide_excel)

        # check that function runs
        expect_no_condition(
          object = l_obj <- read_npx_wide_top(df = df_rand$wide$df_top,
                                              file = wide_excel,
                                              olink_platform = o_platform,
                                              format_spec = format_s)
        )

        # check that output match
        expect_identical(
          object = l_obj$df_oid,
          expected = df_rand$long$df_top$df_oid
        )

        expect_identical(
          object = l_obj$df_plate,
          expected = df_rand$long$df_top$df_plate
        )

        expect_false(
          object = "df_qc_warn" %in% names(l_obj)
        )

        expect_false(
          object = "df_int_ctrl" %in% names(l_obj)
        )

      }
    )

    ## Quantified wo int ctrl ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        o_platform <- "Target 48"
        data_t <- "Quantified"

        df_rand <- olink_wide_synthetic(olink_platform = o_platform,
                                        data_type = data_t,
                                        n_panels = 1L,
                                        n_assays = 45L,
                                        n_samples = 88L,
                                        show_int_ctrl = FALSE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write empty-ish file
        writeLines("foo", wide_excel)

        # check that function runs
        expect_no_condition(
          object = l_obj <- read_npx_wide_top(df = df_rand$wide$df_top,
                                              file = wide_excel,
                                              olink_platform = o_platform,
                                              format_spec = format_s)
        )

        # check that output match
        expect_identical(
          object = l_obj$df_oid,
          expected = df_rand$long$df_top$df_oid
        )

        expect_identical(
          object = l_obj$df_plate,
          expected = df_rand$long$df_top$df_plate
        )

        expect_identical(
          object = l_obj$df_qc_warn,
          expected = df_rand$long$df_top$df_qc_warn
        )

        expect_false(
          object = "df_int_ctrl" %in% names(l_obj)
        )

      }
    )

    ## Quantified w int ctrl ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        o_platform <- "Target 48"
        data_t <- "Quantified"

        df_rand <- olink_wide_synthetic(olink_platform = o_platform,
                                        data_type = data_t,
                                        n_panels = 1L,
                                        n_assays = 45L,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write empty-ish file
        writeLines("foo", wide_excel)

        # check that function runs
        expect_no_condition(
          object = l_obj <- read_npx_wide_top(df = df_rand$wide$df_top,
                                              file = wide_excel,
                                              olink_platform = o_platform,
                                              format_spec = format_s)
        )

        # check that output match
        expect_identical(
          object = l_obj$df_oid,
          expected = df_rand$long$df_top$df_oid
        )

        expect_identical(
          object = l_obj$df_plate,
          expected = df_rand$long$df_top$df_plate
        )

        expect_identical(
          object = l_obj$df_qc_warn,
          expected = df_rand$long$df_top$df_qc_warn
        )

        expect_identical(
          object = l_obj$df_int_ctrl,
          expected = df_rand$long$df_top$df_int_ctrl
        )

      }
    )

    ## Quantified w int ctrl shuffled ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        o_platform <- "Target 48"
        data_t <- "Quantified"

        df_rand <- olink_wide_synthetic(olink_platform = o_platform,
                                        data_type = data_t,
                                        n_panels = 1L,
                                        n_assays = 45L,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = TRUE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write empty-ish file
        writeLines("foo", wide_excel)

        # check that function runs
        expect_no_condition(
          object = l_obj <- read_npx_wide_top(df = df_rand$wide$df_top,
                                              file = wide_excel,
                                              olink_platform = o_platform,
                                              format_spec = format_s)
        )

        # check that output match
        expect_identical(
          object = l_obj$df_oid,
          expected = df_rand$long$df_top$df_oid
        )

        expect_identical(
          object = l_obj$df_plate,
          expected = df_rand$long$df_top$df_plate
        )

        expect_identical(
          object = l_obj$df_qc_warn,
          expected = df_rand$long$df_top$df_qc_warn
        )

        expect_identical(
          object = l_obj$df_int_ctrl,
          expected = df_rand$long$df_top$df_int_ctrl
        )

      }
    )

    ## Quantified w int ctrl shuffled in V2 ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        o_platform <- "Target 48"
        data_t <- "Quantified"

        df_rand <- olink_wide_synthetic(olink_platform = o_platform,
                                        data_type = data_t,
                                        n_panels = 1L,
                                        n_assays = 45L,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V2",
                                        shuffle_assays = TRUE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write empty-ish file
        writeLines("foo", wide_excel)

        # check that function runs
        expect_no_condition(
          object = l_obj <- read_npx_wide_top(df = df_rand$wide$df_top,
                                              file = wide_excel,
                                              olink_platform = o_platform,
                                              format_spec = format_s)
        )

        # check that output match
        expect_identical(
          object = l_obj$df_oid,
          expected = df_rand$long$df_top$df_oid
        )

        expect_identical(
          object = l_obj$df_plate,
          expected = df_rand$long$df_top$df_plate
        )

        expect_identical(
          object = l_obj$df_qc_warn,
          expected = df_rand$long$df_top$df_qc_warn
        )

        expect_identical(
          object = l_obj$df_int_ctrl,
          expected = df_rand$long$df_top$df_int_ctrl
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_top - works - T48 - multiple panels",
  {
    ## NPX ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        o_platform <- "Target 48"
        data_t <- "NPX"

        df_rand <- olink_wide_synthetic(olink_platform = o_platform,
                                        data_type = data_t,
                                        n_panels = 3L,
                                        n_assays = 45L,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write empty-ish file
        writeLines("foo", wide_excel)

        # check that function runs
        expect_no_condition(
          object = l_obj <- read_npx_wide_top(df = df_rand$wide$df_top,
                                              file = wide_excel,
                                              olink_platform = o_platform,
                                              format_spec = format_s)
        )

        # check that output match
        expect_identical(
          object = l_obj$df_oid,
          expected = df_rand$long$df_top$df_oid
        )

        expect_identical(
          object = l_obj$df_plate,
          expected = df_rand$long$df_top$df_plate
        )

        expect_identical(
          object = l_obj$df_qc_warn,
          expected = df_rand$long$df_top$df_qc_warn
        )

        expect_false(
          object = "df_int_ctrl" %in% names(l_obj)
        )

      }
    )

    ## Ct ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        o_platform <- "Target 48"
        data_t <- "Ct"

        df_rand <- olink_wide_synthetic(olink_platform = o_platform,
                                        data_type = data_t,
                                        n_panels = 3L,
                                        n_assays = 45L,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write empty-ish file
        writeLines("foo", wide_excel)

        # check that function runs
        expect_no_condition(
          object = l_obj <- read_npx_wide_top(df = df_rand$wide$df_top,
                                              file = wide_excel,
                                              olink_platform = o_platform,
                                              format_spec = format_s)
        )

        # check that output match
        expect_identical(
          object = l_obj$df_oid,
          expected = df_rand$long$df_top$df_oid
        )

        expect_identical(
          object = l_obj$df_plate,
          expected = df_rand$long$df_top$df_plate
        )

        expect_false(
          object = "df_qc_warn" %in% names(l_obj)
        )

        expect_false(
          object = "df_int_ctrl" %in% names(l_obj)
        )

      }
    )

    ## Quantified wo int ctrl ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        o_platform <- "Target 48"
        data_t <- "Quantified"

        df_rand <- olink_wide_synthetic(olink_platform = o_platform,
                                        data_type = data_t,
                                        n_panels = 3L,
                                        n_assays = 45L,
                                        n_samples = 88L,
                                        show_int_ctrl = FALSE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write empty-ish file
        writeLines("foo", wide_excel)

        # check that function runs
        expect_no_condition(
          object = l_obj <- read_npx_wide_top(df = df_rand$wide$df_top,
                                              file = wide_excel,
                                              olink_platform = o_platform,
                                              format_spec = format_s)
        )

        # check that output match
        expect_identical(
          object = l_obj$df_oid,
          expected = df_rand$long$df_top$df_oid
        )

        expect_identical(
          object = l_obj$df_plate,
          expected = df_rand$long$df_top$df_plate
        )

        expect_identical(
          object = l_obj$df_qc_warn,
          expected = df_rand$long$df_top$df_qc_warn
        )

        expect_false(
          object = "df_int_ctrl" %in% names(l_obj)
        )

      }
    )

    ## Quantified w int ctrl ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        o_platform <- "Target 48"
        data_t <- "Quantified"

        df_rand <- olink_wide_synthetic(olink_platform = o_platform,
                                        data_type = data_t,
                                        n_panels = 3L,
                                        n_assays = 45L,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write empty-ish file
        writeLines("foo", wide_excel)

        # check that function runs
        expect_no_condition(
          object = l_obj <- read_npx_wide_top(df = df_rand$wide$df_top,
                                              file = wide_excel,
                                              olink_platform = o_platform,
                                              format_spec = format_s)
        )

        # check that output match
        expect_identical(
          object = l_obj$df_oid,
          expected = df_rand$long$df_top$df_oid
        )

        expect_identical(
          object = l_obj$df_plate,
          expected = df_rand$long$df_top$df_plate
        )

        expect_identical(
          object = l_obj$df_qc_warn,
          expected = df_rand$long$df_top$df_qc_warn
        )

        expect_identical(
          object = l_obj$df_int_ctrl,
          expected = df_rand$long$df_top$df_int_ctrl
        )

      }
    )

    ## Quantified w int ctrl shuffled ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        o_platform <- "Target 48"
        data_t <- "Quantified"

        df_rand <- olink_wide_synthetic(olink_platform = o_platform,
                                        data_type = data_t,
                                        n_panels = 3L,
                                        n_assays = 45L,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = TRUE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write empty-ish file
        writeLines("foo", wide_excel)

        # check that function runs
        expect_no_condition(
          object = l_obj <- read_npx_wide_top(df = df_rand$wide$df_top,
                                              file = wide_excel,
                                              olink_platform = o_platform,
                                              format_spec = format_s)
        )

        # check that output match
        expect_identical(
          object = l_obj$df_oid,
          expected = df_rand$long$df_top$df_oid
        )

        expect_identical(
          object = l_obj$df_plate,
          expected = df_rand$long$df_top$df_plate
        )

        expect_identical(
          object = l_obj$df_qc_warn,
          expected = df_rand$long$df_top$df_qc_warn
        )

        expect_identical(
          object = l_obj$df_int_ctrl,
          expected = df_rand$long$df_top$df_int_ctrl
        )

      }
    )

    ## Quantified w int ctrl shuffled in V2 ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        o_platform <- "Target 48"
        data_t <- "Quantified"

        df_rand <- olink_wide_synthetic(olink_platform = o_platform,
                                        data_type = data_t,
                                        n_panels = 3L,
                                        n_assays = 45L,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V2",
                                        shuffle_assays = TRUE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write empty-ish file
        writeLines("foo", wide_excel)

        # check that function runs
        expect_no_condition(
          object = l_obj <- read_npx_wide_top(df = df_rand$wide$df_top,
                                              file = wide_excel,
                                              olink_platform = o_platform,
                                              format_spec = format_s)
        )

        # check that output match
        expect_identical(
          object = l_obj$df_oid,
          expected = df_rand$long$df_top$df_oid
        )

        expect_identical(
          object = l_obj$df_plate,
          expected = df_rand$long$df_top$df_plate
        )

        expect_identical(
          object = l_obj$df_qc_warn,
          expected = df_rand$long$df_top$df_qc_warn
        )

        expect_identical(
          object = l_obj$df_int_ctrl,
          expected = df_rand$long$df_top$df_int_ctrl
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_top - works - T96 - single panel",
  {
    ## NPX ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        o_platform <- "Target 96"
        data_t <- "NPX"

        df_rand <- olink_wide_synthetic(olink_platform = o_platform,
                                        data_type = data_t,
                                        n_panels = 1L,
                                        n_assays = 92L,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write empty-ish file
        writeLines("foo", wide_excel)

        # check that function runs
        expect_no_condition(
          object = l_obj <- read_npx_wide_top(df = df_rand$wide$df_top,
                                              file = wide_excel,
                                              olink_platform = o_platform,
                                              format_spec = format_s)
        )

        # check that output match
        expect_identical(
          object = l_obj$df_oid,
          expected = df_rand$long$df_top$df_oid
        )

        expect_identical(
          object = l_obj$df_plate,
          expected = df_rand$long$df_top$df_plate
        )

        expect_identical(
          object = l_obj$df_qc_warn,
          expected = df_rand$long$df_top$df_qc_warn
        )

        expect_false(
          object = "df_int_ctrl" %in% names(l_obj)
        )

      }
    )

    ## Ct ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        o_platform <- "Target 96"
        data_t <- "Ct"

        df_rand <- olink_wide_synthetic(olink_platform = o_platform,
                                        data_type = data_t,
                                        n_panels = 1L,
                                        n_assays = 92L,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write empty-ish file
        writeLines("foo", wide_excel)

        # check that function runs
        expect_no_condition(
          object = l_obj <- read_npx_wide_top(df = df_rand$wide$df_top,
                                              file = wide_excel,
                                              olink_platform = o_platform,
                                              format_spec = format_s)
        )

        # check that output match
        expect_identical(
          object = l_obj$df_oid,
          expected = df_rand$long$df_top$df_oid
        )

        expect_identical(
          object = l_obj$df_plate,
          expected = df_rand$long$df_top$df_plate
        )

        expect_false(
          object = "df_qc_warn" %in% names(l_obj)
        )

        expect_false(
          object = "df_int_ctrl" %in% names(l_obj)
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_top - works - T96 - multiple panels",
  {
    ## NPX ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        o_platform <- "Target 96"
        data_t <- "NPX"

        df_rand <- olink_wide_synthetic(olink_platform = o_platform,
                                        data_type = data_t,
                                        n_panels = 3L,
                                        n_assays = 92L,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write empty-ish file
        writeLines("foo", wide_excel)

        # check that function runs
        expect_no_condition(
          object = l_obj <- read_npx_wide_top(df = df_rand$wide$df_top,
                                              file = wide_excel,
                                              olink_platform = o_platform,
                                              format_spec = format_s)
        )

        # check that output match
        expect_identical(
          object = l_obj$df_oid,
          expected = df_rand$long$df_top$df_oid
        )

        expect_identical(
          object = l_obj$df_plate,
          expected = df_rand$long$df_top$df_plate
        )

        expect_identical(
          object = l_obj$df_qc_warn,
          expected = df_rand$long$df_top$df_qc_warn
        )

        expect_false(
          object = "df_int_ctrl" %in% names(l_obj)
        )

      }
    )

    ## Ct ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        o_platform <- "Target 96"
        data_t <- "Ct"

        df_rand <- olink_wide_synthetic(olink_platform = o_platform,
                                        data_type = data_t,
                                        n_panels = 3L,
                                        n_assays = 92L,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write empty-ish file
        writeLines("foo", wide_excel)

        # check that function runs
        expect_no_condition(
          object = l_obj <- read_npx_wide_top(df = df_rand$wide$df_top,
                                              file = wide_excel,
                                              olink_platform = o_platform,
                                              format_spec = format_s)
        )

        # check that output match
        expect_identical(
          object = l_obj$df_oid,
          expected = df_rand$long$df_top$df_oid
        )

        expect_identical(
          object = l_obj$df_plate,
          expected = df_rand$long$df_top$df_plate
        )

        expect_false(
          object = "df_qc_warn" %in% names(l_obj)
        )

        expect_false(
          object = "df_int_ctrl" %in% names(l_obj)
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_top - works - Flex/Focus - single panel",
  {
    ## NPX ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        o_platform <- "Flex"
        data_t <- "NPX"

        df_rand <- olink_wide_synthetic(olink_platform = o_platform,
                                        data_type = data_t,
                                        n_panels = 1L,
                                        n_assays = 33L,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write empty-ish file
        writeLines("foo", wide_excel)

        # check that function runs
        expect_no_condition(
          object = l_obj <- read_npx_wide_top(df = df_rand$wide$df_top,
                                              file = wide_excel,
                                              olink_platform = o_platform,
                                              format_spec = format_s)
        )

        # check that output match
        expect_identical(
          object = l_obj$df_oid,
          expected = df_rand$long$df_top$df_oid
        )

        expect_identical(
          object = l_obj$df_plate,
          expected = df_rand$long$df_top$df_plate
        )

        expect_identical(
          object = l_obj$df_qc_warn,
          expected = df_rand$long$df_top$df_qc_warn
        )

        expect_false(
          object = "df_int_ctrl" %in% names(l_obj)
        )

      }
    )

    ## Ct ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        o_platform <- "Flex"
        data_t <- "Ct"

        df_rand <- olink_wide_synthetic(olink_platform = o_platform,
                                        data_type = data_t,
                                        n_panels = 1L,
                                        n_assays = 33L,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write empty-ish file
        writeLines("foo", wide_excel)

        # check that function runs
        expect_no_condition(
          object = l_obj <- read_npx_wide_top(df = df_rand$wide$df_top,
                                              file = wide_excel,
                                              olink_platform = o_platform,
                                              format_spec = format_s)
        )

        # check that output match
        expect_identical(
          object = l_obj$df_oid,
          expected = df_rand$long$df_top$df_oid
        )

        expect_identical(
          object = l_obj$df_plate,
          expected = df_rand$long$df_top$df_plate
        )

        expect_false(
          object = "df_qc_warn" %in% names(l_obj)
        )

        expect_false(
          object = "df_int_ctrl" %in% names(l_obj)
        )

      }
    )

    ## Quantified wo int ctrl ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        o_platform <- "Flex"
        data_t <- "Quantified"

        df_rand <- olink_wide_synthetic(olink_platform = o_platform,
                                        data_type = data_t,
                                        n_panels = 1L,
                                        n_assays = 33L,
                                        n_samples = 88L,
                                        show_int_ctrl = FALSE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write empty-ish file
        writeLines("foo", wide_excel)

        # check that function runs
        expect_no_condition(
          object = l_obj <- read_npx_wide_top(df = df_rand$wide$df_top,
                                              file = wide_excel,
                                              olink_platform = o_platform,
                                              format_spec = format_s)
        )

        # check that output match
        expect_identical(
          object = l_obj$df_oid,
          expected = df_rand$long$df_top$df_oid
        )

        expect_identical(
          object = l_obj$df_plate,
          expected = df_rand$long$df_top$df_plate
        )

        expect_identical(
          object = l_obj$df_qc_warn,
          expected = df_rand$long$df_top$df_qc_warn
        )

        expect_false(
          object = "df_int_ctrl" %in% names(l_obj)
        )

      }
    )

    ## Quantified w int ctrl ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        o_platform <- "Flex"
        data_t <- "Quantified"

        df_rand <- olink_wide_synthetic(olink_platform = o_platform,
                                        data_type = data_t,
                                        n_panels = 1L,
                                        n_assays = 33L,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write empty-ish file
        writeLines("foo", wide_excel)

        # check that function runs
        expect_no_condition(
          object = l_obj <- read_npx_wide_top(df = df_rand$wide$df_top,
                                              file = wide_excel,
                                              olink_platform = o_platform,
                                              format_spec = format_s)
        )

        # check that output match
        expect_identical(
          object = l_obj$df_oid,
          expected = df_rand$long$df_top$df_oid
        )

        expect_identical(
          object = l_obj$df_plate,
          expected = df_rand$long$df_top$df_plate
        )

        expect_identical(
          object = l_obj$df_qc_warn,
          expected = df_rand$long$df_top$df_qc_warn
        )

        expect_identical(
          object = l_obj$df_int_ctrl,
          expected = df_rand$long$df_top$df_int_ctrl
        )

      }
    )

    ## Quantified w int ctrl shuffled ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        o_platform <- "Flex"
        data_t <- "Quantified"

        df_rand <- olink_wide_synthetic(olink_platform = o_platform,
                                        data_type = data_t,
                                        n_panels = 1L,
                                        n_assays = 33L,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = TRUE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write empty-ish file
        writeLines("foo", wide_excel)

        # check that function runs
        expect_no_condition(
          object = l_obj <- read_npx_wide_top(df = df_rand$wide$df_top,
                                              file = wide_excel,
                                              olink_platform = o_platform,
                                              format_spec = format_s)
        )

        # check that output match
        expect_identical(
          object = l_obj$df_oid,
          expected = df_rand$long$df_top$df_oid
        )

        expect_identical(
          object = l_obj$df_plate,
          expected = df_rand$long$df_top$df_plate
        )

        expect_identical(
          object = l_obj$df_qc_warn,
          expected = df_rand$long$df_top$df_qc_warn
        )

        expect_identical(
          object = l_obj$df_int_ctrl,
          expected = df_rand$long$df_top$df_int_ctrl
        )

      }
    )

    ## Quantified w int ctrl shuffled in V2 ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        o_platform <- "Flex"
        data_t <- "Quantified"

        df_rand <- olink_wide_synthetic(olink_platform = o_platform,
                                        data_type = data_t,
                                        n_panels = 1L,
                                        n_assays = 33L,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V2",
                                        shuffle_assays = TRUE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write empty-ish file
        writeLines("foo", wide_excel)

        # check that function runs
        expect_no_condition(
          object = l_obj <- read_npx_wide_top(df = df_rand$wide$df_top,
                                              file = wide_excel,
                                              olink_platform = o_platform,
                                              format_spec = format_s)
        )

        # check that output match
        expect_identical(
          object = l_obj$df_oid,
          expected = df_rand$long$df_top$df_oid
        )

        expect_identical(
          object = l_obj$df_plate,
          expected = df_rand$long$df_top$df_plate
        )

        expect_identical(
          object = l_obj$df_qc_warn,
          expected = df_rand$long$df_top$df_qc_warn
        )

        expect_identical(
          object = l_obj$df_int_ctrl,
          expected = df_rand$long$df_top$df_int_ctrl
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_top - works - Flex/Focus - multiple panels",
  {
    ## NPX ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        o_platform <- "Focus"
        data_t <- "NPX"

        df_rand <- olink_wide_synthetic(olink_platform = o_platform,
                                        data_type = data_t,
                                        n_panels = 3L,
                                        n_assays = 21L,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write empty-ish file
        writeLines("foo", wide_excel)

        # check that function runs
        expect_no_condition(
          object = l_obj <- read_npx_wide_top(df = df_rand$wide$df_top,
                                              file = wide_excel,
                                              olink_platform = o_platform,
                                              format_spec = format_s)
        )

        # check that output match
        expect_identical(
          object = l_obj$df_oid,
          expected = df_rand$long$df_top$df_oid
        )

        expect_identical(
          object = l_obj$df_plate,
          expected = df_rand$long$df_top$df_plate
        )

        expect_identical(
          object = l_obj$df_qc_warn,
          expected = df_rand$long$df_top$df_qc_warn
        )

        expect_false(
          object = "df_int_ctrl" %in% names(l_obj)
        )

      }
    )

    ## Ct ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        o_platform <- "Focus"
        data_t <- "Ct"

        df_rand <- olink_wide_synthetic(olink_platform = o_platform,
                                        data_type = data_t,
                                        n_panels = 3L,
                                        n_assays = 21L,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write empty-ish file
        writeLines("foo", wide_excel)

        # check that function runs
        expect_no_condition(
          object = l_obj <- read_npx_wide_top(df = df_rand$wide$df_top,
                                              file = wide_excel,
                                              olink_platform = o_platform,
                                              format_spec = format_s)
        )

        # check that output match
        expect_identical(
          object = l_obj$df_oid,
          expected = df_rand$long$df_top$df_oid
        )

        expect_identical(
          object = l_obj$df_plate,
          expected = df_rand$long$df_top$df_plate
        )

        expect_false(
          object = "df_qc_warn" %in% names(l_obj)
        )

        expect_false(
          object = "df_int_ctrl" %in% names(l_obj)
        )

      }
    )

    ## Quantified wo int ctrl ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        o_platform <- "Focus"
        data_t <- "Quantified"

        df_rand <- olink_wide_synthetic(olink_platform = o_platform,
                                        data_type = data_t,
                                        n_panels = 3L,
                                        n_assays = 21L,
                                        n_samples = 88L,
                                        show_int_ctrl = FALSE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write empty-ish file
        writeLines("foo", wide_excel)

        # check that function runs
        expect_no_condition(
          object = l_obj <- read_npx_wide_top(df = df_rand$wide$df_top,
                                              file = wide_excel,
                                              olink_platform = o_platform,
                                              format_spec = format_s)
        )

        # check that output match
        expect_identical(
          object = l_obj$df_oid,
          expected = df_rand$long$df_top$df_oid
        )

        expect_identical(
          object = l_obj$df_plate,
          expected = df_rand$long$df_top$df_plate
        )

        expect_identical(
          object = l_obj$df_qc_warn,
          expected = df_rand$long$df_top$df_qc_warn
        )

        expect_false(
          object = "df_int_ctrl" %in% names(l_obj)
        )

      }
    )

    ## Quantified w int ctrl ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        o_platform <- "Focus"
        data_t <- "Quantified"

        df_rand <- olink_wide_synthetic(olink_platform = o_platform,
                                        data_type = data_t,
                                        n_panels = 3L,
                                        n_assays = 21L,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write empty-ish file
        writeLines("foo", wide_excel)

        # check that function runs
        expect_no_condition(
          object = l_obj <- read_npx_wide_top(df = df_rand$wide$df_top,
                                              file = wide_excel,
                                              olink_platform = o_platform,
                                              format_spec = format_s)
        )

        # check that output match
        expect_identical(
          object = l_obj$df_oid,
          expected = df_rand$long$df_top$df_oid
        )

        expect_identical(
          object = l_obj$df_plate,
          expected = df_rand$long$df_top$df_plate
        )

        expect_identical(
          object = l_obj$df_qc_warn,
          expected = df_rand$long$df_top$df_qc_warn
        )

        expect_identical(
          object = l_obj$df_int_ctrl,
          expected = df_rand$long$df_top$df_int_ctrl
        )

      }
    )

    ## Quantified w int ctrl shuffled ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        o_platform <- "Focus"
        data_t <- "Quantified"

        df_rand <- olink_wide_synthetic(olink_platform = o_platform,
                                        data_type = data_t,
                                        n_panels = 3L,
                                        n_assays = 21L,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = TRUE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write empty-ish file
        writeLines("foo", wide_excel)

        # check that function runs
        expect_no_condition(
          object = l_obj <- read_npx_wide_top(df = df_rand$wide$df_top,
                                              file = wide_excel,
                                              olink_platform = o_platform,
                                              format_spec = format_s)
        )

        # check that output match
        expect_identical(
          object = l_obj$df_oid,
          expected = df_rand$long$df_top$df_oid
        )

        expect_identical(
          object = l_obj$df_plate,
          expected = df_rand$long$df_top$df_plate
        )

        expect_identical(
          object = l_obj$df_qc_warn,
          expected = df_rand$long$df_top$df_qc_warn
        )

        expect_identical(
          object = l_obj$df_int_ctrl,
          expected = df_rand$long$df_top$df_int_ctrl
        )

      }
    )

    ## Quantified w int ctrl shuffled in V2 ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        o_platform <- "Focus"
        data_t <- "Quantified"

        df_rand <- olink_wide_synthetic(olink_platform = o_platform,
                                        data_type = data_t,
                                        n_panels = 3L,
                                        n_assays = 21L,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V2",
                                        shuffle_assays = TRUE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write empty-ish file
        writeLines("foo", wide_excel)

        # check that function runs
        expect_no_condition(
          object = l_obj <- read_npx_wide_top(df = df_rand$wide$df_top,
                                              file = wide_excel,
                                              olink_platform = o_platform,
                                              format_spec = format_s)
        )

        # check that output match
        expect_identical(
          object = l_obj$df_oid,
          expected = df_rand$long$df_top$df_oid
        )

        expect_identical(
          object = l_obj$df_plate,
          expected = df_rand$long$df_top$df_plate
        )

        expect_identical(
          object = l_obj$df_qc_warn,
          expected = df_rand$long$df_top$df_qc_warn
        )

        expect_identical(
          object = l_obj$df_int_ctrl,
          expected = df_rand$long$df_top$df_int_ctrl
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_top - error - missing or too many labels in rows 2 & 3",
  {
    # df contains an unrecognizable tag ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        o_platform <- "Target 48"
        data_t <- "NPX"

        df_rand <- olink_wide_synthetic(olink_platform = o_platform,
                                        data_type = data_t,
                                        n_panels = 1L,
                                        n_assays = 45L,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # add a new column with an unrecognizable tag
        df_top_add_col_cname <- paste0("V", (ncol(df_rand$wide$df_top) + 1L))
        df_top_last_cname <- paste0("V", ncol(df_rand$wide$df_top))
        df_top_add_col <- df_rand$wide$df_top |>
          dplyr::select(
            dplyr::all_of(df_top_last_cname)
          ) |>
          dplyr::rename(
            {{df_top_add_col_cname}} := dplyr::all_of(df_top_last_cname)
          ) |>
          dplyr::mutate(
            dplyr::across(
              dplyr::everything(),
              ~ dplyr::if_else(dplyr::row_number() == 2L, "Unknown", .x)
            )
          )

        df_rand$wide$df_top <- df_rand$wide$df_top |>
          dplyr::bind_cols(
            df_top_add_col
          )

        # write empty-ish file
        writeLines("foo", wide_excel)

        # check that function runs with error
        expect_error(
          object = read_npx_wide_top(df = df_rand$wide$df_top,
                                     file = wide_excel,
                                     olink_platform = o_platform,
                                     format_spec = format_s),
          regexp = "The top matrix with the assays metadata in file"
        )

      }
    )

    # df contains QC Warning with data type Ct ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        o_platform <- "Target 48"
        data_t <- "Ct"

        df_rand <- olink_wide_synthetic(olink_platform = o_platform,
                                        data_type = "NPX",
                                        n_panels = 1L,
                                        n_assays = 45L,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write empty-ish file
        writeLines("foo", wide_excel)

        # check that function runs with error
        expect_error(
          object = read_npx_wide_top(df = df_rand$wide$df_top,
                                     file = wide_excel,
                                     olink_platform = o_platform,
                                     format_spec = format_s),
          regexp = "The top matrix with the assays metadata in file"
        )

      }
    )

    # df contains contains internal controls with data type NPX ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        o_platform <- "Target 48"
        data_t <- "NPX"

        df_rand <- olink_wide_synthetic(olink_platform = o_platform,
                                        data_type = "Quantified",
                                        n_panels = 1L,
                                        n_assays = 45L,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # remove row with V1 = Unit to run the test
        df_rand$wide$df_top <- df_rand$wide$df_top |>
          dplyr::slice(
            1L:4L
          )

        # write empty-ish file
        writeLines("foo", wide_excel)

        # check that function runs with error
        expect_error(
          object = read_npx_wide_top(df = df_rand$wide$df_top,
                                     file = wide_excel,
                                     olink_platform = o_platform,
                                     format_spec = format_s),
          regexp = "The top matrix with the assays metadata in file"
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_top - error - NAs in OlinkID/Uniprot/Assay",
  {
    ## OlinkID = NA 1 instance ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        o_platform <- "Target 48"
        data_t <- "NPX"

        df_rand <- olink_wide_synthetic(olink_platform = o_platform,
                                        data_type = data_t,
                                        n_panels = 1L,
                                        n_assays = 45L,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # intrduce NA cells to test
        df_rand$wide$df_top <- df_rand$wide$df_top |>
          dplyr::mutate(
            V2 = dplyr::if_else(.data[["V2"]] %in% "Uniprot1",
                                NA_character_,
                                .data[["V2"]])
          )

        # write empty-ish file
        writeLines("foo", wide_excel)

        # check that function runs with error
        expect_error(
          object = read_npx_wide_top(df = df_rand$wide$df_top,
                                     file = wide_excel,
                                     olink_platform = o_platform,
                                     format_spec = format_s),
          regexp = "Detected 1 empty cells in columns"
        )

      }
    )

    ## OlinkID = NA 4 instances ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        o_platform <- "Target 48"
        data_t <- "NPX"

        df_rand <- olink_wide_synthetic(olink_platform = o_platform,
                                        data_type = data_t,
                                        n_panels = 1L,
                                        n_assays = 45L,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # intrduce NA cells to test
        df_rand$wide$df_top <- df_rand$wide$df_top |>
          dplyr::mutate(
            V2 = dplyr::if_else(
              .data[["V2"]] %in% paste0(c("Assay", "Uniprot"), 1L),
              NA_character_,
              .data[["V2"]]
            ),
            V3 = dplyr::if_else(
              .data[["V3"]] %in% paste0(c("Assay", "Uniprot"), 2L),
              NA_character_,
              .data[["V3"]]
            )
          )

        # write empty-ish file
        writeLines("foo", wide_excel)

        # check that function runs with error
        expect_error(
          object = read_npx_wide_top(df = df_rand$wide$df_top,
                                     file = wide_excel,
                                     olink_platform = o_platform,
                                     format_spec = format_s),
          regexp = "Detected 4 empty cells in columns"
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_top - error - wrong # of assays",
  {
    ## T48 1 panel ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        o_platform <- "Target 48"
        data_t <- "NPX"
        n_panel <- 1L
        n_assay <- 40L

        df_rand <- olink_wide_synthetic(olink_platform = o_platform,
                                        data_type = data_t,
                                        n_panels = n_panel,
                                        n_assays = n_assay,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write empty-ish file
        writeLines("foo", wide_excel)

        # check that function runs
        expect_error(
          object = read_npx_wide_top(df = df_rand$wide$df_top,
                                     file = wide_excel,
                                     olink_platform = o_platform,
                                     format_spec = format_s),
          regexp = "Detected 40 assays in 1 panels in file"
        )

      }
    )

    ## T48 2 panels ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        o_platform <- "Target 48"
        data_t <- "NPX"
        n_panel <- 2L
        n_assay <- 32L

        df_rand <- olink_wide_synthetic(olink_platform = o_platform,
                                        data_type = data_t,
                                        n_panels = n_panel,
                                        n_assays = n_assay,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write empty-ish file
        writeLines("foo", wide_excel)

        # check that function runs
        expect_error(
          object = read_npx_wide_top(df = df_rand$wide$df_top,
                                     file = wide_excel,
                                     olink_platform = o_platform,
                                     format_spec = format_s),
          regexp = "Detected 64 assays in 2 panels in file"
        )

      }
    )

    ## T96 1 panel ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        o_platform <- "Target 96"
        data_t <- "NPX"
        n_panel <- 1L
        n_assay <- 67L

        df_rand <- olink_wide_synthetic(olink_platform = o_platform,
                                        data_type = data_t,
                                        n_panels = n_panel,
                                        n_assays = n_assay,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write empty-ish file
        writeLines("foo", wide_excel)

        # check that function runs
        expect_error(
          object = read_npx_wide_top(df = df_rand$wide$df_top,
                                     file = wide_excel,
                                     olink_platform = o_platform,
                                     format_spec = format_s),
          regexp = "Detected 67 assays in 1 panels in file"
        )

      }
    )

    ## T96 2 panels ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        o_platform <- "Target 96"
        data_t <- "NPX"
        n_panel <- 2L
        n_assay <- 78L

        df_rand <- olink_wide_synthetic(olink_platform = o_platform,
                                        data_type = data_t,
                                        n_panels = n_panel,
                                        n_assays = n_assay,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write empty-ish file
        writeLines("foo", wide_excel)

        # check that function runs
        expect_error(
          object = read_npx_wide_top(df = df_rand$wide$df_top,
                                     file = wide_excel,
                                     olink_platform = o_platform,
                                     format_spec = format_s),
          regexp = "Detected 156 assays in 2 panels in file"
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_top - error - uneven # of Plate ID and QC_Warning cols",
  {
    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        o_platform <- "Target 48"
        n_panel <- 2L
        n_assay <- 45L
        data_t <- "NPX"

        df_rand <- olink_wide_synthetic(olink_platform = o_platform,
                                        data_type = data_t,
                                        n_panels = n_panel,
                                        n_assays = n_assay,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # remove one column from df_top to reproduce the error
        remove_col <- paste0("V", (1L + (n_panel * n_assay) + (2L * n_panel)))

        df_rand$wide$df_top <- df_rand$wide$df_top |>
          dplyr::select(
            -dplyr::all_of(remove_col)
          )

        # write empty-ish file
        writeLines("foo", wide_excel)

        # check that function runs
        expect_error(
          object = read_npx_wide_top(df = df_rand$wide$df_top,
                                     file = wide_excel,
                                     olink_platform = o_platform,
                                     format_spec = format_s),
          regexp = "Expected equal number of \"Plate ID\" and \"QC\ Warning\""
        )

      }
    )

  }
)

# Test read_npx_wide_bottom ----

test_that(
  "read_npx_wide_bottom - works - T48 - single panel",
  {
    ## NPX ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_t <- "NPX"

        df_rand <- olink_wide_synthetic(olink_platform = "Target 48",
                                        data_type = data_t,
                                        n_panels = 1L,
                                        n_assays = 45L,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # extract col_split and assa_cols
        col_s <- df_rand$long$df_top$df_plate |>
          dplyr::mutate(
            col_index_n = stringr::str_sub(string = .data[["col_index"]],
                                           start = 2L) |>
              as.integer()
          ) |>
          dplyr::arrange(.data[["col_index_n"]]) |>
          dplyr::slice_head(n = 1L) |>
          dplyr::pull(.data[["col_index"]])
        assay_c <- df_rand$long$df_top$df_oid$col_index

        # write empty-ish file
        writeLines("foo", wide_excel)

        # check that function runs
        expect_no_condition(
          object = df_obj <- read_npx_wide_bottom(df = df_rand$wide$df_bottom,
                                                  file = wide_excel,
                                                  col_split = col_s,
                                                  assay_cols = assay_c,
                                                  int_ctrl_cols = NULL,
                                                  format_spec = format_s)
        )

        # check that dfs are identical
        expect_identical(
          object = df_obj$df_oid |>
            dplyr::select(
              order(colnames(df_obj$df_oid))
            ),
          expected = df_rand$long$df_bottom$df_oid
        )

        expect_false(object = "df_int_ctrl" %in% names(df_obj))

      }
    )

    ## Quantified ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_t <- "Quantified"

        df_rand <- olink_wide_synthetic(olink_platform = "Target 48",
                                        data_type = data_t,
                                        n_panels = 1L,
                                        n_assays = 45L,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # extract col_split and assa_cols
        col_s <- df_rand$long$df_top$df_plate |>
          dplyr::mutate(
            col_index_n = stringr::str_sub(string = .data[["col_index"]],
                                           start = 2L) |>
              as.integer()
          ) |>
          dplyr::arrange(.data[["col_index_n"]]) |>
          dplyr::slice_head(n = 1L) |>
          dplyr::pull(.data[["col_index"]])
        assay_c <- df_rand$long$df_top$df_oid$col_index

        # write empty-ish file
        writeLines("foo", wide_excel)

        # run function ----

        # switched from expect_no_condition to the one below because of a very
        # weird silent message thrown by rlang when using group_by and arrange
        # in dplyr. This is documented here
        # https://github.com/aryoda/tryCatchLog/issues/62
        # Looks like it has been fixed, but we still get the error.
        expect_no_error(
          expect_no_warning(
            object = df_obj <- read_npx_wide_bottom(df = df_rand$wide$df_bottom,
                                                    file = wide_excel,
                                                    col_split = col_s,
                                                    assay_cols = assay_c,
                                                    int_ctrl_cols = NULL,
                                                    format_spec = format_s)
          )
        )

        # check that dfs are identical
        expect_identical(
          object = df_obj$df_oid |>
            dplyr::select(
              order(colnames(df_obj$df_oid))
            ),
          expected = df_rand$long$df_bottom$df_oid
        )

        expect_false(object = "df_int_ctrl" %in% names(df_obj))

      }
    )

    ## Quantified shuffled ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_t <- "Quantified"

        df_rand <- olink_wide_synthetic(olink_platform = "Target 48",
                                        data_type = data_t,
                                        n_panels = 1L,
                                        n_assays = 45L,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = TRUE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # extract col_split and assa_cols
        col_s <- df_rand$long$df_top$df_plate |>
          dplyr::mutate(
            col_index_n = stringr::str_sub(string = .data[["col_index"]],
                                           start = 2L) |>
              as.integer()
          ) |>
          dplyr::arrange(.data[["col_index_n"]]) |>
          dplyr::slice_head(n = 1L) |>
          dplyr::pull(.data[["col_index"]])
        assay_c <- df_rand$long$df_top$df_oid$col_index
        int_ctrl_c <- df_rand$long$df_top$df_int_ctrl$col_index

        # write empty-ish file
        writeLines("foo", wide_excel)

        # run function ----

        # switched from expect_no_condition to the one below because of a very
        # weird silent message thrown by rlang when using group_by and arrange
        # in dplyr. This is documented here
        # https://github.com/aryoda/tryCatchLog/issues/62
        # Looks like it has been fixed, but we still get the error.
        expect_no_error(
          expect_no_warning(
            object = df_obj <- read_npx_wide_bottom(df = df_rand$wide$df_bottom,
                                                    file = wide_excel,
                                                    col_split = col_s,
                                                    assay_cols = assay_c,
                                                    int_ctrl_cols = int_ctrl_c,
                                                    format_spec = format_s)
          )
        )

        # check that dfs are identical
        expect_identical(
          object = df_obj$df_oid |>
            dplyr::select(
              order(colnames(df_obj$df_oid))
            ),
          expected = df_rand$long$df_bottom$df_oid
        )

        expect_identical(
          object = df_obj$df_int_ctrl |>
            dplyr::select(
              order(colnames(df_obj$df_int_ctrl))
            ),
          expected = df_rand$long$df_bottom$df_int_ctrl
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_bottom - works - T48 - multiple panels",
  {
    ## NPX ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_t <- "NPX"

        df_rand <- olink_wide_synthetic(olink_platform = "Target 48",
                                        data_type = data_t,
                                        n_panels = 4L,
                                        n_assays = 45L,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # extract col_split and assa_cols
        col_s <- df_rand$long$df_top$df_plate |>
          dplyr::mutate(
            col_index_n = stringr::str_sub(string = .data[["col_index"]],
                                           start = 2L) |>
              as.integer()
          ) |>
          dplyr::arrange(.data[["col_index_n"]]) |>
          dplyr::slice_head(n = 1L) |>
          dplyr::pull(.data[["col_index"]])
        assay_c <- df_rand$long$df_top$df_oid$col_index

        # write empty-ish file
        writeLines("foo", wide_excel)

        # check that function runs
        expect_no_condition(
          object = df_obj <- read_npx_wide_bottom(df = df_rand$wide$df_bottom,
                                                  file = wide_excel,
                                                  col_split = col_s,
                                                  assay_cols = assay_c,
                                                  int_ctrl_cols = NULL,
                                                  format_spec = format_s)
        )

        # check that dfs are identical
        expect_identical(
          object = df_obj$df_oid |>
            dplyr::select(
              order(colnames(df_obj$df_oid))
            ),
          expected = df_rand$long$df_bottom$df_oid
        )

        expect_false(object = "df_int_ctrl" %in% names(df_obj))

      }
    )

    ## Quantified ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_t <- "Quantified"

        df_rand <- olink_wide_synthetic(olink_platform = "Target 48",
                                        data_type = data_t,
                                        n_panels = 4L,
                                        n_assays = 45L,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # extract col_split and assa_cols
        col_s <- df_rand$long$df_top$df_plate |>
          dplyr::mutate(
            col_index_n = stringr::str_sub(string = .data[["col_index"]],
                                           start = 2L) |>
              as.integer()
          ) |>
          dplyr::arrange(.data[["col_index_n"]]) |>
          dplyr::slice_head(n = 1L) |>
          dplyr::pull(.data[["col_index"]])
        assay_c <- df_rand$long$df_top$df_oid$col_index

        # write empty-ish file
        writeLines("foo", wide_excel)

        # run function ----

        # switched from expect_no_condition to the one below because of a very
        # weird silent message thrown by rlang when using group_by and arrange
        # in dplyr. This is documented here
        # https://github.com/aryoda/tryCatchLog/issues/62
        # Looks like it has been fixed, but we still get the error.
        expect_no_error(
          expect_no_warning(
            object = df_obj <- read_npx_wide_bottom(df = df_rand$wide$df_bottom,
                                                    file = wide_excel,
                                                    col_split = col_s,
                                                    assay_cols = assay_c,
                                                    int_ctrl_cols = NULL,
                                                    format_spec = format_s)
          )
        )

        # check that dfs are identical
        expect_identical(
          object = df_obj$df_oid |>
            dplyr::select(
              order(colnames(df_obj$df_oid))
            ),
          expected = df_rand$long$df_bottom$df_oid
        )

        expect_false(object = "df_int_ctrl" %in% names(df_obj))

      }
    )

    ## Quantified shuffled ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_t <- "Quantified"

        df_rand <- olink_wide_synthetic(olink_platform = "Target 48",
                                        data_type = data_t,
                                        n_panels = 4L,
                                        n_assays = 45L,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = TRUE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # extract col_split and assa_cols
        col_s <- df_rand$long$df_top$df_plate |>
          dplyr::mutate(
            col_index_n = stringr::str_sub(string = .data[["col_index"]],
                                           start = 2L) |>
              as.integer()
          ) |>
          dplyr::arrange(.data[["col_index_n"]]) |>
          dplyr::slice_head(n = 1L) |>
          dplyr::pull(.data[["col_index"]])
        assay_c <- df_rand$long$df_top$df_oid$col_index
        int_ctrl_c <- df_rand$long$df_top$df_int_ctrl$col_index

        # write empty-ish file
        writeLines("foo", wide_excel)

        # run function ----

        # switched from expect_no_condition to the one below because of a very
        # weird silent message thrown by rlang when using group_by and arrange
        # in dplyr. This is documented here
        # https://github.com/aryoda/tryCatchLog/issues/62
        # Looks like it has been fixed, but we still get the error.
        expect_no_error(
          expect_no_warning(
            object = df_obj <- read_npx_wide_bottom(df = df_rand$wide$df_bottom,
                                                    file = wide_excel,
                                                    col_split = col_s,
                                                    assay_cols = assay_c,
                                                    int_ctrl_cols = int_ctrl_c,
                                                    format_spec = format_s)
          )
        )

        # check that dfs are identical
        expect_identical(
          object = df_obj$df_oid |>
            dplyr::select(
              order(colnames(df_obj$df_oid))
            ),
          expected = df_rand$long$df_bottom$df_oid
        )

        expect_identical(
          object = df_obj$df_int_ctrl |>
            dplyr::select(
              order(colnames(df_obj$df_int_ctrl))
            ),
          expected = df_rand$long$df_bottom$df_int_ctrl
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_bottom - works - T96 - single panel",
  {
    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_t <- "NPX"

        df_rand <- olink_wide_synthetic(olink_platform = "Target 96",
                                        data_type = data_t,
                                        n_panels = 1L,
                                        n_assays = 92L,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # extract col_split and assa_cols
        col_s <- df_rand$long$df_top$df_plate |>
          dplyr::mutate(
            col_index_n = stringr::str_sub(string = .data[["col_index"]],
                                           start = 2L) |>
              as.integer()
          ) |>
          dplyr::arrange(.data[["col_index_n"]]) |>
          dplyr::slice_head(n = 1L) |>
          dplyr::pull(.data[["col_index"]])
        assay_c <- df_rand$long$df_top$df_oid$col_index

        # write empty-ish file
        writeLines("foo", wide_excel)

        # check that function runs
        expect_no_condition(
          object = df_obj <- read_npx_wide_bottom(df = df_rand$wide$df_bottom,
                                                  file = wide_excel,
                                                  col_split = col_s,
                                                  assay_cols = assay_c,
                                                  int_ctrl_cols = NULL,
                                                  format_spec = format_s)
        )

        # check that dfs are identical
        expect_identical(
          object = df_obj$df_oid |>
            dplyr::select(
              order(colnames(df_obj$df_oid))
            ),
          expected = df_rand$long$df_bottom$df_oid
        )

        expect_false(object = "df_int_ctrl" %in% names(df_obj))

      }
    )

  }
)

test_that(
  "read_npx_wide_bottom - works - T96 - multiple panels",
  {
    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_t <- "NPX"

        df_rand <- olink_wide_synthetic(olink_platform = "Target 96",
                                        data_type = data_t,
                                        n_panels = 4L,
                                        n_assays = 92L,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # extract col_split and assa_cols
        col_s <- df_rand$long$df_top$df_plate |>
          dplyr::mutate(
            col_index_n = stringr::str_sub(string = .data[["col_index"]],
                                           start = 2L) |>
              as.integer()
          ) |>
          dplyr::arrange(.data[["col_index_n"]]) |>
          dplyr::slice_head(n = 1L) |>
          dplyr::pull(.data[["col_index"]])
        assay_c <- df_rand$long$df_top$df_oid$col_index

        # write empty-ish file
        writeLines("foo", wide_excel)

        # check that function runs
        expect_no_condition(
          object = df_obj <- read_npx_wide_bottom(df = df_rand$wide$df_bottom,
                                                  file = wide_excel,
                                                  col_split = col_s,
                                                  assay_cols = assay_c,
                                                  int_ctrl_cols = NULL,
                                                  format_spec = format_s)
        )

        # check that dfs are identical
        expect_identical(
          object = df_obj$df_oid |>
            dplyr::select(
              order(colnames(df_obj$df_oid))
            ),
          expected = df_rand$long$df_bottom$df_oid
        )

        expect_false(object = "df_int_ctrl" %in% names(df_obj))

      }
    )

  }
)

test_that(
  "read_npx_wide_bottom - works - Flex/Focus - single panel",
  {
    ## NPX ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_t <- "NPX"

        df_rand <- olink_wide_synthetic(olink_platform = "Focus",
                                        data_type = data_t,
                                        n_panels = 1L,
                                        n_assays = 33L,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # extract col_split and assa_cols
        col_s <- df_rand$long$df_top$df_plate |>
          dplyr::mutate(
            col_index_n = stringr::str_sub(string = .data[["col_index"]],
                                           start = 2L) |>
              as.integer()
          ) |>
          dplyr::arrange(.data[["col_index_n"]]) |>
          dplyr::slice_head(n = 1L) |>
          dplyr::pull(.data[["col_index"]])
        assay_c <- df_rand$long$df_top$df_oid$col_index

        # write empty-ish file
        writeLines("foo", wide_excel)

        # check that function runs
        expect_no_condition(
          object = df_obj <- read_npx_wide_bottom(df = df_rand$wide$df_bottom,
                                                  file = wide_excel,
                                                  col_split = col_s,
                                                  assay_cols = assay_c,
                                                  int_ctrl_cols = NULL,
                                                  format_spec = format_s)
        )

        # check that dfs are identical
        expect_identical(
          object = df_obj$df_oid |>
            dplyr::select(
              order(colnames(df_obj$df_oid))
            ),
          expected = df_rand$long$df_bottom$df_oid
        )

        expect_false(object = "df_int_ctrl" %in% names(df_obj))

      }
    )

    ## Quantified ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_t <- "Quantified"

        df_rand <- olink_wide_synthetic(olink_platform = "Focus",
                                        data_type = data_t,
                                        n_panels = 1L,
                                        n_assays = 33L,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # extract col_split and assa_cols
        col_s <- df_rand$long$df_top$df_plate |>
          dplyr::mutate(
            col_index_n = stringr::str_sub(string = .data[["col_index"]],
                                           start = 2L) |>
              as.integer()
          ) |>
          dplyr::arrange(.data[["col_index_n"]]) |>
          dplyr::slice_head(n = 1L) |>
          dplyr::pull(.data[["col_index"]])
        assay_c <- df_rand$long$df_top$df_oid$col_index

        # write empty-ish file
        writeLines("foo", wide_excel)

        # switched from expect_no_condition to the one below because of a very
        # weird silent message thrown by rlang when using group_by and arrange
        # in dplyr. This is documented here
        # https://github.com/aryoda/tryCatchLog/issues/62
        # Looks like it has been fixed, but we still get the error.
        expect_no_error(
          expect_no_warning(
            object = df_obj <- read_npx_wide_bottom(df = df_rand$wide$df_bottom,
                                                    file = wide_excel,
                                                    col_split = col_s,
                                                    assay_cols = assay_c,
                                                    int_ctrl_cols = NULL,
                                                    format_spec = format_s)
          )
        )

        # check that dfs are identical
        expect_identical(
          object = df_obj$df_oid |>
            dplyr::select(
              order(colnames(df_obj$df_oid))
            ),
          expected = df_rand$long$df_bottom$df_oid
        )

        expect_false(object = "df_int_ctrl" %in% names(df_obj))

      }
    )

    ## Quantified shuffled ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_t <- "Quantified"

        df_rand <- olink_wide_synthetic(olink_platform = "Focus",
                                        data_type = data_t,
                                        n_panels = 1L,
                                        n_assays = 33L,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = TRUE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # extract col_split and assa_cols
        col_s <- df_rand$long$df_top$df_plate |>
          dplyr::mutate(
            col_index_n = stringr::str_sub(string = .data[["col_index"]],
                                           start = 2L) |>
              as.integer()
          ) |>
          dplyr::arrange(.data[["col_index_n"]]) |>
          dplyr::slice_head(n = 1L) |>
          dplyr::pull(.data[["col_index"]])
        assay_c <- df_rand$long$df_top$df_oid$col_index
        int_ctrl_c <- df_rand$long$df_top$df_int_ctrl$col_index

        # write empty-ish file
        writeLines("foo", wide_excel)

        # switched from expect_no_condition to the one below because of a very
        # weird silent message thrown by rlang when using group_by and arrange
        # in dplyr. This is documented here
        # https://github.com/aryoda/tryCatchLog/issues/62
        # Looks like it has been fixed, but we still get the error.
        expect_no_error(
          expect_no_warning(
            object = df_obj <- read_npx_wide_bottom(df = df_rand$wide$df_bottom,
                                                    file = wide_excel,
                                                    col_split = col_s,
                                                    assay_cols = assay_c,
                                                    int_ctrl_cols = int_ctrl_c,
                                                    format_spec = format_s)
          )
        )

        # check that dfs are identical
        expect_identical(
          object = df_obj$df_oid |>
            dplyr::select(
              order(colnames(df_obj$df_oid))
            ),
          expected = df_rand$long$df_bottom$df_oid
        )

        expect_identical(
          object = df_obj$df_int_ctrl |>
            dplyr::select(
              order(colnames(df_obj$df_int_ctrl))
            ),
          expected = df_rand$long$df_bottom$df_int_ctrl
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_bottom - works - Flex/Focus - multiple panels",
  {
    ## NPX ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_t <- "NPX"

        df_rand <- olink_wide_synthetic(olink_platform = "Flex",
                                        data_type = data_t,
                                        n_panels = 4L,
                                        n_assays = 21L,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # extract col_split and assa_cols
        col_s <- df_rand$long$df_top$df_plate |>
          dplyr::mutate(
            col_index_n = stringr::str_sub(string = .data[["col_index"]],
                                           start = 2L) |>
              as.integer()
          ) |>
          dplyr::arrange(.data[["col_index_n"]]) |>
          dplyr::slice_head(n = 1L) |>
          dplyr::pull(.data[["col_index"]])
        assay_c <- df_rand$long$df_top$df_oid$col_index

        # write empty-ish file
        writeLines("foo", wide_excel)

        # check that function runs
        expect_no_condition(
          object = df_obj <- read_npx_wide_bottom(df = df_rand$wide$df_bottom,
                                                  file = wide_excel,
                                                  col_split = col_s,
                                                  assay_cols = assay_c,
                                                  int_ctrl_cols = NULL,
                                                  format_spec = format_s)
        )

        # check that dfs are identical
        expect_identical(
          object = df_obj$df_oid |>
            dplyr::select(
              order(colnames(df_obj$df_oid))
            ),
          expected = df_rand$long$df_bottom$df_oid
        )

        expect_false(object = "df_int_ctrl" %in% names(df_obj))

      }
    )

    ## Quantified ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_t <- "Quantified"

        df_rand <- olink_wide_synthetic(olink_platform = "Flex",
                                        data_type = data_t,
                                        n_panels = 4L,
                                        n_assays = 21L,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # extract col_split and assa_cols
        col_s <- df_rand$long$df_top$df_plate |>
          dplyr::mutate(
            col_index_n = stringr::str_sub(string = .data[["col_index"]],
                                           start = 2L) |>
              as.integer()
          ) |>
          dplyr::arrange(.data[["col_index_n"]]) |>
          dplyr::slice_head(n = 1L) |>
          dplyr::pull(.data[["col_index"]])
        assay_c <- df_rand$long$df_top$df_oid$col_index

        # write empty-ish file
        writeLines("foo", wide_excel)

        # switched from expect_no_condition to the one below because of a very
        # weird silent message thrown by rlang when using group_by and arrange
        # in dplyr. This is documented here
        # https://github.com/aryoda/tryCatchLog/issues/62
        # Looks like it has been fixed, but we still get the error.
        expect_no_error(
          expect_no_warning(
            object = df_obj <- read_npx_wide_bottom(df = df_rand$wide$df_bottom,
                                                    file = wide_excel,
                                                    col_split = col_s,
                                                    assay_cols = assay_c,
                                                    int_ctrl_cols = NULL,
                                                    format_spec = format_s)
          )
        )

        # check that dfs are identical
        expect_identical(
          object = df_obj$df_oid |>
            dplyr::select(
              order(colnames(df_obj$df_oid))
            ),
          expected = df_rand$long$df_bottom$df_oid
        )

        expect_false(object = "df_int_ctrl" %in% names(df_obj))

      }
    )

    ## Quantified shuffled ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_t <- "Quantified"

        df_rand <- olink_wide_synthetic(olink_platform = "Flex",
                                        data_type = data_t,
                                        n_panels = 4L,
                                        n_assays = 21L,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = TRUE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # extract col_split and assa_cols
        col_s <- df_rand$long$df_top$df_plate |>
          dplyr::mutate(
            col_index_n = stringr::str_sub(string = .data[["col_index"]],
                                           start = 2L) |>
              as.integer()
          ) |>
          dplyr::arrange(.data[["col_index_n"]]) |>
          dplyr::slice_head(n = 1L) |>
          dplyr::pull(.data[["col_index"]])
        assay_c <- df_rand$long$df_top$df_oid$col_index
        int_ctrl_c <- df_rand$long$df_top$df_int_ctrl$col_index

        # write empty-ish file
        writeLines("foo", wide_excel)

        # switched from expect_no_condition to the one below because of a very
        # weird silent message thrown by rlang when using group_by and arrange
        # in dplyr. This is documented here
        # https://github.com/aryoda/tryCatchLog/issues/62
        # Looks like it has been fixed, but we still get the error.
        expect_no_error(
          expect_no_warning(
            object = df_obj <- read_npx_wide_bottom(df = df_rand$wide$df_bottom,
                                                    file = wide_excel,
                                                    col_split = col_s,
                                                    assay_cols = assay_c,
                                                    int_ctrl_cols = int_ctrl_c,
                                                    format_spec = format_s)
          )
        )

        # check that dfs are identical
        expect_identical(
          object = df_obj$df_oid |>
            dplyr::select(
              order(colnames(df_obj$df_oid))
            ),
          expected = df_rand$long$df_bottom$df_oid
        )

        expect_identical(
          object = df_obj$df_int_ctrl |>
            dplyr::select(
              order(colnames(df_obj$df_int_ctrl))
            ),
          expected = df_rand$long$df_bottom$df_int_ctrl
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_bottom - works - additonal all-NA columns",
  {
    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_t <- "NPX"

        df_rand <- olink_wide_synthetic(olink_platform = "Target 48",
                                        data_type = data_t,
                                        n_panels = 1L,
                                        n_assays = 45L,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # extract col_split and assa_cols
        col_s <- df_rand$long$df_top$df_plate |>
          dplyr::mutate(
            col_index_n = stringr::str_sub(string = .data[["col_index"]],
                                           start = 2L) |>
              as.integer()
          ) |>
          dplyr::arrange(.data[["col_index_n"]]) |>
          dplyr::slice_head(n = 1L) |>
          dplyr::pull(.data[["col_index"]])
        assay_c <- df_rand$long$df_top$df_oid$col_index

        # add new column with all NA
        df_rand$wide$df_bottom <- df_rand$wide$df_bottom |>
          dplyr::mutate(
            {{col_s}} := rep(x = NA_character_,
                             times = nrow(df_rand$wide$df_bottom))
          )

        # write empty-ish file
        writeLines("foo", wide_excel)

        # check that function runs with error
        expect_no_condition(
          object = df_obj <- read_npx_wide_bottom(df = df_rand$wide$df_bottom,
                                                  file = wide_excel,
                                                  col_split = col_s,
                                                  assay_cols = assay_c,
                                                  int_ctrl_cols = NULL,
                                                  format_spec = format_s)
        )

        # check that dfs are identical
        expect_identical(
          object = df_obj$df_oid |>
            dplyr::select(
              order(colnames(df_obj$df_oid))
            ),
          expected = df_rand$long$df_bottom$df_oid
        )

        expect_false(object = "df_int_ctrl" %in% names(df_obj))

      }
    )

  }
)

test_that(
  "read_npx_wide_bottom - error - unexpected values in V1",
  {
    ## NPX v1 ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_t <- "NPX"

        df_rand <- olink_wide_synthetic(olink_platform = "Target 48",
                                        data_type = data_t,
                                        n_panels = 1L,
                                        n_assays = 45L,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # extract col_split and assa_cols
        col_s <- df_rand$long$df_top$df_plate |>
          dplyr::mutate(
            col_index_n = stringr::str_sub(string = .data[["col_index"]],
                                           start = 2L) |>
              as.integer()
          ) |>
          dplyr::arrange(.data[["col_index_n"]]) |>
          dplyr::slice_head(n = 1L) |>
          dplyr::pull(.data[["col_index"]])
        assay_c <- df_rand$long$df_top$df_oid$col_index

        # modify df with wrong V1
        df_rand$wide$df_bottom <- df_rand$wide$df_bottom |>
          dplyr::mutate(
            V1 = dplyr::if_else(.data[["V1"]] == "LOD",
                                "LOD2",
                                .data[["V1"]])
          )

        # write empty-ish file
        writeLines("foo", wide_excel)

        # check that function runs with error
        expect_error(
          object = read_npx_wide_bottom(df = df_rand$wide$df_bottom,
                                        file = wide_excel,
                                        col_split = col_s,
                                        assay_cols = assay_c,
                                        int_ctrl_cols = NULL,
                                        format_spec = format_s),
          regexp = "Column 1 of the bottom matrix with assay metadata in file"
        )

      }
    )

    ## NPX v2 ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_t <- "NPX"

        df_rand <- olink_wide_synthetic(olink_platform = "Target 48",
                                        data_type = "Quantified",
                                        n_panels = 1L,
                                        n_assays = 45L,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # extract col_split and assa_cols
        col_s <- df_rand$long$df_top$df_plate |>
          dplyr::mutate(
            col_index_n = stringr::str_sub(string = .data[["col_index"]],
                                           start = 2L) |>
              as.integer()
          ) |>
          dplyr::arrange(.data[["col_index_n"]]) |>
          dplyr::slice_head(n = 1L) |>
          dplyr::pull(.data[["col_index"]])
        assay_c <- df_rand$long$df_top$df_oid$col_index

        # write empty-ish file
        writeLines("foo", wide_excel)

        # check that function runs with error
        expect_error(
          object = read_npx_wide_bottom(df = df_rand$wide$df_bottom,
                                        file = wide_excel,
                                        col_split = col_s,
                                        assay_cols = assay_c,
                                        int_ctrl_cols = NULL,
                                        format_spec = format_s),
          regexp = "Column 1 of the bottom matrix with assay metadata in file"
        )

      }
    )

    ## Quantified v1 ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_t <- "Quantified"

        df_rand <- olink_wide_synthetic(olink_platform = "Target 48",
                                        data_type = data_t,
                                        n_panels = 1L,
                                        n_assays = 45L,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # extract col_split and assa_cols
        col_s <- df_rand$long$df_top$df_plate |>
          dplyr::mutate(
            col_index_n = stringr::str_sub(string = .data[["col_index"]],
                                           start = 2L) |>
              as.integer()
          ) |>
          dplyr::arrange(.data[["col_index_n"]]) |>
          dplyr::slice_head(n = 1L) |>
          dplyr::pull(.data[["col_index"]])
        assay_c <- df_rand$long$df_top$df_oid$col_index

        # modify df with wrong V1
        df_rand$wide$df_bottom <- df_rand$wide$df_bottom |>
          dplyr::mutate(
            V1 = dplyr::if_else(.data[["V1"]] == "Assay warning",
                                "I_am_Unexpected",
                                .data[["V1"]])
          )

        # write empty-ish file
        writeLines("foo", wide_excel)

        # check that function runs with error
        expect_error(
          object = read_npx_wide_bottom(df = df_rand$wide$df_bottom,
                                        file = wide_excel,
                                        col_split = col_s,
                                        assay_cols = assay_c,
                                        int_ctrl_cols = NULL,
                                        format_spec = format_s),
          regexp = "Column 1 of the bottom matrix with assay metadata in file"
        )

      }
    )

    ## Quantified v2 ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_t <- "Quantified"

        df_rand <- olink_wide_synthetic(olink_platform = "Target 48",
                                        data_type = "NPX",
                                        n_panels = 1L,
                                        n_assays = 45L,
                                        n_samples = 88L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # extract col_split and assa_cols
        col_s <- df_rand$long$df_top$df_plate |>
          dplyr::mutate(
            col_index_n = stringr::str_sub(string = .data[["col_index"]],
                                           start = 2L) |>
              as.integer()
          ) |>
          dplyr::arrange(.data[["col_index_n"]]) |>
          dplyr::slice_head(n = 1L) |>
          dplyr::pull(.data[["col_index"]])
        assay_c <- df_rand$long$df_top$df_oid$col_index

        # write empty-ish file
        writeLines("foo", wide_excel)

        # check that function runs with error
        expect_error(
          object = read_npx_wide_bottom(df = df_rand$wide$df_bottom,
                                        file = wide_excel,
                                        col_split = col_s,
                                        assay_cols = assay_c,
                                        int_ctrl_cols = NULL,
                                        format_spec = format_s),
          regexp = "Column 1 of the bottom matrix with assay metadata in file"
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_bottom - error - incorrect # of plates x QC data",
  {
    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_t <- "Quantified"

        df_rand <- olink_wide_synthetic(olink_platform = "Target 48",
                                        data_type = data_t,
                                        n_panels = 1L,
                                        n_assays = 45L,
                                        n_samples = 100L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # extract col_split and assa_cols
        col_s <- df_rand$long$df_top$df_plate |>
          dplyr::mutate(
            col_index_n = stringr::str_sub(string = .data[["col_index"]],
                                           start = 2L) |>
              as.integer()
          ) |>
          dplyr::arrange(.data[["col_index_n"]]) |>
          dplyr::slice_head(n = 1L) |>
          dplyr::pull(.data[["col_index"]])
        assay_c <- df_rand$long$df_top$df_oid$col_index

        # remove one row with plate into from bottom row
        df_rand$wide$df_bottom <- df_rand$wide$df_bottom |>
          dplyr::filter(
            !(.data[["V1"]] == "Plate LOD"
              & .data[[col_s]] == "Plate2")
          )

        # write empty-ish file
        writeLines("foo", wide_excel)

        # check that function runs with error
        expect_error(
          object = read_npx_wide_bottom(df = df_rand$wide$df_bottom,
                                        file = wide_excel,
                                        col_split = col_s,
                                        assay_cols = assay_c,
                                        int_ctrl_cols = NULL,
                                        format_spec = format_s),
          regexp = "Column 1 of the bottom matrix contains uneven rows of plate"
        )

      }
    )

  }
)

# Test read_npx_wide_middle ----

test_that(
  "read_npx_wide_middle - works",
  {
    # NPX 1 panel ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_t <- "NPX"

        df_rand <- olink_wide_synthetic(olink_platform = "Target 48",
                                        data_type = data_t,
                                        n_panels = 1L,
                                        n_assays = 45L,
                                        n_samples = 200L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # get column names
        assay_c <- df_rand$long$df_top$df_oid$col_index
        plate_c <- df_rand$long$df_top$df_plate$col_index
        qc_warn_c <- df_rand$long$df_top$df_qc_warn$col_index

        # write empty-ish file
        writeLines("foo", wide_excel)

        # check that function runs
        expect_no_condition(
          object = l_obj <- read_npx_wide_middle(df = df_rand$wide$df_middle,
                                                 file = wide_excel,
                                                 data_type = data_t,
                                                 assay_cols = assay_c,
                                                 plate_cols = plate_c,
                                                 qc_warn_cols = qc_warn_c,
                                                 int_ctrl_cols = NULL)
        )

        # check that dfs are identical
        expect_identical(
          object = l_obj$df_oid,
          expected = df_rand$long$df_middle$df_oid
        )

        expect_identical(
          object = l_obj$df_plate,
          expected = df_rand$long$df_middle$df_plate
        )

        expect_identical(
          object = l_obj$df_qc_warn,
          expected = df_rand$long$df_middle$df_qc_warn
        )

        expect_false(object = "df_int_ctrl" %in% names(l_obj$df_plate))

      }
    )

    # NPX multi-panel ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_t <- "NPX"

        df_rand <- olink_wide_synthetic(olink_platform = "Target 48",
                                        data_type = data_t,
                                        n_panels = 4L,
                                        n_assays = 45L,
                                        n_samples = 200L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # get column names
        assay_c <- df_rand$long$df_top$df_oid$col_index
        plate_c <- df_rand$long$df_top$df_plate$col_index
        qc_warn_c <- df_rand$long$df_top$df_qc_warn$col_index

        # write empty-ish file
        writeLines("foo", wide_excel)

        # check that function runs
        expect_no_condition(
          object = l_obj <- read_npx_wide_middle(df = df_rand$wide$df_middle,
                                                 file = wide_excel,
                                                 data_type = data_t,
                                                 assay_cols = assay_c,
                                                 plate_cols = plate_c,
                                                 qc_warn_cols = qc_warn_c,
                                                 int_ctrl_cols = NULL)
        )

        # check that dfs are identical
        expect_identical(
          object = l_obj$df_oid,
          expected = df_rand$long$df_middle$df_oid
        )

        expect_identical(
          object = l_obj$df_plate,
          expected = df_rand$long$df_middle$df_plate
        )

        expect_identical(
          object = l_obj$df_qc_warn,
          expected = df_rand$long$df_middle$df_qc_warn
        )

        expect_false(object = "df_int_ctrl" %in% names(l_obj$df_plate))

      }
    )

    # Ct 1 panel ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_t <- "Ct"

        df_rand <- olink_wide_synthetic(olink_platform = "Target 48",
                                        data_type = data_t,
                                        n_panels = 1L,
                                        n_assays = 45L,
                                        n_samples = 200L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # get column names
        assay_c <- df_rand$long$df_top$df_oid$col_index
        plate_c <- df_rand$long$df_top$df_plate$col_index

        # write empty-ish file
        writeLines("foo", wide_excel)

        # check that function runs
        expect_no_condition(
          object = l_obj <- read_npx_wide_middle(df = df_rand$wide$df_middle,
                                                 file = wide_excel,
                                                 data_type = data_t,
                                                 assay_cols = assay_c,
                                                 plate_cols = plate_c,
                                                 qc_warn_cols = NULL,
                                                 int_ctrl_cols = NULL)
        )

        # check that dfs are identical
        expect_identical(
          object = l_obj$df_oid,
          expected = df_rand$long$df_middle$df_oid
        )

        expect_identical(
          object = l_obj$df_plate,
          expected = df_rand$long$df_middle$df_plate
        )

        expect_false(object = "df_qc_warn" %in% names(l_obj$df_plate))

        expect_false(object = "df_int_ctrl" %in% names(l_obj$df_plate))

      }
    )

    # Ct multi-panel ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_t <- "Ct"

        df_rand <- olink_wide_synthetic(olink_platform = "Target 48",
                                        data_type = data_t,
                                        n_panels = 4L,
                                        n_assays = 45L,
                                        n_samples = 200L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # get column names
        assay_c <- df_rand$long$df_top$df_oid$col_index
        plate_c <- df_rand$long$df_top$df_plate$col_index

        # write empty-ish file
        writeLines("foo", wide_excel)

        # check that function runs
        expect_no_condition(
          object = l_obj <- read_npx_wide_middle(df = df_rand$wide$df_middle,
                                                 file = wide_excel,
                                                 data_type = data_t,
                                                 assay_cols = assay_c,
                                                 plate_cols = plate_c,
                                                 qc_warn_cols = NULL,
                                                 int_ctrl_cols = NULL)
        )

        # check that dfs are identical
        expect_identical(
          object = l_obj$df_oid,
          expected = df_rand$long$df_middle$df_oid
        )

        expect_identical(
          object = l_obj$df_plate,
          expected = df_rand$long$df_middle$df_plate
        )

        expect_false(object = "df_qc_warn" %in% names(l_obj$df_plate))

        expect_false(object = "df_int_ctrl" %in% names(l_obj$df_plate))

      }
    )

    # Quantified 1 panel ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_t <- "Quantified"

        df_rand <- olink_wide_synthetic(olink_platform = "Target 48",
                                        data_type = data_t,
                                        n_panels = 1L,
                                        n_assays = 45L,
                                        n_samples = 200L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # get column names
        assay_c <- df_rand$long$df_top$df_oid$col_index
        plate_c <- df_rand$long$df_top$df_plate$col_index
        qc_warn_c <- df_rand$long$df_top$df_qc_warn$col_index
        int_ctrl_cols <- df_rand$long$df_top$df_int_ctrl$col_index

        # write empty-ish file
        writeLines("foo", wide_excel)

        # check that function runs
        expect_no_condition(
          object = l_obj <- read_npx_wide_middle(df = df_rand$wide$df_middle,
                                                 file = wide_excel,
                                                 data_type = data_t,
                                                 assay_cols = assay_c,
                                                 plate_cols = plate_c,
                                                 qc_warn_cols = qc_warn_c,
                                                 int_ctrl_cols = int_ctrl_cols)
        )

        # check that dfs are identical
        expect_identical(
          object = l_obj$df_oid,
          expected = df_rand$long$df_middle$df_oid
        )

        expect_identical(
          object = l_obj$df_plate,
          expected = df_rand$long$df_middle$df_plate
        )

        expect_identical(
          object = l_obj$df_qc_warn,
          expected = df_rand$long$df_middle$df_qc_warn
        )

        expect_identical(
          object = l_obj$df_int_ctrl,
          expected = df_rand$long$df_middle$df_int_ctrl
        )

      }
    )

    # Quantified multi-panel ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_t <- "Quantified"

        df_rand <- olink_wide_synthetic(olink_platform = "Target 48",
                                        data_type = data_t,
                                        n_panels = 4L,
                                        n_assays = 45L,
                                        n_samples = 200L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # get column names
        assay_c <- df_rand$long$df_top$df_oid$col_index
        plate_c <- df_rand$long$df_top$df_plate$col_index
        qc_warn_c <- df_rand$long$df_top$df_qc_warn$col_index
        int_ctrl_cols <- df_rand$long$df_top$df_int_ctrl$col_index

        # write empty-ish file
        writeLines("foo", wide_excel)

        # check that function runs
        expect_no_condition(
          object = l_obj <- read_npx_wide_middle(df = df_rand$wide$df_middle,
                                                 file = wide_excel,
                                                 data_type = data_t,
                                                 assay_cols = assay_c,
                                                 plate_cols = plate_c,
                                                 qc_warn_cols = qc_warn_c,
                                                 int_ctrl_cols = int_ctrl_cols)
        )

        # check that dfs are identical
        expect_identical(
          object = l_obj$df_oid,
          expected = df_rand$long$df_middle$df_oid
        )

        expect_identical(
          object = l_obj$df_plate,
          expected = df_rand$long$df_middle$df_plate
        )

        expect_identical(
          object = l_obj$df_qc_warn,
          expected = df_rand$long$df_middle$df_qc_warn
        )

        expect_identical(
          object = l_obj$df_int_ctrl,
          expected = df_rand$long$df_middle$df_int_ctrl
        )

      }
    )

    # Quantified multi-panel shuffled ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_t <- "Quantified"

        df_rand <- olink_wide_synthetic(olink_platform = "Target 48",
                                        data_type = data_t,
                                        n_panels = 4L,
                                        n_assays = 45L,
                                        n_samples = 200L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = TRUE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # get column names
        assay_c <- df_rand$long$df_top$df_oid$col_index
        plate_c <- df_rand$long$df_top$df_plate$col_index
        qc_warn_c <- df_rand$long$df_top$df_qc_warn$col_index
        int_ctrl_cols <- df_rand$long$df_top$df_int_ctrl$col_index

        # write empty-ish file
        writeLines("foo", wide_excel)

        # check that function runs
        expect_no_condition(
          object = l_obj <- read_npx_wide_middle(df = df_rand$wide$df_middle,
                                                 file = wide_excel,
                                                 data_type = data_t,
                                                 assay_cols = assay_c,
                                                 plate_cols = plate_c,
                                                 qc_warn_cols = qc_warn_c,
                                                 int_ctrl_cols = int_ctrl_cols)
        )

        # check that dfs are identical
        expect_identical(
          object = l_obj$df_oid,
          expected = df_rand$long$df_middle$df_oid
        )

        expect_identical(
          object = l_obj$df_plate,
          expected = df_rand$long$df_middle$df_plate
        )

        expect_identical(
          object = l_obj$df_qc_warn,
          expected = df_rand$long$df_middle$df_qc_warn
        )

        expect_identical(
          object = l_obj$df_int_ctrl,
          expected = df_rand$long$df_middle$df_int_ctrl
        )

      }
    )
  }
)

test_that(
  "read_npx_wide_middle - error - non-unique sample id",
  {
    # 1 duplicate ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_t <- "NPX"

        df_rand <- olink_wide_synthetic(olink_platform = "Target 48",
                                        data_type = data_t,
                                        n_panels = 2L,
                                        n_assays = 45L,
                                        n_samples = 101L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # get column names
        assay_c <- df_rand$long$df_top$df_oid$col_index
        plate_c <- df_rand$long$df_top$df_plate$col_index
        qc_warn_c <- df_rand$long$df_top$df_qc_warn$col_index

        # write empty-ish file
        writeLines("foo", wide_excel)

        # introduce duplicate sample id for the test
        df_rand$wide$df_middle <- df_rand$wide$df_middle |>
          dplyr::mutate(
            V1 = dplyr::if_else(.data[["V1"]] == "S2",
                                "S1",
                                .data[["V1"]])
          )

        # check that function runs with error
        expect_error(
          object = read_npx_wide_middle(df = df_rand$wide$df_middle,
                                        file = wide_excel,
                                        data_type = data_t,
                                        assay_cols = assay_c,
                                        plate_cols = plate_c,
                                        qc_warn_cols = qc_warn_c,
                                        int_ctrl_cols = NULL),
          regexp = "does not contain unique sample identifiers."
        )

      }
    )

    # 3 duplicates ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_t <- "NPX"

        df_rand <- olink_wide_synthetic(olink_platform = "Target 48",
                                        data_type = data_t,
                                        n_panels = 2L,
                                        n_assays = 45L,
                                        n_samples = 101L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # get column names
        assay_c <- df_rand$long$df_top$df_oid$col_index
        plate_c <- df_rand$long$df_top$df_plate$col_index
        qc_warn_c <- df_rand$long$df_top$df_qc_warn$col_index

        # write empty-ish file
        writeLines("foo", wide_excel)

        # introduce duplicate sample id for the test
        df_rand$wide$df_middle <- df_rand$wide$df_middle |>
          dplyr::mutate(
            V1 = dplyr::case_when(.data[["V1"]] %in% c("S2", "S3") ~ "S1",
                                  .data[["V1"]] %in% c("S4", "S5") ~ "S2",
                                  TRUE ~ .data[["V1"]],
                                  .default = .data[["V1"]])
          )

        # check that function runs with error
        expect_error(
          object = read_npx_wide_middle(df = df_rand$wide$df_middle,
                                        file = wide_excel,
                                        data_type = data_t,
                                        assay_cols = assay_c,
                                        plate_cols = plate_c,
                                        qc_warn_cols = qc_warn_c,
                                        int_ctrl_cols = NULL),
          regexp = "does not contain unique sample identifiers."
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_middle - error - uneven number of platid and qc_warning",
  {
    # Missing plateid column ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_t <- "NPX"

        df_rand <- olink_wide_synthetic(olink_platform = "Target 48",
                                        data_type = data_t,
                                        n_panels = 2L,
                                        n_assays = 45L,
                                        n_samples = 101L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # get column names
        assay_c <- df_rand$long$df_top$df_oid$col_index
        plate_c <- df_rand$long$df_top$df_plate$col_index
        qc_warn_c <- df_rand$long$df_top$df_qc_warn$col_index

        # write empty-ish file
        writeLines("foo", wide_excel)

        # remove one column with "Plate ID"
        df_rand$wide$df_middle <- df_rand$wide$df_middle |>
          dplyr::select(
            -dplyr::all_of(plate_c[1])
          )
        plate_c <- plate_c[plate_c != plate_c[1]]

        # check that function runs with error
        expect_error(
          object = read_npx_wide_middle(df = df_rand$wide$df_middle,
                                        file = wide_excel,
                                        data_type = data_t,
                                        assay_cols = assay_c,
                                        plate_cols = plate_c,
                                        qc_warn_cols = qc_warn_c,
                                        int_ctrl_cols = NULL),
          regexp = "Uneven number of entries of \"Plate ID\" and \"QC Warning\""
        )

      }
    )

    # Missing qc warning column ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_t <- "NPX"

        df_rand <- olink_wide_synthetic(olink_platform = "Target 48",
                                        data_type = data_t,
                                        n_panels = 2L,
                                        n_assays = 45L,
                                        n_samples = 101L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # get column names
        assay_c <- df_rand$long$df_top$df_oid$col_index
        plate_c <- df_rand$long$df_top$df_plate$col_index
        qc_warn_c <- df_rand$long$df_top$df_qc_warn$col_index

        # write empty-ish file
        writeLines("foo", wide_excel)

        # remove one column with "Plate ID"
        df_rand$wide$df_middle <- df_rand$wide$df_middle |>
          dplyr::select(
            -dplyr::all_of(qc_warn_c[1])
          )
        qc_warn_c <- qc_warn_c[qc_warn_c != qc_warn_c[1]]

        # check that function runs with error
        expect_error(
          object = read_npx_wide_middle(df = df_rand$wide$df_middle,
                                        file = wide_excel,
                                        data_type = data_t,
                                        assay_cols = assay_c,
                                        plate_cols = plate_c,
                                        qc_warn_cols = qc_warn_c,
                                        int_ctrl_cols = NULL),
          regexp = "Uneven number of entries of \"Plate ID\" and \"QC Warning\""
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_middle - works/error - uneven number of internal controls",
  {
    # Not shuffled internal controls ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_t <- "Quantified"

        df_rand <- olink_wide_synthetic(olink_platform = "Target 48",
                                        data_type = data_t,
                                        n_panels = 2L,
                                        n_assays = 45L,
                                        n_samples = 110L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # get column names
        assay_c <- df_rand$long$df_top$df_oid$col_index
        plate_c <- df_rand$long$df_top$df_plate$col_index
        qc_warn_c <- df_rand$long$df_top$df_qc_warn$col_index
        int_ctrl_cols <- df_rand$long$df_top$df_int_ctrl$col_index

        # write empty-ish file
        writeLines("foo", wide_excel)

        # remove one column with "Internal Control"
        df_rand$wide$df_middle <- df_rand$wide$df_middle |>
          dplyr::select(
            -dplyr::all_of(int_ctrl_cols[1])
          )
        int_ctrl_cols <- int_ctrl_cols[int_ctrl_cols != int_ctrl_cols[1]]

        # check that function runs with error
        expect_error(
          object = read_npx_wide_middle(df = df_rand$wide$df_middle,
                                        file = wide_excel,
                                        data_type = data_t,
                                        assay_cols = assay_c,
                                        plate_cols = plate_c,
                                        qc_warn_cols = qc_warn_c,
                                        int_ctrl_cols = int_ctrl_cols),
          regexp = "Uneven number of entries of \"Internal Control\" assays in"
        )

      }
    )

    # Shuffled internal controls ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_t <- "Quantified"

        df_rand <- olink_wide_synthetic(olink_platform = "Target 48",
                                        data_type = data_t,
                                        n_panels = 2L,
                                        n_assays = 45L,
                                        n_samples = 110L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = TRUE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # get column names
        assay_c <- df_rand$long$df_top$df_oid$col_index
        plate_c <- df_rand$long$df_top$df_plate$col_index
        qc_warn_c <- df_rand$long$df_top$df_qc_warn$col_index
        int_ctrl_cols <- df_rand$long$df_top$df_int_ctrl$col_index

        # write empty-ish file
        writeLines("foo", wide_excel)

        # remove one column with "Internal Control"
        df_rand$wide$df_middle <- df_rand$wide$df_middle |>
          dplyr::select(
            -dplyr::all_of(int_ctrl_cols[1])
          )
        int_ctrl_cols <- int_ctrl_cols[int_ctrl_cols != int_ctrl_cols[1]]

        # check that function runs with error
        expect_error(
          object = read_npx_wide_middle(df = df_rand$wide$df_middle,
                                        file = wide_excel,
                                        data_type = data_t,
                                        assay_cols = assay_c,
                                        plate_cols = plate_c,
                                        qc_warn_cols = qc_warn_c,
                                        int_ctrl_cols = int_ctrl_cols),
          regexp = "Uneven number of entries of \"Internal Control\" assays in"
        )

      }
    )

    # 2 internal controls - NO ERROR ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_t <- "Quantified"

        df_rand <- olink_wide_synthetic(olink_platform = "Target 48",
                                        data_type = data_t,
                                        n_panels = 2L,
                                        n_assays = 45L,
                                        n_samples = 110L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = TRUE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # get column names
        assay_c <- df_rand$long$df_top$df_oid$col_index
        plate_c <- df_rand$long$df_top$df_plate$col_index
        qc_warn_c <- df_rand$long$df_top$df_qc_warn$col_index
        int_ctrl_cols <- df_rand$long$df_top$df_int_ctrl$col_index

        # write empty-ish file
        writeLines("foo", wide_excel)

        # remove one column with "Internal Control"
        df_rand$wide$df_middle <- df_rand$wide$df_middle |>
          dplyr::select(
            -dplyr::all_of(int_ctrl_cols[1:2])
          )
        int_ctrl_cols <- int_ctrl_cols[!(int_ctrl_cols %in% int_ctrl_cols[1:2])]

        # check that function runs
        expect_no_condition(
          object = l_obj <- read_npx_wide_middle(df = df_rand$wide$df_middle,
                                                 file = wide_excel,
                                                 data_type = data_t,
                                                 assay_cols = assay_c,
                                                 plate_cols = plate_c,
                                                 qc_warn_cols = qc_warn_c,
                                                 int_ctrl_cols = int_ctrl_cols)
        )

      }
    )

    # 1 internal controls - NO ERROR ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_t <- "Quantified"

        df_rand <- olink_wide_synthetic(olink_platform = "Target 48",
                                        data_type = data_t,
                                        n_panels = 2L,
                                        n_assays = 45L,
                                        n_samples = 110L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = TRUE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # get column names
        assay_c <- df_rand$long$df_top$df_oid$col_index
        plate_c <- df_rand$long$df_top$df_plate$col_index
        qc_warn_c <- df_rand$long$df_top$df_qc_warn$col_index
        int_ctrl_cols <- df_rand$long$df_top$df_int_ctrl$col_index

        # write empty-ish file
        writeLines("foo", wide_excel)

        # remove one column with "Internal Control"
        df_rand$wide$df_middle <- df_rand$wide$df_middle |>
          dplyr::select(
            -dplyr::all_of(int_ctrl_cols[1:4])
          )
        int_ctrl_cols <- int_ctrl_cols[!(int_ctrl_cols %in% int_ctrl_cols[1:4])]

        # check that function runs
        expect_no_condition(
          object = l_obj <- read_npx_wide_middle(df = df_rand$wide$df_middle,
                                                 file = wide_excel,
                                                 data_type = data_t,
                                                 assay_cols = assay_c,
                                                 plate_cols = plate_c,
                                                 qc_warn_cols = qc_warn_c,
                                                 int_ctrl_cols = int_ctrl_cols)
        )

      }
    )

  }
)

# Test read_npx_wide_long_assay_quant ----

test_that(
  "read_npx_wide_long_assay_quant - error - non-identical col_index",
  {
    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # write empty-ish file
        writeLines("foo", wide_excel)

        # synthetic wide df
        data_t <- "NPX"

        df_rand <- olink_wide_synthetic(olink_platform = "Target 48",
                                        data_type = data_t,
                                        n_panels = 2L,
                                        n_assays = 45L,
                                        n_samples = 101L,
                                        show_int_ctrl = TRUE,
                                        loc_int_ctrl = "V3",
                                        shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # modify df_rand$long$df_middle$df_oid to reproduce error
        df_rand$long$df_middle$df_oid <- df_rand$long$df_middle$df_oid |>
          dplyr::filter(
            .data[["col_index"]] != "V2"
          )

        # check that function runs with error
        expect_error(
          object = read_npx_wide_long_assay_quant(
            df_top_oid = df_rand$long$df_top$df_oid,
            df_mid_quant = df_rand$long$df_middle$df_oid,
            file = wide_excel,
            format_spec = format_s
          ),
          regexp = "Column \"col_index\" of the top matrix with assay metadata"
        )

      }
    )

  }
)
