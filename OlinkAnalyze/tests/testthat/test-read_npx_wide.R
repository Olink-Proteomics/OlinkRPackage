# Test read_npx_wide_split_row ----

test_that(
  "read_npx_wide_split_row - works",
  {
    # variables that apply to all tests
    olink_platform <- "Target 48"
    n_panels <- 3L
    n_assays <- 45L
    n_samples <- 88L
    show_int_ctrl <- TRUE

    ## NPX ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"
        show_dev_int_ctrl <- TRUE
        version <- 1L

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = show_int_ctrl,
          version = version
        )

        # write wide df
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_error(
          expect_no_warning(
            object = df_out <- read_npx_wide_split_row(
              df = df_rand$list_df_wide$df_wide,
              file = olink_wide_format,
              data_type = data_type,
              format_spec = get_format_spec(data_type = data_type)
            )
          )
        )

        # check that df_head works
        expect_identical(
          object = remove_all_na_cols(df = df_out$df_head),
          expected = df_rand$list_df_wide$df_head_wide
        )

        # check that df_top works
        expect_identical(
          object = df_out$df_top,
          expected = df_rand$list_df_wide$df_top_wide
        )

        # check that df_mid works
        expect_identical(
          object = df_out$df_mid,
          expected = df_rand$list_df_wide$df_middle_wide
        )

        # check that df_bottom works
        expect_identical(
          object = remove_all_na_cols(df = df_out$df_bottom),
          expected = df_rand$list_df_wide$df_bottom_wide
        )

      }
    )

    ## Ct ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Ct"
        show_dev_int_ctrl <- FALSE
        version <- 0L

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = show_int_ctrl,
          version = version
        )

        # write wide df
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_error(
          expect_no_warning(
            object = df_out <- read_npx_wide_split_row(
              df = df_rand$list_df_wide$df_wide,
              file = olink_wide_format,
              data_type = data_type,
              format_spec = get_format_spec(data_type = data_type)
            )
          )
        )

        # check that df_head works
        expect_identical(
          object = remove_all_na_cols(df = df_out$df_head),
          expected = df_rand$list_df_wide$df_head_wide
        )

        # check that df_top works
        expect_identical(
          object = df_out$df_top,
          expected = df_rand$list_df_wide$df_top_wide
        )

        # check that df_mid works
        expect_identical(
          object = df_out$df_mid,
          expected = df_rand$list_df_wide$df_middle_wide
        )

        # check that df_bottom works
        expect_true(object = is.null(df_out$df_bottom))

      }
    )

    ## Quantified ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Quantified"
        show_dev_int_ctrl <- TRUE
        version <- 0L

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = show_int_ctrl,
          version = version
        )

        # write wide df
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_error(
          expect_no_warning(
            object = df_out <- read_npx_wide_split_row(
              df = df_rand$list_df_wide$df_wide,
              file = olink_wide_format,
              data_type = data_type,
              format_spec = get_format_spec(data_type = data_type)
            )
          )
        )

        # check that df_head works
        expect_identical(
          object = remove_all_na_cols(df = df_out$df_head),
          expected = df_rand$list_df_wide$df_head_wide
        )

        # check that df_top works
        expect_identical(
          object = df_out$df_top,
          expected = df_rand$list_df_wide$df_top_wide
        )

        # check that df_mid works
        expect_identical(
          object = df_out$df_mid,
          expected = df_rand$list_df_wide$df_middle_wide
        )

        # check that df_bottom works
        expect_identical(
          object = remove_all_na_cols(df = df_out$df_bottom),
          expected = df_rand$list_df_wide$df_bottom_wide
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_split_row - works - consecutive all-NA rows",
  {
    # variables that apply to all tests
    olink_platform <- "Target 96"
    n_panels <- 1L
    n_assays <- 92L
    n_samples <- 88L
    show_int_ctrl <- TRUE

    ## NPX ----

    data_type <- "NPX"
    show_dev_int_ctrl <- TRUE
    version <- 1L

    # get wide synthetic data
    df_rand_npx <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = n_panels,
      n_assays = n_assays,
      n_samples = n_samples,
      show_dev_int_ctrl = show_dev_int_ctrl,
      show_int_ctrl = show_int_ctrl,
      version = version
    )

    # matrix specifications
    format_spec_npx <- get_format_spec(data_type = data_type)

    ### NPX - 2 all-NA + 1 all-NA ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # modify wide df
        df_wide <- df_rand_npx$list_df_wide$df_head_wide |>
          dplyr::bind_rows(
            df_rand_npx$list_df_wide$df_top_wide
          ) |>
          dplyr::bind_rows(
            df_rand_npx$list_df_wide$df_na_wide
          ) |>
          dplyr::bind_rows(
            df_rand_npx$list_df_wide$df_na_wide
          ) |>
          dplyr::bind_rows(
            df_rand_npx$list_df_wide$df_middle_wide
          ) |>
          dplyr::bind_rows(
            df_rand_npx$list_df_wide$df_na_wide
          ) |>
          dplyr::bind_rows(
            df_rand_npx$list_df_wide$df_bottom_wide
          )

        # write wide df
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_error(
          expect_no_warning(
            object = df_out <- read_npx_wide_split_row(
              df = df_wide,
              file = olink_wide_format,
              data_type = data_type,
              format_spec = format_spec_npx
            )
          )
        )

        # check that df_head works
        expect_identical(
          object = remove_all_na_cols(df = df_out$df_head),
          expected = df_rand_npx$list_df_wide$df_head_wide
        )

        # check that df_top works
        expect_identical(
          object = df_out$df_top,
          expected = df_rand_npx$list_df_wide$df_top_wide
        )

        # check that df_mid works
        expect_identical(
          object = df_out$df_mid,
          expected = df_rand_npx$list_df_wide$df_middle_wide
        )

        # check that df_bottom works
        expect_identical(
          object = remove_all_na_cols(df = df_out$df_bottom),
          expected = df_rand_npx$list_df_wide$df_bottom_wide
        )

      }
    )

    ### NPX - 2 all-NA + 2 all-NA ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # modify wide df
        df_wide <- df_rand_npx$list_df_wide$df_head_wide |>
          dplyr::bind_rows(
            df_rand_npx$list_df_wide$df_top_wide
          ) |>
          dplyr::bind_rows(
            df_rand_npx$list_df_wide$df_na_wide
          ) |>
          dplyr::bind_rows(
            df_rand_npx$list_df_wide$df_na_wide
          ) |>
          dplyr::bind_rows(
            df_rand_npx$list_df_wide$df_middle_wide
          ) |>
          dplyr::bind_rows(
            df_rand_npx$list_df_wide$df_na_wide
          ) |>
          dplyr::bind_rows(
            df_rand_npx$list_df_wide$df_na_wide
          ) |>
          dplyr::bind_rows(
            df_rand_npx$list_df_wide$df_bottom_wide
          )

        # write wide df
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_error(
          expect_no_warning(
            object = df_out <- read_npx_wide_split_row(
              df = df_wide,
              file = olink_wide_format,
              data_type = data_type,
              format_spec = format_spec_npx
            )
          )
        )

        # check that df_head works
        expect_identical(
          object = remove_all_na_cols(df = df_out$df_head),
          expected = df_rand_npx$list_df_wide$df_head_wide
        )

        # check that df_top works
        expect_identical(
          object = df_out$df_top,
          expected = df_rand_npx$list_df_wide$df_top_wide
        )

        # check that df_mid works
        expect_identical(
          object = df_out$df_mid,
          expected = df_rand_npx$list_df_wide$df_middle_wide
        )

        # check that df_bottom works
        expect_identical(
          object = remove_all_na_cols(df = df_out$df_bottom),
          expected = df_rand_npx$list_df_wide$df_bottom_wide
        )

      }
    )


    ### NPX - 1 all-NA + 2 all-NA ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # modify wide df
        df_wide <- df_rand_npx$list_df_wide$df_head_wide |>
          dplyr::bind_rows(
            df_rand_npx$list_df_wide$df_top_wide
          ) |>
          dplyr::bind_rows(
            df_rand_npx$list_df_wide$df_na_wide
          ) |>
          dplyr::bind_rows(
            df_rand_npx$list_df_wide$df_middle_wide
          ) |>
          dplyr::bind_rows(
            df_rand_npx$list_df_wide$df_na_wide
          ) |>
          dplyr::bind_rows(
            df_rand_npx$list_df_wide$df_na_wide
          ) |>
          dplyr::bind_rows(
            df_rand_npx$list_df_wide$df_bottom_wide
          )

        # write wide df
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_error(
          expect_no_warning(
            object = df_out <- read_npx_wide_split_row(
              df = df_wide,
              file = olink_wide_format,
              data_type = data_type,
              format_spec = format_spec_npx
            )
          )
        )

        # check that df_head works
        expect_identical(
          object = remove_all_na_cols(df = df_out$df_head),
          expected = df_rand_npx$list_df_wide$df_head_wide
        )

        # check that df_top works
        expect_identical(
          object = df_out$df_top,
          expected = df_rand_npx$list_df_wide$df_top_wide
        )

        # check that df_mid works
        expect_identical(
          object = df_out$df_mid,
          expected = df_rand_npx$list_df_wide$df_middle_wide
        )

        # check that df_bottom works
        expect_identical(
          object = remove_all_na_cols(df = df_out$df_bottom),
          expected = df_rand_npx$list_df_wide$df_bottom_wide
        )

      }
    )

    ### NPX - 4 all-NA + 3 all-NA ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # modify wide df
        df_wide <- df_rand_npx$list_df_wide$df_head_wide |>
          dplyr::bind_rows(
            df_rand_npx$list_df_wide$df_top_wide
          ) |>
          dplyr::bind_rows(
            df_rand_npx$list_df_wide$df_na_wide
          ) |>
          dplyr::bind_rows(
            df_rand_npx$list_df_wide$df_na_wide
          ) |>
          dplyr::bind_rows(
            df_rand_npx$list_df_wide$df_na_wide
          ) |>
          dplyr::bind_rows(
            df_rand_npx$list_df_wide$df_na_wide
          ) |>
          dplyr::bind_rows(
            df_rand_npx$list_df_wide$df_middle_wide
          ) |>
          dplyr::bind_rows(
            df_rand_npx$list_df_wide$df_na_wide
          ) |>
          dplyr::bind_rows(
            df_rand_npx$list_df_wide$df_na_wide
          ) |>
          dplyr::bind_rows(
            df_rand_npx$list_df_wide$df_na_wide
          ) |>
          dplyr::bind_rows(
            df_rand_npx$list_df_wide$df_bottom_wide
          )

        # write wide df
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_error(
          expect_no_warning(
            object = df_out <- read_npx_wide_split_row(
              df = df_wide,
              file = olink_wide_format,
              data_type = data_type,
              format_spec = format_spec_npx
            )
          )
        )

        # check that df_head works
        expect_identical(
          object = remove_all_na_cols(df = df_out$df_head),
          expected = df_rand_npx$list_df_wide$df_head_wide
        )

        # check that df_top works
        expect_identical(
          object = df_out$df_top,
          expected = df_rand_npx$list_df_wide$df_top_wide
        )

        # check that df_mid works
        expect_identical(
          object = df_out$df_mid,
          expected = df_rand_npx$list_df_wide$df_middle_wide
        )

        # check that df_bottom works
        expect_identical(
          object = remove_all_na_cols(df = df_out$df_bottom),
          expected = df_rand_npx$list_df_wide$df_bottom_wide
        )

      }
    )

    ## Ct ----

    data_type <- "Ct"
    show_dev_int_ctrl <- FALSE
    version <- 0L

    # get wide synthetic data
    df_rand_ct <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = n_panels,
      n_assays = n_assays,
      n_samples = n_samples,
      show_dev_int_ctrl = show_dev_int_ctrl,
      show_int_ctrl = show_int_ctrl,
      version = version
    )

    # matrix specifications
    format_spec_ct <- get_format_spec(data_type = data_type)

    ### Ct - 2 all-NA ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # modify wide df
        df_wide <- df_rand_ct$list_df_wide$df_head_wide |>
          dplyr::bind_rows(
            df_rand_ct$list_df_wide$df_top_wide
          ) |>
          dplyr::bind_rows(
            df_rand_ct$list_df_wide$df_na_wide
          ) |>
          dplyr::bind_rows(
            df_rand_ct$list_df_wide$df_na_wide
          ) |>
          dplyr::bind_rows(
            df_rand_ct$list_df_wide$df_middle_wide
          )

        # write wide df
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_error(
          expect_no_warning(
            object = df_out <- read_npx_wide_split_row(
              df = df_wide,
              file = olink_wide_format,
              data_type = data_type,
              format_spec = format_spec_ct
            )
          )
        )

        # check that df_head works
        expect_identical(
          object = remove_all_na_cols(df = df_out$df_head),
          expected = df_rand_ct$list_df_wide$df_head_wide
        )

        # check that df_top works
        expect_identical(
          object = df_out$df_top,
          expected = df_rand_ct$list_df_wide$df_top_wide
        )

        # check that df_mid works
        expect_identical(
          object = df_out$df_mid,
          expected = df_rand_ct$list_df_wide$df_middle_wide
        )

        # check that df_bottom works
        expect_true(object = is.null(df_out$df_bottom))

      }
    )

    ### Ct - 4 all-NA ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # modify wide df
        df_wide <- df_rand_ct$list_df_wide$df_head_wide |>
          dplyr::bind_rows(
            df_rand_ct$list_df_wide$df_top_wide
          ) |>
          dplyr::bind_rows(
            df_rand_ct$list_df_wide$df_na_wide
          ) |>
          dplyr::bind_rows(
            df_rand_ct$list_df_wide$df_na_wide
          ) |>
          dplyr::bind_rows(
            df_rand_ct$list_df_wide$df_na_wide
          ) |>
          dplyr::bind_rows(
            df_rand_ct$list_df_wide$df_na_wide
          ) |>
          dplyr::bind_rows(
            df_rand_ct$list_df_wide$df_middle_wide
          )

        # write wide df
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_error(
          expect_no_warning(
            object = df_out <- read_npx_wide_split_row(
              df = df_wide,
              file = olink_wide_format,
              data_type = data_type,
              format_spec = format_spec_ct
            )
          )
        )

        # check that df_head works
        expect_identical(
          object = remove_all_na_cols(df = df_out$df_head),
          expected = df_rand_ct$list_df_wide$df_head_wide
        )

        # check that df_top works
        expect_identical(
          object = df_out$df_top,
          expected = df_rand_ct$list_df_wide$df_top_wide
        )

        # check that df_mid works
        expect_identical(
          object = df_out$df_mid,
          expected = df_rand_ct$list_df_wide$df_middle_wide
        )

        # check that df_bottom works
        expect_true(object = is.null(df_out$df_bottom))

      }
    )

  }
)

test_that(
  "read_npx_wide_split_row - error - no or too many all-NA rows",
  {
    # variables that apply to all tests
    olink_platform <- "Target 48"
    data_type <- "NPX"
    n_panels <- 3L
    n_assays <- 45L
    n_samples <- 88L
    show_dev_int_ctrl <- TRUE
    show_int_ctrl <- TRUE
    version <- 1L

    # get wide synthetic data
    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = n_panels,
      n_assays = n_assays,
      n_samples = n_samples,
      show_dev_int_ctrl = show_dev_int_ctrl,
      show_int_ctrl = show_int_ctrl,
      version = version
    )

    # matrix specifications
    format_spec <- get_format_spec(data_type = data_type)

    ## No all-NA rows ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # modify wide df
        df_rand$list_df_wide$df_wide <- df_rand$list_df_wide$df_wide |>
          dplyr::filter(
            !is.na(.data[["V1"]])
          )

        # write wide df
        writeLines("foo", olink_wide_format)

        # check that function runs with error
        expect_error(
          object = read_npx_wide_split_row(
            df = df_rand$list_df_wide$df_wide,
            file = olink_wide_format,
            data_type = data_type,
            format_spec = format_spec
          ),
          regexp = "We identified 0 rows with all columns `NA` in file"
        )
      }
    )

    ## Too many all-NA rows ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # modify and write wide df
        df_rand$list_df_wide$df_wide <- df_rand$list_df_wide$df_wide |>
          dplyr::mutate(
            V1 = dplyr::if_else(.data[["V1"]] == .env[["data_type"]],
                                NA_character_,
                                .data[["V1"]])
          )

        # write wide df
        writeLines("foo", olink_wide_format)

        # check that function runs with error
        expect_error(
          object = read_npx_wide_split_row(
            df = df_rand$list_df_wide$df_wide,
            file = olink_wide_format,
            data_type = data_type,
            format_spec = format_spec
          ),
          regexp = "We identified 3 rows with all columns `NA` in file"
        )
      }
    )

  }
)

test_that(
  "read_npx_wide_split_row - error - not as many all-NA rows as expected",
  {
    # variables that apply to all tests
    olink_platform <- "Target 48"
    n_panels <- 3L
    n_assays <- 45L
    n_samples <- 88L
    show_int_ctrl <- TRUE

    ## NPX ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = "Ct",
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = show_int_ctrl,
          version = 0L
        )

        # write wide df
        writeLines("foo", olink_wide_format)

        # check that function runs with error
        expect_error(
          object = read_npx_wide_split_row(
            df = df_rand$list_df_wide$df_wide,
            file = olink_wide_format,
            data_type = data_type,
            format_spec = get_format_spec(data_type = data_type)
          ),
          regexp = "We identified 1 rows with all columns `NA` in file"
        )

      }
    )

    ## Ct ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Ct"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = "NPX",
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = TRUE,
          show_int_ctrl = show_int_ctrl,
          version = 1L
        )

        # write wide df
        writeLines("foo", olink_wide_format)

        # check that function runs with error
        expect_error(
          object = read_npx_wide_split_row(
            df = df_rand$list_df_wide$df_wide,
            file = olink_wide_format,
            data_type = data_type,
            format_spec = get_format_spec(data_type = data_type)
          ),
          regexp = "We identified 2 rows with all columns `NA` in file"
        )

      }
    )

    ## Quantified ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Quantified"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = "Ct",
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = show_int_ctrl,
          version = 0L
        )

        # write wide df
        writeLines("foo", olink_wide_format)

        # check that function runs with error
        expect_error(
          object = read_npx_wide_split_row(
            df = df_rand$list_df_wide$df_wide,
            file = olink_wide_format,
            data_type = data_type,
            format_spec = get_format_spec(data_type = data_type)
          ),
          regexp = "We identified 1 rows with all columns `NA` in file"
        )

      }
    )

  }
)

# Test read_npx_wide_check_top ----

test_that(
  "read_npx_wide_check_top - works",
  {
    # variables that apply to all tests
    olink_platform <- "Target 48"
    n_panels <- 3L
    n_assays <- 45L
    n_samples <- 88L

    ## NPX no int ctrl & no dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = FALSE,
          version = 1L
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = read_npx_wide_check_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

      }
    )

    ## NPX with int ctrl & no dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = TRUE,
          version = 1L
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = read_npx_wide_check_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

      }
    )

    ## NPX with int ctrl & with dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = TRUE,
          show_int_ctrl = TRUE,
          version = 1L
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = read_npx_wide_check_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

      }
    )

    ## Ct no int ctrl & no dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Ct"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = FALSE,
          version = 0L
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = read_npx_wide_check_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

      }
    )

    ## Ct with int ctrl & no dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Ct"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = TRUE,
          version = 0L
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = read_npx_wide_check_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

      }
    )

    ## Quantified no int ctrl & no dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Quantified"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = FALSE,
          version = 0L
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = read_npx_wide_check_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

      }
    )

    ## Quantified with int ctrl & no dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Quantified"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = TRUE,
          version = 0L
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = read_npx_wide_check_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

      }
    )

    ## Quantified with int ctrl & no dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Quantified"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = TRUE,
          show_int_ctrl = TRUE,
          version = 0L
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = read_npx_wide_check_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_check_top - error - missing labels in V1",
  {
    # variables that apply to all tests
    olink_platform <- "Target 48"
    n_panels <- 1L
    n_assays <- 45L
    n_samples <- 88L
    show_dev_int_ctrl <- FALSE
    show_int_ctrl <- FALSE

    ## NPX/Ct ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = show_int_ctrl,
          version = 1L
        )

        # modify df_top
        df_rand$list_df_wide$df_top_wide <- df_rand$list_df_wide$df_top_wide |>
          dplyr::filter(
            .data[["V1"]] != "OlinkID"
          )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs with error
        expect_error(
          object = read_npx_wide_check_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            format_spec = get_format_spec(data_type = data_type)
          ),
          regexp = "Column 1 of the top matrix with assay data in file"
        )

      }
    )

    ## Quantified ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Quantified"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = show_int_ctrl,
          version = 0L
        )

        # modify df_top_wide
        df_top_add_row <- dplyr::tibble(
          "X" = c("Extra_Row",
                  rep(x = "A", times = (n_panels * n_assays)),
                  rep(x = NA_character_, times = n_panels),
                  rep(x = NA_character_, times = n_panels))
        ) |>
          t()
        rownames(df_top_add_row) <- NULL
        colnames(df_top_add_row) <- colnames(df_rand$list_df_wide$df_top_wide)
        df_top_add_row <- dplyr::as_tibble(df_top_add_row)

        df_rand$list_df_wide$df_top_wide <- df_rand$list_df_wide$df_top_wide |>
          dplyr::bind_rows(
            df_top_add_row
          )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs with error
        expect_error(
          object = read_npx_wide_check_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            format_spec = get_format_spec(data_type = data_type)
          ),
          regexp = "Column 1 of the top matrix with assay data in file"
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_check_top - error - missing labels in row 2 (Assay)",
  {
    # variables that apply to all tests
    olink_platform <- "Target 48"
    n_panels <- 1L
    n_assays <- 45L
    n_samples <- 88L
    show_dev_int_ctrl <- FALSE
    show_int_ctrl <- FALSE

    ## NPX ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = show_int_ctrl,
          version = 1L
        )

        # modify df_top_wide - remove QC Warning
        qc_warn_col <- colnames(df_rand$list_df_wide$df_top_wide) |> tail(1L)

        df_rand$list_df_wide$df_top_wide <- df_rand$list_df_wide$df_top_wide |>
          dplyr::select(
            -dplyr::all_of(qc_warn_col)
          )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs with error
        expect_error(
          object = read_npx_wide_check_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            format_spec = get_format_spec(data_type = data_type)
          ),
          regexp = "does not contain: QC Warning"
        )

      }
    )

    ## Ct ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Ct"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = show_int_ctrl,
          version = 0L
        )

        # modify df_top_wide - remove Plate ID
        plate_id_col <- colnames(df_rand$list_df_wide$df_top_wide) |> tail(1L)

        df_rand$list_df_wide$df_top_wide <- df_rand$list_df_wide$df_top_wide |>
          dplyr::select(
            -dplyr::all_of(plate_id_col)
          )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs with error
        expect_error(
          object = read_npx_wide_check_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            format_spec = get_format_spec(data_type = data_type)
          ),
          regexp = "does not contain: Plate ID"
        )

      }
    )

    ## Quantified ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Quantified"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = show_int_ctrl,
          version = 0L
        )

        # modify df_top_wide - remove Plate ID and QC Warning
        pid_qcw_col <- colnames(df_rand$list_df_wide$df_top_wide) |> tail(2L)

        df_rand$list_df_wide$df_top_wide <- df_rand$list_df_wide$df_top_wide |>
          dplyr::select(
            -dplyr::all_of(pid_qcw_col)
          )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs with error
        expect_error(
          object = read_npx_wide_check_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            format_spec = get_format_spec(data_type = data_type)
          ),
          regexp = "does not contain: Plate ID and QC Warning"
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_check_top - error - incorrect num of int ctrl",
  {
    # variables that apply to all tests
    olink_platform <- "Target 48"
    data_type <- "NPX"
    n_assays <- 45L
    n_samples <- 88L
    show_dev_int_ctrl <- FALSE
    show_int_ctrl <- TRUE
    version <- 1L

    # matrix specifications
    format_spec <- get_format_spec(data_type = data_type)

    ## 1 missing int ctrl in 1 panel ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = 1L,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = show_int_ctrl,
          version = version
        )

        # modify df_top
        remove_col <- df_rand$list_df_long$df_top_long$df_int_ctrl |>
          dplyr::sample_n(
            size = 1L
          ) |>
          dplyr::pull(
            .data[["col_index"]]
          )

        df_rand$list_df_wide$df_top_wide <- df_rand$list_df_wide$df_top_wide |>
          dplyr::select(
            -dplyr::all_of(remove_col)
          )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs with error
        expect_error(
          object = read_npx_wide_check_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            format_spec = format_spec
          ),
          regexp = "is missing one or more of the internal control"
        )

      }
    )

    ## 1 missing int ctrl 2 panels ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = 3L,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = show_int_ctrl,
          version = version
        )

        # modify df_top
        remove_col <- df_rand$list_df_long$df_top_long$df_int_ctrl |>
          dplyr::sample_n(
            size = 1L
          ) |>
          dplyr::pull(
            .data[["col_index"]]
          )

        df_rand$list_df_wide$df_top_wide <- df_rand$list_df_wide$df_top_wide |>
          dplyr::select(
            -dplyr::all_of(remove_col)
          )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs with error
        expect_error(
          object = read_npx_wide_check_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            format_spec = format_spec
          ),
          regexp = "is missing one or more of the internal control"
        )

      }
    )

    ## 4 missing int ctrl 3 panels ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = 3L,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = show_int_ctrl,
          version = version
        )

        # modify df_top
        remove_col <- df_rand$list_df_long$df_top_long$df_int_ctrl |>
          dplyr::sample_n(
            size = 4L
          ) |>
          dplyr::pull(
            .data[["col_index"]]
          )

        df_rand$list_df_wide$df_top_wide <- df_rand$list_df_wide$df_top_wide |>
          dplyr::select(
            -dplyr::all_of(remove_col)
          )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs with error
        expect_error(
          object = read_npx_wide_check_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            format_spec = format_spec
          ),
          regexp = "are missing one or more of the internal control"
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_check_top - error - incorrect num of dev int ctrl",
  {
    # variables that apply to all tests
    olink_platform <- "Target 48"
    data_type <- "NPX"
    n_assays <- 45L
    n_samples <- 88L
    show_dev_int_ctrl <- TRUE
    show_int_ctrl <- TRUE
    version <- 1L

    # matrix specifications
    format_spec <- get_format_spec(data_type = data_type)

    ## 1 missing int ctrl in 1 panel ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = 1L,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = show_int_ctrl,
          version = version
        )

        # modify df_top
        remove_col <- df_rand$list_df_long$df_top_long$df_dev_int_ctrl |>
          dplyr::sample_n(
            size = 1L
          ) |>
          dplyr::pull(
            .data[["col_index"]]
          )

        df_rand$list_df_wide$df_top_wide <- df_rand$list_df_wide$df_top_wide |>
          dplyr::select(
            -dplyr::all_of(remove_col)
          )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs with error
        expect_error(
          object = read_npx_wide_check_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            format_spec = format_spec
          ),
          regexp = "is missing one or more of the deviations"
        )

      }
    )

    ## 1 missing int ctrl 2 panels ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = 3L,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = show_int_ctrl,
          version = version
        )

        # modify df_top
        remove_col <- df_rand$list_df_long$df_top_long$df_dev_int_ctrl |>
          dplyr::sample_n(
            size = 1L
          ) |>
          dplyr::pull(
            .data[["col_index"]]
          )

        df_rand$list_df_wide$df_top_wide <- df_rand$list_df_wide$df_top_wide |>
          dplyr::select(
            -dplyr::all_of(remove_col)
          )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs with error
        expect_error(
          object = read_npx_wide_check_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            format_spec = format_spec
          ),
          regexp = "is missing one or more of the deviations"
        )

      }
    )

    ## 4 missing int ctrl 3 panels ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = 3L,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = show_int_ctrl,
          version = version
        )

        # modify df_top
        remove_col <- df_rand$list_df_long$df_top_long$df_dev_int_ctrl |>
          dplyr::sample_n(
            size = 4L
          ) |>
          dplyr::pull(
            .data[["col_index"]]
          )

        df_rand$list_df_wide$df_top_wide <- df_rand$list_df_wide$df_top_wide |>
          dplyr::select(
            -dplyr::all_of(remove_col)
          )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs with error
        expect_error(
          object = read_npx_wide_check_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            format_spec = format_spec
          ),
          regexp = "are missing one or more of the deviations"
        )

      }
    )

  }
)

# Test read_npx_wide_top ----

test_that(
  "read_npx_wide_top - works - T48 - single panel",
  {
    # variables that apply to all tests
    olink_platform <- "Target 48"
    n_panels <- 1L
    n_assays <- 45L
    n_samples <- 88L

    ## NPX no int ctrl and no dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = FALSE,
          version = 1L
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_top_oid,
          expected = df_rand$list_df_long$df_top_long$df_oid
        )

        expect_identical(
          object = df_out$df_top_plate,
          expected = df_rand$list_df_long$df_top_long$df_plate
        )

        expect_identical(
          object = df_out$df_top_qc_warn,
          expected = df_rand$list_df_long$df_top_long$df_qc_warn
        )

        expect_false(
          object = "df_top_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_int_ctrl) == 0L
        )

        expect_false(
          object = "df_top_dev_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_dev_int_ctrl) == 0L
        )

      }
    )

    ## NPX with int ctrl and no dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = TRUE,
          version = 1L
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_top_oid,
          expected = df_rand$list_df_long$df_top_long$df_oid
        )

        expect_identical(
          object = df_out$df_top_plate,
          expected = df_rand$list_df_long$df_top_long$df_plate
        )

        expect_identical(
          object = df_out$df_top_qc_warn,
          expected = df_rand$list_df_long$df_top_long$df_qc_warn
        )

        expect_identical(
          object = df_out$df_top_int_ctrl,
          expected = df_rand$list_df_long$df_top_long$df_int_ctrl
        )

        expect_false(
          object = "df_top_dev_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_dev_int_ctrl) == 0L
        )

      }
    )

    ## NPX no int ctrl and with dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = TRUE,
          show_int_ctrl = FALSE,
          version = 1L
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_top_oid,
          expected = df_rand$list_df_long$df_top_long$df_oid
        )

        expect_identical(
          object = df_out$df_top_plate,
          expected = df_rand$list_df_long$df_top_long$df_plate
        )

        expect_identical(
          object = df_out$df_top_qc_warn,
          expected = df_rand$list_df_long$df_top_long$df_qc_warn
        )

        expect_false(
          object = "df_top_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_int_ctrl) == 0L
        )

        expect_identical(
          object = df_out$df_top_dev_int_ctrl,
          expected = df_rand$list_df_long$df_top_long$df_dev_int_ctrl
        )

      }
    )

    ## NPX with int ctrl and with dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = TRUE,
          show_int_ctrl = TRUE,
          version = 1L
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_top_oid,
          expected = df_rand$list_df_long$df_top_long$df_oid
        )

        expect_identical(
          object = df_out$df_top_plate,
          expected = df_rand$list_df_long$df_top_long$df_plate
        )

        expect_identical(
          object = df_out$df_top_qc_warn,
          expected = df_rand$list_df_long$df_top_long$df_qc_warn
        )

        expect_identical(
          object = df_out$df_top_int_ctrl,
          expected = df_rand$list_df_long$df_top_long$df_int_ctrl
        )

        expect_identical(
          object = df_out$df_top_dev_int_ctrl,
          expected = df_rand$list_df_long$df_top_long$df_dev_int_ctrl
        )

      }
    )


    ## Ct no int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Ct"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = FALSE,
          version = 0L
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_top_oid,
          expected = df_rand$list_df_long$df_top_long$df_oid
        )

        expect_identical(
          object = df_out$df_top_plate,
          expected = df_rand$list_df_long$df_top_long$df_plate
        )

        expect_false(
          object = "df_top_qc_warn" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_qc_warn) == 0L
        )

        expect_false(
          object = "df_top_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_int_ctrl) == 0L
        )

        expect_false(
          object = "df_top_dev_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_dev_int_ctrl) == 0L
        )

      }
    )

    ## Ct with int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Ct"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = TRUE,
          version = 0L
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_top_oid,
          expected = df_rand$list_df_long$df_top_long$df_oid
        )

        expect_identical(
          object = df_out$df_top_plate,
          expected = df_rand$list_df_long$df_top_long$df_plate
        )

        expect_false(
          object = "df_top_qc_warn" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_qc_warn) == 0L
        )

        expect_identical(
          object = df_out$df_top_int_ctrl,
          expected = df_rand$list_df_long$df_top_long$df_int_ctrl
        )

        expect_false(
          object = "df_top_dev_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_dev_int_ctrl) == 0L
        )

      }
    )

    ## Quantified no int ctrl and no dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Quantified"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = FALSE,
          version = 0L
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_top_oid,
          expected = df_rand$list_df_long$df_top_long$df_oid
        )

        expect_identical(
          object = df_out$df_top_plate,
          expected = df_rand$list_df_long$df_top_long$df_plate
        )

        expect_identical(
          object = df_out$df_top_qc_warn,
          expected = df_rand$list_df_long$df_top_long$df_qc_warn
        )

        expect_false(
          object = "df_top_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_int_ctrl) == 0L
        )

        expect_false(
          object = "df_top_dev_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_dev_int_ctrl) == 0L
        )

      }
    )

    ## Quantified with int ctrl and no dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Quantified"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = TRUE,
          version = 0L
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_top_oid,
          expected = df_rand$list_df_long$df_top_long$df_oid
        )

        expect_identical(
          object = df_out$df_top_plate,
          expected = df_rand$list_df_long$df_top_long$df_plate
        )

        expect_identical(
          object = df_out$df_top_qc_warn,
          expected = df_rand$list_df_long$df_top_long$df_qc_warn
        )

        expect_identical(
          object = df_out$df_top_int_ctrl,
          expected = df_rand$list_df_long$df_top_long$df_int_ctrl
        )

        expect_false(
          object = "df_top_dev_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_dev_int_ctrl) == 0L
        )

      }
    )

    ## Quantified no int ctrl and with dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Quantified"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = TRUE,
          show_int_ctrl = FALSE,
          version = 0L
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_top_oid,
          expected = df_rand$list_df_long$df_top_long$df_oid
        )

        expect_identical(
          object = df_out$df_top_plate,
          expected = df_rand$list_df_long$df_top_long$df_plate
        )

        expect_identical(
          object = df_out$df_top_qc_warn,
          expected = df_rand$list_df_long$df_top_long$df_qc_warn
        )

        expect_false(
          object = "df_top_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_int_ctrl) == 0L
        )

        expect_identical(
          object = df_out$df_top_dev_int_ctrl,
          expected = df_rand$list_df_long$df_top_long$df_dev_int_ctrl
        )

      }
    )

    ## Quantified with int ctrl and with dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Quantified"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = TRUE,
          show_int_ctrl = TRUE,
          version = 0L
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_top_oid,
          expected = df_rand$list_df_long$df_top_long$df_oid
        )

        expect_identical(
          object = df_out$df_top_plate,
          expected = df_rand$list_df_long$df_top_long$df_plate
        )

        expect_identical(
          object = df_out$df_top_qc_warn,
          expected = df_rand$list_df_long$df_top_long$df_qc_warn
        )

        expect_identical(
          object = df_out$df_top_int_ctrl,
          expected = df_rand$list_df_long$df_top_long$df_int_ctrl
        )

        expect_identical(
          object = df_out$df_top_dev_int_ctrl,
          expected = df_rand$list_df_long$df_top_long$df_dev_int_ctrl
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_top - works - T48 - multiple panels",
  {
    # variables that apply to all tests
    olink_platform <- "Target 48"
    n_panels <- 3L
    n_assays <- 45L
    n_samples <- 88L

    ## NPX no int ctrl and no dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = FALSE,
          version = 1L
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_top_oid,
          expected = df_rand$list_df_long$df_top_long$df_oid
        )

        expect_identical(
          object = df_out$df_top_plate,
          expected = df_rand$list_df_long$df_top_long$df_plate
        )

        expect_identical(
          object = df_out$df_top_qc_warn,
          expected = df_rand$list_df_long$df_top_long$df_qc_warn
        )

        expect_false(
          object = "df_top_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_int_ctrl) == 0L
        )

        expect_false(
          object = "df_top_dev_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_dev_int_ctrl) == 0L
        )

      }
    )

    ## NPX with int ctrl and no dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = TRUE,
          version = 1L
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_top_oid,
          expected = df_rand$list_df_long$df_top_long$df_oid
        )

        expect_identical(
          object = df_out$df_top_plate,
          expected = df_rand$list_df_long$df_top_long$df_plate
        )

        expect_identical(
          object = df_out$df_top_qc_warn,
          expected = df_rand$list_df_long$df_top_long$df_qc_warn
        )

        expect_identical(
          object = df_out$df_top_int_ctrl,
          expected = df_rand$list_df_long$df_top_long$df_int_ctrl
        )

        expect_false(
          object = "df_top_dev_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_dev_int_ctrl) == 0L
        )

      }
    )

    ## NPX no int ctrl and with dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = TRUE,
          show_int_ctrl = FALSE,
          version = 1L
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_top_oid,
          expected = df_rand$list_df_long$df_top_long$df_oid
        )

        expect_identical(
          object = df_out$df_top_plate,
          expected = df_rand$list_df_long$df_top_long$df_plate
        )

        expect_identical(
          object = df_out$df_top_qc_warn,
          expected = df_rand$list_df_long$df_top_long$df_qc_warn
        )

        expect_false(
          object = "df_top_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_int_ctrl) == 0L
        )

        expect_identical(
          object = df_out$df_top_dev_int_ctrl,
          expected = df_rand$list_df_long$df_top_long$df_dev_int_ctrl
        )

      }
    )

    ## NPX with int ctrl and with dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = TRUE,
          show_int_ctrl = TRUE,
          version = 1L
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_top_oid,
          expected = df_rand$list_df_long$df_top_long$df_oid
        )

        expect_identical(
          object = df_out$df_top_plate,
          expected = df_rand$list_df_long$df_top_long$df_plate
        )

        expect_identical(
          object = df_out$df_top_qc_warn,
          expected = df_rand$list_df_long$df_top_long$df_qc_warn
        )

        expect_identical(
          object = df_out$df_top_int_ctrl,
          expected = df_rand$list_df_long$df_top_long$df_int_ctrl
        )

        expect_identical(
          object = df_out$df_top_dev_int_ctrl,
          expected = df_rand$list_df_long$df_top_long$df_dev_int_ctrl
        )

      }
    )


    ## Ct no int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Ct"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = FALSE,
          version = 0L
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_top_oid,
          expected = df_rand$list_df_long$df_top_long$df_oid
        )

        expect_identical(
          object = df_out$df_top_plate,
          expected = df_rand$list_df_long$df_top_long$df_plate
        )

        expect_false(
          object = "df_top_qc_warn" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_qc_warn) == 0L
        )

        expect_false(
          object = "df_top_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_int_ctrl) == 0L
        )

        expect_false(
          object = "df_top_dev_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_dev_int_ctrl) == 0L
        )

      }
    )

    ## Ct with int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Ct"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = TRUE,
          version = 0L
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_top_oid,
          expected = df_rand$list_df_long$df_top_long$df_oid
        )

        expect_identical(
          object = df_out$df_top_plate,
          expected = df_rand$list_df_long$df_top_long$df_plate
        )

        expect_false(
          object = "df_top_qc_warn" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_qc_warn) == 0L
        )

        expect_identical(
          object = df_out$df_top_int_ctrl,
          expected = df_rand$list_df_long$df_top_long$df_int_ctrl
        )

        expect_false(
          object = "df_top_dev_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_dev_int_ctrl) == 0L
        )

      }
    )

    ## Quantified no int ctrl and no dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Quantified"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = FALSE,
          version = 0L
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_top_oid,
          expected = df_rand$list_df_long$df_top_long$df_oid
        )

        expect_identical(
          object = df_out$df_top_plate,
          expected = df_rand$list_df_long$df_top_long$df_plate
        )

        expect_identical(
          object = df_out$df_top_qc_warn,
          expected = df_rand$list_df_long$df_top_long$df_qc_warn
        )

        expect_false(
          object = "df_top_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_int_ctrl) == 0L
        )

        expect_false(
          object = "df_top_dev_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_dev_int_ctrl) == 0L
        )

      }
    )

    ## Quantified with int ctrl and no dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Quantified"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = TRUE,
          version = 0L
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_top_oid,
          expected = df_rand$list_df_long$df_top_long$df_oid
        )

        expect_identical(
          object = df_out$df_top_plate,
          expected = df_rand$list_df_long$df_top_long$df_plate
        )

        expect_identical(
          object = df_out$df_top_qc_warn,
          expected = df_rand$list_df_long$df_top_long$df_qc_warn
        )

        expect_identical(
          object = df_out$df_top_int_ctrl,
          expected = df_rand$list_df_long$df_top_long$df_int_ctrl
        )

        expect_false(
          object = "df_top_dev_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_dev_int_ctrl) == 0L
        )

      }
    )

    ## Quantified no int ctrl and with dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Quantified"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = TRUE,
          show_int_ctrl = FALSE,
          version = 0L
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_top_oid,
          expected = df_rand$list_df_long$df_top_long$df_oid
        )

        expect_identical(
          object = df_out$df_top_plate,
          expected = df_rand$list_df_long$df_top_long$df_plate
        )

        expect_identical(
          object = df_out$df_top_qc_warn,
          expected = df_rand$list_df_long$df_top_long$df_qc_warn
        )

        expect_false(
          object = "df_top_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_int_ctrl) == 0L
        )

        expect_identical(
          object = df_out$df_top_dev_int_ctrl,
          expected = df_rand$list_df_long$df_top_long$df_dev_int_ctrl
        )

      }
    )

    ## Quantified with int ctrl and with dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Quantified"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = TRUE,
          show_int_ctrl = TRUE,
          version = 0L
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_top_oid,
          expected = df_rand$list_df_long$df_top_long$df_oid
        )

        expect_identical(
          object = df_out$df_top_plate,
          expected = df_rand$list_df_long$df_top_long$df_plate
        )

        expect_identical(
          object = df_out$df_top_qc_warn,
          expected = df_rand$list_df_long$df_top_long$df_qc_warn
        )

        expect_identical(
          object = df_out$df_top_int_ctrl,
          expected = df_rand$list_df_long$df_top_long$df_int_ctrl
        )

        expect_identical(
          object = df_out$df_top_dev_int_ctrl,
          expected = df_rand$list_df_long$df_top_long$df_dev_int_ctrl
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_top - works - T96 - single panel",
  {
    # variables that apply to all tests
    olink_platform <- "Target 96"
    n_panels <- 1L
    n_assays <- 92L
    n_samples <- 88L

    ## NPX no int ctrl and no dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = FALSE,
          version = 1L
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_top_oid,
          expected = df_rand$list_df_long$df_top_long$df_oid
        )

        expect_identical(
          object = df_out$df_top_plate,
          expected = df_rand$list_df_long$df_top_long$df_plate
        )

        expect_identical(
          object = df_out$df_top_qc_warn,
          expected = df_rand$list_df_long$df_top_long$df_qc_warn
        )

        expect_false(
          object = "df_top_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_int_ctrl) == 0L
        )

        expect_false(
          object = "df_top_dev_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_dev_int_ctrl) == 0L
        )

      }
    )

    ## NPX with int ctrl and no dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = TRUE,
          version = 1L
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_top_oid,
          expected = df_rand$list_df_long$df_top_long$df_oid
        )

        expect_identical(
          object = df_out$df_top_plate,
          expected = df_rand$list_df_long$df_top_long$df_plate
        )

        expect_identical(
          object = df_out$df_top_qc_warn,
          expected = df_rand$list_df_long$df_top_long$df_qc_warn
        )

        expect_identical(
          object = df_out$df_top_int_ctrl,
          expected = df_rand$list_df_long$df_top_long$df_int_ctrl
        )

        expect_false(
          object = "df_top_dev_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_dev_int_ctrl) == 0L
        )

      }
    )

    ## NPX no int ctrl and with dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = TRUE,
          show_int_ctrl = FALSE,
          version = 1L
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_top_oid,
          expected = df_rand$list_df_long$df_top_long$df_oid
        )

        expect_identical(
          object = df_out$df_top_plate,
          expected = df_rand$list_df_long$df_top_long$df_plate
        )

        expect_identical(
          object = df_out$df_top_qc_warn,
          expected = df_rand$list_df_long$df_top_long$df_qc_warn
        )

        expect_false(
          object = "df_top_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_int_ctrl) == 0L
        )

        expect_identical(
          object = df_out$df_top_dev_int_ctrl,
          expected = df_rand$list_df_long$df_top_long$df_dev_int_ctrl
        )

      }
    )

    ## NPX with int ctrl and with dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = TRUE,
          show_int_ctrl = TRUE,
          version = 1L
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_top_oid,
          expected = df_rand$list_df_long$df_top_long$df_oid
        )

        expect_identical(
          object = df_out$df_top_plate,
          expected = df_rand$list_df_long$df_top_long$df_plate
        )

        expect_identical(
          object = df_out$df_top_qc_warn,
          expected = df_rand$list_df_long$df_top_long$df_qc_warn
        )

        expect_identical(
          object = df_out$df_top_int_ctrl,
          expected = df_rand$list_df_long$df_top_long$df_int_ctrl
        )

        expect_identical(
          object = df_out$df_top_dev_int_ctrl,
          expected = df_rand$list_df_long$df_top_long$df_dev_int_ctrl
        )

      }
    )


    ## Ct no int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Ct"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = FALSE,
          version = 0L
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_top_oid,
          expected = df_rand$list_df_long$df_top_long$df_oid
        )

        expect_identical(
          object = df_out$df_top_plate,
          expected = df_rand$list_df_long$df_top_long$df_plate
        )

        expect_false(
          object = "df_top_qc_warn" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_qc_warn) == 0L
        )

        expect_false(
          object = "df_top_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_int_ctrl) == 0L
        )

        expect_false(
          object = "df_top_dev_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_dev_int_ctrl) == 0L
        )

      }
    )

    ## Ct with int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Ct"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = TRUE,
          version = 0L
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_top_oid,
          expected = df_rand$list_df_long$df_top_long$df_oid
        )

        expect_identical(
          object = df_out$df_top_plate,
          expected = df_rand$list_df_long$df_top_long$df_plate
        )

        expect_false(
          object = "df_top_qc_warn" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_qc_warn) == 0L
        )

        expect_identical(
          object = df_out$df_top_int_ctrl,
          expected = df_rand$list_df_long$df_top_long$df_int_ctrl
        )

        expect_false(
          object = "df_top_dev_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_dev_int_ctrl) == 0L
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_top - works - T96 - multiple panels",
  {
    # variables that apply to all tests
    olink_platform <- "Target 96"
    n_panels <- 3L
    n_assays <- 92L
    n_samples <- 88L

    ## NPX no int ctrl and no dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = FALSE,
          version = 1L
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_top_oid,
          expected = df_rand$list_df_long$df_top_long$df_oid
        )

        expect_identical(
          object = df_out$df_top_plate,
          expected = df_rand$list_df_long$df_top_long$df_plate
        )

        expect_identical(
          object = df_out$df_top_qc_warn,
          expected = df_rand$list_df_long$df_top_long$df_qc_warn
        )

        expect_false(
          object = "df_top_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_int_ctrl) == 0L
        )

        expect_false(
          object = "df_top_dev_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_dev_int_ctrl) == 0L
        )

      }
    )

    ## NPX with int ctrl and no dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = TRUE,
          version = 1L
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_top_oid,
          expected = df_rand$list_df_long$df_top_long$df_oid
        )

        expect_identical(
          object = df_out$df_top_plate,
          expected = df_rand$list_df_long$df_top_long$df_plate
        )

        expect_identical(
          object = df_out$df_top_qc_warn,
          expected = df_rand$list_df_long$df_top_long$df_qc_warn
        )

        expect_identical(
          object = df_out$df_top_int_ctrl,
          expected = df_rand$list_df_long$df_top_long$df_int_ctrl
        )

        expect_false(
          object = "df_top_dev_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_dev_int_ctrl) == 0L
        )

      }
    )

    ## NPX no int ctrl and with dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = TRUE,
          show_int_ctrl = FALSE,
          version = 1L
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_top_oid,
          expected = df_rand$list_df_long$df_top_long$df_oid
        )

        expect_identical(
          object = df_out$df_top_plate,
          expected = df_rand$list_df_long$df_top_long$df_plate
        )

        expect_identical(
          object = df_out$df_top_qc_warn,
          expected = df_rand$list_df_long$df_top_long$df_qc_warn
        )

        expect_false(
          object = "df_top_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_int_ctrl) == 0L
        )

        expect_identical(
          object = df_out$df_top_dev_int_ctrl,
          expected = df_rand$list_df_long$df_top_long$df_dev_int_ctrl
        )

      }
    )

    ## NPX with int ctrl and with dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = TRUE,
          show_int_ctrl = TRUE,
          version = 1L
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_top_oid,
          expected = df_rand$list_df_long$df_top_long$df_oid
        )

        expect_identical(
          object = df_out$df_top_plate,
          expected = df_rand$list_df_long$df_top_long$df_plate
        )

        expect_identical(
          object = df_out$df_top_qc_warn,
          expected = df_rand$list_df_long$df_top_long$df_qc_warn
        )

        expect_identical(
          object = df_out$df_top_int_ctrl,
          expected = df_rand$list_df_long$df_top_long$df_int_ctrl
        )

        expect_identical(
          object = df_out$df_top_dev_int_ctrl,
          expected = df_rand$list_df_long$df_top_long$df_dev_int_ctrl
        )

      }
    )


    ## Ct no int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Ct"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = FALSE,
          version = 0L
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_top_oid,
          expected = df_rand$list_df_long$df_top_long$df_oid
        )

        expect_identical(
          object = df_out$df_top_plate,
          expected = df_rand$list_df_long$df_top_long$df_plate
        )

        expect_false(
          object = "df_top_qc_warn" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_qc_warn) == 0L
        )

        expect_false(
          object = "df_top_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_int_ctrl) == 0L
        )

        expect_false(
          object = "df_top_dev_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_dev_int_ctrl) == 0L
        )

      }
    )

    ## Ct with int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Ct"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = TRUE,
          version = 0L
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_top_oid,
          expected = df_rand$list_df_long$df_top_long$df_oid
        )

        expect_identical(
          object = df_out$df_top_plate,
          expected = df_rand$list_df_long$df_top_long$df_plate
        )

        expect_false(
          object = "df_top_qc_warn" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_qc_warn) == 0L
        )

        expect_identical(
          object = df_out$df_top_int_ctrl,
          expected = df_rand$list_df_long$df_top_long$df_int_ctrl
        )

        expect_false(
          object = "df_top_dev_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_dev_int_ctrl) == 0L
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_top - works - Flex - single panel",
  {
    # variables that apply to all tests
    olink_platform <- "Flex"
    n_panels <- 1L
    n_assays <- 20L
    n_samples <- 88L

    ## NPX no int ctrl and no dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = FALSE,
          version = 1L
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_top_oid,
          expected = df_rand$list_df_long$df_top_long$df_oid
        )

        expect_identical(
          object = df_out$df_top_plate,
          expected = df_rand$list_df_long$df_top_long$df_plate
        )

        expect_identical(
          object = df_out$df_top_qc_warn,
          expected = df_rand$list_df_long$df_top_long$df_qc_warn
        )

        expect_false(
          object = "df_top_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_int_ctrl) == 0L
        )

        expect_false(
          object = "df_top_dev_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_dev_int_ctrl) == 0L
        )

      }
    )

    ## NPX with int ctrl and no dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = TRUE,
          version = 1L
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_top_oid,
          expected = df_rand$list_df_long$df_top_long$df_oid
        )

        expect_identical(
          object = df_out$df_top_plate,
          expected = df_rand$list_df_long$df_top_long$df_plate
        )

        expect_identical(
          object = df_out$df_top_qc_warn,
          expected = df_rand$list_df_long$df_top_long$df_qc_warn
        )

        expect_identical(
          object = df_out$df_top_int_ctrl,
          expected = df_rand$list_df_long$df_top_long$df_int_ctrl
        )

        expect_false(
          object = "df_top_dev_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_dev_int_ctrl) == 0L
        )

      }
    )

    ## NPX no int ctrl and with dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = TRUE,
          show_int_ctrl = FALSE,
          version = 1L
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_top_oid,
          expected = df_rand$list_df_long$df_top_long$df_oid
        )

        expect_identical(
          object = df_out$df_top_plate,
          expected = df_rand$list_df_long$df_top_long$df_plate
        )

        expect_identical(
          object = df_out$df_top_qc_warn,
          expected = df_rand$list_df_long$df_top_long$df_qc_warn
        )

        expect_false(
          object = "df_top_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_int_ctrl) == 0L
        )

        expect_identical(
          object = df_out$df_top_dev_int_ctrl,
          expected = df_rand$list_df_long$df_top_long$df_dev_int_ctrl
        )

      }
    )

    ## NPX with int ctrl and with dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = TRUE,
          show_int_ctrl = TRUE,
          version = 1L
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_top_oid,
          expected = df_rand$list_df_long$df_top_long$df_oid
        )

        expect_identical(
          object = df_out$df_top_plate,
          expected = df_rand$list_df_long$df_top_long$df_plate
        )

        expect_identical(
          object = df_out$df_top_qc_warn,
          expected = df_rand$list_df_long$df_top_long$df_qc_warn
        )

        expect_identical(
          object = df_out$df_top_int_ctrl,
          expected = df_rand$list_df_long$df_top_long$df_int_ctrl
        )

        expect_identical(
          object = df_out$df_top_dev_int_ctrl,
          expected = df_rand$list_df_long$df_top_long$df_dev_int_ctrl
        )

      }
    )


    ## Ct no int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Ct"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = FALSE,
          version = 0L
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_top_oid,
          expected = df_rand$list_df_long$df_top_long$df_oid
        )

        expect_identical(
          object = df_out$df_top_plate,
          expected = df_rand$list_df_long$df_top_long$df_plate
        )

        expect_false(
          object = "df_top_qc_warn" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_qc_warn) == 0L
        )

        expect_false(
          object = "df_top_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_int_ctrl) == 0L
        )

        expect_false(
          object = "df_top_dev_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_dev_int_ctrl) == 0L
        )

      }
    )

    ## Ct with int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Ct"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = TRUE,
          version = 0L
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_top_oid,
          expected = df_rand$list_df_long$df_top_long$df_oid
        )

        expect_identical(
          object = df_out$df_top_plate,
          expected = df_rand$list_df_long$df_top_long$df_plate
        )

        expect_false(
          object = "df_top_qc_warn" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_qc_warn) == 0L
        )

        expect_identical(
          object = df_out$df_top_int_ctrl,
          expected = df_rand$list_df_long$df_top_long$df_int_ctrl
        )

        expect_false(
          object = "df_top_dev_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_dev_int_ctrl) == 0L
        )

      }
    )

    ## Quantified no int ctrl and no dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Quantified"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = FALSE,
          version = 0L
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_top_oid,
          expected = df_rand$list_df_long$df_top_long$df_oid
        )

        expect_identical(
          object = df_out$df_top_plate,
          expected = df_rand$list_df_long$df_top_long$df_plate
        )

        expect_identical(
          object = df_out$df_top_qc_warn,
          expected = df_rand$list_df_long$df_top_long$df_qc_warn
        )

        expect_false(
          object = "df_top_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_int_ctrl) == 0L
        )

        expect_false(
          object = "df_top_dev_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_dev_int_ctrl) == 0L
        )

      }
    )

    ## Quantified with int ctrl and no dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Quantified"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = TRUE,
          version = 0L
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_top_oid,
          expected = df_rand$list_df_long$df_top_long$df_oid
        )

        expect_identical(
          object = df_out$df_top_plate,
          expected = df_rand$list_df_long$df_top_long$df_plate
        )

        expect_identical(
          object = df_out$df_top_qc_warn,
          expected = df_rand$list_df_long$df_top_long$df_qc_warn
        )

        expect_identical(
          object = df_out$df_top_int_ctrl,
          expected = df_rand$list_df_long$df_top_long$df_int_ctrl
        )

        expect_false(
          object = "df_top_dev_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_dev_int_ctrl) == 0L
        )

      }
    )

    ## Quantified no int ctrl and with dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Quantified"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = TRUE,
          show_int_ctrl = FALSE,
          version = 0L
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_top_oid,
          expected = df_rand$list_df_long$df_top_long$df_oid
        )

        expect_identical(
          object = df_out$df_top_plate,
          expected = df_rand$list_df_long$df_top_long$df_plate
        )

        expect_identical(
          object = df_out$df_top_qc_warn,
          expected = df_rand$list_df_long$df_top_long$df_qc_warn
        )

        expect_false(
          object = "df_top_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_int_ctrl) == 0L
        )

        expect_identical(
          object = df_out$df_top_dev_int_ctrl,
          expected = df_rand$list_df_long$df_top_long$df_dev_int_ctrl
        )

      }
    )

    ## Quantified with int ctrl and with dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Quantified"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = TRUE,
          show_int_ctrl = TRUE,
          version = 0L
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_top_oid,
          expected = df_rand$list_df_long$df_top_long$df_oid
        )

        expect_identical(
          object = df_out$df_top_plate,
          expected = df_rand$list_df_long$df_top_long$df_plate
        )

        expect_identical(
          object = df_out$df_top_qc_warn,
          expected = df_rand$list_df_long$df_top_long$df_qc_warn
        )

        expect_identical(
          object = df_out$df_top_int_ctrl,
          expected = df_rand$list_df_long$df_top_long$df_int_ctrl
        )

        expect_identical(
          object = df_out$df_top_dev_int_ctrl,
          expected = df_rand$list_df_long$df_top_long$df_dev_int_ctrl
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_top - works - Flex - multiple panels",
  {
    # variables that apply to all tests
    olink_platform <- "Flex"
    n_panels <- 3L
    n_assays <- 20L
    n_samples <- 88L

    ## NPX no int ctrl and no dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = FALSE,
          version = 1L
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_top_oid,
          expected = df_rand$list_df_long$df_top_long$df_oid
        )

        expect_identical(
          object = df_out$df_top_plate,
          expected = df_rand$list_df_long$df_top_long$df_plate
        )

        expect_identical(
          object = df_out$df_top_qc_warn,
          expected = df_rand$list_df_long$df_top_long$df_qc_warn
        )

        expect_false(
          object = "df_top_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_int_ctrl) == 0L
        )

        expect_false(
          object = "df_top_dev_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_dev_int_ctrl) == 0L
        )

      }
    )

    ## NPX with int ctrl and no dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = TRUE,
          version = 1L
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_top_oid,
          expected = df_rand$list_df_long$df_top_long$df_oid
        )

        expect_identical(
          object = df_out$df_top_plate,
          expected = df_rand$list_df_long$df_top_long$df_plate
        )

        expect_identical(
          object = df_out$df_top_qc_warn,
          expected = df_rand$list_df_long$df_top_long$df_qc_warn
        )

        expect_identical(
          object = df_out$df_top_int_ctrl,
          expected = df_rand$list_df_long$df_top_long$df_int_ctrl
        )

        expect_false(
          object = "df_top_dev_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_dev_int_ctrl) == 0L
        )

      }
    )

    ## NPX no int ctrl and with dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = TRUE,
          show_int_ctrl = FALSE,
          version = 1L
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_top_oid,
          expected = df_rand$list_df_long$df_top_long$df_oid
        )

        expect_identical(
          object = df_out$df_top_plate,
          expected = df_rand$list_df_long$df_top_long$df_plate
        )

        expect_identical(
          object = df_out$df_top_qc_warn,
          expected = df_rand$list_df_long$df_top_long$df_qc_warn
        )

        expect_false(
          object = "df_top_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_int_ctrl) == 0L
        )

        expect_identical(
          object = df_out$df_top_dev_int_ctrl,
          expected = df_rand$list_df_long$df_top_long$df_dev_int_ctrl
        )

      }
    )

    ## NPX with int ctrl and with dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = TRUE,
          show_int_ctrl = TRUE,
          version = 1L
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_top_oid,
          expected = df_rand$list_df_long$df_top_long$df_oid
        )

        expect_identical(
          object = df_out$df_top_plate,
          expected = df_rand$list_df_long$df_top_long$df_plate
        )

        expect_identical(
          object = df_out$df_top_qc_warn,
          expected = df_rand$list_df_long$df_top_long$df_qc_warn
        )

        expect_identical(
          object = df_out$df_top_int_ctrl,
          expected = df_rand$list_df_long$df_top_long$df_int_ctrl
        )

        expect_identical(
          object = df_out$df_top_dev_int_ctrl,
          expected = df_rand$list_df_long$df_top_long$df_dev_int_ctrl
        )

      }
    )


    ## Ct no int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Ct"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = FALSE,
          version = 0L
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_top_oid,
          expected = df_rand$list_df_long$df_top_long$df_oid
        )

        expect_identical(
          object = df_out$df_top_plate,
          expected = df_rand$list_df_long$df_top_long$df_plate
        )

        expect_false(
          object = "df_top_qc_warn" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_qc_warn) == 0L
        )

        expect_false(
          object = "df_top_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_int_ctrl) == 0L
        )

        expect_false(
          object = "df_top_dev_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_dev_int_ctrl) == 0L
        )

      }
    )

    ## Ct with int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Ct"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = TRUE,
          version = 0L
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_top_oid,
          expected = df_rand$list_df_long$df_top_long$df_oid
        )

        expect_identical(
          object = df_out$df_top_plate,
          expected = df_rand$list_df_long$df_top_long$df_plate
        )

        expect_false(
          object = "df_top_qc_warn" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_qc_warn) == 0L
        )

        expect_identical(
          object = df_out$df_top_int_ctrl,
          expected = df_rand$list_df_long$df_top_long$df_int_ctrl
        )

        expect_false(
          object = "df_top_dev_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_dev_int_ctrl) == 0L
        )

      }
    )

    ## Quantified no int ctrl and no dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Quantified"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = FALSE,
          version = 0L
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_top_oid,
          expected = df_rand$list_df_long$df_top_long$df_oid
        )

        expect_identical(
          object = df_out$df_top_plate,
          expected = df_rand$list_df_long$df_top_long$df_plate
        )

        expect_identical(
          object = df_out$df_top_qc_warn,
          expected = df_rand$list_df_long$df_top_long$df_qc_warn
        )

        expect_false(
          object = "df_top_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_int_ctrl) == 0L
        )

        expect_false(
          object = "df_top_dev_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_dev_int_ctrl) == 0L
        )

      }
    )

    ## Quantified with int ctrl and no dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Quantified"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = TRUE,
          version = 0L
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_top_oid,
          expected = df_rand$list_df_long$df_top_long$df_oid
        )

        expect_identical(
          object = df_out$df_top_plate,
          expected = df_rand$list_df_long$df_top_long$df_plate
        )

        expect_identical(
          object = df_out$df_top_qc_warn,
          expected = df_rand$list_df_long$df_top_long$df_qc_warn
        )

        expect_identical(
          object = df_out$df_top_int_ctrl,
          expected = df_rand$list_df_long$df_top_long$df_int_ctrl
        )

        expect_false(
          object = "df_top_dev_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_dev_int_ctrl) == 0L
        )

      }
    )

    ## Quantified no int ctrl and with dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Quantified"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = TRUE,
          show_int_ctrl = FALSE,
          version = 0L
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_top_oid,
          expected = df_rand$list_df_long$df_top_long$df_oid
        )

        expect_identical(
          object = df_out$df_top_plate,
          expected = df_rand$list_df_long$df_top_long$df_plate
        )

        expect_identical(
          object = df_out$df_top_qc_warn,
          expected = df_rand$list_df_long$df_top_long$df_qc_warn
        )

        expect_false(
          object = "df_top_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_int_ctrl) == 0L
        )

        expect_identical(
          object = df_out$df_top_dev_int_ctrl,
          expected = df_rand$list_df_long$df_top_long$df_dev_int_ctrl
        )

      }
    )

    ## Quantified with int ctrl and with dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Quantified"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = TRUE,
          show_int_ctrl = TRUE,
          version = 0L
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_top_oid,
          expected = df_rand$list_df_long$df_top_long$df_oid
        )

        expect_identical(
          object = df_out$df_top_plate,
          expected = df_rand$list_df_long$df_top_long$df_plate
        )

        expect_identical(
          object = df_out$df_top_qc_warn,
          expected = df_rand$list_df_long$df_top_long$df_qc_warn
        )

        expect_identical(
          object = df_out$df_top_int_ctrl,
          expected = df_rand$list_df_long$df_top_long$df_int_ctrl
        )

        expect_identical(
          object = df_out$df_top_dev_int_ctrl,
          expected = df_rand$list_df_long$df_top_long$df_dev_int_ctrl
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_top - works - Focus - single panel",
  {
    # variables that apply to all tests
    olink_platform <- "Focus"
    n_panels <- 1L
    n_assays <- 33L
    n_samples <- 88L
    version <- 0L

    ## NPX no int ctrl and no dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = FALSE,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_top_oid,
          expected = df_rand$list_df_long$df_top_long$df_oid
        )

        expect_identical(
          object = df_out$df_top_plate,
          expected = df_rand$list_df_long$df_top_long$df_plate
        )

        expect_identical(
          object = df_out$df_top_qc_warn,
          expected = df_rand$list_df_long$df_top_long$df_qc_warn
        )

        expect_false(
          object = "df_top_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_int_ctrl) == 0L
        )

        expect_false(
          object = "df_top_dev_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_dev_int_ctrl) == 0L
        )

      }
    )

    ## NPX with int ctrl and no dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = TRUE,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_top_oid,
          expected = df_rand$list_df_long$df_top_long$df_oid
        )

        expect_identical(
          object = df_out$df_top_plate,
          expected = df_rand$list_df_long$df_top_long$df_plate
        )

        expect_identical(
          object = df_out$df_top_qc_warn,
          expected = df_rand$list_df_long$df_top_long$df_qc_warn
        )

        expect_identical(
          object = df_out$df_top_int_ctrl,
          expected = df_rand$list_df_long$df_top_long$df_int_ctrl
        )

        expect_false(
          object = "df_top_dev_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_dev_int_ctrl) == 0L
        )

      }
    )

    ## NPX no int ctrl and with dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = TRUE,
          show_int_ctrl = FALSE,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_top_oid,
          expected = df_rand$list_df_long$df_top_long$df_oid
        )

        expect_identical(
          object = df_out$df_top_plate,
          expected = df_rand$list_df_long$df_top_long$df_plate
        )

        expect_identical(
          object = df_out$df_top_qc_warn,
          expected = df_rand$list_df_long$df_top_long$df_qc_warn
        )

        expect_false(
          object = "df_top_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_int_ctrl) == 0L
        )

        expect_identical(
          object = df_out$df_top_dev_int_ctrl,
          expected = df_rand$list_df_long$df_top_long$df_dev_int_ctrl
        )

      }
    )

    ## NPX with int ctrl and with dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = TRUE,
          show_int_ctrl = TRUE,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_top_oid,
          expected = df_rand$list_df_long$df_top_long$df_oid
        )

        expect_identical(
          object = df_out$df_top_plate,
          expected = df_rand$list_df_long$df_top_long$df_plate
        )

        expect_identical(
          object = df_out$df_top_qc_warn,
          expected = df_rand$list_df_long$df_top_long$df_qc_warn
        )

        expect_identical(
          object = df_out$df_top_int_ctrl,
          expected = df_rand$list_df_long$df_top_long$df_int_ctrl
        )

        expect_identical(
          object = df_out$df_top_dev_int_ctrl,
          expected = df_rand$list_df_long$df_top_long$df_dev_int_ctrl
        )

      }
    )


    ## Ct no int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Ct"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = FALSE,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_top_oid,
          expected = df_rand$list_df_long$df_top_long$df_oid
        )

        expect_identical(
          object = df_out$df_top_plate,
          expected = df_rand$list_df_long$df_top_long$df_plate
        )

        expect_false(
          object = "df_top_qc_warn" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_qc_warn) == 0L
        )

        expect_false(
          object = "df_top_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_int_ctrl) == 0L
        )

        expect_false(
          object = "df_top_dev_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_dev_int_ctrl) == 0L
        )

      }
    )

    ## Ct with int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Ct"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = TRUE,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_top_oid,
          expected = df_rand$list_df_long$df_top_long$df_oid
        )

        expect_identical(
          object = df_out$df_top_plate,
          expected = df_rand$list_df_long$df_top_long$df_plate
        )

        expect_false(
          object = "df_top_qc_warn" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_qc_warn) == 0L
        )

        expect_identical(
          object = df_out$df_top_int_ctrl,
          expected = df_rand$list_df_long$df_top_long$df_int_ctrl
        )

        expect_false(
          object = "df_top_dev_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_dev_int_ctrl) == 0L
        )

      }
    )

    ## Quantified no int ctrl and no dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Quantified"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = FALSE,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_top_oid,
          expected = df_rand$list_df_long$df_top_long$df_oid
        )

        expect_identical(
          object = df_out$df_top_plate,
          expected = df_rand$list_df_long$df_top_long$df_plate
        )

        expect_identical(
          object = df_out$df_top_qc_warn,
          expected = df_rand$list_df_long$df_top_long$df_qc_warn
        )

        expect_false(
          object = "df_top_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_int_ctrl) == 0L
        )

        expect_false(
          object = "df_top_dev_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_dev_int_ctrl) == 0L
        )

      }
    )

    ## Quantified with int ctrl and no dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Quantified"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = TRUE,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_top_oid,
          expected = df_rand$list_df_long$df_top_long$df_oid
        )

        expect_identical(
          object = df_out$df_top_plate,
          expected = df_rand$list_df_long$df_top_long$df_plate
        )

        expect_identical(
          object = df_out$df_top_qc_warn,
          expected = df_rand$list_df_long$df_top_long$df_qc_warn
        )

        expect_identical(
          object = df_out$df_top_int_ctrl,
          expected = df_rand$list_df_long$df_top_long$df_int_ctrl
        )

        expect_false(
          object = "df_top_dev_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_dev_int_ctrl) == 0L
        )

      }
    )

    ## Quantified no int ctrl and with dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Quantified"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = TRUE,
          show_int_ctrl = FALSE,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_top_oid,
          expected = df_rand$list_df_long$df_top_long$df_oid
        )

        expect_identical(
          object = df_out$df_top_plate,
          expected = df_rand$list_df_long$df_top_long$df_plate
        )

        expect_identical(
          object = df_out$df_top_qc_warn,
          expected = df_rand$list_df_long$df_top_long$df_qc_warn
        )

        expect_false(
          object = "df_top_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_int_ctrl) == 0L
        )

        expect_identical(
          object = df_out$df_top_dev_int_ctrl,
          expected = df_rand$list_df_long$df_top_long$df_dev_int_ctrl
        )

      }
    )

    ## Quantified with int ctrl and with dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Quantified"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = TRUE,
          show_int_ctrl = TRUE,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_top_oid,
          expected = df_rand$list_df_long$df_top_long$df_oid
        )

        expect_identical(
          object = df_out$df_top_plate,
          expected = df_rand$list_df_long$df_top_long$df_plate
        )

        expect_identical(
          object = df_out$df_top_qc_warn,
          expected = df_rand$list_df_long$df_top_long$df_qc_warn
        )

        expect_identical(
          object = df_out$df_top_int_ctrl,
          expected = df_rand$list_df_long$df_top_long$df_int_ctrl
        )

        expect_identical(
          object = df_out$df_top_dev_int_ctrl,
          expected = df_rand$list_df_long$df_top_long$df_dev_int_ctrl
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_top - works - Focus - multiple panels",
  {
    # variables that apply to all tests
    olink_platform <- "Focus"
    n_panels <- 3L
    n_assays <- 33L
    n_samples <- 88L
    version <- 0L

    ## NPX no int ctrl and no dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = FALSE,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_top_oid,
          expected = df_rand$list_df_long$df_top_long$df_oid
        )

        expect_identical(
          object = df_out$df_top_plate,
          expected = df_rand$list_df_long$df_top_long$df_plate
        )

        expect_identical(
          object = df_out$df_top_qc_warn,
          expected = df_rand$list_df_long$df_top_long$df_qc_warn
        )

        expect_false(
          object = "df_top_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_int_ctrl) == 0L
        )

        expect_false(
          object = "df_top_dev_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_dev_int_ctrl) == 0L
        )

      }
    )

    ## NPX with int ctrl and no dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = TRUE,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_top_oid,
          expected = df_rand$list_df_long$df_top_long$df_oid
        )

        expect_identical(
          object = df_out$df_top_plate,
          expected = df_rand$list_df_long$df_top_long$df_plate
        )

        expect_identical(
          object = df_out$df_top_qc_warn,
          expected = df_rand$list_df_long$df_top_long$df_qc_warn
        )

        expect_identical(
          object = df_out$df_top_int_ctrl,
          expected = df_rand$list_df_long$df_top_long$df_int_ctrl
        )

        expect_false(
          object = "df_top_dev_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_dev_int_ctrl) == 0L
        )

      }
    )

    ## NPX no int ctrl and with dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = TRUE,
          show_int_ctrl = FALSE,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_top_oid,
          expected = df_rand$list_df_long$df_top_long$df_oid
        )

        expect_identical(
          object = df_out$df_top_plate,
          expected = df_rand$list_df_long$df_top_long$df_plate
        )

        expect_identical(
          object = df_out$df_top_qc_warn,
          expected = df_rand$list_df_long$df_top_long$df_qc_warn
        )

        expect_false(
          object = "df_top_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_int_ctrl) == 0L
        )

        expect_identical(
          object = df_out$df_top_dev_int_ctrl,
          expected = df_rand$list_df_long$df_top_long$df_dev_int_ctrl
        )

      }
    )

    ## NPX with int ctrl and with dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = TRUE,
          show_int_ctrl = TRUE,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_top_oid,
          expected = df_rand$list_df_long$df_top_long$df_oid
        )

        expect_identical(
          object = df_out$df_top_plate,
          expected = df_rand$list_df_long$df_top_long$df_plate
        )

        expect_identical(
          object = df_out$df_top_qc_warn,
          expected = df_rand$list_df_long$df_top_long$df_qc_warn
        )

        expect_identical(
          object = df_out$df_top_int_ctrl,
          expected = df_rand$list_df_long$df_top_long$df_int_ctrl
        )

        expect_identical(
          object = df_out$df_top_dev_int_ctrl,
          expected = df_rand$list_df_long$df_top_long$df_dev_int_ctrl
        )

      }
    )


    ## Ct no int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Ct"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = FALSE,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_top_oid,
          expected = df_rand$list_df_long$df_top_long$df_oid
        )

        expect_identical(
          object = df_out$df_top_plate,
          expected = df_rand$list_df_long$df_top_long$df_plate
        )

        expect_false(
          object = "df_top_qc_warn" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_qc_warn) == 0L
        )

        expect_false(
          object = "df_top_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_int_ctrl) == 0L
        )

        expect_false(
          object = "df_top_dev_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_dev_int_ctrl) == 0L
        )

      }
    )

    ## Ct with int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Ct"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = TRUE,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_top_oid,
          expected = df_rand$list_df_long$df_top_long$df_oid
        )

        expect_identical(
          object = df_out$df_top_plate,
          expected = df_rand$list_df_long$df_top_long$df_plate
        )

        expect_false(
          object = "df_top_qc_warn" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_qc_warn) == 0L
        )

        expect_identical(
          object = df_out$df_top_int_ctrl,
          expected = df_rand$list_df_long$df_top_long$df_int_ctrl
        )

        expect_false(
          object = "df_top_dev_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_dev_int_ctrl) == 0L
        )

      }
    )

    ## Quantified no int ctrl and no dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Quantified"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = FALSE,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_top_oid,
          expected = df_rand$list_df_long$df_top_long$df_oid
        )

        expect_identical(
          object = df_out$df_top_plate,
          expected = df_rand$list_df_long$df_top_long$df_plate
        )

        expect_identical(
          object = df_out$df_top_qc_warn,
          expected = df_rand$list_df_long$df_top_long$df_qc_warn
        )

        expect_false(
          object = "df_top_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_int_ctrl) == 0L
        )

        expect_false(
          object = "df_top_dev_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_dev_int_ctrl) == 0L
        )

      }
    )

    ## Quantified with int ctrl and no dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Quantified"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = TRUE,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_top_oid,
          expected = df_rand$list_df_long$df_top_long$df_oid
        )

        expect_identical(
          object = df_out$df_top_plate,
          expected = df_rand$list_df_long$df_top_long$df_plate
        )

        expect_identical(
          object = df_out$df_top_qc_warn,
          expected = df_rand$list_df_long$df_top_long$df_qc_warn
        )

        expect_identical(
          object = df_out$df_top_int_ctrl,
          expected = df_rand$list_df_long$df_top_long$df_int_ctrl
        )

        expect_false(
          object = "df_top_dev_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_dev_int_ctrl) == 0L
        )

      }
    )

    ## Quantified no int ctrl and with dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Quantified"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = TRUE,
          show_int_ctrl = FALSE,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_top_oid,
          expected = df_rand$list_df_long$df_top_long$df_oid
        )

        expect_identical(
          object = df_out$df_top_plate,
          expected = df_rand$list_df_long$df_top_long$df_plate
        )

        expect_identical(
          object = df_out$df_top_qc_warn,
          expected = df_rand$list_df_long$df_top_long$df_qc_warn
        )

        expect_false(
          object = "df_top_int_ctrl" %in% names(df_out)
        )

        expect_true(
          object = nrow(df_rand$list_df_long$df_top_long$df_int_ctrl) == 0L
        )

        expect_identical(
          object = df_out$df_top_dev_int_ctrl,
          expected = df_rand$list_df_long$df_top_long$df_dev_int_ctrl
        )

      }
    )

    ## Quantified with int ctrl and with dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Quantified"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = TRUE,
          show_int_ctrl = TRUE,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_top(
            df = df_rand$list_df_wide$df_top_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            format_spec = get_format_spec(data_type = data_type)
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_top_oid,
          expected = df_rand$list_df_long$df_top_long$df_oid
        )

        expect_identical(
          object = df_out$df_top_plate,
          expected = df_rand$list_df_long$df_top_long$df_plate
        )

        expect_identical(
          object = df_out$df_top_qc_warn,
          expected = df_rand$list_df_long$df_top_long$df_qc_warn
        )

        expect_identical(
          object = df_out$df_top_int_ctrl,
          expected = df_rand$list_df_long$df_top_long$df_int_ctrl
        )

        expect_identical(
          object = df_out$df_top_dev_int_ctrl,
          expected = df_rand$list_df_long$df_top_long$df_dev_int_ctrl
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_top - error - missing or too many labels in rows 2 & 3",
  {
    # variables that apply to all tests
    olink_platform <- "Target 48"
    data_type <- "NPX"
    n_panels <- 1L
    n_assays <- 45L
    n_samples <- 88L
    show_dev_int_ctrl <- FALSE
    show_int_ctrl <- FALSE
    version <- 1L

    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = n_panels,
      n_assays = n_assays,
      n_samples = n_samples,
      show_dev_int_ctrl = show_dev_int_ctrl,
      show_int_ctrl = show_int_ctrl,
      version = version
    )

    # df contains unrecognizable tag ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # modify df_top_wide to add a new column with an unrecognizable tag
        df_top_wide_last_v <- ncol(df_rand$list_df_wide$df_top_wide)
        df_top_add_col_cname <- paste0("V", (df_top_wide_last_v + 1L))
        df_top_last_cname <- paste0("V", df_top_wide_last_v)
        df_top_add_col <- df_rand$list_df_wide$df_top_wide |>
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

        df_rand$list_df_wide$df_top_wide <- df_rand$list_df_wide$df_top_wide |>
          dplyr::bind_cols(
            df_top_add_col
          )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs with error
        expect_error(
          object = read_npx_wide_top(df = df_rand$list_df_wide$df_top_wide,
                                     file = olink_wide_format,
                                     olink_platform = olink_platform,
                                     format_spec = get_format_spec(
                                       data_type = data_type
                                     )),
          regexp = "in row `Assay` contains unrecognized values in columns: V49"
        )

      }
    )

    # df contains QC Warning when data_type Ct ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Ct"

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs with error
        expect_error(
          object = read_npx_wide_top(df = df_rand$list_df_wide$df_top_wide,
                                     file = olink_wide_format,
                                     olink_platform = olink_platform,
                                     format_spec = get_format_spec(
                                       data_type = data_type
                                     )),
          regexp = "in row `Assay` contains unrecognized values in columns: V48"
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_top - error - NAs in OlinkID/Uniprot/Assay",
  {
    # variables that apply to all tests
    olink_platform <- "Target 48"
    data_type <- "NPX"
    n_panels <- 1L
    n_assays <- 45L
    n_samples <- 88L
    show_int_ctrl <- FALSE
    show_dev_int_ctrl <- FALSE
    version <- 1L

    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = n_panels,
      n_assays = n_assays,
      n_samples = n_samples,
      show_dev_int_ctrl = show_dev_int_ctrl,
      show_int_ctrl = show_int_ctrl,
      version = version
    )

    # matrix specifications
    format_spec <- get_format_spec(data_type = data_type)

    ## OlinkID = NA 1 instance ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # modify dt_top_wide intrduce NA cells to test
        df_rand$list_df_wide$df_top_wide <- df_rand$list_df_wide$df_top_wide |>
          dplyr::mutate(
            V2 = dplyr::if_else(.data[["V2"]] %in% "Uniprot1",
                                NA_character_,
                                .data[["V2"]])
          )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs with error
        expect_error(
          object = read_npx_wide_top(df = df_rand$list_df_wide$df_top_wide,
                                     file = olink_wide_format,
                                     olink_platform = olink_platform,
                                     format_spec = format_spec),
          regexp = "Identified 1 empty cells!"
        )

      }
    )

    ## OlinkID = NA 4 instances ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # intrduce NA cells to test
        df_rand$list_df_wide$df_top_wide <- df_rand$list_df_wide$df_top_wide |>
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
        writeLines("foo", olink_wide_format)

        # check that function runs with error
        expect_error(
          object = read_npx_wide_top(df = df_rand$list_df_wide$df_top_wide,
                                     file = olink_wide_format,
                                     olink_platform = olink_platform,
                                     format_spec = format_spec),
          regexp = "Identified 4 empty cells!"
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_top - error - wrong # of assays",
  {
    # variables that apply to all tests
    olink_platform <- "Target 48"
    data_type <- "NPX"
    n_assays <- 45L
    n_samples <- 88L
    show_int_ctrl <- FALSE
    show_dev_int_ctrl <- FALSE
    version <- 1L

    # matrix specifications
    format_spec <- get_format_spec(data_type = data_type)

    ## T96 1 panel ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = 1L,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = show_int_ctrl,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_error(
          object = read_npx_wide_top(df = df_rand$list_df_wide$df_top_wide,
                                     file = olink_wide_format,
                                     olink_platform = "Target 96",
                                     format_spec = format_spec),
          regexp = "Detected 45 assays in 1 panels in file"
        )

      }
    )

    ## T96 3 panels ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = 3L,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = show_int_ctrl,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_error(
          object = read_npx_wide_top(df = df_rand$list_df_wide$df_top_wide,
                                     file = olink_wide_format,
                                     olink_platform = "Target 96",
                                     format_spec = format_spec),
          regexp = "Detected 135 assays in 3 panels in file"
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_top - error - uneven # of Plate ID and QC_Warning cols",
  {
    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        olink_platform <- "Target 48"
        data_type <- "NPX"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = 3L,
          n_assays = 45L,
          n_samples = 88L,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = FALSE,
          version = 1L
        )

        # remove one column from df_top to reproduce the error
        remove_col <- colnames(df_rand$list_df_wide$df_top_wide) |> tail(1L)

        df_rand$list_df_wide$df_top_wide <- df_rand$list_df_wide$df_top_wide |>
          dplyr::select(
            -dplyr::all_of(remove_col)
          )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_error(
          object = read_npx_wide_top(df = df_rand$list_df_wide$df_top_wide,
                                     file = olink_wide_format,
                                     olink_platform = olink_platform,
                                     format_spec = get_format_spec(
                                       data_type = data_type
                                     )),
          regexp = "Expected equal number of `Plate ID` and `QC\ Warning`"
        )

      }
    )

  }
)

# Test read_npx_wide_middle ----

test_that(
  "read_npx_wide_middle - works - single panel",
  {
    # variables that apply to all tests
    olink_platform <- "Target 48"
    n_panels <- 1L
    n_assays <- 45L
    n_samples <- 88L

    ## NPX no int ctrl and no dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = FALSE,
          version = 1L
        )

        # column names of each subset of data
        col_names <- sapply(df_rand$list_df_long$df_top_long,
                            function(x) x$col_index)
        col_names <- col_names[which(lapply(col_names, length) != 0L)]
        names(col_names) <- strsplit(x = names(col_names), split = "_") |>
          lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
          unlist()
        names(col_names) <- paste0("df_top_", names(col_names))

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_middle(
            df = df_rand$list_df_wide$df_middle_wide,
            file = olink_wide_format,
            data_type = data_type,
            col_names = col_names
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_mid_oid,
          expected = df_rand$list_df_long$df_middle_long$df_oid
        )

        expect_identical(
          object = df_out$df_mid_plate,
          expected = df_rand$list_df_long$df_middle_long$df_plate
        )

        expect_identical(
          object = df_out$df_mid_qc_warn,
          expected = df_rand$list_df_long$df_middle_long$df_qc_warn
        )

        expect_false(
          object = "df_mid_int_ctrl" %in% names(df_out)
        )

        expect_false(
          object = "df_int_ctrl" %in% names(df_rand$list_df_long$df_middle_long)
        )

        expect_false(
          object = "df_mid_dev_int_ctrl" %in% names(df_out)
        )

        expect_false(
          object = "df_dev_int_ctrl" %in%
            names(df_rand$list_df_long$df_middle_long)
        )

      }
    )

    ## NPX with int ctrl and no dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = TRUE,
          version = 1L
        )

        # column names of each subset of data
        col_names <- sapply(df_rand$list_df_long$df_top_long,
                            function(x) x$col_index)
        col_names <- col_names[which(lapply(col_names, length) != 0L)]
        names(col_names) <- strsplit(x = names(col_names), split = "_") |>
          lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
          unlist()
        names(col_names) <- paste0("df_top_", names(col_names))

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_middle(
            df = df_rand$list_df_wide$df_middle_wide,
            file = olink_wide_format,
            data_type = data_type,
            col_names = col_names
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_mid_oid,
          expected = df_rand$list_df_long$df_middle_long$df_oid
        )

        expect_identical(
          object = df_out$df_mid_plate,
          expected = df_rand$list_df_long$df_middle_long$df_plate
        )

        expect_identical(
          object = df_out$df_mid_qc_warn,
          expected = df_rand$list_df_long$df_middle_long$df_qc_warn
        )

        expect_identical(
          object = df_out$df_mid_int_ctrl,
          expected = df_rand$list_df_long$df_middle_long$df_int_ctrl
        )

        expect_false(
          object = "df_mid_dev_int_ctrl" %in% names(df_out)
        )

        expect_false(
          object = "df_dev_int_ctrl" %in%
            names(df_rand$list_df_long$df_middle_long)
        )

      }
    )

    ## NPX no int ctrl and with dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = TRUE,
          show_int_ctrl = FALSE,
          version = 1L
        )

        # column names of each subset of data
        col_names <- sapply(df_rand$list_df_long$df_top_long,
                            function(x) x$col_index)
        col_names <- col_names[which(lapply(col_names, length) != 0L)]
        names(col_names) <- strsplit(x = names(col_names), split = "_") |>
          lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
          unlist()
        names(col_names) <- paste0("df_top_", names(col_names))

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_middle(
            df = df_rand$list_df_wide$df_middle_wide,
            file = olink_wide_format,
            data_type = data_type,
            col_names = col_names
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_mid_oid,
          expected = df_rand$list_df_long$df_middle_long$df_oid
        )

        expect_identical(
          object = df_out$df_mid_plate,
          expected = df_rand$list_df_long$df_middle_long$df_plate
        )

        expect_identical(
          object = df_out$df_mid_qc_warn,
          expected = df_rand$list_df_long$df_middle_long$df_qc_warn
        )

        expect_false(
          object = "df_mid_int_ctrl" %in% names(df_out)
        )

        expect_false(
          object = "df_int_ctrl" %in%
            names(df_rand$list_df_long$df_middle_long)
        )

        expect_identical(
          object = df_out$df_mid_dev_int_ctrl,
          expected = df_rand$list_df_long$df_middle_long$df_dev_int_ctrl
        )

      }
    )

    ## NPX with int ctrl and with dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = TRUE,
          show_int_ctrl = TRUE,
          version = 1L
        )

        # column names of each subset of data
        col_names <- sapply(df_rand$list_df_long$df_top_long,
                            function(x) x$col_index)
        col_names <- col_names[which(lapply(col_names, length) != 0L)]
        names(col_names) <- strsplit(x = names(col_names), split = "_") |>
          lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
          unlist()
        names(col_names) <- paste0("df_top_", names(col_names))

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_middle(
            df = df_rand$list_df_wide$df_middle_wide,
            file = olink_wide_format,
            data_type = data_type,
            col_names = col_names
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_mid_oid,
          expected = df_rand$list_df_long$df_middle_long$df_oid
        )

        expect_identical(
          object = df_out$df_mid_plate,
          expected = df_rand$list_df_long$df_middle_long$df_plate
        )

        expect_identical(
          object = df_out$df_mid_qc_warn,
          expected = df_rand$list_df_long$df_middle_long$df_qc_warn
        )

        expect_identical(
          object = df_out$df_mid_int_ctrl,
          expected = df_rand$list_df_long$df_middle_long$df_int_ctrl
        )

        expect_identical(
          object = df_out$df_mid_dev_int_ctrl,
          expected = df_rand$list_df_long$df_middle_long$df_dev_int_ctrl
        )

      }
    )


    ## Ct no int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Ct"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = FALSE,
          version = 0L
        )

        # column names of each subset of data
        col_names <- sapply(df_rand$list_df_long$df_top_long,
                            function(x) x$col_index)
        col_names <- col_names[which(lapply(col_names, length) != 0L)]
        names(col_names) <- strsplit(x = names(col_names), split = "_") |>
          lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
          unlist()
        names(col_names) <- paste0("df_top_", names(col_names))

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_middle(
            df = df_rand$list_df_wide$df_middle_wide,
            file = olink_wide_format,
            data_type = data_type,
            col_names = col_names
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_mid_oid,
          expected = df_rand$list_df_long$df_middle_long$df_oid
        )

        expect_identical(
          object = df_out$df_mid_plate,
          expected = df_rand$list_df_long$df_middle_long$df_plate
        )

        expect_false(
          object = "df_mid_qc_warn" %in% names(df_out)
        )

        expect_false(
          object = "df_qc_warn" %in% names(df_rand$list_df_long$df_middle_long)
        )

        expect_false(
          object = "df_mid_int_ctrl" %in% names(df_out)
        )

        expect_false(
          object = "df_int_ctrl" %in% names(df_rand$list_df_long$df_middle_long)
        )

        expect_false(
          object = "df_mid_dev_int_ctrl" %in% names(df_out)
        )

        expect_false(
          object = "df_dev_int_ctrl" %in%
            names(df_rand$list_df_long$df_middle_long)
        )

      }
    )

    ## Ct with int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Ct"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = TRUE,
          version = 0L
        )

        # column names of each subset of data
        col_names <- sapply(df_rand$list_df_long$df_top_long,
                            function(x) x$col_index)
        col_names <- col_names[which(lapply(col_names, length) != 0L)]
        names(col_names) <- strsplit(x = names(col_names), split = "_") |>
          lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
          unlist()
        names(col_names) <- paste0("df_top_", names(col_names))

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_middle(
            df = df_rand$list_df_wide$df_middle_wide,
            file = olink_wide_format,
            data_type = data_type,
            col_names = col_names
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_mid_oid,
          expected = df_rand$list_df_long$df_middle_long$df_oid
        )

        expect_identical(
          object = df_out$df_mid_plate,
          expected = df_rand$list_df_long$df_middle_long$df_plate
        )

        expect_false(
          object = "df_mid_qc_warn" %in% names(df_out)
        )

        expect_false(
          object = "df_qc_warn" %in% names(df_rand$list_df_long$df_middle_long)
        )

        expect_identical(
          object = df_out$df_mid_int_ctrl,
          expected = df_rand$list_df_long$df_middle_long$df_int_ctrl
        )

        expect_false(
          object = "df_mid_dev_int_ctrl" %in% names(df_out)
        )

        expect_false(
          object = "df_dev_int_ctrl" %in%
            names(df_rand$list_df_long$df_middle_long)
        )

      }
    )

    ## Quantified no int ctrl and no dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Quantified"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = FALSE,
          version = 0L
        )

        # column names of each subset of data
        col_names <- sapply(df_rand$list_df_long$df_top_long,
                            function(x) x$col_index)
        col_names <- col_names[which(lapply(col_names, length) != 0L)]
        names(col_names) <- strsplit(x = names(col_names), split = "_") |>
          lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
          unlist()
        names(col_names) <- paste0("df_top_", names(col_names))

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_middle(
            df = df_rand$list_df_wide$df_middle_wide,
            file = olink_wide_format,
            data_type = data_type,
            col_names = col_names
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_mid_oid,
          expected = df_rand$list_df_long$df_middle_long$df_oid
        )

        expect_identical(
          object = df_out$df_mid_plate,
          expected = df_rand$list_df_long$df_middle_long$df_plate
        )

        expect_identical(
          object = df_out$df_mid_qc_warn,
          expected = df_rand$list_df_long$df_middle_long$df_qc_warn
        )

        expect_false(
          object = "df_mid_int_ctrl" %in% names(df_out)
        )

        expect_false(
          object = "df_int_ctrl" %in% names(df_rand$list_df_long$df_middle_long)
        )

        expect_false(
          object = "df_mid_dev_int_ctrl" %in% names(df_out)
        )

        expect_false(
          object = "df_dev_int_ctrl" %in%
            names(df_rand$list_df_long$df_middle_long)
        )

      }
    )

    ## Quantified with int ctrl and no dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Quantified"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = TRUE,
          version = 0L
        )

        # column names of each subset of data
        col_names <- sapply(df_rand$list_df_long$df_top_long,
                            function(x) x$col_index)
        col_names <- col_names[which(lapply(col_names, length) != 0L)]
        names(col_names) <- strsplit(x = names(col_names), split = "_") |>
          lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
          unlist()
        names(col_names) <- paste0("df_top_", names(col_names))

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_middle(
            df = df_rand$list_df_wide$df_middle_wide,
            file = olink_wide_format,
            data_type = data_type,
            col_names = col_names
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_mid_oid,
          expected = df_rand$list_df_long$df_middle_long$df_oid
        )

        expect_identical(
          object = df_out$df_mid_plate,
          expected = df_rand$list_df_long$df_middle_long$df_plate
        )

        expect_identical(
          object = df_out$df_mid_qc_warn,
          expected = df_rand$list_df_long$df_middle_long$df_qc_warn
        )

        expect_identical(
          object = df_out$df_mid_int_ctrl,
          expected = df_rand$list_df_long$df_middle_long$df_int_ctrl
        )

        expect_false(
          object = "df_mid_dev_int_ctrl" %in% names(df_out)
        )

        expect_false(
          object = "df_dev_int_ctrl" %in%
            names(df_rand$list_df_long$df_middle_long)
        )

      }
    )

    ## Quantified no int ctrl and with dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Quantified"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = TRUE,
          show_int_ctrl = FALSE,
          version = 0L
        )

        # column names of each subset of data
        col_names <- sapply(df_rand$list_df_long$df_top_long,
                            function(x) x$col_index)
        col_names <- col_names[which(lapply(col_names, length) != 0L)]
        names(col_names) <- strsplit(x = names(col_names), split = "_") |>
          lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
          unlist()
        names(col_names) <- paste0("df_top_", names(col_names))

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_middle(
            df = df_rand$list_df_wide$df_middle_wide,
            file = olink_wide_format,
            data_type = data_type,
            col_names = col_names
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_mid_oid,
          expected = df_rand$list_df_long$df_middle_long$df_oid
        )

        expect_identical(
          object = df_out$df_mid_plate,
          expected = df_rand$list_df_long$df_middle_long$df_plate
        )

        expect_identical(
          object = df_out$df_mid_qc_warn,
          expected = df_rand$list_df_long$df_middle_long$df_qc_warn
        )

        expect_false(
          object = "df_mid_int_ctrl" %in% names(df_out)
        )

        expect_false(
          object = "df_int_ctrl" %in%
            names(df_rand$list_df_long$df_middle_long)
        )

        expect_identical(
          object = df_out$df_mid_dev_int_ctrl,
          expected = df_rand$list_df_long$df_middle_long$df_dev_int_ctrl
        )

      }
    )

    ## Quantified with int ctrl and with dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Quantified"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = TRUE,
          show_int_ctrl = TRUE,
          version = 0L
        )

        # column names of each subset of data
        col_names <- sapply(df_rand$list_df_long$df_top_long,
                            function(x) x$col_index)
        col_names <- col_names[which(lapply(col_names, length) != 0L)]
        names(col_names) <- strsplit(x = names(col_names), split = "_") |>
          lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
          unlist()
        names(col_names) <- paste0("df_top_", names(col_names))

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_middle(
            df = df_rand$list_df_wide$df_middle_wide,
            file = olink_wide_format,
            data_type = data_type,
            col_names = col_names
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_mid_oid,
          expected = df_rand$list_df_long$df_middle_long$df_oid
        )

        expect_identical(
          object = df_out$df_mid_plate,
          expected = df_rand$list_df_long$df_middle_long$df_plate
        )

        expect_identical(
          object = df_out$df_mid_qc_warn,
          expected = df_rand$list_df_long$df_middle_long$df_qc_warn
        )

        expect_identical(
          object = df_out$df_mid_int_ctrl,
          expected = df_rand$list_df_long$df_middle_long$df_int_ctrl
        )

        expect_identical(
          object = df_out$df_mid_dev_int_ctrl,
          expected = df_rand$list_df_long$df_middle_long$df_dev_int_ctrl
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_middle - works - multiple panels",
  {
    # variables that apply to all tests
    olink_platform <- "Target 48"
    n_panels <- 3L
    n_assays <- 45L
    n_samples <- 88L

    ## NPX no int ctrl and no dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = FALSE,
          version = 1L
        )

        # column names of each subset of data
        col_names <- sapply(df_rand$list_df_long$df_top_long,
                            function(x) x$col_index)
        col_names <- col_names[which(lapply(col_names, length) != 0L)]
        names(col_names) <- strsplit(x = names(col_names), split = "_") |>
          lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
          unlist()
        names(col_names) <- paste0("df_top_", names(col_names))

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_middle(
            df = df_rand$list_df_wide$df_middle_wide,
            file = olink_wide_format,
            data_type = data_type,
            col_names = col_names
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_mid_oid,
          expected = df_rand$list_df_long$df_middle_long$df_oid
        )

        expect_identical(
          object = df_out$df_mid_plate,
          expected = df_rand$list_df_long$df_middle_long$df_plate
        )

        expect_identical(
          object = df_out$df_mid_qc_warn,
          expected = df_rand$list_df_long$df_middle_long$df_qc_warn
        )

        expect_false(
          object = "df_mid_int_ctrl" %in% names(df_out)
        )

        expect_false(
          object = "df_int_ctrl" %in% names(df_rand$list_df_long$df_middle_long)
        )

        expect_false(
          object = "df_mid_dev_int_ctrl" %in% names(df_out)
        )

        expect_false(
          object = "df_dev_int_ctrl" %in%
            names(df_rand$list_df_long$df_middle_long)
        )

      }
    )

    ## NPX with int ctrl and no dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = TRUE,
          version = 1L
        )

        # column names of each subset of data
        col_names <- sapply(df_rand$list_df_long$df_top_long,
                            function(x) x$col_index)
        col_names <- col_names[which(lapply(col_names, length) != 0L)]
        names(col_names) <- strsplit(x = names(col_names), split = "_") |>
          lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
          unlist()
        names(col_names) <- paste0("df_top_", names(col_names))

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_middle(
            df = df_rand$list_df_wide$df_middle_wide,
            file = olink_wide_format,
            data_type = data_type,
            col_names = col_names
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_mid_oid,
          expected = df_rand$list_df_long$df_middle_long$df_oid
        )

        expect_identical(
          object = df_out$df_mid_plate,
          expected = df_rand$list_df_long$df_middle_long$df_plate
        )

        expect_identical(
          object = df_out$df_mid_qc_warn,
          expected = df_rand$list_df_long$df_middle_long$df_qc_warn
        )

        expect_identical(
          object = df_out$df_mid_int_ctrl,
          expected = df_rand$list_df_long$df_middle_long$df_int_ctrl
        )

        expect_false(
          object = "df_mid_dev_int_ctrl" %in% names(df_out)
        )

        expect_false(
          object = "df_dev_int_ctrl" %in%
            names(df_rand$list_df_long$df_middle_long)
        )

      }
    )

    ## NPX no int ctrl and with dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = TRUE,
          show_int_ctrl = FALSE,
          version = 1L
        )

        # column names of each subset of data
        col_names <- sapply(df_rand$list_df_long$df_top_long,
                            function(x) x$col_index)
        col_names <- col_names[which(lapply(col_names, length) != 0L)]
        names(col_names) <- strsplit(x = names(col_names), split = "_") |>
          lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
          unlist()
        names(col_names) <- paste0("df_top_", names(col_names))

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_middle(
            df = df_rand$list_df_wide$df_middle_wide,
            file = olink_wide_format,
            data_type = data_type,
            col_names = col_names
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_mid_oid,
          expected = df_rand$list_df_long$df_middle_long$df_oid
        )

        expect_identical(
          object = df_out$df_mid_plate,
          expected = df_rand$list_df_long$df_middle_long$df_plate
        )

        expect_identical(
          object = df_out$df_mid_qc_warn,
          expected = df_rand$list_df_long$df_middle_long$df_qc_warn
        )

        expect_false(
          object = "df_mid_int_ctrl" %in% names(df_out)
        )

        expect_false(
          object = "df_int_ctrl" %in%
            names(df_rand$list_df_long$df_middle_long)
        )

        expect_identical(
          object = df_out$df_mid_dev_int_ctrl,
          expected = df_rand$list_df_long$df_middle_long$df_dev_int_ctrl
        )

      }
    )

    ## NPX with int ctrl and with dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = TRUE,
          show_int_ctrl = TRUE,
          version = 1L
        )

        # column names of each subset of data
        col_names <- sapply(df_rand$list_df_long$df_top_long,
                            function(x) x$col_index)
        col_names <- col_names[which(lapply(col_names, length) != 0L)]
        names(col_names) <- strsplit(x = names(col_names), split = "_") |>
          lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
          unlist()
        names(col_names) <- paste0("df_top_", names(col_names))

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_middle(
            df = df_rand$list_df_wide$df_middle_wide,
            file = olink_wide_format,
            data_type = data_type,
            col_names = col_names
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_mid_oid,
          expected = df_rand$list_df_long$df_middle_long$df_oid
        )

        expect_identical(
          object = df_out$df_mid_plate,
          expected = df_rand$list_df_long$df_middle_long$df_plate
        )

        expect_identical(
          object = df_out$df_mid_qc_warn,
          expected = df_rand$list_df_long$df_middle_long$df_qc_warn
        )

        expect_identical(
          object = df_out$df_mid_int_ctrl,
          expected = df_rand$list_df_long$df_middle_long$df_int_ctrl
        )

        expect_identical(
          object = df_out$df_mid_dev_int_ctrl,
          expected = df_rand$list_df_long$df_middle_long$df_dev_int_ctrl
        )

      }
    )


    ## Ct no int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Ct"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = FALSE,
          version = 0L
        )

        # column names of each subset of data
        col_names <- sapply(df_rand$list_df_long$df_top_long,
                            function(x) x$col_index)
        col_names <- col_names[which(lapply(col_names, length) != 0L)]
        names(col_names) <- strsplit(x = names(col_names), split = "_") |>
          lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
          unlist()
        names(col_names) <- paste0("df_top_", names(col_names))

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_middle(
            df = df_rand$list_df_wide$df_middle_wide,
            file = olink_wide_format,
            data_type = data_type,
            col_names = col_names
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_mid_oid,
          expected = df_rand$list_df_long$df_middle_long$df_oid
        )

        expect_identical(
          object = df_out$df_mid_plate,
          expected = df_rand$list_df_long$df_middle_long$df_plate
        )

        expect_false(
          object = "df_mid_qc_warn" %in% names(df_out)
        )

        expect_false(
          object = "df_qc_warn" %in% names(df_rand$list_df_long$df_middle_long)
        )

        expect_false(
          object = "df_mid_int_ctrl" %in% names(df_out)
        )

        expect_false(
          object = "df_int_ctrl" %in% names(df_rand$list_df_long$df_middle_long)
        )

        expect_false(
          object = "df_mid_dev_int_ctrl" %in% names(df_out)
        )

        expect_false(
          object = "df_dev_int_ctrl" %in%
            names(df_rand$list_df_long$df_middle_long)
        )

      }
    )

    ## Ct with int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Ct"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = TRUE,
          version = 0L
        )

        # column names of each subset of data
        col_names <- sapply(df_rand$list_df_long$df_top_long,
                            function(x) x$col_index)
        col_names <- col_names[which(lapply(col_names, length) != 0L)]
        names(col_names) <- strsplit(x = names(col_names), split = "_") |>
          lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
          unlist()
        names(col_names) <- paste0("df_top_", names(col_names))

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_middle(
            df = df_rand$list_df_wide$df_middle_wide,
            file = olink_wide_format,
            data_type = data_type,
            col_names = col_names
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_mid_oid,
          expected = df_rand$list_df_long$df_middle_long$df_oid
        )

        expect_identical(
          object = df_out$df_mid_plate,
          expected = df_rand$list_df_long$df_middle_long$df_plate
        )

        expect_false(
          object = "df_mid_qc_warn" %in% names(df_out)
        )

        expect_false(
          object = "df_qc_warn" %in% names(df_rand$list_df_long$df_middle_long)
        )

        expect_identical(
          object = df_out$df_mid_int_ctrl,
          expected = df_rand$list_df_long$df_middle_long$df_int_ctrl
        )

        expect_false(
          object = "df_mid_dev_int_ctrl" %in% names(df_out)
        )

        expect_false(
          object = "df_dev_int_ctrl" %in%
            names(df_rand$list_df_long$df_middle_long)
        )

      }
    )

    ## Quantified no int ctrl and no dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Quantified"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = FALSE,
          version = 0L
        )

        # column names of each subset of data
        col_names <- sapply(df_rand$list_df_long$df_top_long,
                            function(x) x$col_index)
        col_names <- col_names[which(lapply(col_names, length) != 0L)]
        names(col_names) <- strsplit(x = names(col_names), split = "_") |>
          lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
          unlist()
        names(col_names) <- paste0("df_top_", names(col_names))

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_middle(
            df = df_rand$list_df_wide$df_middle_wide,
            file = olink_wide_format,
            data_type = data_type,
            col_names = col_names
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_mid_oid,
          expected = df_rand$list_df_long$df_middle_long$df_oid
        )

        expect_identical(
          object = df_out$df_mid_plate,
          expected = df_rand$list_df_long$df_middle_long$df_plate
        )

        expect_identical(
          object = df_out$df_mid_qc_warn,
          expected = df_rand$list_df_long$df_middle_long$df_qc_warn
        )

        expect_false(
          object = "df_mid_int_ctrl" %in% names(df_out)
        )

        expect_false(
          object = "df_int_ctrl" %in% names(df_rand$list_df_long$df_middle_long)
        )

        expect_false(
          object = "df_mid_dev_int_ctrl" %in% names(df_out)
        )

        expect_false(
          object = "df_dev_int_ctrl" %in%
            names(df_rand$list_df_long$df_middle_long)
        )

      }
    )

    ## Quantified with int ctrl and no dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Quantified"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = TRUE,
          version = 0L
        )

        # column names of each subset of data
        col_names <- sapply(df_rand$list_df_long$df_top_long,
                            function(x) x$col_index)
        col_names <- col_names[which(lapply(col_names, length) != 0L)]
        names(col_names) <- strsplit(x = names(col_names), split = "_") |>
          lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
          unlist()
        names(col_names) <- paste0("df_top_", names(col_names))

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_middle(
            df = df_rand$list_df_wide$df_middle_wide,
            file = olink_wide_format,
            data_type = data_type,
            col_names = col_names
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_mid_oid,
          expected = df_rand$list_df_long$df_middle_long$df_oid
        )

        expect_identical(
          object = df_out$df_mid_plate,
          expected = df_rand$list_df_long$df_middle_long$df_plate
        )

        expect_identical(
          object = df_out$df_mid_qc_warn,
          expected = df_rand$list_df_long$df_middle_long$df_qc_warn
        )

        expect_identical(
          object = df_out$df_mid_int_ctrl,
          expected = df_rand$list_df_long$df_middle_long$df_int_ctrl
        )

        expect_false(
          object = "df_mid_dev_int_ctrl" %in% names(df_out)
        )

        expect_false(
          object = "df_dev_int_ctrl" %in%
            names(df_rand$list_df_long$df_middle_long)
        )

      }
    )

    ## Quantified no int ctrl and with dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Quantified"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = TRUE,
          show_int_ctrl = FALSE,
          version = 0L
        )

        # column names of each subset of data
        col_names <- sapply(df_rand$list_df_long$df_top_long,
                            function(x) x$col_index)
        col_names <- col_names[which(lapply(col_names, length) != 0L)]
        names(col_names) <- strsplit(x = names(col_names), split = "_") |>
          lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
          unlist()
        names(col_names) <- paste0("df_top_", names(col_names))

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_middle(
            df = df_rand$list_df_wide$df_middle_wide,
            file = olink_wide_format,
            data_type = data_type,
            col_names = col_names
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_mid_oid,
          expected = df_rand$list_df_long$df_middle_long$df_oid
        )

        expect_identical(
          object = df_out$df_mid_plate,
          expected = df_rand$list_df_long$df_middle_long$df_plate
        )

        expect_identical(
          object = df_out$df_mid_qc_warn,
          expected = df_rand$list_df_long$df_middle_long$df_qc_warn
        )

        expect_false(
          object = "df_mid_int_ctrl" %in% names(df_out)
        )

        expect_false(
          object = "df_int_ctrl" %in%
            names(df_rand$list_df_long$df_middle_long)
        )

        expect_identical(
          object = df_out$df_mid_dev_int_ctrl,
          expected = df_rand$list_df_long$df_middle_long$df_dev_int_ctrl
        )

      }
    )

    ## Quantified with int ctrl and with dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Quantified"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = TRUE,
          show_int_ctrl = TRUE,
          version = 0L
        )

        # column names of each subset of data
        col_names <- sapply(df_rand$list_df_long$df_top_long,
                            function(x) x$col_index)
        col_names <- col_names[which(lapply(col_names, length) != 0L)]
        names(col_names) <- strsplit(x = names(col_names), split = "_") |>
          lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
          unlist()
        names(col_names) <- paste0("df_top_", names(col_names))

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_middle(
            df = df_rand$list_df_wide$df_middle_wide,
            file = olink_wide_format,
            data_type = data_type,
            col_names = col_names
          )
        )

        # check that output match
        expect_identical(
          object = df_out$df_mid_oid,
          expected = df_rand$list_df_long$df_middle_long$df_oid
        )

        expect_identical(
          object = df_out$df_mid_plate,
          expected = df_rand$list_df_long$df_middle_long$df_plate
        )

        expect_identical(
          object = df_out$df_mid_qc_warn,
          expected = df_rand$list_df_long$df_middle_long$df_qc_warn
        )

        expect_identical(
          object = df_out$df_mid_int_ctrl,
          expected = df_rand$list_df_long$df_middle_long$df_int_ctrl
        )

        expect_identical(
          object = df_out$df_mid_dev_int_ctrl,
          expected = df_rand$list_df_long$df_middle_long$df_dev_int_ctrl
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_middle - message - non-unique sample id",
  {
    # variables that apply to all tests
    olink_platform <- "Target 48"
    data_type <- "NPX"
    n_panels <- 3L
    n_assays <- 45L
    n_samples <- 99L
    show_int_ctrl <- TRUE
    show_dev_int_ctrl <- TRUE
    version <- 1L

    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = n_panels,
      n_assays = n_assays,
      n_samples = n_samples,
      show_dev_int_ctrl = show_dev_int_ctrl,
      show_int_ctrl = show_int_ctrl,
      version = version
    )

    # 1 duplicate ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # column names of each subset of data
        col_names <- sapply(df_rand$list_df_long$df_top_long,
                            function(x) x$col_index)
        col_names <- col_names[which(lapply(col_names, length) != 0L)]
        names(col_names) <- strsplit(x = names(col_names), split = "_") |>
          lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
          unlist()
        names(col_names) <- paste0("df_top_", names(col_names))

        # introduce duplicate sample id for the test
        df_rand$list_df_wide$df_middle_wide <-
          df_rand$list_df_wide$df_middle_wide |>
          dplyr::mutate(
            V1 = dplyr::if_else(.data[["V1"]] == "S2",
                                "S1",
                                .data[["V1"]])
          )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs with error
        expect_message(
          object = df_out <- read_npx_wide_middle(
            df = df_rand$list_df_wide$df_middle_wide,
            file = olink_wide_format,
            data_type = data_type,
            col_names = col_names
          ),
          regexp = "does not contain unique sample identifiers."
        )

        # check that output match

        # rename samples from the separate dfs of the reference input
        df_rand$list_df_long$df_middle_long <-
          lapply(df_rand$list_df_long$df_middle_long, function(.x) {
            .x |>
              dplyr::mutate(
                SampleID = dplyr::if_else(.data[["SampleID"]] == "S2",
                                          "S1",
                                          .data[["SampleID"]])
              )
          })

        expect_identical(
          object = df_out$df_mid_oid,
          expected = df_rand$list_df_long$df_middle_long$df_oid
        )

        expect_identical(
          object = df_out$df_mid_plate,
          expected = df_rand$list_df_long$df_middle_long$df_plate
        )

        expect_identical(
          object = df_out$df_mid_qc_warn,
          expected = df_rand$list_df_long$df_middle_long$df_qc_warn
        )

        expect_identical(
          object = df_out$df_mid_int_ctrl,
          expected = df_rand$list_df_long$df_middle_long$df_int_ctrl
        )

        expect_identical(
          object = df_out$df_mid_dev_int_ctrl,
          expected = df_rand$list_df_long$df_middle_long$df_dev_int_ctrl
        )

      }
    )

    # 3 duplicates ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # column names of each subset of data
        col_names <- sapply(df_rand$list_df_long$df_top_long,
                            function(x) x$col_index)
        col_names <- col_names[which(lapply(col_names, length) != 0L)]
        names(col_names) <- strsplit(x = names(col_names), split = "_") |>
          lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
          unlist()
        names(col_names) <- paste0("df_top_", names(col_names))

        # introduce duplicate sample id for the test
        df_rand$list_df_wide$df_middle_wide <-
          df_rand$list_df_wide$df_middle_wide |>
          dplyr::mutate(
            V1 = dplyr::case_when(.data[["V1"]] %in% c("S2", "S3") ~ "S1",
                                  .data[["V1"]] %in% c("S4", "S5") ~ "S2",
                                  TRUE ~ .data[["V1"]],
                                  .default = .data[["V1"]])
          )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs with error
        expect_message(
          object = df_out <- read_npx_wide_middle(
            df = df_rand$list_df_wide$df_middle_wide,
            file = olink_wide_format,
            data_type = data_type,
            col_names = col_names
          ),
          regexp = "does not contain unique sample identifiers."
        )

        # check that output match

        # rename samples from the separate dfs of the reference input
        df_rand$list_df_long$df_middle_long <-
          lapply(df_rand$list_df_long$df_middle_long, function(.x) {
            .x |>
              dplyr::mutate(
                SampleID = dplyr::case_when(
                  .data[["SampleID"]] %in% c("S2", "S3") ~ "S1",
                  .data[["SampleID"]] %in% c("S4", "S5") ~ "S2",
                  TRUE ~ .data[["SampleID"]],
                  .default = .data[["SampleID"]]
                )
              )
          })

        expect_identical(
          object = df_out$df_mid_oid,
          expected = df_rand$list_df_long$df_middle_long$df_oid
        )

        expect_identical(
          object = df_out$df_mid_plate,
          expected = df_rand$list_df_long$df_middle_long$df_plate
        )

        expect_identical(
          object = df_out$df_mid_qc_warn,
          expected = df_rand$list_df_long$df_middle_long$df_qc_warn
        )

        expect_identical(
          object = df_out$df_mid_int_ctrl,
          expected = df_rand$list_df_long$df_middle_long$df_int_ctrl
        )

        expect_identical(
          object = df_out$df_mid_dev_int_ctrl,
          expected = df_rand$list_df_long$df_middle_long$df_dev_int_ctrl
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_middle - error - uneven number of platid and qc_warning",
  {
    # variables that apply to all tests
    olink_platform <- "Target 48"
    data_type <- "NPX"
    n_panels <- 3L
    n_assays <- 45L
    n_samples <- 99L
    show_int_ctrl <- FALSE
    show_dev_int_ctrl <- FALSE
    version <- 1L

    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = n_panels,
      n_assays = n_assays,
      n_samples = n_samples,
      show_dev_int_ctrl = show_dev_int_ctrl,
      show_int_ctrl = show_int_ctrl,
      version = version
    )

    # Missing plateid column ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # column names of each subset of data
        col_names <- sapply(df_rand$list_df_long$df_top_long,
                            function(x) x$col_index)
        col_names <- col_names[which(lapply(col_names, length) != 0L)]
        names(col_names) <- strsplit(x = names(col_names), split = "_") |>
          lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
          unlist()
        names(col_names) <- paste0("df_top_", names(col_names))

        # remove one column with "Plate ID"
        df_rand$list_df_wide$df_middle_wide <-
          df_rand$list_df_wide$df_middle_wide |>
          dplyr::select(
            -dplyr::all_of(head(col_names$df_top_plate, 1L))
          )
        col_names$df_top_plate <- tail(col_names$df_top_plate, 1L)

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs with error
        expect_error(
          object = read_npx_wide_middle(
            df = df_rand$list_df_wide$df_middle_wide,
            file = olink_wide_format,
            data_type = data_type,
            col_names = col_names
          ),
          regexp = "Uneven number of entries of \"Plate ID\" and \"QC Warning\""
        )

      }
    )

    # Missing qc warning column ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # column names of each subset of data
        col_names <- sapply(df_rand$list_df_long$df_top_long,
                            function(x) x$col_index)
        col_names <- col_names[which(lapply(col_names, length) != 0L)]
        names(col_names) <- strsplit(x = names(col_names), split = "_") |>
          lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
          unlist()
        names(col_names) <- paste0("df_top_", names(col_names))

        # remove one column with "Plate ID"
        df_rand$list_df_wide$df_middle_wide <-
          df_rand$list_df_wide$df_middle_wide |>
          dplyr::select(
            -dplyr::all_of(head(col_names$df_top_qc_warn, 1L))
          )
        col_names$df_top_qc_warn <- tail(col_names$df_top_qc_warn, 1L)

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs with error
        expect_error(
          object = read_npx_wide_middle(
            df = df_rand$list_df_wide$df_middle_wide,
            file = olink_wide_format,
            data_type = data_type,
            col_names = col_names
          ),
          regexp = "Uneven number of entries of \"Plate ID\" and \"QC Warning\""
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_middle - error - unidentified cols in mid matrix",
  {
    # variables that apply to all tests
    olink_platform <- "Target 48"
    data_type <- "NPX"
    n_panels <- 3L
    n_assays <- 45L
    n_samples <- 99L
    show_int_ctrl <- FALSE
    show_dev_int_ctrl <- FALSE
    version <- 1L

    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = n_panels,
      n_assays = n_assays,
      n_samples = n_samples,
      show_dev_int_ctrl = show_dev_int_ctrl,
      show_int_ctrl = show_int_ctrl,
      version = version
    )

    # 1 additional column ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # column names of each subset of data
        col_names <- sapply(df_rand$list_df_long$df_top_long,
                            function(x) x$col_index)
        col_names <- col_names[which(lapply(col_names, length) != 0L)]
        names(col_names) <- strsplit(x = names(col_names), split = "_") |>
          lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
          unlist()
        names(col_names) <- paste0("df_top_", names(col_names))

        # add one column to mid wide
        last_col_idx <- ncol(df_rand$list_df_wide$df_middle_wide) + 1L
        last_col <- paste0("V", last_col_idx)
        df_rand$list_df_wide$df_middle_wide <-
          df_rand$list_df_wide$df_middle_wide |>
          dplyr::mutate(
            {{last_col}} := NA
          )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs with error
        expect_error(
          object = read_npx_wide_middle(
            df = df_rand$list_df_wide$df_middle_wide,
            file = olink_wide_format,
            data_type = data_type,
            col_names = col_names
          ),
          regexp = "143 from the Olink wide format file"
        )

      }
    )

    # 3 additional column ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # column names of each subset of data
        col_names <- sapply(df_rand$list_df_long$df_top_long,
                            function(x) x$col_index)
        col_names <- col_names[which(lapply(col_names, length) != 0L)]
        names(col_names) <- strsplit(x = names(col_names), split = "_") |>
          lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
          unlist()
        names(col_names) <- paste0("df_top_", names(col_names))

        # add one column to mid wide
        last_col_idx <- ncol(df_rand$list_df_wide$df_middle_wide) + 1L
        last_col_idx <- last_col_idx:(last_col_idx + 2L)
        last_col <- paste0("V", last_col_idx)
        rep_col <- colnames(df_rand$list_df_wide$df_middle_wide) |> tail(1L)
        df_rand$list_df_wide$df_middle_wide[last_col] <- NA

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs with error
        expect_error(
          object = read_npx_wide_middle(
            df = df_rand$list_df_wide$df_middle_wide,
            file = olink_wide_format,
            data_type = data_type,
            col_names = col_names
          ),
          regexp = "143, 144, and 145 from the Olink wide format file"
        )

      }
    )

  }
)

# Test red_npx_wide_top_mid_long ----

test_that(
  "red_npx_wide_top_mid_long - works - single panel - single plate",
  {
    # variables that apply to all tests
    olink_platform <- "Target 48"
    n_panels <- 1L
    n_assays <- 45L
    n_samples <- 88L

    ## NPX no int ctrl and no dev int ctrl ----

    # synthetic wide df
    data_type <- "NPX"
    show_int_ctrl <- FALSE

    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = n_panels,
      n_assays = n_assays,
      n_samples = n_samples,
      show_dev_int_ctrl = FALSE,
      show_int_ctrl = show_int_ctrl,
      version = 1L
    )

    # column names of each subset of data
    names(df_rand$list_df_long$df_top_long) <- strsplit(
      x = names(df_rand$list_df_long$df_top_long),
      split = "_"
    ) |>
      lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
      unlist() |>
      (\(x) paste0("df_top_", x))()
    names(df_rand$list_df_long$df_middle_long) <- strsplit(
      x = names(df_rand$list_df_long$df_middle_long),
      split = "_"
    ) |>
      lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
      unlist() |>
      (\(x) paste0("df_mid_", x))()

    # expected dim of output df
    n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                      n_assays = n_assays,
                                      n_samples = n_samples,
                                      has_int_ctrl = show_int_ctrl,
                                      num_int_ctrl = 3L)

    # check that function runs
    expect_no_condition(
      object = df_out <- red_npx_wide_top_mid_long(
        df_top_list = df_rand$list_df_long$df_top_long,
        df_middle_list = df_rand$list_df_long$df_middle_long,
        data_type = data_type,
        format_spec = get_format_spec(data_type = data_type)
      )
    )

    # rename from NPXS
    olink_wide_rename_npxs_tmp <- olink_wide_rename_npxs |>
      dplyr::filter(
        .data[["OA_internal"]] %in% colnames(df_out)
      )

    # check that output match
    expect_true(
      object = identical(nrow(df_out), n_row_exp)
    )

    expect_identical(
      object = dplyr::select(df_out,
                             -dplyr::all_of("col_index")),
      expected = df_rand$list_df_long$df_long |>
        dplyr::rename_with(
          .fn = ~olink_wide_rename_npxs_tmp$OA_internal,
          .cols = dplyr::all_of(olink_wide_rename_npxs_tmp$NPXS)
        ) |>
        dplyr::select(dplyr::any_of(colnames(df_out)))
    )

    ## NPX with int ctrl and no dev int ctrl ----

    # synthetic wide df
    data_type <- "NPX"
    show_int_ctrl <- TRUE

    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = n_panels,
      n_assays = n_assays,
      n_samples = n_samples,
      show_dev_int_ctrl = FALSE,
      show_int_ctrl = show_int_ctrl,
      version = 1L
    )

    # column names of each subset of data
    names(df_rand$list_df_long$df_top_long) <- strsplit(
      x = names(df_rand$list_df_long$df_top_long),
      split = "_"
    ) |>
      lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
      unlist() |>
      (\(x) paste0("df_top_", x))()
    names(df_rand$list_df_long$df_middle_long) <- strsplit(
      x = names(df_rand$list_df_long$df_middle_long),
      split = "_"
    ) |>
      lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
      unlist() |>
      (\(x) paste0("df_mid_", x))()

    # expected dim of output df
    n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                      n_assays = n_assays,
                                      n_samples = n_samples,
                                      has_int_ctrl = show_int_ctrl,
                                      num_int_ctrl = 3L)

    # check that function runs
    expect_no_condition(
      object = df_out <- red_npx_wide_top_mid_long(
        df_top_list = df_rand$list_df_long$df_top_long,
        df_middle_list = df_rand$list_df_long$df_middle_long,
        data_type = data_type,
        format_spec = get_format_spec(data_type = data_type)
      )
    )

    # rename from NPXS
    olink_wide_rename_npxs_tmp <- olink_wide_rename_npxs |>
      dplyr::filter(
        .data[["OA_internal"]] %in% colnames(df_out)
      )

    # check that output match
    expect_true(
      object = identical(nrow(df_out), n_row_exp)
    )

    expect_identical(
      object = dplyr::select(df_out,
                             -dplyr::all_of("col_index")),
      expected = df_rand$list_df_long$df_long |>
        dplyr::rename_with(
          .fn = ~olink_wide_rename_npxs_tmp$OA_internal,
          .cols = dplyr::all_of(olink_wide_rename_npxs_tmp$NPXS)
        ) |>
        dplyr::select(dplyr::any_of(colnames(df_out)))
    )

    ## NPX no int ctrl and with dev int ctrl ----

    # synthetic wide df
    data_type <- "NPX"
    show_int_ctrl <- FALSE

    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = n_panels,
      n_assays = n_assays,
      n_samples = n_samples,
      show_dev_int_ctrl = TRUE,
      show_int_ctrl = show_int_ctrl,
      version = 1L
    )

    # column names of each subset of data
    names(df_rand$list_df_long$df_top_long) <- strsplit(
      x = names(df_rand$list_df_long$df_top_long),
      split = "_"
    ) |>
      lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
      unlist() |>
      (\(x) paste0("df_top_", x))()
    names(df_rand$list_df_long$df_middle_long) <- strsplit(
      x = names(df_rand$list_df_long$df_middle_long),
      split = "_"
    ) |>
      lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
      unlist() |>
      (\(x) paste0("df_mid_", x))()

    # expected dim of output df
    n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                      n_assays = n_assays,
                                      n_samples = n_samples,
                                      has_int_ctrl = show_int_ctrl,
                                      num_int_ctrl = 3L)

    # check that function runs
    expect_no_condition(
      object = df_out <- red_npx_wide_top_mid_long(
        df_top_list = df_rand$list_df_long$df_top_long,
        df_middle_list = df_rand$list_df_long$df_middle_long,
        data_type = data_type,
        format_spec = get_format_spec(data_type = data_type)
      )
    )

    # rename from NPXS
    olink_wide_rename_npxs_tmp <- olink_wide_rename_npxs |>
      dplyr::filter(
        .data[["OA_internal"]] %in% colnames(df_out)
      )

    # check that output match
    expect_true(
      object = identical(nrow(df_out), n_row_exp)
    )

    expect_identical(
      object = dplyr::select(df_out,
                             -dplyr::all_of("col_index")),
      expected = df_rand$list_df_long$df_long |>
        dplyr::rename_with(
          .fn = ~olink_wide_rename_npxs_tmp$OA_internal,
          .cols = dplyr::all_of(olink_wide_rename_npxs_tmp$NPXS)
        ) |>
        dplyr::select(dplyr::any_of(colnames(df_out)))
    )

    ## NPX with int ctrl and with dev int ctrl ----

    # synthetic wide df
    data_type <- "NPX"
    show_int_ctrl <- TRUE

    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = n_panels,
      n_assays = n_assays,
      n_samples = n_samples,
      show_dev_int_ctrl = TRUE,
      show_int_ctrl = show_int_ctrl,
      version = 1L
    )

    # column names of each subset of data
    names(df_rand$list_df_long$df_top_long) <- strsplit(
      x = names(df_rand$list_df_long$df_top_long),
      split = "_"
    ) |>
      lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
      unlist() |>
      (\(x) paste0("df_top_", x))()
    names(df_rand$list_df_long$df_middle_long) <- strsplit(
      x = names(df_rand$list_df_long$df_middle_long),
      split = "_"
    ) |>
      lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
      unlist() |>
      (\(x) paste0("df_mid_", x))()

    # expected dim of output df
    n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                      n_assays = n_assays,
                                      n_samples = n_samples,
                                      has_int_ctrl = show_int_ctrl,
                                      num_int_ctrl = 3L)

    # check that function runs
    expect_no_condition(
      object = df_out <- red_npx_wide_top_mid_long(
        df_top_list = df_rand$list_df_long$df_top_long,
        df_middle_list = df_rand$list_df_long$df_middle_long,
        data_type = data_type,
        format_spec = get_format_spec(data_type = data_type)
      )
    )

    # rename from NPXS
    olink_wide_rename_npxs_tmp <- olink_wide_rename_npxs |>
      dplyr::filter(
        .data[["OA_internal"]] %in% colnames(df_out)
      )

    # check that output match
    expect_true(
      object = identical(nrow(df_out), n_row_exp)
    )

    expect_identical(
      object = dplyr::select(df_out,
                             -dplyr::all_of("col_index")),
      expected = df_rand$list_df_long$df_long |>
        dplyr::rename_with(
          .fn = ~olink_wide_rename_npxs_tmp$OA_internal,
          .cols = dplyr::all_of(olink_wide_rename_npxs_tmp$NPXS)
        ) |>
        dplyr::select(dplyr::any_of(colnames(df_out)))
    )


    ## Ct no int ctrl ----

    # synthetic wide df
    data_type <- "Ct"
    show_int_ctrl <- FALSE

    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = n_panels,
      n_assays = n_assays,
      n_samples = n_samples,
      show_dev_int_ctrl = FALSE,
      show_int_ctrl = show_int_ctrl,
      version = 0L
    )

    # column names of each subset of data
    names(df_rand$list_df_long$df_top_long) <- strsplit(
      x = names(df_rand$list_df_long$df_top_long),
      split = "_"
    ) |>
      lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
      unlist() |>
      (\(x) paste0("df_top_", x))()
    names(df_rand$list_df_long$df_middle_long) <- strsplit(
      x = names(df_rand$list_df_long$df_middle_long),
      split = "_"
    ) |>
      lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
      unlist() |>
      (\(x) paste0("df_mid_", x))()

    # expected dim of output df
    n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                      n_assays = n_assays,
                                      n_samples = n_samples,
                                      has_int_ctrl = show_int_ctrl,
                                      num_int_ctrl = 3L)

    # check that function runs
    expect_no_condition(
      object = df_out <- red_npx_wide_top_mid_long(
        df_top_list = df_rand$list_df_long$df_top_long,
        df_middle_list = df_rand$list_df_long$df_middle_long,
        data_type = data_type,
        format_spec = get_format_spec(data_type = data_type)
      )
    )

    # rename from NPXS
    olink_wide_rename_npxs_tmp <- olink_wide_rename_npxs |>
      dplyr::filter(
        .data[["OA_internal"]] %in% colnames(df_out)
      )

    # check that output match
    expect_true(
      object = identical(nrow(df_out), n_row_exp)
    )

    expect_identical(
      object = dplyr::select(df_out,
                             -dplyr::all_of("col_index")),
      expected = df_rand$list_df_long$df_long |>
        dplyr::rename_with(
          .fn = ~olink_wide_rename_npxs_tmp$OA_internal,
          .cols = dplyr::all_of(olink_wide_rename_npxs_tmp$NPXS)
        ) |>
        dplyr::select(dplyr::any_of(colnames(df_out)))
    )

    ## Ct with int ctrl ----

    # synthetic wide df
    data_type <- "Ct"
    show_int_ctrl <- TRUE

    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = n_panels,
      n_assays = n_assays,
      n_samples = n_samples,
      show_dev_int_ctrl = FALSE,
      show_int_ctrl = show_int_ctrl,
      version = 0L
    )

    # column names of each subset of data
    names(df_rand$list_df_long$df_top_long) <- strsplit(
      x = names(df_rand$list_df_long$df_top_long),
      split = "_"
    ) |>
      lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
      unlist() |>
      (\(x) paste0("df_top_", x))()
    names(df_rand$list_df_long$df_middle_long) <- strsplit(
      x = names(df_rand$list_df_long$df_middle_long),
      split = "_"
    ) |>
      lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
      unlist() |>
      (\(x) paste0("df_mid_", x))()

    # expected dim of output df
    n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                      n_assays = n_assays,
                                      n_samples = n_samples,
                                      has_int_ctrl = show_int_ctrl,
                                      num_int_ctrl = 3L)

    # check that function runs
    expect_no_condition(
      object = df_out <- red_npx_wide_top_mid_long(
        df_top_list = df_rand$list_df_long$df_top_long,
        df_middle_list = df_rand$list_df_long$df_middle_long,
        data_type = data_type,
        format_spec = get_format_spec(data_type = data_type)
      )
    )

    # rename from NPXS
    olink_wide_rename_npxs_tmp <- olink_wide_rename_npxs |>
      dplyr::filter(
        .data[["OA_internal"]] %in% colnames(df_out)
      )

    # check that output match
    expect_true(
      object = identical(nrow(df_out), n_row_exp)
    )

    expect_identical(
      object = dplyr::select(df_out,
                             -dplyr::all_of("col_index")),
      expected = df_rand$list_df_long$df_long |>
        dplyr::rename_with(
          .fn = ~olink_wide_rename_npxs_tmp$OA_internal,
          .cols = dplyr::all_of(olink_wide_rename_npxs_tmp$NPXS)
        ) |>
        dplyr::select(dplyr::any_of(colnames(df_out)))
    )

    ## Quantified no int ctrl and no dev int ctrl ----

    # synthetic wide df
    data_type <- "Quantified"
    show_int_ctrl <- FALSE

    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = n_panels,
      n_assays = n_assays,
      n_samples = n_samples,
      show_dev_int_ctrl = FALSE,
      show_int_ctrl = show_int_ctrl,
      version = 0L
    )

    # column names of each subset of data
    names(df_rand$list_df_long$df_top_long) <- strsplit(
      x = names(df_rand$list_df_long$df_top_long),
      split = "_"
    ) |>
      lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
      unlist() |>
      (\(x) paste0("df_top_", x))()
    names(df_rand$list_df_long$df_middle_long) <- strsplit(
      x = names(df_rand$list_df_long$df_middle_long),
      split = "_"
    ) |>
      lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
      unlist() |>
      (\(x) paste0("df_mid_", x))()

    # expected dim of output df
    n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                      n_assays = n_assays,
                                      n_samples = n_samples,
                                      has_int_ctrl = show_int_ctrl,
                                      num_int_ctrl = 3L)

    # check that function runs
    expect_no_condition(
      object = df_out <- red_npx_wide_top_mid_long(
        df_top_list = df_rand$list_df_long$df_top_long,
        df_middle_list = df_rand$list_df_long$df_middle_long,
        data_type = data_type,
        format_spec = get_format_spec(data_type = data_type)
      )
    )

    # rename from NPXS
    olink_wide_rename_npxs_tmp <- olink_wide_rename_npxs |>
      dplyr::filter(
        .data[["OA_internal"]] %in% colnames(df_out)
      )

    # check that output match
    expect_true(
      object = identical(nrow(df_out), n_row_exp)
    )

    expect_identical(
      object = dplyr::select(df_out,
                             -dplyr::all_of("col_index")),
      expected = df_rand$list_df_long$df_long |>
        dplyr::rename_with(
          .fn = ~olink_wide_rename_npxs_tmp$OA_internal,
          .cols = dplyr::all_of(olink_wide_rename_npxs_tmp$NPXS)
        ) |>
        dplyr::select(dplyr::any_of(colnames(df_out)))
    )

    ## Quantified with int ctrl and no dev int ctrl ----

    # synthetic wide df
    data_type <- "Quantified"
    show_int_ctrl <- TRUE

    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = n_panels,
      n_assays = n_assays,
      n_samples = n_samples,
      show_dev_int_ctrl = FALSE,
      show_int_ctrl = show_int_ctrl,
      version = 0L
    )

    # column names of each subset of data
    names(df_rand$list_df_long$df_top_long) <- strsplit(
      x = names(df_rand$list_df_long$df_top_long),
      split = "_"
    ) |>
      lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
      unlist() |>
      (\(x) paste0("df_top_", x))()
    names(df_rand$list_df_long$df_middle_long) <- strsplit(
      x = names(df_rand$list_df_long$df_middle_long),
      split = "_"
    ) |>
      lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
      unlist() |>
      (\(x) paste0("df_mid_", x))()

    # expected dim of output df
    n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                      n_assays = n_assays,
                                      n_samples = n_samples,
                                      has_int_ctrl = show_int_ctrl,
                                      num_int_ctrl = 3L)

    # check that function runs
    expect_no_condition(
      object = df_out <- red_npx_wide_top_mid_long(
        df_top_list = df_rand$list_df_long$df_top_long,
        df_middle_list = df_rand$list_df_long$df_middle_long,
        data_type = data_type,
        format_spec = get_format_spec(data_type = data_type)
      )
    )

    # rename from NPXS
    olink_wide_rename_npxs_tmp <- olink_wide_rename_npxs |>
      dplyr::filter(
        .data[["OA_internal"]] %in% colnames(df_out)
      )

    # check that output match
    expect_true(
      object = identical(nrow(df_out), n_row_exp)
    )

    expect_identical(
      object = dplyr::select(df_out,
                             -dplyr::all_of("col_index")),
      expected = df_rand$list_df_long$df_long |>
        dplyr::rename_with(
          .fn = ~olink_wide_rename_npxs_tmp$OA_internal,
          .cols = dplyr::all_of(olink_wide_rename_npxs_tmp$NPXS)
        ) |>
        dplyr::select(dplyr::any_of(colnames(df_out)))
    )

    ## Quantified no int ctrl and with dev int ctrl ----

    # synthetic wide df
    data_type <- "Quantified"
    show_int_ctrl <- FALSE

    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = n_panels,
      n_assays = n_assays,
      n_samples = n_samples,
      show_dev_int_ctrl = TRUE,
      show_int_ctrl = show_int_ctrl,
      version = 0L
    )

    # column names of each subset of data
    names(df_rand$list_df_long$df_top_long) <- strsplit(
      x = names(df_rand$list_df_long$df_top_long),
      split = "_"
    ) |>
      lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
      unlist() |>
      (\(x) paste0("df_top_", x))()
    names(df_rand$list_df_long$df_middle_long) <- strsplit(
      x = names(df_rand$list_df_long$df_middle_long),
      split = "_"
    ) |>
      lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
      unlist() |>
      (\(x) paste0("df_mid_", x))()

    # expected dim of output df
    n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                      n_assays = n_assays,
                                      n_samples = n_samples,
                                      has_int_ctrl = show_int_ctrl,
                                      num_int_ctrl = 3L)

    # check that function runs
    expect_no_condition(
      object = df_out <- red_npx_wide_top_mid_long(
        df_top_list = df_rand$list_df_long$df_top_long,
        df_middle_list = df_rand$list_df_long$df_middle_long,
        data_type = data_type,
        format_spec = get_format_spec(data_type = data_type)
      )
    )

    # rename from NPXS
    olink_wide_rename_npxs_tmp <- olink_wide_rename_npxs |>
      dplyr::filter(
        .data[["OA_internal"]] %in% colnames(df_out)
      )

    # check that output match
    expect_true(
      object = identical(nrow(df_out), n_row_exp)
    )

    expect_identical(
      object = dplyr::select(df_out,
                             -dplyr::all_of("col_index")),
      expected = df_rand$list_df_long$df_long |>
        dplyr::rename_with(
          .fn = ~olink_wide_rename_npxs_tmp$OA_internal,
          .cols = dplyr::all_of(olink_wide_rename_npxs_tmp$NPXS)
        ) |>
        dplyr::select(dplyr::any_of(colnames(df_out)))
    )

    ## Quantified with int ctrl and with dev int ctrl ----

    # synthetic wide df
    data_type <- "Quantified"
    show_int_ctrl <- TRUE

    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = n_panels,
      n_assays = n_assays,
      n_samples = n_samples,
      show_dev_int_ctrl = TRUE,
      show_int_ctrl = show_int_ctrl,
      version = 0L
    )

    # column names of each subset of data
    names(df_rand$list_df_long$df_top_long) <- strsplit(
      x = names(df_rand$list_df_long$df_top_long),
      split = "_"
    ) |>
      lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
      unlist() |>
      (\(x) paste0("df_top_", x))()
    names(df_rand$list_df_long$df_middle_long) <- strsplit(
      x = names(df_rand$list_df_long$df_middle_long),
      split = "_"
    ) |>
      lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
      unlist() |>
      (\(x) paste0("df_mid_", x))()

    # expected dim of output df
    n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                      n_assays = n_assays,
                                      n_samples = n_samples,
                                      has_int_ctrl = show_int_ctrl,
                                      num_int_ctrl = 3L)

    # check that function runs
    expect_no_condition(
      object = df_out <- red_npx_wide_top_mid_long(
        df_top_list = df_rand$list_df_long$df_top_long,
        df_middle_list = df_rand$list_df_long$df_middle_long,
        data_type = data_type,
        format_spec = get_format_spec(data_type = data_type)
      )
    )

    # rename from NPXS
    olink_wide_rename_npxs_tmp <- olink_wide_rename_npxs |>
      dplyr::filter(
        .data[["OA_internal"]] %in% colnames(df_out)
      )

    # check that output match
    expect_true(
      object = identical(nrow(df_out), n_row_exp)
    )

    expect_identical(
      object = dplyr::select(df_out,
                             -dplyr::all_of("col_index")),
      expected = df_rand$list_df_long$df_long |>
        dplyr::rename_with(
          .fn = ~olink_wide_rename_npxs_tmp$OA_internal,
          .cols = dplyr::all_of(olink_wide_rename_npxs_tmp$NPXS)
        ) |>
        dplyr::select(dplyr::any_of(colnames(df_out)))
    )

  }
)

test_that(
  "red_npx_wide_top_mid_long - works - multiple panels - multiple plates",
  {
    # variables that apply to all tests
    olink_platform <- "Target 48"
    n_panels <- 3L
    n_assays <- 45L
    n_samples <- 99L

    ## NPX no int ctrl and no dev int ctrl ----

    # synthetic wide df
    data_type <- "NPX"
    show_int_ctrl <- FALSE

    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = n_panels,
      n_assays = n_assays,
      n_samples = n_samples,
      show_dev_int_ctrl = FALSE,
      show_int_ctrl = show_int_ctrl,
      version = 1L
    )

    # column names of each subset of data
    names(df_rand$list_df_long$df_top_long) <- strsplit(
      x = names(df_rand$list_df_long$df_top_long),
      split = "_"
    ) |>
      lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
      unlist() |>
      (\(x) paste0("df_top_", x))()
    names(df_rand$list_df_long$df_middle_long) <- strsplit(
      x = names(df_rand$list_df_long$df_middle_long),
      split = "_"
    ) |>
      lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
      unlist() |>
      (\(x) paste0("df_mid_", x))()

    # expected dim of output df
    n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                      n_assays = n_assays,
                                      n_samples = n_samples,
                                      has_int_ctrl = show_int_ctrl,
                                      num_int_ctrl = 3L)

    # check that function runs
    expect_no_condition(
      object = df_out <- red_npx_wide_top_mid_long(
        df_top_list = df_rand$list_df_long$df_top_long,
        df_middle_list = df_rand$list_df_long$df_middle_long,
        data_type = data_type,
        format_spec = get_format_spec(data_type = data_type)
      )
    )

    # rename from NPXS
    olink_wide_rename_npxs_tmp <- olink_wide_rename_npxs |>
      dplyr::filter(
        .data[["OA_internal"]] %in% colnames(df_out)
      )

    # check that output match
    expect_true(
      object = identical(nrow(df_out), n_row_exp)
    )

    expect_identical(
      object = dplyr::select(df_out,
                             -dplyr::all_of("col_index")),
      expected = df_rand$list_df_long$df_long |>
        dplyr::rename_with(
          .fn = ~olink_wide_rename_npxs_tmp$OA_internal,
          .cols = dplyr::all_of(olink_wide_rename_npxs_tmp$NPXS)
        ) |>
        dplyr::select(dplyr::any_of(colnames(df_out)))
    )

    ## NPX with int ctrl and no dev int ctrl ----

    # synthetic wide df
    data_type <- "NPX"
    show_int_ctrl <- TRUE

    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = n_panels,
      n_assays = n_assays,
      n_samples = n_samples,
      show_dev_int_ctrl = FALSE,
      show_int_ctrl = show_int_ctrl,
      version = 1L
    )

    # column names of each subset of data
    names(df_rand$list_df_long$df_top_long) <- strsplit(
      x = names(df_rand$list_df_long$df_top_long),
      split = "_"
    ) |>
      lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
      unlist() |>
      (\(x) paste0("df_top_", x))()
    names(df_rand$list_df_long$df_middle_long) <- strsplit(
      x = names(df_rand$list_df_long$df_middle_long),
      split = "_"
    ) |>
      lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
      unlist() |>
      (\(x) paste0("df_mid_", x))()

    # expected dim of output df
    n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                      n_assays = n_assays,
                                      n_samples = n_samples,
                                      has_int_ctrl = show_int_ctrl,
                                      num_int_ctrl = 3L)

    # check that function runs
    expect_no_condition(
      object = df_out <- red_npx_wide_top_mid_long(
        df_top_list = df_rand$list_df_long$df_top_long,
        df_middle_list = df_rand$list_df_long$df_middle_long,
        data_type = data_type,
        format_spec = get_format_spec(data_type = data_type)
      )
    )

    # rename from NPXS
    olink_wide_rename_npxs_tmp <- olink_wide_rename_npxs |>
      dplyr::filter(
        .data[["OA_internal"]] %in% colnames(df_out)
      )

    # check that output match
    expect_true(
      object = identical(nrow(df_out), n_row_exp)
    )

    expect_identical(
      object = dplyr::select(df_out,
                             -dplyr::all_of("col_index")),
      expected = df_rand$list_df_long$df_long |>
        dplyr::rename_with(
          .fn = ~olink_wide_rename_npxs_tmp$OA_internal,
          .cols = dplyr::all_of(olink_wide_rename_npxs_tmp$NPXS)
        ) |>
        dplyr::select(dplyr::any_of(colnames(df_out)))
    )

    ## NPX no int ctrl and with dev int ctrl ----

    # synthetic wide df
    data_type <- "NPX"
    show_int_ctrl <- FALSE

    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = n_panels,
      n_assays = n_assays,
      n_samples = n_samples,
      show_dev_int_ctrl = TRUE,
      show_int_ctrl = show_int_ctrl,
      version = 1L
    )

    # column names of each subset of data
    names(df_rand$list_df_long$df_top_long) <- strsplit(
      x = names(df_rand$list_df_long$df_top_long),
      split = "_"
    ) |>
      lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
      unlist() |>
      (\(x) paste0("df_top_", x))()
    names(df_rand$list_df_long$df_middle_long) <- strsplit(
      x = names(df_rand$list_df_long$df_middle_long),
      split = "_"
    ) |>
      lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
      unlist() |>
      (\(x) paste0("df_mid_", x))()

    # expected dim of output df
    n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                      n_assays = n_assays,
                                      n_samples = n_samples,
                                      has_int_ctrl = show_int_ctrl,
                                      num_int_ctrl = 3L)

    # check that function runs
    expect_no_condition(
      object = df_out <- red_npx_wide_top_mid_long(
        df_top_list = df_rand$list_df_long$df_top_long,
        df_middle_list = df_rand$list_df_long$df_middle_long,
        data_type = data_type,
        format_spec = get_format_spec(data_type = data_type)
      )
    )

    # rename from NPXS
    olink_wide_rename_npxs_tmp <- olink_wide_rename_npxs |>
      dplyr::filter(
        .data[["OA_internal"]] %in% colnames(df_out)
      )

    # check that output match
    expect_true(
      object = identical(nrow(df_out), n_row_exp)
    )

    expect_identical(
      object = dplyr::select(df_out,
                             -dplyr::all_of("col_index")),
      expected = df_rand$list_df_long$df_long |>
        dplyr::rename_with(
          .fn = ~olink_wide_rename_npxs_tmp$OA_internal,
          .cols = dplyr::all_of(olink_wide_rename_npxs_tmp$NPXS)
        ) |>
        dplyr::select(dplyr::any_of(colnames(df_out)))
    )

    ## NPX with int ctrl and with dev int ctrl ----

    # synthetic wide df
    data_type <- "NPX"
    show_int_ctrl <- TRUE

    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = n_panels,
      n_assays = n_assays,
      n_samples = n_samples,
      show_dev_int_ctrl = TRUE,
      show_int_ctrl = show_int_ctrl,
      version = 1L
    )

    # column names of each subset of data
    names(df_rand$list_df_long$df_top_long) <- strsplit(
      x = names(df_rand$list_df_long$df_top_long),
      split = "_"
    ) |>
      lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
      unlist() |>
      (\(x) paste0("df_top_", x))()
    names(df_rand$list_df_long$df_middle_long) <- strsplit(
      x = names(df_rand$list_df_long$df_middle_long),
      split = "_"
    ) |>
      lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
      unlist() |>
      (\(x) paste0("df_mid_", x))()

    # expected dim of output df
    n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                      n_assays = n_assays,
                                      n_samples = n_samples,
                                      has_int_ctrl = show_int_ctrl,
                                      num_int_ctrl = 3L)

    # check that function runs
    expect_no_condition(
      object = df_out <- red_npx_wide_top_mid_long(
        df_top_list = df_rand$list_df_long$df_top_long,
        df_middle_list = df_rand$list_df_long$df_middle_long,
        data_type = data_type,
        format_spec = get_format_spec(data_type = data_type)
      )
    )

    # rename from NPXS
    olink_wide_rename_npxs_tmp <- olink_wide_rename_npxs |>
      dplyr::filter(
        .data[["OA_internal"]] %in% colnames(df_out)
      )

    # check that output match
    expect_true(
      object = identical(nrow(df_out), n_row_exp)
    )

    expect_identical(
      object = dplyr::select(df_out,
                             -dplyr::all_of("col_index")),
      expected = df_rand$list_df_long$df_long |>
        dplyr::rename_with(
          .fn = ~olink_wide_rename_npxs_tmp$OA_internal,
          .cols = dplyr::all_of(olink_wide_rename_npxs_tmp$NPXS)
        ) |>
        dplyr::select(dplyr::any_of(colnames(df_out)))
    )


    ## Ct no int ctrl ----

    # synthetic wide df
    data_type <- "Ct"
    show_int_ctrl <- FALSE

    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = n_panels,
      n_assays = n_assays,
      n_samples = n_samples,
      show_dev_int_ctrl = FALSE,
      show_int_ctrl = show_int_ctrl,
      version = 0L
    )

    # column names of each subset of data
    names(df_rand$list_df_long$df_top_long) <- strsplit(
      x = names(df_rand$list_df_long$df_top_long),
      split = "_"
    ) |>
      lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
      unlist() |>
      (\(x) paste0("df_top_", x))()
    names(df_rand$list_df_long$df_middle_long) <- strsplit(
      x = names(df_rand$list_df_long$df_middle_long),
      split = "_"
    ) |>
      lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
      unlist() |>
      (\(x) paste0("df_mid_", x))()

    # expected dim of output df
    n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                      n_assays = n_assays,
                                      n_samples = n_samples,
                                      has_int_ctrl = show_int_ctrl,
                                      num_int_ctrl = 3L)

    # check that function runs
    expect_no_condition(
      object = df_out <- red_npx_wide_top_mid_long(
        df_top_list = df_rand$list_df_long$df_top_long,
        df_middle_list = df_rand$list_df_long$df_middle_long,
        data_type = data_type,
        format_spec = get_format_spec(data_type = data_type)
      )
    )

    # rename from NPXS
    olink_wide_rename_npxs_tmp <- olink_wide_rename_npxs |>
      dplyr::filter(
        .data[["OA_internal"]] %in% colnames(df_out)
      )

    # check that output match
    expect_true(
      object = identical(nrow(df_out), n_row_exp)
    )

    expect_identical(
      object = dplyr::select(df_out,
                             -dplyr::all_of("col_index")),
      expected = df_rand$list_df_long$df_long |>
        dplyr::rename_with(
          .fn = ~olink_wide_rename_npxs_tmp$OA_internal,
          .cols = dplyr::all_of(olink_wide_rename_npxs_tmp$NPXS)
        ) |>
        dplyr::select(dplyr::any_of(colnames(df_out)))
    )

    ## Ct with int ctrl ----

    # synthetic wide df
    data_type <- "Ct"
    show_int_ctrl <- TRUE

    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = n_panels,
      n_assays = n_assays,
      n_samples = n_samples,
      show_dev_int_ctrl = FALSE,
      show_int_ctrl = show_int_ctrl,
      version = 0L
    )

    # column names of each subset of data
    names(df_rand$list_df_long$df_top_long) <- strsplit(
      x = names(df_rand$list_df_long$df_top_long),
      split = "_"
    ) |>
      lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
      unlist() |>
      (\(x) paste0("df_top_", x))()
    names(df_rand$list_df_long$df_middle_long) <- strsplit(
      x = names(df_rand$list_df_long$df_middle_long),
      split = "_"
    ) |>
      lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
      unlist() |>
      (\(x) paste0("df_mid_", x))()

    # expected dim of output df
    n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                      n_assays = n_assays,
                                      n_samples = n_samples,
                                      has_int_ctrl = show_int_ctrl,
                                      num_int_ctrl = 3L)

    # check that function runs
    expect_no_condition(
      object = df_out <- red_npx_wide_top_mid_long(
        df_top_list = df_rand$list_df_long$df_top_long,
        df_middle_list = df_rand$list_df_long$df_middle_long,
        data_type = data_type,
        format_spec = get_format_spec(data_type = data_type)
      )
    )

    # rename from NPXS
    olink_wide_rename_npxs_tmp <- olink_wide_rename_npxs |>
      dplyr::filter(
        .data[["OA_internal"]] %in% colnames(df_out)
      )

    # check that output match
    expect_true(
      object = identical(nrow(df_out), n_row_exp)
    )

    expect_identical(
      object = dplyr::select(df_out,
                             -dplyr::all_of("col_index")),
      expected = df_rand$list_df_long$df_long |>
        dplyr::rename_with(
          .fn = ~olink_wide_rename_npxs_tmp$OA_internal,
          .cols = dplyr::all_of(olink_wide_rename_npxs_tmp$NPXS)
        ) |>
        dplyr::select(dplyr::any_of(colnames(df_out)))
    )

    ## Quantified no int ctrl and no dev int ctrl ----

    # synthetic wide df
    data_type <- "Quantified"
    show_int_ctrl <- FALSE

    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = n_panels,
      n_assays = n_assays,
      n_samples = n_samples,
      show_dev_int_ctrl = FALSE,
      show_int_ctrl = show_int_ctrl,
      version = 0L
    )

    # column names of each subset of data
    names(df_rand$list_df_long$df_top_long) <- strsplit(
      x = names(df_rand$list_df_long$df_top_long),
      split = "_"
    ) |>
      lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
      unlist() |>
      (\(x) paste0("df_top_", x))()
    names(df_rand$list_df_long$df_middle_long) <- strsplit(
      x = names(df_rand$list_df_long$df_middle_long),
      split = "_"
    ) |>
      lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
      unlist() |>
      (\(x) paste0("df_mid_", x))()

    # expected dim of output df
    n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                      n_assays = n_assays,
                                      n_samples = n_samples,
                                      has_int_ctrl = show_int_ctrl,
                                      num_int_ctrl = 3L)

    # check that function runs
    expect_no_condition(
      object = df_out <- red_npx_wide_top_mid_long(
        df_top_list = df_rand$list_df_long$df_top_long,
        df_middle_list = df_rand$list_df_long$df_middle_long,
        data_type = data_type,
        format_spec = get_format_spec(data_type = data_type)
      )
    )

    # rename from NPXS
    olink_wide_rename_npxs_tmp <- olink_wide_rename_npxs |>
      dplyr::filter(
        .data[["OA_internal"]] %in% colnames(df_out)
      )

    # check that output match
    expect_true(
      object = identical(nrow(df_out), n_row_exp)
    )

    expect_identical(
      object = dplyr::select(df_out,
                             -dplyr::all_of("col_index")),
      expected = df_rand$list_df_long$df_long |>
        dplyr::rename_with(
          .fn = ~olink_wide_rename_npxs_tmp$OA_internal,
          .cols = dplyr::all_of(olink_wide_rename_npxs_tmp$NPXS)
        ) |>
        dplyr::select(dplyr::any_of(colnames(df_out)))
    )

    ## Quantified with int ctrl and no dev int ctrl ----

    # synthetic wide df
    data_type <- "Quantified"
    show_int_ctrl <- TRUE

    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = n_panels,
      n_assays = n_assays,
      n_samples = n_samples,
      show_dev_int_ctrl = FALSE,
      show_int_ctrl = show_int_ctrl,
      version = 0L
    )

    # column names of each subset of data
    names(df_rand$list_df_long$df_top_long) <- strsplit(
      x = names(df_rand$list_df_long$df_top_long),
      split = "_"
    ) |>
      lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
      unlist() |>
      (\(x) paste0("df_top_", x))()
    names(df_rand$list_df_long$df_middle_long) <- strsplit(
      x = names(df_rand$list_df_long$df_middle_long),
      split = "_"
    ) |>
      lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
      unlist() |>
      (\(x) paste0("df_mid_", x))()

    # expected dim of output df
    n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                      n_assays = n_assays,
                                      n_samples = n_samples,
                                      has_int_ctrl = show_int_ctrl,
                                      num_int_ctrl = 3L)

    # check that function runs
    expect_no_condition(
      object = df_out <- red_npx_wide_top_mid_long(
        df_top_list = df_rand$list_df_long$df_top_long,
        df_middle_list = df_rand$list_df_long$df_middle_long,
        data_type = data_type,
        format_spec = get_format_spec(data_type = data_type)
      )
    )

    # rename from NPXS
    olink_wide_rename_npxs_tmp <- olink_wide_rename_npxs |>
      dplyr::filter(
        .data[["OA_internal"]] %in% colnames(df_out)
      )

    # check that output match
    expect_true(
      object = identical(nrow(df_out), n_row_exp)
    )

    expect_identical(
      object = dplyr::select(df_out,
                             -dplyr::all_of("col_index")),
      expected = df_rand$list_df_long$df_long |>
        dplyr::rename_with(
          .fn = ~olink_wide_rename_npxs_tmp$OA_internal,
          .cols = dplyr::all_of(olink_wide_rename_npxs_tmp$NPXS)
        ) |>
        dplyr::select(dplyr::any_of(colnames(df_out)))
    )

    ## Quantified no int ctrl and with dev int ctrl ----

    # synthetic wide df
    data_type <- "Quantified"
    show_int_ctrl <- FALSE

    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = n_panels,
      n_assays = n_assays,
      n_samples = n_samples,
      show_dev_int_ctrl = TRUE,
      show_int_ctrl = show_int_ctrl,
      version = 0L
    )

    # column names of each subset of data
    names(df_rand$list_df_long$df_top_long) <- strsplit(
      x = names(df_rand$list_df_long$df_top_long),
      split = "_"
    ) |>
      lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
      unlist() |>
      (\(x) paste0("df_top_", x))()
    names(df_rand$list_df_long$df_middle_long) <- strsplit(
      x = names(df_rand$list_df_long$df_middle_long),
      split = "_"
    ) |>
      lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
      unlist() |>
      (\(x) paste0("df_mid_", x))()

    # expected dim of output df
    n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                      n_assays = n_assays,
                                      n_samples = n_samples,
                                      has_int_ctrl = show_int_ctrl,
                                      num_int_ctrl = 3L)

    # check that function runs
    expect_no_condition(
      object = df_out <- red_npx_wide_top_mid_long(
        df_top_list = df_rand$list_df_long$df_top_long,
        df_middle_list = df_rand$list_df_long$df_middle_long,
        data_type = data_type,
        format_spec = get_format_spec(data_type = data_type)
      )
    )

    # rename from NPXS
    olink_wide_rename_npxs_tmp <- olink_wide_rename_npxs |>
      dplyr::filter(
        .data[["OA_internal"]] %in% colnames(df_out)
      )

    # check that output match
    expect_true(
      object = identical(nrow(df_out), n_row_exp)
    )

    expect_identical(
      object = dplyr::select(df_out,
                             -dplyr::all_of("col_index")),
      expected = df_rand$list_df_long$df_long |>
        dplyr::rename_with(
          .fn = ~olink_wide_rename_npxs_tmp$OA_internal,
          .cols = dplyr::all_of(olink_wide_rename_npxs_tmp$NPXS)
        ) |>
        dplyr::select(dplyr::any_of(colnames(df_out)))
    )

    ## Quantified with int ctrl and with dev int ctrl ----

    # synthetic wide df
    data_type <- "Quantified"
    show_int_ctrl <- TRUE

    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = n_panels,
      n_assays = n_assays,
      n_samples = n_samples,
      show_dev_int_ctrl = TRUE,
      show_int_ctrl = show_int_ctrl,
      version = 0L
    )

    # column names of each subset of data
    names(df_rand$list_df_long$df_top_long) <- strsplit(
      x = names(df_rand$list_df_long$df_top_long),
      split = "_"
    ) |>
      lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
      unlist() |>
      (\(x) paste0("df_top_", x))()
    names(df_rand$list_df_long$df_middle_long) <- strsplit(
      x = names(df_rand$list_df_long$df_middle_long),
      split = "_"
    ) |>
      lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
      unlist() |>
      (\(x) paste0("df_mid_", x))()

    # expected dim of output df
    n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                      n_assays = n_assays,
                                      n_samples = n_samples,
                                      has_int_ctrl = show_int_ctrl,
                                      num_int_ctrl = 3L)

    # check that function runs
    expect_no_condition(
      object = df_out <- red_npx_wide_top_mid_long(
        df_top_list = df_rand$list_df_long$df_top_long,
        df_middle_list = df_rand$list_df_long$df_middle_long,
        data_type = data_type,
        format_spec = get_format_spec(data_type = data_type)
      )
    )

    # rename from NPXS
    olink_wide_rename_npxs_tmp <- olink_wide_rename_npxs |>
      dplyr::filter(
        .data[["OA_internal"]] %in% colnames(df_out)
      )

    # check that output match
    expect_true(
      object = identical(nrow(df_out), n_row_exp)
    )

    expect_identical(
      object = dplyr::select(df_out,
                             -dplyr::all_of("col_index")),
      expected = df_rand$list_df_long$df_long |>
        dplyr::rename_with(
          .fn = ~olink_wide_rename_npxs_tmp$OA_internal,
          .cols = dplyr::all_of(olink_wide_rename_npxs_tmp$NPXS)
        ) |>
        dplyr::select(dplyr::any_of(colnames(df_out)))
    )

  }
)

# NPX data from T96 NPX Manager, PlateID and QC_Warning columns did not contain
# version of panel
test_that(
  "red_npx_wide_top_mid_long - works - T96 - NPXM missing panel/QC version",
  {
    # variables that apply to all tests
    olink_platform <- "Target 96"
    n_panels <- 3L
    n_assays <- 92L
    n_samples <- 99L
    data_type <- "NPX"
    show_int_ctrl <- TRUE
    show_dev_int_ctrl <- TRUE
    version <- 1L

    # synthetic wide df
    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = n_panels,
      n_assays = n_assays,
      n_samples = n_samples,
      show_dev_int_ctrl = show_dev_int_ctrl,
      show_int_ctrl = show_int_ctrl,
      version = version
    )

    # modify pid, qc_warn and dev_int_ctrl dfs
    df_rand_top_long_tmp <- df_rand$list_df_long$df_top_long[
      c("df_plate",
        "df_qc_warn",
        "df_dev_int_ctrl")
    ] |>
      lapply(function(.x) {
        .x |>
          dplyr::mutate(
            Panel = strsplit(x = .data[["Panel"]], split = "(", fixed = TRUE) |>
              lapply(head, 1L) |>
              unlist()
          )
      })
    df_rand_top_long <- append(
      x = df_rand$list_df_long$df_top_long[c("df_oid",
                                             "df_int_ctrl")],
      values = df_rand_top_long_tmp
    )

    # column names of each subset of data
    names(df_rand_top_long) <- strsplit(
      x = names(df_rand_top_long),
      split = "_"
    ) |>
      lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
      unlist() |>
      (\(x) paste0("df_top_", x))()
    names(df_rand$list_df_long$df_middle_long) <- strsplit(
      x = names(df_rand$list_df_long$df_middle_long),
      split = "_"
    ) |>
      lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
      unlist() |>
      (\(x) paste0("df_mid_", x))()

    # expected dim of output df
    n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                      n_assays = n_assays,
                                      n_samples = n_samples,
                                      has_int_ctrl = show_int_ctrl,
                                      num_int_ctrl = 3L)

    # check that function runs
    expect_no_condition(
      object = df_out <- red_npx_wide_top_mid_long(
        df_top_list = df_rand_top_long,
        df_middle_list = df_rand$list_df_long$df_middle_long,
        data_type = data_type,
        format_spec = get_format_spec(data_type = data_type)
      )
    )

    # rename from NPXS
    olink_wide_rename_npxs_tmp <- olink_wide_rename_npxs |>
      dplyr::filter(
        .data[["OA_internal"]] %in% colnames(df_out)
      )

    # check that output match
    expect_true(
      object = identical(nrow(df_out), n_row_exp)
    )

    expect_identical(
      object = dplyr::select(df_out,
                             -dplyr::all_of("col_index")),
      expected = df_rand$list_df_long$df_long |>
        dplyr::rename_with(
          .fn = ~olink_wide_rename_npxs_tmp$OA_internal,
          .cols = dplyr::all_of(olink_wide_rename_npxs_tmp$NPXS)
        ) |>
        dplyr::select(dplyr::any_of(colnames(df_out)))
    )
  }
)

# Test read_npx_wide_bottom ----

test_that(
  "read_npx_wide_bottom - works - T48 - single panel - single plate",
  {
    # variables that apply to all tests
    olink_platform <- "Target 48"
    n_panels <- 1L
    n_assays <- 45L
    n_samples <- 88L
    show_dev_int_ctrl <- FALSE

    ## NPX no int ctrl v1 ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = FALSE,
          version = 1L
        )

        # column names of each subset of data
        col_names <- sapply(df_rand$list_df_long$df_top_long,
                            function(x) x$col_index)
        col_names <- col_names[which(lapply(col_names, length) != 0L)]
        names(col_names) <- strsplit(x = names(col_names), split = "_") |>
          lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
          unlist()
        names(col_names) <- paste0("df_top_", names(col_names))

        # plate panel combos
        df_plate_panel <- df_rand$list_df_long$df_oid_long |>
          dplyr::select(
            dplyr::all_of(c("col_index", "Panel"))
          ) |>
          dplyr::distinct() |>
          dplyr::left_join(
            df_rand$list_df_long$df_plate_long |>
              dplyr::select(
                dplyr::all_of(c("PlateID", "Panel"))
              ) |>
              dplyr::distinct(),
            by = "Panel",
            relationship = "many-to-many"
          )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_bottom(
            df = df_rand$list_df_wide$df_bottom_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            data_type = data_type,
            col_names = col_names,
            format_spec = get_format_spec(data_type = data_type),
            df_plate_panel = df_plate_panel
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out$df_bottom_oid) |> sort(),
          expected = colnames(df_rand$list_df_long$df_bottom_long$df_oid) |>
            sort()
        )
        expect_identical(
          object = df_out$df_bottom_oid,
          expected = df_rand$list_df_long$df_bottom_long$df_oid |>
            dplyr::select(
              dplyr::all_of(colnames(df_out$df_bottom_oid))
            )
        )

        expect_false(
          object = "df_bottom_int_ctrl" %in% names(df_out)
        )

        expect_false(
          object = "df_int_ctrl" %in% names(df_rand$list_df_long$df_bottom_long)
        )

      }
    )

    ## NPX with int ctrl v1 ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = TRUE,
          version = 1L
        )

        # column names of each subset of data
        col_names <- sapply(df_rand$list_df_long$df_top_long,
                            function(x) x$col_index)
        col_names <- col_names[which(lapply(col_names, length) != 0L)]
        names(col_names) <- strsplit(x = names(col_names), split = "_") |>
          lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
          unlist()
        names(col_names) <- paste0("df_top_", names(col_names))

        # plate panel combos
        df_plate_panel <- df_rand$list_df_long$df_oid_long |>
          dplyr::select(
            dplyr::all_of(c("col_index", "Panel"))
          ) |>
          dplyr::distinct() |>
          dplyr::bind_rows(
            df_rand$list_df_long$df_int_ctrl_long |>
              dplyr::select(
                dplyr::all_of(c("col_index", "Panel"))
              ) |>
              dplyr::distinct()
          ) |>
          dplyr::left_join(
            df_rand$list_df_long$df_plate_long |>
              dplyr::select(
                dplyr::all_of(c("PlateID", "Panel"))
              ) |>
              dplyr::distinct(),
            by = "Panel",
            relationship = "many-to-many"
          )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_bottom(
            df = df_rand$list_df_wide$df_bottom_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            data_type = data_type,
            col_names = col_names,
            format_spec = get_format_spec(data_type = data_type),
            df_plate_panel = df_plate_panel
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out$df_bottom_oid) |> sort(),
          expected = colnames(df_rand$list_df_long$df_bottom_long$df_oid) |>
            sort()
        )
        expect_identical(
          object = df_out$df_bottom_oid,
          expected = df_rand$list_df_long$df_bottom_long$df_oid |>
            dplyr::select(
              dplyr::all_of(colnames(df_out$df_bottom_oid))
            )
        )

        expect_identical(
          object = colnames(df_out$df_bottom_int_ctrl) |> sort(),
          expected = df_rand$list_df_long$df_bottom_long$df_int_ctrl |>
            colnames() |>
            sort()
        )
        expect_identical(
          object = df_out$df_bottom_int_ctrl,
          expected = df_rand$list_df_long$df_bottom_long$df_int_ctrl |>
            dplyr::select(
              dplyr::all_of(colnames(df_out$df_bottom_int_ctrl))
            )
        )

      }
    )

    ## NPX no int ctrl v2 ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = FALSE,
          version = 2L
        )

        # column names of each subset of data
        col_names <- sapply(df_rand$list_df_long$df_top_long,
                            function(x) x$col_index)
        col_names <- col_names[which(lapply(col_names, length) != 0L)]
        names(col_names) <- strsplit(x = names(col_names), split = "_") |>
          lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
          unlist()
        names(col_names) <- paste0("df_top_", names(col_names))

        # plate panel combos
        df_plate_panel <- df_rand$list_df_long$df_oid_long |>
          dplyr::select(
            dplyr::all_of(c("col_index", "Panel"))
          ) |>
          dplyr::distinct() |>
          dplyr::left_join(
            df_rand$list_df_long$df_plate_long |>
              dplyr::select(
                dplyr::all_of(c("PlateID", "Panel"))
              ) |>
              dplyr::distinct(),
            by = "Panel",
            relationship = "many-to-many"
          )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_bottom(
            df = df_rand$list_df_wide$df_bottom_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            data_type = data_type,
            col_names = col_names,
            format_spec = get_format_spec(data_type = data_type),
            df_plate_panel = df_plate_panel
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out$df_bottom_oid) |> sort(),
          expected = colnames(df_rand$list_df_long$df_bottom_long$df_oid) |>
            sort()
        )
        expect_identical(
          object = df_out$df_bottom_oid,
          expected = df_rand$list_df_long$df_bottom_long$df_oid |>
            dplyr::select(
              dplyr::all_of(colnames(df_out$df_bottom_oid))
            )
        )

        expect_false(
          object = "df_bottom_int_ctrl" %in% names(df_out)
        )

        expect_false(
          object = "df_int_ctrl" %in% names(df_rand$list_df_long$df_bottom_long)
        )

      }
    )

    ## NPX with int ctrl v2 ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = TRUE,
          version = 2L
        )

        # column names of each subset of data
        col_names <- sapply(df_rand$list_df_long$df_top_long,
                            function(x) x$col_index)
        col_names <- col_names[which(lapply(col_names, length) != 0L)]
        names(col_names) <- strsplit(x = names(col_names), split = "_") |>
          lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
          unlist()
        names(col_names) <- paste0("df_top_", names(col_names))

        # plate panel combos
        df_plate_panel <- df_rand$list_df_long$df_oid_long |>
          dplyr::select(
            dplyr::all_of(c("col_index", "Panel"))
          ) |>
          dplyr::distinct() |>
          dplyr::bind_rows(
            df_rand$list_df_long$df_int_ctrl_long |>
              dplyr::select(
                dplyr::all_of(c("col_index", "Panel"))
              ) |>
              dplyr::distinct()
          ) |>
          dplyr::left_join(
            df_rand$list_df_long$df_plate_long |>
              dplyr::select(
                dplyr::all_of(c("PlateID", "Panel"))
              ) |>
              dplyr::distinct(),
            by = "Panel",
            relationship = "many-to-many"
          )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_bottom(
            df = df_rand$list_df_wide$df_bottom_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            data_type = data_type,
            col_names = col_names,
            format_spec = get_format_spec(data_type = data_type),
            df_plate_panel = df_plate_panel
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out$df_bottom_oid) |> sort(),
          expected = colnames(df_rand$list_df_long$df_bottom_long$df_oid) |>
            sort()
        )
        expect_identical(
          object = df_out$df_bottom_oid,
          expected = df_rand$list_df_long$df_bottom_long$df_oid |>
            dplyr::select(
              dplyr::all_of(colnames(df_out$df_bottom_oid))
            )
        )

        expect_identical(
          object = colnames(df_out$df_bottom_int_ctrl) |> sort(),
          expected = df_rand$list_df_long$df_bottom_long$df_int_ctrl |>
            colnames() |>
            sort()
        )
        expect_identical(
          object = df_out$df_bottom_int_ctrl,
          expected = df_rand$list_df_long$df_bottom_long$df_int_ctrl |>
            dplyr::select(
              dplyr::all_of(colnames(df_out$df_bottom_int_ctrl))
            )
        )

      }
    )

    ## Quantified no int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Quantified"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = FALSE,
          version = 0L
        )

        # column names of each subset of data
        col_names <- sapply(df_rand$list_df_long$df_top_long,
                            function(x) x$col_index)
        col_names <- col_names[which(lapply(col_names, length) != 0L)]
        names(col_names) <- strsplit(x = names(col_names), split = "_") |>
          lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
          unlist()
        names(col_names) <- paste0("df_top_", names(col_names))

        # plate panel combos
        df_plate_panel <- df_rand$list_df_long$df_oid_long |>
          dplyr::select(
            dplyr::all_of(c("col_index", "Panel"))
          ) |>
          dplyr::distinct() |>
          dplyr::left_join(
            df_rand$list_df_long$df_plate_long |>
              dplyr::select(
                dplyr::all_of(c("PlateID", "Panel"))
              ) |>
              dplyr::distinct(),
            by = "Panel",
            relationship = "many-to-many"
          )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_bottom(
            df = df_rand$list_df_wide$df_bottom_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            data_type = data_type,
            col_names = col_names,
            format_spec = get_format_spec(data_type = data_type),
            df_plate_panel = df_plate_panel
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out$df_bottom_oid) |> sort(),
          expected = colnames(df_rand$list_df_long$df_bottom_long$df_oid) |>
            sort()
        )
        expect_identical(
          object = df_out$df_bottom_oid,
          expected = df_rand$list_df_long$df_bottom_long$df_oid |>
            dplyr::select(
              dplyr::all_of(colnames(df_out$df_bottom_oid))
            )
        )

        expect_false(
          object = "df_bottom_int_ctrl" %in% names(df_out)
        )

        expect_false(
          object = "df_int_ctrl" %in% names(df_rand$list_df_long$df_bottom_long)
        )

      }
    )

    ## Quantified with int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Quantified"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = TRUE,
          version = 0L
        )

        # column names of each subset of data
        col_names <- sapply(df_rand$list_df_long$df_top_long,
                            function(x) x$col_index)
        col_names <- col_names[which(lapply(col_names, length) != 0L)]
        names(col_names) <- strsplit(x = names(col_names), split = "_") |>
          lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
          unlist()
        names(col_names) <- paste0("df_top_", names(col_names))

        # plate panel combos
        df_plate_panel <- df_rand$list_df_long$df_oid_long |>
          dplyr::select(
            dplyr::all_of(c("col_index", "Panel"))
          ) |>
          dplyr::distinct() |>
          dplyr::bind_rows(
            df_rand$list_df_long$df_int_ctrl_long |>
              dplyr::select(
                dplyr::all_of(c("col_index", "Panel"))
              ) |>
              dplyr::distinct()
          ) |>
          dplyr::left_join(
            df_rand$list_df_long$df_plate_long |>
              dplyr::select(
                dplyr::all_of(c("PlateID", "Panel"))
              ) |>
              dplyr::distinct(),
            by = "Panel",
            relationship = "many-to-many"
          )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_bottom(
            df = df_rand$list_df_wide$df_bottom_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            data_type = data_type,
            col_names = col_names,
            format_spec = get_format_spec(data_type = data_type),
            df_plate_panel = df_plate_panel
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out$df_bottom_oid) |> sort(),
          expected = colnames(df_rand$list_df_long$df_bottom_long$df_oid) |>
            sort()
        )
        expect_identical(
          object = df_out$df_bottom_oid,
          expected = df_rand$list_df_long$df_bottom_long$df_oid |>
            dplyr::select(
              dplyr::all_of(colnames(df_out$df_bottom_oid))
            )
        )

        expect_identical(
          object = colnames(df_out$df_bottom_int_ctrl) |> sort(),
          expected = df_rand$list_df_long$df_bottom_long$df_int_ctrl |>
            colnames() |>
            sort()
        )
        expect_identical(
          object = df_out$df_bottom_int_ctrl,
          expected = df_rand$list_df_long$df_bottom_long$df_int_ctrl |>
            dplyr::select(
              dplyr::all_of(colnames(df_out$df_bottom_int_ctrl))
            )
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_bottom - works - T48 - multiple panels - single plate",
  {
    # variables that apply to all tests
    olink_platform <- "Target 48"
    n_panels <- 3L
    n_assays <- 45L
    n_samples <- 88L
    show_dev_int_ctrl <- FALSE

    ## NPX no int ctrl v1 ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = FALSE,
          version = 1L
        )

        # column names of each subset of data
        col_names <- sapply(df_rand$list_df_long$df_top_long,
                            function(x) x$col_index)
        col_names <- col_names[which(lapply(col_names, length) != 0L)]
        names(col_names) <- strsplit(x = names(col_names), split = "_") |>
          lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
          unlist()
        names(col_names) <- paste0("df_top_", names(col_names))

        # plate panel combos
        df_plate_panel <- df_rand$list_df_long$df_oid_long |>
          dplyr::select(
            dplyr::all_of(c("col_index", "Panel"))
          ) |>
          dplyr::distinct() |>
          dplyr::left_join(
            df_rand$list_df_long$df_plate_long |>
              dplyr::select(
                dplyr::all_of(c("PlateID", "Panel"))
              ) |>
              dplyr::distinct(),
            by = "Panel",
            relationship = "many-to-many"
          )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_bottom(
            df = df_rand$list_df_wide$df_bottom_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            data_type = data_type,
            col_names = col_names,
            format_spec = get_format_spec(data_type = data_type),
            df_plate_panel = df_plate_panel
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out$df_bottom_oid) |> sort(),
          expected = colnames(df_rand$list_df_long$df_bottom_long$df_oid) |>
            sort()
        )
        expect_identical(
          object = df_out$df_bottom_oid,
          expected = df_rand$list_df_long$df_bottom_long$df_oid |>
            dplyr::select(
              dplyr::all_of(colnames(df_out$df_bottom_oid))
            )
        )

        expect_false(
          object = "df_bottom_int_ctrl" %in% names(df_out)
        )

        expect_false(
          object = "df_int_ctrl" %in% names(df_rand$list_df_long$df_bottom_long)
        )

      }
    )

    ## NPX with int ctrl v1 ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = TRUE,
          version = 1L
        )

        # column names of each subset of data
        col_names <- sapply(df_rand$list_df_long$df_top_long,
                            function(x) x$col_index)
        col_names <- col_names[which(lapply(col_names, length) != 0L)]
        names(col_names) <- strsplit(x = names(col_names), split = "_") |>
          lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
          unlist()
        names(col_names) <- paste0("df_top_", names(col_names))

        # plate panel combos
        df_plate_panel <- df_rand$list_df_long$df_oid_long |>
          dplyr::select(
            dplyr::all_of(c("col_index", "Panel"))
          ) |>
          dplyr::distinct() |>
          dplyr::bind_rows(
            df_rand$list_df_long$df_int_ctrl_long |>
              dplyr::select(
                dplyr::all_of(c("col_index", "Panel"))
              ) |>
              dplyr::distinct()
          ) |>
          dplyr::left_join(
            df_rand$list_df_long$df_plate_long |>
              dplyr::select(
                dplyr::all_of(c("PlateID", "Panel"))
              ) |>
              dplyr::distinct(),
            by = "Panel",
            relationship = "many-to-many"
          )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_bottom(
            df = df_rand$list_df_wide$df_bottom_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            data_type = data_type,
            col_names = col_names,
            format_spec = get_format_spec(data_type = data_type),
            df_plate_panel = df_plate_panel
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out$df_bottom_oid) |> sort(),
          expected = colnames(df_rand$list_df_long$df_bottom_long$df_oid) |>
            sort()
        )
        expect_identical(
          object = df_out$df_bottom_oid,
          expected = df_rand$list_df_long$df_bottom_long$df_oid |>
            dplyr::select(
              dplyr::all_of(colnames(df_out$df_bottom_oid))
            )
        )

        expect_identical(
          object = colnames(df_out$df_bottom_int_ctrl) |> sort(),
          expected = df_rand$list_df_long$df_bottom_long$df_int_ctrl |>
            colnames() |>
            sort()
        )
        expect_identical(
          object = df_out$df_bottom_int_ctrl,
          expected = df_rand$list_df_long$df_bottom_long$df_int_ctrl |>
            dplyr::select(
              dplyr::all_of(colnames(df_out$df_bottom_int_ctrl))
            )
        )

      }
    )

    ## NPX no int ctrl v2 ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = FALSE,
          version = 2L
        )

        # column names of each subset of data
        col_names <- sapply(df_rand$list_df_long$df_top_long,
                            function(x) x$col_index)
        col_names <- col_names[which(lapply(col_names, length) != 0L)]
        names(col_names) <- strsplit(x = names(col_names), split = "_") |>
          lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
          unlist()
        names(col_names) <- paste0("df_top_", names(col_names))

        # plate panel combos
        df_plate_panel <- df_rand$list_df_long$df_oid_long |>
          dplyr::select(
            dplyr::all_of(c("col_index", "Panel"))
          ) |>
          dplyr::distinct() |>
          dplyr::left_join(
            df_rand$list_df_long$df_plate_long |>
              dplyr::select(
                dplyr::all_of(c("PlateID", "Panel"))
              ) |>
              dplyr::distinct(),
            by = "Panel",
            relationship = "many-to-many"
          )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_bottom(
            df = df_rand$list_df_wide$df_bottom_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            data_type = data_type,
            col_names = col_names,
            format_spec = get_format_spec(data_type = data_type),
            df_plate_panel = df_plate_panel
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out$df_bottom_oid) |> sort(),
          expected = colnames(df_rand$list_df_long$df_bottom_long$df_oid) |>
            sort()
        )
        expect_identical(
          object = df_out$df_bottom_oid,
          expected = df_rand$list_df_long$df_bottom_long$df_oid |>
            dplyr::select(
              dplyr::all_of(colnames(df_out$df_bottom_oid))
            )
        )

        expect_false(
          object = "df_bottom_int_ctrl" %in% names(df_out)
        )

        expect_false(
          object = "df_int_ctrl" %in% names(df_rand$list_df_long$df_bottom_long)
        )

      }
    )

    ## NPX with int ctrl v2 ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = TRUE,
          version = 2L
        )

        # column names of each subset of data
        col_names <- sapply(df_rand$list_df_long$df_top_long,
                            function(x) x$col_index)
        col_names <- col_names[which(lapply(col_names, length) != 0L)]
        names(col_names) <- strsplit(x = names(col_names), split = "_") |>
          lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
          unlist()
        names(col_names) <- paste0("df_top_", names(col_names))

        # plate panel combos
        df_plate_panel <- df_rand$list_df_long$df_oid_long |>
          dplyr::select(
            dplyr::all_of(c("col_index", "Panel"))
          ) |>
          dplyr::distinct() |>
          dplyr::bind_rows(
            df_rand$list_df_long$df_int_ctrl_long |>
              dplyr::select(
                dplyr::all_of(c("col_index", "Panel"))
              ) |>
              dplyr::distinct()
          ) |>
          dplyr::left_join(
            df_rand$list_df_long$df_plate_long |>
              dplyr::select(
                dplyr::all_of(c("PlateID", "Panel"))
              ) |>
              dplyr::distinct(),
            by = "Panel",
            relationship = "many-to-many"
          )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_bottom(
            df = df_rand$list_df_wide$df_bottom_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            data_type = data_type,
            col_names = col_names,
            format_spec = get_format_spec(data_type = data_type),
            df_plate_panel = df_plate_panel
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out$df_bottom_oid) |> sort(),
          expected = colnames(df_rand$list_df_long$df_bottom_long$df_oid) |>
            sort()
        )
        expect_identical(
          object = df_out$df_bottom_oid,
          expected = df_rand$list_df_long$df_bottom_long$df_oid |>
            dplyr::select(
              dplyr::all_of(colnames(df_out$df_bottom_oid))
            )
        )

        expect_identical(
          object = colnames(df_out$df_bottom_int_ctrl) |> sort(),
          expected = df_rand$list_df_long$df_bottom_long$df_int_ctrl |>
            colnames() |>
            sort()
        )
        expect_identical(
          object = df_out$df_bottom_int_ctrl,
          expected = df_rand$list_df_long$df_bottom_long$df_int_ctrl |>
            dplyr::select(
              dplyr::all_of(colnames(df_out$df_bottom_int_ctrl))
            )
        )

      }
    )

    ## Quantified no int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Quantified"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = FALSE,
          version = 0L
        )

        # column names of each subset of data
        col_names <- sapply(df_rand$list_df_long$df_top_long,
                            function(x) x$col_index)
        col_names <- col_names[which(lapply(col_names, length) != 0L)]
        names(col_names) <- strsplit(x = names(col_names), split = "_") |>
          lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
          unlist()
        names(col_names) <- paste0("df_top_", names(col_names))

        # plate panel combos
        df_plate_panel <- df_rand$list_df_long$df_oid_long |>
          dplyr::select(
            dplyr::all_of(c("col_index", "Panel"))
          ) |>
          dplyr::distinct() |>
          dplyr::left_join(
            df_rand$list_df_long$df_plate_long |>
              dplyr::select(
                dplyr::all_of(c("PlateID", "Panel"))
              ) |>
              dplyr::distinct(),
            by = "Panel",
            relationship = "many-to-many"
          )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_bottom(
            df = df_rand$list_df_wide$df_bottom_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            data_type = data_type,
            col_names = col_names,
            format_spec = get_format_spec(data_type = data_type),
            df_plate_panel = df_plate_panel
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out$df_bottom_oid) |> sort(),
          expected = colnames(df_rand$list_df_long$df_bottom_long$df_oid) |>
            sort()
        )
        expect_identical(
          object = df_out$df_bottom_oid,
          expected = df_rand$list_df_long$df_bottom_long$df_oid |>
            dplyr::select(
              dplyr::all_of(colnames(df_out$df_bottom_oid))
            )
        )

        expect_false(
          object = "df_bottom_int_ctrl" %in% names(df_out)
        )

        expect_false(
          object = "df_int_ctrl" %in% names(df_rand$list_df_long$df_bottom_long)
        )

      }
    )

    ## Quantified with int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Quantified"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = TRUE,
          version = 0L
        )

        # column names of each subset of data
        col_names <- sapply(df_rand$list_df_long$df_top_long,
                            function(x) x$col_index)
        col_names <- col_names[which(lapply(col_names, length) != 0L)]
        names(col_names) <- strsplit(x = names(col_names), split = "_") |>
          lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
          unlist()
        names(col_names) <- paste0("df_top_", names(col_names))

        # plate panel combos
        df_plate_panel <- df_rand$list_df_long$df_oid_long |>
          dplyr::select(
            dplyr::all_of(c("col_index", "Panel"))
          ) |>
          dplyr::distinct() |>
          dplyr::bind_rows(
            df_rand$list_df_long$df_int_ctrl_long |>
              dplyr::select(
                dplyr::all_of(c("col_index", "Panel"))
              ) |>
              dplyr::distinct()
          ) |>
          dplyr::left_join(
            df_rand$list_df_long$df_plate_long |>
              dplyr::select(
                dplyr::all_of(c("PlateID", "Panel"))
              ) |>
              dplyr::distinct(),
            by = "Panel",
            relationship = "many-to-many"
          )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_bottom(
            df = df_rand$list_df_wide$df_bottom_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            data_type = data_type,
            col_names = col_names,
            format_spec = get_format_spec(data_type = data_type),
            df_plate_panel = df_plate_panel
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out$df_bottom_oid) |> sort(),
          expected = colnames(df_rand$list_df_long$df_bottom_long$df_oid) |>
            sort()
        )
        expect_identical(
          object = df_out$df_bottom_oid,
          expected = df_rand$list_df_long$df_bottom_long$df_oid |>
            dplyr::select(
              dplyr::all_of(colnames(df_out$df_bottom_oid))
            )
        )

        expect_identical(
          object = colnames(df_out$df_bottom_int_ctrl) |> sort(),
          expected = df_rand$list_df_long$df_bottom_long$df_int_ctrl |>
            colnames() |>
            sort()
        )
        expect_identical(
          object = df_out$df_bottom_int_ctrl,
          expected = df_rand$list_df_long$df_bottom_long$df_int_ctrl |>
            dplyr::select(
              dplyr::all_of(colnames(df_out$df_bottom_int_ctrl))
            )
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_bottom - works - T48 - single panel - multiple plates",
  {
    # variables that apply to all tests
    olink_platform <- "Target 48"
    n_panels <- 1L
    n_assays <- 45L
    n_samples <- 99L
    show_dev_int_ctrl <- FALSE

    ## NPX no int ctrl v1 ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = FALSE,
          version = 1L
        )

        # column names of each subset of data
        col_names <- sapply(df_rand$list_df_long$df_top_long,
                            function(x) x$col_index)
        col_names <- col_names[which(lapply(col_names, length) != 0L)]
        names(col_names) <- strsplit(x = names(col_names), split = "_") |>
          lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
          unlist()
        names(col_names) <- paste0("df_top_", names(col_names))

        # plate panel combos
        df_plate_panel <- df_rand$list_df_long$df_oid_long |>
          dplyr::select(
            dplyr::all_of(c("col_index", "Panel"))
          ) |>
          dplyr::distinct() |>
          dplyr::left_join(
            df_rand$list_df_long$df_plate_long |>
              dplyr::select(
                dplyr::all_of(c("PlateID", "Panel"))
              ) |>
              dplyr::distinct(),
            by = "Panel",
            relationship = "many-to-many"
          )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_bottom(
            df = df_rand$list_df_wide$df_bottom_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            data_type = data_type,
            col_names = col_names,
            format_spec = get_format_spec(data_type = data_type),
            df_plate_panel = df_plate_panel
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out$df_bottom_oid) |> sort(),
          expected = colnames(df_rand$list_df_long$df_bottom_long$df_oid) |>
            sort()
        )
        expect_identical(
          object = df_out$df_bottom_oid,
          expected = df_rand$list_df_long$df_bottom_long$df_oid |>
            dplyr::select(
              dplyr::all_of(colnames(df_out$df_bottom_oid))
            )
        )

        expect_false(
          object = "df_bottom_int_ctrl" %in% names(df_out)
        )

        expect_false(
          object = "df_int_ctrl" %in% names(df_rand$list_df_long$df_bottom_long)
        )

      }
    )

    ## NPX with int ctrl v1 ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = TRUE,
          version = 1L
        )

        # column names of each subset of data
        col_names <- sapply(df_rand$list_df_long$df_top_long,
                            function(x) x$col_index)
        col_names <- col_names[which(lapply(col_names, length) != 0L)]
        names(col_names) <- strsplit(x = names(col_names), split = "_") |>
          lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
          unlist()
        names(col_names) <- paste0("df_top_", names(col_names))

        # plate panel combos
        df_plate_panel <- df_rand$list_df_long$df_oid_long |>
          dplyr::select(
            dplyr::all_of(c("col_index", "Panel"))
          ) |>
          dplyr::distinct() |>
          dplyr::bind_rows(
            df_rand$list_df_long$df_int_ctrl_long |>
              dplyr::select(
                dplyr::all_of(c("col_index", "Panel"))
              ) |>
              dplyr::distinct()
          ) |>
          dplyr::left_join(
            df_rand$list_df_long$df_plate_long |>
              dplyr::select(
                dplyr::all_of(c("PlateID", "Panel"))
              ) |>
              dplyr::distinct(),
            by = "Panel",
            relationship = "many-to-many"
          )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_bottom(
            df = df_rand$list_df_wide$df_bottom_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            data_type = data_type,
            col_names = col_names,
            format_spec = get_format_spec(data_type = data_type),
            df_plate_panel = df_plate_panel
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out$df_bottom_oid) |> sort(),
          expected = colnames(df_rand$list_df_long$df_bottom_long$df_oid) |>
            sort()
        )
        expect_identical(
          object = df_out$df_bottom_oid,
          expected = df_rand$list_df_long$df_bottom_long$df_oid |>
            dplyr::select(
              dplyr::all_of(colnames(df_out$df_bottom_oid))
            )
        )

        expect_identical(
          object = colnames(df_out$df_bottom_int_ctrl) |> sort(),
          expected = df_rand$list_df_long$df_bottom_long$df_int_ctrl |>
            colnames() |>
            sort()
        )
        expect_identical(
          object = df_out$df_bottom_int_ctrl,
          expected = df_rand$list_df_long$df_bottom_long$df_int_ctrl |>
            dplyr::select(
              dplyr::all_of(colnames(df_out$df_bottom_int_ctrl))
            )
        )

      }
    )

    ## NPX no int ctrl v2 ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = FALSE,
          version = 2L
        )

        # column names of each subset of data
        col_names <- sapply(df_rand$list_df_long$df_top_long,
                            function(x) x$col_index)
        col_names <- col_names[which(lapply(col_names, length) != 0L)]
        names(col_names) <- strsplit(x = names(col_names), split = "_") |>
          lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
          unlist()
        names(col_names) <- paste0("df_top_", names(col_names))

        # plate panel combos
        df_plate_panel <- df_rand$list_df_long$df_oid_long |>
          dplyr::select(
            dplyr::all_of(c("col_index", "Panel"))
          ) |>
          dplyr::distinct() |>
          dplyr::left_join(
            df_rand$list_df_long$df_plate_long |>
              dplyr::select(
                dplyr::all_of(c("PlateID", "Panel"))
              ) |>
              dplyr::distinct(),
            by = "Panel",
            relationship = "many-to-many"
          )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_bottom(
            df = df_rand$list_df_wide$df_bottom_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            data_type = data_type,
            col_names = col_names,
            format_spec = get_format_spec(data_type = data_type),
            df_plate_panel = df_plate_panel
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out$df_bottom_oid) |> sort(),
          expected = colnames(df_rand$list_df_long$df_bottom_long$df_oid) |>
            sort()
        )
        expect_identical(
          object = df_out$df_bottom_oid,
          expected = df_rand$list_df_long$df_bottom_long$df_oid |>
            dplyr::select(
              dplyr::all_of(colnames(df_out$df_bottom_oid))
            )
        )

        expect_false(
          object = "df_bottom_int_ctrl" %in% names(df_out)
        )

        expect_false(
          object = "df_int_ctrl" %in% names(df_rand$list_df_long$df_bottom_long)
        )

      }
    )

    ## NPX with int ctrl v2 ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = TRUE,
          version = 2L
        )

        # column names of each subset of data
        col_names <- sapply(df_rand$list_df_long$df_top_long,
                            function(x) x$col_index)
        col_names <- col_names[which(lapply(col_names, length) != 0L)]
        names(col_names) <- strsplit(x = names(col_names), split = "_") |>
          lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
          unlist()
        names(col_names) <- paste0("df_top_", names(col_names))

        # plate panel combos
        df_plate_panel <- df_rand$list_df_long$df_oid_long |>
          dplyr::select(
            dplyr::all_of(c("col_index", "Panel"))
          ) |>
          dplyr::distinct() |>
          dplyr::bind_rows(
            df_rand$list_df_long$df_int_ctrl_long |>
              dplyr::select(
                dplyr::all_of(c("col_index", "Panel"))
              ) |>
              dplyr::distinct()
          ) |>
          dplyr::left_join(
            df_rand$list_df_long$df_plate_long |>
              dplyr::select(
                dplyr::all_of(c("PlateID", "Panel"))
              ) |>
              dplyr::distinct(),
            by = "Panel",
            relationship = "many-to-many"
          )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_bottom(
            df = df_rand$list_df_wide$df_bottom_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            data_type = data_type,
            col_names = col_names,
            format_spec = get_format_spec(data_type = data_type),
            df_plate_panel = df_plate_panel
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out$df_bottom_oid) |> sort(),
          expected = colnames(df_rand$list_df_long$df_bottom_long$df_oid) |>
            sort()
        )
        expect_identical(
          object = df_out$df_bottom_oid,
          expected = df_rand$list_df_long$df_bottom_long$df_oid |>
            dplyr::select(
              dplyr::all_of(colnames(df_out$df_bottom_oid))
            )
        )

        expect_identical(
          object = colnames(df_out$df_bottom_int_ctrl) |> sort(),
          expected = df_rand$list_df_long$df_bottom_long$df_int_ctrl |>
            colnames() |>
            sort()
        )
        expect_identical(
          object = df_out$df_bottom_int_ctrl,
          expected = df_rand$list_df_long$df_bottom_long$df_int_ctrl |>
            dplyr::select(
              dplyr::all_of(colnames(df_out$df_bottom_int_ctrl))
            )
        )

      }
    )

    ## Quantified no int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Quantified"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = FALSE,
          version = 0L
        )

        # column names of each subset of data
        col_names <- sapply(df_rand$list_df_long$df_top_long,
                            function(x) x$col_index)
        col_names <- col_names[which(lapply(col_names, length) != 0L)]
        names(col_names) <- strsplit(x = names(col_names), split = "_") |>
          lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
          unlist()
        names(col_names) <- paste0("df_top_", names(col_names))

        # plate panel combos
        df_plate_panel <- df_rand$list_df_long$df_oid_long |>
          dplyr::select(
            dplyr::all_of(c("col_index", "Panel"))
          ) |>
          dplyr::distinct() |>
          dplyr::left_join(
            df_rand$list_df_long$df_plate_long |>
              dplyr::select(
                dplyr::all_of(c("PlateID", "Panel"))
              ) |>
              dplyr::distinct(),
            by = "Panel",
            relationship = "many-to-many"
          )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_bottom(
            df = df_rand$list_df_wide$df_bottom_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            data_type = data_type,
            col_names = col_names,
            format_spec = get_format_spec(data_type = data_type),
            df_plate_panel = df_plate_panel
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out$df_bottom_oid) |> sort(),
          expected = colnames(df_rand$list_df_long$df_bottom_long$df_oid) |>
            sort()
        )
        expect_identical(
          object = df_out$df_bottom_oid,
          expected = df_rand$list_df_long$df_bottom_long$df_oid |>
            dplyr::select(
              dplyr::all_of(colnames(df_out$df_bottom_oid))
            )
        )

        expect_false(
          object = "df_bottom_int_ctrl" %in% names(df_out)
        )

        expect_false(
          object = "df_int_ctrl" %in% names(df_rand$list_df_long$df_bottom_long)
        )

      }
    )

    ## Quantified with int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Quantified"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = TRUE,
          version = 0L
        )

        # column names of each subset of data
        col_names <- sapply(df_rand$list_df_long$df_top_long,
                            function(x) x$col_index)
        col_names <- col_names[which(lapply(col_names, length) != 0L)]
        names(col_names) <- strsplit(x = names(col_names), split = "_") |>
          lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
          unlist()
        names(col_names) <- paste0("df_top_", names(col_names))

        # plate panel combos
        df_plate_panel <- df_rand$list_df_long$df_oid_long |>
          dplyr::select(
            dplyr::all_of(c("col_index", "Panel"))
          ) |>
          dplyr::distinct() |>
          dplyr::bind_rows(
            df_rand$list_df_long$df_int_ctrl_long |>
              dplyr::select(
                dplyr::all_of(c("col_index", "Panel"))
              ) |>
              dplyr::distinct()
          ) |>
          dplyr::left_join(
            df_rand$list_df_long$df_plate_long |>
              dplyr::select(
                dplyr::all_of(c("PlateID", "Panel"))
              ) |>
              dplyr::distinct(),
            by = "Panel",
            relationship = "many-to-many"
          )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_bottom(
            df = df_rand$list_df_wide$df_bottom_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            data_type = data_type,
            col_names = col_names,
            format_spec = get_format_spec(data_type = data_type),
            df_plate_panel = df_plate_panel
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out$df_bottom_oid) |> sort(),
          expected = colnames(df_rand$list_df_long$df_bottom_long$df_oid) |>
            sort()
        )
        expect_identical(
          object = df_out$df_bottom_oid,
          expected = df_rand$list_df_long$df_bottom_long$df_oid |>
            dplyr::select(
              dplyr::all_of(colnames(df_out$df_bottom_oid))
            )
        )

        expect_identical(
          object = colnames(df_out$df_bottom_int_ctrl) |> sort(),
          expected = df_rand$list_df_long$df_bottom_long$df_int_ctrl |>
            colnames() |>
            sort()
        )
        expect_identical(
          object = df_out$df_bottom_int_ctrl,
          expected = df_rand$list_df_long$df_bottom_long$df_int_ctrl |>
            dplyr::select(
              dplyr::all_of(colnames(df_out$df_bottom_int_ctrl))
            )
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_bottom - works - T48 - multiple panels - multiple plates",
  {
    # variables that apply to all tests
    olink_platform <- "Target 48"
    n_panels <- 3L
    n_assays <- 45L
    n_samples <- 99L
    show_dev_int_ctrl <- FALSE

    ## NPX no int ctrl v1 ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = FALSE,
          version = 1L
        )

        # column names of each subset of data
        col_names <- sapply(df_rand$list_df_long$df_top_long,
                            function(x) x$col_index)
        col_names <- col_names[which(lapply(col_names, length) != 0L)]
        names(col_names) <- strsplit(x = names(col_names), split = "_") |>
          lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
          unlist()
        names(col_names) <- paste0("df_top_", names(col_names))

        # plate panel combos
        df_plate_panel <- df_rand$list_df_long$df_oid_long |>
          dplyr::select(
            dplyr::all_of(c("col_index", "Panel"))
          ) |>
          dplyr::distinct() |>
          dplyr::left_join(
            df_rand$list_df_long$df_plate_long |>
              dplyr::select(
                dplyr::all_of(c("PlateID", "Panel"))
              ) |>
              dplyr::distinct(),
            by = "Panel",
            relationship = "many-to-many"
          )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_bottom(
            df = df_rand$list_df_wide$df_bottom_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            data_type = data_type,
            col_names = col_names,
            format_spec = get_format_spec(data_type = data_type),
            df_plate_panel = df_plate_panel
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out$df_bottom_oid) |> sort(),
          expected = colnames(df_rand$list_df_long$df_bottom_long$df_oid) |>
            sort()
        )
        expect_identical(
          object = df_out$df_bottom_oid,
          expected = df_rand$list_df_long$df_bottom_long$df_oid |>
            dplyr::select(
              dplyr::all_of(colnames(df_out$df_bottom_oid))
            )
        )

        expect_false(
          object = "df_bottom_int_ctrl" %in% names(df_out)
        )

        expect_false(
          object = "df_int_ctrl" %in% names(df_rand$list_df_long$df_bottom_long)
        )

      }
    )

    ## NPX with int ctrl v1 ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = TRUE,
          version = 1L
        )

        # column names of each subset of data
        col_names <- sapply(df_rand$list_df_long$df_top_long,
                            function(x) x$col_index)
        col_names <- col_names[which(lapply(col_names, length) != 0L)]
        names(col_names) <- strsplit(x = names(col_names), split = "_") |>
          lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
          unlist()
        names(col_names) <- paste0("df_top_", names(col_names))

        # plate panel combos
        df_plate_panel <- df_rand$list_df_long$df_oid_long |>
          dplyr::select(
            dplyr::all_of(c("col_index", "Panel"))
          ) |>
          dplyr::distinct() |>
          dplyr::bind_rows(
            df_rand$list_df_long$df_int_ctrl_long |>
              dplyr::select(
                dplyr::all_of(c("col_index", "Panel"))
              ) |>
              dplyr::distinct()
          ) |>
          dplyr::left_join(
            df_rand$list_df_long$df_plate_long |>
              dplyr::select(
                dplyr::all_of(c("PlateID", "Panel"))
              ) |>
              dplyr::distinct(),
            by = "Panel",
            relationship = "many-to-many"
          )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_bottom(
            df = df_rand$list_df_wide$df_bottom_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            data_type = data_type,
            col_names = col_names,
            format_spec = get_format_spec(data_type = data_type),
            df_plate_panel = df_plate_panel
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out$df_bottom_oid) |> sort(),
          expected = colnames(df_rand$list_df_long$df_bottom_long$df_oid) |>
            sort()
        )
        expect_identical(
          object = df_out$df_bottom_oid,
          expected = df_rand$list_df_long$df_bottom_long$df_oid |>
            dplyr::select(
              dplyr::all_of(colnames(df_out$df_bottom_oid))
            )
        )

        expect_identical(
          object = colnames(df_out$df_bottom_int_ctrl) |> sort(),
          expected = df_rand$list_df_long$df_bottom_long$df_int_ctrl |>
            colnames() |>
            sort()
        )
        expect_identical(
          object = df_out$df_bottom_int_ctrl,
          expected = df_rand$list_df_long$df_bottom_long$df_int_ctrl |>
            dplyr::select(
              dplyr::all_of(colnames(df_out$df_bottom_int_ctrl))
            )
        )

      }
    )

    ## NPX no int ctrl v2 ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = FALSE,
          version = 2L
        )

        # column names of each subset of data
        col_names <- sapply(df_rand$list_df_long$df_top_long,
                            function(x) x$col_index)
        col_names <- col_names[which(lapply(col_names, length) != 0L)]
        names(col_names) <- strsplit(x = names(col_names), split = "_") |>
          lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
          unlist()
        names(col_names) <- paste0("df_top_", names(col_names))

        # plate panel combos
        df_plate_panel <- df_rand$list_df_long$df_oid_long |>
          dplyr::select(
            dplyr::all_of(c("col_index", "Panel"))
          ) |>
          dplyr::distinct() |>
          dplyr::left_join(
            df_rand$list_df_long$df_plate_long |>
              dplyr::select(
                dplyr::all_of(c("PlateID", "Panel"))
              ) |>
              dplyr::distinct(),
            by = "Panel",
            relationship = "many-to-many"
          )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_bottom(
            df = df_rand$list_df_wide$df_bottom_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            data_type = data_type,
            col_names = col_names,
            format_spec = get_format_spec(data_type = data_type),
            df_plate_panel = df_plate_panel
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out$df_bottom_oid) |> sort(),
          expected = colnames(df_rand$list_df_long$df_bottom_long$df_oid) |>
            sort()
        )
        expect_identical(
          object = df_out$df_bottom_oid,
          expected = df_rand$list_df_long$df_bottom_long$df_oid |>
            dplyr::select(
              dplyr::all_of(colnames(df_out$df_bottom_oid))
            )
        )

        expect_false(
          object = "df_bottom_int_ctrl" %in% names(df_out)
        )

        expect_false(
          object = "df_int_ctrl" %in% names(df_rand$list_df_long$df_bottom_long)
        )

      }
    )

    ## NPX with int ctrl v2 ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = TRUE,
          version = 2L
        )

        # column names of each subset of data
        col_names <- sapply(df_rand$list_df_long$df_top_long,
                            function(x) x$col_index)
        col_names <- col_names[which(lapply(col_names, length) != 0L)]
        names(col_names) <- strsplit(x = names(col_names), split = "_") |>
          lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
          unlist()
        names(col_names) <- paste0("df_top_", names(col_names))

        # plate panel combos
        df_plate_panel <- df_rand$list_df_long$df_oid_long |>
          dplyr::select(
            dplyr::all_of(c("col_index", "Panel"))
          ) |>
          dplyr::distinct() |>
          dplyr::bind_rows(
            df_rand$list_df_long$df_int_ctrl_long |>
              dplyr::select(
                dplyr::all_of(c("col_index", "Panel"))
              ) |>
              dplyr::distinct()
          ) |>
          dplyr::left_join(
            df_rand$list_df_long$df_plate_long |>
              dplyr::select(
                dplyr::all_of(c("PlateID", "Panel"))
              ) |>
              dplyr::distinct(),
            by = "Panel",
            relationship = "many-to-many"
          )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_bottom(
            df = df_rand$list_df_wide$df_bottom_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            data_type = data_type,
            col_names = col_names,
            format_spec = get_format_spec(data_type = data_type),
            df_plate_panel = df_plate_panel
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out$df_bottom_oid) |> sort(),
          expected = colnames(df_rand$list_df_long$df_bottom_long$df_oid) |>
            sort()
        )
        expect_identical(
          object = df_out$df_bottom_oid,
          expected = df_rand$list_df_long$df_bottom_long$df_oid |>
            dplyr::select(
              dplyr::all_of(colnames(df_out$df_bottom_oid))
            )
        )

        expect_identical(
          object = colnames(df_out$df_bottom_int_ctrl) |> sort(),
          expected = df_rand$list_df_long$df_bottom_long$df_int_ctrl |>
            colnames() |>
            sort()
        )
        expect_identical(
          object = df_out$df_bottom_int_ctrl,
          expected = df_rand$list_df_long$df_bottom_long$df_int_ctrl |>
            dplyr::select(
              dplyr::all_of(colnames(df_out$df_bottom_int_ctrl))
            )
        )

      }
    )

    ## Quantified no int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Quantified"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = FALSE,
          version = 0L
        )

        # column names of each subset of data
        col_names <- sapply(df_rand$list_df_long$df_top_long,
                            function(x) x$col_index)
        col_names <- col_names[which(lapply(col_names, length) != 0L)]
        names(col_names) <- strsplit(x = names(col_names), split = "_") |>
          lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
          unlist()
        names(col_names) <- paste0("df_top_", names(col_names))

        # plate panel combos
        df_plate_panel <- df_rand$list_df_long$df_oid_long |>
          dplyr::select(
            dplyr::all_of(c("col_index", "Panel"))
          ) |>
          dplyr::distinct() |>
          dplyr::left_join(
            df_rand$list_df_long$df_plate_long |>
              dplyr::select(
                dplyr::all_of(c("PlateID", "Panel"))
              ) |>
              dplyr::distinct(),
            by = "Panel",
            relationship = "many-to-many"
          )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_bottom(
            df = df_rand$list_df_wide$df_bottom_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            data_type = data_type,
            col_names = col_names,
            format_spec = get_format_spec(data_type = data_type),
            df_plate_panel = df_plate_panel
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out$df_bottom_oid) |> sort(),
          expected = colnames(df_rand$list_df_long$df_bottom_long$df_oid) |>
            sort()
        )
        expect_identical(
          object = df_out$df_bottom_oid,
          expected = df_rand$list_df_long$df_bottom_long$df_oid |>
            dplyr::select(
              dplyr::all_of(colnames(df_out$df_bottom_oid))
            )
        )

        expect_false(
          object = "df_bottom_int_ctrl" %in% names(df_out)
        )

        expect_false(
          object = "df_int_ctrl" %in% names(df_rand$list_df_long$df_bottom_long)
        )

      }
    )

    ## Quantified with int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Quantified"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = TRUE,
          version = 0L
        )

        # column names of each subset of data
        col_names <- sapply(df_rand$list_df_long$df_top_long,
                            function(x) x$col_index)
        col_names <- col_names[which(lapply(col_names, length) != 0L)]
        names(col_names) <- strsplit(x = names(col_names), split = "_") |>
          lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
          unlist()
        names(col_names) <- paste0("df_top_", names(col_names))

        # plate panel combos
        df_plate_panel <- df_rand$list_df_long$df_oid_long |>
          dplyr::select(
            dplyr::all_of(c("col_index", "Panel"))
          ) |>
          dplyr::distinct() |>
          dplyr::bind_rows(
            df_rand$list_df_long$df_int_ctrl_long |>
              dplyr::select(
                dplyr::all_of(c("col_index", "Panel"))
              ) |>
              dplyr::distinct()
          ) |>
          dplyr::left_join(
            df_rand$list_df_long$df_plate_long |>
              dplyr::select(
                dplyr::all_of(c("PlateID", "Panel"))
              ) |>
              dplyr::distinct(),
            by = "Panel",
            relationship = "many-to-many"
          )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_bottom(
            df = df_rand$list_df_wide$df_bottom_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            data_type = data_type,
            col_names = col_names,
            format_spec = get_format_spec(data_type = data_type),
            df_plate_panel = df_plate_panel
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out$df_bottom_oid) |> sort(),
          expected = colnames(df_rand$list_df_long$df_bottom_long$df_oid) |>
            sort()
        )
        expect_identical(
          object = df_out$df_bottom_oid,
          expected = df_rand$list_df_long$df_bottom_long$df_oid |>
            dplyr::select(
              dplyr::all_of(colnames(df_out$df_bottom_oid))
            )
        )

        expect_identical(
          object = colnames(df_out$df_bottom_int_ctrl) |> sort(),
          expected = df_rand$list_df_long$df_bottom_long$df_int_ctrl |>
            colnames() |>
            sort()
        )
        expect_identical(
          object = df_out$df_bottom_int_ctrl,
          expected = df_rand$list_df_long$df_bottom_long$df_int_ctrl |>
            dplyr::select(
              dplyr::all_of(colnames(df_out$df_bottom_int_ctrl))
            )
        )

      }
    )

  }
)

# T96 NPX data v3
test_that(
  "read_npx_wide_bottom - works - T96 - NPX - v3",
  {
    # variables that apply to all tests
    olink_platform <- "Target 96"
    data_type <- "NPX"
    n_panels <- 3L
    n_assays <- 92L
    n_samples <- 99L

    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = n_panels,
      n_assays = n_assays,
      n_samples = n_samples,
      show_dev_int_ctrl = TRUE,
      show_int_ctrl = TRUE,
      version = 3L
    )

    # column names of each subset of data
    col_names <- sapply(df_rand$list_df_long$df_top_long,
                        function(x) x$col_index)
    col_names <- col_names[which(lapply(col_names, length) != 0L)]
    names(col_names) <- strsplit(x = names(col_names), split = "_") |>
      lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
      unlist()
    names(col_names) <- paste0("df_top_", names(col_names))

    # plate panel combos
    df_plate_panel <- df_rand$list_df_long$df_oid_long |>
      dplyr::select(
        dplyr::all_of(c("col_index", "Panel"))
      ) |>
      dplyr::distinct() |>
      dplyr::bind_rows(
        df_rand$list_df_long$df_int_ctrl_long |>
          dplyr::select(
            dplyr::all_of(c("col_index", "Panel"))
          ) |>
          dplyr::distinct()
      ) |>
      dplyr::left_join(
        df_rand$list_df_long$df_plate_long |>
          dplyr::select(
            dplyr::all_of(c("PlateID", "Panel"))
          ) |>
          dplyr::distinct(),
        by = "Panel",
        relationship = "many-to-many"
      )

    # check bottom df
    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_bottom(
            df = df_rand$list_df_wide$df_bottom_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            data_type = data_type,
            col_names = col_names,
            format_spec = get_format_spec(data_type = data_type),
            df_plate_panel = df_plate_panel
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out$df_bottom_oid) |> sort(),
          expected = colnames(df_rand$list_df_long$df_bottom_long$df_oid) |>
            sort()
        )
        expect_identical(
          object = df_out$df_bottom_oid,
          expected = df_rand$list_df_long$df_bottom_long$df_oid |>
            dplyr::select(
              dplyr::all_of(colnames(df_out$df_bottom_oid))
            )
        )

        expect_identical(
          object = colnames(df_out$df_bottom_int_ctrl) |> sort(),
          expected = df_rand$list_df_long$df_bottom_long$df_int_ctrl |>
            colnames() |>
            sort()
        )
        expect_identical(
          object = df_out$df_bottom_int_ctrl,
          expected = df_rand$list_df_long$df_bottom_long$df_int_ctrl |>
            dplyr::select(
              dplyr::all_of(colnames(df_out$df_bottom_int_ctrl))
            )
        )

      }
    )
  }
)

# T96 NPX data from 2018 with NPX Manager
test_that(
  "read_npx_wide_bottom - works - T96 - NPX - NPX Manager 2018",
  {
    # variables that apply to all tests
    olink_platform <- "Target 96"
    data_type <- "NPX"
    n_panels <- 3L
    n_assays <- 92L
    n_samples <- 99L
    show_dev_int_ctrl <- FALSE

    df_rand <- get_wide_synthetic_data(
      olink_platform = olink_platform,
      data_type = data_type,
      n_panels = n_panels,
      n_assays = n_assays,
      n_samples = n_samples,
      show_dev_int_ctrl = show_dev_int_ctrl,
      show_int_ctrl = FALSE,
      version = 1L
    )

    # column names of each subset of data
    col_names <- sapply(df_rand$list_df_long$df_top_long,
                        function(x) x$col_index)
    col_names <- col_names[which(lapply(col_names, length) != 0L)]
    names(col_names) <- strsplit(x = names(col_names), split = "_") |>
      lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
      unlist()
    names(col_names) <- paste0("df_top_", names(col_names))

    # plate panel combos
    df_plate_panel <- df_rand$list_df_long$df_oid_long |>
      dplyr::select(
        dplyr::all_of(c("col_index", "Panel"))
      ) |>
      dplyr::distinct() |>
      dplyr::left_join(
        df_rand$list_df_long$df_plate_long |>
          dplyr::select(
            dplyr::all_of(c("PlateID", "Panel"))
          ) |>
          dplyr::distinct(),
        by = "Panel",
        relationship = "many-to-many"
      )

    # check bottom df
    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs
        expect_no_condition(
          object = df_out <- read_npx_wide_bottom(
            df = df_rand$list_df_wide$df_bottom_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            data_type = data_type,
            col_names = col_names,
            format_spec = get_format_spec(data_type = data_type),
            df_plate_panel = df_plate_panel
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out$df_bottom_oid) |> sort(),
          expected = colnames(df_rand$list_df_long$df_bottom_long$df_oid) |>
            sort()
        )
        expect_identical(
          object = df_out$df_bottom_oid,
          expected = df_rand$list_df_long$df_bottom_long$df_oid |>
            dplyr::select(
              dplyr::all_of(colnames(df_out$df_bottom_oid))
            )
        )

        expect_false(
          object = "df_bottom_int_ctrl" %in% names(df_out)
        )

        expect_false(
          object = "df_int_ctrl" %in% names(df_rand$list_df_long$df_bottom_long)
        )

      }
    )
  }
)

test_that(
  "read_npx_wide_bottom - error - unexpected values in V1",
  {
    # variables that apply to all tests
    olink_platform <- "Target 48"
    n_panels <- 1L
    n_assays <- 45L
    n_samples <- 88L
    show_dev_int_ctrl <- FALSE

    ## NPX - LOD2 ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = FALSE,
          version = 1L
        )

        # column names of each subset of data
        col_names <- sapply(df_rand$list_df_long$df_top_long,
                            function(x) x$col_index)
        col_names <- col_names[which(lapply(col_names, length) != 0L)]
        names(col_names) <- strsplit(x = names(col_names), split = "_") |>
          lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
          unlist()
        names(col_names) <- paste0("df_top_", names(col_names))

        # plate panel combos
        df_plate_panel <- df_rand$list_df_long$df_long |>
          dplyr::select(
            dplyr::all_of(c("Panel", "PlateID"))
          ) |>
          dplyr::distinct() |>
          dplyr::left_join(
            df_rand$list_df_long$df_plate_long |>
              dplyr::select(
                dplyr::all_of(c("PlateID", "Panel"))
              ) |>
              dplyr::distinct(),
            by = "Panel",
            relationship = "many-to-many"
          )

        # modify df with wrong V1
        df_rand$list_df_wide$df_bottom_wide <-
          df_rand$list_df_wide$df_bottom_wide |>
          dplyr::mutate(
            V1 = dplyr::if_else(.data[["V1"]] == "LOD",
                                "LOD2",
                                .data[["V1"]])
          )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs with error
        expect_error(
          object = df_out <- read_npx_wide_bottom(
            df = df_rand$list_df_wide$df_bottom_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            data_type = data_type,
            col_names = col_names,
            format_spec = get_format_spec(data_type = data_type),
            df_plate_panel = df_plate_panel
          ),
          regexp = "Unexpected values in column 1 of the bottom matrix with QC"
        )

      }
    )

    ## NPX - Additional vals in V1 ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = "Quantified",
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = FALSE,
          version = 0L
        )

        # column names of each subset of data
        col_names <- sapply(df_rand$list_df_long$df_top_long,
                            function(x) x$col_index)
        col_names <- col_names[which(lapply(col_names, length) != 0L)]
        names(col_names) <- strsplit(x = names(col_names), split = "_") |>
          lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
          unlist()
        names(col_names) <- paste0("df_top_", names(col_names))

        # plate panel combos
        df_plate_panel <- df_rand$list_df_long$df_long |>
          dplyr::select(
            dplyr::all_of(c("Panel", "PlateID"))
          ) |>
          dplyr::distinct() |>
          dplyr::left_join(
            df_rand$list_df_long$df_plate_long |>
              dplyr::select(
                dplyr::all_of(c("PlateID", "Panel"))
              ) |>
              dplyr::distinct(),
            by = "Panel",
            relationship = "many-to-many"
          )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs with error
        expect_error(
          object = df_out <- read_npx_wide_bottom(
            df = df_rand$list_df_wide$df_bottom_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            data_type = data_type,
            col_names = col_names,
            format_spec = get_format_spec(data_type = data_type),
            df_plate_panel = df_plate_panel
          ),
          regexp = "Unexpected values in column 1 of the bottom matrix with QC"
        )

      }
    )

    ## Quantified v1 ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Quantified"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = FALSE,
          version = 0L
        )

        # column names of each subset of data
        col_names <- sapply(df_rand$list_df_long$df_top_long,
                            function(x) x$col_index)
        col_names <- col_names[which(lapply(col_names, length) != 0L)]
        names(col_names) <- strsplit(x = names(col_names), split = "_") |>
          lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
          unlist()
        names(col_names) <- paste0("df_top_", names(col_names))

        # plate panel combos
        df_plate_panel <- df_rand$list_df_long$df_long |>
          dplyr::select(
            dplyr::all_of(c("Panel", "PlateID"))
          ) |>
          dplyr::distinct() |>
          dplyr::left_join(
            df_rand$list_df_long$df_plate_long |>
              dplyr::select(
                dplyr::all_of(c("PlateID", "Panel"))
              ) |>
              dplyr::distinct(),
            by = "Panel",
            relationship = "many-to-many"
          )

        # modify df with wrong V1
        df_rand$list_df_wide$df_bottom_wide <-
          df_rand$list_df_wide$df_bottom_wide |>
          dplyr::mutate(
            V1 = dplyr::if_else(.data[["V1"]] == "Assay warning",
                                "I_am_Unexpected",
                                .data[["V1"]])
          )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs with error
        expect_error(
          object = df_out <- read_npx_wide_bottom(
            df = df_rand$list_df_wide$df_bottom_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            data_type = data_type,
            col_names = col_names,
            format_spec = get_format_spec(data_type = data_type),
            df_plate_panel = df_plate_panel
          ),
          regexp = "Unexpected values in column 1 of the bottom matrix with QC"
        )

      }
    )

    ## Quantified v2 ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Quantified"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = "NPX",
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = FALSE,
          version = 1L
        )

        # column names of each subset of data
        col_names <- sapply(df_rand$list_df_long$df_top_long,
                            function(x) x$col_index)
        col_names <- col_names[which(lapply(col_names, length) != 0L)]
        names(col_names) <- strsplit(x = names(col_names), split = "_") |>
          lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
          unlist()
        names(col_names) <- paste0("df_top_", names(col_names))

        # plate panel combos
        df_plate_panel <- df_rand$list_df_long$df_long |>
          dplyr::select(
            dplyr::all_of(c("Panel", "PlateID"))
          ) |>
          dplyr::distinct() |>
          dplyr::left_join(
            df_rand$list_df_long$df_plate_long |>
              dplyr::select(
                dplyr::all_of(c("PlateID", "Panel"))
              ) |>
              dplyr::distinct(),
            by = "Panel",
            relationship = "many-to-many"
          )

        # modify df with wrong V1
        df_rand$list_df_wide$df_bottom_wide <-
          df_rand$list_df_wide$df_bottom_wide |>
          dplyr::mutate(
            V1 = dplyr::if_else(.data[["V1"]] == "Assay warning",
                                "I_am_Unexpected",
                                .data[["V1"]])
          )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs with error
        expect_error(
          object = df_out <- read_npx_wide_bottom(
            df = df_rand$list_df_wide$df_bottom_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            data_type = data_type,
            col_names = col_names,
            format_spec = get_format_spec(data_type = data_type),
            df_plate_panel = df_plate_panel
          ),
          regexp = "Unexpected values in column 1 of the bottom matrix with QC"
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_bottom - error - incorrect # of plates x QC data",
  {
    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Quantified"
        olink_platform <- "Target 48"

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = 1L,
          n_assays = 45L,
          n_samples = 99L,
          show_dev_int_ctrl = FALSE,
          show_int_ctrl = FALSE,
          version = 0L
        )

        # column names of each subset of data
        col_names <- sapply(df_rand$list_df_long$df_top_long,
                            function(x) x$col_index)
        col_names <- col_names[which(lapply(col_names, length) != 0L)]
        names(col_names) <- strsplit(x = names(col_names), split = "_") |>
          lapply(function(x) paste(x[x != "df"], collapse = "_")) |>
          unlist()
        names(col_names) <- paste0("df_top_", names(col_names))

        # plate panel combos
        df_plate_panel <- df_rand$list_df_long$df_long |>
          dplyr::select(
            dplyr::all_of(c("Panel", "PlateID"))
          ) |>
          dplyr::distinct() |>
          dplyr::left_join(
            df_rand$list_df_long$df_plate_long |>
              dplyr::select(
                dplyr::all_of(c("PlateID", "Panel"))
              ) |>
              dplyr::distinct(),
            by = "Panel",
            relationship = "many-to-many"
          )

        # modify df with wrong V1
        df_rand$list_df_wide$df_bottom_wide <-
          df_rand$list_df_wide$df_bottom_wide |>
          dplyr::filter(
            !(.data[["V1"]] == "Assay warning"
              & .data[["V47"]] == "Plate1_Panel1")
          )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # check that function runs with error
        expect_error(
          object = read_npx_wide_bottom(
            df = df_rand$list_df_wide$df_bottom_wide,
            file = olink_wide_format,
            olink_platform = olink_platform,
            data_type = data_type,
            col_names = col_names,
            format_spec = get_format_spec(data_type = data_type),
            df_plate_panel = df_plate_panel
          ),
          regexp = "Column 1 of the bottom matrix does not contain the same"
        )

      }
    )

  }
)

# Test read_npx_wide ----

test_that(
  "read_npx_wide - works - T48 - single panel - single plate",
  {
    # variables that apply to all tests
    olink_platform <- "Target 48"
    n_panels <- 1L
    n_assays <- 45L
    n_samples <- 88L

    ## NPX no int ctrl, no dev int ctrl, v1 ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"
        show_int_ctrl <- FALSE
        show_dev_int_ctrl <- FALSE
        version <- 1L

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = show_int_ctrl,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # expected dim of output df
        n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                          n_assays = n_assays,
                                          n_samples = n_samples,
                                          has_int_ctrl = show_int_ctrl,
                                          num_int_ctrl = 3L)

        # check that function runs
        expect_no_error(
          expect_no_warning(
            object = df_out <- read_npx_wide(
              df = df_rand$list_df_wide$df_wide,
              file = olink_wide_format,
              data_type = data_type,
              olink_platform = olink_platform
            )
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out) |> sort(),
          expected = colnames(df_rand$list_df_long$df_long) |> sort()
        )

        expect_identical(
          object = df_out,
          expected = df_rand$list_df_long$df_long |>
            dplyr::select(
              dplyr::all_of(colnames(df_out))
            )
        )

        # check that output match
        expect_true(
          object = identical(nrow(df_out), n_row_exp)
        )

      }
    )

    ## NPX no int ctrl, no dev int ctrl, v2 ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"
        show_int_ctrl <- FALSE
        show_dev_int_ctrl <- FALSE
        version <- 2L

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = show_int_ctrl,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # expected dim of output df
        n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                          n_assays = n_assays,
                                          n_samples = n_samples,
                                          has_int_ctrl = show_int_ctrl,
                                          num_int_ctrl = 3L)

        # check that function runs
        expect_no_error(
          expect_no_warning(
            object = df_out <- read_npx_wide(
              df = df_rand$list_df_wide$df_wide,
              file = olink_wide_format,
              data_type = data_type,
              olink_platform = olink_platform
            )
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out) |> sort(),
          expected = colnames(df_rand$list_df_long$df_long) |> sort()
        )

        expect_identical(
          object = df_out,
          expected = df_rand$list_df_long$df_long |>
            dplyr::select(
              dplyr::all_of(colnames(df_out))
            )
        )

        # check that output match
        expect_true(
          object = identical(nrow(df_out), n_row_exp)
        )

      }
    )

    ## NPX no int ctrl, with dev int ctrl, v1 ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"
        show_int_ctrl <- FALSE
        show_dev_int_ctrl <- TRUE
        version <- 1L

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = show_int_ctrl,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # expected dim of output df
        n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                          n_assays = n_assays,
                                          n_samples = n_samples,
                                          has_int_ctrl = show_int_ctrl,
                                          num_int_ctrl = 3L)

        # check that function runs
        expect_no_error(
          expect_no_warning(
            object = df_out <- read_npx_wide(
              df = df_rand$list_df_wide$df_wide,
              file = olink_wide_format,
              data_type = data_type,
              olink_platform = olink_platform
            )
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out) |> sort(),
          expected = colnames(df_rand$list_df_long$df_long) |> sort()
        )

        expect_identical(
          object = df_out,
          expected = df_rand$list_df_long$df_long |>
            dplyr::select(
              dplyr::all_of(colnames(df_out))
            )
        )

        # check that output match
        expect_true(
          object = identical(nrow(df_out), n_row_exp)
        )

      }
    )

    ## NPX no int ctrl, with dev int ctrl, v2 ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"
        show_int_ctrl <- FALSE
        show_dev_int_ctrl <- TRUE
        version <- 2L

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = show_int_ctrl,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # expected dim of output df
        n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                          n_assays = n_assays,
                                          n_samples = n_samples,
                                          has_int_ctrl = show_int_ctrl,
                                          num_int_ctrl = 3L)

        # check that function runs
        expect_no_error(
          expect_no_warning(
            object = df_out <- read_npx_wide(
              df = df_rand$list_df_wide$df_wide,
              file = olink_wide_format,
              data_type = data_type,
              olink_platform = olink_platform
            )
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out) |> sort(),
          expected = colnames(df_rand$list_df_long$df_long) |> sort()
        )

        expect_identical(
          object = df_out,
          expected = df_rand$list_df_long$df_long |>
            dplyr::select(
              dplyr::all_of(colnames(df_out))
            )
        )

        # check that output match
        expect_true(
          object = identical(nrow(df_out), n_row_exp)
        )

      }
    )

    ## NPX with int ctrl, no dev int ctrl, v1 ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"
        show_int_ctrl <- TRUE
        show_dev_int_ctrl <- FALSE
        version <- 1L

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = show_int_ctrl,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # expected dim of output df
        n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                          n_assays = n_assays,
                                          n_samples = n_samples,
                                          has_int_ctrl = show_int_ctrl,
                                          num_int_ctrl = 3L)

        # check that function runs
        expect_no_error(
          expect_no_warning(
            object = df_out <- read_npx_wide(
              df = df_rand$list_df_wide$df_wide,
              file = olink_wide_format,
              data_type = data_type,
              olink_platform = olink_platform
            )
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out) |> sort(),
          expected = colnames(df_rand$list_df_long$df_long) |> sort()
        )

        expect_identical(
          object = df_out,
          expected = df_rand$list_df_long$df_long |>
            dplyr::select(
              dplyr::all_of(colnames(df_out))
            )
        )

        # check that output match
        expect_true(
          object = identical(nrow(df_out), n_row_exp)
        )

      }
    )

    ## NPX with int ctrl, no dev int ctrl, v2 ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"
        show_int_ctrl <- TRUE
        show_dev_int_ctrl <- FALSE
        version <- 2L

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = show_int_ctrl,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # expected dim of output df
        n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                          n_assays = n_assays,
                                          n_samples = n_samples,
                                          has_int_ctrl = show_int_ctrl,
                                          num_int_ctrl = 3L)

        # check that function runs
        expect_no_error(
          expect_no_warning(
            object = df_out <- read_npx_wide(
              df = df_rand$list_df_wide$df_wide,
              file = olink_wide_format,
              data_type = data_type,
              olink_platform = olink_platform
            )
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out) |> sort(),
          expected = colnames(df_rand$list_df_long$df_long) |> sort()
        )

        expect_identical(
          object = df_out,
          expected = df_rand$list_df_long$df_long |>
            dplyr::select(
              dplyr::all_of(colnames(df_out))
            )
        )

        # check that output match
        expect_true(
          object = identical(nrow(df_out), n_row_exp)
        )

      }
    )

    ## NPX with int ctrl, with dev int ctrl, v1 ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"
        show_int_ctrl <- TRUE
        show_dev_int_ctrl <- TRUE
        version <- 1L

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = show_int_ctrl,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # expected dim of output df
        n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                          n_assays = n_assays,
                                          n_samples = n_samples,
                                          has_int_ctrl = show_int_ctrl,
                                          num_int_ctrl = 3L)

        # check that function runs
        expect_no_error(
          expect_no_warning(
            object = df_out <- read_npx_wide(
              df = df_rand$list_df_wide$df_wide,
              file = olink_wide_format,
              data_type = data_type,
              olink_platform = olink_platform
            )
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out) |> sort(),
          expected = colnames(df_rand$list_df_long$df_long) |> sort()
        )

        expect_identical(
          object = df_out,
          expected = df_rand$list_df_long$df_long |>
            dplyr::select(
              dplyr::all_of(colnames(df_out))
            )
        )

        # check that output match
        expect_true(
          object = identical(nrow(df_out), n_row_exp)
        )

      }
    )

    ## NPX with int ctrl, with dev int ctrl, v2 ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"
        show_int_ctrl <- TRUE
        show_dev_int_ctrl <- TRUE
        version <- 2L

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = show_int_ctrl,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # expected dim of output df
        n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                          n_assays = n_assays,
                                          n_samples = n_samples,
                                          has_int_ctrl = show_int_ctrl,
                                          num_int_ctrl = 3L)

        # check that function runs
        expect_no_error(
          expect_no_warning(
            object = df_out <- read_npx_wide(
              df = df_rand$list_df_wide$df_wide,
              file = olink_wide_format,
              data_type = data_type,
              olink_platform = olink_platform
            )
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out) |> sort(),
          expected = colnames(df_rand$list_df_long$df_long) |> sort()
        )

        expect_identical(
          object = df_out,
          expected = df_rand$list_df_long$df_long |>
            dplyr::select(
              dplyr::all_of(colnames(df_out))
            )
        )

        # check that output match
        expect_true(
          object = identical(nrow(df_out), n_row_exp)
        )

      }
    )

    ## Ct no int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Ct"
        show_int_ctrl <- FALSE
        show_dev_int_ctrl <- FALSE
        version <- 0L

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = show_int_ctrl,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # expected dim of output df
        n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                          n_assays = n_assays,
                                          n_samples = n_samples,
                                          has_int_ctrl = show_int_ctrl,
                                          num_int_ctrl = 3L)

        # check that function runs
        expect_no_error(
          expect_no_warning(
            object = df_out <- read_npx_wide(
              df = df_rand$list_df_wide$df_wide,
              file = olink_wide_format,
              data_type = data_type,
              olink_platform = olink_platform
            )
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out) |> sort(),
          expected = colnames(df_rand$list_df_long$df_long) |> sort()
        )

        expect_identical(
          object = df_out,
          expected = df_rand$list_df_long$df_long |>
            dplyr::select(
              dplyr::all_of(colnames(df_out))
            )
        )

        # check that output match
        expect_true(
          object = identical(nrow(df_out), n_row_exp)
        )

      }
    )

    ## Ct with int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Ct"
        show_int_ctrl <- TRUE
        show_dev_int_ctrl <- FALSE
        version <- 0L

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = show_int_ctrl,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # expected dim of output df
        n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                          n_assays = n_assays,
                                          n_samples = n_samples,
                                          has_int_ctrl = show_int_ctrl,
                                          num_int_ctrl = 3L)

        # check that function runs
        expect_no_error(
          expect_no_warning(
            object = df_out <- read_npx_wide(
              df = df_rand$list_df_wide$df_wide,
              file = olink_wide_format,
              data_type = data_type,
              olink_platform = olink_platform
            )
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out) |> sort(),
          expected = colnames(df_rand$list_df_long$df_long) |> sort()
        )

        expect_identical(
          object = df_out,
          expected = df_rand$list_df_long$df_long |>
            dplyr::select(
              dplyr::all_of(colnames(df_out))
            )
        )

        # check that output match
        expect_true(
          object = identical(nrow(df_out), n_row_exp)
        )

      }
    )

    ## Quantified no int ctrl, no dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Quantified"
        show_int_ctrl <- FALSE
        show_dev_int_ctrl <- FALSE
        version <- 0L

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = show_int_ctrl,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # expected dim of output df
        n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                          n_assays = n_assays,
                                          n_samples = n_samples,
                                          has_int_ctrl = show_int_ctrl,
                                          num_int_ctrl = 3L)

        # check that function runs
        expect_no_error(
          expect_no_warning(
            object = df_out <- read_npx_wide(
              df = df_rand$list_df_wide$df_wide,
              file = olink_wide_format,
              data_type = data_type,
              olink_platform = olink_platform
            )
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out) |> sort(),
          expected = colnames(df_rand$list_df_long$df_long) |> sort()
        )

        expect_identical(
          object = df_out,
          expected = df_rand$list_df_long$df_long |>
            dplyr::select(
              dplyr::all_of(colnames(df_out))
            )
        )

        # check that output match
        expect_true(
          object = identical(nrow(df_out), n_row_exp)
        )

      }
    )

    ## Quantified no int ctrl, with dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Quantified"
        show_int_ctrl <- FALSE
        show_dev_int_ctrl <- TRUE
        version <- 0L

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = show_int_ctrl,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # expected dim of output df
        n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                          n_assays = n_assays,
                                          n_samples = n_samples,
                                          has_int_ctrl = show_int_ctrl,
                                          num_int_ctrl = 3L)

        # check that function runs
        expect_no_error(
          expect_no_warning(
            object = df_out <- read_npx_wide(
              df = df_rand$list_df_wide$df_wide,
              file = olink_wide_format,
              data_type = data_type,
              olink_platform = olink_platform
            )
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out) |> sort(),
          expected = colnames(df_rand$list_df_long$df_long) |> sort()
        )

        expect_identical(
          object = df_out,
          expected = df_rand$list_df_long$df_long |>
            dplyr::select(
              dplyr::all_of(colnames(df_out))
            )
        )

        # check that output match
        expect_true(
          object = identical(nrow(df_out), n_row_exp)
        )

      }
    )

    ## Quantified with int ctrl, no dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Quantified"
        show_int_ctrl <- TRUE
        show_dev_int_ctrl <- FALSE
        version <- 0L

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = show_int_ctrl,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # expected dim of output df
        n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                          n_assays = n_assays,
                                          n_samples = n_samples,
                                          has_int_ctrl = show_int_ctrl,
                                          num_int_ctrl = 3L)

        # check that function runs
        expect_no_error(
          expect_no_warning(
            object = df_out <- read_npx_wide(
              df = df_rand$list_df_wide$df_wide,
              file = olink_wide_format,
              data_type = data_type,
              olink_platform = olink_platform
            )
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out) |> sort(),
          expected = colnames(df_rand$list_df_long$df_long) |> sort()
        )

        expect_identical(
          object = df_out,
          expected = df_rand$list_df_long$df_long |>
            dplyr::select(
              dplyr::all_of(colnames(df_out))
            )
        )

        # check that output match
        expect_true(
          object = identical(nrow(df_out), n_row_exp)
        )

      }
    )

    ## Quantified with int ctrl, with dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Quantified"
        show_int_ctrl <- TRUE
        show_dev_int_ctrl <- TRUE
        version <- 0L

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = show_int_ctrl,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # expected dim of output df
        n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                          n_assays = n_assays,
                                          n_samples = n_samples,
                                          has_int_ctrl = show_int_ctrl,
                                          num_int_ctrl = 3L)

        # check that function runs
        expect_no_error(
          expect_no_warning(
            object = df_out <- read_npx_wide(
              df = df_rand$list_df_wide$df_wide,
              file = olink_wide_format,
              data_type = data_type,
              olink_platform = olink_platform
            )
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out) |> sort(),
          expected = colnames(df_rand$list_df_long$df_long) |> sort()
        )

        expect_identical(
          object = df_out,
          expected = df_rand$list_df_long$df_long |>
            dplyr::select(
              dplyr::all_of(colnames(df_out))
            )
        )

        # check that output match
        expect_true(
          object = identical(nrow(df_out), n_row_exp)
        )

      }
    )

  }
)

test_that(
  "read_npx_wide - works - T48 - multiple panels - single plate",
  {
    # variables that apply to all tests
    olink_platform <- "Target 48"
    n_panels <- 3L
    n_assays <- 45L
    n_samples <- 88L

    ## NPX no int ctrl, no dev int ctrl, v1 ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"
        show_int_ctrl <- FALSE
        show_dev_int_ctrl <- FALSE
        version <- 1L

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = show_int_ctrl,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # expected dim of output df
        n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                          n_assays = n_assays,
                                          n_samples = n_samples,
                                          has_int_ctrl = show_int_ctrl,
                                          num_int_ctrl = 3L)

        # check that function runs
        expect_no_error(
          expect_no_warning(
            object = df_out <- read_npx_wide(
              df = df_rand$list_df_wide$df_wide,
              file = olink_wide_format,
              data_type = data_type,
              olink_platform = olink_platform
            )
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out) |> sort(),
          expected = colnames(df_rand$list_df_long$df_long) |> sort()
        )

        expect_identical(
          object = df_out,
          expected = df_rand$list_df_long$df_long |>
            dplyr::select(
              dplyr::all_of(colnames(df_out))
            )
        )

        # check that output match
        expect_true(
          object = identical(nrow(df_out), n_row_exp)
        )

      }
    )

    ## NPX no int ctrl, no dev int ctrl, v2 ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"
        show_int_ctrl <- FALSE
        show_dev_int_ctrl <- FALSE
        version <- 2L

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = show_int_ctrl,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # expected dim of output df
        n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                          n_assays = n_assays,
                                          n_samples = n_samples,
                                          has_int_ctrl = show_int_ctrl,
                                          num_int_ctrl = 3L)

        # check that function runs
        expect_no_error(
          expect_no_warning(
            object = df_out <- read_npx_wide(
              df = df_rand$list_df_wide$df_wide,
              file = olink_wide_format,
              data_type = data_type,
              olink_platform = olink_platform
            )
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out) |> sort(),
          expected = colnames(df_rand$list_df_long$df_long) |> sort()
        )

        expect_identical(
          object = df_out,
          expected = df_rand$list_df_long$df_long |>
            dplyr::select(
              dplyr::all_of(colnames(df_out))
            )
        )

        # check that output match
        expect_true(
          object = identical(nrow(df_out), n_row_exp)
        )

      }
    )

    ## NPX no int ctrl, with dev int ctrl, v1 ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"
        show_int_ctrl <- FALSE
        show_dev_int_ctrl <- TRUE
        version <- 1L

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = show_int_ctrl,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # expected dim of output df
        n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                          n_assays = n_assays,
                                          n_samples = n_samples,
                                          has_int_ctrl = show_int_ctrl,
                                          num_int_ctrl = 3L)

        # check that function runs
        expect_no_error(
          expect_no_warning(
            object = df_out <- read_npx_wide(
              df = df_rand$list_df_wide$df_wide,
              file = olink_wide_format,
              data_type = data_type,
              olink_platform = olink_platform
            )
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out) |> sort(),
          expected = colnames(df_rand$list_df_long$df_long) |> sort()
        )

        expect_identical(
          object = df_out,
          expected = df_rand$list_df_long$df_long |>
            dplyr::select(
              dplyr::all_of(colnames(df_out))
            )
        )

        # check that output match
        expect_true(
          object = identical(nrow(df_out), n_row_exp)
        )

      }
    )

    ## NPX no int ctrl, with dev int ctrl, v2 ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"
        show_int_ctrl <- FALSE
        show_dev_int_ctrl <- TRUE
        version <- 2L

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = show_int_ctrl,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # expected dim of output df
        n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                          n_assays = n_assays,
                                          n_samples = n_samples,
                                          has_int_ctrl = show_int_ctrl,
                                          num_int_ctrl = 3L)

        # check that function runs
        expect_no_error(
          expect_no_warning(
            object = df_out <- read_npx_wide(
              df = df_rand$list_df_wide$df_wide,
              file = olink_wide_format,
              data_type = data_type,
              olink_platform = olink_platform
            )
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out) |> sort(),
          expected = colnames(df_rand$list_df_long$df_long) |> sort()
        )

        expect_identical(
          object = df_out,
          expected = df_rand$list_df_long$df_long |>
            dplyr::select(
              dplyr::all_of(colnames(df_out))
            )
        )

        # check that output match
        expect_true(
          object = identical(nrow(df_out), n_row_exp)
        )

      }
    )

    ## NPX with int ctrl, no dev int ctrl, v1 ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"
        show_int_ctrl <- TRUE
        show_dev_int_ctrl <- FALSE
        version <- 1L

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = show_int_ctrl,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # expected dim of output df
        n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                          n_assays = n_assays,
                                          n_samples = n_samples,
                                          has_int_ctrl = show_int_ctrl,
                                          num_int_ctrl = 3L)

        # check that function runs
        expect_no_error(
          expect_no_warning(
            object = df_out <- read_npx_wide(
              df = df_rand$list_df_wide$df_wide,
              file = olink_wide_format,
              data_type = data_type,
              olink_platform = olink_platform
            )
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out) |> sort(),
          expected = colnames(df_rand$list_df_long$df_long) |> sort()
        )

        expect_identical(
          object = df_out,
          expected = df_rand$list_df_long$df_long |>
            dplyr::select(
              dplyr::all_of(colnames(df_out))
            )
        )

        # check that output match
        expect_true(
          object = identical(nrow(df_out), n_row_exp)
        )

      }
    )

    ## NPX with int ctrl, no dev int ctrl, v2 ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"
        show_int_ctrl <- TRUE
        show_dev_int_ctrl <- FALSE
        version <- 2L

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = show_int_ctrl,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # expected dim of output df
        n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                          n_assays = n_assays,
                                          n_samples = n_samples,
                                          has_int_ctrl = show_int_ctrl,
                                          num_int_ctrl = 3L)

        # check that function runs
        expect_no_error(
          expect_no_warning(
            object = df_out <- read_npx_wide(
              df = df_rand$list_df_wide$df_wide,
              file = olink_wide_format,
              data_type = data_type,
              olink_platform = olink_platform
            )
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out) |> sort(),
          expected = colnames(df_rand$list_df_long$df_long) |> sort()
        )

        expect_identical(
          object = df_out,
          expected = df_rand$list_df_long$df_long |>
            dplyr::select(
              dplyr::all_of(colnames(df_out))
            )
        )

        # check that output match
        expect_true(
          object = identical(nrow(df_out), n_row_exp)
        )

      }
    )

    ## NPX with int ctrl, with dev int ctrl, v1 ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"
        show_int_ctrl <- TRUE
        show_dev_int_ctrl <- TRUE
        version <- 1L

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = show_int_ctrl,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # expected dim of output df
        n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                          n_assays = n_assays,
                                          n_samples = n_samples,
                                          has_int_ctrl = show_int_ctrl,
                                          num_int_ctrl = 3L)

        # check that function runs
        expect_no_error(
          expect_no_warning(
            object = df_out <- read_npx_wide(
              df = df_rand$list_df_wide$df_wide,
              file = olink_wide_format,
              data_type = data_type,
              olink_platform = olink_platform
            )
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out) |> sort(),
          expected = colnames(df_rand$list_df_long$df_long) |> sort()
        )

        expect_identical(
          object = df_out,
          expected = df_rand$list_df_long$df_long |>
            dplyr::select(
              dplyr::all_of(colnames(df_out))
            )
        )

        # check that output match
        expect_true(
          object = identical(nrow(df_out), n_row_exp)
        )

      }
    )

    ## NPX with int ctrl, with dev int ctrl, v2 ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"
        show_int_ctrl <- TRUE
        show_dev_int_ctrl <- TRUE
        version <- 2L

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = show_int_ctrl,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # expected dim of output df
        n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                          n_assays = n_assays,
                                          n_samples = n_samples,
                                          has_int_ctrl = show_int_ctrl,
                                          num_int_ctrl = 3L)

        # check that function runs
        expect_no_error(
          expect_no_warning(
            object = df_out <- read_npx_wide(
              df = df_rand$list_df_wide$df_wide,
              file = olink_wide_format,
              data_type = data_type,
              olink_platform = olink_platform
            )
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out) |> sort(),
          expected = colnames(df_rand$list_df_long$df_long) |> sort()
        )

        expect_identical(
          object = df_out,
          expected = df_rand$list_df_long$df_long |>
            dplyr::select(
              dplyr::all_of(colnames(df_out))
            )
        )

        # check that output match
        expect_true(
          object = identical(nrow(df_out), n_row_exp)
        )

      }
    )

    ## Ct no int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Ct"
        show_int_ctrl <- FALSE
        show_dev_int_ctrl <- FALSE
        version <- 0L

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = show_int_ctrl,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # expected dim of output df
        n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                          n_assays = n_assays,
                                          n_samples = n_samples,
                                          has_int_ctrl = show_int_ctrl,
                                          num_int_ctrl = 3L)

        # check that function runs
        expect_no_error(
          expect_no_warning(
            object = df_out <- read_npx_wide(
              df = df_rand$list_df_wide$df_wide,
              file = olink_wide_format,
              data_type = data_type,
              olink_platform = olink_platform
            )
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out) |> sort(),
          expected = colnames(df_rand$list_df_long$df_long) |> sort()
        )

        expect_identical(
          object = df_out,
          expected = df_rand$list_df_long$df_long |>
            dplyr::select(
              dplyr::all_of(colnames(df_out))
            )
        )

        # check that output match
        expect_true(
          object = identical(nrow(df_out), n_row_exp)
        )

      }
    )

    ## Ct with int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Ct"
        show_int_ctrl <- TRUE
        show_dev_int_ctrl <- FALSE
        version <- 0L

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = show_int_ctrl,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # expected dim of output df
        n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                          n_assays = n_assays,
                                          n_samples = n_samples,
                                          has_int_ctrl = show_int_ctrl,
                                          num_int_ctrl = 3L)

        # check that function runs
        expect_no_error(
          expect_no_warning(
            object = df_out <- read_npx_wide(
              df = df_rand$list_df_wide$df_wide,
              file = olink_wide_format,
              data_type = data_type,
              olink_platform = olink_platform
            )
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out) |> sort(),
          expected = colnames(df_rand$list_df_long$df_long) |> sort()
        )

        expect_identical(
          object = df_out,
          expected = df_rand$list_df_long$df_long |>
            dplyr::select(
              dplyr::all_of(colnames(df_out))
            )
        )

        # check that output match
        expect_true(
          object = identical(nrow(df_out), n_row_exp)
        )

      }
    )

    ## Quantified no int ctrl, no dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Quantified"
        show_int_ctrl <- FALSE
        show_dev_int_ctrl <- FALSE
        version <- 0L

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = show_int_ctrl,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # expected dim of output df
        n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                          n_assays = n_assays,
                                          n_samples = n_samples,
                                          has_int_ctrl = show_int_ctrl,
                                          num_int_ctrl = 3L)

        # check that function runs
        expect_no_error(
          expect_no_warning(
            object = df_out <- read_npx_wide(
              df = df_rand$list_df_wide$df_wide,
              file = olink_wide_format,
              data_type = data_type,
              olink_platform = olink_platform
            )
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out) |> sort(),
          expected = colnames(df_rand$list_df_long$df_long) |> sort()
        )

        expect_identical(
          object = df_out,
          expected = df_rand$list_df_long$df_long |>
            dplyr::select(
              dplyr::all_of(colnames(df_out))
            )
        )

        # check that output match
        expect_true(
          object = identical(nrow(df_out), n_row_exp)
        )

      }
    )

    ## Quantified no int ctrl, with dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Quantified"
        show_int_ctrl <- FALSE
        show_dev_int_ctrl <- TRUE
        version <- 0L

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = show_int_ctrl,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # expected dim of output df
        n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                          n_assays = n_assays,
                                          n_samples = n_samples,
                                          has_int_ctrl = show_int_ctrl,
                                          num_int_ctrl = 3L)

        # check that function runs
        expect_no_error(
          expect_no_warning(
            object = df_out <- read_npx_wide(
              df = df_rand$list_df_wide$df_wide,
              file = olink_wide_format,
              data_type = data_type,
              olink_platform = olink_platform
            )
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out) |> sort(),
          expected = colnames(df_rand$list_df_long$df_long) |> sort()
        )

        expect_identical(
          object = df_out,
          expected = df_rand$list_df_long$df_long |>
            dplyr::select(
              dplyr::all_of(colnames(df_out))
            )
        )

        # check that output match
        expect_true(
          object = identical(nrow(df_out), n_row_exp)
        )

      }
    )

    ## Quantified with int ctrl, no dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Quantified"
        show_int_ctrl <- TRUE
        show_dev_int_ctrl <- FALSE
        version <- 0L

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = show_int_ctrl,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # expected dim of output df
        n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                          n_assays = n_assays,
                                          n_samples = n_samples,
                                          has_int_ctrl = show_int_ctrl,
                                          num_int_ctrl = 3L)

        # check that function runs
        expect_no_error(
          expect_no_warning(
            object = df_out <- read_npx_wide(
              df = df_rand$list_df_wide$df_wide,
              file = olink_wide_format,
              data_type = data_type,
              olink_platform = olink_platform
            )
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out) |> sort(),
          expected = colnames(df_rand$list_df_long$df_long) |> sort()
        )

        expect_identical(
          object = df_out,
          expected = df_rand$list_df_long$df_long |>
            dplyr::select(
              dplyr::all_of(colnames(df_out))
            )
        )

        # check that output match
        expect_true(
          object = identical(nrow(df_out), n_row_exp)
        )

      }
    )

    ## Quantified with int ctrl, with dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Quantified"
        show_int_ctrl <- TRUE
        show_dev_int_ctrl <- TRUE
        version <- 0L

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = show_int_ctrl,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # expected dim of output df
        n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                          n_assays = n_assays,
                                          n_samples = n_samples,
                                          has_int_ctrl = show_int_ctrl,
                                          num_int_ctrl = 3L)

        # check that function runs
        expect_no_error(
          expect_no_warning(
            object = df_out <- read_npx_wide(
              df = df_rand$list_df_wide$df_wide,
              file = olink_wide_format,
              data_type = data_type,
              olink_platform = olink_platform
            )
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out) |> sort(),
          expected = colnames(df_rand$list_df_long$df_long) |> sort()
        )

        expect_identical(
          object = df_out,
          expected = df_rand$list_df_long$df_long |>
            dplyr::select(
              dplyr::all_of(colnames(df_out))
            )
        )

        # check that output match
        expect_true(
          object = identical(nrow(df_out), n_row_exp)
        )

      }
    )

  }
)

test_that(
  "read_npx_wide - works - T48 - single panel - multiple plates",
  {
    # variables that apply to all tests
    olink_platform <- "Target 48"
    n_panels <- 1L
    n_assays <- 45L
    n_samples <- 99L

    ## NPX no int ctrl, no dev int ctrl, v1 ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"
        show_int_ctrl <- FALSE
        show_dev_int_ctrl <- FALSE
        version <- 1L

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = show_int_ctrl,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # expected dim of output df
        n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                          n_assays = n_assays,
                                          n_samples = n_samples,
                                          has_int_ctrl = show_int_ctrl,
                                          num_int_ctrl = 3L)

        # check that function runs
        expect_no_error(
          expect_no_warning(
            object = df_out <- read_npx_wide(
              df = df_rand$list_df_wide$df_wide,
              file = olink_wide_format,
              data_type = data_type,
              olink_platform = olink_platform
            )
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out) |> sort(),
          expected = colnames(df_rand$list_df_long$df_long) |> sort()
        )

        expect_identical(
          object = df_out,
          expected = df_rand$list_df_long$df_long |>
            dplyr::select(
              dplyr::all_of(colnames(df_out))
            )
        )

        # check that output match
        expect_true(
          object = identical(nrow(df_out), n_row_exp)
        )

      }
    )

    ## NPX no int ctrl, no dev int ctrl, v2 ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"
        show_int_ctrl <- FALSE
        show_dev_int_ctrl <- FALSE
        version <- 2L

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = show_int_ctrl,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # expected dim of output df
        n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                          n_assays = n_assays,
                                          n_samples = n_samples,
                                          has_int_ctrl = show_int_ctrl,
                                          num_int_ctrl = 3L)

        # check that function runs
        expect_no_error(
          expect_no_warning(
            object = df_out <- read_npx_wide(
              df = df_rand$list_df_wide$df_wide,
              file = olink_wide_format,
              data_type = data_type,
              olink_platform = olink_platform
            )
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out) |> sort(),
          expected = colnames(df_rand$list_df_long$df_long) |> sort()
        )

        expect_identical(
          object = df_out,
          expected = df_rand$list_df_long$df_long |>
            dplyr::select(
              dplyr::all_of(colnames(df_out))
            )
        )

        # check that output match
        expect_true(
          object = identical(nrow(df_out), n_row_exp)
        )

      }
    )

    ## NPX no int ctrl, with dev int ctrl, v1 ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"
        show_int_ctrl <- FALSE
        show_dev_int_ctrl <- TRUE
        version <- 1L

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = show_int_ctrl,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # expected dim of output df
        n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                          n_assays = n_assays,
                                          n_samples = n_samples,
                                          has_int_ctrl = show_int_ctrl,
                                          num_int_ctrl = 3L)

        # check that function runs
        expect_no_error(
          expect_no_warning(
            object = df_out <- read_npx_wide(
              df = df_rand$list_df_wide$df_wide,
              file = olink_wide_format,
              data_type = data_type,
              olink_platform = olink_platform
            )
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out) |> sort(),
          expected = colnames(df_rand$list_df_long$df_long) |> sort()
        )

        expect_identical(
          object = df_out,
          expected = df_rand$list_df_long$df_long |>
            dplyr::select(
              dplyr::all_of(colnames(df_out))
            )
        )

        # check that output match
        expect_true(
          object = identical(nrow(df_out), n_row_exp)
        )

      }
    )

    ## NPX no int ctrl, with dev int ctrl, v2 ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"
        show_int_ctrl <- FALSE
        show_dev_int_ctrl <- TRUE
        version <- 2L

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = show_int_ctrl,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # expected dim of output df
        n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                          n_assays = n_assays,
                                          n_samples = n_samples,
                                          has_int_ctrl = show_int_ctrl,
                                          num_int_ctrl = 3L)

        # check that function runs
        expect_no_error(
          expect_no_warning(
            object = df_out <- read_npx_wide(
              df = df_rand$list_df_wide$df_wide,
              file = olink_wide_format,
              data_type = data_type,
              olink_platform = olink_platform
            )
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out) |> sort(),
          expected = colnames(df_rand$list_df_long$df_long) |> sort()
        )

        expect_identical(
          object = df_out,
          expected = df_rand$list_df_long$df_long |>
            dplyr::select(
              dplyr::all_of(colnames(df_out))
            )
        )

        # check that output match
        expect_true(
          object = identical(nrow(df_out), n_row_exp)
        )

      }
    )

    ## NPX with int ctrl, no dev int ctrl, v1 ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"
        show_int_ctrl <- TRUE
        show_dev_int_ctrl <- FALSE
        version <- 1L

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = show_int_ctrl,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # expected dim of output df
        n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                          n_assays = n_assays,
                                          n_samples = n_samples,
                                          has_int_ctrl = show_int_ctrl,
                                          num_int_ctrl = 3L)

        # check that function runs
        expect_no_error(
          expect_no_warning(
            object = df_out <- read_npx_wide(
              df = df_rand$list_df_wide$df_wide,
              file = olink_wide_format,
              data_type = data_type,
              olink_platform = olink_platform
            )
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out) |> sort(),
          expected = colnames(df_rand$list_df_long$df_long) |> sort()
        )

        expect_identical(
          object = df_out,
          expected = df_rand$list_df_long$df_long |>
            dplyr::select(
              dplyr::all_of(colnames(df_out))
            )
        )

        # check that output match
        expect_true(
          object = identical(nrow(df_out), n_row_exp)
        )

      }
    )

    ## NPX with int ctrl, no dev int ctrl, v2 ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"
        show_int_ctrl <- TRUE
        show_dev_int_ctrl <- FALSE
        version <- 2L

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = show_int_ctrl,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # expected dim of output df
        n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                          n_assays = n_assays,
                                          n_samples = n_samples,
                                          has_int_ctrl = show_int_ctrl,
                                          num_int_ctrl = 3L)

        # check that function runs
        expect_no_error(
          expect_no_warning(
            object = df_out <- read_npx_wide(
              df = df_rand$list_df_wide$df_wide,
              file = olink_wide_format,
              data_type = data_type,
              olink_platform = olink_platform
            )
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out) |> sort(),
          expected = colnames(df_rand$list_df_long$df_long) |> sort()
        )

        expect_identical(
          object = df_out,
          expected = df_rand$list_df_long$df_long |>
            dplyr::select(
              dplyr::all_of(colnames(df_out))
            )
        )

        # check that output match
        expect_true(
          object = identical(nrow(df_out), n_row_exp)
        )

      }
    )

    ## NPX with int ctrl, with dev int ctrl, v1 ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"
        show_int_ctrl <- TRUE
        show_dev_int_ctrl <- TRUE
        version <- 1L

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = show_int_ctrl,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # expected dim of output df
        n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                          n_assays = n_assays,
                                          n_samples = n_samples,
                                          has_int_ctrl = show_int_ctrl,
                                          num_int_ctrl = 3L)

        # check that function runs
        expect_no_error(
          expect_no_warning(
            object = df_out <- read_npx_wide(
              df = df_rand$list_df_wide$df_wide,
              file = olink_wide_format,
              data_type = data_type,
              olink_platform = olink_platform
            )
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out) |> sort(),
          expected = colnames(df_rand$list_df_long$df_long) |> sort()
        )

        expect_identical(
          object = df_out,
          expected = df_rand$list_df_long$df_long |>
            dplyr::select(
              dplyr::all_of(colnames(df_out))
            )
        )

        # check that output match
        expect_true(
          object = identical(nrow(df_out), n_row_exp)
        )

      }
    )

    ## NPX with int ctrl, with dev int ctrl, v2 ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"
        show_int_ctrl <- TRUE
        show_dev_int_ctrl <- TRUE
        version <- 2L

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = show_int_ctrl,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # expected dim of output df
        n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                          n_assays = n_assays,
                                          n_samples = n_samples,
                                          has_int_ctrl = show_int_ctrl,
                                          num_int_ctrl = 3L)

        # check that function runs
        expect_no_error(
          expect_no_warning(
            object = df_out <- read_npx_wide(
              df = df_rand$list_df_wide$df_wide,
              file = olink_wide_format,
              data_type = data_type,
              olink_platform = olink_platform
            )
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out) |> sort(),
          expected = colnames(df_rand$list_df_long$df_long) |> sort()
        )

        expect_identical(
          object = df_out,
          expected = df_rand$list_df_long$df_long |>
            dplyr::select(
              dplyr::all_of(colnames(df_out))
            )
        )

        # check that output match
        expect_true(
          object = identical(nrow(df_out), n_row_exp)
        )

      }
    )

    ## Ct no int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Ct"
        show_int_ctrl <- FALSE
        show_dev_int_ctrl <- FALSE
        version <- 0L

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = show_int_ctrl,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # expected dim of output df
        n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                          n_assays = n_assays,
                                          n_samples = n_samples,
                                          has_int_ctrl = show_int_ctrl,
                                          num_int_ctrl = 3L)

        # check that function runs
        expect_no_error(
          expect_no_warning(
            object = df_out <- read_npx_wide(
              df = df_rand$list_df_wide$df_wide,
              file = olink_wide_format,
              data_type = data_type,
              olink_platform = olink_platform
            )
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out) |> sort(),
          expected = colnames(df_rand$list_df_long$df_long) |> sort()
        )

        expect_identical(
          object = df_out,
          expected = df_rand$list_df_long$df_long |>
            dplyr::select(
              dplyr::all_of(colnames(df_out))
            )
        )

        # check that output match
        expect_true(
          object = identical(nrow(df_out), n_row_exp)
        )

      }
    )

    ## Ct with int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Ct"
        show_int_ctrl <- TRUE
        show_dev_int_ctrl <- FALSE
        version <- 0L

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = show_int_ctrl,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # expected dim of output df
        n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                          n_assays = n_assays,
                                          n_samples = n_samples,
                                          has_int_ctrl = show_int_ctrl,
                                          num_int_ctrl = 3L)

        # check that function runs
        expect_no_error(
          expect_no_warning(
            object = df_out <- read_npx_wide(
              df = df_rand$list_df_wide$df_wide,
              file = olink_wide_format,
              data_type = data_type,
              olink_platform = olink_platform
            )
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out) |> sort(),
          expected = colnames(df_rand$list_df_long$df_long) |> sort()
        )

        expect_identical(
          object = df_out,
          expected = df_rand$list_df_long$df_long |>
            dplyr::select(
              dplyr::all_of(colnames(df_out))
            )
        )

        # check that output match
        expect_true(
          object = identical(nrow(df_out), n_row_exp)
        )

      }
    )

    ## Quantified no int ctrl, no dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Quantified"
        show_int_ctrl <- FALSE
        show_dev_int_ctrl <- FALSE
        version <- 0L

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = show_int_ctrl,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # expected dim of output df
        n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                          n_assays = n_assays,
                                          n_samples = n_samples,
                                          has_int_ctrl = show_int_ctrl,
                                          num_int_ctrl = 3L)

        # check that function runs
        expect_no_error(
          expect_no_warning(
            object = df_out <- read_npx_wide(
              df = df_rand$list_df_wide$df_wide,
              file = olink_wide_format,
              data_type = data_type,
              olink_platform = olink_platform
            )
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out) |> sort(),
          expected = colnames(df_rand$list_df_long$df_long) |> sort()
        )

        expect_identical(
          object = df_out,
          expected = df_rand$list_df_long$df_long |>
            dplyr::select(
              dplyr::all_of(colnames(df_out))
            )
        )

        # check that output match
        expect_true(
          object = identical(nrow(df_out), n_row_exp)
        )

      }
    )

    ## Quantified no int ctrl, with dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Quantified"
        show_int_ctrl <- FALSE
        show_dev_int_ctrl <- TRUE
        version <- 0L

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = show_int_ctrl,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # expected dim of output df
        n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                          n_assays = n_assays,
                                          n_samples = n_samples,
                                          has_int_ctrl = show_int_ctrl,
                                          num_int_ctrl = 3L)

        # check that function runs
        expect_no_error(
          expect_no_warning(
            object = df_out <- read_npx_wide(
              df = df_rand$list_df_wide$df_wide,
              file = olink_wide_format,
              data_type = data_type,
              olink_platform = olink_platform
            )
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out) |> sort(),
          expected = colnames(df_rand$list_df_long$df_long) |> sort()
        )

        expect_identical(
          object = df_out,
          expected = df_rand$list_df_long$df_long |>
            dplyr::select(
              dplyr::all_of(colnames(df_out))
            )
        )

        # check that output match
        expect_true(
          object = identical(nrow(df_out), n_row_exp)
        )

      }
    )

    ## Quantified with int ctrl, no dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Quantified"
        show_int_ctrl <- TRUE
        show_dev_int_ctrl <- FALSE
        version <- 0L

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = show_int_ctrl,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # expected dim of output df
        n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                          n_assays = n_assays,
                                          n_samples = n_samples,
                                          has_int_ctrl = show_int_ctrl,
                                          num_int_ctrl = 3L)

        # check that function runs
        expect_no_error(
          expect_no_warning(
            object = df_out <- read_npx_wide(
              df = df_rand$list_df_wide$df_wide,
              file = olink_wide_format,
              data_type = data_type,
              olink_platform = olink_platform
            )
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out) |> sort(),
          expected = colnames(df_rand$list_df_long$df_long) |> sort()
        )

        expect_identical(
          object = df_out,
          expected = df_rand$list_df_long$df_long |>
            dplyr::select(
              dplyr::all_of(colnames(df_out))
            )
        )

        # check that output match
        expect_true(
          object = identical(nrow(df_out), n_row_exp)
        )

      }
    )

    ## Quantified with int ctrl, with dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Quantified"
        show_int_ctrl <- TRUE
        show_dev_int_ctrl <- TRUE
        version <- 0L

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = show_int_ctrl,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # expected dim of output df
        n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                          n_assays = n_assays,
                                          n_samples = n_samples,
                                          has_int_ctrl = show_int_ctrl,
                                          num_int_ctrl = 3L)

        # check that function runs
        expect_no_error(
          expect_no_warning(
            object = df_out <- read_npx_wide(
              df = df_rand$list_df_wide$df_wide,
              file = olink_wide_format,
              data_type = data_type,
              olink_platform = olink_platform
            )
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out) |> sort(),
          expected = colnames(df_rand$list_df_long$df_long) |> sort()
        )

        expect_identical(
          object = df_out,
          expected = df_rand$list_df_long$df_long |>
            dplyr::select(
              dplyr::all_of(colnames(df_out))
            )
        )

        # check that output match
        expect_true(
          object = identical(nrow(df_out), n_row_exp)
        )

      }
    )

  }
)

test_that(
  "read_npx_wide - works - T48 - multiple panels - multiple plates",
  {
    # variables that apply to all tests
    olink_platform <- "Target 48"
    n_panels <- 3L
    n_assays <- 45L
    n_samples <- 99L

    ## NPX no int ctrl, no dev int ctrl, v1 ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"
        show_int_ctrl <- FALSE
        show_dev_int_ctrl <- FALSE
        version <- 1L

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = show_int_ctrl,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # expected dim of output df
        n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                          n_assays = n_assays,
                                          n_samples = n_samples,
                                          has_int_ctrl = show_int_ctrl,
                                          num_int_ctrl = 3L)

        # check that function runs
        expect_no_error(
          expect_no_warning(
            object = df_out <- read_npx_wide(
              df = df_rand$list_df_wide$df_wide,
              file = olink_wide_format,
              data_type = data_type,
              olink_platform = olink_platform
            )
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out) |> sort(),
          expected = colnames(df_rand$list_df_long$df_long) |> sort()
        )

        expect_identical(
          object = df_out,
          expected = df_rand$list_df_long$df_long |>
            dplyr::select(
              dplyr::all_of(colnames(df_out))
            )
        )

        # check that output match
        expect_true(
          object = identical(nrow(df_out), n_row_exp)
        )

      }
    )

    ## NPX no int ctrl, no dev int ctrl, v2 ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"
        show_int_ctrl <- FALSE
        show_dev_int_ctrl <- FALSE
        version <- 2L

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = show_int_ctrl,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # expected dim of output df
        n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                          n_assays = n_assays,
                                          n_samples = n_samples,
                                          has_int_ctrl = show_int_ctrl,
                                          num_int_ctrl = 3L)

        # check that function runs
        expect_no_error(
          expect_no_warning(
            object = df_out <- read_npx_wide(
              df = df_rand$list_df_wide$df_wide,
              file = olink_wide_format,
              data_type = data_type,
              olink_platform = olink_platform
            )
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out) |> sort(),
          expected = colnames(df_rand$list_df_long$df_long) |> sort()
        )

        expect_identical(
          object = df_out,
          expected = df_rand$list_df_long$df_long |>
            dplyr::select(
              dplyr::all_of(colnames(df_out))
            )
        )

        # check that output match
        expect_true(
          object = identical(nrow(df_out), n_row_exp)
        )

      }
    )

    ## NPX no int ctrl, with dev int ctrl, v1 ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"
        show_int_ctrl <- FALSE
        show_dev_int_ctrl <- TRUE
        version <- 1L

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = show_int_ctrl,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # expected dim of output df
        n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                          n_assays = n_assays,
                                          n_samples = n_samples,
                                          has_int_ctrl = show_int_ctrl,
                                          num_int_ctrl = 3L)

        # check that function runs
        expect_no_error(
          expect_no_warning(
            object = df_out <- read_npx_wide(
              df = df_rand$list_df_wide$df_wide,
              file = olink_wide_format,
              data_type = data_type,
              olink_platform = olink_platform
            )
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out) |> sort(),
          expected = colnames(df_rand$list_df_long$df_long) |> sort()
        )

        expect_identical(
          object = df_out,
          expected = df_rand$list_df_long$df_long |>
            dplyr::select(
              dplyr::all_of(colnames(df_out))
            )
        )

        # check that output match
        expect_true(
          object = identical(nrow(df_out), n_row_exp)
        )

      }
    )

    ## NPX no int ctrl, with dev int ctrl, v2 ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"
        show_int_ctrl <- FALSE
        show_dev_int_ctrl <- TRUE
        version <- 2L

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = show_int_ctrl,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # expected dim of output df
        n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                          n_assays = n_assays,
                                          n_samples = n_samples,
                                          has_int_ctrl = show_int_ctrl,
                                          num_int_ctrl = 3L)

        # check that function runs
        expect_no_error(
          expect_no_warning(
            object = df_out <- read_npx_wide(
              df = df_rand$list_df_wide$df_wide,
              file = olink_wide_format,
              data_type = data_type,
              olink_platform = olink_platform
            )
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out) |> sort(),
          expected = colnames(df_rand$list_df_long$df_long) |> sort()
        )

        expect_identical(
          object = df_out,
          expected = df_rand$list_df_long$df_long |>
            dplyr::select(
              dplyr::all_of(colnames(df_out))
            )
        )

        # check that output match
        expect_true(
          object = identical(nrow(df_out), n_row_exp)
        )

      }
    )

    ## NPX with int ctrl, no dev int ctrl, v1 ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"
        show_int_ctrl <- TRUE
        show_dev_int_ctrl <- FALSE
        version <- 1L

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = show_int_ctrl,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # expected dim of output df
        n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                          n_assays = n_assays,
                                          n_samples = n_samples,
                                          has_int_ctrl = show_int_ctrl,
                                          num_int_ctrl = 3L)

        # check that function runs
        expect_no_error(
          expect_no_warning(
            object = df_out <- read_npx_wide(
              df = df_rand$list_df_wide$df_wide,
              file = olink_wide_format,
              data_type = data_type,
              olink_platform = olink_platform
            )
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out) |> sort(),
          expected = colnames(df_rand$list_df_long$df_long) |> sort()
        )

        expect_identical(
          object = df_out,
          expected = df_rand$list_df_long$df_long |>
            dplyr::select(
              dplyr::all_of(colnames(df_out))
            )
        )

        # check that output match
        expect_true(
          object = identical(nrow(df_out), n_row_exp)
        )

      }
    )

    ## NPX with int ctrl, no dev int ctrl, v2 ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"
        show_int_ctrl <- TRUE
        show_dev_int_ctrl <- FALSE
        version <- 2L

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = show_int_ctrl,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # expected dim of output df
        n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                          n_assays = n_assays,
                                          n_samples = n_samples,
                                          has_int_ctrl = show_int_ctrl,
                                          num_int_ctrl = 3L)

        # check that function runs
        expect_no_error(
          expect_no_warning(
            object = df_out <- read_npx_wide(
              df = df_rand$list_df_wide$df_wide,
              file = olink_wide_format,
              data_type = data_type,
              olink_platform = olink_platform
            )
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out) |> sort(),
          expected = colnames(df_rand$list_df_long$df_long) |> sort()
        )

        expect_identical(
          object = df_out,
          expected = df_rand$list_df_long$df_long |>
            dplyr::select(
              dplyr::all_of(colnames(df_out))
            )
        )

        # check that output match
        expect_true(
          object = identical(nrow(df_out), n_row_exp)
        )

      }
    )

    ## NPX with int ctrl, with dev int ctrl, v1 ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"
        show_int_ctrl <- TRUE
        show_dev_int_ctrl <- TRUE
        version <- 1L

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = show_int_ctrl,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # expected dim of output df
        n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                          n_assays = n_assays,
                                          n_samples = n_samples,
                                          has_int_ctrl = show_int_ctrl,
                                          num_int_ctrl = 3L)

        # check that function runs
        expect_no_error(
          expect_no_warning(
            object = df_out <- read_npx_wide(
              df = df_rand$list_df_wide$df_wide,
              file = olink_wide_format,
              data_type = data_type,
              olink_platform = olink_platform
            )
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out) |> sort(),
          expected = colnames(df_rand$list_df_long$df_long) |> sort()
        )

        expect_identical(
          object = df_out,
          expected = df_rand$list_df_long$df_long |>
            dplyr::select(
              dplyr::all_of(colnames(df_out))
            )
        )

        # check that output match
        expect_true(
          object = identical(nrow(df_out), n_row_exp)
        )

      }
    )

    ## NPX with int ctrl, with dev int ctrl, v2 ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "NPX"
        show_int_ctrl <- TRUE
        show_dev_int_ctrl <- TRUE
        version <- 2L

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = show_int_ctrl,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # expected dim of output df
        n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                          n_assays = n_assays,
                                          n_samples = n_samples,
                                          has_int_ctrl = show_int_ctrl,
                                          num_int_ctrl = 3L)

        # check that function runs
        expect_no_error(
          expect_no_warning(
            object = df_out <- read_npx_wide(
              df = df_rand$list_df_wide$df_wide,
              file = olink_wide_format,
              data_type = data_type,
              olink_platform = olink_platform
            )
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out) |> sort(),
          expected = colnames(df_rand$list_df_long$df_long) |> sort()
        )

        expect_identical(
          object = df_out,
          expected = df_rand$list_df_long$df_long |>
            dplyr::select(
              dplyr::all_of(colnames(df_out))
            )
        )

        # check that output match
        expect_true(
          object = identical(nrow(df_out), n_row_exp)
        )

      }
    )

    ## Ct no int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Ct"
        show_int_ctrl <- FALSE
        show_dev_int_ctrl <- FALSE
        version <- 0L

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = show_int_ctrl,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # expected dim of output df
        n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                          n_assays = n_assays,
                                          n_samples = n_samples,
                                          has_int_ctrl = show_int_ctrl,
                                          num_int_ctrl = 3L)

        # check that function runs
        expect_no_error(
          expect_no_warning(
            object = df_out <- read_npx_wide(
              df = df_rand$list_df_wide$df_wide,
              file = olink_wide_format,
              data_type = data_type,
              olink_platform = olink_platform
            )
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out) |> sort(),
          expected = colnames(df_rand$list_df_long$df_long) |> sort()
        )

        expect_identical(
          object = df_out,
          expected = df_rand$list_df_long$df_long |>
            dplyr::select(
              dplyr::all_of(colnames(df_out))
            )
        )

        # check that output match
        expect_true(
          object = identical(nrow(df_out), n_row_exp)
        )

      }
    )

    ## Ct with int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Ct"
        show_int_ctrl <- TRUE
        show_dev_int_ctrl <- FALSE
        version <- 0L

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = show_int_ctrl,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # expected dim of output df
        n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                          n_assays = n_assays,
                                          n_samples = n_samples,
                                          has_int_ctrl = show_int_ctrl,
                                          num_int_ctrl = 3L)

        # check that function runs
        expect_no_error(
          expect_no_warning(
            object = df_out <- read_npx_wide(
              df = df_rand$list_df_wide$df_wide,
              file = olink_wide_format,
              data_type = data_type,
              olink_platform = olink_platform
            )
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out) |> sort(),
          expected = colnames(df_rand$list_df_long$df_long) |> sort()
        )

        expect_identical(
          object = df_out,
          expected = df_rand$list_df_long$df_long |>
            dplyr::select(
              dplyr::all_of(colnames(df_out))
            )
        )

        # check that output match
        expect_true(
          object = identical(nrow(df_out), n_row_exp)
        )

      }
    )

    ## Quantified no int ctrl, no dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Quantified"
        show_int_ctrl <- FALSE
        show_dev_int_ctrl <- FALSE
        version <- 0L

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = show_int_ctrl,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # expected dim of output df
        n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                          n_assays = n_assays,
                                          n_samples = n_samples,
                                          has_int_ctrl = show_int_ctrl,
                                          num_int_ctrl = 3L)

        # check that function runs
        expect_no_error(
          expect_no_warning(
            object = df_out <- read_npx_wide(
              df = df_rand$list_df_wide$df_wide,
              file = olink_wide_format,
              data_type = data_type,
              olink_platform = olink_platform
            )
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out) |> sort(),
          expected = colnames(df_rand$list_df_long$df_long) |> sort()
        )

        expect_identical(
          object = df_out,
          expected = df_rand$list_df_long$df_long |>
            dplyr::select(
              dplyr::all_of(colnames(df_out))
            )
        )

        # check that output match
        expect_true(
          object = identical(nrow(df_out), n_row_exp)
        )

      }
    )

    ## Quantified no int ctrl, with dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Quantified"
        show_int_ctrl <- FALSE
        show_dev_int_ctrl <- TRUE
        version <- 0L

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = show_int_ctrl,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # expected dim of output df
        n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                          n_assays = n_assays,
                                          n_samples = n_samples,
                                          has_int_ctrl = show_int_ctrl,
                                          num_int_ctrl = 3L)

        # check that function runs
        expect_no_error(
          expect_no_warning(
            object = df_out <- read_npx_wide(
              df = df_rand$list_df_wide$df_wide,
              file = olink_wide_format,
              data_type = data_type,
              olink_platform = olink_platform
            )
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out) |> sort(),
          expected = colnames(df_rand$list_df_long$df_long) |> sort()
        )

        expect_identical(
          object = df_out,
          expected = df_rand$list_df_long$df_long |>
            dplyr::select(
              dplyr::all_of(colnames(df_out))
            )
        )

        # check that output match
        expect_true(
          object = identical(nrow(df_out), n_row_exp)
        )

      }
    )

    ## Quantified with int ctrl, no dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Quantified"
        show_int_ctrl <- TRUE
        show_dev_int_ctrl <- FALSE
        version <- 0L

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = show_int_ctrl,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # expected dim of output df
        n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                          n_assays = n_assays,
                                          n_samples = n_samples,
                                          has_int_ctrl = show_int_ctrl,
                                          num_int_ctrl = 3L)

        # check that function runs
        expect_no_error(
          expect_no_warning(
            object = df_out <- read_npx_wide(
              df = df_rand$list_df_wide$df_wide,
              file = olink_wide_format,
              data_type = data_type,
              olink_platform = olink_platform
            )
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out) |> sort(),
          expected = colnames(df_rand$list_df_long$df_long) |> sort()
        )

        expect_identical(
          object = df_out,
          expected = df_rand$list_df_long$df_long |>
            dplyr::select(
              dplyr::all_of(colnames(df_out))
            )
        )

        # check that output match
        expect_true(
          object = identical(nrow(df_out), n_row_exp)
        )

      }
    )

    ## Quantified with int ctrl, with dev int ctrl ----

    withr::with_tempfile(
      new = "olink_wide_format",
      pattern = "test-olink-wide",
      fileext = ".xlsx",
      code = {

        # synthetic wide df
        data_type <- "Quantified"
        show_int_ctrl <- TRUE
        show_dev_int_ctrl <- TRUE
        version <- 0L

        df_rand <- get_wide_synthetic_data(
          olink_platform = olink_platform,
          data_type = data_type,
          n_panels = n_panels,
          n_assays = n_assays,
          n_samples = n_samples,
          show_dev_int_ctrl = show_dev_int_ctrl,
          show_int_ctrl = show_int_ctrl,
          version = version
        )

        # write empty-ish file
        writeLines("foo", olink_wide_format)

        # expected dim of output df
        n_row_exp <- olink_wide2long_rows(n_panels = n_panels,
                                          n_assays = n_assays,
                                          n_samples = n_samples,
                                          has_int_ctrl = show_int_ctrl,
                                          num_int_ctrl = 3L)

        # check that function runs
        expect_no_error(
          expect_no_warning(
            object = df_out <- read_npx_wide(
              df = df_rand$list_df_wide$df_wide,
              file = olink_wide_format,
              data_type = data_type,
              olink_platform = olink_platform
            )
          )
        )

        # check that output match
        expect_identical(
          object = colnames(df_out) |> sort(),
          expected = colnames(df_rand$list_df_long$df_long) |> sort()
        )

        expect_identical(
          object = df_out,
          expected = df_rand$list_df_long$df_long |>
            dplyr::select(
              dplyr::all_of(colnames(df_out))
            )
        )

        # check that output match
        expect_true(
          object = identical(nrow(df_out), n_row_exp)
        )

      }
    )

  }
)
