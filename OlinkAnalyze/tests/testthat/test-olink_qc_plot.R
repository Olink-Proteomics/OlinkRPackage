test_that("OSI errors: using npx_data1 and OSI columns", {

  testthat::skip_if_not_installed(pkg = "ggrepel")

  # ----------------------------
  # OSICategory invalid value
  # ----------------------------
  df_bad_cat <- data1 |>
    dplyr::mutate(OSICategory = ifelse(dplyr::row_number() == 1, "7",
                                       as.character(OSICategory)))

  # No error when not using OSI column
  testthat::expect_no_error(
    testthat::expect_message(
      olink_qc_plot(df_bad_cat, color_g = "QC_Warning"),
      regexp = "`check_log` not provided. Running `check_npx()`.",
      fixed = TRUE
    )
  )

  testthat::expect_error(
    testthat::expect_message(olink_qc_plot(df_bad_cat, color_g = "OSICategory"),
      regexp = paste0("`check_log` not provided. ",
                      "Running `check_npx()`."),
      fixed = TRUE
    ),
    regexp = paste0('Invalid values detected in OSICategory\\.', #nolint quotes_linter
                    ' Expected only 0, 1, 2, 3, or 4\\. Found: "7".') #nolint quotes_linter
  )

  # ----------------------------
  # OSICategory all NA
  # ----------------------------
  df_cat_all_na <- data1 |>
    dplyr::mutate(OSICategory = NA)

  testthat::expect_error(
    testthat::expect_message(olink_qc_plot(df_cat_all_na,
                                           color_g = "OSICategory"),
                             regexp = paste0("`check_log` not provided. ",
                                             "Running `check_npx()`."),
                             fixed = TRUE),
    regexp = paste0("All values are NA in OSICategory\\. ",
                    "Please check your data to confirm OSI data is present\\.")
  )

  # ------------------------------------------
  # Continuous OSI column non-numeric value
  # ------------------------------------------
  df_bad_cont_nonnum <- data1 |>
    dplyr::mutate(OSITimeToCentrifugation = ifelse(
      dplyr::row_number() == 1,
      "oops",
      as.character(OSITimeToCentrifugation)
    )
    )

  testthat::expect_error(
    testthat::expect_message(olink_qc_plot(df_bad_cont_nonnum, color_g = "OSITimeToCentrifugation"), #nolint line_length_linter
                             regexp = paste0("`check_log` not provided. ",
                                             "Running `check_npx()`."),
                             fixed = TRUE),
    regexp = paste0(
      'Invalid values detected in OSITimeToCentrifugation\\.', #nolint quotes_linter
      ' Expected continuous numeric values between 0 and 1\\.', #nolint quotes_linter
      ' Found non-numeric value\\(s\\): "oops".' #nolint quotes_linter
    )
  )

  # ------------------------------------
  # Continuous OSI column out of range
  # ------------------------------------
  df_bad_cont_oor <- data1 |>
    dplyr::mutate(OSIPreparationTemperature = ifelse(dplyr::row_number() == 1, 1.2, OSIPreparationTemperature)) #nolint line_length_linter

  testthat::expect_error(
    testthat::expect_message(olink_qc_plot(df_bad_cont_oor, color_g = "OSIPreparationTemperature"), #nolint line_length_linter
                             regexp = paste0("`check_log` not provided. ",
                                             "Running `check_npx()`."),
                             fixed = TRUE),
    regexp = paste0(
      "Invalid values detected in OSIPreparationTemperature\\.",
      " Expected continuous numeric values between 0 and 1\\.",
      " Found out-of-range value\\(s\\): 1\\.2"
    )
  )

  # ----------------------------
  # Continuous OSI column all NA
  # ----------------------------
  df_cont_all_na <- data1 |>
    dplyr::mutate(OSISummary = NA)

  testthat::expect_error(
    testthat::expect_message(olink_qc_plot(df_cont_all_na, color_g = "OSISummary"), #nolint line_length_linter
                             regexp = paste0("`check_log` not provided. ",
                                             "Running `check_npx()`."),
                             fixed = TRUE),
    regexp = paste0("All values are NA in OSISummary\\.",
                    " Please check your data to confirm OSI data is present\\.")
  )

  # ------------------------------------------------------
  # Valid OSI values should NOT trigger OSI error strings
  # ------------------------------------------------------
  testthat::expect_no_error(
    testthat::expect_message(olink_qc_plot(data1, color_g = "OSICategory"),
                             regexp = paste0("`check_log` not provided. ",
                                             "Running `check_npx()`."),
                             fixed = TRUE)
  )

  testthat::expect_no_error(
    testthat::expect_message(olink_qc_plot(data1, color_g = "OSISummary"),
                             regexp = paste0("`check_log` not provided. ",
                                             "Running `check_npx()`."),
                             fixed = TRUE)
  )

  testthat::expect_no_error(
    testthat::expect_message(olink_qc_plot(data1, color_g = "OSIPreparationTemperature"), #nolint line_length_linter
                             regexp = paste0("`check_log` not provided. ",
                                             "Running `check_npx()`."),
                             fixed = TRUE)
  )

  testthat::expect_no_error(
    testthat::expect_message(olink_qc_plot(data1, color_g = "OSITimeToCentrifugation"), #nolint line_length_linter
                             regexp = paste0("`check_log` not provided. ",
                                             "Running `check_npx()`."),
                             fixed = TRUE)
  )

})

test_that(
  "olink_qc_plot - works - strange datasets",
  {
    skip_if_not_installed(pkg = "ggrepel")

    # Load data with hidden/excluded assays (all NPX=NA)
    npx_data_format221010 <- get_example_data(
      filename = "npx_data_format221010.rds"
    )
    npx_data_format221121 <- get_example_data(
      filename = "npx_data_format221121.rds"
    )
    npx_data_extended_format221121 <- get_example_data(
      filename = "npx_data_extended_format221121.rds"
    )

    # data with all NPX=NA for some assays
    expect_no_error(
      object = expect_no_warning(
        object = expect_no_message(
          object = olink_qc_plot(
            df = npx_data_format221010,
            check_log = check_npx(df = npx_data_format221010) |>
              suppressMessages() |>
              suppressWarnings()
          )
        )
      )
    )

    # data with all NPX=NA for some assays
    expect_no_error(
      object = expect_no_warning(
        object = expect_no_message(
          object = olink_qc_plot(
            df = npx_data_format221121,
            check_log = check_npx(df = npx_data_format221121) |>
              suppressMessages() |>
              suppressWarnings()
          )
        )
      )
    )

    # data with all NPX=NA for some assays
    expect_no_error(
      object = expect_no_warning(
        object = expect_no_message(
          object = olink_qc_plot(
            df = npx_data_extended_format221121,
            check_log = check_npx(df = npx_data_extended_format221121) |>
              suppressMessages() |>
              suppressWarnings()
          )
        )
      )
    )
  }
)

test_that(
  "olink_qc_plot - works - snapshots",
  {

    testthat::skip_if_not_installed(pkg = "ggrepel")

    qc_plot <- npx_data1 |>
      dplyr::mutate(SampleID = paste(SampleID, "_", Index, sep = "")) |>
      olink_qc_plot(label_outliers = FALSE) |>
      suppressMessages()

    qc_plot2 <- npx_data1 |>
      dplyr::mutate(SampleID = paste(SampleID, "_", Index, sep = "")) |>
      olink_qc_plot(coloroption =  c("teal", "pink"), label_outliers = FALSE) |>
      suppressMessages()

    skip_on_cran()
    skip_if_not_installed("vdiffr")

    qc_plot_name <- "QC plot"
    check_snap_exist(test_dir_name = "olink_qc_plot", snap_name = qc_plot_name)
    vdiffr::expect_doppelganger(qc_plot_name, qc_plot)

    qc_plot2_name <- "QC plot with coloroption"
    check_snap_exist(test_dir_name = "olink_qc_plot", snap_name = qc_plot2_name)
    vdiffr::expect_doppelganger(qc_plot2_name, qc_plot2)
  }
)
