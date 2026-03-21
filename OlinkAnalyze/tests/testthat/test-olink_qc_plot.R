test_that(
  "olink_qc_plot - works - OSI",
  {
    skip_if_not_installed(pkg = "ggrepel")

    osi_data <- get_example_data("example_osi_data.rds")

    # ----------------------------
    # OSICategory invalid value
    # ----------------------------
    df_bad_cat <- osi_data |>
      dplyr::mutate(
        OSICategory = dplyr::if_else(
          dplyr::row_number() == 1L, "7", as.character(.data[["OSICategory"]])
        )
      )
    df_bad_cat_check <- check_npx(df = df_bad_cat) |>
      suppressMessages() |>
      suppressWarnings()

    # No error when not using OSI column
    expect_no_error(
      object = expect_no_message(
        object = expect_no_warning(
          object = olink_qc_plot(
            df = df_bad_cat,
            color_g = "QC_Warning",
            check_log = df_bad_cat_check
          )
        )
      )
    )

    expect_error(
      object = expect_no_message(
        object = expect_no_warning(
          object = olink_qc_plot(
            df = df_bad_cat,
            color_g = "OSICategory",
            check_log = df_bad_cat_check
          )
        )
      ),
      regexp = "Invalid values detected in column \"OSICategory\" of `df`!"
    )

    # ----------------------------
    # OSICategory all NA
    # ----------------------------
    df_cat_all_na <- osi_data |>
      dplyr::mutate(OSICategory = NA)

    expect_error(
      object = olink_qc_plot(
        df = df_cat_all_na,
        color_g = "OSICategory",
        check_log = check_npx(df = df_cat_all_na) |>
          suppressMessages() |>
          suppressWarnings()
      ),
      regexp = paste("All values are 'NA' in the column \"OSICategory\" of the",
                     "dataset `df`!")
    )

    # ------------------------------------------
    # Continuous OSI column non-numeric value
    # ------------------------------------------
    df_bad_cont_nonnum <- osi_data |>
      dplyr::mutate(
        OSITimeToCentrifugation = dplyr::if_else(
          dplyr::row_number() == 1L,
          "oops",
          as.character(.data[["OSITimeToCentrifugation"]])
        )
      )

    expect_error(
      object = olink_qc_plot(
        df = df_bad_cont_nonnum,
        color_g = "OSITimeToCentrifugation",
        check_log = check_npx(df = df_bad_cont_nonnum) |>
          suppressMessages() |>
          suppressWarnings()
      ),
      regexp = paste("Non-numeric values detected in column",
                     "\"OSITimeToCentrifugation\" of `df`!")
    )

    # ----------------------------
    # Continuous OSI column all NA
    # ----------------------------
    df_cont_all_na <- osi_data |>
      dplyr::mutate(OSISummary = NA)

    expect_error(
      object = olink_qc_plot(
        df = df_cont_all_na,
        color_g = "OSISummary",
        check_log = check_npx(df = df_cont_all_na) |>
          suppressMessages() |>
          suppressWarnings()
      ),
      regexp = paste("All values are 'NA' in the column \"OSISummary\" of the",
                     "dataset `df`!")
    )

    # ------------------------------------------------------
    # Valid OSI values should NOT trigger OSI error strings
    # ------------------------------------------------------

    check_log <- check_npx(df = osi_data) |>
      suppressMessages() |>
      suppressWarnings()

    expect_no_error(
      object = expect_no_warning(
        object = expect_no_message(
          object = olink_qc_plot(
            df = osi_data,
            color_g = "OSICategory",
            check_log = check_log
          )
        )
      )
    )

    expect_no_error(
      object = expect_no_warning(
        object = expect_no_message(
          object = olink_qc_plot(
            df = osi_data,
            color_g = "OSISummary",
            check_log = check_log
          )
        )
      )
    )

    expect_no_error(
      object = expect_no_warning(
        object = expect_no_message(
          object = olink_qc_plot(
            df = osi_data,
            color_g = "OSIPreparationTemperature",
            check_log = check_log
          )
        )
      )
    )

    expect_no_error(
      object = expect_no_warning(
        object = expect_no_message(
          object = olink_qc_plot(
            df = osi_data,
            color_g = "OSITimeToCentrifugation",
            check_log = check_log
          )
        )
      )
    )

  }
)

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
    skip_on_cran()
    skip_if_not_installed("ggrepel")
    skip_if_not_installed("vdiffr")

    qc_plot <- npx_data1 |>
      dplyr::mutate(
        SampleID = paste(.data[["SampleID"]], "_",
                         .data[["Index"]], sep = "")
      ) |>
      olink_qc_plot(
        label_outliers = FALSE
      ) |>
      suppressMessages()
    qc_plot_name <- "QC plot"
    check_snap_exist(test_dir_name = "olink_qc_plot", snap_name = qc_plot_name)
    vdiffr::expect_doppelganger(qc_plot_name, qc_plot)

    qc_plot2 <- npx_data1 |>
      dplyr::mutate(
        SampleID = paste(.data[["SampleID"]], "_",
                         .data[["Index"]], sep = "")
      ) |>
      olink_qc_plot(
        coloroption = c("teal", "pink"),
        label_outliers = FALSE
      ) |>
      suppressMessages()
    qc_plot2_name <- "QC plot with coloroption"
    check_snap_exist(test_dir_name = "olink_qc_plot", snap_name = qc_plot2_name)
    vdiffr::expect_doppelganger(qc_plot2_name, qc_plot2)
  }
)
