test_that(
  "osi_distibution plots works - errors",
  {
    osi_data <- get_example_data("example_osi_data.rds")

    osi_check_log <- check_npx(osi_data) |>
      suppressWarnings() |>
      suppressMessages()

    # osi_score = "OSICategory" error ----

    expect_error(
      object = olink_osi_dist_plot(
        df = osi_data,
        check_log = osi_check_log,
        osi_score = "OSICategory"
      ),
      regexp = paste("The argument `osi_score` should be one of the continuous",
                     "OSI scores, not \"OSICategory\"."),
      fixed = TRUE
    )

    # duplicate SampleID ----

    osi_data_dup <- osi_data |>
      dplyr::bind_rows(
        osi_data |>
          dplyr::filter(
            .data[["SampleID"]] == "A1"
          )
      )

    check_log_dup <- check_npx(df = osi_data_dup) |>
      suppressWarnings() |>
      suppressMessages()

    expect_warning(
      object = expect_error(
        object = olink_osi_dist_plot(
          df = osi_data_dup,
          check_log = check_log_dup,
          osi_score = "OSIPreparationTemperature"
        ),
        regexp = "Multiple OSI values detected for the same sample identifier"
      ),
      regexp = paste("NA values detected in column",
                     "\"OSIPreparationTemperature\" of `df`. Filtering out NA",
                     "values."),
      fixed = TRUE
    )

    # NA values in OSI column ----

    expect_warning(
      object = olink_osi_dist_plot(
        df = osi_data |>
          dplyr::mutate(
            OSIPreparationTemperature = NA
          ),
        check_log = osi_check_log,
        osi_score = "OSITimeToCentrifugation"
      ),
      regexp = paste("NA values detected in column \"OSITimeToCentrifugation\"",
                     "of `df`. Filtering out NA values."),
      fixed = TRUE
    )

  }
)

test_that(
  "olink_osi_dist_plot - works - snapshot",
  {
    skip_if_not_installed(pkg = "vdiffr")

    osi_data <- get_example_data("example_osi_data.rds") |>
      dplyr::filter(
        !is.na(.data[["OSISummary"]])
      )

    osi_check_log <- check_npx(df = osi_data) |>
      suppressWarnings() |>
      suppressMessages()

    osi_dist_plot_name <- "osisummary-plot"
    check_snap_exist(test_dir_name = "osi_dist_plot",
                     snap_name = osi_dist_plot_name)
    vdiffr::expect_doppelganger(
      title = "OSISummary Plot",
      fig = olink_osi_dist_plot(
        df = osi_data,
        check_log = osi_check_log,
        osi_score = "OSISummary"
      )
    )
  }
)

test_that(
  "olink_osi_dist_plot - works - olink_class",
  {
    skip_if_not_installed(pkg = "vdiffr")

    osi_data <- get_example_data("example_osi_data.rds") |>
      dplyr::filter(
        !is.na(.data[["OSISummary"]])
      )

    expect_no_error(
      object = expect_no_warning(
        object = expect_no_message(
          object = osi_data_obj <- attach_check_log(
            df = osi_data,
            out_df = "tibble"
          )
        )
      )
    )

    # we can't use expect_equal_ggplot because df inherits olink_class and when
    # the plot is generated, the olink_class is maintained in the plot data,
    # which causes the test to fail as it does not match the ggplot object from
    # the test using simple tibble osi_data. So instead we use vdiffr to check
    # the plot output, which should be the same regardless of the class of the
    # input data. This adds a snapshot test for the plot output when using
    # olink_class data, which is important to ensure that the plot function
    # works correctly with olink_class data and produces the expected visual
    # output.

    osi_dist_plot_name <- "osisummary-plot-olink-class"
    check_snap_exist(test_dir_name = "osi_dist_plot",
                     snap_name = osi_dist_plot_name)
    vdiffr::expect_doppelganger(
      title = "OSISummary Plot - olink class",
      fig = olink_osi_dist_plot(
        df = osi_data_obj,
        osi_score = "OSISummary"
      )
    )
  }
)
