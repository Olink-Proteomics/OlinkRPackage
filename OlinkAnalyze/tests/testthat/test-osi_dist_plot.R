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

    osi_check_log <- check_npx(osi_data) |>
      suppressWarnings() |>
      suppressMessages()

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
