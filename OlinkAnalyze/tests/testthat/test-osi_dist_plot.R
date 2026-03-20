test_that("osi_distibution plots works - errors", {
  osi_data <- get_example_data("example_osi_data.rds")

  osi_check_log <- check_npx(osi_data) |>
    suppressWarnings() |>
    suppressWarnings()

  expect_error(olink_osi_dist_plot(df = data1,
                                   check_log = check_log_1,
                                   osi_score = "OSICategory"),
               regexp = "`osi_score` must be one of OSISummary,") # fix

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
      object = check_osi(
        df = osi_data_dup,
        check_log = check_log_dup,
        osi_score = "OSIPreparationTemperature"
      ),
      regexp = "Multiple OSI values detected for same Sample ID."
    ),
    regexp = paste0("NA values detected in column ",
                    "\"OSIPreparationTemperature\"")
  )

  data1$OSIPreparationTemperature <- NA

  expect_warning(olink_osi_dist_plot(df = data1,
                                     check_log = check_log_1,
                                     osi_score = "OSITimeToCentrifugation"),
                 regexp = paste0("NA values detected in column ",
                                 "\"OSITimeToCentrifugation\""))

})

test_that("olink_osi_dist_plot - works", {
  testthat::skip_if_not_installed(pkg = "vdiffr")

  data1 <- get_example_data("example_osi_data.rds")
  check_log_1 <- check_npx(data1)

  data1 <- data1 |>
    dplyr::filter(!is.na(OSISummary))

  vdiffr::expect_doppelganger("OSISummary Plot",
    olink_osi_dist_plot(df = data1,
                        check_log = check_log_1,
                        osi_score = "OSISummary")
  )
})
