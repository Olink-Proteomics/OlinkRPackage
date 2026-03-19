test_that("osi_distibution plots works - errors", {
  data1 <- get_example_data("example_osi_data.rds")
  check_log_1 <- check_npx(data1)

  expect_error(olink_osi_dist_plot(data1,
                                   check_log_1),
               regexp = "`osi_score` must be one of OSISummary,")
  expect_error(olink_osi_dist_plot(df = data1,
                                   check_log = check_log_1,
                                   osi_score = "not a real score"),
               regexp = "`osi_score` must be one of OSISummary,")

  expect_error(olink_osi_dist_plot(df = data1,
                                   check_log = check_log_1,
                                   osi_score = "OSICategory"),
               regexp = "`osi_score` must be one of OSISummary,") # fix

  data1 <- data1 |>
    dplyr::select(-OSISummary)

  expect_error(olink_osi_dist_plot(df = data1,
                                   check_log = check_log_1,
                                   osi_score = "OSISummary"),
               regexp = "missing the required columns: \"OSISummary\"")

  data1_a1 <- data1 |>
    dplyr::filter(.data[["SampleID"]] == "A1")

  data1_dup <- dplyr::bind_rows(data1,
                                data1_a1)
  check_log_dup <- check_npx(data1_dup) |>
    suppressWarnings()

  expect_warning(
    expect_error(olink_osi_dist_plot(df = data1_dup,
                                     check_log = check_log_dup,
                                     osi_score = "OSIPreparationTemperature"),
                 regexp = "Multiple OSI values detected for same Sample ID."),
    regexp = paste0("NA values detected in column ",
                    "\"OSIPreparationTemperature\"")
  )

  data1$OSIPreparationTemperature <- NA
  expect_error(olink_osi_dist_plot(df = data1,
                                   check_log = check_log_1,
                                   osi_score = "OSIPreparationTemperature"),
               regexp = paste0("All values are 'NA' ",
                               "in the column \"OSIPreparationTemperature\""))

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
