
data1 <- get_example_data("osi_data.rds")
check_log_1 <- check_npx(data1)
test_that("osi_distibution plots works - errors", {
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
  data1$OSIPreparationTemperature <- NA
  expect_error(olink_osi_dist_plot(df = data1,
                                   check_log = check_log_1,
                                   osi_score = "OSIPreparationTemperature"),
               regexp = paste0("All values are NA ",
                               "in OSIPreparationTemperature. ",
                               "Please check your data to confirm OSI "))

  data1$OSIPreparationTemperature <- 0.5
  data1$OSIPreparationTemperature[1] <- 0.25
  expect_error(olink_osi_dist_plot(df = data1,
                                   check_log = check_log_1,
                                   osi_score = "OSIPreparationTemperature"),
               regexp = "Multiple OSI values detected for same Sample ID.")

  expect_warning(olink_osi_dist_plot(df = data1,
                                     check_log = check_log_1,
                                     osi_score = "OSITimeToCentrifugation"),
                 regexp = "NA data detected in")
})

test_that("olink_osi_dist_plot - works", {
  data1 <- data1 |>
    dplyr::filter(!is.na(OSISummary))
  vdiffr::expect_doppelganger("OSISummary Plot",
    olink_osi_dist_plot(df = data1,
                        check_log = check_log_1,
                        osi_score = "OSISummary")
  )
})
