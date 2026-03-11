skip_if_not_installed("dplyr")
skip_if_not_installed("tibble")

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
               regexp = "`osi_score` must be one of OSISummary,")
  data1$OSIPreparationTemperature <- NA
  expect_error(olink_osi_dist_plot(df = data1,
                      check_log = check_log_1,
                      osi_score = "OSIPreparationTemperature"),
               regexp = paste0("OSIPreparationTemperature are all NA. ",
                               "Please check your data to confirm OSI "))
  
  expect_warning(olink_osi_dist_plot(df = data1,
                                     check_log = check_log_1,
                                     osi_score = "OSITimeToCentrifugation"),
                 regexp = "NA data detected in")
})

test_that("olink_osi_dist_plot - works",{
  data1 <- data1 |> 
    dplyr::filter(!is.na(OSISummary))
  vdiffr::expect_doppelganger("OSISummary Plot",
                              olink_osi_dist_plot(df = data1,
                                      check_log = check_log_1,
                                      osi_score = "OSISummary"))
})
