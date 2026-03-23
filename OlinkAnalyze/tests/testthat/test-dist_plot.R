
test_that("olink_dist_plot works", {
  skip_if_not_installed("ggplot2", minimum_version = "3.4.0")

  #Load data with hidden/excluded assays (all NPX=NA)
  npx_data_format221010 <- get_example_data(filename = "npx_data_format221010.rds")
  npx_data_format221121 <- get_example_data(filename = "npx_data_format221121.rds")
  npx_data_extended_format221121 <- get_example_data(filename = "npx_data_extended_format221121.rds")

  check_log_format221010 <- check_npx(df = npx_data_format221010) |>
    suppressMessages() |>
    suppressWarnings()

  check_log_format221121 <- check_npx(df = npx_data_format221121) |>
    suppressMessages() |>
    suppressWarnings()

  check_log_extended_format221121 <- check_npx(
    df = npx_data_extended_format221121) |>
    suppressMessages() |>
    suppressWarnings()

  # data with all NPX=NA for some assays
  expect_no_message(
    olink_dist_plot(npx_data_format221010,
                    check_log = check_log_format221010)
  )

  expect_no_message(
    olink_dist_plot(npx_data_format221121,
                    check_log = check_log_format221121)
  )

  expect_no_message(
    olink_dist_plot(npx_data_extended_format221121,
                    check_log = check_log_extended_format221121)
  )

})

test_that("olink_dist_plot works - vdiffr", {
  skip_if_not_installed("ggplot2", minimum_version = "3.4.0")
  skip_on_cran()
  skip_if_not_installed("vdiffr")

  #To keep the file size down
  sampleSubset <- c('A1', 'A10', 'A11', 'A12', 'A13', 'A14', 'B79', 'B8', 'B9')

  npx_data1_filtered <-  npx_data1 |>
    dplyr::filter(SampleID %in% sampleSubset)

  check_log_filtered <- check_npx(
    df = npx_data1_filtered) |>
    suppressMessages() |>
    suppressWarnings()

  expect_no_message(
    distribution_plot <- olink_dist_plot(
      df = npx_data1_filtered,
      check_log =  check_log_filtered
    )
  )

  expect_no_message(
    distribution_plot_treatColor <- olink_dist_plot(
      df = npx_data1_filtered,
      check_log =  check_log_filtered,
      color_g = 'Treatment'
    )
  )

  distribution_plot_name <- "Distribution plot"
  check_snap_exist(test_dir_name = "dist_plot", snap_name = distribution_plot_name)
  vdiffr::expect_doppelganger(distribution_plot_name, distribution_plot)

  distribution_plot_treatColor_name <- 'Distribution plot col by treatment'
  check_snap_exist(test_dir_name = "dist_plot", snap_name = distribution_plot_treatColor_name)
  vdiffr::expect_doppelganger(distribution_plot_treatColor_name, distribution_plot_treatColor)
})
