skip_if_not_installed("ggplotify")
#Load data with hidden/excluded assays (all NPX=NA)
npx_data_format_oct <- get_example_data(filename =
                                          "npx_data_format-Oct-2022.rds")
check_log_oct <- check_npx(npx_data_format_oct) |> suppressWarnings()
npx_data_format <- clean_npx(npx_data_format_oct,
                             check_log_oct,
                             verbose = FALSE)

npx_data <- npx_data1 |>
  dplyr::filter(!stringr::str_detect(SampleID, "CONT"))
check_log <- check_npx(npx_data)


testthat::test_that("Heatmap works", {

  expect_no_error(olink_heatmap_plot(npx_data_format,
                                     check_log = check_log_oct,
                                     variable_row_list = "treatment2"))

  expect_no_error(olink_heatmap_plot(df = npx_data,
                                     check_log = check_log,
                                     variable_row_list = c("Time", "Site")))

  expect_no_error(olink_heatmap_plot(df = npx_data,
                                     check_log = check_log,
                                     cutree_rows = 3))

})
