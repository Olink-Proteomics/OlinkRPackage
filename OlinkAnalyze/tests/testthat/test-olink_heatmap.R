#Load data with hidden/excluded assays (all NPX=NA)
npx_data_format_Oct <- get_example_data(filename = "npx_data_format-Oct-2022.rds")
check_log <- check_npx(npx_data_format_Oct) |> suppressWarnings()
npx_data_format <- clean_npx(npx_data_format_Oct,
                             check_log,
                             verbose = FALSE)
  

testthat::test_that("Heatmap works",{
  skip_if_not_installed("ggplotify")
  expect_warning(olink_heatmap_plot(npx_data_format,
                                    check_log = check_log,
                                    variable_row_list = "treatment2"))
})
