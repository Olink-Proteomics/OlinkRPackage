#Load reference results

refRes_file <- testthat::test_path('data','refResults.RData')
load(refRes_file)

set.seed(10) #There's some randomness to how the labels are placed on the plot => failed test. Setting the seed should avoid this
volcano_plot <- olink_volcano_plot(ref_results$t.test_results,
                                   olinkid_list = c(""))

volcano_plot2 <- olink_volcano_plot(ref_results$t.test_results,
                                   olinkid_list = c(""),
                                   coloroption =  c('teal', 'pink'))


test_that("olink_volcano_plot works", {
  skip_if_not_installed("vdiffr")
  skip_on_cran()

  volcano_plot_name <- "volcano plot"
  check_snap_exist(test_dir_name = "Volcano_plot", snap_name = volcano_plot_name)
  vdiffr::expect_doppelganger(volcano_plot_name, volcano_plot)

  volcano_plot2_name <- "volcano plot with coloroption"
  check_snap_exist(test_dir_name = "Volcano_plot", snap_name = volcano_plot2_name)
  vdiffr::expect_doppelganger(volcano_plot2_name, volcano_plot2)
})
