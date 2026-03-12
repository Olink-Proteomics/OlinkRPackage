#Load reference results
skip_if_not_installed("broom")
ref_results <- get_example_data("reference_results.rds")
# There's some randomness to how the labels are placed on the plot.
# Setting the seed should avoid this
set.seed(10)
volcano_plot <- olink_volcano_plot(ref_results$t_test,
                                   olinkid_list = c(""))

volcano_plot2 <- olink_volcano_plot(ref_results$t_test,
                                    olinkid_list = c(""),
                                    coloroption =  c("teal", "pink"))

volcano_plot_sig <- olink_volcano_plot(ref_results$t_test,
                                       coloroption =  c("teal", "pink"))


test_that("olink_volcano_plot works", {
  skip_if_not_installed("vdiffr")
  skip_on_cran()

  volcano_plot_name <- "volcano plot"
  check_snap_exist(test_dir_name = "volcano_plot",
                   snap_name = volcano_plot_name)
  vdiffr::expect_doppelganger(volcano_plot_name, volcano_plot)

  volcano_plot2_name <- "volcano plot with coloroption"
  check_snap_exist(test_dir_name = "volcano_plot",
                   snap_name = volcano_plot2_name)
  vdiffr::expect_doppelganger(volcano_plot2_name, volcano_plot2)
  
  volcano_plot3_name <- "volcano plot with labels"
  check_snap_exist(test_dir_name = "volcano_plot",
                   snap_name = volcano_plot3_name)
  vdiffr::expect_doppelganger(volcano_plot3_name, volcano_plot_sig)
})

test_that("olink_volcano_plot errors", {
  expect_error(olink_volcano_plot(ref_results$t_test, fake_variable = 1),
               "The ... option only takes the coloroption")
  expect_error(olink_volcano_plot(ref_results$t_test,
                                  fake_variable1 = 1,
                                  fake_variable2 = 2),
               "The ... option only takes one argument")
})
