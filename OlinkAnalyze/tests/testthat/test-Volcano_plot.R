#Load reference results
testthat::skip_if_not_installed("here")
refRes_file <- testthat::test_path('../data/refResults.RData')
load(refRes_file)

set.seed(10) #There's some randomness to how the labels are placed on the plot => failed test. Setting the seed should avoid this
volcano_plot <- olink_volcano_plot(ref_results$t.test_results,
                                   olinkid_list = c(""))

volcano_plot2 <- olink_volcano_plot(ref_results$t.test_results,
                                   olinkid_list = c(""),
                                   coloroption =  c('teal', 'pink'))


test_that("olink_volcano_plot works", {
  vdiffr::expect_doppelganger('volcano plot', volcano_plot)
  vdiffr::expect_doppelganger('volcano plot with coloroption', volcano_plot2)
})
