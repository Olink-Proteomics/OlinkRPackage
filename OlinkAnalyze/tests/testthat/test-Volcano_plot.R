#Load reference results
refRes_file <- '../data/refResults.RData'
load(refRes_file)

set.seed(10) #There's some randomness to how the labels are placed on the plot => failed test. Setting the seed should avoid this
volcano_plot <- olink_volcano_plot(ref_results$ttestresults,
                                   olinkid_list = {ref_results$ttestresults %>%
                                       head(10) %>%
                                       dplyr::pull(OlinkID)})

volcano_plot2 <- olink_volcano_plot(ref_results$ttestresults,
                                   olinkid_list = {ref_results$ttestresults %>%
                                       head(10) %>%
                                       dplyr::pull(OlinkID)},
                                   coloroption =  c('teal', 'pink'))


test_that("olink_volcano_plot works", {
  skip_on_ci()
  vdiffr::expect_doppelganger('volcano plot', volcano_plot)
  vdiffr::expect_doppelganger('volcano plot with coloroption', volcano_plot2)
})
