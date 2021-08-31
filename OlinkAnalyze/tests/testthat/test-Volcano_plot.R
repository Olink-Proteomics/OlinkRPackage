#Load reference results
refRes_file <- '../data/refResults.RData'
load(refRes_file)

volcano_plot <- olink_volcano_plot(ref_results$ttestresults,
                                   olinkid_list = {ref_results$ttestresults %>%
                                       head(10) %>%
                                       dplyr::pull(OlinkID)})

test_that("olink_volcano_plot works", {
  vdiffr::expect_doppelganger('volcano plot', volcano_plot)
})
