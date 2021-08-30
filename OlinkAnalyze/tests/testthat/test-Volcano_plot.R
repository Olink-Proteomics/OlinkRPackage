#Load reference results
refImages_file <- '../data/refImages.RData'
load(refImages_file)
refRes_file <- '../data/refResults.RData'
load(refRes_file)

#Run function
volcano_plot <- olink_volcano_plot(ref_results$ttestresults,
                                   olinkid_list = {ref_results$ttestresults %>%
                                       head(10) %>%
                                       dplyr::pull(OlinkID)})

test_that("olink_volcano_plot works", {
  expect_equal(volcano_plot$labels$x, "Estimate")
  expect_equal(volcano_plot$labels$y, "-log10(p-value)")
  expect_equal(volcano_plot$labels$label, "Assay")
  expect_equal(volcano_plot$labels$yintercept, "yintercept")

  expect_error(olink_volcano_plot(olinkid_list = {ref_results$ttestresults %>%
      head(10) %>%
      dplyr::pull(OlinkID)}))

  expect_equal(ggplot2::ggplot_build(volcano_plot)$data[[1]],
               ggplot2::ggplot_build(ref_images$volcano_plot)$data[[1]])
})
