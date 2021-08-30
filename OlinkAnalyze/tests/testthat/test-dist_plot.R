#Load reference results
refImages_file <- '../data/refImages.RData'
load(refImages_file)

#Run function
sampleSubset <- c('A1', 'A10', 'A11', 'A12', 'A13', 'A14', 'B79', 'B8', 'B9')
distribution_plot <- npx_data1 %>%
  dplyr::filter(SampleID %in% sampleSubset) %>%
  olink_dist_plot()

test_that("olink_dist_plot works", {
  expect_s3_class(distribution_plot, class = "ggplot")
  expect_equal(distribution_plot$data, ref_images$distribution_plot$data)
  expect_equal(distribution_plot$layers[[1]]$geom_params, ref_images$distribution_plot$layers[[1]]$geom_params)
  expect_equal(ggplot2::ggplot_build(distribution_plot)$data[[1]],
               ggplot2::ggplot_build(ref_images$distribution_plot)$data[[1]])
})
