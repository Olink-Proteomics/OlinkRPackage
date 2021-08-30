#Load reference results
refImages_file <- '../data/refImages.RData'
load(refImages_file)

#Run function
sampleSubset <- c('A1', 'A10', 'A11', 'A12', 'A13', 'A14', 'B79', 'B8', 'B9')
pca_plot <- npx_data1 %>%
  dplyr::filter(SampleID %in% sampleSubset) %>%
  olink_pca_plot()

test_that("olink_pca_plot works", {
  expect_s3_class(pca_plot, class = "ggplot")
  expect_equal(pca_plot[[1]], ref_images$pca_plot[[1]])
  expect_equal(ggplot2::ggplot_build(pca_plot)$data[[1]],
               ggplot2::ggplot_build(ref_images$pca_plot)$data[[1]])
})
