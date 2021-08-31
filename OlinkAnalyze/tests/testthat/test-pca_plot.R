sampleSubset <- c('A1', 'A10', 'A11', 'A12', 'A13', 'A14', 'B79', 'B8', 'B9')
pca_plot <- npx_data1 %>%
  dplyr::filter(SampleID %in% sampleSubset) %>%
  olink_pca_plot()

test_that("olink_pca_plot works", {
  vdiffr::expect_doppelganger('PCA plot', pca_plot)
})
