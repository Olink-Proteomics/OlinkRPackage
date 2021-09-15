sampleSubset <- c('A1', 'A10', 'A11', 'A12', 'A13', 'A14', 'B79', 'B8', 'B9') #To keep the file size down
distribution_plot <- npx_data1 %>%
  dplyr::filter(SampleID %in% sampleSubset) %>%
  olink_dist_plot()

distribution_plot_treatColor <- npx_data1 %>%
  dplyr::filter(SampleID %in% sampleSubset) %>%
  olink_dist_plot(color_g = 'Treatment')

test_that("olink_dist_plot works", {
  skip_on_ci()
  vdiffr::expect_doppelganger('Distribution plot', distribution_plot)
  vdiffr::expect_doppelganger('Distribution plot col by treatment', distribution_plot_treatColor)
})
