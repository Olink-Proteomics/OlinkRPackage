skip_if_not_installed("ggplot2", minimum_version = "3.4.0")

#Load data with hidden/excluded assays (all NPX=NA)
load(file = test_path('data','npx_data_format221010.RData'))
load(file = test_path('data','npx_data_format221121.RData'))

sampleSubset <- c('A1', 'A10', 'A11', 'A12', 'A13', 'A14', 'B79', 'B8', 'B9') #To keep the file size down
distribution_plot <- npx_data1 %>%
  dplyr::filter(SampleID %in% sampleSubset) %>%
  olink_dist_plot()

distribution_plot_treatColor <- npx_data1 %>%
  dplyr::filter(SampleID %in% sampleSubset) %>%
  olink_dist_plot(color_g = 'Treatment')

test_that("olink_dist_plot works", {
  if (requireNamespace("vdiffr", quietly = TRUE) ){
    vdiffr::expect_doppelganger('Distribution plot', distribution_plot)
    vdiffr::expect_doppelganger('Distribution plot col by treatment', distribution_plot_treatColor)
  }

  expect_warning(olink_dist_plot(npx_data_format221010)) # data with all NPX=NA for some assays
  expect_warning(olink_dist_plot(npx_data_format221121)) # data with all NPX=NA for some assays
  expect_warning(olink_dist_plot(npx_data_extended_format221121)) # data with all NPX=NA for some assays
})
