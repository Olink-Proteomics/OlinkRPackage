# This script creates reference plots for usage in the unit tests

#Use this subset of samples to reduce the file size
sampleSubset <- c('A1', 'A10', 'A11', 'A12', 'A13', 'A14', 'B79', 'B8', 'B9')

#### olink_dist_plot ####
distribution_plot <- npx_data1 %>%
  dplyr::filter(SampleID %in% sampleSubset) %>%
  olink_dist_plot()

#### olink_pca_plot ####
pca_plot <- npx_data1 %>%
  dplyr::filter(SampleID %in% sampleSubset) %>%
  olink_pca_plot()

#### olink_qc_plot ####
qc_plot <- npx_data1 %>%
  dplyr::filter(SampleID %in% sampleSubset) %>%
  olink_qc_plot()

#### Wrap up the results ####
ref_images <- list(distribution_plot = distribution_plot,
                   pca_plot = pca_plot,
                   qc_plot = qc_plot)
save(ref_images, file = 'refImages.RData')
