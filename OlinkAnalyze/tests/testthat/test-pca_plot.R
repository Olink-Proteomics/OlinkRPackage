set.seed(10)
#Load reference results
refRes_file <- '../data/refResults.RData'
load(refRes_file)

pca_plot <- npx_data1 %>%
  mutate(SampleID = paste(SampleID, "_", Index, sep = "")) %>%
  olink_pca_plot()

pca_plot_treatCol <- npx_data1 %>%
  mutate(SampleID = paste(SampleID, "_", Index, sep = "")) %>%
  filter(!is.na(Treatment)) %>% #Or else, a warning shows up in the test results
  olink_pca_plot(color_g = 'Treatment')

pca_plot_treatCol_topLoadings <- npx_data1 %>%
  mutate(SampleID = paste(SampleID, "_", Index, sep = "")) %>%
  filter(!is.na(Treatment)) %>% #Or else, a warning shows up in the test results
  olink_pca_plot(color_g = 'Treatment',
                 loadings_list = {ref_results$t.test_results %>%
                     head(5) %>%
                     pull(OlinkID)})

#PCA by panel
pca_plot_byPanel <- npx_data1 %>%
  mutate(SampleID = paste(SampleID, "_", Index, sep = "")) %>%
  olink_pca_plot(byPanel = T)

#Label outliers
pca_plot_byPanel_outliers <- npx_data1 %>%
  mutate(SampleID = paste(SampleID, "_", Index, sep = "")) %>%
  olink_pca_plot(byPanel = T, outlierDefX = 4, outlierDefY = 2.5)
outliers <- lapply(pca_plot_byPanel_outliers, function(x){x$data}) %>%
  bind_rows() %>%
  filter(Outlier == 1)

test_that("olink_pca_plot works", {

  # Two Warnings thrown: for dropped assays and droppes samples
  expect_warning(
    expect_warning(
      pca_plot_drop <- npx_data1 %>%
      mutate(SampleID = paste(SampleID, "_", Index, sep = "")) %>%
      olink_pca_plot(drop_assays = TRUE, drop_samples = TRUE)
    )
  )

  expect_equal(outliers$SampleID, c("B4_83", "A14_15", "A15_16", "A19_21"))
  expect_equal(outliers$Panel, c("Cardiometabolic", "Inflammation", "Inflammation", "Inflammation"))

  skip_on_ci()
  vdiffr::expect_doppelganger('PCA plot', pca_plot[[1]])
  vdiffr::expect_doppelganger('PCA plot color by treatment', pca_plot_treatCol[[1]])
  vdiffr::expect_doppelganger('PCA plot with loadings', pca_plot_treatCol_topLoadings[[1]])
  vdiffr::expect_doppelganger('PCA plot drop_assays and drop_samples', pca_plot_drop[[1]])
  vdiffr::expect_doppelganger('PCA plot panel 1', pca_plot_byPanel[[1]])
  vdiffr::expect_doppelganger('PCA plot panel 2', pca_plot_byPanel[[2]])
})
