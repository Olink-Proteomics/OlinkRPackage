set.seed(10)

qc_plot <- npx_data1 %>%
  mutate(SampleID = paste(SampleID, "_", Index, sep = "")) %>%
  olink_qc_plot()

qc_plot2 <- npx_data1 %>%
  mutate(SampleID = paste(SampleID, "_", Index, sep = "")) %>%
  olink_qc_plot(coloroption =  c('teal', 'pink'))

test_that("olink_pca_plot works", {
  skip_on_ci()
  vdiffr::expect_doppelganger('QC plot', qc_plot)
  vdiffr::expect_doppelganger('QC plot with coloroption', qc_plot2)
})
