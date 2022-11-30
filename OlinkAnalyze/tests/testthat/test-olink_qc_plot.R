#Load data with hidden/excluded assays (all NPX=NA)
load(file = '../data/npx_data_format221010.RData')
load(file = '../data/npx_data_format221121.RData')


qc_plot <- npx_data1 %>%
  mutate(SampleID = paste(SampleID, "_", Index, sep = "")) %>%
  olink_qc_plot(label_outliers = FALSE)

qc_plot2 <- npx_data1 %>%
  mutate(SampleID = paste(SampleID, "_", Index, sep = "")) %>%
  olink_qc_plot(coloroption =  c('teal', 'pink'), label_outliers = FALSE)

test_that("olink_qc_plot works", {
  vdiffr::expect_doppelganger('QC plot', qc_plot)
  vdiffr::expect_doppelganger('QC plot with coloroption', qc_plot2)

  expect_warning(olink_qc_plot(npx_data_format221010)) # data with all NPX=NA for some assays
  expect_warning(olink_qc_plot(npx_data_format221121)) # data with all NPX=NA for some assays
  expect_warning(olink_qc_plot(npx_data_extended_format221121)) # data with all NPX=NA for some assays
})
