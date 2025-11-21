#Load data with hidden/excluded assays (all NPX=NA)
load(file = test_path('data','npx_data_format221010.RData'))
load(file = test_path('data','npx_data_format221121.RData'))


qc_plot <- npx_data1 %>%
  mutate(SampleID = paste(SampleID, "_", Index, sep = "")) %>%
  olink_qc_plot(label_outliers = FALSE)

qc_plot2 <- npx_data1 %>%
  mutate(SampleID = paste(SampleID, "_", Index, sep = "")) %>%
  olink_qc_plot(coloroption =  c('teal', 'pink'), label_outliers = FALSE)

test_that("olink_qc_plot works", {
  expect_warning(olink_qc_plot(npx_data_format221010)) # data with all NPX=NA for some assays
  expect_warning(olink_qc_plot(npx_data_format221121)) # data with all NPX=NA for some assays
  expect_warning(olink_qc_plot(npx_data_extended_format221121)) # data with all NPX=NA for some assays
})

test_that("olink_qc_plot works - vdiffr", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")

  qc_plot_name <- "QC plot"
  check_snap_exist(test_dir_name = "olink_qc_plot", snap_name = qc_plot_name)
  vdiffr::expect_doppelganger(qc_plot_name, qc_plot)

  qc_plot2_name <- "QC plot with coloroption"
  check_snap_exist(test_dir_name = "olink_qc_plot", snap_name = qc_plot2_name)
  vdiffr::expect_doppelganger(qc_plot2_name, qc_plot2)
})
