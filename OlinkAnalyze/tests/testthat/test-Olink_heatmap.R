#Load data with hidden/excluded assays (all NPX=NA)
load(file = '../data/npx_data_format221010.RData')
npx_Check <- suppressWarnings(npxCheck(npx_data_format221010))
npx_data_format <- npx_data_format221010 %>%
  filter(OlinkID %in% c(npx_Check$all_nas[1:5],"OID30538", "OID30550", "OID30519"))

if (requireNamespace("ggplotify", quietly = TRUE) ){
  testthat::test_that("Heatmap works",{
  # heatmap_check <- suppressWarnings(olink_heatmap_plot(npx_data_format, variable_row_list = "treatment2"))
    expect_warning(olink_heatmap_plot(npx_data_format, variable_row_list = "treatment2"))
  })
}
