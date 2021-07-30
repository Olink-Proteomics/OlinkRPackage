test_that("plot output", {
  p <- olink_pca_plot(npx_data1 %>% filter(!str_detect(SampleID,"CONTROL")))
  vdiffr::expect_doppelganger("PCA plot", p)
})
