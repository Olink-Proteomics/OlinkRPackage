skip_on_cran()

set.seed(123)
npx_df <- npx_data1 %>% filter(!grepl('control',SampleID, ignore.case = TRUE))
ttest_results <- olink_ttest(df=npx_df,
                             variable = 'Treatment',
                             alternative = 'two.sided')

test_that("Valid Keyword needed", {
  skip_if_not_installed("clusterProfiler")
  gsea_results <- olink_pathway_enrichment(data = npx_data1, test_results = ttest_results)
  set.seed(123)
  expect_error(olink_pathway_heatmap(enrich_results = gsea_results, test_results = ttest_results, keyword = "asfhdlk"))
})

test_that("Plot works",{
  skip_if_not_installed("clusterProfiler")
  gsea_results <- olink_pathway_enrichment(data = npx_data1, test_results = ttest_results)
  ora_results <- olink_pathway_enrichment(data = npx_data1,
                                          test_results = ttest_results, method = "ORA")
  gsea_heatmap<-olink_pathway_heatmap(enrich_results = gsea_results, test_results = ttest_results)
  ora_heatmap <- olink_pathway_heatmap(enrich_results = ora_results, test_results = ttest_results,
                                       method = "ORA", keyword = "cell")

  set.seed(123)
  skip_on_ci()
  vdiffr::expect_doppelganger("GSEA Heatmap", gsea_heatmap)
  vdiffr::expect_doppelganger("ORA Heatmap with Keyword", ora_heatmap)
})
