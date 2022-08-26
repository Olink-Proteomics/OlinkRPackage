skip_on_cran()
skip_if_not_installed("clusterProfiler")

set.seed(123)

npx_df <- npx_data1 %>%
  dplyr::filter(!stringr::str_detect(SampleID, "CONTROL")) %>%
  dplyr::filter(Site %in% c("Site_C", "Site_E")) %>%
  dplyr::filter(Time %in% c("Baseline", "Week.12")) %>%
  dplyr::filter(PlateID == "Example_Data_1_CAM.csv")


ttest_results <- olink_ttest(df=npx_df,
                             variable = 'Treatment',
                             alternative = 'two.sided')


gsea_results <- olink_pathway_enrichment(data = npx_df,
                                         ontology = "Reactome",
                                         test_results = ttest_results)
ora_results <- olink_pathway_enrichment(data = npx_df,
                                        ontology = "Reactome",
                                        test_results = ttest_results,
                                        method = "ORA")

test_that("Valid Keyword needed", {
  expect_error(olink_pathway_visualization(enrich_results = gsea_results,
                                           keyword = "hfdklahfajikshf"))
  expect_error(olink_pathway_visualization(enrich_results = ora_results,
                                           method = "ORA",
                                           keyword = "hfdklahfajikshf"))
})

test_that("Plot works",{
  gsea_vis <- olink_pathway_visualization(enrich_results = gsea_results)
  gsea_vis_keyword <- olink_pathway_visualization(enrich_results = gsea_results,
                                                  keyword = "immune")
  ora_vis_terms <- olink_pathway_visualization(enrich_results = ora_results,
                                               method = "ORA",
                                               number_of_terms = 15)
  ora_vis_keyword <- olink_pathway_visualization(enrich_results = ora_results,
                                                 method = "ORA",
                                                 keyword = "SIGNALING")


  set.seed(123)
  vdiffr::expect_doppelganger("GSEA Visualization", gsea_vis)
  vdiffr::expect_doppelganger("GSEA Vis with Keyword", gsea_vis_keyword)
  vdiffr::expect_doppelganger("ORA Vis with Terms", ora_vis_terms)
  vdiffr::expect_doppelganger("ORA Vis with keyword", ora_vis_keyword)
})
