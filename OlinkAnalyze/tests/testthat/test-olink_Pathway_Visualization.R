skip_on_cran()
skip_on_ci()
skip_if_not_installed("clusterProfiler")
skip_if_not_installed("ggplot2", minimum_version = "3.4.0")
skip_if_not_installed("msigdbr", minimum_version = "24.1.0")

set.seed(123)

npx_df <- npx_data1 %>%
  dplyr::filter(!stringr::str_detect(.data[["SampleID"]], "CONTROL")) %>%
  dplyr::filter(Site %in% c("Site_C", "Site_E")) %>%
  dplyr::filter(Time %in% c("Baseline", "Week.12")) %>%
  dplyr::filter(PlateID == "Example_Data_1_CAM.csv")

check_log <- check_npx(npx_df)


ttest_results <- olink_ttest(df = npx_df,
                             check_log = check_log,
                             variable = "Treatment",
                             alternative = "two.sided")


gsea_results <- olink_pathway_enrichment(data = npx_df,
                                         check_log = check_log,
                                         ontology = "Reactome",
                                         test_results = ttest_results)
ora_results <- olink_pathway_enrichment(data = npx_df,
                                        check_log = check_log,
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

test_that("Plot works", {
  gsea_vis <- olink_pathway_visualization(enrich_results = gsea_results)
  gsea_vis_keyword <- olink_pathway_visualization(enrich_results = gsea_results,
                                                  keyword = "immune")
  ora_vis_terms <- olink_pathway_visualization(enrich_results = ora_results,
                                               method = "ORA",
                                               number_of_terms = 15)
  ora_vis_keyword <- olink_pathway_visualization(enrich_results = ora_results,
                                                 method = "ORA",
                                                 keyword = "SIGNALING")

  skip_on_cran()
  skip_if_not_installed("vdiffr")

  set.seed(123)

  gsea_vis_name <- "GSEA Visualization"
  check_snap_exist(test_dir_name = "olink_Pathway_Visualization",
                   snap_name = gsea_vis_name)
  vdiffr::expect_doppelganger(gsea_vis_name, gsea_vis)

  gsea_vis_keyword_name <- "GSEA Vis with Keyword"
  check_snap_exist(test_dir_name = "olink_Pathway_Visualization",
                   snap_name = gsea_vis_keyword_name)
  vdiffr::expect_doppelganger(gsea_vis_keyword_name, gsea_vis_keyword)

  ora_vis_terms_name <- "ORA Vis with Terms"
  check_snap_exist(test_dir_name = "olink_Pathway_Visualization",
                   snap_name = ora_vis_terms_name)
  vdiffr::expect_doppelganger(ora_vis_terms_name, ora_vis_terms)

  ora_vis_keyword_name <- "ORA Vis with keyword"
  check_snap_exist(test_dir_name = "olink_Pathway_Visualization",
                   snap_name = ora_vis_keyword_name)
  vdiffr::expect_doppelganger(ora_vis_keyword_name, ora_vis_keyword)
})
