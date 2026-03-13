skip_on_cran()
skip_if_not_installed("clusterProfiler")
skip_if_not_installed("ggplot2", minimum_version = "3.4.0")
skip_if_not_installed("msigdbr", minimum_version = "24.1.0")
skip_if_not_installed("broom")
skip_if_not_installed("car")

set.seed(123)
npx_df <- npx_data1 |>
  dplyr::filter(!grepl("control",
                       .data[["SampleID"]],
                       ignore.case = TRUE))
check_log <- check_npx(npx_df)
ttest_results <- olink_ttest(df = npx_df,
                             check_log = check_log,
                             variable = "Treatment",
                             alternative = "two.sided")

gsea_results <- olink_pathway_enrichment(df = npx_df,
                                         check_log = check_log,
                                         test_results = ttest_results)

ora_results <- olink_pathway_enrichment(df = npx_df,
                                        check_log = check_log,
                                        test_results = ttest_results,
                                        method = "ORA")


test_that("Valid Keyword needed", {
  set.seed(123)
  expect_error(olink_pathway_heatmap(enrich_results = gsea_results,
                                     test_results = ttest_results,
                                     keyword = "asfhdlk"))
})

test_that("Plot works", {
  gsea_heatmap <- olink_pathway_heatmap(enrich_results = gsea_results,
                                        test_results = ttest_results)
  ora_heatmap <- olink_pathway_heatmap(enrich_results = ora_results,
                                       test_results = ttest_results,
                                       method = "ORA",
                                       keyword = "cell")

  skip_on_cran()
  skip_if_not_installed("vdiffr")

  set.seed(123)

  gsea_heatmap_name <- "GSEA Heatmap"
  check_snap_exist(test_dir_name = "olink_pathway_heatmap",
                   snap_name = gsea_heatmap_name)
  vdiffr::expect_doppelganger(gsea_heatmap_name, gsea_heatmap)

  ora_heatmap_name <- "ORA Heatmap with Keyword"
  check_snap_exist(test_dir_name = "olink_pathway_heatmap",
                   snap_name = ora_heatmap_name)
  vdiffr::expect_doppelganger(ora_heatmap_name, ora_heatmap)
})
