skip_on_cran()
skip_if_not_installed("clusterProfiler")
skip_if_not_installed("msigdbr")
skip_if_not_installed("msigdbdf")

load(file = test_path('data','npx_data_format221010.RData'))

npx_df <- npx_data1 %>%
  dplyr::filter(!stringr::str_detect(SampleID, "CONTROL"))
ttest_results <- olink_ttest(df = npx_df, variable = "Treatment")
anova_results <- olink_anova(df = npx_df, variable = "Site")
anova_posthoc_results <- olink_anova_posthoc(npx_df, variable = "Site", effect = "Site")
lme_results <- olink_lmer_posthoc(npx_df, variable = "Time", random = "Site", effect = "Time")
set.seed(123)

duplicate_assay_data <- npx_data1 |>
  dplyr::filter(Assay == "MET") |>
  dplyr::mutate(OlinkID = "OID01254") |>
  dplyr::mutate(LOD = LOD + 1)

npx_platelod <- npx_data1 |>
  rbind(duplicate_assay_data) |>
  dplyr::mutate(SampleQC = QC_Warning) |>
  dplyr::mutate(PlateLOD = LOD) |>
  dplyr::select(-QC_Warning, -LOD)

npx_maxlod <- npx_data1 |>
  rbind(duplicate_assay_data) |>
  dplyr::mutate(SampleQC = QC_Warning) |>
  dplyr::mutate(MaxLOD = LOD) |>
  dplyr::select(-QC_Warning, -LOD)

npx_nolod <- npx_data1 |>
  rbind(duplicate_assay_data) |>
  dplyr::mutate(SampleQC = QC_Warning) |>
  dplyr::select(-QC_Warning, -LOD)

ttest_results_no_estimate <- ttest_results %>% dplyr::select(-estimate)
ttest_na <- suppressWarnings(olink_ttest(df = npx_data_format221010, variable = "treatment1"))

test_that("Input data equal for different LOD names", {
  expect_equal(unique(data_prep(npx_data1)$OlinkID), unique(data_prep(npx_platelod)$OlinkID))
  expect_equal(unique(data_prep(npx_data1)$OlinkID), unique(data_prep(npx_maxlod)$OlinkID))
  expect_equal(unique(data_prep(npx_data1)$OlinkID), unique(data_prep(npx_nolod)$OlinkID))
})


test_that("T-test GSEA works", {
  skip_if_not_installed("clusterProfiler")
  skip_if_not_installed("msigdbr")
  skip_if_not_installed("msigdbdf")
  tt_gsea <- olink_pathway_enrichment(npx_df, test_results = ttest_results)
  set.seed(123)
  expect_equal(nrow(tt_gsea), 560)
  expect_warning(expect_warning(olink_pathway_enrichment(npx_data_format221010, test_results = ttest_na)))
})

test_that("Reactome GSEA works", {
  skip_if_not_installed("clusterProfiler")
  skip_if_not_installed("msigdbr")
  skip_if_not_installed("msigdbdf")
  tt_gsea_reactome <- olink_pathway_enrichment(npx_df, test_results = ttest_results, ontology = "Reactome")
  set.seed(123)
  expect_equal(nrow(tt_gsea_reactome), 18)
})

test_that("KEGG GSEA works", {
  skip_if_not_installed("clusterProfiler")
  skip_if_not_installed("msigdbr")
  skip_if_not_installed("msigdbdf")
  tt_gsea_kegg <- olink_pathway_enrichment(npx_df, test_results = ttest_results, ontology = "KEGG")
  set.seed(123)
  expect_equal(nrow(tt_gsea_kegg), 5)
})

test_that("GO GSEA works", {
  skip_if_not_installed("clusterProfiler")
  skip_if_not_installed("msigdbr")
  skip_if_not_installed("msigdbdf")
  tt_gsea_go <- olink_pathway_enrichment(npx_df, test_results = ttest_results, ontology = "GO")
  set.seed(123)
  expect_equal(nrow(tt_gsea_go), 384)
})


test_that("T-test ORA works", {
  skip_if_not_installed("clusterProfiler")
  skip_if_not_installed("msigdbr")
  skip_if_not_installed("msigdbdf")
  tt_ora <- olink_pathway_enrichment(npx_df, test_results = ttest_results, method = "ORA", ontology = "MSigDb")
  set.seed(123)
  expect_equal(nrow(tt_ora), 329)
})

test_that("Reactome ORA works", {
  skip_if_not_installed("clusterProfiler")
  skip_if_not_installed("msigdbr")
  skip_if_not_installed("msigdbdf")
  tt_ora_reactome <- olink_pathway_enrichment(npx_df, test_results = ttest_results, method = "ORA", ontology = "Reactome")
  set.seed(123)
  expect_equal(nrow(tt_ora_reactome), 15)
})

test_that("KEGG ORA works", {
  skip_if_not_installed("clusterProfiler")
  skip_if_not_installed("msigdbr")
  skip_if_not_installed("msigdbdf")
  tt_ora_kegg <- olink_pathway_enrichment(npx_df, test_results = ttest_results, method = "ORA", ontology = "KEGG")
  set.seed(123)
  expect_equal(nrow(tt_ora_kegg), 3)
})

test_that("GO ORA works", {
  skip_if_not_installed("clusterProfiler")
  skip_if_not_installed("msigdbr")
  skip_if_not_installed("msigdbdf")
  tt_ora_go <- olink_pathway_enrichment(npx_df, test_results = ttest_results, method = "ORA", ontology = "GO")
  set.seed(123)
  expect_equal(nrow(tt_ora_go), 226)
})


test_that("Error if more than 1 contrast", {
  skip_if_not_installed("clusterProfiler")
  skip_if_not_installed("msigdbr")
  skip_if_not_installed("msigdbdf")
  set.seed(123)
  expect_error(olink_pathway_enrichment(npx_df, anova_posthoc_results))
})

test_that("Nonsense method errors",{
  skip_if_not_installed("clusterProfiler")
  skip_if_not_installed("msigdbr")
  skip_if_not_installed("msigdbdf")
  set.seed(123)
  expect_error(olink_pathway_enrichment(npx_df, anova_posthoc_results, method = "IRA"))
})

test_that("Unsupported databases flag",{
  skip_if_not_installed("clusterProfiler")
  skip_if_not_installed("msigdbr")
  skip_if_not_installed("msigdbdf")
  set.seed(123)
  expect_error(olink_pathway_enrichment(npx_df, anova_posthoc_results, ontology = "WikiPathways"))
})

test_that("Estimate column must be present",{
  skip_if_not_installed("clusterProfiler")
  skip_if_not_installed("msigdbr")
  skip_if_not_installed("msigdbdf")
  set.seed(123)
  expect_error(olink_pathway_enrichment(npx_df, ttest_results_no_estimate))
})


test_that(" ORA warns assays not found in database. ", {
  skip_if_not_installed("clusterProfiler")
  skip_if_not_installed("msigdbr")
  skip_if_not_installed("msigdbdf")
  messages <- capture_messages(olink_pathway_enrichment(npx_data1, test_results = ttest_results,
                                                        method = "ORA", ontology = "MSigDb"))
  messages <- paste(messages, collapse = "")
  expect_true(grepl(pattern = "assays are not found in the database.", x = messages))

})

test_that(" gsea warns assays not found in database. ", {
  skip_if_not_installed("clusterProfiler")
  skip_if_not_installed("msigdbr")
  skip_if_not_installed("msigdbdf")

  messages <- capture_messages(olink_pathway_enrichment(npx_df, test_results = ttest_results))
  messages <- paste(messages, collapse = "")
  expect_true(grepl(pattern = "assays are not found in the database.", x = messages))
})
