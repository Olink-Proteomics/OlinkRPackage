npx_df <- npx_data1 %>% 
  dplyr::filter(!stringr::str_detect(SampleID, "CONTROL"))
ttest_results <- olink_ttest(df = npx_df, variable = "Treatment")
anova_results <- olink_anova(df = npx_df, variable = "Site")
anova_posthoc_results <- olink_anova_posthoc(npx_df, variable = "Site", effect = "Site")
lme_results <- olink_lmer_posthoc(npx_df, variable = "Time", random = "Site", effect = "Time")
set.seed(123)
tt_gsea <- olink_pathway_enrichment(npx_df, test_results = ttest_results)
tt_gsea_reactome <- olink_pathway_enrichment(npx_df, test_results = ttest_results, ontology = "Reactome")
tt_gsea_kegg <- olink_pathway_enrichment(npx_df, test_results = ttest_results, ontology = "KEGG")
tt_gsea_go <- olink_pathway_enrichment(npx_df, test_results = ttest_results, ontology = "GO")

tt_ora <- olink_pathway_enrichment(npx_df, test_results = ttest_results, method = "ORA", ontology = "MSigDb")
tt_ora_reactome <- olink_pathway_enrichment(npx_df, test_results = ttest_results, method = "ORA", ontology = "Reactome")
tt_ora_kegg <- olink_pathway_enrichment(npx_df, test_results = ttest_results, method = "ORA", ontology = "KEGG")
tt_ora_go <- olink_pathway_enrichment(npx_df, test_results = ttest_results, method = "ORA", ontology = "GO")

ttest_results_no_estimate <- ttest_results %>% dplyr::select(-estimate)

test_that("T-test GSEA works", {
  set.seed(123)
  expect_equal(nrow(tt_gsea), 560)
})

test_that("Reactome GSEA works", {
  set.seed(123)
  expect_equal(nrow(tt_gsea_reactome), 18)
})

test_that("KEGG GSEA works", {
  set.seed(123)
  expect_equal(nrow(tt_gsea_kegg), 5)
})

test_that("GO GSEA works", {
  set.seed(123)
  expect_equal(nrow(tt_gsea_go), 384)
})


test_that("T-test ORA works", {
  set.seed(123)
  expect_equal(nrow(tt_ora), 329)
})

test_that("Reactome ORA works", {
  set.seed(123)
  expect_equal(nrow(tt_ora_reactome), 15)
})

test_that("KEGG ORA works", {
  set.seed(123)
  expect_equal(nrow(tt_ora_kegg), 3)
})

test_that("GO ORA works", {
  set.seed(123)
  expect_equal(nrow(tt_ora_go), 226)
})


test_that("Error if more than 1 contrast", {
  set.seed(123)
  expect_error(olink_pathway_enrichment(npx_df, anova_posthoc_results))
})

test_that("Nonsense method errors",{
  set.seed(123)
  expect_error(olink_pathway_enrichment(npx_df, anova_posthoc_results, method = "IRA"))
})

test_that("Unsupported databases flag",{
  set.seed(123)
  expect_error(olink_pathway_enrichment(npx_df, anova_posthoc_results, ontology = "WikiPathways"))
})

test_that("Estimate column must be present",{
  set.seed(123)
  expect_error(olink_pathway_enrichment(npx_df, ttest_results_no_estimate))
})
