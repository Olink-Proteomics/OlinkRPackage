# data ----

npx_df <- npx_data1 |>
  dplyr::filter(
    !grepl(pattern = "control",
           x = .data[["SampleID"]],
           ignore.case = TRUE)
  )

check_log <- check_npx(npx_df)

# statistics ----

ttest_results <- olink_ttest(
  df = npx_df,
  check_log = check_log,
  variable = "Treatment",
  alternative = "two.sided"
)

# pathway enrichment ----

set.seed(123)

## GSEA ----

gsea_results <- OlinkAnalyze::olink_pathway_enrichment(
  df = npx_df,
  check_log = check_log,
  test_results = ttest_results
)

## ORA ----

ora_results <- OlinkAnalyze::olink_pathway_enrichment(
  df = npx_df,
  check_log = check_log,
  test_results = ttest_results,
  method = "ORA"
)

# write the file

pathway_enrichment_results <- list(
  t_test_results = ttest_results,
  gsea = gsea_results,
  ora = ora_results
)

output_file <- "tests/testthat/data/pathway_enrichment_results.rds"
i_want_to_override <- FALSE

if (file.exists(output_file)) {
  if (i_want_to_override == TRUE) {
    saveRDS(
      object = pathway_enrichment_results,
      file = output_file,
      version = 2L,
      compress = "gzip"
    )
  } else {
    message("Certain to override? Set \"i_want_to_override = TRUE\"")
  }
} else {
  saveRDS(
    object = pathway_enrichment_results,
    file = output_file,
    version = 2L,
    compress = "gzip"
  )
}
