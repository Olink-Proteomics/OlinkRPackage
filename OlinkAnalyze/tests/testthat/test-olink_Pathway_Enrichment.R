skip_on_cran()
skip_if_not_installed("clusterProfiler")
skip_if_not_installed("msigdbr", minimum_version = "24.1.0")

# clean up npa_data1
npx_df <- npx_data1 |>
  dplyr::filter(
    !stringr::str_detect(.data[["SampleID"]], "CONTROL")
  )

check_log <- check_npx(npx_df)

# statistical tests
ttest_results <- olink_ttest(
  df = npx_df,
  check_log = check_log,
  variable = "Treatment"
) |>
  suppressMessages() |>
  suppressWarnings()

anova_posthoc_results <- olink_anova_posthoc(
  df = npx_df,
  check_log = check_log,
  variable = "Site",
  effect = "Site"
) |>
  suppressMessages() |>
  suppressWarnings()

# set seed
set.seed(123)

test_that("Error with duplicated assays", {
  duplicate_assay_data <- npx_data1 |>
    dplyr::filter(
      .data[["Assay"]] == "MET"
    ) |>
    dplyr::mutate(
      OlinkID = "OID01254"
    ) |>
    dplyr::mutate(
      LOD = .data[["LOD"]] + 1L
    )


  # error with duplicate assays
  duplicated_assay_data <- npx_data1 |>
    dplyr::bind_rows(duplicate_assay_data)
  expect_error(object = data_prep(data = duplicated_assay_data,
                                  test_results = ttest_results,
                                  check_log = check_log),
               regexp = "Duplicated assays detected:")
})

test_that("T-test GSEA works", {
  skip_on_cran()
  skip_if_not_installed("clusterProfiler")
  skip_if_not_installed("msigdbr", minimum_version = "24.1.0")

  expect_no_warning(
    object = expect_no_error(
      object = tt_gsea <- olink_pathway_enrichment(
        data = npx_df,
        check_log = check_log,
        test_results = ttest_results
      ) |>
        suppressMessages() |>
        suppressWarnings()
    )
  )

  expect_equal(
    object = nrow(tt_gsea),
    expected = 571L
  )

  # example data
  npx_data_format22 <- get_example_data("npx_data_format-Oct-2022.rds")

  check_log_221010 <- check_npx(npx_data_format22) |>
    suppressWarnings() |>
    suppressMessages()

  ttest_na <- olink_ttest(
    df = npx_data_format22,
    check_log = check_log_221010,
    variable = "treatment1"
  ) |>
    suppressMessages() |>
    suppressWarnings()

  npx_data_format22 <- npx_data_format22 |>
    dplyr::mutate(OlinkID = ifelse(OlinkID == "OID30646",
                                   "OID12345",
                                   OlinkID))
  check_log_221010 <- check_npx(npx_data_format22) |>
    suppressWarnings() |>
    suppressMessages()

  expect_message(
    object = expect_warning(
      object = olink_pathway_enrichment(
        data = npx_data_format22,
        test_results = ttest_na,
        check_log = check_log_221010
      ),
      regexp = "The Olink IDs in the data do not all match"
    ),
    regexp = "Filtering data for overlapping OlinkIDs in data"
  )
})

test_that("Reactome GSEA works", {
  skip_on_cran()
  skip_if_not_installed("clusterProfiler")
  skip_if_not_installed("msigdbr", minimum_version = "24.1.0")

  expect_no_warning(
    expect_no_error(
      object = tt_gsea_reactome <- olink_pathway_enrichment(
        data = npx_df,
        check_log = check_log,
        test_results = ttest_results,
        ontology = "Reactome"
      ) |>
        suppressMessages()
    )
  )

  expect_equal(
    object = nrow(tt_gsea_reactome),
    expected = 20L
  )
})

test_that("MSigDB_com GSEA works", {
  skip_on_cran()
  skip_if_not_installed("clusterProfiler")
  skip_if_not_installed("msigdbr", minimum_version = "24.1.0")

  expect_no_warning(
    object = expect_no_error(
      object = tt_gsea_msigdb_com <- olink_pathway_enrichment(
        data = npx_df,
        check_log = check_log,
        test_results = ttest_results,
        ontology = "MSigDb_com"
      ) |>
        suppressMessages()
    )
  )

  expect_equal(
    object = nrow(tt_gsea_msigdb_com),
    expected = 566L
  )
})


test_that("KEGG GSEA works", {
  skip_on_cran()
  skip_if_not_installed("clusterProfiler")
  skip_if_not_installed("msigdbr", minimum_version = "24.1.0")

  expect_no_warning(
    object = expect_no_error(
      object = tt_gsea_kegg <- olink_pathway_enrichment(
        data = npx_df,
        check_log = check_log,
        test_results = ttest_results,
        ontology = "KEGG"
      ) |>
        suppressMessages()
    )
  )

  expect_equal(
    object = nrow(tt_gsea_kegg),
    expected = 0L
  )
})

test_that("GO GSEA works", {
  skip_on_cran()
  skip_if_not_installed("clusterProfiler")
  skip_if_not_installed("msigdbr", minimum_version = "24.1.0")

  expect_no_warning(
    object = expect_no_error(
      object = tt_gsea_go <- olink_pathway_enrichment(
        data = npx_df,
        check_log = check_log,
        test_results = ttest_results,
        ontology = "GO"
      ) |>
        suppressMessages()
    )
  )

  expect_equal(
    object = nrow(tt_gsea_go),
    expected = 355L
  )
})

test_that("T-test ORA works", {
  skip_on_cran()
  skip_if_not_installed("clusterProfiler")
  skip_if_not_installed("msigdbr", minimum_version = "24.1.0")

  expect_no_warning(
    object = expect_no_error(
      object = tt_ora <- olink_pathway_enrichment(
        data = npx_df,
        check_log = check_log,
        test_results = ttest_results,
        method = "ORA",
        ontology = "MSigDb"
      ) |>
        suppressMessages()
    )
  )

  expect_equal(
    object = nrow(tt_ora),
    expected = 352L
  )
})

test_that("Reactome ORA works", {
  skip_on_cran()
  skip_if_not_installed("clusterProfiler")
  skip_if_not_installed("msigdbr", minimum_version = "24.1.0")

  expect_no_warning(
    object = expect_no_error(
      object = tt_ora_reactome <- olink_pathway_enrichment(
        data = npx_df,
        check_log = check_log,
        test_results = ttest_results,
        method = "ORA",
        ontology = "Reactome"
      ) |>
        suppressMessages()
    )
  )

  expect_equal(
    object = nrow(tt_ora_reactome),
    expected = 15L
  )
})

test_that("KEGG ORA works", {
  skip_on_cran()
  skip_if_not_installed("clusterProfiler")
  skip_if_not_installed("msigdbr", minimum_version = "24.1.0")

  expect_warning(
    object = tt_ora_kegg <- olink_pathway_enrichment(
      data = npx_df,
      check_log = check_log,
      test_results = ttest_results,
      method = "ORA",
      ontology = "KEGG"
    ) |>
      suppressMessages(),
    regexp = "No remaining pathways within the range 10-500 proteins!"
  )

  expect_true(
    object = is.null(tt_ora_kegg)
  )
})

test_that("GO ORA works", {
  skip_on_cran()
  skip_if_not_installed("clusterProfiler")
  skip_if_not_installed("msigdbr", minimum_version = "24.1.0")

  expect_no_warning(
    object = expect_no_error(
      object = tt_ora_go <- olink_pathway_enrichment(
        data = npx_df,
        check_log = check_log,
        test_results = ttest_results,
        method = "ORA",
        ontology = "GO"
      ) |>
        suppressMessages()
    )
  )

  expect_equal(
    object = nrow(tt_ora_go),
    expected = 217L
  )
})

test_that("Error if more than 1 contrast", {
  skip_on_cran()
  skip_if_not_installed("clusterProfiler")
  skip_if_not_installed("msigdbr", minimum_version = "24.1.0")

  expect_error(
    object = olink_pathway_enrichment(
      data = npx_df,
      check_log = check_log,
      test_results = anova_posthoc_results
    ),
    regexp = "More than one contrast is specified in test results"
  )
})

test_that("Nonsense method errors", {
  skip_on_cran()
  skip_if_not_installed("clusterProfiler")
  skip_if_not_installed("msigdbr", minimum_version = "24.1.0")

  expect_error(
    object = olink_pathway_enrichment(
      data = npx_df,
      check_log = check_log,
      test_results = anova_posthoc_results,
      method = "IRA"
    )
  )
})

test_that("Unsupported databases flag", {
  skip_on_cran()
  skip_if_not_installed("clusterProfiler")
  skip_if_not_installed("msigdbr", minimum_version = "24.1.0")

  expect_error(
    object = olink_pathway_enrichment(
      data = npx_df,
      check_log = check_log,
      test_results = anova_posthoc_results,
      ontology = "WikiPathways"
    )
  )
})

test_that("Estimate column must be present", {
  skip_on_cran()
  skip_if_not_installed("clusterProfiler")
  skip_if_not_installed("msigdbr", minimum_version = "24.1.0")

  expect_error(
    object = olink_pathway_enrichment(
      data = npx_df,
      check_log = check_log,
      test_results = ttest_results |>
        dplyr::select(
          -dplyr::all_of("estimate")
        )
    )
  )
})

test_that("ORA warns assays not found in database", {
  skip_on_cran()
  skip_if_not_installed("clusterProfiler")
  skip_if_not_installed("msigdbr", minimum_version = "24.1.0")

  messages <- capture_messages(
    code = olink_pathway_enrichment(
      data = npx_df,
      check_log = check_log,
      test_results = ttest_results,
      method = "ORA",
      ontology = "MSigDb"
    )
  )
  expect_true(
    object = grepl(
      pattern = "assays are not found in the database.",
      x = paste(messages, collapse = "")
    )
  )
})

test_that("gsea warns assays not found in database", {
  skip_on_cran()
  skip_if_not_installed("clusterProfiler")
  skip_if_not_installed("msigdbr", minimum_version = "24.1.0")

  messages <- capture_messages(
    code = olink_pathway_enrichment(
      data = npx_df,
      check_log = check_log,
      test_results = ttest_results
    )
  )
  expect_true(
    object = grepl(
      pattern = "assays are not found in the database.",
      x = paste(messages, collapse = "")
    )
  )
})
