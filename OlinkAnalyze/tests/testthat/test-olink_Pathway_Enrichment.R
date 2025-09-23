skip_on_cran()
skip_if_not_installed("clusterProfiler")
skip_if_not_installed("msigdbr", minimum_version = "9.0.0")
skip_if_not_installed("msigdbdf")

# clean up npa_data1
npx_df <- npx_data1 |>
  dplyr::filter(
    !stringr::str_detect(.data[["SampleID"]], "CONTROL")
  )

# statistical tests
ttest_results <- olink_ttest(
  df = npx_df,
  variable = "Treatment"
) |>
  suppressMessages() |>
  suppressWarnings()

anova_posthoc_results <- olink_anova_posthoc(
  df = npx_df,
  variable = "Site",
  effect = "Site"
) |>
  suppressMessages() |>
  suppressWarnings()

# set seed
set.seed(123)

test_that("Input data equal for different LOD names", {
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

  # PlateLOD
  expect_equal(
    object = data_prep(data = npx_data1) |>
      suppressMessages() |>
      suppressWarnings() |>
      dplyr::pull(
        .data[["OlinkID"]]
      ) |>
      unique(),
    expected = npx_data1 |>
      dplyr::bind_rows(
        duplicate_assay_data
      ) |>
      dplyr::rename(
        "SampleQC" = "QC_Warning",
        "PlateLOD" = "LOD"
      ) |>
      data_prep() |>
      suppressMessages() |>
      suppressWarnings() |>
      dplyr::pull(
        .data[["OlinkID"]]
      ) |>
      unique()
  )

  # MaxLOD
  expect_equal(
    object = data_prep(data = npx_data1) |>
      suppressMessages() |>
      suppressWarnings() |>
      dplyr::pull(
        .data[["OlinkID"]]
      ) |>
      unique(),
    expected = npx_data1 |>
      dplyr::bind_rows(
        duplicate_assay_data
      ) |>
      dplyr::rename(
        "SampleQC" = "QC_Warning",
        "MaxLOD" = "LOD"
      ) |>
      data_prep() |>
      suppressMessages() |>
      suppressWarnings() |>
      dplyr::pull(
        .data[["OlinkID"]]
      ) |>
      unique()
  )

  # no LOD
  expect_equal(
    object = data_prep(data = npx_data1) |>
      suppressMessages() |>
      suppressWarnings() |>
      dplyr::pull(
        .data[["OlinkID"]]
      ) |>
      unique(),
    expected = npx_data1 |>
      dplyr::bind_rows(
        duplicate_assay_data
      ) |>
      dplyr::rename(
        "SampleQC" = "QC_Warning"
      ) |>
      dplyr::select(
        -dplyr::all_of("LOD")
      ) |>
      data_prep() |>
      suppressMessages() |>
      suppressWarnings() |>
      dplyr::pull(
        .data[["OlinkID"]]
      ) |>
      unique()
  )
})

test_that("T-test GSEA works", {
  skip_on_cran()
  skip_if_not_installed("clusterProfiler")
  skip_if_not_installed("msigdbr", minimum_version = "9.0.0")
  skip_if_not_installed("msigdbdf")

  expect_no_warning(
    object = expect_no_error(
      object = tt_gsea <- olink_pathway_enrichment(
        data = npx_df,
        test_results = ttest_results
      ) |>
        suppressMessages() |>
        suppressWarnings()
    )
  )

  expect_equal(
    object = nrow(tt_gsea),
    expected = 591L
  )

  # example data
  npx_sample_data <- test_path("data", "npx_data_format221010.RData")
  skip_if_not(file.exists(npx_sample_data))
  load(file = npx_sample_data)
  rm(npx_data_format221010.project2, npx_sample_data)

  ttest_na <- olink_ttest(
    df = npx_data_format221010,
    variable = "treatment1"
  ) |>
    suppressMessages() |>
    suppressWarnings()

  expect_warning(
    object = expect_warning(
      object = olink_pathway_enrichment(
        data = npx_data_format221010,
        test_results = ttest_na
      ) |>
        suppressMessages(),
      regexp = "The number of Olink IDs in the data does not equal the number"
    ),
    regexp = "They will be excluded from the analysis"
  )
})

test_that("Reactome GSEA works", {
  skip_on_cran()
  skip_if_not_installed("clusterProfiler")
  skip_if_not_installed("msigdbr", minimum_version = "9.0.0")
  skip_if_not_installed("msigdbdf")

  expect_no_warning(
    expect_no_error(
      object = tt_gsea_reactome <- olink_pathway_enrichment(
        data = npx_df,
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

test_that("KEGG GSEA works", {
  skip_on_cran()
  skip_if_not_installed("clusterProfiler")
  skip_if_not_installed("msigdbr", minimum_version = "9.0.0")
  skip_if_not_installed("msigdbdf")

  expect_no_warning(
    object = expect_no_error(
      object = tt_gsea_kegg <- olink_pathway_enrichment(
        data = npx_df,
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
  skip_if_not_installed("msigdbr", minimum_version = "9.0.0")
  skip_if_not_installed("msigdbdf")

  expect_no_warning(
    object = expect_no_error(
      object = tt_gsea_go <- olink_pathway_enrichment(
        data = npx_df,
        test_results = ttest_results,
        ontology = "GO"
      ) |>
        suppressMessages()
    )
  )

  expect_equal(
    object = nrow(tt_gsea_go),
    expected = 379L
  )
})

test_that("T-test ORA works", {
  skip_on_cran()
  skip_if_not_installed("clusterProfiler")
  skip_if_not_installed("msigdbr", minimum_version = "9.0.0")
  skip_if_not_installed("msigdbdf")

  expect_no_warning(
    object = expect_no_error(
      object = tt_ora <- olink_pathway_enrichment(
        data = npx_df,
        test_results = ttest_results,
        method = "ORA",
        ontology = "MSigDb"
      ) |>
        suppressMessages()
    )
  )

  expect_equal(
    object = nrow(tt_ora),
    expected = 349L
  )
})

test_that("Reactome ORA works", {
  skip_on_cran()
  skip_if_not_installed("clusterProfiler")
  skip_if_not_installed("msigdbr", minimum_version = "9.0.0")
  skip_if_not_installed("msigdbdf")

  expect_no_warning(
    object = expect_no_error(
      object = tt_ora_reactome <- olink_pathway_enrichment(
        data = npx_df,
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
  skip_if_not_installed("msigdbr", minimum_version = "9.0.0")
  skip_if_not_installed("msigdbdf")

  expect_warning(
    object = tt_ora_kegg <- olink_pathway_enrichment(
      data = npx_df,
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
  skip_if_not_installed("msigdbr", minimum_version = "9.0.0")
  skip_if_not_installed("msigdbdf")

  expect_no_warning(
    object = expect_no_error(
      object = tt_ora_go <- olink_pathway_enrichment(
        data = npx_df,
        test_results = ttest_results,
        method = "ORA",
        ontology = "GO"
      ) |>
        suppressMessages()
    )
  )

  expect_equal(
    object = nrow(tt_ora_go),
    expected = 227L
  )
})

test_that("Error if more than 1 contrast", {
  skip_on_cran()
  skip_if_not_installed("clusterProfiler")
  skip_if_not_installed("msigdbr", minimum_version = "9.0.0")
  skip_if_not_installed("msigdbdf")

  expect_error(
    object = olink_pathway_enrichment(
      data = npx_df,
      test_results = anova_posthoc_results
    ),
    regexp = "More than one contrast is specified in test results"
  )
})

test_that("Nonsense method errors", {
  skip_on_cran()
  skip_if_not_installed("clusterProfiler")
  skip_if_not_installed("msigdbr", minimum_version = "9.0.0")
  skip_if_not_installed("msigdbdf")

  expect_error(
    object = olink_pathway_enrichment(
      data = npx_df,
      test_results = anova_posthoc_results,
      method = "IRA"
    )
  )
})

test_that("Unsupported databases flag", {
  skip_on_cran()
  skip_if_not_installed("clusterProfiler")
  skip_if_not_installed("msigdbr", minimum_version = "9.0.0")
  skip_if_not_installed("msigdbdf")

  expect_error(
    object = olink_pathway_enrichment(
      data = npx_df,
      test_results = anova_posthoc_results,
      ontology = "WikiPathways"
    )
  )
})

test_that("Estimate column must be present", {
  skip_on_cran()
  skip_if_not_installed("clusterProfiler")
  skip_if_not_installed("msigdbr", minimum_version = "9.0.0")
  skip_if_not_installed("msigdbdf")

  expect_error(
    object = olink_pathway_enrichment(
      data = npx_df,
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
  skip_if_not_installed("msigdbr", minimum_version = "9.0.0")
  skip_if_not_installed("msigdbdf")
  messages <- capture_messages(
    code = olink_pathway_enrichment(
      data = npx_data1,
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
  skip_if_not_installed("msigdbr", minimum_version = "9.0.0")
  skip_if_not_installed("msigdbdf")

  messages <- capture_messages(
    code = olink_pathway_enrichment(
      data = npx_df,
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
