test_that(
  "olink_pathway_heatmap - works",
  {
    skip_on_cran()
    suppressMessages(skip_if_not_installed("clusterProfiler"))
    skip_if_not_installed("msigdbr", minimum_version = "24.1.0")
    skip_if_not_installed("vdiffr")

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
    ) |>
      suppressMessages()

    # pathway enrichment ----

    set.seed(123)

    ## GSEA ----

    expect_no_error(
      object = gsea_results <- olink_pathway_enrichment(
        df = npx_df,
        check_log = check_log,
        test_results = ttest_results
      ) |>
        suppressMessages() |>
        suppressWarnings()
    )

    ## ORA ----

    expect_no_error(
      object = ora_results <- olink_pathway_enrichment(
        df = npx_df,
        check_log = check_log,
        test_results = ttest_results,
        method = "ORA"
      ) |>
        suppressMessages() |>
        suppressWarnings()
    )

    # Errors ----

    expect_error(
      object = olink_pathway_heatmap(),
      regexp = "Arguments `enrich_results` and `test_results` are required!"
    )

    expect_error(
      object = olink_pathway_heatmap(
        enrich_results = gsea_results
      ),
      regexp = "Arguments `enrich_results` and `test_results` are required!"
    )

    expect_error(
      object = olink_pathway_heatmap(
        enrich_results = gsea_results,
        test_results = ttest_results,
        keyword = "hfdklahfajikshf"
      ),
      regexp = paste("Filtering `enrich_results` for `enrich_results` =",
                     "\"hfdklahfajikshf\" did not return any results.")
    )

    expect_error(
      object = olink_pathway_heatmap(
        enrich_results = ora_results,
        test_results = ttest_results,
        method = "ORA",
        keyword = "hfdklahfajikshf"
      ),
      regexp = paste("Filtering `enrich_results` for `enrich_results` =",
                     "\"hfdklahfajikshf\" did not return any results.")
    )

    # Snapshots ----

    ## v1 ----

    gsea_heatmap <- olink_pathway_heatmap(
      enrich_results = gsea_results,
      test_results = ttest_results
    )
    gsea_heatmap_name <- "GSEA Heatmap"
    check_snap_exist(test_dir_name = "olink_pathway_heatmap",
                     snap_name = gsea_heatmap_name)
    vdiffr::expect_doppelganger(gsea_heatmap_name, gsea_heatmap)

    ## v2 ----

    ora_heatmap <- olink_pathway_heatmap(
      enrich_results = ora_results,
      test_results = ttest_results,
      method = "ORA",
      keyword = "cell"
    )
    ora_heatmap_name <- "ORA Heatmap with Keyword"
    check_snap_exist(test_dir_name = "olink_pathway_heatmap",
                     snap_name = ora_heatmap_name)
    vdiffr::expect_doppelganger(ora_heatmap_name, ora_heatmap)

  }
)
