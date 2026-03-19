test_that(
  "olink_pathway_heatmap - works",
  {
    # Load pe reference results - skipped if files are absent
    pe_results <- get_example_data(filename = "pathway_enrichment_results.rds")

    skip_on_cran()
    skip_if_not_installed("vdiffr")

    # Errors ----

    expect_error(
      object = olink_pathway_heatmap(),
      regexp = "Arguments `enrich_results` and `test_results` are required!"
    )

    expect_error(
      object = olink_pathway_heatmap(
        enrich_results = pe_results$gsea
      ),
      regexp = "Arguments `enrich_results` and `test_results` are required!"
    )

    expect_error(
      object = olink_pathway_heatmap(
        enrich_results = pe_results$gsea,
        test_results = pe_results$ttest_results,
        keyword = "hfdklahfajikshf"
      ),
      regexp = paste("Filtering `enrich_results` for `enrich_results` =",
                     "\"hfdklahfajikshf\" did not return any results.")
    )

    expect_error(
      object = olink_pathway_heatmap(
        enrich_results = pe_results$ora,
        test_results = pe_results$ttest_results,
        method = "ORA",
        keyword = "hfdklahfajikshf"
      ),
      regexp = paste("Filtering `enrich_results` for `enrich_results` =",
                     "\"hfdklahfajikshf\" did not return any results.")
    )

    # Snapshots ----

    ## v1 ----

    gsea_heatmap <- olink_pathway_heatmap(
      enrich_results = pe_results$gsea,
      test_results = pe_results$ttest_results
    )
    gsea_heatmap_name <- "GSEA Heatmap"
    check_snap_exist(test_dir_name = "olink_pathway_heatmap",
                     snap_name = gsea_heatmap_name)
    vdiffr::expect_doppelganger(gsea_heatmap_name, gsea_heatmap)

    ## v2 ----

    ora_heatmap <- olink_pathway_heatmap(
      enrich_results = pe_results$ora,
      test_results = pe_results$ttest_results,
      method = "ORA",
      keyword = "cell"
    )
    ora_heatmap_name <- "ORA Heatmap with Keyword"
    check_snap_exist(test_dir_name = "olink_pathway_heatmap",
                     snap_name = ora_heatmap_name)
    vdiffr::expect_doppelganger(ora_heatmap_name, ora_heatmap)

  }
)
