test_that(
  "olink_pathway_visualization - works",
  {
    # Load pe reference results - skipped if files are absent
    pe_results <- get_example_data(filename = "pathway_enrichment_results.rds")

    skip_on_cran()
    skip_if_not_installed("vdiffr")

    # Errors ----

    expect_error(
      object = olink_pathway_visualization(),
      regexp = "Argument `enrich_results` is required!"
    )

    expect_error(
      object = olink_pathway_visualization(
        enrich_results = pe_results$gsea,
        keyword = "hfdklahfajikshf"
      ),
      regexp = paste("Filtering `enrich_results` for `enrich_results` =",
                     "\"hfdklahfajikshf\" did not return any results.")
    )

    expect_error(
      object = olink_pathway_visualization(
        enrich_results = pe_results$ora,
        method = "ORA",
        keyword = "hfdklahfajikshf"
      ),
      regexp = paste("Filtering `enrich_results` for `enrich_results` =",
                     "\"hfdklahfajikshf\" did not return any results.")
    )

    # Snapshots ----

    ## v1 ----

    gsea_vis <- olink_pathway_visualization(
      enrich_results = pe_results$gsea
    )
    gsea_vis_name <- "GSEA Visualization"
    check_snap_exist(test_dir_name = "olink_pathway_visualization",
                     snap_name = gsea_vis_name)
    vdiffr::expect_doppelganger(gsea_vis_name, gsea_vis)

    ## v2 ----

    gsea_vis_keyword <- olink_pathway_visualization(
      enrich_results = pe_results$gsea,
      keyword = "immune"
    )
    gsea_vis_keyword_name <- "GSEA Vis with Keyword"
    check_snap_exist(test_dir_name = "olink_pathway_visualization",
                     snap_name = gsea_vis_keyword_name)
    vdiffr::expect_doppelganger(gsea_vis_keyword_name, gsea_vis_keyword)

    ## v3 ----

    ora_vis_terms <- olink_pathway_visualization(
      enrich_results = pe_results$ora,
      method = "ORA",
      number_of_terms = 15L
    )
    ora_vis_terms_name <- "ORA Vis with Terms"
    check_snap_exist(test_dir_name = "olink_pathway_visualization",
                     snap_name = ora_vis_terms_name)
    vdiffr::expect_doppelganger(ora_vis_terms_name, ora_vis_terms)

    ## v4 ----

    ora_vis_keyword <- olink_pathway_visualization(
      enrich_results = pe_results$ora,
      method = "ORA",
      keyword = "SIGNALING"
    )
    ora_vis_keyword_name <- "ORA Vis with keyword"
    check_snap_exist(test_dir_name = "olink_pathway_visualization",
                     snap_name = ora_vis_keyword_name)
    vdiffr::expect_doppelganger(ora_vis_keyword_name, ora_vis_keyword)
  }
)
