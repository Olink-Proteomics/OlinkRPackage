test_that(
  "olink_pathway_visualization - valid Keyword needed",
  {
    skip_on_cran()
    suppressMessages(skip_if_not_installed("clusterProfiler"))
    skip_if_not_installed("msigdbr", minimum_version = "24.1.0")
    skip_if_not_installed("vdiffr")

    # data ----

    npx_df <- npx_data1 |>
      dplyr::filter(
        !stringr::str_detect(string = .data[["SampleID"]],
                             pattern = "CONTROL")
      ) |>
      dplyr::filter(
        .data[["Site"]] %in% c("Site_C", "Site_E")
      ) |>
      dplyr::filter(
        .data[["Time"]] %in% c("Baseline", "Week.12")
      ) |>
      dplyr::filter(
        .data[["PlateID"]] == "Example_Data_1_CAM.csv"
      )

    npx_df_check <- check_npx(df = npx_df) |>
      suppressMessages() |>
      suppressWarnings()

    # statistics ----

    ttest_results <- olink_ttest(
      df = npx_df,
      check_log = npx_df_check,
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
        check_log = npx_df_check,
        ontology = "Reactome",
        test_results = ttest_results
      ) |>
        suppressMessages() |>
        suppressWarnings()
    )

    ## ORA ----

    expect_no_error(
      object = ora_results <- olink_pathway_enrichment(
        df = npx_df,
        check_log = npx_df_check,
        ontology = "Reactome",
        test_results = ttest_results,
        method = "ORA"
      ) |>
        suppressMessages() |>
        suppressWarnings()
    )

    # Errors ----

    expect_error(
      object = olink_pathway_visualization(),
      regexp = "Argument `enrich_results` is required!"
    )

    expect_error(
      object = olink_pathway_visualization(
        enrich_results = gsea_results,
        keyword = "hfdklahfajikshf"
      ),
      regexp = paste("Filtering `enrich_results` for `enrich_results` =",
                     "\"hfdklahfajikshf\" did not return any results.")
    )

    expect_error(
      object = olink_pathway_visualization(
        enrich_results = ora_results,
        method = "ORA",
        keyword = "hfdklahfajikshf"
      ),
      regexp = paste("Filtering `enrich_results` for `enrich_results` =",
                     "\"hfdklahfajikshf\" did not return any results.")
    )

    # Snapshots ----

    ## v1 ----

    gsea_vis <- olink_pathway_visualization(
      enrich_results = gsea_results
    )
    gsea_vis_name <- "GSEA Visualization"
    check_snap_exist(test_dir_name = "olink_pathway_visualization",
                     snap_name = gsea_vis_name)
    vdiffr::expect_doppelganger(gsea_vis_name, gsea_vis)

    ## v2 ----

    gsea_vis_keyword <- olink_pathway_visualization(
      enrich_results = gsea_results,
      keyword = "immune"
    )
    gsea_vis_keyword_name <- "GSEA Vis with Keyword"
    check_snap_exist(test_dir_name = "olink_pathway_visualization",
                     snap_name = gsea_vis_keyword_name)
    vdiffr::expect_doppelganger(gsea_vis_keyword_name, gsea_vis_keyword)

    ## v3 ----

    ora_vis_terms <- olink_pathway_visualization(
      enrich_results = ora_results,
      method = "ORA",
      number_of_terms = 15L
    )
    ora_vis_terms_name <- "ORA Vis with Terms"
    check_snap_exist(test_dir_name = "olink_pathway_visualization",
                     snap_name = ora_vis_terms_name)
    vdiffr::expect_doppelganger(ora_vis_terms_name, ora_vis_terms)

    ## v4 ----

    ora_vis_keyword <- olink_pathway_visualization(
      enrich_results = ora_results,
      method = "ORA",
      keyword = "SIGNALING"
    )
    ora_vis_keyword_name <- "ORA Vis with keyword"
    check_snap_exist(test_dir_name = "olink_pathway_visualization",
                     snap_name = ora_vis_keyword_name)
    vdiffr::expect_doppelganger(ora_vis_keyword_name, ora_vis_keyword)
  }
)
