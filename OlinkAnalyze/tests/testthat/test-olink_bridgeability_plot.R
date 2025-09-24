# Test olink_bridgeability_plot ----

test_that(
  "olink_bridgeability_plot - works",
  {
    skip_on_cran()

    npx_ht <- OlinkAnalyze:::data_ht_small |>
      dplyr::filter(
        .data[["SampleType"]] == "SAMPLE"
      )

    npx_3072 <- OlinkAnalyze:::data_3k_small |>
      dplyr::filter(
        .data[["SampleType"]] == "SAMPLE"
      )

    overlapping_samples <- intersect(
      x = npx_ht$SampleID,
      y = npx_3072$SampleID
    )

    expect_message(
      object = expect_message(
        object = expect_message(
          object = data_norm <- OlinkAnalyze::olink_normalization(
            df1 = npx_ht,
            df2 = npx_3072,
            overlapping_samples_df1 = overlapping_samples,
            df1_project_nr = "Explore HT",
            df2_project_nr = "Explore 3072",
            reference_project = "Explore HT"
          ),
          regexp = "Cross-product normalization will be performed!"
        ),
        regexp = "exhibited assay QC warning"
      ),
      regexp = "exhibited assay QC warning"
    )

    withCallingHandlers({
      data_norm_bridge_all <- OlinkAnalyze::olink_bridgeability_plot(
        data = data_norm,
        olink_id = c("OID40770", "OID40835"),
        median_counts_threshold = 150L,
        min_count = 10L
      )
    }, warning = function(w) {
      if (grepl(x = w, pattern = "not found in PostScript font database")
          || grepl(x = w, pattern = "exhibited assay QC warning"))
        invokeRestart("muffleWarning")
    })

    expect_identical(
      object = length(data_norm_bridge_all),
      expected = 2L
    )

    expect_identical(
      object = names(data_norm_bridge_all),
      expected = c("OID40770", "OID40835")
    )

    withCallingHandlers({
      data_norm_bridge_pick1 <- OlinkAnalyze::olink_bridgeability_plot(
        data = data_norm,
        olink_id = c("OID40770"),
        median_counts_threshold = 150L,
        min_count = 10L
      )
    }, warning = function(w) {
      if (grepl(x = w, pattern = "not found in PostScript font database")
          || grepl(x = w, pattern = "exhibited assay QC warning"))
        invokeRestart("muffleWarning")
    })

    expect_identical(
      object = length(data_norm_bridge_pick1),
      expected = 1L
    )

    expect_identical(
      object = names(data_norm_bridge_pick1),
      expected = "OID40770"
    )
  }
)

test_that(
  "olink_bridgeability_plot - works - plots",
  {
    skip_if_not_installed("vdiffr")
    skip_on_cran()

    npx_ht <- OlinkAnalyze:::data_ht_small |>
      dplyr::filter(
        .data[["SampleType"]] == "SAMPLE"
      )

    npx_3072 <- OlinkAnalyze:::data_3k_small |>
      dplyr::filter(
        .data[["SampleType"]] == "SAMPLE"
      )

    overlapping_samples <- intersect(
      x = npx_ht$SampleID,
      y = npx_3072$SampleID
    )

    expect_message(
      object = expect_message(
        object = expect_message(
          object = data_norm <- OlinkAnalyze::olink_normalization(
            df1 = npx_ht,
            df2 = npx_3072,
            overlapping_samples_df1 = overlapping_samples,
            df1_project_nr = "Explore HT",
            df2_project_nr = "Explore 3072",
            reference_project = "Explore HT"
          ),
          regexp = "Cross-product normalization will be performed!"
        ),
        regexp = "exhibited assay QC warning"
      ),
      regexp = "exhibited assay QC warning"
    )

    data_norm_clean <- OlinkAnalyze:::bridgeability_prep_data(
      data = data_norm,
      min_count = 10L
    ) |>
      dplyr::mutate(
        oid_assay = paste(.data[["Assay"]], .data[["OlinkID"]], sep = " - ")
      )

    # oid = "OID40770" ----

    data_norm_clean_oidOID40770 <- data_norm_clean |>
      dplyr::filter(
        .data[["OlinkID"]] == "OID40770"
      )

    ## iqr plot ----

    vdiffr::expect_doppelganger(
      title = "bridgeable-plot_iqr_OID40770",
      fig = bridgeability_iqr_range_plt(data = data_norm_clean_oidOID40770),
      cran = FALSE
    )

    ## r2 plot ----

    vdiffr::expect_doppelganger(
      title = "bridgeable-plot_r2_OID40770",
      fig = bridgeability_r2_plt(data = data_norm_clean_oidOID40770),
      cran = FALSE
    )

    ## counts plot ----

    vdiffr::expect_doppelganger(
      title = "bridgeable-plot_counts_OID40770",
      fig = bridgeability_counts_plt(
        data = data_norm_clean_oidOID40770,
        median_counts_threshold = 150L
      ),
      cran = FALSE
    )

    ## ks plot ----

    vdiffr::expect_doppelganger(
      title = "bridgeable-plot_ks_OID40770",
      fig = bridgeability_ks_plt(data = data_norm_clean_oidOID40770),
      cran = FALSE
    )

    # oid = "OID40835" ----

    data_norm_clean_oidOID40835 <- data_norm_clean |>
      dplyr::filter(
        .data[["OlinkID"]] == "OID40835"
      )

    ## iqr plot ----

    vdiffr::expect_doppelganger(
      title = "bridgeable-plot_iqr_OID40835",
      fig = bridgeability_iqr_range_plt(data = data_norm_clean_oidOID40835),
      cran = FALSE
    )

    ## r2 plot ----

    vdiffr::expect_doppelganger(
      title = "bridgeable-plot_r2_OID40835",
      fig = bridgeability_r2_plt(data = data_norm_clean_oidOID40835),
      cran = FALSE
    )

    ## counts plot ----

    vdiffr::expect_doppelganger(
      title = "bridgeable-plot_counts_OID40835",
      fig = bridgeability_counts_plt(
        data = data_norm_clean_oidOID40835,
        median_counts_threshold = 150L
      ),
      cran = FALSE
    )

    ## ks plot ----

    vdiffr::expect_doppelganger(
      title = "bridgeable-plot_ks_OID40835",
      fig = bridgeability_ks_plt(data = data_norm_clean_oidOID40835),
      cran = FALSE
    )
  }
)

test_that(
  "olink_bridgeability_plot - error - projects",
  {
    npx_ht <- OlinkAnalyze:::data_ht_small |>
      dplyr::filter(
        .data[["SampleType"]] == "SAMPLE"
      )

    npx_3072 <- OlinkAnalyze:::data_3k_small |>
      dplyr::filter(
        .data[["SampleType"]] == "SAMPLE"
      )

    overlapping_samples <- intersect(
      x = npx_ht$SampleID,
      y = npx_3072$SampleID
    )

    expect_message(
      object = expect_message(
        object = expect_message(
          object = data_norm <- OlinkAnalyze::olink_normalization(
            df1 = npx_ht,
            df2 = npx_3072,
            overlapping_samples_df1 = overlapping_samples,
            df1_project_nr = "Explore HT",
            df2_project_nr = "Explore 3072",
            reference_project = "Explore HT"
          ),
          regexp = "Cross-product normalization will be performed!"
        ),
        regexp = "exhibited assay QC warning"
      ),
      regexp = "exhibited assay QC warning"
    )

    # add one project
    expect_error(
      object = OlinkAnalyze::olink_bridgeability_plot(
        data = data_norm |>
          dplyr::mutate(
            Project = dplyr::if_else(
              .data[["Project"]] == "Explore HT"
              & .data[["OlinkID"]] == "OID40770",
              "I am a new proejct",
              .data[["Project"]]
            )
          ),
        median_counts_threshold = 150L,
        min_count = 10L
      ),
      regexp = "Identified 3 projects"
    )

    # remove one project
    expect_error(
      object = OlinkAnalyze::olink_bridgeability_plot(
        data = data_norm |>
          dplyr::filter(
            .data[["Project"]] == "Explore HT"
          ),
        median_counts_threshold = 150L,
        min_count = 10L
      ),
      regexp = "Identified 1 project"
    )
  }
)

test_that(
  "olink_bridgeability_plot - error - BridgingRecommendation",
  {
    npx_ht <- OlinkAnalyze:::data_ht_small |>
      dplyr::filter(
        .data[["SampleType"]] == "SAMPLE"
      )

    npx_3072 <- OlinkAnalyze:::data_3k_small |>
      dplyr::filter(
        .data[["SampleType"]] == "SAMPLE"
      )

    overlapping_samples <- intersect(
      x = npx_ht$SampleID,
      y = npx_3072$SampleID
    )

    expect_message(
      object = expect_message(
        object = expect_message(
          object = data_norm <- OlinkAnalyze::olink_normalization(
            df1 = npx_ht,
            df2 = npx_3072,
            overlapping_samples_df1 = overlapping_samples,
            df1_project_nr = "Explore HT",
            df2_project_nr = "Explore 3072",
            reference_project = "Explore HT"
          ),
          regexp = "Cross-product normalization will be performed!"
        ),
        regexp = "exhibited assay QC warning"
      ),
      regexp = "exhibited assay QC warning"
    )

    # too many bridging recommendations
    expect_error(
      object = OlinkAnalyze::olink_bridgeability_plot(
        data = data_norm |>
          dplyr::mutate(
            BridgingRecommendation = dplyr::if_else(
              .data[["Project"]] == "Explore HT"
              & .data[["OlinkID"]] == "OID40770"
              & .data[["SampleID"]] %in% c("Sample_A", "Sample_B", "Sample_C"),
              "I am a new bridging recommendation",
              .data[["BridgingRecommendation"]]
            )
          ),
        median_counts_threshold = 150L,
        min_count = 10L
      ),
      regexp = "Identified 2 bridging recommendations"
    )
  }
)
