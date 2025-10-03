# Test olink_bridgeability_plot ----

test_that(
  "olink_bridgeability_plot - works",
  {
    skip_if_not_installed("vdiffr")

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
      object = data_norm <- OlinkAnalyze::olink_normalization(
        df1 = npx_ht,
        df2 = npx_3072,
        overlapping_samples_df1 = overlapping_samples,
        df1_project_nr = "Explore HT",
        df2_project_nr = "Explore 3072",
        reference_project = "Explore HT"
      ),
      regexp = "Cross-product normalization will be performed!"
    )

    data_norm_bridge_all <- OlinkAnalyze::olink_bridgeability_plot(
      data = data_norm,
      olink_id = c("OID40770", "OID40835"),
      median_counts_threshold = 150L,
      min_count = 10L
    )

    expect_identical(
      object = length(data_norm_bridge_all),
      expected = 2L
    )

    data_norm_bridge_pick1 <- OlinkAnalyze::olink_bridgeability_plot(
      data = data_norm,
      olink_id = c("OID40770"),
      median_counts_threshold = 150L,
      min_count = 10L
    )

    expect_identical(
      object = length(data_norm_bridge_pick1),
      expected = 1L
    )

    vdiffr::expect_doppelganger(
      title = "bridgeable-plot_OID40770",
      fig = data_norm_bridge_all$OID40770,
      cran = FALSE
    )

    vdiffr::expect_doppelganger(
      title = "bridgeable-plot_OID40770",
      fig = data_norm_bridge_pick1$OID40770,
      cran = FALSE
    )

    vdiffr::expect_doppelganger(
      title = "bridgeable-plot_OID40835",
      fig = data_norm_bridge_all$OID40835,
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
      object = data_norm <- OlinkAnalyze::olink_normalization(
        df1 = npx_ht,
        df2 = npx_3072,
        overlapping_samples_df1 = overlapping_samples,
        df1_project_nr = "Explore HT",
        df2_project_nr = "Explore 3072",
        reference_project = "Explore HT"
      ),
      regexp = "Cross-product normalization will be performed!"
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
      object = data_norm <- OlinkAnalyze::olink_normalization(
        df1 = npx_ht,
        df2 = npx_3072,
        overlapping_samples_df1 = overlapping_samples,
        df1_project_nr = "Explore HT",
        df2_project_nr = "Explore 3072",
        reference_project = "Explore HT"
      ),
      regexp = "Cross-product normalization will be performed!"
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
