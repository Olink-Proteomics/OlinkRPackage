# Test olink_bridgeability_plot ----

test_that(
  "olink_bridgeability_plot - works",
  {
    skip_on_cran()
    skip_if_not_installed("ggpubr")

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
        object = data_norm <- OlinkAnalyze::olink_normalization(
          df1 = npx_ht,
          df2 = npx_3072,
          overlapping_samples_df1 = overlapping_samples,
          df1_project_nr = "Explore HT",
          df2_project_nr = "Explore 3072",
          reference_project = "Explore HT",
          df1_check_log = check_npx(df = npx_ht) |>
            suppressMessages() |>
            suppressWarnings(),
          df2_check_log = check_npx(df = npx_3072) |>
            suppressMessages() |>
            suppressWarnings()
        ),
        regexp = "Cross-product normalization will be performed!"
      ),
      regexp = "Output includes two sets of bridging samples"
    )

    withCallingHandlers({
      data_norm_bridge_all <- OlinkAnalyze::olink_bridgeability_plot(
        df = data_norm,
        check_log = check_npx(df = data_norm) |>
          suppressMessages() |>
          suppressWarnings(),
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
        df = data_norm,
        check_log = check_npx(df = data_norm) |>
          suppressMessages() |>
          suppressWarnings(),
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
    skip_if_not_installed("ggpubr")
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
        object = data_norm <- OlinkAnalyze::olink_normalization(
          df1 = npx_ht,
          df2 = npx_3072,
          overlapping_samples_df1 = overlapping_samples,
          df1_project_nr = "Explore HT",
          df2_project_nr = "Explore 3072",
          reference_project = "Explore HT",
          df1_check_log = check_npx(df = npx_ht) |>
            suppressMessages() |>
            suppressWarnings(),
          df2_check_log = check_npx(df = npx_3072) |>
            suppressMessages() |>
            suppressWarnings()
        ),
        regexp = "Cross-product normalization will be performed!"
      ),
      regexp = "Output includes two sets of bridging samples"
    )

    data_norm_chk <- check_npx(df = data_norm) |>
      suppressMessages() |>
      suppressWarnings()

    data_norm_clean <- OlinkAnalyze:::bridgeability_prep_data(
      df = data_norm,
      check_log = data_norm_chk,
      min_count = 10L
    ) |>
      dplyr::mutate(
        oid_assay = paste(.data[["Assay"]], .data[["OlinkID"]], sep = " - ")
      )

    # oid = "OID40770" ----

    data_norm_clean_oid40770 <- data_norm_clean |>
      dplyr::filter(
        .data[["OlinkID"]] == "OID40770"
      )

    ## iqr plot ----

    oid40770_iqr_name <- "bridgeable-plot_iqr_OID40770"
    check_snap_exist(test_dir_name = "plot_is_bridgeable",
                     snap_name = oid40770_iqr_name)
    vdiffr::expect_doppelganger(
      title = oid40770_iqr_name,
      fig = bridgeability_iqr_range_plt(
        df = data_norm_clean_oid40770,
        check_log = data_norm_chk
      ),
      cran = FALSE
    )

    ## r2 plot ----

    oid40770_r2_name <- "bridgeable-plot_r2_OID40770"
    check_snap_exist(test_dir_name = "plot_is_bridgeable",
                     snap_name = oid40770_r2_name)
    vdiffr::expect_doppelganger(
      title = oid40770_r2_name,
      fig = bridgeability_r2_plt(
        df = data_norm_clean_oid40770,
        check_log = data_norm_chk
      ),
      cran = FALSE
    )

    ## counts plot ----

    oid40770_counts_name <- "bridgeable-plot_counts_OID40770"
    check_snap_exist(test_dir_name = "plot_is_bridgeable",
                     snap_name = oid40770_counts_name)
    vdiffr::expect_doppelganger(
      title = oid40770_counts_name,
      fig = bridgeability_counts_plt(
        df = data_norm_clean_oid40770,
        median_counts_threshold = 150L,
        check_log = data_norm_chk
      ),
      cran = FALSE
    )

    ## ks plot ----

    id40770_ks_name <- "bridgeable-plot_ks_OID40770"
    check_snap_exist(test_dir_name = "plot_is_bridgeable",
                     snap_name = id40770_ks_name)
    vdiffr::expect_doppelganger(
      title = id40770_ks_name,
      fig = bridgeability_ks_plt(
        df = data_norm_clean_oid40770,
        check_log = data_norm_chk
      ),
      cran = FALSE
    )

    # oid = "OID40835" ----

    data_norm_clean_oid40835 <- data_norm_clean |>
      dplyr::filter(
        .data[["OlinkID"]] == "OID40835"
      )

    ## iqr plot ----

    oid40835_iqr_name <- "bridgeable-plot_iqr_OID40835"
    check_snap_exist(test_dir_name = "plot_is_bridgeable",
                     snap_name = oid40835_iqr_name)
    vdiffr::expect_doppelganger(
      title = oid40835_iqr_name,
      fig = bridgeability_iqr_range_plt(
        df = data_norm_clean_oid40835,
        check_log = data_norm_chk
      ),
      cran = FALSE
    )

    ## r2 plot ----

    oid40835_r2_name <- "bridgeable-plot_r2_OID40835"
    check_snap_exist(test_dir_name = "plot_is_bridgeable",
                     snap_name = oid40835_r2_name)
    vdiffr::expect_doppelganger(
      title = oid40835_r2_name,
      fig = bridgeability_r2_plt(
        df = data_norm_clean_oid40835,
        check_log = data_norm_chk
      ),
      cran = FALSE
    )

    ## counts plot ----

    oid40835_counts_name <- "bridgeable-plot_counts_OID40835"
    check_snap_exist(test_dir_name = "plot_is_bridgeable",
                     snap_name = oid40835_counts_name)
    vdiffr::expect_doppelganger(
      title = oid40835_counts_name,
      fig = bridgeability_counts_plt(
        df = data_norm_clean_oid40835,
        median_counts_threshold = 150L,
        check_log = data_norm_chk
      ),
      cran = FALSE
    )

    ## ks plot ----

    oid40835_ks_name <- "bridgeable-plot_ks_OID40835"
    check_snap_exist(test_dir_name = "plot_is_bridgeable",
                     snap_name = oid40835_ks_name)
    vdiffr::expect_doppelganger(
      title = oid40835_ks_name,
      fig = bridgeability_ks_plt(
        df = data_norm_clean_oid40835,
        check_log = data_norm_chk
      ),
      cran = FALSE
    )
  }
)

test_that(
  "olink_bridgeability_plot - error - projects",
  {
    skip_if_not_installed("ggpubr")
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
        object = data_norm <- OlinkAnalyze::olink_normalization(
          df1 = npx_ht,
          df2 = npx_3072,
          overlapping_samples_df1 = overlapping_samples,
          df1_project_nr = "Explore HT",
          df2_project_nr = "Explore 3072",
          reference_project = "Explore HT",
          df1_check_log = check_npx(df = npx_ht) |>
            suppressMessages() |>
            suppressWarnings(),
          df2_check_log = check_npx(df = npx_3072) |>
            suppressMessages() |>
            suppressWarnings()
        ),
        regexp = "Cross-product normalization will be performed!"
      ),
      regexp = "Output includes two sets of bridging samples"
    )

    # add one project
    expect_error(
      object = OlinkAnalyze::olink_bridgeability_plot(
        df = data_norm |>
          dplyr::mutate(
            Project = dplyr::if_else(
              .data[["Project"]] == "Explore HT"
              & .data[["OlinkID"]] == "OID40770",
              "I am a new proejct",
              .data[["Project"]]
            )
          ),
        check_log = check_npx(df = data_norm) |>
          suppressMessages() |>
          suppressWarnings(),
        median_counts_threshold = 150L,
        min_count = 10L
      ),
      regexp = "Identified 3 projects"
    )

    # remove one project
    expect_error(
      object = OlinkAnalyze::olink_bridgeability_plot(
        df = data_norm |>
          dplyr::filter(
            .data[["Project"]] == "Explore HT"
          ),
        check_log = check_npx(df = data_norm) |>
          suppressMessages() |>
          suppressWarnings(),
        median_counts_threshold = 150L,
        min_count = 10L
      ),
      regexp = "Identified 1 project"
    )
  }
)

test_that(
  "olink_bridgeability_plot - error - BridgingRecommendation - too many",
  {
    skip_if_not_installed("ggpubr")
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
        object = data_norm <- OlinkAnalyze::olink_normalization(
          df1 = npx_ht,
          df2 = npx_3072,
          overlapping_samples_df1 = overlapping_samples,
          df1_project_nr = "Explore HT",
          df2_project_nr = "Explore 3072",
          reference_project = "Explore HT",
          df1_check_log = check_npx(df = npx_ht) |>
            suppressMessages() |>
            suppressWarnings(),
          df2_check_log = check_npx(df = npx_3072) |>
            suppressMessages() |>
            suppressWarnings()
        ),
        regexp = "Cross-product normalization will be performed!"
      ),
      regexp = "Output includes two sets of bridging samples"
    )

    # too many bridging recommendations
    expect_error(
      object = OlinkAnalyze::olink_bridgeability_plot(
        df = data_norm |>
          dplyr::mutate(
            BridgingRecommendation = dplyr::if_else(
              .data[["Project"]] == "Explore HT"
              & .data[["OlinkID"]] == "OID40770"
              & .data[["SampleID"]] %in% c("Sample_A", "Sample_B", "Sample_C"),
              "QuantileSmoothing",
              .data[["BridgingRecommendation"]]
            )
          ),
        check_log = check_npx(df = data_norm) |>
          suppressMessages() |>
          suppressWarnings(),
        median_counts_threshold = 150L,
        min_count = 10L
      ),
      regexp = "Identified 2 bridging recommendations"
    )
  }
)

test_that(
  "olink_bridgeability_plot - error - BridgingRecommendation - wrong flags",
  {
    skip_if_not_installed("ggpubr")
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
        object = data_norm <- OlinkAnalyze::olink_normalization(
          df1 = npx_ht,
          df2 = npx_3072,
          overlapping_samples_df1 = overlapping_samples,
          df1_project_nr = "Explore HT",
          df2_project_nr = "Explore 3072",
          reference_project = "Explore HT",
          df1_check_log = check_npx(df = npx_ht) |>
            suppressMessages() |>
            suppressWarnings(),
          df2_check_log = check_npx(df = npx_3072) |>
            suppressMessages() |>
            suppressWarnings()
        ),
        regexp = "Cross-product normalization will be performed!"
      ),
      regexp = "Output includes two sets of bridging samples"
    )

    # check log with errors
    expect_error(
      object = OlinkAnalyze::olink_bridgeability_plot(
        df = data_norm |>
          dplyr::mutate(
            BridgingRecommendation = dplyr::if_else(
              .data[["Project"]] == "Explore HT"
              & .data[["OlinkID"]] == "OID40770"
              & .data[["SampleID"]] %in% c("Sample_A", "Sample_B", "Sample_C"),
              "I am a new bridging recommendation",
              .data[["BridgingRecommendation"]]
            )
          ),
        check_log = check_npx(df = data_norm) |>
          suppressMessages() |>
          suppressWarnings(),
        olink_id = c("OID40770", "OID40835"),
        median_counts_threshold = 150L,
        min_count = 10L
      ),
      regexp = paste("Identified invalid bridging recommendation in column",
                     "`BridgingRecommendation`: \"I am a new bridging",
                     "recommendation\".")
    )
  }
)

test_that(
  "olink_bridgeability_plot - error - duplicate samples",
  {
    skip_if_not_installed("ggpubr")
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
        object = data_norm <- OlinkAnalyze::olink_normalization(
          df1 = npx_ht,
          df2 = npx_3072,
          overlapping_samples_df1 = overlapping_samples,
          df1_project_nr = "Explore HT",
          df2_project_nr = "Explore 3072",
          reference_project = "Explore HT",
          df1_check_log = check_npx(df = npx_ht) |>
            suppressMessages() |>
            suppressWarnings(),
          df2_check_log = check_npx(df = npx_3072) |>
            suppressMessages() |>
            suppressWarnings()
        ),
        regexp = "Cross-product normalization will be performed!"
      ),
      regexp = "Output includes two sets of bridging samples"
    )

    # check log with errors
    expect_error(
      object = OlinkAnalyze::olink_bridgeability_plot(
        df = data_norm |>
          dplyr::bind_rows(
            data_norm |>
              dplyr::filter(
                .data[["Project"]] == "Explore HT"
                & .data[["OlinkID"]] == "OID40770"
                & .data[["SampleID"]] %in% c("Sample_A", "Sample_B", "Sample_C")
              )
          ),
        check_log = check_npx(df = data_norm) |>
          suppressMessages() |>
          suppressWarnings(),
        olink_id = c("OID40770", "OID40835"),
        median_counts_threshold = 150L,
        min_count = 10L
      ),
      regexp = paste("Identified 3 duplicate samples in dataset `df`:",
                     "\"Sample_A\", \"Sample_B\", and \"Sample_C\".")
    )
  }
)

test_that(
  "olink_bridgeability_plot - error - OlinkID",
  {
    skip_if_not_installed("ggpubr")
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
        object = data_norm <- OlinkAnalyze::olink_normalization(
          df1 = npx_ht,
          df2 = npx_3072,
          overlapping_samples_df1 = overlapping_samples,
          df1_project_nr = "Explore HT",
          df2_project_nr = "Explore 3072",
          reference_project = "Explore HT",
          df1_check_log = check_npx(df = npx_ht) |>
            suppressMessages() |>
            suppressWarnings(),
          df2_check_log = check_npx(df = npx_3072) |>
            suppressMessages() |>
            suppressWarnings()
        ),
        regexp = "Cross-product normalization will be performed!"
      ),
      regexp = "Output includes two sets of bridging samples"
    )

    # OlinkID not present in data
    expect_error(
      object = OlinkAnalyze::olink_bridgeability_plot(
        df = data_norm |>
          dplyr::mutate(
            OlinkID = dplyr::if_else(
              .data[["OlinkID"]] == "OID40770", "OID40771", .data[["OlinkID"]]
            )
          ),
        check_log = check_npx(df = data_norm) |>
          suppressMessages() |>
          suppressWarnings(),
        olink_id = c("OID40770", "OID40835"),
        median_counts_threshold = 150L,
        min_count = 10L
      ),
      regexp = paste("1 Olink assay identifiers is not present in the dataset",
                     "`df`: \"OID40770\"")
    )
  }
)
