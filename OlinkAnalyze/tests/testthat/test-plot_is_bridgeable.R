# Test olink_bridgeability_plot ----

test_that(
  "olink_bridgeability_plot - works - full data",
  {
    skip_on_cran()
    skip_if_not_installed("ggpubr")

    npx_3072 <- get_example_data(filename = "example_3k_data.rds")
    npx_ht <- get_example_data(filename = "example_HT_data.rds")

    npx_ht <- npx_ht |>
      dplyr::filter(
        .data[["SampleType"]] == "SAMPLE"
      )

    npx_3072 <- npx_3072 |>
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
          object = expect_message(
            object = expect_warning(
              data_norm <- OlinkAnalyze::olink_normalization(
                df1 = npx_ht,
                df2 = npx_3072,
                overlapping_samples_df1 = overlapping_samples,
                df1_project_nr = "Explore HT",
                df2_project_nr = "Explore 3072",
                reference_project = "Explore HT",
                # setting format = TRUE to test that function works for when
                # BridgingRecommendation is "NotOverlapping"
                format = TRUE,
                df1_check_log = check_npx(df = npx_ht) |>
                  suppressMessages() |>
                  suppressWarnings(),
                df2_check_log = check_npx(df = npx_3072) |>
                  suppressMessages() |>
                  suppressWarnings()
              ),
              regexp = "2 assays are not shared across products"
            ),
            regexp = "Cross-product normalization will be performed!"
          ),
          regexp = "Output includes two sets of bridging samples"
        ),
        regexp = paste("2 non-overlapping assays are included in the",
                       "normalized dataset without adjustment. Assays found in",
                       "only one project will have decreased statistical power",
                       "due to the lower number of samples.")
      ),
      regexp = paste("2 not bridgeable assays are included in the bridged",
                     "dataset without adjustment.")
    )

    expect_message(
      object = data_norm_bridge_all <- withCallingHandlers({
        OlinkAnalyze::olink_bridgeability_plot(
          df = data_norm,
          check_log = check_npx(df = data_norm) |>
            suppressMessages() |>
            suppressWarnings(),
          olink_id = c("OID40770_OID20117", "OID40835_OID31162",
                       "OID40981_OID30796", "OID40986_OID20052",
                       "OID41032_OID20118", "OID41054_OID20055"),
          median_counts_threshold = 150L,
          min_count = 10L
        )
      }, warning = function(w) {
        if (grepl(x = w, pattern = "not found in PostScript font database")
            || grepl(x = w, pattern = "exhibited assay QC warning"))
          invokeRestart("muffleWarning")
      }),
      regexp = "Removed 21 rows with less than 10 counts from dataset `df`!"
    )

    expect_identical(
      object = length(data_norm_bridge_all),
      expected = 6L
    )

    expect_identical(
      object = names(data_norm_bridge_all),
      expected = c("OID40770_OID20117", "OID40835_OID31162",
                   "OID40981_OID30796", "OID40986_OID20052",
                   "OID41032_OID20118", "OID41054_OID20055")
    )
  }
)

test_that(
  "olink_bridgeability_plot - works - small data",
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

    expect_message(
      object = data_norm_bridge_all <- withCallingHandlers({
        OlinkAnalyze::olink_bridgeability_plot(
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
      }),
      regexp = "Removed 2 rows with less than 10 counts from dataset `df`!"
    )

    expect_identical(
      object = length(data_norm_bridge_all),
      expected = 2L
    )

    expect_identical(
      object = names(data_norm_bridge_all),
      expected = c("OID40770", "OID40835")
    )

    expect_message(
      object = data_norm_bridge_pick1 <- withCallingHandlers({
        OlinkAnalyze::olink_bridgeability_plot(
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
      }),
      regexp = "Removed 1 row with less than 10 counts from dataset `df`!"
    )

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

    # 1 assay with three projects
    expect_error(
      object = expect_message(
        object = OlinkAnalyze::olink_bridgeability_plot(
          df = data_norm |>
            dplyr::mutate(
              Project = dplyr::if_else(
                .data[["Project"]] == "Explore HT"
                & .data[["OlinkID"]] == "OID40770"
                & grepl("^Sample_A", .data[["SampleID"]]),
                "I am a new project",
                .data[["Project"]]
              )
            ),
          check_log = check_npx(df = data_norm) |>
            suppressMessages() |>
            suppressWarnings(),
          median_counts_threshold = 150L,
          min_count = 10L
        ),
        regexp = "Removed 2 rows with less than 10 counts from dataset `df`!"
      ),
      regexp = paste("Identified 1 assay not belonging to exactly 2 projects:",
                     "\"OID40770\".")
    )

    # 1 assay with one project
    expect_error(
      object = expect_message(
        object = OlinkAnalyze::olink_bridgeability_plot(
          df = data_norm |>
            dplyr::mutate(
              Project = dplyr::if_else(
                .data[["OlinkID"]] == "OID40835",
                "Explore HT",
                .data[["Project"]]
              )
            ),
          check_log = check_npx(df = data_norm) |>
            suppressMessages() |>
            suppressWarnings(),
          median_counts_threshold = 150L,
          min_count = 10L
        ),
        regexp = "Removed 2 rows with less than 10 counts from dataset `df`!"
      ),
      regexp = paste("Identified 1 assay not belonging to exactly 2 projects:",
                     "\"OID40835\".")
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
      object = expect_message(
        object = OlinkAnalyze::olink_bridgeability_plot(
          df = data_norm |>
            dplyr::mutate(
              BridgingRecommendation = dplyr::if_else(
                .data[["Project"]] == "Explore HT"
                & .data[["OlinkID"]] == "OID40770"
                & .data[["SampleID"]] %in% c("Sample_A", "Sample_B",
                                             "Sample_C", "Sample_D"),
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
        regexp = "Removed 2 rows with less than 10 counts from dataset `df`!"
      ),
      regexp = paste("Identified 1 assay with multiple bridging",
                     "recommendations in column `BridgingRecommendation`:",
                     "\"OID40770\".")
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
      object = expect_message(
        object = OlinkAnalyze::olink_bridgeability_plot(
          df = data_norm |>
            dplyr::mutate(
              BridgingRecommendation = dplyr::if_else(
                .data[["Project"]] == "Explore HT"
                & .data[["OlinkID"]] == "OID40770"
                & .data[["SampleID"]] %in% c("Sample_A", "Sample_B",
                                             "Sample_C", "Sample_D"),
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
        regexp = "Removed 2 rows with less than 10 counts from dataset `df`!"
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
      object = expect_message(
        object = OlinkAnalyze::olink_bridgeability_plot(
          df = data_norm |>
            dplyr::bind_rows(
              data_norm |>
                dplyr::filter(
                  .data[["Project"]] == "Explore HT"
                  & .data[["OlinkID"]] == "OID40770"
                  & .data[["SampleID"]] %in% c("Sample_A", "Sample_B",
                                               "Sample_C", "Sample_D")
                )
            ),
          check_log = check_npx(df = data_norm) |>
            suppressMessages() |>
            suppressWarnings(),
          olink_id = c("OID40770", "OID40835"),
          median_counts_threshold = 150L,
          min_count = 10L
        ),
        regexp = "Removed 2 rows with less than 10 counts from dataset `df`!"
      ),
      regexp = paste("Identified 4 duplicate samples in dataset `df`:",
                     "\"Sample_A\", \"Sample_B\", \"Sample_C\", and",
                     "\"Sample_D\".")
    )
  }
)

test_that(
  "olink_bridgeability_plot - warning - OlinkID",
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

    expect_warning(
      object = expect_message(
        object = data_norm_bridge_one <- withCallingHandlers({
          OlinkAnalyze::olink_bridgeability_plot(
            df = data_norm,
            check_log = check_npx(df = data_norm) |>
              suppressMessages() |>
              suppressWarnings(),
            olink_id = c("OID40771", "OID40835"),
            median_counts_threshold = 150L,
            min_count = 10L
          )
        }, warning = function(w) {
          if (grepl(x = w, pattern = "not found in PostScript font database")
              || grepl(x = w, pattern = "exhibited assay QC warning"))
            invokeRestart("muffleWarning")
        }),
        regexp = "Removed 1 row with less than 10 counts from dataset `df`!"
      ),
      regexp = paste("1 assay in `olink_id` is not present in the dataset",
                     "`df`: \"OID40771\".")
    )

    expect_identical(
      object = length(data_norm_bridge_one),
      expected = 1L
    )

    expect_identical(
      object = names(data_norm_bridge_one),
      expected = c("OID40835")
    )
  }
)

test_that(
  "olink_bridgeability_plot - warning - NotOverlapping assays",
  {
    skip_on_cran()
    skip_if_not_installed("ggpubr")

    npx_3072 <- get_example_data(filename = "example_3k_data.rds")
    npx_ht <- get_example_data(filename = "example_HT_data.rds")

    npx_ht <- npx_ht |>
      dplyr::filter(
        .data[["SampleType"]] == "SAMPLE"
      )

    npx_3072 <- npx_3072 |>
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
          object = expect_message(
            object = expect_warning(
              data_norm <- OlinkAnalyze::olink_normalization(
                df1 = npx_ht,
                df2 = npx_3072,
                overlapping_samples_df1 = overlapping_samples,
                df1_project_nr = "Explore HT",
                df2_project_nr = "Explore 3072",
                reference_project = "Explore HT",
                # setting format = TRUE to test that function works for when
                # BridgingRecommendation is "NotOverlapping"
                format = TRUE,
                df1_check_log = check_npx(df = npx_ht) |>
                  suppressMessages() |>
                  suppressWarnings(),
                df2_check_log = check_npx(df = npx_3072) |>
                  suppressMessages() |>
                  suppressWarnings()
              ),
              regexp = "2 assays are not shared across products"
            ),
            regexp = "Cross-product normalization will be performed!"
          ),
          regexp = "Output includes two sets of bridging samples"
        ),
        regexp = paste("2 non-overlapping assays are included in the",
                       "normalized dataset without adjustment. Assays found in",
                       "only one project will have decreased statistical power",
                       "due to the lower number of samples.")
      ),
      regexp = paste("2 not bridgeable assays are included in the bridged",
                     "dataset without adjustment.")
    )

    expect_warning(
      object = expect_message(
        object = data_norm_bridge_one <- withCallingHandlers({
          OlinkAnalyze::olink_bridgeability_plot(
            df = data_norm,
            check_log = check_npx(df = data_norm) |>
              suppressMessages() |>
              suppressWarnings(),
            olink_id = c("OID12345", "OID40770_OID20117"),
            median_counts_threshold = 150L,
            min_count = 10L
          )
        }, warning = function(w) {
          if (grepl(x = w, pattern = "not found in PostScript font database")
              || grepl(x = w, pattern = "exhibited assay QC warning"))
            invokeRestart("muffleWarning")
        }),
        regexp = "Removed 7 rows with less than 10 counts from dataset `df`!"
      ),
      regexp = paste("Identified 1 assay with `BridgingRecommendation` equal",
                     "to \"NotOverlapping\": \"OID12345\".")
    )

    expect_identical(
      object = length(data_norm_bridge_one),
      expected = 1L
    )

    expect_identical(
      object = names(data_norm_bridge_one),
      expected = c("OID40770_OID20117")
    )
  }
)
