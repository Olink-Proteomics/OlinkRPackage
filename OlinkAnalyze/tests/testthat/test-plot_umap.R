test_that(
  "olink_umap_plot - works - snapshot",
  {
    skip_if_not_installed("umap")
    skip_if_not_installed("ggrepel")
    skip_if_not_installed("ggpubr")
    skip_if_not_installed("vdiffr")

    withr::local_seed(123)

    cfg <- umap::umap.defaults
    cfg$random_state <- 123

    npx_df <- npx_data1 |>
      dplyr::filter(
        !grepl(pattern = "control",
               x = .data[["SampleID"]],
               ignore.case = TRUE)
      )

    expect_no_error(
      object = expect_no_warning(
        object = expect_no_message(
          object = check_log <- check_npx(df = npx_df)
        )
      )
    )

    expect_no_error(
      object = expect_no_warning(
        object = expect_no_message(
          object = umap_plot <- olink_umap_plot(
            df = npx_df,
            color_g = "QC_Warning",
            quiet = TRUE,
            config = cfg,
            check_log = check_log
          )
        )
      )
    )

    umap_plot_name <- "umap_plot_QC_warning"
    check_snap_exist(test_dir_name = "plot_umap", snap_name = umap_plot_name)
    vdiffr::expect_doppelganger(umap_plot_name, umap_plot[[1L]]) |>
      suppressMessages() |>
      suppressWarnings()
  }
)

test_that(
  "olink_umap_plot - works - clusters are there and outlier group is detected",
  {
    skip_if_not_installed("umap")
    skip_if_not_installed("ggrepel")
    skip_if_not_installed("ggpubr")

    out_group <- c("A1_1", "A10_11", "A12_13", "A17_18", "A25_27",
                   "A69_71", "B2_81", "B31_112", "B66_147", "B71_152")

    umap_data <- dat <- npx_data1 |>
      dplyr::mutate(
        SampleID = paste0(.data[["SampleID"]], "_", .data[["Index"]])
      ) |>
      dplyr::mutate(
        NPX = dplyr::if_else(
          .data[["SampleID"]] %in% .env[["out_group"]],
          .data[["NPX"]] + 5,
          .data[["NPX"]]
        ),
        group = dplyr::if_else(
          .data[["SampleID"]] %in% .env[["out_group"]],
          "A",
          "B"
        )
      )

    check_log <- check_npx(df = umap_data) |>
      suppressWarnings() |>
      suppressMessages()

    # Run UMAP
    expect_no_error(
      object = expect_no_warning(
        object = expect_no_message(
          object = umap_plot <- olink_umap_plot(
            df = umap_data,
            color_g = "group",
            check_log = check_log,
            quiet = TRUE
          )
        )
      )
    )

    # cluster data
    umap_plot_clusters <- kmeans(
      x = umap_plot[[1L]]$data,
      centers = 2L
    )

    # Get the samples in the smallest cluster
    cluster_sizes <- table(umap_plot_clusters$cluster)
    min_cluster <- as.numeric(names(cluster_sizes)[which.min(cluster_sizes)])
    out_group_detected <- umap_plot_clusters$cluster[
      umap_plot_clusters$cluster == min_cluster
    ] |>
      names()

    expect_identical(
      object = sort(out_group_detected),
      expected = sort(out_group)
    )
  }
)

test_that(
  "olink_umap_plot - warnings",
  {
    skip_if_not_installed("umap")
    skip_if_not_installed("ggrepel")
    skip_if_not_installed("ggpubr")

    # Two Warnings thrown: for dropped assays and droppes samples
    expect_warning(
      object = expect_warning(
        object = umap_plot_drop <- npx_data1 |>
          dplyr::mutate(
            SampleID = paste0(.data[["SampleID"]], "_", .data[["Index"]])
          ) |>
          olink_umap_plot(
            drop_assays = TRUE,
            drop_samples = TRUE,
            check_log = check_npx(df = npx_data1) |>
              suppressMessages() |>
              suppressWarnings(),
            quiet = TRUE
          ),
        regexp = "0 assays contain NA and are dropped"
      ),
      regexp = "160 samples contain NA and are dropped"
    )

    # edge case of dataset with all NPX=NA for some assays
    dt_edge_case <- get_example_data(filename = "npx_data_format-Oct-2022.rds")

    expect_warning(
      object = expect_message(
        object = expect_message(
          object = olink_umap_plot(
            df = dt_edge_case,
            quiet = TRUE
          ),
          regexp = "`check_log` not provided. Running `check_npx()`",
          fixed = TRUE
        ),
        regexp = paste("8 assays exhibited assay QC warnings in column",
                       "`Assay_Warning` of the dataset")
      ),
      regexp = paste("\"OID30136\", \"OID30144\", \"OID30166\", \"OID30168\",",
                     "\"OID30438\", \"OID30544\", \"OID30626\", \"OID30695\",",
                     "\"OID30748\", \"OID30866\", \"OID30899\", \"OID31054\",",
                     "\"OID31113\", \"OID31186\", \"OID31225\", \"OID31309\",",
                     "and \"OID31325\" have \"NPX\" = NA for all samples")
    )
  }
)

test_that(
  "olink_qc_plot - works - OSI snapshots",
  {
    skip_if_not_installed("ggrepel")
    skip_if_not_installed("ggpubr")
    skip_if_not_installed("umap")
    skip_if_not_installed("vdiffr")

    withr::local_seed(123)

    cfg <- umap::umap.defaults
    cfg$random_state <- 123

    osi_data <- get_example_data("example_osi_data.rds")

    check_log <- check_npx(df = osi_data) |>
      suppressMessages() |>
      suppressWarnings()

    expect_no_error(
      object = expect_no_warning(
        object = expect_no_message(
          object = umap_osi_cat <- olink_umap_plot(
            df = osi_data,
            color_g = "OSICategory",
            check_log = check_log,
            config = cfg,
            quiet = TRUE
          )
        )
      )
    )
    umap_osi_cat_name <- "umap_plot_osi_category"
    # check_snap_exist(test_dir_name = "plot_umap",
    # snap_name = umap_osi_cat_name)
    vdiffr::expect_doppelganger(umap_osi_cat_name, umap_osi_cat[[1L]]) |>
      suppressMessages() |>
      suppressWarnings()

    expect_no_error(
      object = expect_no_warning(
        object = expect_no_message(
          object = umap_osi_sum <- olink_umap_plot(
            df = osi_data,
            color_g = "OSISummary",
            check_log = check_log,
            config = cfg,
            quiet = TRUE
          )
        )
      )
    )
    umap_osi_sum_name <- "umap_plot_osi_summary"
    # check_snap_exist(test_dir_name = "plot_umap",
    # snap_name = umap_osi_sum_name)
    vdiffr::expect_doppelganger(umap_osi_sum_name, umap_osi_sum[[1L]]) |>
      suppressMessages() |>
      suppressWarnings()

    expect_no_error(
      object = expect_no_warning(
        object = expect_no_message(
          object = umap_osi_temp <- olink_umap_plot(
            df = osi_data,
            color_g = "OSIPreparationTemperature",
            check_log = check_log,
            config = cfg,
            quiet = TRUE
          )
        )
      )
    )
    umap_osi_temp_name <- "umap_plot_osi_temperature"
    # check_snap_exist(test_dir_name = "plot_umap",
    # snap_name = umap_osi_temp_name)
    vdiffr::expect_doppelganger(umap_osi_temp_name, umap_osi_temp[[1L]]) |>
      suppressMessages() |>
      suppressWarnings()

    expect_no_error(
      object = expect_no_warning(
        object = expect_no_message(
          object = umap_osi_centr <- olink_umap_plot(
            df = osi_data,
            color_g = "OSITimeToCentrifugation",
            check_log = check_log,
            config = cfg,
            quiet = TRUE
          )
        )
      )
    )
    umap_osi_centr_name <- "umap_plot_osi_centrifugation"
    # check_snap_exist(test_dir_name = "plot_umap",
    # snap_name = umap_osi_centr_name)
    vdiffr::expect_doppelganger(umap_osi_centr_name, umap_osi_centr[[1L]]) |>
      suppressMessages() |>
      suppressWarnings()
  }
)

test_that(
  "olink_qc_plot - works - OSI edge cases",
  {
    skip_if_not_installed("ggrepel")
    skip_if_not_installed("ggpubr")
    skip_if_not_installed("umap")

    osi_data <- get_example_data("example_osi_data.rds")

    # ----------------------------
    # OSICategory all NA
    # ----------------------------
    df_cat_all_na <- osi_data |>
      dplyr::mutate(OSICategory = NA)

    expect_error(
      object = olink_qc_plot(
        df = df_cat_all_na,
        color_g = "OSICategory",
        check_log = check_npx(df = df_cat_all_na) |>
          suppressMessages() |>
          suppressWarnings()
      ),
      regexp = paste("All values are 'NA' in the column \"OSICategory\" of the",
                     "dataset `df`!")
    )

    # ----------------------------
    # Continuous OSI column all NA
    # ----------------------------
    df_cont_all_na <- osi_data |>
      dplyr::mutate(OSISummary = NA)

    expect_error(
      object = olink_qc_plot(
        df = df_cont_all_na,
        color_g = "OSISummary",
        check_log = check_npx(df = df_cont_all_na) |>
          suppressMessages() |>
          suppressWarnings()
      ),
      regexp = paste("All values are 'NA' in the column \"OSISummary\" of the",
                     "dataset `df`!")
    )
  }
)
