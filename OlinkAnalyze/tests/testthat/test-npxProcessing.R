test_that(
  "npxProcessing_forDimRed - works - no dropped assays or missing assays",
  {
    # Load reference results
    reference_results <- get_example_data(filename = "reference_results.rds")

    npx_data1_uniqueid <- npx_data1 |>
      dplyr::mutate(
        SampleID = paste0(.data[["SampleID"]], "_", .data[["Index"]])
      )

    check_log <- check_npx(df = npx_data1_uniqueid) |>
      suppressMessages() |>
      suppressWarnings()

    # run npxProcessing_forDimRed
    proc_data <- npxProcessing_forDimRed(
      df = npx_data1_uniqueid,
      check_log = check_log,
      color_g = "QC_Warning",
      drop_assays = FALSE,
      drop_samples = FALSE,
      verbose = TRUE
    )

    # test df_wide
    expect_equal(
      object = proc_data$df_wide,
      expected = reference_results$preprocessing_dim_red
    )

    # test df_wide_matrix
    expect_equal(
      object = proc_data$df_wide_matrix,
      expected = reference_results$preprocessing_dim_red |>
        dplyr::select(
          -dplyr::all_of("colors")
        ) |>
        tibble::column_to_rownames(
          var = "SampleID"
        ) |>
        as.matrix()
    )

    # expect null in dropped_assays
    expect_null(proc_data$dropped_assays.na)
    expect_null(proc_data$dropped_assays.missingness)
  }
)

test_that(
  "npxProcessing_forDimRed - works - report dropped missing assays",
  {
    # Load reference results
    reference_results <- get_example_data(filename = "reference_results.rds")

    npx_data1_uniqueid <- npx_data1 |>
      dplyr::mutate(
        SampleID = paste0(.data[["SampleID"]], "_", .data[["Index"]])
      )

    samples <- sample(
      x = npx_data1_uniqueid$SampleID |> unique(),
      size = npx_data1_uniqueid$SampleID |>
        unique() |>
        length() |>
        (\(.) . * 0.15)() |>
        ceiling()
    )

    npx_data1_wth_missing <- npx_data1_uniqueid |>
      dplyr::mutate(
        # assays to be removed due to high missingness
        NPX = dplyr::if_else(
          .data[["SampleID"]] %in% .env[["samples"]] &
            .data[["OlinkID"]] %in% c("OID00482", "OID00483",
                                      "OID00484", "OID00485"),
          NA_real_,
          .data[["NPX"]]
        )
      ) |>
      # assays to be median imputed
      dplyr::mutate(
        NPX = dplyr::if_else(
          .data[["SampleID"]] %in% c("A18_19", "B8_87") &
            .data[["OlinkID"]] %in% c("OID00562", "OID01213", "OID05124"),
          NA_real_,
          .data[["NPX"]]
        )
      )

    check_log <- check_npx(df = npx_data1_wth_missing) |>
      suppressMessages() |>
      suppressWarnings()

    expect_warning(
      object = expect_warning(
        object = proc_data_wth_missing <- npxProcessing_forDimRed(
          df = npx_data1_wth_missing,
          check_log = check_log,
          color_g = "QC_Warning",
          drop_assays = FALSE,
          drop_samples = FALSE,
          verbose = TRUE
        ),
        regexp = "There are 4 assays dropped due to high missingness (>10%).",
        fixed = TRUE
      ),
      regexp = "There are 3 assays were imputed using their median values.",
      fixed = TRUE
    )

    # test df_wide
    expect_equal(
      object = proc_data_wth_missing$df_wide,
      expected = reference_results$preprocessing_dim_red_miss
    )

    # test df_wide_matrix
    expect_equal(
      object = proc_data_wth_missing$df_wide_matrix,
      expected = reference_results$preprocessing_dim_red_miss |>
        dplyr::select(
          -dplyr::all_of("colors")
        ) |>
        tibble::column_to_rownames(
          var = "SampleID"
        ) |>
        as.matrix()
    )

    # expect null in dropped_assays.na
    expect_null(object = proc_data_wth_missing$dropped_assays.na)

    # test dropped_assays.missingness
    expect_equal(
      object = proc_data_wth_missing$dropped_assays.missingness,
      expected = c("OID00482", "OID00483", "OID00484", "OID00485")
    )
  }
)

test_that(
  "npxProcessing_forDimRed - works - snapshot",
  {
    local_edition(3)

    oids_to_use <- sort(unique(npx_data1$OlinkID))[1:10L]
    sids_to_use <- sort(unique(npx_data1$SampleID))[1:10L]

    test_npx_df <- npx_data1 |>
      dplyr::filter(
        .data[["SampleID"]] %in% .env[["sids_to_use"]]
      ) |>
      dplyr::filter(
        .data[["OlinkID"]] %in% .env[["oids_to_use"]]
      ) |>
      dplyr::mutate(
        SampleID = paste0(.data[["SampleID"]], "_", .data[["Index"]])
      )

    check_log <- check_npx(test_npx_df) |>
      suppressMessages() |>
      suppressWarnings()

    expect_snapshot_value(
      npxProcessing_forDimRed(
        df = test_npx_df,
        check_log = check_log,
        color_g = "QC_Warning",
        drop_assays = FALSE,
        drop_samples = FALSE,
        verbose = TRUE
      ),
      style = "deparse"
    )
  }
)
