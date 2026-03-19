test_that(
  "olink_bridgeselector - works",
  {
    check_log <- check_npx(df = npx_data1) |>
      suppressMessages() |>
      suppressWarnings()

    expect_message(
      object = bridge_samples <- olink_bridgeselector(
        df = npx_data1,
        sampleMissingFreq = 0.1,
        n = 8L,
        check_log = check_log
      ),
      regexp = paste("No sample type column detected in the input dataset",
                     "`df`! Ensure that control samples have been filtered",
                     "out!")
    )

    expect_identical(object = dim(bridge_samples), expected = c(8L, 3L))
    expect_equal(
      object = bridge_samples |>
        dplyr::arrange(
          dplyr::desc(.data[["MeanNPX"]])
        ) |>
        dplyr::slice(3L) |>
        dplyr::pull(
          .data[["SampleID"]]
        ),
      expected = "A70"
    )
    expect_equal(
      object = bridge_samples |>
        dplyr::arrange(
          dplyr::desc(.data[["MeanNPX"]])
        ) |>
        dplyr::slice(5L) |>
        dplyr::pull(
          .data[["MeanNPX"]]
        ) |>
        round(digits = 2L),
      expected = 6.21
    )

    # check maximum number of samples ----

    expect_message(
      object = expect_equal(
        object = olink_bridgeselector(
          df = npx_data1,
          sampleMissingFreq = 0.1,
          n = 150,
          check_log = check_log
        ) |>
          nrow(),
        expected = 150L
      ),
      regexp = paste("No sample type column detected in the input dataset",
                     "`df`! Ensure that control samples have been filtered",
                     "out!")
    )
  }
)

test_that(
  "olink_bridgeselector - works - check against reference",
  {
    # Load reference results - skipped if files are absent
    reference_results <- get_example_data(filename = "reference_results.rds")

    # npx_data1 ----

    npx_df1 <- npx_data1 |>
      dplyr::filter(
        !grepl(pattern = "control",
               x = .data[["SampleID"]],
               ignore.case = TRUE
        )
      )

    expect_message(
      object = npx_df1_bridge_samples <- olink_bridgeselector(
        df = npx_df1,
        sampleMissingFreq = 0.1,
        n = 8L,
        check_log = check_npx(df = npx_df1) |>
          suppressMessages() |>
          suppressWarnings()
      ),
      regexp = paste("No sample type column detected in the input dataset",
                     "`df`! Ensure that control samples have been filtered",
                     "out!")
    )

    expect_identical(
      object = npx_df1_bridge_samples |>
        dplyr::arrange(.data[["SampleID"]]),
      expected = reference_results$bridge_samples_npx_data1 |>
        dplyr::arrange(.data[["SampleID"]])
    )

    # npx_data2 ----

    npx_df2 <- npx_data2 |>
      dplyr::filter(
        !grepl(pattern = "control",
               x = .data[["SampleID"]],
               ignore.case = TRUE
        )
      )

    expect_message(
      object = npx_df2_bridge_samples <- olink_bridgeselector(
        df = npx_df2,
        sampleMissingFreq = 0.2,
        n = 16L,
        check_log = check_npx(df = npx_df2) |>
          suppressMessages() |>
          suppressWarnings()
      ),
      regexp = paste("No sample type column detected in the input dataset",
                     "`df`! Ensure that control samples have been filtered",
                     "out!")
    )

    expect_identical(
      object = npx_df2_bridge_samples |>
        dplyr::arrange(.data[["SampleID"]]),
      expected = reference_results$bridge_samples_npx_data2 |>
        dplyr::arrange(.data[["SampleID"]])
    )
  }
)

test_that(
  "olink_bridgeselector - works - alternative column names",
  {
    expect_message(
      object = ref_data <- olink_bridgeselector(
        df = npx_data1,
        sampleMissingFreq = 0.1,
        n = 8L,
        check_log = check_npx(df = npx_data1) |>
          suppressMessages() |>
          suppressWarnings()
      ),
      regexp = paste("No sample type column detected in the input dataset",
                     "`df`! Ensure that control samples have been filtered",
                     "out!")
    )

    # Max LOD ----

    expect_message(
      object = ml_data <- olink_bridgeselector(
        df = npx_data1 |>
          dplyr::mutate(`Max LOD` = .data[["LOD"]]) |>
          dplyr::select(-dplyr::all_of("LOD")),
        sampleMissingFreq = 0.1,
        n = 8,
        check_log = npx_data1 |>
          dplyr::mutate(`Max LOD` = .data[["LOD"]]) |>
          dplyr::select(-dplyr::all_of("LOD")) |>
          check_npx() |>
          suppressMessages() |>
          suppressWarnings()
      ),
      regexp = paste("No sample type column detected in the input dataset",
                     "`df`! Ensure that control samples have been filtered",
                     "out!")
    )

    expect_length(
      object = c(setdiff(x = ref_data$SampleID, y = ml_data$SampleID),
                 setdiff(x = ml_data$SampleID, y = ref_data$SampleID)),
      n = 0L
    )

    # Plate LOD ----

    expect_message(
      object = pl_data <- olink_bridgeselector(
        df = npx_data1 |>
          dplyr::mutate(`Plate LOD` = .data[["LOD"]]) |>
          dplyr::select(-dplyr::all_of("LOD")),
        sampleMissingFreq = 0.1,
        n = 8,
        check_log = npx_data1 |>
          dplyr::mutate(`Plate LOD` = .data[["LOD"]]) |>
          dplyr::select(-dplyr::all_of("LOD")) |>
          check_npx() |>
          suppressMessages() |>
          suppressWarnings()
      ),
      regexp = paste("No sample type column detected in the input dataset",
                     "`df`! Ensure that control samples have been filtered",
                     "out!")
    )

    expect_length(
      object = c(setdiff(x = ref_data$SampleID, y = pl_data$SampleID),
                 setdiff(x = pl_data$SampleID, y = ref_data$SampleID)),
      n = 0
    )

    # Plate_LOD ----

    expect_message(
      object = p_l_data <- olink_bridgeselector(
        df = npx_data1 |>
          dplyr::mutate(Plate_LOD = .data[["LOD"]]) |>
          dplyr::select(-dplyr::all_of("LOD")),
        sampleMissingFreq = 0.1,
        n = 8,
        check_log = npx_data1 |>
          dplyr::mutate(Plate_LOD = .data[["LOD"]]) |>
          dplyr::select(-dplyr::all_of("LOD")) |>
          check_npx() |>
          suppressMessages() |>
          suppressWarnings()
      ),
      regexp = paste("No sample type column detected in the input dataset",
                     "`df`! Ensure that control samples have been filtered",
                     "out!")
    )

    expect_length(
      object = c(setdiff(x = ref_data$SampleID, y = p_l_data$SampleID),
                 setdiff(x = p_l_data$SampleID, y = ref_data$SampleID)),
      n = 0L
    )

    # LODNPX ----

    expect_message(
      object = lodnpx_data <- olink_bridgeselector(
        df = npx_data1 |>
          dplyr::mutate(LODNPX = .data[["LOD"]]) |>
          dplyr::select(-dplyr::all_of("LOD")),
        sampleMissingFreq = 0.1,
        n = 8,
        check_log = npx_data1 |>
          dplyr::mutate(LODNPX = .data[["LOD"]]) |>
          dplyr::select(-dplyr::all_of("LOD")) |>
          check_npx() |>
          suppressMessages() |>
          suppressWarnings()
      ),
      regexp = paste("No sample type column detected in the input dataset",
                     "`df`! Ensure that control samples have been filtered",
                     "out!")
    )

    testthat::expect_length(
      object = c(setdiff(x = ref_data$SampleID, y = lodnpx_data$SampleID),
                 setdiff(x = lodnpx_data$SampleID, y = ref_data$SampleID)),
      n = 0L
    )

    # PlateLOD and MaxLOD ----

    expect_message(
      object = expect_message(
        object = pl_ml_data <- olink_bridgeselector(
          df = npx_data1 |>
            dplyr::mutate(PlateLOD = .data[["LOD"]] - 0.1,
                          MaxLOD = .data[["LOD"]]) |>
            dplyr::select(-dplyr::all_of("LOD")),
          sampleMissingFreq = 0.1,
          n = 8,
          check_log = npx_data1 |>
            dplyr::mutate(PlateLOD = .data[["LOD"]] - 0.1,
                          MaxLOD = .data[["LOD"]]) |>
            dplyr::select(-dplyr::all_of("LOD")) |>
            check_npx() |>
            suppressMessages() |>
            suppressWarnings()
        ),
        regexp = paste("No sample type column detected in the input dataset",
                       "`df`! Ensure that control samples have been filtered",
                       "out!")
      ),
      regexp = paste("Multiple LOD columns detected. Will be using \"MaxLOD\"",
                     "as filter criteria."),
      fixed = TRUE
    )

    testthat::expect_length(
      object = c(setdiff(x = ref_data$SampleID, y = pl_ml_data$SampleID),
                 setdiff(x = pl_ml_data$SampleID, y = ref_data$SampleID)),
      n = 0L
    )

    # SampleQC ----

    expect_message(
      object = sampleqc_data <- olink_bridgeselector(
        df = npx_data1 |>
          dplyr::mutate(SampleQC = .data[["QC_Warning"]]) |>
          dplyr::select(-dplyr::all_of("QC_Warning")),
        sampleMissingFreq = 0.1,
        n = 8,
        check_log = npx_data1 |>
          dplyr::mutate(SampleQC = .data[["QC_Warning"]]) |>
          dplyr::select(-dplyr::all_of("QC_Warning")) |>
          check_npx() |>
          suppressMessages() |>
          suppressWarnings()
      ),
      regexp = paste("No sample type column detected in the input dataset",
                     "`df`! Ensure that control samples have been filtered",
                     "out!")
    )

    expect_length(
      object = c(setdiff(x = ref_data$SampleID, y = sampleqc_data$SampleID),
                 setdiff(x = sampleqc_data$SampleID, y = ref_data$SampleID)),
      n = 0L
    )
  }
)

test_that(
  "olink_bridgeselector - works - alternative argument names",
  {
    expect_message(
      object = ref_data <- olink_bridgeselector(
        df = npx_data1,
        sampleMissingFreq = 0.1,
        n = 8L,
        check_log = check_npx(df = npx_data1) |>
          suppressMessages() |>
          suppressWarnings()
      ),
      regexp = paste("No sample type column detected in the input dataset",
                     "`df`! Ensure that control samples have been filtered",
                     "out!")
    )

    # alt data ----

    expect_message(
      object = alt_data <- olink_bridgeselector(
        df = npx_data1,
        sample_missing_freq = 0.1,
        n = 8L,
        check_log = check_npx(df = npx_data1) |>
          suppressMessages() |>
          suppressWarnings()
      ),
      regexp = paste("No sample type column detected in the input dataset",
                     "`df`! Ensure that control samples have been filtered",
                     "out!")
    )

    expect_identical(
      object = ref_data |> dplyr::arrange(.data[["SampleID"]]),
      expected = alt_data |> dplyr::arrange(.data[["SampleID"]])
    )
  }
)

test_that(
  "olink_bridgeselector - errors",
  {
    # `sampleMissingFreq` or `sample_missing_freq` must be provided ----

    npx_data1_check_log <- check_npx(df = npx_data1) |>
      suppressMessages() |>
      suppressWarnings()

    testthat::expect_error(
      object = olink_bridgeselector(
        df = npx_data1,
        n = 8L,
        check_log = npx_data1_check_log
      ),
      regexp = paste("Please provide a value for either `sampleMissingFreq` or",
                     "`sample_missing_freq`."),
      fixed = TRUE
    )

    # too few eligible samples ----

    # Load reference results - skipped if files are absent
    npx_data_format22 <- get_example_data("npx_data_format-Oct-2022.rds")

    npx_data_format22_check_log <- check_npx(df = npx_data_format22) |>
      suppressMessages() |>
      suppressWarnings()

    testthat::expect_error(
      object = olink_bridgeselector(
        df = npx_data_format22,
        sampleMissingFreq = 0.1,
        n = 2L, check_log = npx_data_format22_check_log
      ),
      regexp = paste("Only 0 samples eligible. Increase `sample_missing_freq`",
                     "and/or decrease `n`."),
      fixed = TRUE
    )
  }
)
