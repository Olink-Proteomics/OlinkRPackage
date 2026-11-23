test_that("npxProcessing_forDimRed - works - no dropped assays or missing assays", {
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
})

test_that("npxProcessing_forDimRed - works - report dropped missing assays", {
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
        .data[["SampleID"]] %in%
          .env[["samples"]] &
          .data[["OlinkID"]] %in%
            c("OID00482", "OID00483", "OID00484", "OID00485"),
        NA_real_,
        .data[["NPX"]]
      )
    ) |>
    # assays to be median imputed
    dplyr::mutate(
      NPX = dplyr::if_else(
        .data[["SampleID"]] %in%
          c("A18_19", "B8_87") &
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
      regexp = "4 assays were dropped due to high missingness (>10%).",
      fixed = TRUE
    ),
    regexp = "3 assays were imputed using their median values.",
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
})

test_that("npxProcessing_forDimRed - works - snapshot", {
  skip_if_not(file.exists(test_path("_snaps", "npxProcessing.md")))

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
})

test_that("npxProcessing_forDimRed - works - olink_class and arrow", {
  local_edition(3)

  oids_to_use <- sort(unique(npx_data1$OlinkID))[1:10L]
  sids_to_use <- sort(unique(npx_data1$SampleID))[1:10L]

  # tibble ----

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

  expect_no_error(
    object = expect_no_warning(
      object = expect_no_message(
        object = npx_proc_tibble <- npxProcessing_forDimRed(
          df = test_npx_df,
          check_log = check_log,
          color_g = "QC_Warning",
          drop_assays = FALSE,
          drop_samples = FALSE,
          verbose = TRUE
        )
      )
    )
  )

  # olink_class ----

  test_npx_df_obj <- attach_check_log(
    df = test_npx_df,
    out_df = "tibble"
  )

  expect_no_error(
    object = expect_no_warning(
      object = expect_no_message(
        object = npx_proc_obj <- npxProcessing_forDimRed(
          df = test_npx_df_obj,
          color_g = "QC_Warning",
          drop_assays = FALSE,
          drop_samples = FALSE,
          verbose = TRUE
        )
      )
    )
  )

  expect_equal(
    object = npx_proc_obj,
    expected = npx_proc_tibble
  )

  # arrow ----

  test_npx_df_arrow <- attach_check_log(
    df = test_npx_df,
    out_df = "arrow"
  )

  expect_no_error(
    object = expect_no_warning(
      object = expect_no_message(
        object = npx_proc_arrow <- npxProcessing_forDimRed(
          df = test_npx_df_arrow,
          color_g = "QC_Warning",
          drop_assays = FALSE,
          drop_samples = FALSE,
          verbose = TRUE
        )
      )
    )
  )

  expect_equal(
    object = npx_proc_arrow,
    expected = npx_proc_tibble
  )
})

test_that("dimension reduction applies cutoff after sample removal", {
  assays <- npx_data1 |>
    dplyr::distinct(.data[["OlinkID"]], .keep_all = TRUE) |>
    dplyr::slice_head(n = 4L)
  make_input <- function(n) {
    df <- assays[rep(seq_len(nrow(assays)), each = n), ]
    df$SampleID <- rep(sprintf("sample%03d", seq_len(n)), times = nrow(assays))
    df$NPX <- rep(seq_len(n), times = nrow(assays)) * 1.0
    df$NPX[
      df$OlinkID == assays$OlinkID[1L] &
        df$SampleID %in% sprintf("sample%03d", 1:5)
    ] <- NA_real_
    df
  }
  for (n in c(88L, 89L)) {
    df <- make_input(n)
    log <- suppressWarnings(suppressMessages(check_npx(df)))
    result <- npxProcessing_forDimRed(
      df = df,
      check_log = log,
      drop_assays = FALSE,
      drop_samples = FALSE
    )
    if (n == 88L) {
      expect_identical(result$dropped_assays.missingness, assays$OlinkID[1L])
    } else {
      expect_null(result$dropped_assays.missingness)
      expect_false(anyNA(result$df_wide_matrix))
    }
  }

  df <- make_input(89L)
  log <- suppressWarnings(suppressMessages(check_npx(df)))
  result <- npxProcessing_forDimRed(
    df = df,
    check_log = log,
    drop_samples = TRUE
  )
  expect_equal(nrow(result$df_wide), 84L)
  expect_null(result$dropped_assays.missingness)
  expect_true(assays$OlinkID[1L] %in% names(result$df_wide))
})

test_that("dimension reduction retains drop checks and warning order", {
  assays <- npx_data1 |>
    dplyr::distinct(.data[["OlinkID"]], .keep_all = TRUE) |>
    dplyr::slice_head(n = 4L)
  df <- assays[rep(seq_len(nrow(assays)), each = 4L), ]
  df$SampleID <- rep(paste0("sample", 1:4), times = nrow(assays))
  df$NPX <- rep(1:4, times = nrow(assays)) * 1.0
  df$NPX[
    df$OlinkID %in% assays$OlinkID[1:2] & df$SampleID == "sample1"
  ] <- NA_real_
  log <- suppressWarnings(suppressMessages(check_npx(df)))
  expect_warning(
    expect_error(
      npxProcessing_forDimRed(
        df,
        check_log = log,
        drop_assays = TRUE,
        verbose = TRUE
      ),
      "Too many assays were removed"
    ),
    "2 assays contain NA"
  )
})
test_that("dimension reduction reports assay and sample drops in order", {
  assays <- npx_data1 |>
    dplyr::distinct(.data[["OlinkID"]], .keep_all = TRUE) |>
    dplyr::slice_head(n = 4L)
  df <- assays[rep(seq_len(nrow(assays)), each = 4L), ]
  df$SampleID <- rep(paste0("sample", 1:4), times = nrow(assays))
  df$NPX <- rep(1:4, times = nrow(assays)) * 1.0
  df$NPX[
    df$OlinkID == assays$OlinkID[1L] & df$SampleID == "sample1"
  ] <- NA_real_
  log <- suppressWarnings(suppressMessages(check_npx(df)))
  expect_warning(
    expect_warning(
      result <- npxProcessing_forDimRed(
        df,
        check_log = log,
        drop_assays = TRUE,
        drop_samples = TRUE,
        verbose = TRUE
      ),
      "4 samples contain NA"
    ),
    "1 assay contain NA"
  )
  expect_identical(result$dropped_assays.na, assays$OlinkID[1L])
  expect_equal(nrow(result$df_wide), 4L)
})
test_that("dimension reduction checks minimum samples after dropping", {
  assays <- npx_data1 |>
    dplyr::distinct(.data[["OlinkID"]], .keep_all = TRUE) |>
    dplyr::slice_head(n = 4L)
  df <- assays[rep(seq_len(nrow(assays)), each = 4L), ]
  df$SampleID <- rep(paste0("sample", 1:4), times = nrow(assays))
  df$NPX <- rep(1:4, times = nrow(assays)) * 1.0
  df$NPX[
    df$OlinkID == assays$OlinkID[1L] &
      df$SampleID %in% c("sample1", "sample2")
  ] <- NA_real_
  df$NPX[
    df$OlinkID == assays$OlinkID[2L] & df$SampleID == "sample3"
  ] <- NA_real_
  log <- suppressWarnings(suppressMessages(check_npx(df)))
  expect_warning(
    expect_error(
      npxProcessing_forDimRed(
        df,
        check_log = log,
        drop_samples = TRUE,
        verbose = TRUE
      ),
      "Too many samples were removed"
    ),
    "4 samples contain NA"
  )
})
