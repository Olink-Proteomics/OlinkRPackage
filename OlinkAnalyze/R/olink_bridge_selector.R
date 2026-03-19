#' Bridge selection function
#'
#' @description
#' The bridge selection function will select a number of bridge samples based
#' on the input data. It selects samples with good detection that pass QC
#' and cover a good range of the data. If possible, Olink recommends 8-16
#' bridge samples. When running the selector, Olink recommends starting at
#' sample_missing_freq = 0.10 which represents a maximum of 10\% data below LOD
#' per sample. If there are not enough samples output, increase to 20\%. \cr\cr
#' The function accepts NPX Excel files with data < LOD replaced.
#'
#' @details
#' `olink_bridgeselector()` is a synonym of `olink_bridge_selector()` .
#'
#' @param df Tibble/data frame in long format such as produced by the
#' Olink Analyze read_npx function.
#' @param sample_missing_freq The threshold for sample wise missingness.
#' @param n Number of bridge samples to be selected.
#' @param check_log A named list returned by [`check_npx()`]. If `NULL`,
#' [`check_npx()`] will be run internally using `df`.
#'
#' @return A "tibble" with sample IDs and mean NPX for a defined number of
#' bridging samples. Columns include:
#' \itemize{
#'   \item{SampleID:} Sample ID
#'   \item{perc_assays_below_lod:} Percent of Assays that are below LOD for
#'   the sample
#'   \item{MeanNPX:} Mean NPX for the sample
#' }
#'
#' @aliases
#' olink_bridgeselector
#'
#' @export
#'
#' @examples
#' \donttest{
#'   check_log <- OlinkAnalyze::check_npx(df = npx_data1)
#'
#'   bridge_samples <- OlinkAnalyze::olink_bridge_selector(
#'     df = npx_data1,
#'     sample_missing_freq = 0.1,
#'     n = 20L,
#'     check_log = check_log
#'   )
#' }
#'
olink_bridge_selector <- function(df,
                                  sample_missing_freq,
                                  n,
                                  check_log = NULL) {
  # ---- STEP 0: Input checks --------------------------------------------------

  check_is_dataset(x = df, error = TRUE)
  check_is_scalar_numeric(x = sample_missing_freq, error = TRUE)
  check_is_scalar_integer(x = n, error = TRUE)

  # ---- STEP 1: Remove invalid OlinkIDs & control samples ---------------------

  check_log <- run_check_npx(df = df, check_log = check_log)

  df_clean <- clean_npx(
    df = df,
    check_log = check_log,
    remove_qc_warning = FALSE,
    remove_assay_warning = FALSE,
    verbose = FALSE
  ) |>
    suppressMessages() |>
    suppressWarnings()

  check_log_clean <- check_npx(df = df_clean) |>
    suppressMessages() |>
    suppressWarnings()

  if (!("sample_type" %in% names(check_log_clean$col_names))) {
    cli::cli_inform(
      "No sample type column detected in the input dataset {.arg df}! Ensure
      that control samples have been filtered out!"
    )
  }

  # ---- STEP 2: Outlier metrics per (Panel, SampleID) -------------------------

  qc_outliers <- df_clean |>
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(
          c(check_log_clean$col_names$panel,
            check_log_clean$col_names$sample_id)
        )
      )
    ) |>
    dplyr::summarise(
      IQR = stats::IQR(
        x = .data[[check_log_clean$col_names$quant]],
        na.rm = TRUE
      ),
      sample_median = stats::median(
        x = .data[[check_log_clean$col_names$quant]],
        na.rm = TRUE
      ),
      .groups = "drop"
    ) |>
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(check_log_clean$col_names$panel)
      )
    ) |>
    dplyr::mutate(
      median_low  = mean(.data[["sample_median"]], na.rm = TRUE) - 3 *
        stats::sd(.data[["sample_median"]], na.rm = TRUE),
      median_high = mean(.data[["sample_median"]], na.rm = TRUE) + 3 *
        stats::sd(.data[["sample_median"]], na.rm = TRUE),
      iqr_low     = mean(.data[["IQR"]], na.rm = TRUE) - 3 *
        stats::sd(.data[["IQR"]], na.rm = TRUE),
      iqr_high    = mean(.data[["IQR"]], na.rm = TRUE) + 3 *
        stats::sd(.data[["IQR"]], na.rm = TRUE)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      Outlier = dplyr::if_else(
        (.data[["sample_median"]] < .data[["median_high"]]) &
          (.data[["sample_median"]] > .data[["median_low"]])  &
          (.data[["IQR"]] > .data[["iqr_low"]]) &
          (.data[["IQR"]] < .data[["iqr_high"]]),
        0, 1
      )
    ) |>
    dplyr::select(
      dplyr::all_of(
        c(check_log_clean$col_names$sample_id,
          check_log_clean$col_names$panel,
          "Outlier")
      )
    )

  # ---- STEP 3: Handle LOD variations ----------------------------------------

  if (!("lod" %in% names(check_log_clean$col_names))) {
    df_clean <- df_clean |>
      dplyr::mutate(
        LOD = -Inf
      )
    check_log_clean$col_names[["lod"]] <- "LOD"

    cli::cli_inform(
      "LOD not available, hence not filtering by LOD."
    )
  } else if (length(check_log_clean$col_names$lod) > 1L) {

    check_log_clean$col_names$lod <- check_log_clean$col_names$lod |>
      unique() |>
      sort() |>
      head(n = 1L)

    cli::cli_inform(
      "Multiple LOD columns detected. Will be using
      {.val {check_log_clean$col_names$lod}} as filter criteria."
    )
  }

  # ---- STEP 4: Sample-level QC and filtering --------------------------------

  df_ready <- df_clean |>
    dplyr::left_join(
      qc_outliers,
      by = c(check_log_clean$col_names$sample_id,
             check_log_clean$col_names$panel),
      relationship = "many-to-one"
    ) |>
    dplyr::mutate(
      quant_na = dplyr::if_else(
        .data[[check_log_clean$col_names$quant]] <=
          .data[[check_log_clean$col_names$lod]],
        NA_real_,
        .data[[check_log_clean$col_names$quant]]
      )
    ) |>
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(check_log_clean$col_names$sample_id)
      )
    ) |>
    dplyr::mutate(
      qc_warn = dplyr::if_else(
        all(toupper(.data[[check_log_clean$col_names$qc_warning]]) == "PASS"),
        "PASS",
        "WARNING"
      ),
      outliers = sum(.data[["Outlier"]], na.rm = TRUE),
      perc_assays_below_lod = sum(is.na(.data[["quant_na"]])) / dplyr::n(),
      MeanNPX = mean(x = .data[["quant_na"]], na.rm = TRUE)
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(
      .data[["qc_warn"]] == "PASS" &
        .data[["outliers"]] == 0L &
        .data[["perc_assays_below_lod"]] < .env[["sample_missing_freq"]]
    ) |>
    dplyr::distinct(
      .data[[check_log_clean$col_names$sample_id]],
      .data[["perc_assays_below_lod"]],
      .data[["MeanNPX"]]
    ) |>
    dplyr::rename(
      "SampleID" = !!check_log_clean$col_names$sample_id
    )

  # ---- STEP 5: Select evenly spread bridge samples ---------------------------

  # if fewer available samples than required by user
  if (nrow(df_ready) < n) {
    cli::cli_abort(
      c(
        "x" = "Only {.val {nrow(df_ready)}} samples eligible. Increase
        {.arg sample_missing_freq} and/or decrease {.arg n}."
      ),
      call = rlang::caller_env(),
      wrap = TRUE
    )
  }

  # if available exactly as many samples as required by user
  if (nrow(df_ready) == n) {
    return(df_ready)
  }

  # if more available samples than required by user, select n samples that are
  # evenly spread across the range of mean NPX values.
  df_ready <- df_ready |>
    dplyr::arrange(
      dplyr::desc(
        .data[["MeanNPX"]]
      )
    ) |>
    dplyr::mutate(
      order = dplyr::row_number()
    )

  bridge_samples <- floor(
    x = seq(
      from = 1L,
      to = nrow(df_ready),
      length.out = n + 2L)[c(-1L, -(n + 2L))]
  )

  selected_bridges <- df_ready |>
    dplyr::filter(
      .data[["order"]] %in% .env[["bridge_samples"]]
    ) |>
    # random order
    dplyr::slice_sample(
      prop = 1
    ) |>
    dplyr::select(
      -dplyr::all_of("order")
    )

  return(selected_bridges)
}

#' @rdname olink_bridge_selector
#' @export
olink_bridgeselector <- function(df, ..., n) { # nolint: object_name_linter

  # Accept either spelling for sample_missing_freq for backward compatibility
  dots <- list(...)
  if (is.null(dots$sample_missing_freq)) {
    sampleMissingFreq <- dots$sampleMissingFreq # nolint: object_name_linter
  } else {
    sampleMissingFreq <- dots$sample_missing_freq # nolint: object_name_linter
  }

  if (is.null(sampleMissingFreq)) {
    cli::cli_abort(
      c(
        "x" = "Please provide a value for either {.arg sampleMissingFreq} or
        {.arg sample_missing_freq}."
      ),
      call = rlang::caller_env(),
      wrap = TRUE
    )
  }

  return(
    olink_bridge_selector(
      df = df,
      sample_missing_freq = sampleMissingFreq,
      n = n,
      check_log = check_log
    )
  )
}
