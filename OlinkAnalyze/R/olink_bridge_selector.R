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
                                  check_log) {
  # ---- STEP 1: Remove invalid OlinkIDs & control samples ---------------------

  check_log <- run_check_npx(df = df, check_log = check_log)

  df_clean <- clean_npx(
    df = df,
    check_log = check_log,
    remove_qc_warning = FALSE,
    remove_assay_warning = FALSE
  )

  check_log_clean <- check_npx(df = df_clean)
  df <- df_clean |>
    dplyr::filter(!(.data[["OlinkID"]] %in% check_log_clean$assay_na)) |>
    dplyr::filter(stringr::str_detect(.data[["OlinkID"]], "OID[0-9]{5}")) |>
    dplyr::filter(!stringr::str_detect(.data[["SampleID"]], "CONTROL_SAMPLE*"))
  # Exclude OlinkIDs with missing NPX
  # Filtering on valid OlinkID
  df <- df_clean |>
    dplyr::filter(!(.data$OlinkID %in% check_log_clean$assay_na)) |>
    dplyr::filter(stringr::str_detect(.data$OlinkID,
                                      "OID[0-9]{5}")) |>
    dplyr::filter(!stringr::str_detect(.data$SampleID, "CONTROL_SAMPLE*"))
  # ---- STEP 2: Outlier metrics per (Panel, SampleID) -------------------------
  qc_outliers <- df |>
    dplyr::group_by(.data[["Panel"]], .data[["SampleID"]]) |>
    dplyr::summarise(
      IQR = stats::IQR(.data[["NPX"]], na.rm = TRUE),
      sample_median = stats::median(.data[["NPX"]], na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::group_by(.data[["Panel"]]) |>
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
    dplyr::select(.data[["SampleID"]], .data[["Panel"]], .data[["Outlier"]])
  # ---- STEP 3: Handle LOD variations ----------------------------------------
  alt_plate_lods <- c("Plate LOD", "PlateLOD", "plateLOD", "Plate_LOD",
                      "LODNPX")
  alt_max_lods   <- c("Max LOD", "MaxLOD", "maxLOD", "Max_LOD")

  if (!("LOD" %in% names(df))) {
    if (any(alt_plate_lods %in% names(df))) {
      df <- df |> dplyr::rename(LOD = dplyr::any_of(alt_plate_lods))
      message("Using plate LOD as filter criteria...")
    } else if (any(alt_max_lods %in% names(df))) {
      df <- df |> dplyr::rename(LOD = dplyr::any_of(alt_max_lods))
      message("Using max LOD as filter criteria...")
    } else {
      df <- df |> dplyr::mutate(LOD = -Inf)
      message("LOD not available, hence not filtering by LOD.")
    }
  }

  if ("SampleQC" %in% names(df)) {
    df <- df |> dplyr::mutate(QC_Warning = .data[["SampleQC"]])
  }

  # ---- STEP 4: Sample-level QC and filtering --------------------------------

  df_1 <- df |>
    dplyr::left_join(qc_outliers, by = c("SampleID", "Panel")) |>
    dplyr::mutate(NPX = ifelse(.data[["NPX"]] <= .data[["LOD"]], NA,
                               .data[["NPX"]])) |>
    dplyr::group_by(.data[["SampleID"]]) |>
    dplyr::mutate(
      QC_Warning = dplyr::if_else(all(toupper(.data[["QC_Warning"]]) == "PASS"),
                                  "PASS", "WARNING"),
      Outliers = sum(.data[["Outlier"]]),
      PercAssaysBelowLOD = sum(is.na(.data[["NPX"]])) / dplyr::n(),
      MeanNPX = mean(.data[["NPX"]], na.rm = TRUE)
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(.data[["QC_Warning"]] == "PASS") |>
    dplyr::filter(.data[["Outliers"]] == 0) |>
    dplyr::filter(.data[["PercAssaysBelowLOD"]] < sample_missing_freq)

  df_2 <- df_1 |>
    dplyr::distinct(.data[["SampleID"]], .data[["PercAssaysBelowLOD"]],
                    .data[["MeanNPX"]])

  # ---- STEP 5: Select evenly spread bridge samples ---------------------------

  if (nrow(df_2) < n) {
    stop(
      paste0(
        "Only ", nrow(df_2), " samples eligible. Increase sample_missing_freq",
        " and/or decrease n."
      )
    )
  }

  if (nrow(df_2) == n) {
    return(df_2)
  }

  df_2 <- df_2 |>
    dplyr::arrange(dplyr::desc(.data[["MeanNPX"]])) |>
    dplyr::mutate(order = dplyr::row_number())

  bridge_samples <- floor(seq(1, nrow(df_2), length.out = n + 2)[c(-1,
                                                                   -(n + 2))])

  selected_bridges <- df_2 |>
    dplyr::filter(.data[["order"]] %in% bridge_samples) |>
    dplyr::slice_sample(prop = 1) |>     # random order
    dplyr::select(-order)

  return(selected_bridges)
}

#' @rdname olink_bridge_selector
#' @export
olink_bridgeselector <- function(df, ..., n) { # nolint: object_name_linter
  dots <- list(...)
  # Accept either spelling
  sampleMissingFreq <- dots$sampleMissingFreq %||% # nolint: object_name_linter
    dots$sample_missing_freq
  if (is.null(sampleMissingFreq)) {
    stop("Please supply either sampleMissingFreq or sample_missing_freq.")
  }
  return(
    olink_bridge_selector(
      df = df,
      sample_missing_freq = sampleMissingFreq,
      n = n
    )
  )
}
