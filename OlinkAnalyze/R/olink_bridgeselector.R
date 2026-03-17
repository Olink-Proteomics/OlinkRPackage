#' Bridge selection function
#'
#' The bridge selection function will select a number of bridge samples based
#' on the input data. It selects samples with 'good detection, which passes QC
#' and cover a good range of the data. If possible, Olink recommends 8-16
#' bridge samples. 'When running the selector, Olink recommends starting at
#' sample_missing_freq = 0.10 which represents a maximum of 10\% data below LOD
#' per sample. If there are not enough samples output, increase to 20\%. \cr\cr
#' The function accepts NPX Excel files with data < LOD replaced.
#'
#' @param df Tibble/data frame in long format such as produced by the
#' Olink Analyze read_NPX function.
#' @param sample_missing_freq The threshold for sample wise missingness.
#' @param n Number of bridge samples to be selected.
#'
#' @return A "tibble" with sample IDs and mean NPX for a defined number of
#' bridging samples. Columns include:
#'
#' \itemize{
#'   \item{SampleID:} Sample ID
#'   \item{perc_assays_below_lod:} Percent of Assays that are below LOD for
#'   the sample
#'   \item{MeanNPX:} Mean NPX for the sample
#' }
#' @export
#'
#' @examples
#' \donttest{bridge_samples <- olink_bridgeselector(npx_data1,
#' sample_missing_freq = 0.1, n = 20)}
#' @importFrom dplyr n select distinct arrange group_by mutate ungroup
#' left_join filter if_else
#' @importFrom stringr str_detect

olink_bridgeselector <- function(df, sample_missing_freq, n) {
  # Check NPX data
  check_log <- OlinkAnalyze::check_npx(df = df)
  # Clean NPX data
  df_clean <- OlinkAnalyze::clean_npx(
    df = df,
    check_log = check_log
  )
  # Re-check NPX data
  check_log_clean <- OlinkAnalyze::check_npx(df = df_clean)
  # Exclude OlinkIDs with missing NPX
  # Filtering on valid OlinkID
  df <- df |>
    dplyr::filter(!(.data$OlinkID %in% check_log_clean$assay_na)) |>
    dplyr::filter(stringr::str_detect(.data$OlinkID,
                                      "OID[0-9]{5}"))
  #Filtering out control samples
  df <- df |>
    dplyr::filter(!stringr::str_detect(.data$SampleID, "CONTROL_SAMPLE*"))
  #Outlier calculation as in qc_plot for filtering
  qc_outliers <- df |>
    dplyr::group_by(.data$Panel, .data$SampleID) |>
    dplyr::mutate(IQR = IQR(.data$NPX, na.rm = TRUE),
                  sample_median = median(.data$NPX, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::select(.data$SampleID, .data$Panel, .data$IQR,
                  .data$sample_median) |>
    dplyr::distinct() |>
    dplyr::group_by(.data$Panel) |>
    dplyr::mutate(median_low = mean(.data$sample_median, na.rm = TRUE) - 3 *
                    sd(.data$sample_median, na.rm = TRUE),
                  median_high = mean(.data$sample_median, na.rm = TRUE) + 3 *
                    sd(.data$sample_median, na.rm = TRUE),
                  iqr_low = mean(.data$IQR, na.rm = TRUE) - 3 *
                    sd(.data$IQR, na.rm = TRUE),
                  iqr_high = mean(.data$IQR, na.rm = TRUE) + 3 *
                    sd(.data$IQR, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::mutate(Outlier = dplyr::if_else(.data$sample_median <
                                             .data$median_high &
                                             .data$sample_median >
                                             .data$median_low &
                                             .data$IQR > .data$iqr_low &
                                             .data$IQR < .data$iqr_high,
                                           0, 1)) |>
    dplyr::select(.data$SampleID, .data$Panel, .data$Outlier)
  # Alternative LODs for when LOD is not present
  alt_plate_lods <- c("Plate LOD", "PlateLOD", "plateLOD", "Plate_LOD")
  alt_max_lods <- c("Max LOD", "MaxLOD", "maxLOD", "Max_LOD")
  if (!("LOD" %in% names(df))) {
    if (any(alt_plate_lods %in% names(df))) {
      df <- df |>
        dplyr::rename(LOD = dplyr::any_of(alt_plate_lods))
      message("Using plate LOD as filter criteria...")
    } else if (any(alt_max_lods %in% names(df))) {
      df <- df |>
        dplyr::rename(LOD = dplyr::any_of(alt_max_lods))
      message("Using max LOD as filter criteria...")
    } else {
      df <- df |>
        dplyr::mutate(LOD = -Inf)
      message("LOD not available. No filtering by LOD...")
    }
  }
  if ("SampleQC" %in% names(df)) {
    df <- df |>
      dplyr::mutate(QC_Warning = .data$SampleQC)
  }
  df_1 <- df |>
    dplyr::left_join(qc_outliers, by = c("SampleID", "Panel")) |>
    dplyr::mutate(NPX = ifelse(.data$NPX <= .data$LOD, NA, .data$NPX)) |>
    dplyr::group_by(.data$SampleID) |>
    dplyr::mutate(.data$QC_Warning <-
                    dplyr::if_else(all(toupper(.data$QC_Warning) ==
                                         "PASS"), "PASS",
                                   "WARNING")) |>
    dplyr::filter(.data$QC_Warning == "PASS") |>
    dplyr::mutate(.data$Outliers <-
                    sum(.data$Outlier)) |>
    dplyr::filter(.data$Outliers == 0) |>
    dplyr::mutate(.data$perc_assays_below_lod <-
                    sum(is.na(.data$NPX)) / dplyr::n()) |>
    dplyr::mutate(MeanNPX = mean(.data$NPX, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::filter(.data$perc_assays_below_lod < .data$sample_missing_freq)
  df_2 <- df_1 |>
    dplyr::select(.data$SampleID, .data$perc_assays_below_lod, .data$MeanNPX) |>
    dplyr::distinct()
  if (nrow(df_2) < n) {
    stop(paste0("With the current settings only ",
                nrow(df_2),
                " samples can be selected. Please increase sample_missing_freq
                and/or decrease n."))
  } else if (nrow(df_2) == n) {
    # if samples satisfying the criteria equal the number of requested samples
    # return all of them
    selected_bridges <- df_2
  } else { #
    df_2  <- df_2  |>
      dplyr::arrange(dplyr::desc(.data$MeanNPX)) |>
      dplyr::mutate(order = c(seq_len(nrow(.data))))
    bridge_samples <- floor(seq(1, nrow(df_2), length.out = n + 2)
                            [c(-1, -(n + 2))])
    selected_bridges <- df_2 |>
      dplyr::filter(order %in% bridge_samples) |>
      dplyr::slice_sample(n = nrow(.data)) |>
      dplyr::select(-order)
  }
  return(selected_bridges)
}
