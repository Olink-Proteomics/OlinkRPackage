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
#'   \item{PercAssaysBelowLOD:} Percent of Assays that are below LOD for
#'   the sample
#'   \item{MeanNPX:} Mean NPX for the sample
#' }
#' @export
#'
#' @examples
#' \donttest{bridge_samples <- olink_bridgeselector(npx_data1,
#' sample_missing_freq = 0.1, n = 20)}
#' @importFrom magrittr |>
#' @importFrom dplyr n select distinct arrange group_by mutate ungroup
#' left_join filter if_else
#' @importFrom stringr str_detect

olink_bridgeselector <- function(df, sample_missing_freq, n) {
  # Exclude OlinkIDs with missing NPX
  # Check NPX data
  check_log <- OlinkAnalyze::check_npx(df = df)
  # Clean NPX data
  df_clean <- OlinkAnalyze::clean_npx(
    df = df,
    check_log = check_log
  )
  # Re-check NPX data
  check_log_clean <- OlinkAnalyze::check_npx(df = df_clean)
  #Filtering on valid OlinkID
  df <- df |>
    dplyr::filter(!(OlinkID %in% check_log_clean$assay_na)) |>
    dplyr::filter(stringr::str_detect(OlinkID,
                                      "OID[0-9]{5}"))
  #Filtering out control samples
  df <- df |>
    dplyr::filter(!stringr::str_detect(SampleID, "CONTROL_SAMPLE*"))
  #Outlier calculation as in qc_plot for filtering
  qc_outliers <- df |>
    dplyr::group_by(Panel, SampleID) |>
    dplyr::mutate(IQR = IQR(NPX, na.rm = TRUE),
                  sample_median = median(NPX, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::select(SampleID, Panel, IQR, sample_median) |>
    dplyr::distinct() |>
    dplyr::group_by(Panel) |>
    dplyr::mutate(median_low = mean(sample_median, na.rm = TRUE) - 3 *
                    sd(sample_median, na.rm = TRUE),
                  median_high = mean(sample_median, na.rm = TRUE) + 3 *
                    sd(sample_median, na.rm = TRUE),
                  iqr_low = mean(IQR, na.rm = TRUE) - 3 *
                    sd(IQR, na.rm = TRUE),
                  iqr_high = mean(IQR, na.rm = TRUE) + 3 *
                    sd(IQR, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::mutate(Outlier = dplyr::if_else(sample_median < median_high &
                                             sample_median > median_low &
                                             IQR > iqr_low &
                                             IQR < iqr_high,
                                           0, 1)) |>
    dplyr::select(SampleID, Panel, Outlier)
  # Alternative LODs for when LOD is not present
  alt_plate_lods <- c("Plate LOD", "PlateLOD", "plateLOD", "Plate_LOD")
  alt_max_lods <- c("Max LOD", "MaxLOD", "maxLOD", "Max_LOD")
  if (!("LOD" %in% names(df))) {
    if (any(alt_plate_lods %in% names(df))) {
      df <- df |>
        dplyr::rename(LOD = any_of(alt_plate_lods))
      message("Using plate LOD as filter criteria...")
    } else if (any(alt_max_lods %in% names(df))) {
      df <- df |>
        dplyr::rename(LOD = any_of(alt_max_lods))
      message("Using max LOD as filter criteria...")
    } else {
      df <- df |>
        dplyr::mutate(LOD = -Inf)
      message("LOD not available. No filtering by LOD...")
    }
  }
  if ("SampleQC" %in% names(df)) {
    df <- df |>
      dplyr::mutate(QC_Warning = SampleQC)
  }
  df_1 <- df |>
    dplyr::left_join(qc_outliers, by = c("SampleID", "Panel")) |>
    dplyr::mutate(NPX = ifelse(NPX <= LOD, NA, NPX)) |>
    dplyr::group_by(SampleID) |>
    dplyr::mutate(QC_Warning = dplyr::if_else(all(toupper(QC_Warning) ==
                                                    "PASS"), "PASS",
                                              "WARNING")) |>
    dplyr::filter(QC_Warning == "PASS") |>
    dplyr::mutate(Outliers = sum(Outlier)) |>
    dplyr::filter(Outliers == 0) |>
    dplyr::mutate(PercAssaysBelowLOD = sum(is.na(NPX)) / dplyr::n()) |>
    dplyr::mutate(MeanNPX = mean(NPX, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::filter(PercAssaysBelowLOD < sample_missing_freq)
  df_2 <- df_1 |>
    dplyr::select(SampleID, PercAssaysBelowLOD, MeanNPX) |>
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
      dplyr::arrange(desc(MeanNPX)) |>
      dplyr::mutate(order = c(seq_len(nrow(.))))
    bridge_samples <- floor(seq(1, nrow(df_2), length.out = n + 2)
                            [c(-1, -(n + 2))])
    selected_bridges <- df_2 |>
      dplyr::filter(order %in% bridge_samples) |>
      dplyr::slice_sample(n = nrow(.)) |>
      dplyr::select(-order)
  }
  return(selected_bridges)
}
