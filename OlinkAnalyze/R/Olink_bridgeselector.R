#' Bridge selection function
#'
#'The bridge selection function will select a number of bridge samples based on the input data. It select samples with
#'good detection, which passes QC and cover a good range of the data. If possible olink recommends 8-16 bridge samples.
#'Bridge sample selection strategy: Output 2x (number of bridging samples) for final selection (by customer).
#'When running the selector, start at 10\% missingness. If there are not enough samples output, increase to 20\%.
#'If still not enough, consider outputting e.g. 1.5x (number of bridging samples) for final selection. \cr\cr
#'The function accepts NPX Excel files with data < LOD replaced.
#'
#' @param df Tibble/data frame in long format such as produced by the olinkr read_NPX function.
#' @param sampleMissingFreq The threshold for sample wise missingness.
#' @param n Number of bridge samples to be selected.
#' @param warning_string The string used to indicate that a sample received a QC warning. Most common, it is either "WARN" or "Warning". If this is not given, the function tries to detect it from the given data frame.
#'
#' @return Tibble with sample ID:s and mean NPX for a defined number of bridging samples.
#' @export
#'
#' @examples
#' \donttest{bridge_samples <- olink_bridgeselector(npx_data1, sampleMissingFreq = 0.1, n = 20)}
#' @importFrom magrittr %>%
#' @importFrom dplyr select distinct arrange group_by mutate ungroup left_join filter
#' @importFrom stringr str_detect

olink_bridgeselector<-function(df, sampleMissingFreq, n, warning_string = NULL){


  # Find the string used to indicate a flagged sample
  if (is.null(warning_string)) {
    QC_Warning_strings <- (df %>%
                             dplyr::select(QC_Warning) %>%
                             dplyr::distinct() %>%
                             dplyr::arrange(QC_Warning))$QC_Warning

    if (length(QC_Warning_strings) == 2) {
      warning_string <- QC_Warning_strings[2]

      if (!grepl('W', warning_string, fixed=TRUE)) {
        stop('Could not identify the string used to indicate a flagged sample. Please specify it with the optional argument warning_string. Most commonly it is either "Warning" or "WARN".')
      }
    } else {
      if (length(QC_Warning_strings) == 1 && grepl('P', QC_Warning_strings[1], fixed=TRUE)) {
        warning_string <- 'Warning'
      } else {
        stop('Could not identify the string used to indicate a flagged sample. Please specify it with the optional argument warning_string. Most commonly it is either "Warning" or "WARN".')
      }
    }
  }

  #Filtering on valid OlinkID
  df <- df %>%
    dplyr::filter(stringr::str_detect(OlinkID,
                               "OID[0-9]{5}"))

  #Filtering out control samples
  df <- df %>%
    dplyr::filter(!stringr::str_detect(SampleID, "CONTROL_SAMPLE*"))

  #Outlier calculation as in qc_plot for filtering
  qc_outliers <- df %>%
    dplyr::group_by(Panel, SampleID, Index) %>%
    dplyr::mutate(IQR = IQR(NPX, na.rm = T),
           sample_median = median(NPX, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::select(SampleID, Index, Panel, IQR, sample_median) %>%
    dplyr::distinct() %>%
    dplyr::group_by(Panel) %>%
    dplyr::mutate(median_low = mean(sample_median, na.rm = T) - 3*sd(sample_median, na.rm = T),
           median_high = mean(sample_median, na.rm = T) + 3*sd(sample_median, na.rm = T),
           iqr_low = mean(IQR, na.rm = T) - 3*sd(IQR, na.rm = T),
           iqr_high = mean(IQR, na.rm = T) + 3*sd(IQR, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Outlier = if_else(sample_median < median_high &
                               sample_median > median_low &
                               IQR > iqr_low &
                               IQR < iqr_high,
                             0, 1)) %>%
    dplyr::select(SampleID, Index, Panel, Outlier)

  df_1 <- df %>%
    dplyr::left_join(qc_outliers, by = c('SampleID', 'Index', 'Panel')) %>%
    dplyr::mutate(NPX = ifelse(NPX <= LOD, NA, NPX)) %>%
    dplyr::group_by(SampleID) %>%
    dplyr::mutate(Warnings = sum(QC_Warning == "Warning")) %>%
    dplyr::filter(Warnings == 0) %>%
    dplyr::mutate(Outliers = sum(Outlier)) %>%
    dplyr::filter(Outliers == 0) %>%
    dplyr::mutate(PercAssaysBelowLOD = sum(is.na(NPX))/dplyr::n()) %>%
    dplyr::mutate(MeanNPX = mean(NPX, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(PercAssaysBelowLOD < sampleMissingFreq)

  df_2 <- df_1 %>%
    dplyr::select(SampleID, PercAssaysBelowLOD, MeanNPX) %>%
    dplyr::distinct() %>%
    dplyr::arrange(desc(MeanNPX)) %>%
    dplyr::mutate(Order = c(1:nrow(.)))

  Bridgesamples <- floor(seq(1,nrow(df_2),length.out = n+2)[c(-1, -(n+2))])

  SelectedBridges <- df_2 %>%
    dplyr::filter(Order %in% Bridgesamples)

  return(SelectedBridges)
}
