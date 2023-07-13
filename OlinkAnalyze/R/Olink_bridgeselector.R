#' Bridge selection function
#'
#'The bridge selection function will select a number of bridge samples based on the input data. It selects samples with
#'good detection, which passes QC and cover a good range of the data. If possible, Olink recommends 8-16 bridge samples.
#'When running the selector, Olink recommends starting at sampleMissingFreq = 0.10 which represents a maximum of 10\%
#'data below LOD per sample. If there are not enough samples output, increase to 20\%. \cr\cr
#'The function accepts NPX Excel files with data < LOD replaced.
#'
#' @param df Tibble/data frame in long format such as produced by the Olink Analyze read_NPX function.
#' @param sampleMissingFreq The threshold for sample wise missingness.
#' @param n Number of bridge samples to be selected.
#'
#' @return A "tibble" with sample IDs and mean NPX for a defined number of bridging samples. Columns include:
#'
#' \itemize{
#'   \item{SampleID:} Sample ID
#'   \item{PercAssaysBelowLOD:} Percent of Assays that are below LOD for the sample
#'   \item{MeanNPX:} Mean NPX for the sample
#' }
#' @export
#'
#' @examples
#' \donttest{bridge_samples <- olink_bridgeselector(npx_data1, sampleMissingFreq = 0.1, n = 20)}
#' @importFrom magrittr %>%
#' @importFrom dplyr n select distinct arrange group_by mutate ungroup left_join filter if_else
#' @importFrom stringr str_detect

olink_bridgeselector<-function(df, sampleMissingFreq, n){
  # Exclude OlinkIDs with missing NPX
  npx_check <- npxCheck(df)

  #Filtering on valid OlinkID
  df <- df %>%
    dplyr::filter(!(OlinkID %in% npx_check$all_nas)) %>%
    dplyr::filter(stringr::str_detect(OlinkID,
                                      "OID[0-9]{5}"))

  #Filtering out control samples
  df <- df %>%
    dplyr::filter(!stringr::str_detect(SampleID, "CONTROL_SAMPLE*"))

  #Outlier calculation as in qc_plot for filtering
  qc_outliers <- df %>%
    dplyr::group_by(Panel, SampleID, Index) %>%
    dplyr::mutate(IQR = IQR(NPX, na.rm = TRUE),
                  sample_median = median(NPX, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::select(SampleID, Index, Panel, IQR, sample_median) %>%
    dplyr::distinct() %>%
    dplyr::group_by(Panel) %>%
    dplyr::mutate(median_low = mean(sample_median, na.rm = TRUE) - 3*sd(sample_median, na.rm = TRUE),
                  median_high = mean(sample_median, na.rm = TRUE) + 3*sd(sample_median, na.rm = TRUE),
                  iqr_low = mean(IQR, na.rm = TRUE) - 3*sd(IQR, na.rm = TRUE),
                  iqr_high = mean(IQR, na.rm = TRUE) + 3*sd(IQR, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Outlier = dplyr::if_else(sample_median < median_high &
                                      sample_median > median_low &
                                      IQR > iqr_low &
                                      IQR < iqr_high,
                                    0, 1)) %>%
    dplyr::select(SampleID, Index, Panel, Outlier)
  
  
  # Alternative LODs for when LOD is not present
  if(!("LOD" %in% names(df))){
    if("Max LOD" %in% names(df)){
      df <- df |> 
        dplyr::mutate(LOD = `Max LOD`)
      message("Using Max LOD as filter criteria...")  
    } else if ("Plate LOD" %in% names(df)){
      df <- df |> 
        dplyr::mutate(LOD = `Plate LOD`)
      
      message("Using Plate LOD as filter criteria...")
    } else if ("Plate_LOD" %in% names(df)){
      df <- df |> 
        dplyr::mutate(LOD = Plate_LOD)
      
      message("Using Plate_LOD as filter criteria...")
    } else {
      df <- df |> 
        dplyr::mutate(LOD = -Inf)
      
      message("LOD not available. No filtering by LOD...")
    }
  }
  
  if("SampleQC" %in% names(df)){
    df <- df |> 
      dplyr::mutate(QC_Warning = SampleQC)
  }

  df_1 <- df %>%
    dplyr::left_join(qc_outliers, by = c('SampleID', 'Index', 'Panel')) %>%
    dplyr::mutate(NPX = ifelse(NPX <= LOD, NA, NPX)) %>%
    dplyr::group_by(SampleID) %>%
    dplyr::mutate(QC_Warning = dplyr::if_else(all(toupper(QC_Warning) == 'PASS'),
                                              'PASS',
                                              'WARNING')) %>%
    dplyr::filter(QC_Warning == 'PASS') %>%
    dplyr::mutate(Outliers = sum(Outlier)) %>%
    dplyr::filter(Outliers == 0) %>%
    dplyr::mutate(PercAssaysBelowLOD = sum(is.na(NPX))/dplyr::n()) %>%
    dplyr::mutate(MeanNPX = mean(NPX, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(PercAssaysBelowLOD < sampleMissingFreq)


  df_2 <- df_1 %>%
    dplyr::select(SampleID, PercAssaysBelowLOD, MeanNPX) %>%
    dplyr::distinct()

  if(nrow(df_2) < n){
    stop(paste0('With the current settings only ',
                nrow(df_2),
                ' samples can be selected. Please increase sampleMissingFreq and/or decrease n.'))

  } else if (nrow(df_2) == n) {
    # if samples satisfying the criteria equal the number of requested samples
    # return all of them
    SelectedBridges <- df_2
  } else { #
    df_2  <- df_2  %>%
      dplyr::arrange(desc(MeanNPX)) %>%
      dplyr::mutate(Order = c(1:nrow(.)))

    Bridgesamples <- floor(seq(1,nrow(df_2),length.out = n+2)[c(-1, -(n+2))])

    SelectedBridges <- df_2 %>%
      dplyr::filter(Order %in% Bridgesamples) %>%
      dplyr::slice_sample(n = nrow(.)) %>%
      dplyr::select(-Order)
  }

  return(SelectedBridges)
}
