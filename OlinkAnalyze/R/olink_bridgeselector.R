#' Old bridge selection function wrapper
#'
#' The bridge selection function will select a number of bridge samples based
#' on the input data. It selects samples with good detection that pass QC
#' and cover a good range of the data. If possible, Olink recommends 8-16
#' bridge samples. When running the selector, Olink recommends starting at
#' sample_missing_freq = 0.10 which represents a maximum of 10\% data below LOD
#' per sample. If there are not enough samples output, increase to 20\%. \cr\cr
#' The function accepts NPX Excel files with data < LOD replaced.
#'
#' `olink_bridgeselector()` is a synonym of `olink_bridge_selector()` .
#'
#' @param df Tibble/data frame in long format such as produced by the
#' Olink Analyze read_npx function.
#' @param sample_missing_freq The threshold for sample wise missingness.
#' @param n Number of bridge samples to be selected.
#'
#' @return A "tibble" with sample IDs and mean NPX for a defined number of
#' bridging samples. Columns include:
#'
#' @aliases
#' olink_bridgeselector
#'
#' \itemize{
#'   \item{SampleID:} Sample ID
#'   \item{perc_assays_below_lod:} Percent of Assays that are below LOD for
#'   the sample
#'   \item{MeanNPX:} Mean NPX for the sample
#' }
#' \donttest{bridge_samples <- olink_bridge_selector(npx_data1,
#' sample_missing_freq = 0.1, n = 20)}
#' @importFrom dplyr n select distinct arrange group_by mutate ungroup
#' left_join filter if_else
#' @importFrom stringr str_detect


#'
#' @export
  dots <- list(...)
  # Accept either spelling
  sampleMissingFreq <- dots$sampleMissingFreq %||% dots$sample_missing_freq
  if (is.null(sampleMissingFreq)) {
    stop("Please supply either sampleMissingFreq or sample_missing_freq.")
  }
  return(olink_bridgeselector(df = df, sample_missing_freq = sampleMissingFreq,
                              n = n))
}







































































































#' @export