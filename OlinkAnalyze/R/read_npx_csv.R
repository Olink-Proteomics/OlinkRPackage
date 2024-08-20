#' Helper function to read in Olink Explore csv or txt files
#'
#' @param filename Path to Olink Software output txt of csv file.
#'
#' @return A "tibble" in long format. Some of the columns are:
#' \itemize{
#'    \item{SampleID:} Sample ID
#'    \item{Index:} Index
#'    \item{OlinkID:} Olink ID
#'    \item{UniProt:} UniProt ID
#'    \item{Assay:} Protein symbol
#'    \item{MissingFreq:} Proportion of sample below LOD
#'    \item{Panel_Version:} Panel Version
#'    \item{PlateID:} Plate ID
#'    \item{QC_Warning:} QC Warning Status
#'    \item{LOD:} Limit of detection
#'    \item{NPX:} Normalized Protein Expression
#' }
#' Additional columns may be present or missing depending on the platform
#'
#' @keywords NPX
#'
#' @examples
#' \donttest{
#' file <- system.file("extdata", "Example_NPX_Data.csv", package = "OlinkAnalyze")
#' read_NPX(file)
#' }
#'

read_npx_csv <- function(filename) {

  df_npx <- data.table::fread(file=filename,
                              data.table=FALSE)

  # if this fails too, then throw an error
  if (is.data.frame(df_npx) &&
      ncol(df_npx) == 1) {

    stop("Could not read NPX csv file! Delimiter is not \";\" or \",\". ")

  }

  df_npx <- df_npx %>%
    dplyr::mutate(SampleID    = as.character(SampleID)) %>%
    dplyr::mutate(dplyr::across(dplyr::ends_with("LOD"), as.numeric))%>%
    dplyr::as_tibble()

  if ("NPX" %in% colnames(df_npx)) {
    df_npx <- df_npx %>%
      mutate(NPX = as.numeric(NPX))
  }

  if ("Quantified_value" %in% colnames(df_npx)) {
    df_npx <- df_npx %>%
      mutate(Quantified_value = as.numeric(Quantified_value))
  }

  if ("MissingFreq" %in% colnames(df_npx)) {
    if (any(grepl("%", df_npx$MissingFreq))) {
    df_npx <- df_npx %>%
      mutate(MissingFreq = as.numeric(gsub("%", "", MissingFreq)) / 100)
    }
  }

  return(df_npx)
}
