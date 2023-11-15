#' Function to read NPX data into long format
#'
#' Imports an NPX or QUANT file exported from Olink Software.
#' No alterations to the output format is allowed.
#'
#' @param filename Path to Olink Software output file.
#' @return A "tibble" in long format. Columns include:
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
#' @keywords NPX
#' @export
#' @examples
#' \donttest{
#' file <- system.file("extdata", "Example_NPX_Data.csv", package = "OlinkAnalyze")
#' read_NPX(file)
#' }
#' @importFrom magrittr %>%

read_NPX <- function(filename) {
  # If the file is csv or txt read_NPX assumes Explore NPX data in long format
  if (tools::file_ext(filename) %in% c("csv", "txt", "zip", "parquet")) {

    read_NPX_explore(filename = filename)

  } else {

    stop("Unrecognized input file extension!")

  }
}

read_NPX_explore <- function(filename) {

  # **** Read ****

  # if input file is compressed
  if (tools::file_ext(filename) == "zip") {

    out <- read_npx_zip(filename = filename)

  } else if (tools::file_ext(filename) == "parquet") {

    out <- read_npx_parquet(file = filename)

  } else if (tools::file_ext(filename) %in% c("csv", "txt")) {

    out <- read_npx_csv(filename = filename)

  }

  # Check that all column names are present
  # We have had 5 column names versions so far: 1, 1.1 and 2, 2.1, and 3
  # please add newer versions to the list chronologically
  header_standard <- c("SampleID", "OlinkID", "UniProt", "Assay",
                       "Panel", "PlateID", "QC_Warning", "NPX")

  header_ext_standard <- c(header_standard, "Sample_Type", "Panel_Lot_Nr",
                           "WellID", "Normalization", "Assay_Warning",
                           "IntraCV", "InterCV", "Processing_StartDate",
                           "Processing_EndDate", "AnalyzerID")

  header_v <- list(
    "header_npx_v1"   = c(header_standard,
                          "Index",
                          "MissingFreq",
                          "LOD",
                          "Panel_Version"),

    "header_npx_v1.1" = c(header_standard,
                          "Index",
                          "MissingFreq",
                          "LOD",
                          "Panel_Version",
                          "Normalization",
                          "Assay_Warning"),

    "header_npx_v2"   = c(header_standard,
                          "Index",
                          "MissingFreq",
                          "LOD",
                          "Panel_Lot_Nr",
                          "Normalization"),

    "header_npx_v2.1" = c(header_standard,
                          "Index",
                          "MissingFreq",
                          "LOD",
                          "Panel_Lot_Nr",
                          "Normalization",
                          "Assay_Warning"),

    "header_npx_v3"   = c(header_standard,
                          "Index",
                          "MissingFreq",
                          "LOD",
                          "Sample_Type",
                          "Panel_Lot_Nr",
                          "Assay_Warning",
                          "Normalization",
                          "ExploreVersion"),

    "header_npx_v4"   = c(header_standard,
                          "WellID",
                          "Block",
                          "Normalization",
                          "ExploreVersion",
                          "AssayType",
                          "Count",
                          "ExtNPX",
                          "PCNormalizedNPX",
                          "SampleType",
                          "Panel_Lot_Nr",
                          "Assay_Warning"
                          ),

    "header_ext_v1"   = c(header_ext_standard,
                          "Index",
                          "MissingFreq",
                          "LOD",
                          "INC_Warning",
                          "AMP_Warning",
                          "Count_Warning"),

    "header_ext_v2"   = c(header_ext_standard,
                          "Index",
                          "MissingFreq",
                          "LOD",
                          "ExploreVersion"),

    "header_parquet" = c("SampleID", "WellID","PlateID", "OlinkID",
                         "UniProt", "Assay", "Panel", "NPX",
                         "Normalization", "ExploreVersion", "Block",
                         "AssayType", "Count", "ExtNPX",
                         "PCNormalizedNPX", "SampleType",
                         "DataAnalysisRefID", "AssayQC", "SampleQC")
  )

  header_match <- header_v |>
    sapply(function(x)
      identical(
        sort(x),
        { colnames(out) |> sort() }
      )
    ) |>
    any() # look for one full match

  if (header_match == FALSE) {

    # if there is a mismatch of the input data with the expected column names:
    # - we pick the set of column names with the shortest distance to any of
    #   the expected ones.
    # - if multiple matches occur, we pick the most recent.
    header_diff_1 <- lapply(header_v, function(x) setdiff(x, colnames(out))) |>
      lapply(length)

    header_diff_2 <- lapply(header_v, function(x) setdiff(colnames(out), x)) |>
      lapply(length)

    header_pick <- data.frame(v_name = names(header_v),
                              v1     = unlist(header_diff_1),
                              v2     = unlist(header_diff_2)) |>
      dplyr::mutate(v = .data[["v1"]] + .data[["v2"]]) |>
      dplyr::arrange(.data[["v"]], .data[["v_name"]]) |>
      dplyr::slice_head(n = 1) |>
      dplyr::pull(.data[["v_name"]])

    # find missing columns
    missing_cols <- setdiff(header_v[[header_pick]], colnames(out))

    if (length(missing_cols)  != 0) {
      #If missing columns, throw a warning and print out which ones we guessed
      # that are missing
      warning(paste0("Cannot find columns: ", paste(missing_cols, collapse = ",")))

    }

  }
  # Remove Target formatting in case of incidental csv
  if(any(grepl("Target", out$Panel))){
    warning("Target csv is not fully supported. Consider Target xlsx format instead.")
    # Replicate read_npx_target formatting.
    out <- out %>%
      dplyr::mutate(Panel =  gsub("\\(.*\\)","",Panel)) %>%
      dplyr::mutate(Panel = stringr::str_to_title(Panel)) %>%
      dplyr::mutate(Panel = gsub("Target 96", "", Panel)) %>%
      dplyr::mutate(Panel = gsub("Target 48", "", Panel)) %>%
      dplyr::mutate(Panel = gsub("Olink", "", Panel)) %>%
      dplyr::mutate(Panel = trimws(Panel, which = "left")) %>%
      tidyr::separate(Panel, " ", into = c("Panel_Start", "Panel_End"), fill = "right") %>%
      dplyr::mutate(Panel_End = ifelse(grepl("Ii", Panel_End), stringr::str_to_upper(Panel_End), Panel_End)) %>%
      dplyr::mutate(Panel_End = ifelse(is.na(Panel_End), " ", Panel_End)) %>%
      dplyr::mutate(Panel = paste("Olink", Panel_Start, Panel_End)) %>%
      dplyr::mutate(Panel = trimws(Panel, which = "right")) %>%
      dplyr::select(-Panel_Start, -Panel_End)
  }

  return(out)

}
