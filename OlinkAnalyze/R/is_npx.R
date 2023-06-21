# Perform NPX data check
#
# This function performs various checks on an NPX data frame to ensure data integrity and #entify any issues.
#
# @param df A data frame or tibble containing NPX data.
#
# @return A list containing the results of the data checks:
#   \item{data_type}{A character indicating the data type ("NPX" or "Quantified_value").}
#   \item{non_conforming_OID}{A character vector of non-conforming OIDs.}
#   \item{all_nas}{A character vector of assays with NA values for all samples.}
#   \item{sample_all_nas}{A character vector of samples with NA values for all assays.}
#   \item{duplicated_samples}{A character vector of duplicated sample IDs.}
#
# @export
#
# @importFrom dplyr distinct filter pull select group_by summarise n
# @importFrom stringr str_detect
# @importFrom rlang ensym
#
# @examples
# npxCheck(npx_data1)

npxCheck <- function(df) {
  # Extract column names
  df_colnames <- colnames(df)

  # Check whether df contains NPX or QUANT ----
  if ("NPX" %in% df_colnames) {
    data_type <- "NPX"
  } else if ("Quantified_value" %in% df_colnames) {
    data_type <- "Quantified_value"
  } else {
    stop("Neither NPX or Quantified_value column present in the data")
  }

  # Check whether df contains recognized OIDs ----
  if (!("OlinkID" %in% df_colnames)) {
    stop("OlinkID column not present in the data")
  } else {
    non_conforming_OID <- df |>
      dplyr::distinct(OlinkID) |>
      dplyr::filter(stringr::str_detect(OlinkID,
                                        "OID[0-9]{5}",
                                        negate = TRUE)) |>
      dplyr::pull()
  }

  # Check for duplicates in SampleID ----
  duplicated_ids <- df |>
    dplyr::select(SampleID,
                  OlinkID) |>
    duplicated()

  # Check if any duplicates are found
  duplicated_samples <- character(0)
  if (any(duplicated_ids)) {
    duplicated_samples <- unique(df$SampleID[duplicated_ids])
    warning(
      "Duplicate sampleIDs found:\n ",
      paste(duplicated_samples, collapse = "\n "),
      call. = FALSE
    )
  }

  # Identify assays that have only NAs ----
  all_nas <- df |>
    dplyr::group_by(OlinkID) |>
    dplyr::summarise(n = dplyr::n(),
                     n_na = sum(is.na(!!rlang::ensym(data_type))),
                     .groups = "drop") |>
    dplyr::filter(n == n_na) |>
    dplyr::pull(OlinkID)

  if (length(all_nas) > 0) {
    warning(
      paste0(
        "The assays ",
        paste(all_nas, collapse = ", "),
        " have NPX = NA for all samples. They will be excluded from the analysis"
      ),
      call. = FALSE
    )
  }

  # Identify samples that have all NAs for an assay ----
  sample_all_nas <- df |>
    dplyr::group_by(SampleID) |>
    dplyr::summarise(n = dplyr::n(),
                     n_na = sum(is.na(!!rlang::ensym(data_type))),
                     .groups = "drop") |>
    dplyr::filter(n == n_na) |>
    dplyr::pull(SampleID)

  if (length(sample_all_nas) > 0) {
    warning(
      paste0(
        "The samples ",
        paste(sample_all_nas, collapse = ", "),
        " have NPX = NA for all assays. They will be excluded from the analysis"
      ),
      call. = FALSE
    )
  }

  # Identify the assays with QC warning ----
  # Identify the assay_warning column
  assay_warning <-
    npx_colnames$Assay_Warning[npx_colnames$Assay_Warning %in% df_colnames]

  if (length(assay_warning) == 0) {
    warning("Assay QC warning column not found.",
            call. = FALSE)
  } else {
    assays_with_warning <- df |>
      dplyr::select(OlinkID,
                    !!rlang::ensym(assay_warning)) |>
      dplyr::filter(grepl("(?i)warn",
                          !!rlang::ensym(assay_warning))) |>
      dplyr::distinct(OlinkID) |>
      dplyr::pull()
    warning(
      paste0(
        "Following assays have ",
        assay_warning,
        " with WARN status: ",
        paste(assays_with_warning, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  return(
    list(
      data_type = data_type,
      non_conforming_OID = non_conforming_OID,
      all_nas = all_nas,
      sample_all_nas = sample_all_nas,
      duplicated_samples = duplicated_samples
    )
  )
}

# Column names, a dictionary of variants of column names based on different versions MyData and Explore NPX software
npx_colnames <-
  list(
    SampleID = c("SampleID"),
    Sample_Type = c("Sample_Type", "SampleType"),
    Index = c("Index", "WellIndex"),
    WellID = c("WellID"),
    PlateID = c("PlateID"),
    OlinkID = c("OlinkID"),
    UniProt = c("UniProt"),
    Assay = c("Assay"),
    AssayType = c("AssayType"),
    Panel = c("Panel"),
    Block = c("Block"),
    Count = c("Count"),
    NPX = c("NPX"),
    Normalization = c("Normalization"),
    QC_Warning = c("QC_Warning"),
    SampleBlockQCWarn = c("SampleBlockQCWarn"),
    SampleBlockQCFail = c("SampleBlockQCFail"),
    BlockQCFail = c("BlockQCFail"),
    Assay_Warning = c("Assay_Warning", "AssayQcWarn"),
    Panel_Lot_Nr = c("Panel_Lot_Nr", "Panel_Version"),
    ExploreVersion = c("ExploreVersion"),
    LOD = c("LOD"),
    MissingFreq = c("MissingFreq")
  )
