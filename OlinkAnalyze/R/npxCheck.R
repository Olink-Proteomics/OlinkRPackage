# @description
#  Perform NPX data check
#  This function performs various checks on an NPX data frame to ensure data integrity and identify any issues. Some other repetitive tasks can be moved here.
#
# @param df A data frame or tibble containing NPX data.
#
# @return A list containing the results of the data checks:
#   \item{data_type}{A character indicating the data type ("NPX" or "Quantified_value").}
#   \item{non_conforming_OID}{A character vector of non-conforming OIDs.}
#   \item{all_nas}{A character vector of assays with NA values for all samples.}
#   \item{sample_all_nas}{A character vector of samples with NA values for all assays.}
#   \item{duplicate_samples}{A character vector of duplicate sample IDs.}
#
# @importFrom dplyr distinct filter pull select group_by summarise n
# @importFrom stringr str_detect
# @importFrom rlang ensym
# @export
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
      dplyr::pull(OlinkID)
  }
  # Check for OlinkIDs that map to multiple UniProtIDs
  uniprot_replace <- assay_identifiers(df)

  # Check for duplicates in SampleID ----
  duplicate_ids <- df |>
    dplyr::select(SampleID,
                  OlinkID) |>
    duplicated()

  # Check if any duplicates are found
  duplicate_samples <- character(0)
  if (any(duplicate_ids)) {
    duplicate_samples <- unique(df$SampleID[duplicate_ids])
    message(
      "Duplicate SampleID(s) detected:\n ",
      paste(duplicate_samples, collapse = "\n ")
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
    df |> dplyr::select(
      tidyselect::any_of(
        c("Assay_Warning", "AssayQC")
      )) |>
    names()

  if (length(assay_warning) > 0) {
    assays_with_warning <- df |>
      dplyr::select(OlinkID,
                    !!rlang::ensym(assay_warning)) |>
      dplyr::filter(grepl("(?i)warn",
                          !!rlang::ensym(assay_warning))) |>
      dplyr::distinct(OlinkID) |>
      dplyr::pull()
    message(
      paste(length(assays_with_warning),
            " assay(s) exhibited assay QC warning. For more information see the ",
            assay_warning," column.", sep = "")
    )
  }

  return(
    list(
      data_type = data_type,
      non_conforming_OID = non_conforming_OID,
      all_nas = all_nas,
      sample_all_nas = sample_all_nas,
      duplicate_samples = duplicate_samples,
      uniprot_replace = uniprot_replace
    )
  )
}
