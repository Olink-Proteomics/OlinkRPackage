
#' # Main function -----------------------------------------------------------
#'
#' #' @title Clean NPX Data Using Common Exclusion Criteria
#' #'
#' #' @description
#' #' Cleans NPX data by removing:
#' #' - Assays with failed normalization (Normalization = "excluded", case-insensitive)
#' #' - Controls based on `SampleType` (e.g., "SAMPLE_CONTROL", "PLATE_CONTROL", "NEGATIVE_CONTROL")
#' #' - Controls based on `SampleID` containing "control"
#' #' - Samples that failed quality control (SampleQC = "FAIL")
#' #'
#' #' @param df A data.frame or tibble containing NPX data.
#' #' @param check_npx_out_list A list from \code{check_npx()} containing unified column names.
#' #'
#' #' @return A cleaned data.frame or tibble with failed or control samples removed.
#' #'
#' #' @examples
#' #' clean_data <- clean_npx(npx_data, check_npx_out_list)
#' #'
#' #' @export
#' clean_npx <- function(df, check_npx_out_list) {
#'   # Step 1: Remove excluded normalization assays ----
#'   df <- clean_failed_assay(df)
#'
#'   # Step 2: Remove sample type controls (if SampleType present) ----
#'   df <- clean_sample_type_controls(df, check_npx_out_list)
#'
#'   # Step 3: Remove sample ID controls ----
#'   df <- clean_sample_id_controls(df, check_npx_out_list)
#'
#'   # Step 4: Remove failed QC ----
#'   df <- clean_qc_warning(df, check_npx_out_list)
#'
#'   return(df)
#' }
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#' # Support function --------------------------------------------------------
#'
#' #' #' @title Clean Excluded Assays from NPX Data Based on Normalization Column
#' #' #'
#' #' #' @description
#' #' #' Removes rows in an NPX dataset where the Normalization status indicates
#' #' #' exclusion. This includes any case variation of the word "excluded".
#' #' #'
#' #' #' @author
#' #' #'  Kang Dong
#' #' #'
#' #' #' @param df A data.frame or tibble containing NPX data.
#' #' #'
#' #' #' @return A filtered data.frame or tibble where failed assays
#' #' #' (Normalization matches "excluded") are removed.
#' #' #'
#' #' #' @examples
#' #' #' clean_data <- clean_excluded_assay(npx_data)
#' #' #'
#' #' #' @export
#' #' clean_excluded_assay <- function(df) {
#' #'
#' #'   # Check input ----
#' #'   check_is_dataset(df = df,
#' #'                    error = TRUE)
#' #'
#' #'   # Check for 'Normalization' column ----
#' #'   if (!"Normalization" %in% colnames(df)) {
#' #'     cli::cli_warn(c(
#' #'       "Column {.var Normalization} not found.",
#' #'       "i" = "Returning data unchanged."
#' #'     ))
#' #'     return(df)
#' #'   }
#' #'
#' #'   # Use stringr to detect 'excluded' (case-insensitive) ----
#' #'   excluded_idx <- stringr::str_detect(df$Normalization,
#' #'                                       regex("excluded", ignore_case = TRUE))
#' #'   n_excluded <- sum(excluded_idx, na.rm = TRUE)
#' #'
#' #'   # Inform user
#' #'   cli::cli_inform(
#' #'     "Removing {n_excluded} row{?s} with {.val Normalization = 'excluded'}
#' #'     (case-insensitive match)."
#' #'     )
#' #'
#' #'   # Filter and return ----
#' #'   cleaned_df <- df[!excluded_idx, ]
#' #'   return(cleaned_df)
#' #' }
#'
#'
#'
#' #' @title Clean Control Samples Based on SampleID
#' #'
#' #' @description
#' #' Removes rows from an NPX dataset where SampleID contains the term "control"
#' #' (case-insensitive).
#' #'
#' #' @author
#' #'  Kang Dong
#' #'
#' #' @param df A data.frame or tibble.
#' #' @param check_npx_out_list A list returned from `check_npx()` with column
#' #' name mapping.
#' #'
#' #' @return A filtered data.frame or tibble.
#' #'
#' #' @export
#' clean_sample_id_controls <- function(df, check_npx_out_list) {
#'
#'   # Check input ----
#'   check_is_dataset(df = df,
#'                    error = TRUE)
#'
#'   # Safely access sample_id from col_names ----
#'   if (!"sample_id" %in% names(check_npx_out_list$col_names)) {
#'     cli::cli_warn(c(
#'       "No column name found for {.var sample_id} in {.code check_npx_out_list}.",
#'       "i" = "Returning data unchanged."
#'     ))
#'     return(df)
#'   }
#'
#'   col_sample_id <- check_npx_out_list$col_names$sample_id
#'
#'   # Check if column exists in data ----
#'   if (!col_sample_id %in% colnames(df)) {
#'     cli::cli_warn(c(
#'       "Column {.var {col_sample_id}} not found in data.",
#'       "i" = "Returning data unchanged."
#'     ))
#'     return(df)
#'   }
#'
#'   # Identify rows containing 'control' ----
#'   control_idx <- stringr::str_detect(df[[col_sample_id]],
#'                                      regex("control", ignore_case = TRUE))
#'   n_controls <- sum(control_idx, na.rm = TRUE)
#'
#'   # Inform user ----
#'   cli::cli_inform("Removing {n_controls} row{?s} with {.var {col_sample_id}}
#'                   containing 'control' (case-insensitive).")
#'
#'   # Filter and return ----
#'   cleaned_df <- df[!control_idx, ]
#'   return(cleaned_df)
#' }
#'
#'
#'
#' #' @title Clean Control Samples Based on SampleType
#' #'
#' #' @description
#' #' Removes rows from an NPX dataset where SampleType indicates control samples.
#' #' These include: "SAMPLE_CONTROL", "PLATE_CONTROL", and "NEGATIVE_CONTROL".
#' #'
#' #' @author
#' #'  Kang Dong
#' #'
#' #' @param df A data.frame or tibble.
#' #' @param check_npx_out_list A list returned from `check_npx()`
#' #' with column name mapping.
#' #'
#' #' @return A filtered data.frame or tibble.
#' #'
#' #' @export
#' clean_sample_type_controls <- function(df, check_npx_out_list) {
#'
#'   # Check input ----
#'   check_is_dataset(df = df,
#'                    error = TRUE)
#'
#'   # Safely access sample_type from col_names ----
#'   if (!"sample_type" %in% names(check_npx_out_list$col_names)) {
#'     cli::cli_warn(c(
#'       "No column name found for {.var sample_type} in
#'       {.code check_npx_out_list}.",
#'       "i" = "Returning data unchanged."
#'     ))
#'     return(df)
#'   }
#'
#'   col_sample_type <- check_npx_out_list$col_names$sample_type
#'
#'   # Check if column is in the actual data ----
#'   if (!col_sample_type %in% colnames(df)) {
#'     cli::cli_warn(c(
#'       "Column {.var {col_sample_type}} not found in data.",
#'       "i" = "Returning data unchanged."
#'     ))
#'     return(df)
#'   }
#'
#'   # Filter out rows ----
#'   control_types <- c("SAMPLE_CONTROL", "PLATE_CONTROL", "NEGATIVE_CONTROL")
#'   control_idx <- df[[col_sample_type]] %in% control_types
#'   n_controls <- sum(control_idx, na.rm = TRUE)
#'
#'   # Inform user ----
#'   cli::cli_inform("Removing {n_controls} row{?s} with {.var {col_sample_type}}
#'                   in {toString(control_types)}.")
#'
#'   # Filter and return ----
#'   cleaned_df <- df[!control_idx, ]
#'   return(cleaned_df)
#' }
#'
#'
#'
#' #' @title Clean Samples That Failed QC
#' #'
#' #' @description
#' #' Removes samples from NPX data where the SampleQC column equals "FAIL"
#' #' (case-insensitive). Also logs a summary of all SampleQC statuses present
#' #' in the dataset.
#' #'
#' #' @author
#' #'  Kang Dong
#' #'
#' #' @param df A data.frame or tibble containing NPX data.
#' #' @param check_npx_out_list A list returned from \code{check_npx()}
#' #' containing standardized column names.
#' #'
#' #' @return A filtered data.frame or tibble with samples failing QC removed.
#' #'
#' #' @examples
#' #' clean_data <- clean_qc_warning(npx_data, check_npx_out_list)
#' #'
#' #' @export
#' clean_qc_warning <- function(df, check_npx_out_list) {
#'
#'   # Check input ----
#'   check_is_dataset(df = df, error = TRUE)
#'
#'   # Get column name for qc_warning ----
#'   if (!"qc_warning" %in% names(check_npx_out_list$col_names)) {
#'     cli::cli_warn(c(
#'       "No column name found for {.var qc_warning} in
#'       {.code check_npx_out_list}.",
#'       "i" = "Returning data unchanged."
#'     ))
#'     return(df)
#'   }
#'
#'   col_qc_warning <- check_npx_out_list$col_names$qc_warning
#'
#'   # Check if the qc_warning column exists ----
#'   if (!col_qc_warning %in% colnames(df)) {
#'     cli::cli_warn(c(
#'       "Column {.var {col_qc_warning}} not found in data.",
#'       "i" = "Returning data unchanged."
#'     ))
#'     return(df)
#'   }
#'
#'   # Summarize SampleQC statuses ----
#'   qc_summary <- table(
#'     toupper(trimws(df[[col_qc_warning]])),
#'     useNA = "ifany")
#'   qc_summary_msg <- paste(names(qc_summary),
#'                           qc_summary,
#'                           sep = ": ", collapse = "; ")
#'   cli::cli_inform("SampleQC status summary — {qc_summary_msg}")
#'
#'   # Identify failed samples ----
#'   fail_idx <- stringr::str_detect(df[[col_qc_warning]],
#'                                   regex("^FAIL$", ignore_case = TRUE))
#'   n_fail <- sum(fail_idx, na.rm = TRUE)
#'
#'   # Inform user and remove ----
#'   cli::cli_inform("Removing {n_fail} row{?s} with {.val SampleQC = 'FAIL'}")
#'
#'   cleaned_df <- df[!fail_idx, ]
#'   return(cleaned_df)
#' }


# -------------------------------------------------------------------------



# Support Functions -------------------------------------------------------


#' Remove assays with only NA values
#'
#' @description
#' This function filters out rows from a data frame where the assay
#' identifier (e.g.,`OlinkID`) matches those listed in `check_npx_log$assay_na`,
#'  which contains assays composed entirely of NA values. It uses CLI messaging
#'  to report which assays were removed.
#'
#' @param df A data frame loads from `read_npx()`, including a column identified
#' by `check_npx_log$col_names$olink_id`.
#' @param check_npx_log A list generated by `check_npx()` function, containing:
#'   - `assay_na`: a character vector of `OlinkID` to be excluded.
#'   - `col_names$olink_id`: the column name in `df` that holds the `OlinkID`.
#' @param out_df Output format of the cleaned data. Options: `"tibble"` (default)
#' or `"arrow"`.
#'
#' @return A filtered data frame with rows corresponding to NA-only
#' assays removed.
#' @export
#'
#' @examples
#' # Assuming df is a valid NPX-like data frame with an "OlinkID" column,
#' # and check_npx_log is a list with assay_na and col_names$olink_id
#' clean_df <- clean_assay_na(df, check_npx_log)
#'
clean_assay_na <- function(df,
                           check_npx_log,
                           out_df = "tibble") {

  # If there are no assays with all NA values, skip filtering
  if (length(check_npx_log$assay_na) == 0) {
    cli::cli_alert_info("No assays with only NA values found.
                        Returning original data frame.")
    return(
      df |>
        convert_read_npx_output(out_df = out_df)
    )
  }

  # Prepare column and values to filter
  col_olink_id <- rlang::sym(check_npx_log$col_names$olink_id)
  olink_ids_to_remove <- check_npx_log$assay_na


  # CLI message listing excluded assays
  cli::cli_alert_warning(
    "Excluding {length(olink_ids_to_remove)} assays with only NA values:
    {paste(olink_ids_to_remove, collapse = ', ')}"
  )


  # Exclude assays with only NA values
  df_cleaned <- df |>
    dplyr::filter(!(!!col_olink_id %in% olink_ids_to_remove)) |>
    dplyr::collect()
  cli::cli_alert_success(
  "Removed rows for assays with only NA values.Returning cleaned data frame."
  )


  # Convert output to desired format (tibble or arrow)
  return(
    df_cleaned |>
      convert_read_npx_output(out_df = out_df)
  )
}


#' Remove assays with invalid OlinkID
#'
#' @description
#' This function filters out rows from a data frame where the assay identifier
#' column (e.g.,`OlinkID`) matches values listed in `check_npx_log$oid_invalid`,
#' which identifies invalid or malformed assay identifiers. Uses CLI messages to
#' inform users of what was excluded.
#'
#' @param df A data frame loaded from `read_npx()`, containing a column
#' specified by `check_npx_log$col_names$olink_id`.
#' @param check_npx_log A list returned by `check_npx()`, containing:
#'   - `oid_invalid`: a character vector of invalid assay identifiers to be removed.
#'   - `col_names$olink_id`: the name of the column in `df` that holds the assay IDs.
#' @param out_df Output format of the cleaned data.
#' Options: `"tibble"` (default) or `"arrow"`.
#'
#' @return A filtered data frame with invalid OlinkIDs removed.
#' @export
#'
#' @examples
#' # df <- read_npx("your_data.parquet")
#' # log <- check_npx(df)
#' # clean_df <- clean_invalid_oid(df, log)
clean_invalid_oid <- function(df,
                              check_npx_log,
                              out_df = "tibble") {

  # Check if there are any invalid OlinkIDs to remove
  if (length(check_npx_log$oid_invalid) == 0) {
    cli::cli_alert_info(
      "No invalid OlinkIDs found. Returning original data frame."
    )
    return(
      df |>
        convert_read_npx_output(out_df = out_df)
    )
  }


  # Prepare column and values to filter
  col_olink_id <- rlang::sym(check_npx_log$col_names$olink_id)
  olink_ids_to_remove <- check_npx_log$oid_invalid


  # Inform user of which assays will be excluded
  cli::cli_alert_warning(
    "Excluding {length(olink_ids_to_remove)} assays with invalid OlinkIDs:
    {paste(olink_ids_to_remove, collapse = ', ')}"
  )


  # Remove rows where the OlinkID is invalid
  df_cleaned <- df |>
    dplyr::filter(!(!!col_olink_id %in% olink_ids_to_remove)) |>
    dplyr::collect()


  # Confirmation message
  cli::cli_alert_success(
    "Removed rows for assays with invalid OlinkIDs. Returning cleaned data frame."
  )


  # Return cleaned data frame in desired format
  return(
    df_cleaned |>
      convert_read_npx_output(out_df = out_df)
  )
}
