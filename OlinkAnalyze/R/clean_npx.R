
# Main function -----------------------------------------------------------

#' @title Clean NPX Data Using Common Exclusion Criteria
#'
#' @description
#' Cleans NPX data by removing:
#' - Assays with failed normalization (Normalization = "excluded", case-insensitive)
#' - Controls based on `SampleType` (e.g., "SAMPLE_CONTROL", "PLATE_CONTROL", "NEGATIVE_CONTROL")
#' - Controls based on `SampleID` containing "control"
#' - Samples that failed quality control (SampleQC = "FAIL")
#'
#' @param df A data.frame or tibble containing NPX data.
#' @param check_npx_out_list A list from \code{check_npx()} containing unified column names.
#'
#' @return A cleaned data.frame or tibble with failed or control samples removed.
#'
#' @examples
#' clean_data <- clean_npx(npx_data, check_npx_out_list)
#'
#' @export
clean_npx <- function(df, check_npx_out_list) {
  # Step 1: Remove excluded normalization assays ----
  df <- clean_failed_assay(df)

  # Step 2: Remove sample type controls (if SampleType present) ----
  df <- clean_sample_type_controls(df, check_npx_out_list)

  # Step 3: Remove sample ID controls ----
  df <- clean_sample_id_controls(df, check_npx_out_list)

  # Step 4: Remove failed QC ----
  df <- clean_qc_warning(df, check_npx_out_list)

  return(df)
}










# Support function --------------------------------------------------------

#' @title Clean Excluded Assays from NPX Data Based on Normalization Column
#'
#' @description
#' Removes rows in an NPX dataset where the Normalization status indicates
#' exclusion. This includes any case variation of the word "excluded".
#'
#' @author
#'  Kang Dong
#'
#' @param df A data.frame or tibble containing NPX data.
#'
#' @return A filtered data.frame or tibble where failed assays
#' (Normalization matches "excluded") are removed.
#'
#' @examples
#' clean_data <- clean_excluded_assay(npx_data)
#'
#' @export
clean_excluded_assay <- function(df) {

  # Check input ----
  check_is_dataset(df = df,
                   error = TRUE)

  # Check for 'Normalization' column ----
  if (!"Normalization" %in% colnames(df)) {
    cli::cli_warn(c(
      "Column {.var Normalization} not found.",
      "i" = "Returning data unchanged."
    ))
    return(df)
  }

  # Use stringr to detect 'excluded' (case-insensitive) ----
  excluded_idx <- stringr::str_detect(df$Normalization,
                                      regex("excluded", ignore_case = TRUE))
  n_excluded <- sum(excluded_idx, na.rm = TRUE)

  # Inform user
  cli::cli_inform(
    "Removing {n_excluded} row{?s} with {.val Normalization = 'excluded'}
    (case-insensitive match)."
    )

  # Filter and return ----
  cleaned_df <- df[!excluded_idx, ]
  return(cleaned_df)
}



#' @title Clean Control Samples Based on SampleID
#'
#' @description
#' Removes rows from an NPX dataset where SampleID contains the term "control"
#' (case-insensitive).
#'
#' @author
#'  Kang Dong
#'
#' @param df A data.frame or tibble.
#' @param check_npx_out_list A list returned from `check_npx()` with column
#' name mapping.
#'
#' @return A filtered data.frame or tibble.
#'
#' @export
clean_sample_id_controls <- function(df, check_npx_out_list) {

  # Check input ----
  check_is_dataset(df = df,
                   error = TRUE)

  # Safely access sample_id from col_names ----
  if (!"sample_id" %in% names(check_npx_out_list$col_names)) {
    cli::cli_warn(c(
      "No column name found for {.var sample_id} in {.code check_npx_out_list}.",
      "i" = "Returning data unchanged."
    ))
    return(df)
  }

  col_sample_id <- check_npx_out_list$col_names$sample_id

  # Check if column exists in data ----
  if (!col_sample_id %in% colnames(df)) {
    cli::cli_warn(c(
      "Column {.var {col_sample_id}} not found in data.",
      "i" = "Returning data unchanged."
    ))
    return(df)
  }

  # Identify rows containing 'control' ----
  control_idx <- stringr::str_detect(df[[col_sample_id]],
                                     regex("control", ignore_case = TRUE))
  n_controls <- sum(control_idx, na.rm = TRUE)

  # Inform user ----
  cli::cli_inform("Removing {n_controls} row{?s} with {.var {col_sample_id}}
                  containing 'control' (case-insensitive).")

  # Filter and return ----
  cleaned_df <- df[!control_idx, ]
  return(cleaned_df)
}



#' @title Clean Control Samples Based on SampleType
#'
#' @description
#' Removes rows from an NPX dataset where SampleType indicates control samples.
#' These include: "SAMPLE_CONTROL", "PLATE_CONTROL", and "NEGATIVE_CONTROL".
#'
#' @author
#'  Kang Dong
#'
#' @param df A data.frame or tibble.
#' @param check_npx_out_list A list returned from `check_npx()`
#' with column name mapping.
#'
#' @return A filtered data.frame or tibble.
#'
#' @export
clean_sample_type_controls <- function(df, check_npx_out_list) {

  # Check input ----
  check_is_dataset(df = df,
                   error = TRUE)

  # Safely access sample_type from col_names ----
  if (!"sample_type" %in% names(check_npx_out_list$col_names)) {
    cli::cli_warn(c(
      "No column name found for {.var sample_type} in
      {.code check_npx_out_list}.",
      "i" = "Returning data unchanged."
    ))
    return(df)
  }

  col_sample_type <- check_npx_out_list$col_names$sample_type

  # Check if column is in the actual data ----
  if (!col_sample_type %in% colnames(df)) {
    cli::cli_warn(c(
      "Column {.var {col_sample_type}} not found in data.",
      "i" = "Returning data unchanged."
    ))
    return(df)
  }

  # Filter out rows ----
  control_types <- c("SAMPLE_CONTROL", "PLATE_CONTROL", "NEGATIVE_CONTROL")
  control_idx <- df[[col_sample_type]] %in% control_types
  n_controls <- sum(control_idx, na.rm = TRUE)

  # Inform user ----
  cli::cli_inform("Removing {n_controls} row{?s} with {.var {col_sample_type}}
                  in {toString(control_types)}.")

  # Filter and return ----
  cleaned_df <- df[!control_idx, ]
  return(cleaned_df)
}



#' @title Clean Samples That Failed QC
#'
#' @description
#' Removes samples from NPX data where the SampleQC column equals "FAIL"
#' (case-insensitive). Also logs a summary of all SampleQC statuses present
#' in the dataset.
#'
#' @author
#'  Kang Dong
#'
#' @param df A data.frame or tibble containing NPX data.
#' @param check_npx_out_list A list returned from \code{check_npx()}
#' containing standardized column names.
#'
#' @return A filtered data.frame or tibble with samples failing QC removed.
#'
#' @examples
#' clean_data <- clean_qc_warning(npx_data, check_npx_out_list)
#'
#' @export
clean_qc_warning <- function(df, check_npx_out_list) {

  # Check input ----
  check_is_dataset(df = df, error = TRUE)

  # Get column name for qc_warning ----
  if (!"qc_warning" %in% names(check_npx_out_list$col_names)) {
    cli::cli_warn(c(
      "No column name found for {.var qc_warning} in
      {.code check_npx_out_list}.",
      "i" = "Returning data unchanged."
    ))
    return(df)
  }

  col_qc_warning <- check_npx_out_list$col_names$qc_warning

  # Check if the qc_warning column exists ----
  if (!col_qc_warning %in% colnames(df)) {
    cli::cli_warn(c(
      "Column {.var {col_qc_warning}} not found in data.",
      "i" = "Returning data unchanged."
    ))
    return(df)
  }

  # Summarize SampleQC statuses ----
  qc_summary <- table(
    toupper(trimws(df[[col_qc_warning]])),
    useNA = "ifany")
  qc_summary_msg <- paste(names(qc_summary),
                          qc_summary,
                          sep = ": ", collapse = "; ")
  cli::cli_inform("SampleQC status summary — {qc_summary_msg}")

  # Identify failed samples ----
  fail_idx <- stringr::str_detect(df[[col_qc_warning]],
                                  regex("^FAIL$", ignore_case = TRUE))
  n_fail <- sum(fail_idx, na.rm = TRUE)

  # Inform user and remove ----
  cli::cli_inform("Removing {n_fail} row{?s} with {.val SampleQC = 'FAIL'}")

  cleaned_df <- df[!fail_idx, ]
  return(cleaned_df)
}




