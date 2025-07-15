# Main Function -----------------------------------------------------------

#' Clean Olink NPX Data Frame
#'
#' @description
#' This function applies a series of cleaning steps to an NPX data based on
#' the results from [`check_npx()`]. It removes problematic samples and assays
#' to prepare a clean NPX data for downstream analysis, including duplicates
#' samples, controls samples, internal control assays, and samples or assays
#' with QC flag. Additionally, an instruction message is printed when user plan
#' to analyze absolute quantification value.
#'
#' The cleaning pipeline performs the following steps:
#'
#' 1. **Remove invalid Olink IDs**: Assays flagged as having invalid Olink IDs.
#' 2. **Remove assays with all NA values**: Assays without quantifiable data.
#' 3. **Remove duplicate Sample IDs**: Ensures uniqueness across samples.
#' 4. **Remove control samples**:
#'    - Based on `SampleType` (e.g., `"SAMPLE_CONTROL"`, `"PLATE_CONTROL"`,
#'    `"NEGATIVE_CONTROL"`).
#'    - Based on `SampleID` matching known control sample IDs.
#' 5. **Remove samples failing QC**: Samples with QC status `'FAIL'`.
#' 6. **Remove internal control assays**: Based on `AssayType` (e.g.,
#' `"ext_ctrl"`, `"inc_ctrl"`, `"amp_ctrl"`).
#' 7. **Remove assays flagged with assay warnings**.
#'
#' #' If the dataset includes absolute quantification, a message is shown
#' recommending log2 transformation for downstream analysis.
#'
#' @author
#' Kang Dong
#'
#' @param df A `tibble` or Arrow `Table` loaded from `read_npx()`.
#' @param check_log A list returned by [`check_npx()`]. If `NULL`,
#' `check_npx()`will be run internally using `df` and `preferred_names`.
#' @param preferred_names An optional named list to supply preferred column
#' names to `check_npx()`, if `check_log` is not provided.
#' @param keep_controls Character string indicating whether to retain any
#' controls:
#'   - `"sample"`: keep only sample-level controls,
#'   - `"assay"`: keep only assay-level controls,
#'   - `"both"`: keep all controls,
#'   - `NULL` (default): remove all controls.
#' @param control_sample_ids A character vector of `SampleID`s to remove.
#' Typically used to remove customer-provided or technical control samples.
#' @param out_df Output format: `"tibble"` (default) or `"arrow"`.
#' @param verbose Logical. If `TRUE` (default), prints step-wise CLI messages.
#'
#' @returns A cleaned NPX dataset in either tibble or Arrow Table format,
#' depending on `out_df`.
#'
#' @examples
#' \dontrun{
#' # Example 1. Run clean_npx() to check multiple errors
#' df <- dplyr::tibble(
#'   SampleID = c(
#'     "ValidSample",     # valid
#'     "InvalidOID",      # invalid OlinkID (too short)
#'     "AllNA",           # all NPX values NA for assay
#'     "DuplicateSample", # duplicate SampleID
#'     "ControlType",     # control SampleType
#'     "ControlID",       # control SampleID (e.g., contains 'control')
#'     "FailQC",          # QC_Warning is FAIL
#'     "ControlAssay",    # internal control assay
#'     "AssayWarn",       # flagged by AssayQC warning
#'     "DuplicateSample"  # duplicate SampleID
#'   ),
#'  OlinkID = c(
#'     "OID12345",  # valid (5 digits)
#'     "OID1234",   # invalid (only 4 digits)
#'     "OID23456",  # valid, but will be all NA
#'     "OID34567",  # valid
#'     "OID45678",  # valid
#'     "OID56789",  # valid
#'     "OID67890",  # valid
#'     "OID78901",  # valid
#'     "OID89012",  # valid
#'     "OID34567"   # valid
#'   ),
#'   NPX = c(
#'     10,   # valid
#'     11,   # still gets removed due to invalid OlinkID
#'     NA,   # all NA for assay
#'     12,   # duplicate SampleID ("DuplicateSample")
#'     13,   # control sample type
#'     14,   # control SampleID
#'     15,   # fails QC
#'     16,   # internal control assay
#'     17,   # flagged by assay warning
#'     12    # duplicate SampleID ("DuplicateSample")
#'   ),
#'   PlateID = rep("plate1", 10),
#'   SampleType = c(
#'     "SAMPLE",
#'     "SAMPLE",
#'     "SAMPLE",
#'     "SAMPLE",
#'     "PLATE_CONTROL",  # control sample
#'     "SAMPLE",
#'     "SAMPLE",
#'     "SAMPLE",
#'     "SAMPLE",
#'     "SAMPLE"
#'   ),
#'   AssayType = c(
#'     "assay",
#'     "assay",
#'     "assay",
#'     "assay",
#'     "assay",
#'     "assay",
#'     "assay",
#'     "ext_ctrl",   #control assay
#'     "assay",
#'     "assay"
#'   ),
#'   SampleQC = c(
#'     "PASS",
#'     "PASS",
#'     "PASS",
#'     "PASS",
#'     "PASS",
#'     "PASS",
#'     "FAIL",    #fails QC
#'     "PASS",
#'     "PASS",
#'     "PASS"
#'   ),
#'  AssayQC = c(
#'     "PASS",
#'     "PASS",
#'     "PASS",
#'     "PASS",
#'     "PASS",
#'     "PASS",
#'     "PASS",
#'     "PASS",
#'     "WARN", #to be removed by clean_assay_warning
#'     "PASS"
#'   )
#' )
#' )
#'
clean_npx <- function(
    df,
    check_log = NULL,
    preferred_names = NULL,
    keep_controls = NULL,  # options are c("sample", "assay", "both"),
    control_sample_ids = NULL,
    out_df = "tibble",
    verbose = TRUE
) {

  # Silence messages and warnings if requested ----
  if (!verbose) {
    withr::local_options(list(
      rlib_message_verbosity = "quiet",
      rlib_warning_verbosity = "quiet"
    ))
  }

  if(verbose) cli::cli_h2("Starting {.fn clean_npx} pipeline")


  # Validate input dataset ----
  check_is_dataset(df, error = TRUE)

  # Validate or generate check_log from check_npx() ----
  if (is.null(check_log)) {
    check_npx_log <- check_npx(df, preferred_names = preferred_names) |>
      suppressWarnings()
  } else if (check_is_list(check_log)) {
    check_npx_log <- check_log
  } else {
    cli::cli_abort(
      "{.arg check_log} must be the result of the {.fn check_npx} function."
    )
  }


  # Clean invalid Olink IDs ----
  if(verbose) cli::cli_h3("Cleaning assays with invalid OlinkIDs")
  df <- clean_invalid_oid(
    df,
    check_npx_log = check_npx_log,
    out_df = out_df
  )


  # Clean assays with all NA values ----
  if(verbose) cli::cli_h3("Cleaning assays with all NA values")
  df <- clean_assay_na(
    df,
    check_npx_log = check_npx_log,
    out_df = out_df
  )


  # Clean duplicate sample IDs ----
  if(verbose) cli::cli_h3("Cleaning duplicate SampleIDs")
  df <- clean_duplicate_sample_id(
    df,
    check_npx_log = check_npx_log,
    out_df = out_df
  )


  # Clean control samples based on sample type ----
  if(verbose) cli::cli_h3("Cleaning control samples based on sample type")

  if(is.null(keep_controls) || !keep_controls %in% c("sample", "both")) {
    keep_control_sample <- FALSE
  } else {
    keep_control_sample <-  TRUE
  }

  df <- clean_sample_type(
    df,
    check_npx_log = check_npx_log,
    keep_control_sample = keep_control_sample,
    out_df = out_df
  )


  # Clean control samples based on Sample ID ----
  if(verbose) cli::cli_h3("Cleaning control samples based on Sample ID")
  df <- clean_control_sample_id(
    df,
    check_npx_log = check_npx_log,
    control_sample_ids = control_sample_ids,
    out_df = out_df
  )


  # Clean Samples with QC Status 'FAIL' ----
  if(verbose) cli::cli_h3("Cleaning Samples with QC Status 'FAIL'")
  df <- clean_qc_warning(
    df,
    check_npx_log = check_npx_log,
    out_df = out_df
  )


  # Clean internal control assays ----
  if(verbose) cli::cli_h3("Cleaning internal control assays")

  if(is.null(keep_controls) || !keep_controls %in% c("assay", "both")) {
    keep_control_assay <- FALSE
  } else {
    keep_control_assay <-  TRUE
  }

  df <- clean_assay_type(
    df,
    check_npx_log = check_npx_log,
    keep_control_assay = keep_control_assay,
    out_df = out_df
  )


  # Clean assays flagged by assay warning ----
  if(verbose) cli::cli_h3("Cleaning assays flagged by assay warning")
  df <- clean_assay_warning(
    df,
    check_npx_log = check_npx_log,
    out_df = out_df
  )


  # Check for absolute quantification and apply log2 transformation
  if(check_npx_log$col_names$quant %in% c("Quantified",
                                          "Quantified_value",
                                          "quantified",
                                          "quantified_value")
  ){
    cli::cli_text("
    Detected absolute quant in column '{check_npx_log$col_names$quant}'.
    Applying log2 transformation is recommended before downstream analysis.
    For example:
    df <- df |>
    mutate(log2_value = log2(.data[['check_npx_log$col_names$quant']]))"
    )
  }


  # Final output
  cli::cli_inform("Completed {.fn clean_npx}. Returning `{out_df}` object.")

  return(
    df |>
      convert_read_npx_output(out_df = out_df)
  )

}


# Support Functions -------------------------------------------------------


#' Clean assays with only NA values
#'
#' @description
#' This function filters out rows from a `tibble` or `arrow` object where the
#' assay identifier (e.g.,`OlinkID`) matches those listed in
#' `check_npx_log$assay_na`, which contains assays composed entirely of NA
#'  values. It uses CLI messaging to report which assays were removed.
#'
#' @author
#' Kang Dong
#'
#' @param df A `tibble` or `arrow` object  loads from `read_npx()`, including a
#'  column identified by `check_npx_log$col_names$olink_id`.
#' @param check_npx_log A list generated by `check_npx()` function, containing:
#'   - `assay_na`: a character vector of `OlinkID` to be excluded.
#'   - `col_names$olink_id`: the column name in `df` that holds the `OlinkID`.
#' @param out_df Output format of the cleaned data. Options:
#' `"tibble"` (default) or `"arrow"`.
#'
#' @return A filtered `tibble` or `arrow` object with rows corresponding to
#' NA-only assays removed.
#'
#' @examples
#' \dontrun{
#' # Example 1: Exclude 1 assay with only NA values
#' df <- dplyr::tibble(
#'   SampleID = c("A", "B", "C", "D"),
#'   OlinkID = rep("OID12345", 4L),
#'   SampleType = rep("SAMPLE", 4L),
#'   NPX = NA,
#'   PlateID = rep("plate1", 4L),
#'   QC_Warning = rep("Pass", 4L),
#'   LOD = rnorm(4L)
#' )
#'
#' log <- OlinkAnalyze::check_npx(df)
#' out <- OlinkAnalyze::clean_assay_na(df, check_npx_log = log)
#' }
clean_assay_na <- function(
    df,
    check_npx_log,
    out_df = "tibble") {

  # If there are no assays with all NA values, skip filtering
  if (length(check_npx_log$assay_na) == 0) {
    cli::cli_inform(
      "No assays with only NA values found. Returning original data frame."
    )
    return(
      df |>
        convert_read_npx_output(out_df = out_df)
    )
  }

  # CLI message listing excluded assays
  cli::cli_inform(
    "Excluding {length(check_npx_log$assay_na)} assay{?s} with only NA values:
    {paste(check_npx_log$assay_na, collapse = ', ')}"
  )

  # Exclude assays with only NA values
  df_cleaned <- df |>
    dplyr::filter(
      !.data[[check_npx_log$col_names$olink_id]] %in% check_npx_log$assay_na
      )

  cli::cli_inform(
    "Removed rows for assays with only NA values.Returning cleaned data table."
  )

  # Convert output to desired format (tibble or arrow)
  return(
    df_cleaned |>
      convert_read_npx_output(out_df = out_df)
  )
}


#' Clean assays with invalid OlinkID
#'
#' @description
#' This function filters out rows from a NPX data where the assay identifier
#' column (e.g.,`OlinkID`) matches values listed in `check_npx_log$oid_invalid`,
#' which identifies invalid or malformed assay identifiers. Uses CLI messages to
#' inform users of what was excluded.
#'
#' @author
#' Kang Dong
#'
#' @param df A `tibble` or `arrow` object loaded from `read_npx()`, containing
#' a column specified by `check_npx_log$col_names$olink_id`.
#' @param check_npx_log A list returned by `check_npx()`, containing:
#'   - `oid_invalid`: a character vector of invalid assay identifiers to be
#'   removed.
#'   - `col_names$olink_id`: the name of the column in `df` that holds the
#'   assay IDs.
#' @param out_df Output format of the cleaned data.
#' Options: `"tibble"` (default) or `"arrow"`.
#'
#' @return A filtered `tibble` or `arrow` object with invalid OlinkIDs removed.
#'
#' @examples
#' \dontrun{
#' # Example 1: Exclude invalid OlinkID using clean_invalid_oid()
#' df <- dplyr::tibble(
#'   SampleID = c("A", "B", "C", "D"),
#'   OlinkID = c(rep("OID12345", 2L), rep("OID123456", 2L)),
#'   SampleType = rep("SAMPLE", 4L),
#'   NPX = c(rep(1.0, 2L), rep(2.0, 2L)),
#'   PlateID = rep("plate1", 4L),
#'   QC_Warning = rep("Pass", 4L),
#'   LOD = rep(1.0, 4L)
#' )
#'
#' log <- suppressWarnings(OlinkAnalyze::check_npx(df))
#' out <- OlinkAnalyze::clean_invalid_oid(df, check_npx_log = log)
#' }
clean_invalid_oid <- function(
    df,
    check_npx_log,
    out_df = "tibble") {

  # Check if there are any invalid OlinkIDs to remove
  if (length(check_npx_log$oid_invalid) == 0) {
    cli::cli_inform(
      "No invalid OlinkIDs found. Returning original data frame."
    )
    return(
      df |>
        convert_read_npx_output(out_df = out_df)
    )
  }

  # Inform user of which assays will be excluded
  cli::cli_inform(
    "Excluding {length(check_npx_log$oid_invalid)} assay{?s} with invalid
    OlinkIDs: {paste(check_npx_log$oid_invalid, collapse = ', ')}"
  )

  # Remove rows where the OlinkID is invalid
  df_cleaned <- df |>
    dplyr::filter(
      !.data[[check_npx_log$col_names$olink_id]] %in% check_npx_log$oid_invalid
      )

  # Confirmation message
  cli::cli_inform(
    "Removed rows for assays with invalid OlinkIDs.
    Returning cleaned data table."
  )

  # Return cleaned data frame in desired format
  return(
    df_cleaned |>
      convert_read_npx_output(out_df = out_df)
  )
}


#' Clean samples with duplicate SampleIDs
#'
#' @description
#' This function filters out rows from a NPX data where the sample identifier
#' (e.g., `SampleID`) appears more than once. The duplicate SampleIDs are
#' identified in `check_npx_log$sample_id_dups`, which is generated by the
#' `check_npx()` function. Uses CLI messages to inform users of exclusions.
#'
#' @author
#' Kang Dong
#'
#' @param df A `tibble` or `arrow` object loaded from `read_npx()`,
#' containing a column specified by `check_npx_log$col_names$sample_id`.
#' @param check_npx_log A list returned by `check_npx()`, containing:
#'   - `sample_id_dups`: a character vector of duplicate SampleIDs to
#'   be removed.
#'   - `col_names$sample_id`: the name of the column in `df` that holds
#'   the SampleIDs.
#' @param out_df Output format of the cleaned data.
#' Options: `"tibble"` (default) or `"arrow"`.
#'
#' @return A filtered tibble or arrow object with duplicated SampleIDs removed.
#'
#' @examples
#' \dontrun{
#' # Example 1: Exclude 1 sample with duplicate SampleID
#' df <- dplyr::tibble(
#'   SampleID = c("A", "B", "C", "C"),
#'   OlinkID = c(rep("OID12345", 2L), rep("OID12345", 2L)),
#'   SampleType = rep("SAMPLE", 4L),
#'   NPX = c(rep(1.0, 2L), rep(2.0, 2L)),
#'   PlateID = rep("plate1", 4L),
#'   QC_Warning = rep("Pass", 4L),
#'   LOD = rep(1.0, 4L)
#' )
#' log <- OlinkAnalyze::check_npx(df)
#' clean_df <- OlinkAnalyze::clean_duplicate_sample_id(df, log)
#' }
clean_duplicate_sample_id <- function(
    df,
    check_npx_log,
    out_df = "tibble") {

  # Check if there are any duplicate SampleIDs to remove
  if (length(check_npx_log$sample_id_dups) == 0) {
    cli::cli_inform(
      "No duplicate SampleIDs found. Returning original data frame."
    )
    return(
      df |>
        convert_read_npx_output(out_df = out_df)
    )
  }

  # Inform user about excluded SampleIDs
  cli::cli_inform(
    "Excluding {length(check_npx_log$sample_id_dups)} sample{?s} with duplicate
    SampleIDs: {paste(check_npx_log$sample_id_dups, collapse = ', ')}"
  )

  # Filter out rows with duplicate SampleIDs
  df_cleaned <- df |>
    dplyr::filter(
      !.data[[check_npx_log$col_names$sample_id]] %in%
        check_npx_log$sample_id_dups
      )

  # Success message
  cli::cli_inform(
    "Removed rows with duplicate SampleIDs. Returning cleaned data table."
  )

  # Convert and return the output in the desired format
  return(
    df_cleaned |>
      convert_read_npx_output(out_df = out_df)
  )
}


#' Clean control samples based on sample type
#'
#' @description
#' This function filters out rows from an NPX dataset where the sample type
#' column matches known control sample types: `"SAMPLE_CONTROL"`, `"PLATE_CONTROL"`,
#' or `"NEGATIVE_CONTROL"`. If `keep_control_sample` is `TRUE`, or if the
#' sample type column is not found in the `check_npx_log`, the function
#' returns the original data unchanged. The function outputs a cleaned dataset
#' in either `tibble` or `arrow` format, depending on user preference.
#'
#' @author
#' Kang Dong
#'
#' @param df A `tibble` or `arrow` object loaded from `read_npx()`.
#' @param check_npx_log A list returned from `check_npx()`, containing:
#'   - `col_names$sample_type`: name of the column in `df` that identifies
#'   sample type.
#' @param keep_control_sample Logical; if `TRUE`, control samples are retained
#' and no filtering is applied. Defaults to `FALSE`.
#' @param out_df Output format: either "tibble" (default) or "arrow".
#'
#' @return A cleaned tibble or arrow object with control sample removed.
#' @examples
#' \dontrun{
#' # Example 1: Run clean_sample_type() with default setting
#' df <- dplyr::tibble(
#'   SampleID = c("A", "B", "C", "D"),
#'   OlinkID = rep("OID12345", 4L),
#'   SampleType = c(
#'     "SAMPLE",
#'     "SAMPLE_CONTROL",
#'     "PLATE_CONTROL",
#'     "NEGATIVE_CONTROL"
#'   ),
#'   NPX = rep(1, 4L),
#'   PlateID = rep("plate1", 4L),
#'   QC_Warning = rep("Pass", 4L),
#'   LOD = rep(1, 4L)
#' )
#' log <- OlinkAnalyze::check_npx(df)
#' clean_df <- OlinkAnalyze::clean_sample_type(df, log)
#'
#' # Example 2: Run control_sample_types = NULL to  return the unchanged data
#' clean_df <- OlinkAnalyze::clean_sample_type(df,
#'   log,
#'   keep_control_sample = FALSE
#' )
#' }

clean_sample_type <- function(
    df,
    check_npx_log,
    keep_control_sample = FALSE,
    out_df = "tibble"
) {

  # Return original data if user chooses to keep control samples
  if (keep_control_sample) {
    cli::cli_inform(
      "Control samples are retained as per user input. Returning data unchanged"
      )
    return(
      df |>
        convert_read_npx_output(out_df = out_df)
    )
  }

  # Check if 'sample_type' column name is available
  if (!"sample_type" %in% names(check_npx_log$col_names)) {
    cli::cli_inform(
      "No column name found for {.var sample_type} in
      {.code check_npx_log$col_names}.",
      "i" = "Returning data unchanged."
    )
    return(
      df |>
        convert_read_npx_output(out_df = out_df)
    )
  }

  # list control sample types to be excluded
  ctrl_sample_type <- c("SAMPLE_CONTROL", "PLATE_CONTROL", "NEGATIVE_CONTROL")

  # Filter out control samples
  df_cleaned <- df |>
    dplyr::filter(
      !.data[[check_npx_log$col_names$sample_type]] %in% ctrl_sample_type
    )

  cli::cli_inform(
    "Control samples: {.val {paste(ctrl_sample_type, collapse = ', ')}} removed.
    Returning cleaned data table."
  )

  # Format and return output
  return(
    df_cleaned |>
      convert_read_npx_output(out_df = out_df)
  )
}


#' Clean Internal Control Assays by Assay Type
#'
#' @description
#' This function filters out internal control assays (`ext_ctrl`, `inc_ctrl`,
#' `amp_ctrl`) from the NPX data, unless user specified to retain them. The
#' function uses column mapping provided in the `check_npx_log` and returns
#' the `tibble` or `arrow` object as specified by the user.
#'
#' @author
#' Kang Dong
#'
#' @param df A `tibble` or `arrow` object loaded from `read_npx()`.
#' @param check_npx_log A list generated by `check_npx()` that includes:
#'   - `col_names$assay_type`: the name of the column holding assay type
#'   information.
#' @param keep_control_assay Logical; if `TRUE`, internal control assays are
#' retained and no filtering is applied. Defaults to `FALSE`.
#' @param out_df Output format: either "tibble" (default) or "arrow".
#'
#' @return A cleaned tibble or arrow object with control assays removed.
#'
#' @examples
#' \dontrun{
#'
#' df <- dplyr::tibble(
#'   SampleID = c("A", "B", "C", "D"),
#'   OlinkID = rep("OID12345", 4L),
#'   NPX = rep(1, 4L),
#'   PlateID = rep("plate1", 4L),
#'   QC_Warning = rep("Pass", 4L),
#'   LOD = rep(1, 4L),
#'   AssayType = c(
#'     "assay",
#'     "ext_ctrl",
#'     "inc_ctrl",
#'     "amp_ctrl"
#'   )
#' )
#'
#' log <- OlinkAnalyze::check_npx(df)
#'
#' # Example 1: Run clean_assay_type() as default.
#' # Internal control assay are removed
#' clean_df <- OlinkAnalyze::clean_assay_type(df, log)
#'
#' # Example 2: Set keep_control_assay = TRUE Return unchanged data
#' clean_df <- OlinkAnalyze::clean_assay_type(df,
#'   log,
#'   keep_control_assay = TRUE
#' )
#' }

clean_assay_type <- function(
    df,
    check_npx_log,
    keep_control_assay = FALSE,
    out_df = "tibble"
){

  # Return original data if user chooses to keep control samples
  if (keep_control_assay) {
    cli::cli_inform(
      "Control assays (inc_ctrl, ext_ctrl, amp_ctrl) are retained as per
      user input. Returning data unchanged."
      )
    return(
      df |>
        convert_read_npx_output(out_df = out_df)
    )
  }

  # Check if 'assay_type' column name is available
  if (!"assay_type" %in% names(check_npx_log$col_names)) {
    cli::cli_inform(
      "No column name found for {.var assay_type} in
      {.code check_npx_log$col_names}.",
      "i" = "Returning data unchanged."
    )
    return(
      df |>
        convert_read_npx_output(out_df = out_df)
    )
  }

  # List control assay types to be excluded
  ctrl_assay_type <- c("ext_ctrl", "inc_ctrl", "amp_ctrl")

  # Filter out control assays
  df_cleaned <- df |>
    dplyr::filter(
      !.data[[check_npx_log$col_names$assay_type]] %in% ctrl_assay_type
    )

  cli::cli_inform(
    "Control assays: {.val {paste(ctrl_assay_type, collapse = ', ')}} removed.
    Returning cleaned data table."
  )

  # Format and return output
  return(
    df_cleaned |>
      convert_read_npx_output(out_df = out_df)
  )
}


#' Clean Samples with QC Status 'FAIL'
#'
#' @description
#' This function use the QC warning column identified by `check_npx_log` and
#' removes samples flagged `qc_warning` = "FAIL" from the NPX data. The function
#' returns the cleaned NPX data in the the desired output format (`"tibble"` or
#' `"arrow"`).
#'
#' @author
#' Kang Dong
#'
#' @param df A `tibble` or `arrow` object loaded using `read_npx()`, containing
#' a QC status column.
#' @param check_npx_log A list generated by `check_npx()` that includes:
#'   - `col_names$qc_warning`: the name of the column indicating QC status.
#' @param out_df A string specifying the output format of the data frame.
#'   Options are `"tibble"` or `"arrow"`. Default is `"tibble"`.
#'
#' @returns A cleaned tibble or arrow object with failed QC sample removed.
#'
#' @examples
#' \dontrun{
#' # Example 1: Remove 1 sample that failed sample QC
#'
#' df <- dplyr::tibble(
#'   SampleID = c("A", "B", "C", "D"),
#'   OlinkID = rep("OID12345", 4L),
#'   NPX = rep(1, 4L),
#'   PlateID = rep("plate1", 4L),
#'   QC_Warning = c(rep("Pass", 3L), "FAIL"),
#'   LOD = rep(1, 4L)
#' )
#'
#' log <- OlinkAnalyze::check_npx(df)
#' clean_df <- OlinkAnalyze::clean_qc_warning(df, log)
#' }

clean_qc_warning <- function(
    df,
    check_npx_log,
    out_df = "tibble") {

  # Check if qc_warning column name is defined in check_npx_log
  if (!"qc_warning" %in% names(check_npx_log$col_names)) {
    cli::cli_inform(
      "No column name found for {.var qc_warning} in
      {.code check_npx_log$col_names}.",
      "i" = "Returning data unchanged."
    )
    return(
      df |>
        convert_read_npx_output(out_df = out_df)
    )
  }

  # Filter out failed samples and return cleaned data
  cli::cli_inform(
    "Samples flaged {.field {check_npx_log$col_names$qc_warning}} =
    {.val 'FAIL'} Removed. Returning cleaned data table."
  )

  df_cleaned <- df |>
    dplyr::filter(
      !.data[[check_npx_log$col_names$qc_warning]] %in% c("FAIL")
      )

  return(
    df_cleaned |>
      convert_read_npx_output(out_df = out_df)
  )
}


#' Clean Assays Flagged with Assay Warning
#'
#' @description
#' Removes assays from the NPX data frame where the Assay warning column (as
#' identified in `check_npx_log`) has a value of `"WARN"`. The function is used
#' to clean assay-level QC warnings from the dataset before analysis.
#'
#' @param df A data frame or tibble containing NPX data.
#' @param check_npx_log A list generated by `check_npx()` that includes:
#'   - `col_names$assay_warning`: the name of the column indicating QC status.
#' @param out_df A string indicating the desired output format. Passed to
#' `convert_read_npx_output()`. Common values include `"tibble"` and `"arrow"`.
#'
#' @return A cleaned tibble or arrow object with warning assay removed.
#'
#' @examples
#' \dontrun{
#'  # Example 1: Remove 1 assay that flagged AssayQC = WARN
#'
#' df <- dplyr::tibble(
#'   SampleID = c("A", "B", "C", "D"),
#'   OlinkID = c(rep("OID1111", 3L), rep("OID22222", 1L)),
#'   NPX = rep(1, 4L),
#'   PlateID = rep("plate1", 4L),
#'   AssayQC = c(rep("Pass", 3L), "WARN"),
#'   LOD = rep(1, 4L)
#' )
#'
#' log <- OlinkAnalyze::check_npx(df)
#' clean_df <- OlinkAnalyze::clean_assay_warning(df, log)
#' }

clean_assay_warning <- function(
    df,
    check_npx_log,
    out_df = "tibble") {

  # Check if assay_warn column name is defined
  if (!"assay_warn" %in% names(check_npx_log$col_names)) {
    cli::cli_inform(
      "No column name found for {.var assay_warn} in
      {.code check_npx_log$col_names}.",
      "i" = "Returning data unchanged."
    )
    return(
      df |>
        convert_read_npx_output(out_df = out_df)
    )
  }

  # Filter out failed assays and return cleaned data
  cli::cli_inform(
    "Assays with {.field {check_npx_log$col_names$assay_warn}} =
    {.val 'WARN'} removed. Returning cleaned data frame."
  )

  df_cleaned <- df |>
    dplyr::filter(
      !.data[[check_npx_log$col_names$assay_warn]] %in% c("WARN", "Warning")
    )

  return(
    df_cleaned |>
      convert_read_npx_output(out_df = out_df)
  )
}


#' Clean Control Samples Based on Sample ID
#'
#' @description
#' This function removes rows from NPX data where the sample ID column (as
#' defined in `check_npx_log`) matches any of the provided control sample IDs.
#' This is useful for filtering out technical or customer control samples prior
#' to downstream analysis.
#'
#' @author
#' Kang Dong
#'
#' @param df A `tibble` or `arrow` object loaded using `read_npx()`, including
#' a sample ID column.
#' @param check_npx_log A list generated by `check_npx()` that includes:
#'   - `col_names$sample_id`: the column name in `df` identifying sample IDs.
#' @param control_sample_idA character vector of control sample IDs,
#' e.g., `c("control_a", "control_b")`. Default is `NULL`, in which case the
#' data is returned unchanged.
#' @param out_df Output format of the returned data frame.
#'   Options: `"tibble"` or `"arrow"`. Default is `"tibble"`.
#'
#' @returns A filtered data table with control samples removed.
#'
#' @examples
#' \dontrun{
#' # Example 1: use npx_data1 to check that clean_control_sample_id() works
#' log <- OlinkAnalyze::npx_data1 |>
#'   OlinkAnalyze::check_npx() |>
#'   suppressWarnings()
#'
#' out <- OlinkAnalyze::clean_control_sample_id(npx_data1,
#'   check_npx_log = log,
#'   control_sample_id = c("CONTROL_SAMPLE_AS 1", "CONTROL_SAMPLE_AS 2")
#' )
#'
#' # Example 2: check default setting of clean_control_sample_id(). Return data
#' # unchanged.
#' out <- OlinkAnalyze::clean_control_sample_id(npx_data1,
#'   check_npx_log = log,
#'   control_sample_id = NULL
#' )
#' }

clean_control_sample_id <- function(
    df,
    check_npx_log,
    control_sample_ids = NULL,
    out_df = "tibble") {

  # Early exit check
  if (is.null(control_sample_ids) ||
    !"sample_id" %in% names(check_npx_log$col_names)) {
    if (is.null(control_sample_ids)) {
      cli::cli_inform(
        "No control sample ID pattern provided. Returning data unchanged."
      )
    } else {
      cli::cli_inform(c(
        "No column name found for {.var sample_id} in
        {.code check_npx_log$col_names}.",
        "i" = "Returning data unchanged."
      ))
    }
    return(
      df |>
        convert_read_npx_output(out_df = out_df)
    )
  }

  # Filter out control samples and return cleaned data
  cli::cli_inform(
    "Control sample: {.val {paste(control_sample_ids, collapse = ', ')}} removed.
    Returning cleaned data table."
  )

  df_cleaned <- df |>
    dplyr::filter(
      !.data[[check_npx_log$col_names$sample_id]] %in% control_sample_ids
      )

  return(
    df_cleaned |>
      convert_read_npx_output(out_df = out_df)
  )
}
