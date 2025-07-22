#' Function Cleaning NPX data
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
#' 7. **Remove assays flagged with assay QC warnings**.
#' 8. **Correct column class**: Ensures columns following expected column class
#' defined in `column_name_dict`.
#'
#' #' If the dataset includes absolute quantification, a message is shown
#' recommending log2 transformation for downstream analysis.
#'
#' @author
#'   Kang Dong
#'   Klev Diamanti
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
#' @param verbose Logical. If `FALSE` (default), Silences step-wise CLI
#' messages.
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
#'   ),
#'    NPX = replace(x = rnorm(n = 10L), list = 3L, values = NA_real_),
#'    PlateID = rep("plate1", 10L),
#'    UniProt = rep("uniprotid1", 10L),
#'    Assay = rep("assay_a", 10L),
#'    Panel = rep("panel_a", 10L),
#'    PanelVersion = rep("panel_version_a", 10L),
#'    LOD = rnorm(10L),
#'    ExtNPX = rnorm(10L),
#'    Count = rnorm(10L),
#'    Normalization = rep("Intensity", 10L)
#' )
#' }
#'
clean_npx <- function(df,
                      check_log = NULL,
                      preferred_names = NULL,
                      control_sample_ids = NULL,
                      remove_assay_na = TRUE,
                      remove_invalid_oid = TRUE,
                      remove_dup_sample_id = TRUE,
                      remove_control_assay = TRUE,
                      remove_control_sample = TRUE,
                      remove_qc_warning = FALSE,
                      remove_assay_warning = TRUE,
                      out_df = "tibble",
                      verbose = FALSE) {

  # Silence messages and warnings if requested
  if (!verbose) {
    withr::local_options(list(
      rlib_message_verbosity = "quiet",
      rlib_warning_verbosity = "quiet"
    ))
  }

  if (verbose) cli::cli_h2("Starting {.fn clean_npx} pipeline")

  # Validate input dataset
  check_is_dataset(df = df, error = TRUE)

  check_is_scalar_boolean(bool = verbose, error = TRUE)

  # Validate or generate check_log from check_npx()
  if (is.null(check_log)) {
    cli::cli_inform("{.arg check_log} is not provided.
                    Running {.fn check_npx}.")
    check_npx_log <- check_npx(df,
                               preferred_names = preferred_names) |>
      suppressWarnings()

  } else if (check_is_list(check_log)) {
    check_npx_log <- check_log

  } else {
    cli::cli_abort(
      "{.arg check_log} must be the result of the {.fn check_npx} function."
    )

  }

  # Clean invalid Olink IDs
  if (verbose) cli::cli_h3("Cleaning assays with invalid OlinkIDs")
  df <- clean_invalid_oid(
    df = df,
    check_npx_log = check_npx_log,
    remove_invalid_oid = remove_invalid_oid,
    verbose = verbose
  )

  # Clean assays with all NA values
  if (verbose) cli::cli_h3("Cleaning assays with all NA values")
  df <- clean_assay_na(
    df = df,
    check_npx_log = check_npx_log,
    remove_assay_na = remove_assay_na,
    verbose = verbose
  )

  # Clean duplicate sample IDs
  if (verbose) cli::cli_h3("Cleaning duplicate SampleIDs")
  df <- clean_duplicate_sample_id(
    df = df,
    check_npx_log = check_npx_log,
    remove_dup_sample_id = remove_dup_sample_id,
    verbose = verbose
  )

  # Clean control samples based on sample type
  if (verbose) cli::cli_h3("Cleaning control samples based on sample type")
  df <- clean_sample_type(
    df = df,
    check_npx_log = check_npx_log,
    remove_control_sample = remove_control_sample,
    verbose = verbose
  )

  # Clean control samples based on Sample ID
  if (verbose) cli::cli_h3("Cleaning control samples based on Sample ID")
  df <- clean_control_sample_id(
    df = df,
    check_npx_log = check_npx_log,
    control_sample_ids = control_sample_ids,
    verbose = verbose
  )

  # Clean Samples with QC Status 'FAIL'
  if (verbose) cli::cli_h3("Cleaning Samples with QC Status 'FAIL'")
  df <- clean_qc_warning(
    df = df,
    check_npx_log = check_npx_log,
    remove_qc_warning = remove_qc_warning,
    verbose = verbose
  )

  # Clean internal control assays
  if (verbose) cli::cli_h3("Cleaning internal control assays")
  df <- clean_assay_type(
    df = df,
    check_npx_log = check_npx_log,
    remove_control_assay = remove_control_assay,
    verbose = verbose
  )

  # Clean assays flagged by assay warning
  if (verbose) cli::cli_h3("Cleaning assays flagged by assay warning")
  df <- clean_assay_warning(
    df = df,
    check_npx_log = check_npx_log,
    remove_assay_warning = remove_assay_warning,
    verbose = verbose
  )

  # Correct column class
  if (verbose) cli::cli_h3("Correcting flagged column class")
  df <- clean_col_class(
    df = df,
    check_npx_log = check_npx_log,
    verbose = verbose
  )

  # Check for absolute quantification and apply log2 transformation
  if (grepl(pattern = "Quantified",
            x = check_npx_log$col_names$quant,
            ignore.case = TRUE)) {
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
    convert_read_npx_output(
      df = df,
      out_df = out_df
    )
  )
}

# Help Functions ----------------------------------------------------------

#' Help function removing assays with all quantified values `NA`.
#'
#' @description
#' This function filters out rows from a `tibble` or `arrow` object where the
#' assay identifier (one of
#' `r ansi_collapse_quot(x = column_name_dict$col_names$olink_id, sep = "or")`)
#' matches those listed in `check_npx_log$assay_na`, which contains assays
#' composed entirely of `NA` values in their quantification column.
#'
#' @author
#'   Kang Dong
#'   Klev Diamanti
#'
#' @param df A `tibble` or `arrow` object loads from `read_npx()`, including a
#' column identified by
#' `r ansi_collapse_quot(x = column_name_dict$col_names$olink_id, sep = "or")`.
#' @param check_npx_log A named list generated by the function `check_npx()`,
#' containing:
#' \itemize{
#' \item `assay_na`: a character vector of assay identifiers with all quantified
#' values `NA`.
#' \item `col_names$olink_id`: the column name of the assay identifier in the
#' dataset.
#' }
#' @param remove_assay_na Logical. If `FALSE`, skips filtering assays with all
#' quantified values `NA`. Defaults to `TRUE`.
#' @param verbose Logical. If `FALSE` (default), silences step-wise CLI
#' messages.
#'
#' @return A `tibble` or `arrow` object with rows corresponding to assays with
#' all quantified values `NA` removed.
#'
clean_assay_na <- function(df,
                           check_npx_log,
                           remove_assay_na = TRUE,
                           verbose = FALSE) {
  # input check
  check_is_scalar_boolean(
    bool = remove_assay_na,
    error = TRUE
  )

  # If assays with all NA values are retained by the user
  if (remove_assay_na == FALSE) {
    if (verbose == TRUE) {
      cli::cli_inform(
        c("Skipping exclusion of assays with all quantified values {.val NA} as
        per user input: {.field remove_assay_na} = {.val {FALSE}}.",
          "i" = "Returning original dataset.")
      )
    }
    return(df)
  }

  # Id not assays with all values NA
  if (length(check_npx_log$assay_na) == 0L) {
    if (verbose == TRUE) {
      cli::cli_inform(
        c("No assays with only {.val NA} values.",
          "i" = "Returning original dataset.")
      )
    }
    return(df)
  }

  # CLI message listing excluded assays
  cli::cli_inform(
    c(
      "Excluding {.val {length(check_npx_log$assay_na)}} assay{?s} with only
    {.val NA} values: {.val {check_npx_log$assay_na}}.",
      "v" = "Returning cleaned dataset."
    )
  )

  # Exclude assays with only NA values
  df_cleaned <- df |>
    dplyr::filter(
      !(.data[[check_npx_log$col_names$olink_id]] %in% check_npx_log$assay_na)
    )

  # Convert output to desired format (tibble or arrow)
  return(df_cleaned)
}

#' Help function removing assays with invalid identifiers.
#'
#' @description
#' This function filters out rows from a `tibble` or `arrow` object where the
#' assay identifier (one of
#' `r ansi_collapse_quot(x = column_name_dict$col_names$olink_id, sep = "or")`)
#' matches values listed in `check_npx_log$oid_invalid`, which identifies
#' invalid or malformed assay identifiers.
#'
#' @author
#'   Kang Dong
#'   Klev Diamanti
#'
#' @param df A `tibble` or `arrow` object loaded from `read_npx()`, including a
#' column identified by
#' `r ansi_collapse_quot(x = column_name_dict$col_names$olink_id, sep = "or")`.
#' @param check_npx_log A named list generated by the function `check_npx()`,
#' containing:
#' \itemize{
#' \item `oid_invalid`: a character vector of invalid assay identifiers.
#' \item `col_names$olink_id`: the column name of the assay identifier in the
#' dataset.
#' }
#' @param remove_invalid_oid Logical. If `FALSE`, skips filtering assays with
#' invalid identifiers. Defaults to `TRUE`.
#' @param verbose Logical. If `FALSE` (default), silences step-wise CLI
#' messages.
#'
#' @return A `tibble` or `arrow` object with rows corresponding to assays with
#' invalid identifiers removed.
#'
clean_invalid_oid <- function(df,
                              check_npx_log,
                              remove_invalid_oid = TRUE,
                              verbose = FALSE) {
  # input check
  check_is_scalar_boolean(
    bool = remove_invalid_oid,
    error = TRUE
  )

  # Keep invalid assay identifiers if user indicates so
  if (remove_invalid_oid == FALSE) {
    if (verbose == TRUE) {
      cli::cli_inform(
        c("Skipping exclusion of assays with invalid identifiers as per user
          input: {.field remove_invalid_oid} = {.val {FALSE}}.",
          "i" = "Returning original dataset.")
      )
    }
    return(df)
  }

  # Check if there are any invalid assay identifiers to remove
  if (length(check_npx_log$oid_invalid) == 0L) {
    if (verbose == TRUE) {
      cli::cli_inform(
        c("No invalid assay identifiers.",
          "i" = "Returning original dataset.")
      )
    }
    return(df)
  }

  # Inform user of which assays will be excluded
  cli::cli_inform(
    c(
      "Excluding {.val {length(check_npx_log$oid_invalid)}} assay{?s} with
      invalid identifier{?s}: {.val {check_npx_log$oid_invalid}}.",
      "v" = "Returning cleaned dataset."
    )
  )

  # Remove rows where the OlinkID is invalid
  df_cleaned <- df |>
    dplyr::filter(
      !(.data[[check_npx_log$col_names$olink_id]]
        %in% check_npx_log$oid_invalid)
    )

  # Return cleaned data frame in desired format
  return(df_cleaned)
}

#' Help function removing samples with duplicate identifiers.
#'
#' @description
#' This function filters out rows from a `tibble` or `arrow` object where the
#' sample identifier (one of
#' `r ansi_collapse_quot(x = column_name_dict$col_names$sample_id, sep = "or")`)
#' matches values listed in `check_npx_log$sample_id_dups`, which identifies
#' samples with duplicated identifiers.
#'
#' @author
#'   Kang Dong
#'   Klev Diamanti
#'
#' @param df A `tibble` or `arrow` object loaded from `read_npx()`, including a
#' column identified by
#' `r ansi_collapse_quot(x = column_name_dict$col_names$sample_id, sep = "or")`.
#' @param check_npx_log A named list generated by the function `check_npx()`,
#' containing:
#' \itemize{
#' \item `sample_id_dups`: a character vector of duplicated sample identifiers.
#' \item `col_names$sample_id`: the column name of the sample identifier in the
#' dataset.
#' }
#' @param remove_dup_sample_id Logical. If `FALSE`, skips filtering samples with
#' duplicate sample identifiers. Defaults to `TRUE`.
#' @param verbose Logical. If `FALSE` (default), silences step-wise CLI
#' messages.
#'
#' @return A `tibble` or `arrow` object with rows corresponding to samples with
#' duplicated identifiers removed.
#'
clean_duplicate_sample_id <- function(df,
                                      check_npx_log,
                                      remove_dup_sample_id = TRUE,
                                      verbose = FALSE) {
  # input check
  check_is_scalar_boolean(
    bool = remove_dup_sample_id,
    error = TRUE
  )

  # Retain samples with duplicate identifiers
  if (remove_dup_sample_id == FALSE) {
    if (verbose == TRUE) {
      cli::cli_inform(
        c("Skipping exclusion of samples with duplicate identifiers as per user
          input: {.field remove_dup_sample_id} = {.val {FALSE}}.",
          "i" = "Returning original dataset.")
      )
    }
    return(df)
  }

  # Check if there are any samples with duplicate identifiers to remove
  if (length(check_npx_log$sample_id_dups) == 0L) {
    if (verbose == TRUE) {
      cli::cli_inform(
        c("No duplicate sample identifiers.",
          "i" = "Returning original dataset.")
      )
    }
    return(df)
  }

  # Inform user about excluded SampleIDs
  cli::cli_inform(
    c(
      "Excluding {.val {length(check_npx_log$sample_id_dups)}} sample{?s} with
    duplicate identifier{?s}: {.val {check_npx_log$sample_id_dups}}.",
      "v" = "Returning cleaned dataset."
    )
  )

  # Filter out rows with duplicate SampleIDs
  df_cleaned <- df |>
    dplyr::filter(
      !(.data[[check_npx_log$col_names$sample_id]]
        %in% check_npx_log$sample_id_dups)
    )

  # Convert and return the output in the desired format
  return(df_cleaned)
}

#' Help function removing control samples based on sample type.
#'
#' @description
#' This function filters out rows from a dataset where the sample type column
#' matches known control sample types: `"SAMPLE_CONTROL"`, `"PLATE_CONTROL"` or
#' `"NEGATIVE_CONTROL"`. If `keep_control_sample` is set to `TRUE`, or if the
#' sample type column is present in the `check_npx_log`, the function returns
#' the original data unchanged.
#'
#' @author
#'   Kang Dong
#'   Klev Diamanti
#'
#' @param df A `tibble` or `arrow` object loaded from `read_npx()`.
#' @param check_npx_log A named list generated by the function `check_npx()`,
#' containing:
#' \itemize{
#' \item `col_names$sample_id`: the column name of the sample identifier in the
#' dataset.
#' \item `col_names$sample_type`: the column name of the sample type in the
#' dataset. \strong{If column is missing then function will return the original
#' dataset.}
#' }
#' @param remove_control_sample If `FALSE`, all control samples are retained. If
#' `TRUE`, all control samples are removed. Alternatively, a character vector
#' with one or more of `r ansi_collapse_quot(x = names(olink_sample_types))`
#' indicating the sample types to remove.
#' @param verbose Logical. If `FALSE` (default), silences step-wise CLI
#' messages.
#'
#' @return A `tibble` or `arrow` object with rows corresponding to control
#' samples removed.
#'
clean_sample_type <- function(df,
                              check_npx_log,
                              remove_control_sample = TRUE,
                              verbose = FALSE) {
  # if remove_control_sample is boolean then we either remove all control
  # samples (when TRUE), or we keep samples (when FALSE).
  # When remove_control_sample is a character vector, then we remove all user
  # designated controls.
  if (check_is_scalar_boolean(bool = remove_control_sample, error = FALSE)) {

    if (remove_control_sample == TRUE) {
      ctrl_sample_type <- olink_sample_types[
        !(names(olink_sample_types) %in% c("sample"))
      ] |>
        unlist() |>
        unname()
    } else {
      ctrl_sample_type <- character(0L)
    }

  } else if (check_is_character(string = remove_control_sample, error = TRUE)) {

    if (all(remove_control_sample %in% names(olink_sample_types))) {
      ctrl_sample_type <- olink_sample_types[names(olink_sample_types)
                                             %in% remove_control_sample] |>
        unlist() |>
        unname()
    } else if (any(remove_control_sample %in% names(olink_sample_types))) {
      ctrl_sample_type <- olink_sample_types[names(olink_sample_types)
                                             %in% remove_control_sample] |>
        unlist() |>
        unname()
      olink_sampless_compl <- setdiff(x = remove_control_sample, # nolint object_usage_linter
                                      y = names(olink_sample_types))

      cli::cli_inform(
        c("Unexpected entries {.val {olink_sampless_compl}} in
          {.arg remove_control_sample}. Expected values:
          {.val {names(olink_sample_types)}}.",
          "i" = "Proceeding with entries: {.val {ctrl_sample_type}}.")
      )
    } else {
      cli::cli_abort(
        c(
          "x" = "{cli::qty(remove_control_sample)} No overlap of value{?s} from
          {.arg remove_control_sample} to expected values.",
          "i" = "Ensure {.arg remove_control_sample} is a scalar boolean or
          contains one or more of {.val {names(olink_sample_types)}}!"
        ),
        call = rlang::caller_env(),
        wrap = TRUE
      )
    }

  }

  # Return original data if user chooses to keep control samples
  if (length(ctrl_sample_type) == 0L) {
    if (verbose == TRUE) {
      cli::cli_inform(
        c("Skipping exclusion of control samples as per user input:
          {.field remove_control_sample} = {.val {remove_control_sample}}.",
          "i" = "Returning original dataset.")
      )
    }
    return(df)
  }

  # Check if 'sample_type' column name is available
  if (!("sample_type" %in% names(check_npx_log$col_names))) {
    cli::cli_inform(
      c("No column marking control samples in dataset.",
        "i" = "Ensure exclusion of control samples for downstream analysis!",
        "i" = "Returning original dataset."
      )
    )
    return(df)
  }

  # detect how many samples are to be removed
  df_sid_stype <- df |>
    dplyr::distinct(
      .data[[check_npx_log$col_names$sample_id]],
      .data[[check_npx_log$col_names$sample_type]]
    ) |>
    dplyr::collect()
  uniq_sample_type <- df_sid_stype |>
    dplyr::pull(
      .data[[check_npx_log$col_names$sample_type]]
    ) |>
    unique()

  # message that we are excluding control samples
  if (any(ctrl_sample_type %in% uniq_sample_type)) {
    uniq_sid <- df_sid_stype |> # nolint object_usage_linter
      dplyr::filter(
        .data[[check_npx_log$col_names$sample_type]] %in% ctrl_sample_type
      ) |>
      dplyr::pull(
        .data[[check_npx_log$col_names$sample_id]]
      )

    cli::cli_inform(
      c(
        "Excluding {.val {length(uniq_sid)}} control sample{?s}:
        {.val {uniq_sid}}.",
        "v" = "Returning cleaned dataset."
      )
    )
  }

  # Filter out control samples
  df_cleaned <- df |>
    dplyr::filter(
      !(.data[[check_npx_log$col_names$sample_type]]
        %in% .env[["ctrl_sample_type"]])
    )

  # Format and return output
  return(df_cleaned)
}

#' Help function removing control assays based on assay type.
#'
#' @description
#' This function filters out internal control assays (`ext_ctrl`, `inc_ctrl`,
#' `amp_ctrl`) from the dataset, unless user specified to retain them. The
#' function uses column mapping provided by `check_npx_log`.
#'
#' @author
#'   Kang Dong
#'   Klev Diamanti
#'
#' @param df A `tibble` or `arrow` object loaded from `read_npx()`.
#' @param check_npx_log A named list generated by the function `check_npx()`,
#' containing:
#' \itemize{
#' \item `col_names$olink_id`: the column name of the assay identifier.
#' \item `col_names$assay_type`: the column name of the assay type. \strong{If
#' column is missing then function will return the original dataset.}
#' }
#' @param remove_control_assay If `FALSE`, all internal control assays are
#' retained. If `TRUE`, all internal control assays are removed. Alternatively,
#' a character vector with one or more of
#' `r ansi_collapse_quot(x = names(olink_assay_types))` indicating the assay
#' types to remove.
#' @param verbose Logical. If `FALSE` (default), silences step-wise CLI
#' messages.
#'
#' @return A `tibble` or `arrow` object with rows corresponding to control
#' assays removed.
#'
clean_assay_type <- function(df,
                             check_npx_log,
                             remove_control_assay = TRUE,
                             verbose = FALSE) {
  # if remove_control_assay is boolean then we either remove all control assays
  # (when TRUE), or we keep all assays (when FALSE).
  # When remove_control_assay is a character vector, then we remove all user
  # designated controls.
  if (check_is_scalar_boolean(bool = remove_control_assay, error = FALSE)) {

    if (remove_control_assay == TRUE) {
      ctrl_assay_type <- olink_assay_types[
        !(names(olink_assay_types) %in% c("assay"))
      ] |>
        unlist() |>
        unname()
    } else {
      ctrl_assay_type <- character(0L)
    }

  } else if (check_is_character(string = remove_control_assay, error = TRUE)) {

    if (all(remove_control_assay %in% names(olink_assay_types))) {
      ctrl_assay_type <- olink_assay_types[names(olink_assay_types)
                                           %in% remove_control_assay] |>
        unlist() |>
        unname()
    } else if (any(remove_control_assay %in% names(olink_assay_types))) {
      ctrl_assay_type <- olink_assay_types[names(olink_assay_types)
                                           %in% remove_control_assay] |>
        unlist() |>
        unname()
      olink_assays_compl <- setdiff(x = remove_control_assay, # nolint object_usage_linter
                                    y = names(olink_assay_types))

      cli::cli_inform(
        c("Unexpected entries {.val {olink_assays_compl}} in
          {.arg remove_control_assay}. Expected values:
          {.val {names(olink_assay_types)}}.",
          "i" = "Proceeding with entries: {.val {ctrl_assay_type}}.")
      )
    } else {
      cli::cli_abort(
        c(
          "x" = "{cli::qty(remove_control_assay)} No overlap of value{?s} from
          {.arg remove_control_assay} to expected values.",
          "i" = "Ensure {.arg remove_control_assay} is a scalar boolean or
          contains one or more of {.val {names(olink_assay_types)}}!"
        ),
        call = rlang::caller_env(),
        wrap = TRUE
      )
    }

  }

  # Return original data if user chooses to keep control samples
  if (length(ctrl_assay_type) == 0L) {
    if (verbose == TRUE) {
      cli::cli_inform(
        c("Skipping exclusion of control assays as per user input:
          {.field remove_control_assay} = {.val {remove_control_assay}}.",
          "i" = "Returning original dataset.")
      )
    }
    return(df)
  }

  # Check if 'assay_type' column name is available
  if (!("assay_type" %in% names(check_npx_log$col_names))) {
    cli::cli_inform(
      c("No column marking control assays in dataset.",
        "i" = "Ensure exclusion of control control for downstream analysis!",
        "i" = "Returning original dataset."
      )
    )
    return(df)
  }

  # detect how many samples are to be removed
  df_oid_atype <- df |>
    dplyr::distinct(
      .data[[check_npx_log$col_names$olink_id]],
      .data[[check_npx_log$col_names$assay_type]]
    ) |>
    dplyr::collect()
  uniq_atype <- df_oid_atype |>
    dplyr::pull(
      .data[[check_npx_log$col_names$assay_type]]
    ) |>
    unique()

  # message that we are excluding control samples
  if (any(ctrl_assay_type %in% uniq_atype)) {
    uniq_oid <- df_oid_atype |> # nolint object_usage_linter
      dplyr::filter(
        .data[[check_npx_log$col_names$assay_type]] %in% ctrl_assay_type
      ) |>
      dplyr::pull(
        .data[[check_npx_log$col_names$olink_id]]
      )

    cli::cli_inform(
      c(
        "Excluding {.val {length(uniq_oid)}} control assay{?s}:
        {.val {uniq_oid}}.",
        "v" = "Returning cleaned dataset."
      )
    )
  }

  # Filter out control assays
  df_cleaned <- df |>
    dplyr::filter(
      !(.data[[check_npx_log$col_names$assay_type]]
        %in% .env[["ctrl_assay_type"]])
    )

  # Format and return output
  return(df_cleaned)
}

#' Help function removing instances of samples that failed QC.
#'
#' @description
#' This function uses the column marking QC warnings identified by
#' `check_npx_log` to remove samples flagged `FAIL` in the dataset.
#'
#' @author
#'   Kang Dong
#'   Klev Diamanti
#'
#' @param df A `tibble` or `arrow` object loaded from `read_npx()`, including a
#' column identified by
#' `r ansi_collapse_quot(x = column_name_dict$col_names$qc_warning, sep ="or")`.
#' @param check_npx_log A named list generated by the function `check_npx()`,
#' containing:
#' \itemize{
#' \item `col_names$sample_id`: the column name of the sample identifier in the
#' dataset.
#' \item `col_names$qc_warning`: the name of the column indicating QC status.
#' }
#' @param remove_qc_warning Logical. If `TRUE`, removes samples flagged as
#' `FAIL` in QC warning. Defaults to `FALSE`.
#' @param verbose Logical. If `FALSE` (default), silences step-wise CLI
#' messages.
#'
#' @returns A `tibble` or `arrow` object with rows corresponding samples failed
#' QC removed.
#'
clean_qc_warning <- function(df,
                             check_npx_log,
                             remove_qc_warning = FALSE,
                             verbose = FALSE) {

  if (remove_qc_warning == FALSE) {
    if (verbose == TRUE) {
      cli::cli_inform(
        c("Skipping exclusion of samples flagged {.val {'FAIL'}} as per user
          input {.field remove_qc_warning} = {.val {FALSE}}.",
          "i" = "Returning original dataset.")
      )
    }
    return(df)
  }

  if (nrow(dplyr::filter(.data = df,
                         grepl(pattern = "fail",
                               x = .data[[check_npx_log$col_names$qc_warning]],
                               ignore.case = TRUE))) > 0L) {

    df_fail_sample <- df |>
      dplyr::filter(
        grepl(
          pattern = "fail",
          x = .data[[check_npx_log$col_names$qc_warning]],
          ignore.case = TRUE
        )
      ) |>
      dplyr::collect()

    fail_sample_n <- df_fail_sample |> # nolint object_usage_linter
      dplyr::pull(
        .data[[check_npx_log$col_names$sample_id]]
      ) |>
      unique()

    # Inform user about failed SampleIDs
    cli::cli_inform(
      c(
        "Excluding {.val {nrow(df_fail_sample)}} datapoint{?s} from
        {.val {length(fail_sample_n)}} sample{?s} flagged with
        {.field {check_npx_log$col_names$qc_warning}} = {.val {'FAIL'}}:
        {.val {fail_sample_n}}.",
        "v" = "Returning cleaned dataset."
      )
    )

    df_cleaned <- df |>
      dplyr::filter(
        !grepl(
          pattern = "fail",
          x = .data[[check_npx_log$col_names$qc_warning]],
          ignore.case = TRUE
        )
      )

    return(df_cleaned)

  } else {

    if (verbose == TRUE) {
      cli::cli_inform(
        c("No samples flagged with {.field {check_npx_log$col_names$qc_warning}}
          = {.val {'FAIL'}}.",
          "i" = "Returning original dataset.")
      )
    }

    return(df)

  }
}

#' Help function removing instances of assays flagged with warnings.
#'
#' @description
#' The function is used to remove assay-level QC warnings from the dataset
#' before analysis. It uses the column marking assay QC warnings identified by
#' `check_npx_log` to remove assays flagged as `WARN` in the dataset.
#'
#' @author
#'   Kang Dong
#'   Klev Diamanti
#'
#' @param df A `tibble` or `arrow` object loaded from `read_npx()`, including a
#' column identified by
#' `r ansi_collapse_quot(x = column_name_dict$col_names$qc_warning, sep ="or")`.
#' @param check_npx_log A named list generated by the function `check_npx()`,
#' containing:
#' \itemize{
#' \item `col_names$olink_id`: the column name of the assay identifier in the
#' dataset.
#' \item `col_names$assay_warning`: the name of the column indicating assay QC
#' status.
#' }
#' @param remove_assay_warning Logical. If `FALSE`, retains assays flagged as
#' `WARN` in assay warning. Defaults to `TRUE`.
#' @param verbose Logical. If `FALSE` (default), silences step-wise CLI
#' messages.
#'
#' @return A `tibble` or `arrow` object with rows corresponding assay with
#' warning flags removed.
#'
clean_assay_warning <- function(df,
                                check_npx_log,
                                remove_assay_warning = TRUE,
                                verbose = FALSE) {
  # retain assays marked with assay warning
  if (remove_assay_warning == FALSE) {
    if (verbose == TRUE) {
      cli::cli_inform(
        c("Skipping exclusion of assays flagged with {.val {'WARN'}} as per user
          input {.field remove_assay_warning} = {.val {FALSE}}.",
          "i" = "Returning original dataset.")
      )
    }
    return(df)
  }

  # Check if assay_warn column name is defined
  if (!("assay_warn" %in% names(check_npx_log$col_names))) {
    cli::cli_inform(
      c("No column marking assay warnings in dataset.",
        "i" = "Ensure assays with QC warnings are removed prior to downstream
          analysis!",
        "i" = "Returning original dataset."
      )
    )
    return(df)
  }

  if (nrow(dplyr::filter(.data = df,
                         grepl(pattern = "warn",
                               x = .data[[check_npx_log$col_names$assay_warn]],
                               ignore.case = TRUE))) > 0L) {

    df_warn_assay <- df |>
      dplyr::filter(
        grepl(
          pattern = "warn",
          x = .data[[check_npx_log$col_names$assay_warn]],
          ignore.case = TRUE
        )
      ) |>
      dplyr::collect()

    warn_assay_n <- df_warn_assay |> # nolint object_usage_linter
      dplyr::pull(
        .data[[check_npx_log$col_names$olink_id]]
      ) |>
      unique()

    # Filter out failed assays and return cleaned data
    cli::cli_inform(
      c(
        "Excluding {.val {nrow(df_warn_assay)}} datapoint{?s} from
        {.val {length(warn_assay_n)}} assay{?s} flagged with
        {.field {check_npx_log$col_names$assay_warn}} = {.val {'WARN'}} or
        {.val {'Warning'}}: {.val {warn_assay_n}}.",
        "v" = "Returning cleaned dataset."
      )
    )

    df_cleaned <- df |>
      dplyr::filter(
        !grepl(
          pattern = "warn",
          x = .data[[check_npx_log$col_names$assay_warn]],
          ignore.case = TRUE
        )
      )

    return(df_cleaned)

  } else {

    if (verbose == TRUE) {
      cli::cli_inform(
        c("No assays flagged with {.field {check_npx_log$col_names$assay_warn}}
          = {.val {'WARN'}} or {.val {'Warning'}}.",
          "i" = "Returning original dataset.")
      )
    }

    return(df)

  }
}

#' Help function removing a set of control samples from the dataset.
#'
#' @description
#' This function removes rows from NPX data where the sample identifiers, as
#' defined in `check_npx_log`, match samples provided in
#' \var{control_sample_ids}. Primary goal of the function is to serve for
#' filtering out technical replicates or control samples prior to downstream
#' analysis.
#'
#' @author
#'   Kang Dong
#'   Klev Diamanti
#'
#' @param df A `tibble` or `arrow` object loaded from `read_npx()`, including a
#' column identified by
#' `r ansi_collapse_quot(x = column_name_dict$col_names$qc_warning, sep ="or")`.
#' @param check_npx_log A named list generated by the function `check_npx()`,
#' containing:
#' \itemize{
#' \item `col_names$sample_id`: the column name of the sample identifier in the
#' dataset.
#' }
#' @param control_sample_ids character vector of sample identifiers of control
#' samples. Default `NULL`, to mark no samples to be removed.
#' @param verbose Logical. If `FALSE` (default), silences step-wise CLI
#' messages.
#'
#' @returns A `tibble` or `arrow` object with rows corresponding to the provided
#' control samples removed.
#'
#' @examples
#' \dontrun{
#' # use npx_data1 to check that clean_control_sample_id() works
#' log <- OlinkAnalyze::check_npx(
#'   df = OlinkAnalyze::npx_data1
#' ) |>
#'   suppressWarnings() |>
#'   suppressMessages()
#'
#' out <- OlinkAnalyze:::clean_control_sample_id(
#'   df = npx_data1,
#'   check_npx_log = log,
#'   control_sample_id = c("CONTROL_SAMPLE_AS 1", "CONTROL_SAMPLE_AS 2")
#' )
#' }
#'
clean_control_sample_id <- function(df,
                                    check_npx_log,
                                    control_sample_ids = NULL,
                                    verbose = FALSE) {

  # Check if sample_id column exist in the data table
  if (is.null(control_sample_ids)) {
    if (verbose == TRUE) {
      cli::cli_inform(
        c("Skipping exclusion of control samples based on
        {.arg control_sample_ids}.",
          "i" = "Returning original dataset.")
      )
    }
    return(df)
  }

  if (nrow(dplyr::filter(.data = df,
                         .data[[check_npx_log$col_names$sample_id]]
                         %in% .env[["control_sample_ids"]])) > 0L) {

    sid <- df |>
      dplyr::distinct(
        .data[[check_npx_log$col_names$sample_id]]
      ) |>
      dplyr::collect() |>
      dplyr::pull(
        .data[[check_npx_log$col_names$sample_id]]
      )

    df_cleaned <- df |>
      dplyr::filter(
        !(.data[[check_npx_log$col_names$sample_id]]
          %in% .env[["control_sample_ids"]])
      )

    # Filter out control samples and return cleaned data
    if (all(control_sample_ids %in% sid)) {
      cli::cli_inform(
        "Excluding sample{?s}: {.val {control_sample_ids}}.",
        "v" = "Returning cleaned dataset."
      )
    } else {
      ctrl_sid_shared <- intersect(x = control_sample_ids, y = sid) # nolint object_usage_linter
      ctrl_sid_setdiff <- setdiff(x = control_sample_ids, y = sid) # nolint object_usage_linter
      cli::cli_inform(
        "{cli::qty(ctrl_sid_shared)} Excluding sample{?s}:
        {.val {ctrl_sid_shared}}. {cli::qty(ctrl_sid_setdiff)}Sample{?s} not in
        dataset: {.val {ctrl_sid_setdiff}}.",
        "v" = "Returning cleaned dataset."
      )
    }

    return(df_cleaned)

  } else {

    cli::cli_inform(
      c("None of the sample identifiers in {.arg control_sample_ids} was present
        in the dataset {.arg df}.",
        "i" = "Returning original dataset.")
    )

    return(df)
  }
}

#' Help function converting types of columns to the expected ones.
#'
#' @description
#' This function checks for mismatches between actual and expected column
#' classes in the input data frame and coerces those columns to the expected
#' class using information from `check_npx_log$col_class`.
#'
#' @author
#'   Kang Dong
#'   Klev Diamanti
#'
#' @param df A `tibble` or `arrow` object loaded from `read_npx()`, including a
#' column identified by
#' `r ansi_collapse_quot(x = column_name_dict$col_names$qc_warning, sep ="or")`.
#' @param check_npx_log A named list generated by the function `check_npx()`,
#' containing:
#' \itemize{
#' \item `col_names$col_class`: a data frame containing column names that need
#' to be converted to a different type.
#' }
#' @param verbose Logical. If `FALSE` (default), silences step-wise CLI
#' messages.
#'
#' @return A `tibble` or `arrow` object with types of selected columns converted
#' to the expected type.
#'
clean_col_class <- function(df,
                            check_npx_log,
                            verbose = FALSE) {

  # Early return if no corrections needed
  if (nrow(check_npx_log$col_class) == 0L) {
    if (verbose == TRUE) {
      cli::cli_inform(
        c("Columns are in the correct format.",
          "i" = "Returning original dataset.")
      )
    }

    return(df)
  }

  # convert columns
  df_cleaned <- df |>
    dplyr::mutate(
      dplyr::across(
        check_npx_log$col_class |>
          dplyr::filter(
            .data[["expected_col_class"]] == "numeric"
          ) |>
          dplyr::pull(
            .data[["col_name"]]
          ),
        ~ suppressWarnings(as.numeric(.x))
      )
    ) |>
    dplyr::mutate(
      dplyr::across(
        check_npx_log$col_class |>
          dplyr::filter(
            .data[["expected_col_class"]] == "character"
          ) |>
          dplyr::pull(
            .data[["col_name"]]
          ),
        ~ suppressWarnings(as.character(.x))
      )
    )

  col_class_msg <- paste0(
    "* \"", check_npx_log$col_class$col_name, "\": ",
    "from \"", check_npx_log$col_class$col_class, "\" converted to ",
    "\"", check_npx_log$col_class$expected_col_class, "\"."
  )

  cli::cli_inform(
    c("{cli::qty(col_class_msg)}Converted class{?es} of column{?s}:",
      col_class_msg,
      "v" = "Returning cleaned dataset.")
  )

  return(df_cleaned)
}
