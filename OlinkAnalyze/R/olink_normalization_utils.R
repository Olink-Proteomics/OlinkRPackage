#' Check inputs of \code{\link{olink_normalization}} function.
#'
#' @description
#' This function is a wrapper of multiple help functions which check the inputs
#' of the \code{\link{olink_normalization}} function.
#'
#' @details
#' The following checks are performed:
#'  - \code{\link{olink_norm_input_validate}}:
#'    - Determines the normalization to be performed by intersecting inputs with
#'    internal global variable `olink_norm_mode_combos`.
#'    - Returns the type of normalization to be performed from
#'    `olink_norm_modes`.
#'    - Message with the normalization type.
#'    - Error message if input is invalid.
#'  - \code{\link{olink_norm_input_class}}:
#'    - Checks if all inputs are of the expected class:
#'      - `df1`, `df2` and `reference_medians`: tibble or R6 ArrowObject
#'      - `overlapping_samples_df1`, `overlapping_samples_df2`,
#'      `df1_project_nr`, `df2_project_nr` and `reference_project`: Character
#'      vector
#'    - Also checks the validity of names of project and reference project.
#'    - Error if invalid input classes are detected.
#'  - \code{\link{olink_norm_input_check_df_cols}}:
#'    - Detects the column names of input datasets `df1` and `df2` to allow for
#'    alternative names.
#'    - Returns named list of column names to use downstream.
#'    - Warning if `Normalization` column missing from all datasets.
#'    - Warning if `LOD` is missing or if there are multiple `LOD` columns.
#'    - Error if required columns are missing.
#'    - Error if not all input datasets have or lack `Normalization` column.
#'    - Error if input datasets have been quantified with different methods.
#'  - \code{\link{olink_norm_input_ref_medians}}:
#'    - Checks validity of dataset containing `reference_medians`.
#'    - Error if required columns are missing based on
#'    `olink_norm_ref_median_cols`.
#'    - Error if columns are not of the correct class bases on
#'    `olink_norm_ref_median_cols`.
#'    - Error if there duplicate assay identifiers.
#'  - \code{\link{olink_norm_input_check_samples}}:
#'    - Check character vectors of reference sample identifiers for:
#'      - Being present in `df1` and/or `df2`.
#'      - Duplicate identifiers.
#'  - \code{\link{olink_norm_input_clean_assays}}:
#'    - Returns a named list with the updated `df1`, `df2` and/or
#'    `reference_medians`.
#'    - Removes assays that are not of the format OID followed by 5 digits.
#'    - Removes assays that are marked with `Normalization = EXCLUDED`.
#'  - \code{\link{olink_norm_input_assay_overlap}}:
#'    - Returns a named list with the updated `df1`, `df2` and/or
#'    `reference_medians`.
#'    - Remove assays not shared between `df1` and `df2`, or between `df1` and
#'    `reference_medians`.
#'  - \code{\link{olink_norm_input_norm_method}}:
#'    - Check if all assays in `df1` and `df2` have been originally normalized
#'    with the same method "Intensity" or "Plate control".
#'    - Warning is thrown if not.
#'
#' @author
#'   Klev Diamanti
#'
#' @param df1 First dataset to be used in normalization (required).
#' @param df2 Second dataset to be used in normalization.
#' @param overlapping_samples_df1 Samples to be used for adjustment factor
#' calculation in df1 (required).
#' @param overlapping_samples_df2 Samples to be used for adjustment factor
#' calculation in df2.
#' @param df1_project_nr Project name of first dataset (df1).
#' @param df2_project_nr Project name of first dataset (df2).
#' @param reference_project Project name of reference_project. Should be one of
#' \var{df1_project_nr} or \var{df2_project_nr}. Indicates the project to which
#' the other project is adjusted to.
#' @param reference_medians Dataset with columns "OlinkID" and "Reference_NPX".
#' Used for reference median normalization.
#'
#' @return Named list of updated inputs to use for normalization:
#'  - `df1`: dataset df1.
#'  - `df2`: `NULL` if reference median normalization, or dataset df2.
#'  - `overlapping_samples_df1`: character vector of reference samples from df1.
#'  - `overlapping_samples_df2`: `NULL` if reference median normalization, or
#'  character vector of reference samples from df1.
#'  - `df1_project_nr`: name of df1 project.
#'  - `df2_project_nr`: `NULL` if reference median normalization, or name of df2
#'  project.
#'  - `reference_project`: `NULL` if reference median normalization, or name of
#'  reference project.
#'  - `reference_medians`: `NULL` if bridge or subset normalization, or dataset
#'  with reference_medians.
#'  - `df1_cols`: column names of df1 to use downstream.
#'  - `df2_cols`: `NULL` if reference median normalization, or column names of
#'  df2 to use downstream.
#'  - `norm_mode`: one of `r cli::ansi_collapse(x = unlist(olink_norm_modes))`
#'  indicating the normalization to be performed.
#'
olink_norm_input_check <- function(df1,
                                   df2,
                                   overlapping_samples_df1,
                                   overlapping_samples_df2,
                                   df1_project_nr,
                                   df2_project_nr,
                                   reference_project,
                                   reference_medians) {
  # Validate the normalization input ----

  norm_valid <- olink_norm_input_validate(
    df1 = df1,
    df2 = df2,
    overlapping_samples_df1 = overlapping_samples_df1,
    overlapping_samples_df2 = overlapping_samples_df2,
    reference_medians = reference_medians
  )
  norm_mode <- norm_valid$norm_mode
  norm_msg <- norm_valid$norm_msg

  # Check that input classes are correct ----

  olink_norm_input_class(
    df1 = df1,
    df2 = df2,
    overlapping_samples_df1 = overlapping_samples_df1,
    overlapping_samples_df2 = overlapping_samples_df2,
    df1_project_nr = df1_project_nr,
    df2_project_nr = df2_project_nr,
    reference_project = reference_project,
    reference_medians = reference_medians,
    norm_mode = norm_mode
  )

# Check column names ----

  if (norm_mode == olink_norm_modes$ref_median) {
    # reference median normalization

    # check columns of df1
    lst_df <- list(df1)
    names(lst_df) <- df1_project_nr
    lst_cols <- olink_norm_input_check_df_cols(lst_df = lst_df)



    # list of samples
    lst_ref_samples <- list(overlapping_samples_df1)
    names(lst_ref_samples) <- df1_project_nr

    # check reference_medians
    olink_norm_input_ref_medians(reference_medians = reference_medians)

  } else {

    # bridge, subset, or cross_product normalization

    reference_medians <- NULL

    lst_df <- list(df1, df2)
    names(lst_df) <- c(df1_project_nr, df2_project_nr)
    lst_cols <- olink_norm_input_check_df_cols(lst_df = lst_df)
    product_ids <- olink_product_identifier_norm(
      lst_df = lst_df,
      reference_project = reference_project,
      lst_cols = lst_cols
    )


    if (norm_mode %in% c(olink_norm_modes$bridge,
                         olink_norm_modes$norm_cross_product)) {
      # check if it is cross_product normalization, or simple bridge normalization
      norm_cross_product <- olink_norm_input_cross_product(
        lst_df = lst_df,
        lst_cols = lst_cols,
        reference_project = reference_project,
        lst_product = product_ids
      )
      norm_mode <- norm_cross_product$norm_mode
      lst_df <- norm_cross_product$lst_df
      # bridge or 3k-HT normalization normalization
      lst_ref_samples <- list(overlapping_samples_df1, overlapping_samples_df1)
    } else if (norm_mode == olink_norm_modes$subset) {
      # subset normalization
      lst_ref_samples <- list(overlapping_samples_df1, overlapping_samples_df2)
    }
    names(lst_ref_samples) <- c(df1_project_nr, df2_project_nr)

  }

  # Update normalization message ----

  # Update the message to inform what type of normalization we will perform
  if (norm_mode == olink_norm_modes$norm_cross_product) {
    norm_msg <- gsub(
      pattern = "Bridge",
      replacement = "Cross-product",
      x = norm_msg
    )
  }

  # Check samples ----

  # extract all unique sample identifiers
  lst_df_samples <- lapply(names(lst_cols), function(l_col) {
    lst_df[[l_col]] |>
      dplyr::select(
        dplyr::all_of(
          lst_cols[[l_col]]$sample_id
        )
      ) |>
      dplyr::distinct() |>
      dplyr::collect() |>
      dplyr::pull(
        .data[[lst_cols[[l_col]]$sample_id]]
      )
  })
  names(lst_df_samples) <- names(lst_cols)

  olink_norm_input_check_samples(
    lst_df_samples = lst_df_samples,
    lst_ref_samples = lst_ref_samples,
    norm_mode = norm_mode
  )

  # Clean assays ----

  # clear df and reference_medians from excluded assays and assays not shared
  # across all inputs
  lst_df_clean_assays <- olink_norm_input_clean_assays(
    lst_df = lst_df,
    reference_medians = reference_medians,
    lst_cols = lst_cols,
    norm_mode = norm_mode
  )
  lst_df <- lst_df_clean_assays$lst_df
  reference_medians <- lst_df_clean_assays$reference_medians

  # Check assays shared across inputs ----

  # check if all assays from input are in all datasets, and remove them if not
  lst_df_overlap_assay <- olink_norm_input_assay_overlap(
    lst_df = lst_df_clean_assays$lst_df,
    reference_medians = lst_df_clean_assays$reference_medians,
    lst_cols = lst_cols,
    norm_mode = norm_mode
  )
  lst_df <- lst_df_overlap_assay$lst_df
  reference_medians <- lst_df_overlap_assay$reference_medians

  # Check normalization approach ----

  all_norm_present <- lst_cols |>
    sapply(function(x) !identical(x = x$normalization, y = character(0L))) |>
    all()

  if (all_norm_present && length(lst_df) == 2L) {
    olink_norm_input_norm_method(
      lst_df = lst_df,
      lst_cols = lst_cols
    )
  }

  # return to normalize ----

  # message to inform user
  cli::cli_inform(message = norm_msg)

  lst_out <- list(
    ref_df = NULL,
    ref_samples = NULL,
    ref_name = NULL,
    ref_cols = NULL,
    not_ref_df = NULL,
    not_ref_samples = NULL,
    not_ref_name = NULL,
    not_ref_cols = NULL,
    reference_medians = NULL,
    norm_mode = NULL
  )

  # set normalization mode
  lst_out$norm_mode <- norm_mode

  if (norm_mode %in% olink_norm_modes$ref_median) {
    # reference median normalization
    lst_out$ref_name <- df1_project_nr
    lst_out$ref_samples <- overlapping_samples_df1
    lst_out$ref_df <- lst_df[[lst_out$ref_name]]
    lst_out$ref_cols <- lst_cols[[lst_out$ref_name]]
    lst_out$reference_medians <- reference_medians
  } else if (norm_mode %in% c(olink_norm_modes$subset,
                              olink_norm_modes$bridge,
                              olink_norm_modes$norm_cross_product)) {
    # bridge or subset normalization
    if (reference_project == df1_project_nr) {
      lst_out$ref_name <- df1_project_nr
      lst_out$not_ref_name <- df2_project_nr
    } else {
      lst_out$ref_name <- df2_project_nr
      lst_out$not_ref_name <- df1_project_nr
    }
    lst_out$ref_df <- lst_df[[lst_out$ref_name]]
    lst_out$ref_cols <- lst_cols[[lst_out$ref_name]]
    lst_out$not_ref_df <- lst_df[[lst_out$not_ref_name]]
    lst_out$not_ref_cols <- lst_cols[[lst_out$not_ref_name]]
    if (norm_mode == olink_norm_modes$subset) {
      if (reference_project == df1_project_nr) {
        lst_out$ref_samples <- overlapping_samples_df1
        lst_out$not_ref_samples <- overlapping_samples_df2
      } else {
        lst_out$ref_samples <- overlapping_samples_df2
        lst_out$not_ref_samples <- overlapping_samples_df1
      }
    } else if (norm_mode %in% c(olink_norm_modes$bridge,
                                olink_norm_modes$norm_cross_product)) {
      lst_out$ref_samples <- overlapping_samples_df1
      lst_out$lst_product <- product_ids
    }
  }

  return(lst_out)

}

#' Validate inputs of normalization function
#'
#' @description
#' This function takes as input some of the inputs of the Olink normalization
#' function and checks the validity of the input.
#'
#' @details
#' Depending on the input the function will return:
#' \itemize{
#' \item \strong{Error}: if the required components are lacking from the input
#' or if the normalization cannot be performed.
#' \item \strong{Warning}: if the normalization can be determined but extra
#' inputs are provided. This will be followed by a message and the type of
#' normalization to be performed.
#' \item \strong{Message}: Information about the type of
#' normalization to be performed.
#' }
#'
#' \strong{Note} that input are passed directly from the main
#' \code{\link{olink_normalization}} function.
#'
#' @author
#'   Klev Diamanti
#'
#' @param df1 First dataset to be used in normalization (required).
#' @param df2 Second dataset to be used in normalization.
#' @param overlapping_samples_df1 Samples to be used for adjustment factor
#' calculation in df1 (required).
#' @param overlapping_samples_df2 Samples to be used for adjustment factor
#' calculation in df2.
#' @param reference_medians Dataset with columns "OlinkID" and "Reference_NPX".
#' Used for reference median normalization.
#'
#' @return Scalar character from \var{olink_norm_modes} if normalization can be
#' determined from the input, otherwise see details.
#'
olink_norm_input_validate <- function(df1,
                                      df2,
                                      overlapping_samples_df1,
                                      overlapping_samples_df2,
                                      reference_medians) {
  # check inputs ----

  ## check df1 ----

  # in any case df1 should be a tibble, data.frame or ArrowObject
  v_df1 <- ifelse(!missing(df1), # nolint
                  TRUE,
                  FALSE)

  ## check df2 ----

  v_df2 <- ifelse(!is.null(df2), # nolint
                  TRUE,
                  FALSE)

  ## check overlapping_samples_df1 ----

  v_overlap_samples_df1 <- ifelse(!missing(overlapping_samples_df1), # nolint
                                  TRUE,
                                  FALSE)

  ## check overlapping_samples_df2 ----

  v_overlap_samples_df2 <- ifelse(!is.null(overlapping_samples_df2), # nolint
                                  TRUE,
                                  FALSE)

  ## check reference_medians ----

  v_reference_medians <- ifelse(!is.null(reference_medians), # nolint
                                TRUE,
                                FALSE)

  # get normalization mode ----

  # use the bits from the v_* variables above to check for errors or warnings in
  # the user input. this will be determined from the data frame
  # olink_norm_mode_combos which contains all combinations of the 5 variables.
  olink_norm_mode_row <- olink_norm_mode_combos |>
    dplyr::filter(
      .data[["df1"]] == .env[["v_df1"]]
      & .data[["df2"]] == .env[["v_df2"]]
      & .data[["overlapping_samples_df1"]] == .env[["v_overlap_samples_df1"]]
      & .data[["overlapping_samples_df2"]] == .env[["v_overlap_samples_df2"]]
      & .data[["reference_medians"]] == .env[["v_reference_medians"]]
    )

  error_msg_row <- olink_norm_mode_row$error_msg[1L]
  warning_msg_row <- olink_norm_mode_row$warning_msg[1L]
  inform_msg_row <- olink_norm_mode_row$inform_msg[1L]
  norm_mode_row <- olink_norm_mode_row$norm_mode[1L]

  # errors, warnings or messages ----

  # if there is an error, throw it and exit
  if (!is.na(error_msg_row)) {

    cli::cli_abort(
      message = c(
        "x" = error_msg_row,
        "i" = "Check function help for examples."
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  } else {

    # in case there is a warning from the use input
    if (!is.na(warning_msg_row)) {
      cli::cli_warn(message = warning_msg_row)
    }

    # return the type of normalization to perform
    return(
      list(
        norm_mode = norm_mode_row,
        norm_msg = inform_msg_row
      )
    )

  }

}

#' Check classes of input in olink_normalization function
#'
#' @description
#' Check if \var{df1}, \var{df2} and/or \var{reference_medians} are tibble or
#' ArrowDataset datasets; if \var{overlapping_samples_df1} and/or
#' \var{overlapping_samples_df2} are character vectors; and if
#' \var{df1_project_nr}, \var{df2_project_nr} and/or \var{reference_project} are
#' scalar character vectors.
#'
#' @author
#'   Klev Diamanti
#'
#' @param df1 First dataset to be used in normalization (required).
#' @param df2 Second dataset to be used in normalization.
#' @param overlapping_samples_df1 Samples to be used for adjustment factor
#' calculation in df1 (required).
#' @param overlapping_samples_df2 Samples to be used for adjustment factor
#' calculation in df2.
#' @param df1_project_nr Project name of first dataset (df1).
#' @param df2_project_nr Project name of first dataset (df2).
#' @param reference_project Project name of reference_project. Should be one of
#' \var{df1_project_nr} or \var{df2_project_nr}. Indicates the project to which
#' the other project is adjusted to.
#' @param reference_medians Dataset with columns "OlinkID" and "Reference_NPX".
#' Used for reference median normalization.
#' @param norm_mode Scalar character from \var{olink_norm_modes} with the
#' normalization to be performed. Output from
#' \code{\link{olink_norm_input_validate}}.
#'
#' @return `NULL` unless there is an error
#'
olink_norm_input_class <- function(df1,
                                   df2,
                                   overlapping_samples_df1,
                                   overlapping_samples_df2,
                                   df1_project_nr,
                                   df2_project_nr,
                                   reference_project,
                                   reference_medians,
                                   norm_mode) {
  # help functions ----

  check_is_tibble_arrow <- function(df) {
    if (!inherits(x = df, what = c("tbl_df", "ArrowObject"))) {
      cli::cli_abort(
        message = c(
          "x" = "{.arg {rlang::caller_arg(df)}} should be a tibble or an R6
        ArrowObject"
        ),
        call = rlang::caller_env(),
        wrap = FALSE
      )
    }
  }

  check_is_character <- function(string,
                                 scalar = FALSE) {
    if (scalar == TRUE) {
      if (!rlang::is_scalar_character(string)) {
        cli::cli_abort(
          message = c(
            "x" = "{.arg {rlang::caller_arg(string)}} should be a character
          vector of length 1."
          ),
          call = rlang::caller_env(),
          wrap = FALSE
        )
      }
    } else {
      if (!rlang::is_character(string)) {
        cli::cli_abort(
          message = c(
            "x" = "{.arg {rlang::caller_arg(string)}} should be a character
          vector."
          ),
          call = rlang::caller_env(),
          wrap = FALSE
        )
      }
    }
  }

  # check inputs ----

  # check those that should always be there
  check_is_tibble_arrow(df = df1)
  check_is_character(string = overlapping_samples_df1,
                     scalar = FALSE)
  check_is_character(string = df1_project_nr,
                     scalar = TRUE)

  ## check per norm_mode ----

  if (norm_mode == olink_norm_modes$ref_median) {
    # if reference median
    check_is_tibble_arrow(df = reference_medians)
  } else {
    # if bridge or subset
    check_is_tibble_arrow(df = df2)
    check_is_character(string = df2_project_nr,
                       scalar = TRUE)
    check_is_character(string = reference_project,
                       scalar = TRUE)

    # if subset
    if (norm_mode == olink_norm_modes$subset) {
      check_is_character(string = overlapping_samples_df2,
                         scalar = FALSE)
    }
  }

  ## check reference_project equals to df1_project_nr OR df2_project_nr ----

  if (norm_mode != olink_norm_modes$ref_median) {
    if (!(reference_project %in% c(df1_project_nr, df2_project_nr))) {
      cli::cli_abort(
        message = c(
          "x" = "{.arg reference_project} should be one of
          {.val {df1_project_nr}} or {.val {df2_project_nr}}!"
        ),
        call = rlang::caller_env(),
        wrap = FALSE
      )
    }

    ## check that df1_project_nr != df2_project_nr ----

    if (df1_project_nr == df2_project_nr) {
      cli::cli_abort(
        message = c(
          "x" = "Values of {.arg df1_project_nr} and {.arg df2_project_nr}
          should be different!"
        ),
        call = rlang::caller_env(),
        wrap = FALSE
      )
    }
  }
}

#' Check columns of a list of datasets to be normalized.
#'
#' @description
#' This function takes as input a named list of datasets and checks if their
#' columns allow the normalization to be performed. The input may contain
#' "tibble", "ArrowTable" or a mixture of them.
#'
#' @author
#'   Klev Diamanti
#'
#' @param lst_df Named list of datasets to be normalized.
#'
#' @return Named list of vectors with the required column names for each dataset
#' in \var{lst_df} if no error.
#'
#' @examples
#' \donttest{
#' # One dataset
#' OlinkAnalyze:::olink_norm_input_check_df_cols(
#'   lst_df = list(
#'     "p1" = npx_data1
#'   ) |>
#'     lapply(function(l_df) {
#'       l_df |>
#'         dplyr::select(
#'           -dplyr::any_of(c("Normalization"))
#'         )
#'      })
#'   )
#'
#' # Two datasets
#' OlinkAnalyze:::olink_norm_input_check_df_cols(
#'   lst_df = list(
#'     "p1" = npx_data1,
#'     "p2" = npx_data2
#'   ) |>
#'     lapply(function(l_df) {
#'       l_df |>
#'         dplyr::select(
#'           -dplyr::any_of(c("Normalization"))
#'         )
#'      })
#'   )
#'
#' # Multiple datasets
#' OlinkAnalyze:::olink_norm_input_check_df_cols(
#'   lst_df = list(
#'     "p1" = npx_data1,
#'     "p2" = npx_data2,
#'     "p3" = npx_data1,
#'     "p4" = npx_data2
#'   ) |>
#'     lapply(function(l_df) {
#'       l_df |>
#'         dplyr::select(
#'           -dplyr::any_of(c("Normalization"))
#'         )
#'      })
#'   )
#' }
#'
olink_norm_input_check_df_cols <- function(lst_df) {
  # check required columns ----

  # this is a list of columns that are expected to be present in one or all
  # datasets, if 2 or more are provided as input. Some columns named have been
  # evolving; to handle this we have added all the possible column names
  # matching the same column as elements of a character vector. All elements of
  # the list should match at least one column name, except from "Normalization"
  # that is allowed to be missing from all.
  required_cols <- list(
    sample_id = "SampleID",
    olink_id = "OlinkID",
    uniprot = "UniProt",
    assay = "Assay",
    panel = "Panel",
    panel_version = c("Panel_Lot_Nr", "Panel_Version", "DataAnalysisRefID"),
    plate_id = "PlateID",
    qc_warn = c("QC_Warning", "SampleQC"),
    assay_warn = c("Assay_Warning", "AssayQC"),
    quant = c("Ct", "NPX", "Quantified_value"),
    lod = c("LOD",
            "Plate LOD", "Plate_LOD", "PlateLOD",
            "Max LOD", "Max_LOD", "MaxLOD"),
    normalization = "Normalization",
    count = "Count"
  )

  # intersect required column names with columns of df
  lst_req_col <- lapply(lst_df, function(l_df) {
    lapply(required_cols, function(r_col) r_col[r_col %in% names(l_df)])
  })

  ## normalization can be missing from both datasets ----

  # we tolerate "Normalization" missing from all datasets, otherwise it is
  # an error
  col_norm <- lapply(lst_req_col, function(x) x$normalization) |>
    unlist()
  if (length(col_norm) != length(lst_req_col)
      && !identical(col_norm, character(0L))) {
    cli::cli_abort(
      c(
        "x" = "{cli::qty(length(lst_req_col) - length(col_norm))} Dataset{?s}
        {.val {setdiff(names(lst_req_col), names(col_norm))}} {?does/do}
        not contain a column named {.val {required_cols$normalization}}!",
        "i" = "The column should be present in all, or missing from all input
        datasets."
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )
  } else if (identical(col_norm, character(0L))) {
    cli::cli_warn(
      c(
        "{cli::qty(names(lst_req_col))} Dataset{?s} {.val {names(lst_req_col)}}
        {cli::qty(names(lst_req_col))} {?does/do} not contain a column named
        {.val {required_cols$normalization}}."
      )
    )
  }

  ## lod can be missing from datasets or have multiple matches (PlateLOD) ----

  col_lod <- lapply(lst_req_col, function(x) x$lod) |>
    lapply(function(x) {
      x[length(x) > 1L] |>
        cli::ansi_collapse()
    }) |>
    unlist()
  col_lod <- col_lod[nchar(col_lod) > 0L]

  if (!identical(unname(col_lod), character(0L))) {
    cli::cli_inform(
      c(
        "{cli::qty(names(col_lod))} Dataset{?s} {.val {names(col_lod)}}
        {cli::qty(names(col_lod))} {?contains/contain} multiple columns matching
        {.var LOD}: {.val {required_cols$lod}}.",
        "i" = "They will be all adjusted"
      )
    )
  }

  ## check for missing columns ----

  # identify missing column names from the set of required_cols and prepare the
  # error to be thrown
  lst_col_miss <- lapply(lst_req_col, function(l_col) {
    lapply(l_col, function(r_col) {
      length(r_col) == 1L
    })
  }) |>
    # remove lod and normalization as it was checked above
    # we allow assay_warn to be missing but we want to match it to reference
    # df in normalization
    lapply(function(sub_lst) {
      sub_lst[!(names(sub_lst) %in% c("lod", "normalization",
                                      "assay_warn", "count"))]
    }) |>
    # remove all elements that have no missing value
    lapply(function(sub_lst) {
      sub_lst[sub_lst == FALSE]
    }) |>
    # keep only elements with no or more than 1 matches to required_cols
    lapply(function(sub_lst) {
      required_cols[names(required_cols) %in% names(sub_lst)] |>
        # collapse columns whose names can differ in different datasets
        lapply(cli::ansi_collapse, sep = ", ", sep2 = " or ", last = " or ") |>
        # unlist for better error printing
        unlist() |>
        # collapse all missing column names for error printing
        cli::ansi_collapse(sep = "; ", sep2 = "; ", last = "; ")
    })
  lst_col_miss <- lst_col_miss[nchar(lst_col_miss) > 0L]

  # error message if there are missing columns
  if (!all(sapply(lst_col_miss, nchar) == 0L)) {
    cli::cli_abort(
      c(
        "x" = "{cli::qty(lst_col_miss)} Dataset{?s} with missing column(s):",
        paste0("* ", names(lst_col_miss), ": ", unlist(lst_col_miss)),
        "i" = "The missing columns are separated by semicolon (;)."
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )
  }

  ## check that quant methods are the same ----

  quant_col <- lapply(lst_req_col, function(r_col) r_col$quant) |>
    unlist()

  # error message if not identical
  if (length(unique(quant_col)) != 1L) {
    cli::cli_abort(
      c(
        "x" = "{cli::qty(quant_col)} Dataset{?s} are not quantified with the
        same method:",
        paste0("* ", names(quant_col), ": ", quant_col),
        "i" = "Re-export data with identical quantifications."
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )
  }

  # non-required column mismatches ----

  # this should work only if there are 2 or more datasets
  if (length(lst_df) > 1L) {

    # get non-required cols for each dataset by creating a data frame of all
    # combos of df names and non-required column names and checking if they are
    # present in all datasets.
    df_non_req_col <- tidyr::expand_grid(
      n_df = names(lst_df),
      n_col = lapply(names(lst_df), function(n_df) {
        df_cnames <- names(lst_df[[n_df]])
        df_cnames[!(df_cnames %in% lst_req_col[[n_df]])]
      }) |>
        unlist() |>
        unique()
    ) |>
      # go row by row and check if columns exists in dataset n_df
      dplyr::rowwise() |>
      dplyr::mutate(
        is_in = .data[["n_col"]] %in% names(lst_df[[.data[["n_df"]]]])
      ) |>
      dplyr::ungroup() |>
      # keep only missing ones
      dplyr::filter(
        .data[["is_in"]] == FALSE
      ) |>
      # print message
      dplyr::group_by(
        dplyr::pick(
          dplyr::all_of(
            c("n_df")
          )
        )
      ) |>
      dplyr::summarise(
        prnt_msg = cli::ansi_collapse(.data[["n_col"]]),
        .groups = "drop"
      )

    # warning message
    if (nrow(df_non_req_col) > 0L) {
      cli::cli_warn(
        c(
          "{cli::qty(df_non_req_col$n_df)} Column{?s} not present across
          datasets:",
          paste0("* ", df_non_req_col$n_df, ": ", df_non_req_col$prnt_msg),
          "i" = "Columns will be added with {.val {NA}} values."
        )
      )
    }

  }

  # check that column classes of datasets match ----

  # we need to check if classes of columns of the datasets to be normalized
  # match each other. This is to ensure that when we do bind_rows, there is no
  # error.

  lst_class <- lapply(names(lst_df), function(l_name) {
    lst_df[[l_name]] |>
      dplyr::select(
        dplyr::all_of(
          unlist(lst_req_col[[l_name]])
        )
      ) |>
      dplyr::collect() |>
      sapply(class)
  })
  names(lst_class) <- names(lst_df)
  # find shared names across all datasets
  lst_class_shared <- Reduce(f = intersect, x = lapply(lst_class, names))
  # check classes across shared columns
  lst_class_non_match <- lst_class |>
    lapply(function(x) x[names(x) %in% lst_class_shared]) |>
    as.data.frame() |>
    tibble::rownames_to_column(
      var = "df_name"
    ) |>
    dplyr::as_tibble() |>
    tidyr::pivot_longer(
      cols = -dplyr::all_of(c("df_name")),
      names_to = "df",
      values_to = "class"
    ) |>
    dplyr::group_by(
      dplyr::pick(
        dplyr::all_of("df_name")
      )
    ) |>
    dplyr::summarise(
      n = unique(.data[["class"]]) |> length(),
      .groups = "drop"
    ) |>
    dplyr::filter(
      .data[["n"]] > 1L
    ) |>
    dplyr::mutate(
      alt_names = required_cols[.data[["df_name"]]] |>
        sapply(cli::ansi_collapse, sep2 = ", or ", last = ", or ")
    )

  # error message if non matching classes
  if (nrow(lst_class_non_match) != 0L) {
    cli::cli_abort(
      c(
        "x" = "{cli::qty(lst_class_non_match$df_name)} Column{?s} with
        non-matching classes:",
        paste0("* \"", lst_class_non_match$df_name,
               "\" with alternative names: ",
               lst_class_non_match$alt_names),
        "i" = "Column classes should be identical between datasets to be
        normalized."
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )
  }

  # return list of required colnames ----

  return(lst_req_col)

}

#' Check if bridge or cross-platform normalization
#'
#' @author
#'   Klev Diamanti
#'
#' @description
#' A function to check whether we are to perform simple bridge normalization, or
#' cross-platform (Olink Explore 3072 - Olink Explore HT/Olink Reveal) normalization.
#'
#' The function uses the internal dataset \var{eHT_e3072_mapping} to determine
#' the product source of each dataset. If both datasets originate from the same
#' Olink product, then it will return
#' `r OlinkAnalyze:::olink_norm_modes$bridge`. If the datasets to be normalized
#' originate from Olink Explore HT and Olink Explore 3072
#' or Olink Reveal and Olink Explore 3072, it will return
#' `r OlinkAnalyze:::olink_norm_modes$norm_cross_product`. In any other case an error is
#' thrown.
#'
#' @param lst_df Named list of datasets to be normalized.
#' @param lst_cols Named list of vectors with the required column names for each
#' dataset in \var{lst_df}.
#' @param reference_project Project name of reference_project. Should be one of
#' \var{df1_project_nr} or \var{df2_project_nr}. Indicates the project to which
#' the other project is adjusted to.
#' @param lst_product list of lists containing named list of product
#' (3k, Reveal, HT, or other) and reference (ref or not_ref) for each project
#'
#' @return Character string indicating the type of normalization to be
#' performed. One of
#' `r cli::ansi_collapse(x = OlinkAnalyze:::olink_norm_modes, sep2 = " or ", last = " or ")`. # nolint
#' And the updated list of datasets in case of cross-platform normalization.
#'
olink_norm_input_cross_product <- function(lst_df,
                                           lst_cols,
                                           reference_project,
                                           lst_product) {

  # check and correct norm_mode if needed ----

  # check if each df comes from a different olink product


  # if all elements of the array contain the same product, it is simple
  # bridge normalization. In case of 3k-3k bridging lst_product should contain
  # only "3k" as element. For other olink products, all elements should be
  # NA_character.
  # If more than one products are in the vector, then it should be exclusively
  # 3k and HT or 3k and Reveal.
  # In any other case (e.g. 3k and NA_character) means that one df is 3k, but
  # the other one probably T96, T48, which we do not normalize.
  lst_prod_uniq <- lst_product$product |> unique() |> sort()
  if (length(lst_prod_uniq) == 1L
      && all(lst_prod_uniq %in% c("3k", "HT", "Reveal","other"))) {
    norm_mode <- olink_norm_modes$bridge
  } else if (identical(x = lst_prod_uniq, y = c("3k", "HT"))) {
    norm_mode <- olink_norm_modes$norm_cross_product
  } else if (identical(x = lst_prod_uniq, y = c("3k", "Reveal"))) {
    norm_mode <- olink_norm_modes$norm_cross_product
  } else {
    cli::cli_abort(
      c(
        "x" = "Unexpected datasets to be bridge normalized!",
        "i" = paste0("Normalization is supported within",
                     " the same Olink product or between the following",
                     " sets of products where the first product must be",
                     " the reference product:"),
        "*" = "Explore HT and Explore 3072",
        "*" = "Reveal and Explore 3072"),
      call = rlang::caller_env(),
      wrap = TRUE
    )
  }

  # check if reference dataset is HT or Reveal if cross-product normalization ----

  if (norm_mode == olink_norm_modes$norm_cross_product
      && (!(lst_product$product[lst_product$reference == "ref"] %in% c("Reveal", "HT"))))  {

    cli::cli_abort(
      c(
        "x" = "Incorrect reference project!",
        "i" = "When normalizing between Olink Explore 3072 and Olink Explore
        HT or Olink Reveal, the latter should be set as reference project in
        {.arg reference_project}!"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

  # cross-product specific checks ----

  if (norm_mode == olink_norm_modes$norm_cross_product) {

    # update Olink assay identifiers if cross product normalization ----

    # add combined OlinkID to HT dataset
    l_ref_name <- names(lst_product$product)[lst_product$reference == "ref"]
    ref_product <- lst_product$product[lst_product$reference == "ref"]
    l_ref_oid_rename <- paste0(lst_cols[[l_ref_name]]$olink_id, "_",
                               ref_product)
    ref_3k_map_ref_rename <- stats::setNames(
      object = c(paste0("OlinkID_", ref_product), "OlinkID"),
      nm = c(paste0("OlinkID_", ref_product), lst_cols[[l_ref_name]]$olink_id)
    )

    ref_map_3k <- mapping_file_id(ref_product = ref_product)

    lst_df[[l_ref_name]] <- lst_df[[l_ref_name]] |>
      dplyr::rename(
        !!l_ref_oid_rename := lst_cols[[l_ref_name]]$olink_id
      ) |>
      dplyr::left_join(
        ref_map_3k |>
          dplyr::select(
            dplyr::all_of(
              ref_3k_map_ref_rename
            )
          ),
        by = stats::setNames(object = paste0("OlinkID_",ref_product), nm = l_ref_oid_rename),
        relationship = "many-to-many"
      ) |>
      # If matched OlinkID is not found in mapping file, set OlinkID_ref to OlinkID
        dplyr::mutate(OlinkID = ifelse(is.na(.data[["OlinkID"]]),
                                       .data[[paste0("OlinkID_", ref_product)]],
                                       .data[["OlinkID"]]))


    # add combined OlinkID to 3k dataset
    l_3k_name <- names(lst_product$product)[lst_product$product == "3k"]
    l_3k_oid_rename <- paste0(lst_cols[[l_3k_name]]$olink_id, "_E3072")
    ref_3k_map_3k_rename <- stats::setNames(
      object = c("OlinkID_E3072", "OlinkID"),
      nm = c("OlinkID_E3072", lst_cols[[l_3k_name]]$olink_id)
    )
    lst_df[[l_3k_name]] <- lst_df[[l_3k_name]] |>
      dplyr::rename(
        !!l_3k_oid_rename := lst_cols[[l_3k_name]]$olink_id
      ) |>
      dplyr::left_join(
        ref_map_3k |>
          dplyr::select(
            dplyr::all_of(
              ref_3k_map_3k_rename
            )
          ),
        by = stats::setNames(object = "OlinkID_E3072", nm = l_3k_oid_rename),
        relationship = "many-to-one"
      ) |>
      # If matched OlinkID is not found in mapping file, set OlinkID_HT to OlinkID
      dplyr::mutate(OlinkID = ifelse(is.na(.data[["OlinkID"]]),
                                     .data[[l_3k_oid_rename]],
                                     .data[["OlinkID"]]))

    # check if both datasets contain the count column ----

    check_cnt <- lapply(lst_cols, function(x) x[["count"]]) |> unlist()

    if (length(check_cnt) != 2L) {
      cnt_miss <- names(lst_cols)[!(names(lst_cols) %in% names(check_cnt))]

      cli::cli_abort(
        c(
          "x" = "Column {.val {\"Count\"}} not found in {cli::qty(cnt_miss)}
        dataset{?s} {.val {cnt_miss}}!",
          "i" = "When performing cross-product normalization, count values from
        both datasets are required for QS normalization. Re-export of NPX files
        from Olink software may be required."
        ),
        call = rlang::caller_env(),
        wrap = FALSE
      )
    }

  }

  # return ----

  return(
    list(
      norm_mode = norm_mode,
      lst_df = lst_df
    )
  )

}

#' Check reference samples to be used for normalization
#'
#' @description
#' This function takes as input a two named lists of character vectors with
#' matching names and checks the validity of the reference samples. In case of 1
#' set of df samples, then all checks are skipped as reference median
#' normalization is to be performed.
#'
#' @author
#'   Klev Diamanti
#'
#' @param lst_df_samples Named list of all sample identifiers from datasets to
#' be normalized.
#' @param lst_ref_samples Named list of reference sample identifiers to be used
#' for normalization.
#' @param norm_mode Character string indicating the type of normalization to be
#' performed. Expecting one of
#' `r cli::ansi_collapse(x = OlinkAnalyze:::olink_norm_modes, sep2 = " or ", last = " or ")`. # nolint
#'
#' @return `NULL` if no warning or error.
#'
#' @examples
#' \donttest{
#' # Reference median normalization
#' OlinkAnalyze:::olink_norm_input_check_samples(
#'   lst_df_samples = list(
#'     "p1" = unique(npx_data1$SampleID)
#'   ),
#'   lst_ref_samples = list(
#'     "p1" = npx_data1 |>
#'       dplyr::filter(
#'         !grepl(pattern = "CONTROL_SAMPLE",
#'         x = .data[["SampleID"]],
#'         fixed = TRUE)
#'       ) |>
#'       dplyr::pull(.data[["SampleID"]]) |>
#'       unique() |>
#'       sort() |>
#'       head(n = 6L)
#'   ),
#'   norm_mode = "ref_median"
#' )
#'
#' # Bridge normalization
#' ref_samples_bridge <- intersect(x = npx_data1$SampleID,
#'                                 y = npx_data2$SampleID) |>
#'   (\(x) x[!grepl(pattern = "CONTROL_SAMPLE", x = x, fixed = TRUE)])()
#'
#' OlinkAnalyze:::olink_norm_input_check_samples(
#'   lst_df_samples = list(
#'     "p1" = unique(npx_data1$SampleID),
#'     "p2" = unique(npx_data2$SampleID)
#'   ),
#'   lst_ref_samples = list(
#'     "p1" = ref_samples_bridge,
#'     "p2" = ref_samples_bridge
#'   ),
#'   norm_mode = "bridge"
#' )
#'
#' # Subset normalization
#' ref_samples_subset_1 <- npx_data1 |>
#'   dplyr::filter(
#'     !grepl(pattern = "CONTROL_SAMPLE",
#'            x = .data[["SampleID"]],
#'            fixed = TRUE)
#'     & .data[["QC_Warning"]] == "Pass"
#'   ) |>
#'   dplyr::pull(
#'     .data[["SampleID"]]
#'   ) |>
#'   unique()
#' ref_samples_subset_2 <- npx_data2 |>
#'   dplyr::filter(
#'     !grepl(pattern = "CONTROL_SAMPLE",
#'            x = .data[["SampleID"]],
#'            fixed = TRUE)
#'     & .data[["QC_Warning"]] == "Pass"
#'   ) |>
#'   dplyr::pull(
#'     .data[["SampleID"]]
#'   ) |>
#'   unique()
#'
#' OlinkAnalyze:::olink_norm_input_check_samples(
#'   lst_df_samples = list(
#'     "p1" = unique(npx_data1$SampleID),
#'     "p2" = unique(npx_data2$SampleID)
#'   ),
#'   lst_ref_samples = list(
#'     "p1" = ref_samples_subset_1,
#'     "p2" = ref_samples_subset_2
#'   ),
#'   norm_mode = "subset"
#' )
#' }
#'
olink_norm_input_check_samples <- function(lst_df_samples,
                                           lst_ref_samples,
                                           norm_mode) {

  if (!(length(lst_df_samples) %in% c(1L, 2L))) {
    # if 0 or more than 2 datasets are provided
    cli::cli_abort(
      c(
        "x" = "{cli::qty(lst_df_samples)} {?No/One/More than 2} set{?s} of
        samples provided in {.var lst_df_samples}!",
        "i" = "Expected 1 or 2 sets."
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )
  }

  if (!(length(lst_ref_samples) %in% c(1L, 2L))) {
    # if 0 or more than 2 sample sets are provided
    cli::cli_abort(
      c(
        "x" = "{cli::qty(lst_ref_samples)} {?No/One/More than 2} set{?s} of
        samples provided in {.var lst_ref_samples}!",
        "i" = "Expected 1 or 2 sets."
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )
  }

  # check only if there are 1 or 2 datasets provided. if yes, it means that we
  # are performing a reference median, bridge or subset normalization and in
  # this case reference samples should be checked.
  if (length(lst_df_samples) == length(lst_ref_samples)) {
    ## missing samples ----

    # find samples in lst_ref_samples that are not present in the dataset
    miss_samples <- lapply(names(lst_df_samples), function(n_df) {
      setdiff(
        x = lst_ref_samples[[n_df]],
        y = lst_df_samples[[n_df]]
      ) |>
        cli::ansi_collapse()
    }) |>
      unlist()
    names(miss_samples) <- names(lst_df_samples)
    # remove instances with no missing samples
    miss_samples <- miss_samples[nchar(miss_samples) > 0L]

    # error message if there are missing samples
    if (!all(sapply(miss_samples, nchar) == 0L)) {
      cli::cli_abort(
        c(
          "x" = "Normalization sample(s) missing from {cli::qty(miss_samples)}
        dataset{?s}:",
          paste0("* ", names(miss_samples), ": ", unlist(miss_samples)),
          "i" = "Sample identifiers are separated by comma (,)."
        ),
        call = rlang::caller_env(),
        wrap = FALSE
      )
    }

    ## duplicate samples ----

    # check that there are no duplicate sample identifiers within vectors of
    # lst_ref_samples
    if (lst_ref_samples |> lapply(duplicated) |> sapply(any) |> any()) {
      # get duplicated samples
      lst_sample_dups <- lst_ref_samples |>
        lapply(duplicated) |>
        sapply(any) |>
        (\(x) {
          lst_ref_samples[x] |>
            lapply(function(y) {
              y[duplicated(y)] |>
                unique() |>
                cli::ansi_collapse()
            })
        })()

      # error message for duplicated samples
      cli::cli_abort(
        c(
          "x" = "Duplicated reference sample identifier(s) detected in
          {cli::qty(lst_sample_dups)} vector{?s}:",
          paste0("* ", names(lst_sample_dups), ": ", unlist(lst_sample_dups)),
          "i" = "Expected no duplicates."
        ),
        call = rlang::caller_env(),
        wrap = FALSE
      )
    }

    ## equal number of bridge samples ----

    # check the number of samples is equal if bridge normalization
    if (tolower(norm_mode) %in% c(olink_norm_modes$bridge,
                                  olink_norm_modes$norm_cross_product)
        && sapply(lst_ref_samples, length) |> unique() |> length() != 1L) {
      # error message for uneven number of bridge samples
      cli::cli_abort(
        c(
          "x" = "There are {length(lst_ref_samples[[1L]])} bridge samples for
          dataset {.var {names(lst_ref_samples)[1L]}} and
          {length(lst_ref_samples[[2L]])} bridge samples for dataset
          {.var {names(lst_ref_samples)[2L]}}!",
          "i" = "Expected the same number of samples."
        ),
        call = rlang::caller_env(),
        wrap = FALSE
      )
    }

  } else {

    # if lst_df_samples is 2 but lst_ref_samples is anything else then
    # lst_df_samples and lst_ref_samples do not match
    cli::cli_abort(
      c(
        "x" = "Number of sample vectors in {.var lst_df_samples} differs from
        the number of reference sample vectors in {.var lst_ref_samples}!",
        "i" = "Expected equal number of vectors of samples."
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }
}

#' Check datasets of \var{reference_medians}
#'
#' @author
#'   Klev Diamanti
#'
#' @param reference_medians Dataset with columns "OlinkID" and "Reference_NPX".
#' Used for reference median normalization.
#'
#' @return `NULL` otherwise error.
#'
olink_norm_input_ref_medians <- function(reference_medians) {

  # check columns ----

  if (!identical(x = sort(names(reference_medians)),
                 y = sort(olink_norm_ref_median_cols$cols))) {

    cli::cli_abort(
      c(
        "x" = "{.arg reference_medians} should have
        {length(olink_norm_ref_median_cols)} columns!",
        "i" = "Expected: {.val {unlist(olink_norm_ref_median_cols)}}"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

  # check class ----

  ref_med_class <- sapply(
    seq_len(nrow(olink_norm_ref_median_cols)),
    function(i) {
      reference_medians |>
        dplyr::select(
          dplyr::all_of(
            olink_norm_ref_median_cols$cols[i]
          )
        ) |>
        dplyr::collect() |>
        dplyr::pull(
          .data[[olink_norm_ref_median_cols$cols[i]]]
        ) |>
        (\(x) inherits(x = x, what = olink_norm_ref_median_cols$class[i]))()
    }
  )
  names(ref_med_class) <- olink_norm_ref_median_cols$cols

  if (any(ref_med_class == FALSE)) {

    wrong_class <- names(ref_med_class)[ref_med_class == FALSE] # nolint

    cli::cli_abort(
      c(
        "x" = "{cli::qty(wrong_class)} Column{?s} {.val {wrong_class}} of
        {.arg reference_medians} {?has/have} the wrong class!",
        "i" = "Expected:",
        olink_norm_ref_median_cols |>
          dplyr::mutate(
            x = paste0("* ", .data[["cols"]], ": ", .data[["class"]])
          ) |>
          dplyr::pull(.data[["x"]])
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

  # check duplicates ----

  oid_name <- olink_norm_ref_median_cols |>
    dplyr::filter(
      .data[["name"]] == "olink_id"
    ) |>
    dplyr::pull(
      .data[["cols"]]
    )
  oid_dups <- reference_medians |>
    dplyr::count(
      .data[[oid_name]]
    ) |>
    dplyr::filter(
      .data[["n"]] > 1L
    ) |>
    dplyr::select(
      dplyr::all_of(oid_name)
    ) |>
    dplyr::collect() |>
    dplyr::pull(
      .data[[oid_name]]
    ) |>
    unique()

  if (length(oid_dups) > 0L) {

    cli::cli_abort(
      c(
        "x" = "Found {length(oid_dups)} duplicated {cli::qty(oid_dups)}
        assay{?s} in {.arg reference_medians}: {.val {oid_dups}}.",
        "i" = "Expected no duplicates!"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

}

#' Check \var{datasets} and \var{reference_medians} for unexpected Olink
#' identifiers or excluded assays
#'
#' @author
#'   Klev Diamanti
#'
#' @param lst_df Named list of datasets to be normalized.
#' @param reference_medians Dataset with columns "OlinkID" and "Reference_NPX".
#' Used for reference median normalization.
#' @param lst_cols Named list of vectors with the required column names for each
#' dataset in \var{lst_df}.
#' @param norm_mode Character string indicating the type of normalization to be
#' performed. Expecting one of
#' `r cli::ansi_collapse(x = OlinkAnalyze:::olink_norm_modes, sep2 = " or ", last = " or ")`. # nolint
#'
#' @return A named list containing \var{lst_df} and \var{reference_medians}
#' stripped from unexpected Olink identifiers or excluded assays
#'
olink_norm_input_clean_assays <- function(lst_df,
                                          reference_medians,
                                          lst_cols,
                                          norm_mode) {
  # help functions ----

  # remove all assays that do not match the pattern and that have NA for OlinkID
  check_oid <- function(df, col_name, norm_mode) {
    if (norm_mode == olink_norm_modes$norm_cross_product) {
      df |>
        dplyr::filter(
          grepl(
            pattern = "^OID\\d{5}_OID\\d{5}$|^OID\\d{5}$",
            x = .data[[col_name]]
          )
        )
    } else {
      df |>
        dplyr::filter(
          grepl(
            pattern = "^OID\\d{5}$",
            x = .data[[col_name]]
          )
        )
    }
  }

  # help variables ----

  lst_out <- list()

  # remove assays ----

  ## remove non-OID assays ----

  ### remove from input df ----

  lst_df_oid <- lapply(names(lst_df), function(l_name) {
    check_oid(df = lst_df[[l_name]],
              col_name = lst_cols[[l_name]]$olink_id,
              norm_mode = norm_mode)
  })
  names(lst_df_oid) <- names(lst_df)
  lst_out$lst_df <- lst_df_oid

  # message to inform the user

  # first find the removed assays
  oid_removed <- lapply(names(lst_df), function(l_name) {
    # OlinkID in the original dataset
    oid_orig <- lst_df[[l_name]] |>
      dplyr::select(
        dplyr::all_of(lst_cols[[l_name]]$olink_id)
      ) |>
      dplyr::distinct() |>
      dplyr::collect() |>
      dplyr::pull(
        .data[[lst_cols[[l_name]]$olink_id]]
      )
    # OlinkID in the cleaned dataset
    oid_out <- lst_df_oid[[l_name]] |>
      dplyr::select(
        dplyr::all_of(lst_cols[[l_name]]$olink_id)
      ) |>
      dplyr::distinct() |>
      dplyr::collect() |>
      dplyr::pull(
        .data[[lst_cols[[l_name]]$olink_id]]
      )
    setdiff(x = oid_orig,
            y = oid_out) |>
      cli::ansi_collapse()
  })
  names(oid_removed) <- names(lst_df)
  # remove entries with no missing assays
  oid_removed <- oid_removed[sapply(oid_removed, nchar) > 0L]

  # message to user
  if (length(oid_removed) > 0L) {
    cli::cli_inform(
      c("Assay(s) from the following input {cli::qty(oid_removed)} dataset{?s}
      have been excluded from normalization:",
        paste0("* ", names(oid_removed), ": ", unlist(oid_removed)),
        "i" = "Lacking the pattern \"OID\" followed by 5 digits."
      )
    )
  }

  ### remove from reference medians ----

  if (!is.null(reference_medians)) {
    reference_medians_out <- check_oid(df = reference_medians,
                                       col_name = "OlinkID",
                                       norm_mode = norm_mode)
    lst_out$reference_medians <- reference_medians_out

    # error message to use that all assays were removed
    if (nrow(lst_out$reference_medians) == 0L) {
      cli::cli_abort(
        c(
          "x" = "All assays were removed from input {.arg reference_medians}!",
          "i" = "No assay identifiers matched the pattern \"OID\" followed by 5
          digits."
        ),
        call = rlang::caller_env(),
        wrap = FALSE
      )
    }

    # message to inform the user

    # first find the removed assays
    oid_ref_med_orig <- reference_medians |>
      dplyr::select(
        dplyr::all_of("OlinkID")
      ) |>
      dplyr::distinct() |>
      dplyr::collect() |>
      dplyr::pull(
        .data[["OlinkID"]]
      )
    # OlinkID in the cleaned reference_medians
    oid_ref_med_out <- reference_medians_out |>
      dplyr::select(
        dplyr::all_of("OlinkID")
      ) |>
      dplyr::distinct() |>
      dplyr::collect() |>
      dplyr::pull(
        .data[["OlinkID"]]
      )
    oid_ref_med_removed <- setdiff(x = oid_ref_med_orig,
                                   y = oid_ref_med_out) |>
      cli::ansi_collapse()

    # message to user
    if (nchar(oid_ref_med_removed) > 0L) {
      cli::cli_inform(
        "Assay(s) from the reference median dataset have been excluded from
      normalization: {oid_ref_med_removed}.",
        "i" = "Lacking the pattern \"OID\" followed by 5 digits."
      )
    }
  } else {
    lst_out$reference_medians <- NULL
  }

  ## remove excluded assays ----

  excluded_assay_flag <- "EXCLUDED"

  lst_df_excluded <- lapply(names(lst_df_oid), function(l_name) {
    if (length(lst_cols[[l_name]]$normalization) > 0L) {
      lst_df_oid[[l_name]] |>
        dplyr::filter(
          .data[[lst_cols[[l_name]]$normalization]] != excluded_assay_flag
        )
    } else {
      lst_df_oid[[l_name]]
    }
  })
  names(lst_df_excluded) <- names(lst_df_oid)
  lst_out$lst_df <- lst_df_excluded

  # check that df's have still rows
  if (any(sapply(lst_out$lst_df, nrow) == 0L)) {
    no_row_df <- names(lst_out$lst_df)[sapply(lst_out$lst_df, nrow) == 0L] # nolint

    cli::cli_abort(
      c(
        "x" = "All assays were removed from {cli::qty(no_row_df)} dataset{?s}
        {.val {no_row_df}}!",
        "i" = "No assay identifiers matched the pattern \"OID\" followed by 5
          digits, or assays were marked as \"{excluded_assay_flag}\""
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )
  }

  # message to inform the user

  # first find the removed assays
  oid_excluded <- lapply(names(lst_df_oid), function(l_name) {
    # OlinkID in the original dataset
    oid_orig <- lst_df_oid[[l_name]] |>
      dplyr::select(
        dplyr::all_of(lst_cols[[l_name]]$olink_id)
      ) |>
      dplyr::distinct() |>
      dplyr::collect() |>
      dplyr::pull(
        .data[[lst_cols[[l_name]]$olink_id]]
      )
    # OlinkID in the cleaned dataset
    oid_out <- lst_df_excluded[[l_name]] |>
      dplyr::select(
        dplyr::all_of(lst_cols[[l_name]]$olink_id)
      ) |>
      dplyr::distinct() |>
      dplyr::collect() |>
      dplyr::pull(
        .data[[lst_cols[[l_name]]$olink_id]]
      )
    setdiff(x = oid_orig,
            y = oid_out) |>
      cli::ansi_collapse()
  })
  names(oid_excluded) <- names(lst_df_oid)
  # remove entries with no missing assays
  oid_excluded <- oid_excluded[sapply(oid_excluded, nchar) > 0L]

  # message to user
  if (length(oid_excluded) > 0L) {
    cli::cli_inform(
      c("Assay(s) from the following input {cli::qty(oid_excluded)} dataset{?s}
      have been excluded from normalization:",
        paste0("* ", names(oid_excluded), ": ", unlist(oid_excluded)),
        "i" = "Were marked as \"{excluded_assay_flag}\"."
      )
    )
  }

  # return ----

  return(lst_out)
}

#' Check \var{datasets} and \var{reference_medians} for Olink identifiers not
#' shared across datasets.
#'
#' @author
#'   Klev Diamanti
#'
#' @param lst_df Named list of datasets to be normalized.
#' @param reference_medians Dataset with columns "OlinkID" and "Reference_NPX".
#' Used for reference median normalization.
#' @param lst_cols Named list of vectors with the required column names for each
#' dataset in \var{lst_df}.
#' @param norm_mode Character string indicating the type of normalization to be
#' performed. Expecting one of
#' `r cli::ansi_collapse(x = OlinkAnalyze:::olink_norm_modes, sep2 = " or ", last = " or ")`. # nolint
#'
#' @return A named list containing \var{lst_df} and \var{reference_medians}
#' will assays shared across all datasets.
#'
olink_norm_input_assay_overlap <- function(lst_df,
                                           reference_medians,
                                           lst_cols,
                                           norm_mode = norm_mode) {
  # help variables
  lst_out <- list()

  # get unique OID for each dataset
  lst_df_oid <- lapply(names(lst_df), function(l_name) {
    lst_df[[l_name]] |>
      dplyr::select(
        dplyr::all_of(lst_cols[[l_name]]$olink_id)
      ) |>
      dplyr::distinct() |>
      dplyr::collect() |>
      dplyr::pull(
        .data[[lst_cols[[l_name]]$olink_id]]
      )
  })
  names(lst_df_oid) <- names(lst_df)

  # add reference medians to lst_df_oid, if available
  if (!is.null(reference_medians)) {
    lst_df_oid$reference_medians <- reference_medians |>
      dplyr::select(
        dplyr::all_of("OlinkID")
      ) |>
      dplyr::distinct() |>
      dplyr::collect() |>
      dplyr::pull(
        .data[["OlinkID"]]
      )
  }

  # check for non-shared OIDs
  oid_combos_miss <- expand.grid(X = names(lst_df_oid),
                                 Y = names(lst_df_oid)) |>
    dplyr::as_tibble() |>
    dplyr::filter(
      .data[["X"]] != .data[["Y"]]
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      Z = setdiff(x = lst_df_oid[[.data[["X"]]]],
                  y = lst_df_oid[[.data[["Y"]]]]) |>
        list()
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      L = sapply(.data[["Z"]], length),
      M = sapply(.data[["Z"]], cli::ansi_collapse),
      M = paste0("In ", .data[["X"]], " & not in ", .data[["Y"]], ": ",
                 .data[["M"]])
    ) |>
    dplyr::filter(
      .data[["L"]] != 0L
    )
  oid_removed <- oid_combos_miss$Z |> unlist() |> unique()

  # remove non-shared assays and throw a warning message about it
  if (nrow(oid_combos_miss) > 0L) {

    # remove non-shared assays
    lst_out$lst_df <- lapply(names(lst_df), function(l_name) {
      lst_df[[l_name]] |>
        dplyr::filter(
          !(.data[[lst_cols[[l_name]]$olink_id]] %in% oid_removed)
        )
    })
    names(lst_out$lst_df) <- names(lst_df)

    # remove from reference_medians too, if available
    if (!is.null(reference_medians)) {
      lst_out$reference_medians <- reference_medians |>
        dplyr::filter(
          !(.data[["OlinkID"]] %in% oid_removed)
        )
    }
    if (norm_mode == olink_norm_modes$norm_cross_product){
      cli::cli_warn(
        c(
          "{length(oid_removed)} assay{?s} are not shared across products.",
          "i" = "{length(oid_removed)} assay{?s} will be removed from
        normalization."
        ),
        wrap = FALSE)

    } else{    # warning message
    cli::cli_warn(
      c(
        "Assay{?s} {.val {oid_removed}} not shared across input dataset(s):",
        dplyr::pull(oid_combos_miss, .data[["M"]]),
        "i" = "{cli::qty(oid_removed)} Assay{?s} will be removed from
        normalization."
      ),
      wrap = FALSE
    )}


  } else {

    # if all assays shared, return original datasets
    lst_out <- list(
      lst_df = lst_df,
      reference_medians = reference_medians
    )

  }

  # return
  return(lst_out)
}

#' Check \var{datasets} and \var{reference_medians} for Olink identifiers not
#' shared across datasets.
#'
#' @author
#'   Klev Diamanti;
#'   Kathleen Nevola
#'
#' @param lst_df Named list of datasets to be normalized.
#' @param lst_cols Named list of vectors with the required column names for each
#' dataset in \var{lst_df}.
#'
#' @return `NULL` if all assays are normalized with the same approach.
#'
olink_norm_input_norm_method <- function(lst_df,
                                         lst_cols) {
  all_norm_present <- lst_cols |>
    sapply(function(x) !identical(x = x$normalization, y = character(0L))) |>
    all()

  if (all_norm_present && length(lst_df) == 2L) {

    lst_df_norm <- lapply(names(lst_df), function(l_name) {
      select_cols <- c(lst_cols[[l_name]]$olink_id,
                       lst_cols[[l_name]]$normalization)
      names(select_cols) <- c("olink_id", l_name)
      lst_df[[l_name]] |>
        # EXCLUDED assays have been removed already in
        # olink_norm_input_clean_assays
        dplyr::select(
          dplyr::all_of(select_cols)
        ) |>
        dplyr::distinct() |>
        dplyr::collect()
    })
    names(lst_df_norm) <- names(lst_df)

    oid_norm_diff <- lst_df_norm[[1L]] |>
      # we assume that there are no duplicated assays within df and that assays
      # were normalized with the same approach within df
      dplyr::inner_join(
        lst_df_norm[[2L]],
        by = "olink_id",
        relationship = "one-to-one"
      ) |>
      dplyr::filter(
        .data[[names(lst_df_norm)[1L]]] != .data[[names(lst_df_norm)[2L]]]
      ) |>
      dplyr::pull(
        .data[["olink_id"]]
      )

    if (!identical(oid_norm_diff, character(0L))) {
      cli::cli_warn(
        c(
          "{length(oid_norm_diff)} {cli::qty(oid_norm_diff)} assay{?s} not
          normalized with the same approach: {.val {oid_norm_diff}}",
          "i" = "Consider renormalizing!"
        )
      )
    }

  } else if (length(lst_df) != 2L) {
    cli::cli_abort(
      c(
        "x" = "Unable to check if all assays were normalized with the same
        approach!",
        "i" = "Can apply only to 2 datasets."
      )
    )
  } else {
    cli::cli_abort(
      c(
        "x" = "Unable to check if all assays were normalized with the same
        approach!",
        "i" = "Column {.var {\"Normalization\"}} not present in all datasets."
      )
    )
  }
}

#' Identify names of product for each project
#'
#' @author
#'   Kathy Nevola
#'   Klev Diamanti
#'
#' @param lst_df Named list of datasets to be normalized.
#' @param lst_cols Named list of vectors with the required column names for each
#' dataset in \var{lst_df}.
#'
#' @return Named character vector with the Olink product name that  each input
#' datatset matches to.
#'
olink_norm_product_id <- function(lst_df,
                                  lst_cols) {
  # Identify product from panel names by going through the input datasets
  lst_product <- sapply(names(lst_df), function(d_name) {
    # get unique panels
    u_panel <- lst_df[[d_name]] |>
      dplyr::pull(
        .data[[lst_cols[[d_name]]$panel]]
      ) |>
      unique()
    if (all(u_panel %in% eHT_e3072_mapping$Panel_E3072)) {
      return("3k")
    } else if (all(u_panel == "Explore_HT")) {
      return("HT")
    } else if (all(u_panel == "Reveal")) {
      return("Reveal")
    } else {
      return("other")
    }
  })
  names(lst_product) <- names(lst_df)

  return(lst_product)

}

#' Identify reference project.
#'
#' @author
#'   Kathy Nevola
#'   Klev Diamanti
#'
#' @param lst_product Named character vector with the Olink product name that
#' each input datatset matches to.
#' @param reference_project Project name of reference_project. Should be one of
#' \var{df1_project_nr} or \var{df2_project_nr}. Indicates the project to which
#' the other project is adjusted to.
#'
#' @returns  Named character vector with \var{df1_project_nr} and
#' \var{df2_project_nr} marked as "ref" and "not_ref".
#'
olink_nrom_reference_id <- function(lst_product,
                                    reference_project) {

  ref_names <- lst_product
  ref_names[names(ref_names) %in% reference_project] <- "ref"
  ref_names[!(names(ref_names) %in% reference_project)] <- "not_ref"

  return(ref_names)
}

#' Identifying which mapping file to use
#'
#' @param ref_product one of "HT" or "Reveal" depending on reference product
#'
#' @return dataframe of mapping file to use for OlinkID mapping
#' (eHT_e3072_mapping or reveal_e3072_mapping)
#'
mapping_file_id <- function(ref_product){
  # Ref mapping file
  if(ref_product == "HT") {
    ref_map_3k <- eHT_e3072_mapping
  }
  if (ref_product == "Reveal"){
    ref_map_3k <- reveal_e3072_mapping
  }
  return(ref_map_3k)
}
