#' Check NPX data format
#'
#' @description
#' This function performs various checks on NPX data, including checking
#' column names, validating Olink identifiers, identifying assays with \emph{NA}
#' values for all samples and detecting duplicate sample identifiers.
#'
#' @details
#' OlinkAnalyze uses pre-defined names of columns of data frames to perform
#' downstream analyses. At the same time, different Olink platforms export data
#' with different column names (e.g. different protein quantification metric).
#' This function aims to instruct each function of OlinkAnalyze on the column it
#' should be using for the downstream analysis. This should be seamless for data
#' exported from Olink Software and imported to R using the read_npx function.
#'
#' However, in certain cases the columns of interest might be named differently.
#' This function allows assigning custom-named columns of a data frame to
#' internally expected variables that will in turn instruct Olink Analyze
#' functions to use them for downstream analysis. For example, if one wished to
#' use the column \var{PCNormalizedNPX} for their analysis instead of the
#' column \var{NPX}, then they can assign this new name to the internal
#' variable \var{quant} to inform the package that in the downstream analysis
#' \var{PCNormalizedNPX} should be used. See example 3.
#'
#' Similarly, in case of multiple matches (e.g. the data frame contains both
#' columns \var{LOD} and \var{PlateLOD}) the ties will need to be resolved by
#' the user using the argument \var{preferred_names} from this function.  See
#' example 4.
#'
#' The argument \var{preferred_names} is a named character vector with internal
#' column names as names and column names of the current data set as values.
#' Names of the input vector can be one or more of the following:
#' `r ansi_collapse_quot(x = column_name_dict$col_key)`
#'
#' @author
#'   Masoumeh Sheikhi
#'   Klev Diamanti
#'
#' @inheritParams .downstream_fun_args
#' @param preferred_names A named character vector where names are internal
#' column names and values are column names to be selected from the input data
#' frame. Read the \emph{description} for further information.
#'
#' @return A list containing the following elements:
#' \itemize{
#'   \item \strong{col_names} List of column names from the input data frame
#'   marking the columns to be used in downstream analyses.
#'   \item \strong{oid_invalid} Character vector of invalid \var{OlinkID}.
#'   \item \strong{assay_na} Character vector of assays with all samples having
#'   \emph{NA} values.
#'   \item \strong{sample_id_dups} Character vector of duplicate \var{SampleID}.
#'   \item \strong{col_class} Data frame with columns of incorrect type
#'   including column key \var{col_key}, column name \var{col_name}, detected
#'   column type \var{col_class} and expected column type
#'   \var{expected_col_class}.
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Example 0: Use npx_data1 to check that check_npx works
#' check_npx_result <- OlinkAnalyze::npx_data1 |>
#'   OlinkAnalyze::check_npx() |>
#'   suppressWarnings()
#'
#' # read NPX data
#' npx_file <- system.file("extdata",
#'                         "npx_data_ext.parquet",
#'                         package = "OlinkAnalyze")
#' npx_df <- OlinkAnalyze::read_npx(filename = npx_file)
#'
#' # Example 1: run df as is
#' OlinkAnalyze::check_npx(df = npx_df)
#'
#' # Example 2: SampleType missing from data frame
#' npx_df |>
#'   dplyr::select(
#'     -dplyr::all_of(
#'       c("SampleType")
#'     )
#'   ) |>
#'   OlinkAnalyze::check_npx()
#'
#' # Example 3: Use PCNormalizedNPX instead on NPX
#' OlinkAnalyze::check_npx(
#'   df = npx_df,
#'   preferred_names = c("quant" = "PCNormalizedNPX")
#' )
#'
#' # Example 4: Use PCNormalizedNPX instead on NPX, and PlateLOD instead of LOD
#' npx_df |>
#'   dplyr::mutate(
#'     LOD = 1L,
#'     PlateLOD = 2L
#'   ) |>
#'   OlinkAnalyze::check_npx(
#'     preferred_names = c("quant" = "PCNormalizedNPX",
#'                         "lod" = "PlateLOD")
#'   )
#' }
#'
check_npx <- function(df,
                      preferred_names = NULL) {

  # check input ----

  check_is_dataset(x = df,
                   error = TRUE)

  # check functions ----

  check_npx_out_lst <- list()

  # column names
  check_npx_out_lst$col_names <- check_npx_col_names(
    df = df,
    preferred_names = preferred_names
  )

  # check Olink IDs
  check_npx_out_lst$oid_invalid <- check_npx_olinkid(
    df = df,
    col_names = check_npx_out_lst$col_names
  )

  # assays with all NA values
  check_npx_out_lst$assay_na <- check_npx_all_na_assays(
    df = df,
    col_names = check_npx_out_lst$col_names
  )

  # duplicate sample IDs
  check_npx_out_lst$sample_id_dups <- check_npx_duplicate_sample_ids(
    df = df,
    col_names = check_npx_out_lst$col_names
  )

  # samples with all NA values
  check_npx_out_lst$sample_id_na <- check_npx_all_na_sample(
    df = df,
    col_names = check_npx_out_lst$col_names
  )

  # column classes
  check_npx_out_lst$col_class <- check_npx_col_class(
    df = df,
    col_names = check_npx_out_lst$col_names
  )

  # assay QC
  check_npx_out_lst$assay_qc <- check_npx_qcwarn_assays(
    df = df,
    col_names = check_npx_out_lst$col_names
  )

  # return results ----

  return(check_npx_out_lst)

}

#' Check, update and define column names used in downstream analyses
#'
#' @description
#' OlinkAnalyze uses pre-defined names of columns of data frames to perform
#' downstream analyses. At the same time, different Olink platforms export data
#' with different column names (e.g. different protein quantification metric).
#' This function aims to instruct each function of OlinkAnalyze on the column it
#' should be using for the downstream analysis. This should be seamless for data
#' exported from Olink Software and imported to R using the read_npx function.
#'
#' However, in certain cases the columns of interest might be named differently.
#' This function allows assigning custom-named columns of a data frame to
#' internally expected variables that will in turn instruct Olink Analyze
#' functions to use them for downstream analysis. For example, if one wished to
#' use the column \var{PCNormalizedNPX} for their analysis instead of the
#' column \var{NPX}, then they can assign this new name to the internal
#' variable \var{quant} to inform the package that in the downstream analysis
#' \var{PCNormalizedNPX} should be used. See example 3.
#'
#' Similarly, in case of multiple matches (e.g. the data frame contains both
#' columns \var{LOD} and \var{PlateLOD}) the ties will need to be resolved by
#' the user using the argument \var{preferred_names} from this function.  See
#' example 4.
#'
#' The argument \var{preferred_names} is a named character vector with internal
#' column names as names and column names of the current data set as values.
#' Names of the input vector can be one or more of the following:
#' `r cli::ansi_collapse(x = column_name_dict$col_key)`
#'
#' @author
#'  Klev Diamanti;
#'  Masoumeh Sheikhi
#'
#' @inheritParams check_npx
#'
#' @return List of column names from the input data frame marking the columns to
#' be used in downstream analyses.
#'
#' @examples
#' \donttest{
#' # read NPX data
#' npx_file <- system.file("extdata",
#'                         "npx_data_ext.parquet",
#'                         package = "OlinkAnalyze")
#' npx_df <- OlinkAnalyze::read_npx(filename = npx_file)
#'
#' # Example 1: run df as is
#' OlinkAnalyze:::check_npx_col_names(df = npx_df)
#'
#' # Example 2: SampleType missing from data frame
#' npx_df |>
#'   dplyr::select(
#'     -dplyr::all_of(
#'       c("SampleType")
#'     )
#'   ) |>
#'   OlinkAnalyze:::check_npx_col_names()
#'
#' # Example 3: Use PCNormalizedNPX instead on NPX
#' OlinkAnalyze:::check_npx_col_names(
#'   df = npx_df,
#'   preferred_names = c("quant" = "PCNormalizedNPX")
#' )
#'
#' # Example 4: Use PCNormalizedNPX instead on NPX, and PlateLOD instead of LOD
#' npx_df |>
#'   dplyr::mutate(
#'     LOD = 1L,
#'     PlateLOD = 2L
#'   ) |>
#'   OlinkAnalyze:::check_npx_col_names(
#'     preferred_names = c("quant" = "PCNormalizedNPX",
#'                         "lod" = "PlateLOD")
#'   )
#' }
#'
check_npx_col_names <- function(df,
                                preferred_names = NULL) {

  # if not NULL, preferred_names is checked in check_npx_update_col_names
  if (!is.null(preferred_names)) {

    column_name_dict_updated <- check_npx_update_col_names(
      preferred_names = preferred_names
    )

  } else {

    column_name_dict_updated <- column_name_dict |>
      dplyr::mutate(
        col_name_mod = FALSE
      )

  }

  # Intersect expected names to column names ----

  column_name_dict_updated <- column_name_dict_updated |>
    dplyr::mutate(
      col_df = lapply(
        .data[["col_names"]],
        function(x) {
          intersect( # nolint return_linter
            x = x,
            y = names(df)
          )
        }
      ),
      col_df_len = sapply(.data[["col_df"]], length)
    )

  # Check correctness of preferred_names ----

  # is user's input correct?
  # check if the user input has no matches to the column names of the data frame
  df_custom_names <- column_name_dict_updated |>
    dplyr::filter(
      .data[["col_name_mod"]] == TRUE
      & .data[["col_df_len"]] == 0L
    )

  if (!is.null(preferred_names) && nrow(df_custom_names) > 0L) {

    cli::cli_abort(
      c(
        "x" = "{cli::qty(df_custom_names$col_key)} Value{?s}
        {.val {unlist(df_custom_names$col_names)}} from {.arg preferred_names}
        corresponding to key{?s} {.val {df_custom_names$col_key}} {?is/are}
        missing from the input dataset {.arg df}.",
        "i" = "Please ensure all provided column names are present in the data!"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

  # check presence of required columns ----

  # keep all required cols for which there is no matching column in dataset
  df_req_cols <- column_name_dict_updated |>
    dplyr::filter(
      .data[["col_miss"]] == FALSE
      & .data[["col_df_len"]] == 0L
    )

  if (nrow(df_req_cols) > 0L) {

    miss_cols <- paste0(
      "* \"", df_req_cols$col_key, "\": One of ",
      sapply(df_req_cols$col_names,
             ansi_collapse_quot,
             sep = "or"), "."
    )

    cli::cli_abort(
      c("x" = "{cli::qty(df_req_cols$col_key)} There {?is/are} no column
        name{?s} associated with the following key{?s}:",
        miss_cols,
        "i" = "Please ensure presence of columns above in dataset {.arg df}. If
        columns are present and the column name does not match one of the
        expected ones, please use argument {.arg preferred_names} to point to
        the correct column."),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

  # break ties for multi-matches by order ----

  # keep all required cols for which there is no matching column in dataset
  df_multi_ties_cols <- column_name_dict_updated |>
    dplyr::filter(
      .data[["col_multi"]] == FALSE
      & .data[["col_df_len"]] > 1L
      & .data[["col_order"]] == TRUE
    ) |>
    dplyr::rename(
      "col_df_tmp" = "col_df"
    ) |>
    dplyr::mutate(
      col_df = lapply(.data[["col_df_tmp"]], utils::head, n = 1L),
      col_df_len = sapply(.data[["col_df"]], length)
    )

  if (nrow(df_multi_ties_cols) > 0L) {

    # update column_name_dict_updated
    column_name_dict_updated <- column_name_dict_updated |>
      dplyr::filter(
        !(.data[["col_key"]] %in% df_multi_ties_cols$col_key)
      ) |>
      dplyr::bind_rows(
        df_multi_ties_cols |>
          dplyr::select(
            -dplyr::all_of("col_df_tmp")
          )
      ) |>
      dplyr::arrange(
        match(x = .data[["col_key"]], table = column_name_dict$col_key)
      )

    # inform message string
    multi_ties_cols <- paste0(
      "* \"", df_multi_ties_cols$col_key, "\": \"",
      unlist(df_multi_ties_cols$col_df), "\" was selected. Options were ",
      sapply(df_multi_ties_cols$col_df_tmp,
             ansi_collapse_quot,
             sep = "or"), "."
    )

    cli::cli_inform(
      c("i" = "{cli::qty(df_multi_ties_cols$col_key)} More than one column names
      in {.arg df} was associated with certain key{?s}. One was selected based
      on an ordered list:",
        multi_ties_cols,
        "Please use {.arg preferred_names} to select a different column
        name."),
      wrap = FALSE
    )

  }


  # check multi-matches in non-multi cols columns ----

  # keep all cols that are not allowed to have multiple matches and have more
  # than one matching columns in dataset
  df_multi_cols <- column_name_dict_updated |>
    dplyr::filter(
      .data[["col_multi"]] == FALSE
      & .data[["col_df_len"]] > 1L
    )

  if (nrow(df_multi_cols) > 0L) {

    multi_cols <- paste0(
      "* \"", df_multi_cols$col_key, "\": ",
      sapply(df_multi_cols$col_names,
             ansi_collapse_quot,
             sep = "or"), "."
    )

    cli::cli_abort(
      c("x" = "{cli::qty(df_multi_cols$col_key)} There is more than one column
      names in {.arg df} associated with the following key{?s}:",
        multi_cols,
        "i" = "Please use {.arg preferred_names} to break ties of column
        names."),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

  # check if no columns from the data frame match the same key from
  # column_name_dict


  # return ----

  # remove any nullable columns
  column_name_df <- column_name_dict_updated |>
    dplyr::filter(
      .data[["col_df_len"]] >= 1L
    ) |>
    dplyr::pull(
      .data[["col_df"]]
    )

  return(column_name_df)
}

#' Update column names to be used in downstream analyses
#'
#' @description
#' OlinkAnalyze uses pre-defined names of columns of data frames to perform
#' downstream analyses. However, in certain cases the columns of interest might
#' be named differently. The aim of this function is to assign custom-named
#' columns of a data frame to internally expected variables that will in turn
#' enable analysis of Olink data. For example, if one wished to #' use the
#' column \var{PCNormalizedNPX} for their analysis instead of the column
#' \var{NPX}, then they can assign this new name to the internal variable
#' \var{quant} to inform the package that in the downstream analysis
#' \var{PCNormalizedNPX} should be used.
#'
#' This function takes as input a named character vector with internal column
#' names as names and column names of the current data set as values. Names of
#' the input vector can be one or more of the following:
#' `r cli::ansi_collapse(x = column_name_dict$col_key)`
#'
#' @author
#'  Klev Diamanti
#'  Masoumeh Sheikhi
#'
#' @inheritParams check_npx
#'
#' @return \var{column_name_dict} updated based on \var{preferred_names}.
#'
check_npx_update_col_names <- function(preferred_names) {

  # check input ----

  # Check if preferred_names is character
  check_is_character(x = preferred_names,
                     error = TRUE)

  # check for names not matching expected ----

  # Check valid names
  if (!all(names(preferred_names) %in% column_name_dict$col_key)) {

    # identify names of the vector preferred_names that do not match names from
    # column_name_dict. Names should match to be able to update the field.
    missing_names <- setdiff(x = names(preferred_names), # nolint object_usage_linter
                             y = column_name_dict$col_key)

    cli::cli_abort(
      c("x" = "Unexpected name{?s} in {.arg preferred_names}:
        {.val {missing_names}}!",
        "i" = "Expected one or more of the following names:
        {.val {column_name_dict$col_key}}"),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

  # check for duplicated names ----

  dup_names <- names(preferred_names)[duplicated(names(preferred_names))]

  if (length(dup_names) > 0L) {

    cli::cli_abort(
      c("x" = "Duplicated name{?s} in {.arg preferred_names}:
        {.val {dup_names}}!",
        "i" = "Expected unique names for each column."),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }


  # update column names ----

  # Do not update entries that are not specified in `preferred_names`
  column_name_dict_keep <- column_name_dict |>
    dplyr::filter(
      !(.data[["col_key"]] %in% names(preferred_names))
    ) |>
    dplyr::mutate(
      col_name_mod = FALSE
    )
  # Update entries that are specified in `preferred_names`
  column_name_dict_change <- column_name_dict |>
    dplyr::filter(
      .data[["col_key"]] %in% names(preferred_names)
    ) |>
    dplyr::arrange(
      match(
        x = .data[["col_key"]],
        table = names(preferred_names)
      )
    ) |>
    dplyr::mutate(
      col_names = as.list(.env[["preferred_names"]]),
      col_name_mod = TRUE
    )
  # Merge the entries to a new updated dictionary
  column_name_dict_updated <- column_name_dict_keep |>
    dplyr::bind_rows(
      column_name_dict_change
    ) |>
    dplyr::arrange(
      match(
        x = .data[["col_key"]],
        table = column_name_dict$col_key
      )
    )

  # return ----

  return(column_name_dict_updated)

}

#' Help function checking whether df contains invalid Olink identifiers
#'
#' @description
#' This function checks if Olink identifiers (\var{OlinkID}) match the pattern
#' of a prefix "OID" followed by 5 integer numbers.
#'
#' @author
#'  Masoumeh Sheikhi
#'
#' @inheritParams check_npx
#' @param col_names A list of matched column names. This is the output of the
#' \var{check_npx_col_names} function.
#'
#' @return A character vector with invalid \var{OlinkID}.
#'
check_npx_olinkid <- function(df,
                              col_names) {

  # identify Olink identifiers ----

  # extract invalid Olink IDs
  invalid_oid <- df |>
    dplyr::distinct(
      .data[[col_names$olink_id]]
    )  |>
    dplyr::filter(
      !grepl(
        pattern = "^OID\\d{5}$",
        x = .data[[col_names$olink_id]]
      )
    )  |>
    dplyr::collect() |>
    dplyr::pull(
      .data[[col_names$olink_id]]
    )

  # warning if there are invalid Olink identifiers ----

  # warning if there is any invalid Olink ID
  if (length(invalid_oid) > 0L) {
    cli::cli_warn(c(
      "Unrecognized OlinkID{?s} detected: {.val {invalid_oid}}"
    ))
  }

  # return ----

  return(invalid_oid)
}

#' Help function to identify Olink assays with all quantified values \emph{NA}
#'
#' @description
#' This function checks if there are assays with the quantified values for all
#' samples \emph{NA}.
#'
#' @details
#' We have added the tags importFrom for "dbplyr" and "duckdb" because
#' "devtools::check()" would complain with a note that the two libraries are
#' imported but never used. To avoid that we used solutions taken from here:
#' 1. https://github.com/hadley/r-pkgs/issues/203
#' 2. https://github.com/pbs-software/pbs-modelling/issues/95
#'
#' @author
#'  Simon Forsberg
#'  Masoumeh Sheikhi
#'
#' @inheritParams check_npx
#' @inheritParams check_npx_olinkid
#'
#' @return A character vector containing \var{OlinkID} of assays with quantified
#' values \emph{NA} for all samples, otherwise returns \emph{character(0)}.
#'
#' @importFrom duckdb duckdb
#' @importFrom dbplyr memdb_frame
#'
check_npx_all_na_assays <- function(df, col_names) {

  # Identify assays with only NAs
  all_nas <- df |>
    dplyr::select(
      dplyr::all_of(
        c(col_names$olink_id,
          col_names$quant)
      )
    ) |>
    dplyr::group_by(
      .data[[col_names$olink_id]]
    ) |>
    dplyr::mutate(
      is_na = dplyr::if_else(is.na(.data[[col_names$quant]]), 1L, 0L)
    ) |>
    arrow::to_duckdb() |>
    dplyr::summarise(
      n = dplyr::n(),
      n_na = sum(.data[["is_na"]], na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::filter(
      .data[["n"]] == .data[["n_na"]]
    ) |>
    dplyr::collect() |>
    dplyr::pull(
      .data[[col_names$olink_id]]
    ) |>
    sort()

  # Issue warning if any assays with only NAs are found
  if (length(all_nas) > 0L) {
    cli::cli_warn(c(
      "{.val {all_nas}} ha{?s/ve} {.val {col_names$quant}} = NA for all
      samples."
    ))
  }

  return(all_nas)
}

#' Help function checking for duplicate sample identifiers in data.
#'
#' @description
#' This function checks if there are duplicate sample identifiers for any assay.
#'
#' @author
#'  Masoumeh Sheikhi
#'
#' @inheritParams check_npx
#' @inheritParams check_npx_olinkid
#'
#' @return A character vector of duplicate \var{SampleID} found in the data.
#'
check_npx_duplicate_sample_ids <- function(df, col_names) {

  # Select relevant columns
  sample_summary <- df  |>
    dplyr::select(dplyr::all_of(c(
      col_names$sample_id,
      col_names$olink_id
    ))) |>
    dplyr::group_by(
      .data[[col_names$sample_id]],
      .data[[col_names$olink_id]]
    ) |>
    dplyr::summarise(freq = dplyr::n(),
                     .groups = "drop") |>
    dplyr::collect()

  # Find duplicates
  duplicates <- character(0L)
  duplicates <- sample_summary |>
    dplyr::filter(.data[["freq"]] > 1) |>
    dplyr::collect() |>
    dplyr::pull(.data[[col_names$sample_id]]) |>
    unique()

  # Warn if duplicates are found
  if (length(duplicates) > 0L) {
    cli::cli_warn(c(
      "Duplicate SampleID{?s} detected: {.val {duplicates}}"
    ))
  }

  return(duplicates)
}

#' Help function to identify Olink samples with all quantified values \emph{NA}
#'
#' @description
#' This function checks if there are samples with the quantified values for all
#' assays \emph{NA}.
#'
#' @details
#' We have added the tags importFrom for "dbplyr" and "duckdb" because
#' "devtools::check()" would complain with a note that the two libraries are
#' imported but never used. To avoid that we used solutions taken from here:
#' 1. https://github.com/hadley/r-pkgs/issues/203
#' 2. https://github.com/pbs-software/pbs-modelling/issues/95
#'
#' @author
#'  Simon Forsberg
#'  Masoumeh Sheikhi
#'  Klev Diamanti
#'
#' @inheritParams check_npx
#' @inheritParams check_npx_olinkid
#'
#' @return A character vector containing \var{SampleID} of samples with
#' quantified values \emph{NA} for all assays, otherwise returns
#' \emph{character(0)}.
#'
#' @importFrom duckdb duckdb
#' @importFrom dbplyr memdb_frame
#'
check_npx_all_na_sample <- function(df, col_names) {

  # Identify assays with only NAs
  all_na_sample <- df |>
    dplyr::select(
      dplyr::all_of(
        c(col_names$sample_id,
          col_names$quant)
      )
    ) |>
    dplyr::group_by(
      .data[[col_names$sample_id]]
    ) |>
    dplyr::mutate(
      is_na = dplyr::if_else(is.na(.data[[col_names$quant]]), 1L, 0L)
    ) |>
    arrow::to_duckdb() |>
    dplyr::summarise(
      n = dplyr::n(),
      n_na = sum(.data[["is_na"]], na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::filter(
      .data[["n"]] == .data[["n_na"]]
    ) |>
    dplyr::collect() |>
    dplyr::pull(
      .data[[col_names$sample_id]]
    ) |>
    sort()

  # Issue warning if any assays with only NAs are found
  if (length(all_na_sample) > 0L) {
    cli::cli_warn(c(
      "{.val {all_na_sample}} ha{?s/ve} {.val {col_names$quant}} = NA for all
      assays."
    ))
  }

  return(all_na_sample)
}

#' Help function checking types of columns in data.
#'
#' @description
#' This function checks if certain columns from \var{df} have the correct type
#' to enable downstream analysis. Columns to be checked are marked as such in
#' the columns \var{col_class} and \var{col_class_check} of
#' \var{column_name_dict}.
#'
#' @author
#'  Klev Diamanti
#'
#' @inheritParams check_npx
#' @inheritParams check_npx_olinkid
#'
#' @returns A data frame with the columns \var{col_name}, \var{col_key},
#' \var{col_class} and \var{expected_col_class} marking columns with the
#' incorrect type.
#'
check_npx_col_class <- function(df, col_names) {

  # we first convert 'col_names' into a data frame with 'col_key' the names of
  # 'col_names', and 'col_df' the elements of the list of 'col_names'.
  # Basically, 'col_key' is used to match to 'column_name_dict', and 'col_df' is
  # used to match to the actual column names of the dataset 'df'.
  df_col_names <- dplyr::tibble(
    col_key = names(col_names),
    col_df = unname(col_names)
  ) |>
    tidyr::unnest(
      cols = dplyr::all_of(
        c("col_key", "col_df")
      )
    )

  # we select only the columns for which we want to confirm they column class.
  # Then we select only 'col_key' and 'col_class', which we rename to
  # 'expected_col_class'. 'col_key' is used to match to 'df_col_names' to allow
  # checking only the column names of the dataset 'df' in question.
  # 'expected_col_class' is used to retain the expected column classes. The
  # goal is that this data frame will contain 'col_class' that will allow us to
  # check if it matches the dataset, and 'col_df' that will match the column
  # name of the dataset.
  col_keys_check <- column_name_dict |>
    dplyr::filter(
      .data[["col_class_check"]] == TRUE
    ) |>
    dplyr::select(
      dplyr::all_of(
        c("col_key", "expected_col_class" = "col_class")
      )
    ) |>
    dplyr::inner_join(
      df_col_names,
      by = "col_key",
      relationship = "one-to-many"
    )

  # Check column class for each column in dataset 'df' using the internal
  # functions 'check_is_numeric' and 'check_is_character'.
  col_class_numeric <- df |>
    dplyr::slice_head(
      n = 100L
    ) |>
    dplyr::collect() |>
    lapply(
      check_is_numeric,
      error = FALSE
    ) |>
    as.matrix()
  col_class_character <- df |>
    dplyr::slice_head(
      n = 100L
    ) |>
    dplyr::collect() |>
    lapply(
      check_is_character,
      error = FALSE
    ) |>
    as.matrix()

  # combine the checks from numeric and character from above before, and
  # ultimately, check if expected column class matches the column class found in
  # the dataset 'df'.
  df_col_class <- dplyr::tibble(
    col_name = rownames(col_class_numeric),
    is_numeric = col_class_numeric[, 1L]
  ) |>
    dplyr::left_join(
      dplyr::tibble(
        col_name = rownames(col_class_character),
        is_character = col_class_character[, 1L]
      ),
      by = "col_name",
      relationship = "one-to-one"
    ) |>
    dplyr::mutate(
      col_class = dplyr::case_when(
        .data[["is_numeric"]] == TRUE ~ "numeric",
        .data[["is_character"]] == TRUE ~ "character",
        .data[["is_numeric"]] == TRUE
        & .data[["is_character"]] == TRUE ~ "unknown",
        TRUE ~ "other",
        .default = "unknown"
      )
    ) |>
    dplyr::select(
      -dplyr::all_of(
        c("is_numeric", "is_character")
      )
    ) |>
    dplyr::inner_join(
      col_keys_check,
      by = c("col_name" = "col_df"),
      relationship = "one-to-one"
    ) |>
    dplyr::filter(
      .data[["col_class"]] != .data[["expected_col_class"]]
    )

  if (nrow(df_col_class) > 0L) {

    col_class_msg <- paste0("* \"", df_col_class$col_name, "\"",
                            ": Expected \"", df_col_class$expected_col_class,
                            "\". Detected \"", df_col_class$col_class, "\".")

    cli::cli_warn(
      c(
        "{cli::qty(col_class_msg)} Detected column{?s} with incorrect data
        type{?s}:",
        col_class_msg,
        "i" = "{cli::qty(col_class_msg)} Use the function {.fn clean_npx} or
        manually convert the column{?s} to the expected type prior to downstream
        analyses."
      )
    )

  }

  return(df_col_class)

}

#' Help function checking data for assay QC warnings.
#'
#' @author
#'  Klev Diamanti
#'
#' @inheritParams check_npx
#' @inheritParams check_npx_olinkid
#'
#' @returns A character vector containing \var{OlinkID} of assays with at least
#' one QC warning, otherwise a \emph{character(0)}.
#'
check_npx_qcwarn_assays <- function(df, col_names) {

  if ("assay_warn" %in% names(col_names)) {

    qc_warn_assays <- df |>
      dplyr::select(
        dplyr::all_of(
          c(col_names$olink_id, col_names$assay_warn)
        )
      ) |>
      dplyr::filter(
        grepl(
          pattern = "warn",
          x = .data[[col_names$assay_warn]],
          ignore.case = TRUE
        )
      ) |>
      dplyr::distinct(
        .data[[col_names$olink_id]]
      ) |>
      dplyr::collect() |>
      dplyr::pull(
        .data[[col_names$olink_id]]
      ) |>
      unique() |>
      sort()

    if (length(qc_warn_assays) > 0L) {
      cli::cli_inform(
        c("{.val {length(qc_warn_assays)}} assay{?s} exhibited assay QC warnings
        in column {.arg {unname(col_names$assay_warn)}} of the dataset:
          {.val {qc_warn_assays}}.")
      )
    }

  } else {

    qc_warn_assays <- character(0L)

  }

  return(qc_warn_assays)
}
