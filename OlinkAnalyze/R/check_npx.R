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
#' `r cli::ansi_collapse(x = names(column_name_dict))`
#'
#' @author
#'   Masoumeh Sheikhi
#'
#' @param df A tibble or and arrow object from \code{\link{read_npx}}.
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
check_npx <- function(df = df,
                      preferred_names = NULL) {

  # check input ----

  check_is_arrow_or_tibble(df = df,
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
#' `r cli::ansi_collapse(x = names(column_name_dict))`
#'
#' @author
#'  Klev Diamanti;
#'  Masoumeh Sheikhi
#'
#' @param df A tibble or and arrow object from \code{\link{read_npx}}.
#' @param preferred_names A named character vector where names are internal
#' column names and values are column names to be selected from the input data
#' frame. Read the \emph{description} for further information.
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

  # check input ----

  check_is_arrow_or_tibble(df = df,
                           error = TRUE)

  # if not NULL, preferred_names is checked in check_npx_update_col_names
  if (!is.null(preferred_names)) {

    column_name_dict_updated <- check_npx_update_col_names(
      preferred_names = preferred_names
    )

  } else {

    column_name_dict_updated <- column_name_dict

  }

  # matches names to column names ----

  column_name_df <- lapply(
    seq_along(column_name_dict_updated),
    function(x) {
      # get collection of alternative names
      col_alt_names <- column_name_dict_updated[[x]]
      # identify names present in the column names of the data frame
      col_names <- col_alt_names[col_alt_names %in% names(df)]
      # return NA ONLY if the entry in the list contains some NA element.
      # NA elements in column_name_dict signify that the column can be missing
      if (length(col_names) == 0L && any(is.na(col_alt_names))) {
        return(NA_character_)
      } else {
        return(col_names)
      }
    }
  )
  names(column_name_df) <- names(column_name_dict_updated)

  # check correctness ----

  # is user's input correct?
  # check if the user input has no matches to the column names of the data frame
  if (!is.null(preferred_names)) {
    # pick user-defined column names
    col_name_user <- column_name_df[names(preferred_names)]

    if (any(sapply(col_name_user, length) == 0L) || any(is.na(col_name_user))) {
      cli::cli_abort(
        c("x" = "Some of the values of {.val preferred_names} are not detected
          in the column names of the input data set {.val df}."),
        call = rlang::caller_env(),
        wrap = FALSE
      )

    }
  }

  # check if multiple columns from the data frame match the same key from
  # column_name_dict
  column_name_multi <- sapply(column_name_df, length)

  if (any(column_name_multi > 1L)) {

    column_name_multi_lst <- column_name_df[column_name_multi > 1L]
    column_name_multi_prnt <- lapply( #nolint
      seq_along(column_name_multi_lst),
      function(i) {
        paste0("* \"", names(column_name_multi_lst[i]), "\": ",
               cli::ansi_collapse(x = unlist(column_name_multi_lst[i])))
      }
    ) |>
      unlist()

    cli::cli_abort(
      c("x" = "There are multiple column names associated with the following
        key(s):",
        column_name_multi_prnt,
        "i" = "Please use {.val preferred_names} to break ties of column
        names."),
      call = rlang::caller_env(),
      wrap = FALSE
    )
  }

  # check if no columns from the data frame match the same key from
  # column_name_dict
  if (any(column_name_multi == 0L)) {

    cli::cli_abort(
      c("x" = "There are no column names associated with the following key(s):",
        paste0("* \"", names(column_name_df[column_name_multi == 0L]), "\""),
        "i" = "Please use {.val preferred_names} to select the correct column
      names."),
      call = rlang::caller_env(),
      wrap = FALSE
    )
  }

  # return ----

  # remove any nullable columns
  column_name_df <- column_name_df[!is.na(column_name_df)]

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
#' `r cli::ansi_collapse(x = names(column_name_dict))`
#'
#' @author
#'  Klev Diamanti
#'  Masoumeh Sheikhi
#'
#' @param preferred_names A named character vector where names are internal
#' column names and values are column names to be selected from the input data
#' frame. Read the \emph{description} for further information.
#'
#' @return \var{column_name_dict} updated based on \var{preferred_names}.
#'
check_npx_update_col_names <- function(preferred_names) {

  # check input ----

  # Check if preferred_names is character
  check_is_character(string = preferred_names,
                     error = TRUE)

  # check for names not matching expected ----

  # Check valid names
  if (!all(names(preferred_names) %in% names(column_name_dict))) {

    # identify names of the vector preferred_names that do not match names from
    # column_name_dict. Names should match to be able to update the field.
    missing_names <- names(preferred_names)[!(names(preferred_names) %in% # nolint
                                                names(column_name_dict))]

    cli::cli_abort(
      c("x" = "Unexpected name{?s} in {.val preferred_names}:
        {.val {missing_names}}!",
        "i" = "Expected one or more of the following names:
        {names(column_name_dict)}"),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

  # update column names ----

  # Do not update entries that are not specified in `preferred_names`
  column_name_dict_keep <- column_name_dict[setdiff(x = names(column_name_dict),
                                                    y = names(preferred_names))]
  # Update entries that are specified in `preferred_names`
  column_name_dict_change <- as.list(preferred_names)
  # Merge the entries to a new updated dictionary
  column_name_dict_updated <- append(column_name_dict_keep,
                                     column_name_dict_change)

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
#' @param df A tibble or an arrow object containing containing column
#' \var{OlinkID}.
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
      stringr::str_detect(string = .data[[col_names$olink_id]],
                          pattern = "^OID[0-9]{5}$",
                          negate = TRUE)
    )  |>
    dplyr::collect() |>
    dplyr::pull(
      .data[[col_names$olink_id]]
    )

  # warning if there are invalid Olink identifiers ----

  # warning if there is any invalid Olink ID
  if (length(invalid_oid) > 0L) {
    cli::cli_warn(c(
      "Unrecognized OlinkID{?s} detected: {invalid_oid}"
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
#' @author
#'  Simon Forsberg;
#'  Masoumeh Sheikhi
#'
#' @param df A tibble or an arrow object containing columns \var{OlinkID} and
#' the quantification column `r accepted_olink_platforms$quant_method |> unlist() |> unique() |> sort() |> cli::ansi_collapse(sep = ", ", last = " or ")` # nolint
#' @param col_names A list of matched column names. This is the output of the
#' \var{check_npx_col_names} function.
#'
#' @return A character vector containing \var{OlinkID} of assays with quantified
#' values \emph{NA} for all samples, otherwise returns \emph{character(0)}.
#'
check_npx_all_na_assays <- function(df, col_names) {

  # check if duckdb and dbplyr are installed
  check_library_installed(libraries = c("duckdb", "dbplyr"),
                          error = TRUE)
  # they are needed when we use arrow::to_duckdb()

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
    )

  # Issue warning if any assays with only NAs are found
  if (length(all_nas) > 0L) {
    cli::cli_warn(c(
      "{all_nas} ha{?s/ve} {col_names$quant} = NA for all samples."
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
#' @param df A tibble or an arrow object containing the columns \var{SampleID}
#' and \var{OlinkID}.
#' @param col_names A list of matched column names. This is the output of the
#' \var{check_npx_col_names} function.
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
      "Duplicate SampleID{?s} detected: {duplicates}"
    ))
  }

  return(duplicates)
}
