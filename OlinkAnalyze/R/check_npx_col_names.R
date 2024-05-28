#' Check, update and define column names used in downstream analyses.
#'
#' @description
#' OlinkAnalyze uses pre-defined names of columns of data frames to perform
#' downstream analyses. At the same time, different Olink platforms are capable
#' of exporting data with different quantification approaches. This function
#' aims to instruct each function of OlinkAnalyze on the column it should be
#' using for the downstream analysis. This should be seamless for data exported
#' from Olink Software and imported to R using the read_npx function.
#'
#' However, in certain cases the columns of interest might be named differently.
#' This function allows assigning custom-named columns of a data frame to
#' internally expected variables that will in turn instruct Olink Analyze
#' functions to use them for downstream analysis. For example, if one has
#' transformed the existing NPX value and has stored the result in a new column
#' called NPX_2, then they can assign this new name to the internal variable
#' `quant` to inform the package that in the downstream analysis `NPX_2` should
#' be used. See example 1.
#'
#' Similarly, in case of multiple matches (e.g. the data frame contains both
#' columns `NPX` and `Quantified_value`) the ties will need to be resolved by
#' the user using the argument `preferred_names` from this function.  See
#' example 2.
#'
#' The argument `preferred_names` is a named character vector with internal
#' column names as names and column names of the current data set as values.
#' Names of the input vector can be one or more of the following: `sample_id`,
#' `sample_type`, `olink_id`, `plate_id`, `qc_warning`, `lod`, `quant`.
#'
#' @author
#'  Klev Diamanti
#'  Masoumeh Sheikhi
#'
#' @param df A tibble or and arrow object from `read_npx`.
#' @param preferred_names A named character vector where names are internal
#' column names and values are column names to be selected from current data
#' frame. Read the `description` for further information.
#'
#' @return A list of matched column names based on the preferred names.
#'
#' @examples
#' \donttest{
#' file <- system.file("extdata",
#'                     "npx_data_ext.parquet",
#'                      package = "OlinkAnalyze")
#' df <- read_npx(filename = file)
#'
#' # run df as is
#' OlinkAnalyze:::check_npx_col_names(df = df)
#'
#' # SampleType missing
#' df |>
#'   dplyr::select(
#'     -dplyr::all_of("SampleType")
#'   ) |>
#'   OlinkAnalyze:::check_npx_col_names()
#'
#' # use PCNormalizedNPX instead on NPX
#' OlinkAnalyze:::check_npx_col_names(
#'   df = df,
#'   preferred_names = c("quant" = "PCNormalizedNPX")
#' )
#'
#' # use PCNormalizedNPX instead on NPX, and PlateLOD instead of LOD
#' df |>
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
        call = rlang::caller_env()
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
      call = rlang::caller_env()
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
      call = rlang::caller_env()
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
#' enable analysis of Olink data. For example, if one has transformed the
#' existing NPX value and has stored the result in a new column called NPX_2,
#' then they can assign this new name to the internal variable `quant` to inform
#' the package that in the downstream analysis `NPX_2` should be used.
#'
#' This function takes as input a named character vector with internal column
#' names as names and column names of the current data set as values. Names of
#' the input vector can be one or more of the following: `sample_id`,
#' `sample_type`, `olink_id`, `plate_id`, `qc_warning`, `lod`, `quant`.
#'
#' @author
#'  Klev Diamanti
#'  Masoumeh Sheikhi
#'
#' @param preferred_names A named character vector where names are internal
#' column names and values are column names to be selected from current data
#' frame. Read the `description` for further information.
#'
#' @return `column_name_dict` updated based on `preferred_names`.
#'
check_npx_update_col_names <- function(preferred_names) {

  # Check if preferred_names is character
  check_is_character(preferred_names, error = TRUE)

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
      call = rlang::caller_env()
    )

  }

  # Do not update entries that are not specified in `preferred_names`
  column_name_dict_keep <- column_name_dict[setdiff(x = names(column_name_dict),
                                                    y = names(preferred_names))]
  # Update entries that are specified in `preferred_names`
  column_name_dict_change <- as.list(preferred_names)
  # Merge the entries to a new updated dictionary
  column_name_dict_updated <- append(column_name_dict_keep,
                                     column_name_dict_change)

  return(column_name_dict_updated)
}
