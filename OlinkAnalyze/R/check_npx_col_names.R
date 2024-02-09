# Help function that returns column name matches from `column_name_dict`

#' @author
#'

#' @description
#' A short description...
#'
#' @param
#'
#' @return


check_npx_col_names <- function(df, preferred_names = NULL) {
  check_is_arrow_object(df, error = TRUE)

  if (!is.null(preferred_names)) {
    column_name_dict_updated <- check_npx_update_col_names(
      preferred_names = preferred_names
      )
  } else {
    column_name_dict_updated <- column_name_dict
  }

  # Find matches
  column_name_df <- lapply(
    seq_along(column_name_dict_updated),
    function(x) {
      col_alt_names <-  column_name_dict_updated[[x]]
      col_names <- col_alt_names[col_alt_names %in% names(df)]
      if (length(col_names) == 0L && any(is.na(col_alt_names))) {
        return(NA_character_)
      } else {
        return(col_names)
      }
    })

  names(column_name_df) <- names(column_name_dict_updated)

  # is user's input correct?
  if (!is.null(preferred_names)) {
    col_name_user <- column_name_df[names(preferred_names)]
    if (any(sapply(col_name_user, length) == 0L) || any(is.na(col_name_user))) {
      cli::cli_abort(
        c(
          "x" = "Some of the entries of `preferred_names`
          are not detected in the column names of {names(df)}"
        ),
        call = rlang::caller_env())
    }
  }

  # check multiple matches
  column_name_multi <- sapply(column_name_df, length)

  if (any(column_name_multi > 1L)) {
    cli::cli_abort(
      c(
        "x" = "There are multiple column names associated with:
        {names(column_name_df[column_name_multi > 1L])}.",
        "i" = "Please use `preferred_names` to select unique column names."
      ),
      call = rlang::caller_env())
  }

  # check no matches
  if (any(column_name_multi == 0L)) {
    cli::cli_abort(
      c(
        "x" = "There are no column names associated with:
        {names(column_name_df[column_name_multi == 0L])}.",
        "i" = "Please use `preferred_names` to set column names."
      ),
      call = rlang::caller_env())
  }


  column_name_df <- column_name_df[!is.na(column_name_df)]


  return(column_name_df)

}



# help function here to check that preferred name list only contains valid strings

check_npx_update_col_names <- function(preferred_names) {
  # check is char
  check_is_character(preferred_names, error = TRUE)
  # check valid names
  if (!all(names(preferred_names) %in% names(column_name_dict))) {
    missing_names <- names(preferred_names)[!(names(preferred_names) %in%
                                                names(column_name_dict))]
    cli::cli_abort(
      c(
        "x" = "Unexpected name{?s} in `preferred_names`: {missing_names}",
        "i" = "Expected names: {names(column_name_dict)}"),
      call = rlang::caller_env())
  }

  # update the dict names
  column_name_dict_keep <- column_name_dict[setdiff(names(column_name_dict),
                                                    names(preferred_names))]
  column_name_dict_change <- as.list(preferred_names)

  column_name_dict_updated <- append(column_name_dict_keep,
                                     column_name_dict_change)

  return(column_name_dict_updated)

}
