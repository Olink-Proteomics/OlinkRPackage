# Help function that matches column names to column_name_dict

#' @author
#'

#' @description
#' A short description...
#'
#' @param
#' @return

check_column_names <- function(preferred_columns = NULL) {
  # check arg is list
  if (!is.null(preferred_columns)) {
    check_is_list(preferred_columns, error = TRUE) ## error true?
  }
  #
}
