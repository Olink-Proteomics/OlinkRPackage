#' Help function to check if the variable is a data frame.
#'
#' @author Klev Diamanti
#'
#' @param df Data frame to check.
#' @param error Boolean to return error or a boolean (default).
#'
#' @return Boolean if the object is a data frame or not, and an error if
#' `error = TRUE`.
#'
#' @seealso
#'   [check_is_tibble()]
#'   [check_is_arrow_object()]
#'
check_is_data_frame <- function(df,
                                error = FALSE) {

  # check if input error is boolean vector of length 1
  check_is_scalar_boolean(bool = error,
                          error = TRUE)

  if (!is.data.frame(x = df)) {

    if (error == TRUE) {

      # error if df is not a tibble
      cli::cli_abort(
        c(
          "x" = "{.arg {rlang::caller_arg(df)}} is not a data frame!"
        ),
        call = rlang::caller_env(),
        wrap = TRUE
      )

    } else {

      return(FALSE)

    }

  } else {

    return(TRUE)

  }

}
