#' Help function checking if a variable is a character vector of length 1.
#'
#' @author Klev Diamanti
#'
#' @param string Input to check.
#' @param error Boolean to return error or a boolean (default).
#'
#' @return Boolean if the variable is a character vector of length 1 or not, and
#' an error if `error = TRUE`.
#'
#' @seealso
#'   [check_is_scalar_boolean()]
#'   [check_is_scalar_integer()]
#'   [check_is_scalar_numeric()]
#'
check_is_scalar_character <- function(string,
                                      error = FALSE) {

  # check if input error is boolean vector of length 1
  check_is_scalar_boolean(bool = error,
                          error = TRUE)

  # check that the input is a character vector of length 1
  if (!rlang::is_scalar_character(string)
      || rlang::is_na(string)) {

    if (error == TRUE) {

      cli::cli_abort(
        c(
          "x" = "{.arg {rlang::caller_arg(string)}} must be a string!"
        ),
        call = rlang::caller_env(),
        wrap = FALSE
      )

    } else {

      return(FALSE)

    }

  } else {

    return(TRUE)

  }

}
