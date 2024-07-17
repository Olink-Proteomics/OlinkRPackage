#' Help function checking if a variable is a character vector.
#'
#' @author
#'   Klev Diamanti
#'
#' @param string String variable to check.
#' @param error Return error or a boolean (default = FALSE).
#'
#' @return Boolean if the variable is a character vector or not, and an error if
#' "error = TRUE".
#'
#' @seealso
#'   \code{\link{check_is_boolean}}
#'   \code{\link{check_is_integer}}
#'   \code{\link{check_is_numeric}}
#'
check_is_character <- function(string,
                               error = FALSE) {

  # check if input error is boolean vector of length 1
  check_is_scalar_boolean(bool = error,
                          error = TRUE)

  # check that the input is a numeric vector
  if (!rlang::is_character(string)
      || any(rlang::are_na(string))) {

    if (error == TRUE) {

      cli::cli_abort(
        c(
          "x" = "{.arg {rlang::caller_arg(string)}} must be a character vector!"
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

#' Help function checking if a variable is a character vector of length 1.
#'
#' @author
#'   Klev Diamanti
#'
#' @param string String variable to check.
#' @param error Return error or a boolean (default = FALSE).
#'
#' @return Boolean if the variable is a character vector of length 1 or not, and
#' an error if "error = TRUE".
#'
#' @seealso
#'   \code{\link{check_is_scalar_boolean}}
#'   \code{\link{check_is_scalar_integer}}
#'   \code{\link{check_is_scalar_numeric}}
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
