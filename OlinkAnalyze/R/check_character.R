#' Help function checking if a variable is a vector of characters.
#'
#' @author
#'   Klev Diamanti
#'
#' @param string Variable to check.
#' @param error Scalar boolean to return an error instead of a `FALSE`
#' (`default = FALSE`).
#'
#' @return `TRUE` if the variable is a character vector, and `FALSE` if not;
#' error if the variable is not a character vector and `error = TRUE`.
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

  # check if input is a character vector
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

#' Help function checking if a variable is a scalar character.
#'
#' @author
#'   Klev Diamanti
#'
#' @param string Variable to check.
#' @param error Scalar boolean to return an error instead of a `FALSE`
#' (`default = FALSE`).
#'
#' @return `TRUE` if the variable is a character vector of length 1, and `FALSE`
#' if not; error if the variable is not a character vector of length 1, and
#' `error = TRUE`.
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

  # check if input is a character vector of length 1
  if (!rlang::is_scalar_character(string)
      || rlang::is_na(string)) {

    if (error == TRUE) {

      cli::cli_abort(
        c(
          "x" = "{.arg {rlang::caller_arg(string)}} must be a scalar character!"
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
