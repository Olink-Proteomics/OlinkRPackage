#' Help function checking if a variable is a vector of booleans.
#'
#' @author
#'   Klev Diamanti
#'
#' @param bool Variable to check.
#' @param error Scalar boolean to return an error instead of a `FALSE`
#' (`default = FALSE`).
#'
#' @return `TRUE` if the variable is a boolean vector, and `FALSE` if not; error
#' if the variable is not a boolean vector, and `error = TRUE`.
#'
#' @seealso
#'   \code{\link{check_is_character}}
#'   \code{\link{check_is_integer}}
#'   \code{\link{check_is_numeric}}
#'
check_is_boolean <- function(bool,
                             error = FALSE) {

  # check if input is a boolean vector
  if (!rlang::is_logical(bool)
      || any(rlang::are_na(bool))) {

    if (error == TRUE) {

      cli::cli_abort(
        c(
          "x" = "{.arg {rlang::caller_arg(bool)}} must be a boolean vector!"
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

#' Help function checking if a variable is a scalar boolean.
#'
#' @author
#'   Klev Diamanti
#'
#' @param bool Variable to check.
#' @param error Scalar boolean to return an error instead of a `FALSE`
#' (`default = FALSE`).
#'
#' @return `TRUE` if the variable is a boolean vector of length 1, and `FALSE`
#' if not; error if the variable is not a boolean vector of length 1, and
#' `error = TRUE`.
#'
#' @seealso
#'   \code{\link{check_is_scalar_character}}
#'   \code{\link{check_is_scalar_integer}}
#'   \code{\link{check_is_scalar_numeric}}
#'
check_is_scalar_boolean <- function(bool,
                                    error = FALSE) {

  # check if input is a boolean vector of length 1
  if (!rlang::is_scalar_logical(bool)
      || rlang::is_na(bool)) {

    if (error == TRUE) {

      cli::cli_abort(
        c(
          "x" = "{.arg {rlang::caller_arg(bool)}} must be a scalar boolean!"
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
