#' Help function checking if a variable is a boolean vector.
#'
#' @author
#'   Klev Diamanti
#'
#' @param bool Boolean variable to check.
#' @param error Return error or a boolean (default).
#'
#' @return Boolean if the variable is a boolean vector or not, and an error if
#' "error = TRUE".
#'
#' @seealso
#'   \code{\link{check_is_character}}
#'   \code{\link{check_is_integer}}
#'   \code{\link{check_is_numeric}}
#'
check_is_boolean <- function(bool,
                             error = FALSE) {

  # check that the input is a boolean vector
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

#' Help function checking if a variable is a boolean vector of length 1.
#'
#' @author
#'   Klev Diamanti
#'
#' @param bool Boolean variable to check.
#' @param error Return error or a boolean (default).
#'
#' @return Boolean if the variable is a boolean vector of length 1 or not, and
#' an error if "error = TRUE".
#'
#' @seealso
#'   \code{\link{check_is_scalar_character}}
#'   \code{\link{check_is_scalar_integer}}
#'   \code{\link{check_is_scalar_numeric}}
#'
check_is_scalar_boolean <- function(bool,
                                    error = FALSE) {

  # check that the input is a character vector of length 1
  if (!rlang::is_scalar_logical(bool)
      || rlang::is_na(bool)) {

    if (error == TRUE) {

      cli::cli_abort(
        c(
          "x" = "{.arg {rlang::caller_arg(bool)}} must be a boolean!"
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
