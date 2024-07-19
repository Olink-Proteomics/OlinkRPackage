#' Help function checking if a variable is an integer vector.
#'
#' @author
#'   Klev Diamanti
#'
#' @param int Integer to check.
#' @param error Return error or a boolean (default = FALSE).
#'
#' @return Boolean if the variable is an integer vector or not, and an error if
#' "error = TRUE".
#'
#' @seealso
#'   \code{\link{check_is_boolean}}
#'   \code{\link{check_is_character}}
#'   \code{\link{check_is_numeric}}
#'
check_is_integer <- function(int,
                             error = FALSE) {

  # check if input error is boolean vector of length 1
  check_is_scalar_boolean(bool = error,
                          error = TRUE)

  # check that the input is an integer vector
  if (!rlang::is_integer(int)
      || any(rlang::are_na(int))) {

    if (error == TRUE) {

      cli::cli_abort(
        c(
          "x" = "{.arg {rlang::caller_arg(int)}} must be an integer vector!"
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

#' Help function checking if a variable is an integer vector of length 1.
#'
#' @author
#'   Klev Diamanti
#'
#' @param int Integer vector to check.
#' @param error Return error or a boolean (default = FALSE).
#'
#' @return Boolean if the variable is an integer vector of length 1 or not, and
#' an error if "error = TRUE".
#'
#' @seealso
#'   \code{\link{check_is_scalar_boolean}}
#'   \code{\link{check_is_scalar_character}}
#'   \code{\link{check_is_scalar_numeric}}
#'
check_is_scalar_integer <- function(int,
                                    error = FALSE) {

  # check if input error is boolean vector of length 1
  check_is_scalar_boolean(bool = error,
                          error = TRUE)

  # check that the input is a character vector of length 1
  if (!rlang::is_scalar_integer(int)
      || rlang::is_na(int)) {

    if (error == TRUE) {

      cli::cli_abort(
        c(
          "x" = "{.arg {rlang::caller_arg(int)}} must be an integer!"
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
