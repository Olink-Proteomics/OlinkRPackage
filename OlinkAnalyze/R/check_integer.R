#' Help function checking if a variable is a vector of integers.
#'
#' @author
#'   Klev Diamanti
#'
#' @param int Variable to check.
#' @param error Scalar boolean to return an error instead of a `FALSE`
#' (`default = FALSE`).
#'
#' @return `TRUE` if the variable is an integer vector, and `FALSE` if not;
#' error if the variable is not an integer vector and `error = TRUE`.
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

  # check if input is an integer vector
  if (!rlang::is_integer(int)
      || any(rlang::are_na(int))) {

    if (error == TRUE) {

      cli::cli_abort( # nolint return_linter
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

#' Help function checking if a variable is a scalar integer.
#'
#' @author
#'   Klev Diamanti
#'
#' @param int Variable to check.
#' @param error Scalar boolean to return an error instead of a `FALSE`
#' (`default = FALSE`).
#'
#' @return `TRUE` if the variable is an integer vector of length 1, and `FALSE`
#' if not; error if the variable is not an integer vector of length 1, and
#' `error = TRUE`.
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

  # check if input is an integer vector of length 1
  if (!rlang::is_scalar_integer(int)
      || rlang::is_na(int)) {

    if (error == TRUE) {

      cli::cli_abort( # nolint return_linter
        c(
          "x" = "{.arg {rlang::caller_arg(int)}} must be a scalar integer!"
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
