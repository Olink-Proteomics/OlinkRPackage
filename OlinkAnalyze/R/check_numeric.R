#' Help function checking if a variable is a vector of numerics.
#'
#' @author
#'   Klev Diamanti
#'
#' @param num Variable to check.
#' @param error Scalar boolean to return an error instead of a `FALSE`
#' (`default = FALSE`).
#'
#' @return `TRUE` if the variable is a numeric vector, and `FALSE` if not;
#' error if the variable is not a numeric vector and `error = TRUE`.
#'
#' @seealso
#'   \code{\link{check_is_boolean}}
#'   \code{\link{check_is_character}}
#'   \code{\link{check_is_integer}}
#'
check_is_numeric <- function(num,
                             error = FALSE) {

  # check if input error is boolean vector of length 1
  check_is_scalar_boolean(bool = error,
                          error = TRUE)

  # check if input is a numeric vector
  if ((!rlang::is_integer(num) && !is.numeric(num))
      || any(rlang::are_na(num))) {

    if (error == TRUE) {

      cli::cli_abort( # nolint return_linter
        c(
          "x" = "{.arg {rlang::caller_arg(num)}} must be a numeric vector!"
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

#' Help function checking if a variable is a scalar numeric
#'
#' @author
#'   Klev Diamanti
#'
#' @param num Variable to check.
#' @param error Scalar boolean to return an error instead of a `FALSE`
#' (`default = FALSE`).
#'
#' @return `TRUE` if the variable is a numeric vector of length 1, and `FALSE`
#' if not; error if the variable is not a numeric vector of length 1, and
#' `error = TRUE`.
#'
#' @seealso
#'   \code{\link{check_is_scalar_boolean}}
#'   \code{\link{check_is_scalar_character}}
#'   \code{\link{check_is_scalar_integer}}
#'
check_is_scalar_numeric <- function(num,
                                    error = FALSE) {

  # check if input error is boolean vector of length 1
  check_is_scalar_boolean(bool = error,
                          error = TRUE)

  # check if input is a numeric vector of length 1
  if ((!rlang::is_scalar_double(num) && !rlang::is_scalar_integer(num))
      || rlang::is_na(num)) {

    if (error == TRUE) {

      cli::cli_abort( # nolint return_linter
        c(
          "x" = "{.arg {rlang::caller_arg(num)}} must be a scalar numeric!"
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
