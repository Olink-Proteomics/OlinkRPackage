#' Help function checking if a variable is a numeric vector.
#'
#' @author Klev Diamanti
#'
#' @param num Input to check.
#' @param error Boolean to return error or a boolean (default).
#'
#' @return Boolean if the variable is a numeric vector or not, and an error if
#' `error = TRUE`.
#'
#' @seealso
#'   [check_is_boolean()]
#'   [check_is_character()]
#'   [check_is_integer()]
#'
check_is_numeric <- function(num,
                             error = FALSE) {

  # check if input error is boolean vector of length 1
  check_is_scalar_boolean(bool = error,
                          error = TRUE)

  # check that the input is a character vector
  if ((!rlang::is_character(num) && !rlang::is_integer(num) && !is.numeric(num))
      || any(rlang::are_na(num))) {

    if (error == TRUE) {

      cli::cli_abort(
        c(
          "x" = "{.arg {rlang::caller_arg(num)}} must be a number vector!"
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

#' Help function checking if a variable is a numeric vector of length 1.
#'
#' @author Klev Diamanti
#'
#' @param num Input to check.
#' @param error Boolean to return error or a boolean (default).
#'
#' @return Boolean if the variable is a numeric vector of length 1 or not, and
#' an error if `error = TRUE`.
#'
#' @seealso
#'   [check_is_scalar_boolean()]
#'   [check_is_scalar_character()]
#'   [check_is_scalar_integer()]
#'
check_is_scalar_numeric <- function(num,
                                    error = FALSE) {

  # check if input error is boolean vector of length 1
  check_is_scalar_boolean(bool = error,
                          error = TRUE)

  # check that the input is a character vector of length 1
  if ((!rlang::is_scalar_double(num) && !rlang::is_scalar_integer(num))
      || rlang::is_na(num)) {

    if (error == TRUE) {

      cli::cli_abort(
        c(
          "x" = "{.arg {rlang::caller_arg(num)}} must be a number!"
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
