#' Help function checking if a variable is a numeric vector.
#'
#' @param num Input to check.
#' @param error Boolean to return error or a boolean (default).
#'
#' @return An error if the input is not a character vector.
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
