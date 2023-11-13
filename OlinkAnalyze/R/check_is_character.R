#' Help function checking if a variable is a character vector.
#'
#' @param string Input to check.
#' @param error Boolean to return error or a boolean (default).
#'
#' @return An error if the input is not a numeric vector.
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
