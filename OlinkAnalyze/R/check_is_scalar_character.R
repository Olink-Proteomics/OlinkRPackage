#' Help function checking if a variable is a character vector of length 1.
#'
#' @param string Input to check.
#' @param error Boolean to return error or a boolean (default).
#'
#' @return An error if the input is not a character vector of length 1
#'
check_is_scalar_character <- function(string,
                                      error = FALSE) {

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
