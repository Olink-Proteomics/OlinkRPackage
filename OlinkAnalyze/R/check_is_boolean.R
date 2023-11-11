#' Help function checking if a variable is a boolean vector.
#'
#' @param bool Input to check.
#' @param error Boolean to return error or a boolean (default).
#'
#' @return An error if the input is not a boolean vector.
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
