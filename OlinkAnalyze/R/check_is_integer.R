#' Help function checking if a variable is an integer vector.
#'
#' @param int Input to check.
#' @param error Boolean to return error or a boolean (default).
#'
#' @return An error if the input is not a character vector.
#'
check_is_integer <- function(int,
                             error = FALSE) {

  # check that the input is a character vector
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
