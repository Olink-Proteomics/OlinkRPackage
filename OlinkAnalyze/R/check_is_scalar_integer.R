#' Help function checking if a variable is an integer vector of length 1.
#'
#' @param int Input to check.
#' @param error Boolean to return error or a boolean (default).
#'
#' @return An error if the input is not an integer vector of length 1
#'
check_is_scalar_integer <- function(int,
                                    error = FALSE) {

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
