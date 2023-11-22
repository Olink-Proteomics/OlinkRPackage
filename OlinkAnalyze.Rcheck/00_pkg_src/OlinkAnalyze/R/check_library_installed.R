#' Help function to check if suggested libraries are installed when required.
#'
#' @author Klev Diamanti
#'
#' @param libraries A character vector of R libraries.
#' @param error Boolean to return error or a boolean (default).
#'
#' @return Boolean if the library is installed or not, and an error if
#' `error = TRUE`.
#'
check_library_installed <- function(libraries,
                                    error = FALSE) {

  # check that the input is a character vector
  check_is_character(string = libraries,
                     error = TRUE)

  # check if input error is boolean vector of length 1
  check_is_scalar_boolean(bool = error,
                          error = TRUE)

  # check that required libraries are installed
  if (!rlang::is_installed(libraries)) {

    if (error == TRUE) {

      cli::cli_abort(
        c(
          "x" = "One or more missing libraries: {libraries}",
          "i" = "Please install the required R libraries!"
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
