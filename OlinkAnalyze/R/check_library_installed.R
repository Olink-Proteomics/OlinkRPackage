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

  # check for missing libraries
  missing_libraries <- sapply(libraries, rlang::is_installed) |>
    (\(x) x[x == FALSE])() |>
    names()

  # check that required libraries are installed
  if (length(missing_libraries) > 0) {

    if (error == TRUE) {

      cli::cli_abort(
        c(
          "x" = "Missing librar{?y/ies}: {.pkg {missing_libraries}}",
          "i" = "Please install!"
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
