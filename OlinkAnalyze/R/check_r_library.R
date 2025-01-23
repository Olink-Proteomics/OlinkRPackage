#' Help function checking if one or more R libraries are installed.
#'
#' @author
#'   Klev Diamanti
#'
#' @param libraries Character vector of R libraries.
#' @param error Scalar boolean to return an error instead of a `FALSE`
#' (`default = FALSE`).
#'
#' @return `TRUE` if the libraries are installed, and `FALSE` if not; error if
#' the libraries are not installed and `error = TRUE`.
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
