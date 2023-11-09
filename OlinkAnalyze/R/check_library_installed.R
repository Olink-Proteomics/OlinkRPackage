#' Help function to check if suggested libraries are installed when required.
#'
#' @param libraries A character vector of R libraries.
#'
#' @return An error if one of the libraries is missing.
#'
check_library_installed <- function(libraries) {

  # check that required libraries are installed
  if (!rlang::is_installed(libraries)) {

    cli::cli_abort(
      c(
        "x" = "One or more missing libraries: {libraries}",
        "i" = "Please install the required R libraries!"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

}
