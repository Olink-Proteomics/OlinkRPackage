#' Help function checking if a variable is a character vector of length 1.
#'
#' @param string Input to check.
#'
#' @return An error if the input is not a character vector of length 1
#'
check_is_scalar_character <- function(string) {

  # check that required libraries are installed
  if (!rlang::is_scalar_character(string)
      || rlang::is_na(string)) {

    cli::cli_abort(
      c(
        "x" = "{.arg {rlang::caller_arg(string)}} must be a string!"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

}
