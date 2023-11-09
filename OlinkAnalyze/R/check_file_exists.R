#' Help function checking if file exists.
#'
#' @param file Path to the file.
#'
#' @description
#' Only one file at a time!
#'
#'
check_file_exists <- function(file) {

  if (!file.exists(file)) {

    # error if the file does not exist
    cli::cli_abort(
      c(
        "x" = "Unable to locate file: {file}",
        "i" = "Missing?"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

}
