#' Help function checking if file exists.
#'
#' @param file Path to the file.
#'
#' @description
#' Only one file at a time!
#'
#'
check_file_exists <- function(file) {

  if (is.null(file)) {

    # error if the file variable is NULL
    cli::cli_abort(
      c(
        "x" = "Unable to locate file: {file}",
        "i" = "File cannot be NULL!"
      ),
      call = NULL,
      wrap = FALSE
    )

  } else if (any(is.na(file))) {

    # error if the file variable is NA
    cli::cli_abort(
      c(
        "x" = "Unable to locate file: {file}",
        "i" = "File cannot be NA!"
      ),
      call = NULL,
      wrap = FALSE
    )

  } else if (length(file) != 1) {

    # error if the file variable is not of length 1L
    cli::cli_abort(
      c(
        "x" = "Only one file is allowed!",
        "i" = "Provided: {file}"
      ),
      call = NULL,
      wrap = FALSE
    )

  } else if (!file.exists(file)) {

    # error if the file does not exist
    cli::cli_abort(
      c(
        "x" = "Unable to locate file: {file}",
        "i" = "Missing?"
      ),
      call = NULL,
      wrap = FALSE
    )

  }

}
