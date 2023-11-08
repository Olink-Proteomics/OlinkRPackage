#' Help function checking if file exists.
#'
#' @param file Path to the file.
#'
check_file_exists <- function(file) {

  if (is.null(file)) {

    cli::cli_abort(
      c(
        "x" = "Unable to locate file: {file}",
        "i" = "File cannot be NULL!"
      ),
      call = NULL,
      wrap = FALSE
    )

  } else if (any(is.na(file))) {

    cli::cli_abort(
      c(
        "x" = "Unable to locate file: {file}",
        "i" = "File cannot be NA!"
      ),
      call = NULL,
      wrap = FALSE
    )

  } else if (any(!file.exists(file))) {

    missing_files <- file[!file.exists(file)]

    cli::cli_abort(
      c(
        "x" = "Unable to locate file{?s}: {missing_files}",
        "i" = "Missing?"
      ),
      call = NULL,
      wrap = FALSE
    )

  }

}
