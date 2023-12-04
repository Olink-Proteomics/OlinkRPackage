#' Help function checking if file exists.
#'
#' @author Klev Diamanti
#'
#' @param file Path to the file.
#' @param error Boolean to return error or a boolean (default).
#'
#' @description
#' Only one file at a time!
#'
#' @return Boolean if the file exists or not, and an error if `error = TRUE`.
#'
check_file_exists <- function(file,
                              error = FALSE) {

  # check if input file is character vector of length 1
  check_is_scalar_character(string = file,
                            error = TRUE)

  # check if input error is boolean vector of length 1
  check_is_scalar_boolean(bool = error,
                          error = TRUE)

  if (!file.exists(file)) {

    if (error == TRUE) {

      # error if the file does not exist
      cli::cli_abort(
        c(
          "x" = "Unable to locate file: {.file {file}}",
          "i" = "Missing {.arg file}?"
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
