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

#' Help function checking if file extension is acceptable.
#'
#' @author Klev Diamanti
#'
#' @param file Path to the file.
#'
#' @return The type of the file extension based on the global variable
#' `accepted_npx_file_ext`
#'
check_file_extension <- function(file) {
  # check input ----

  check_is_scalar_character(string = file,
                            error = TRUE)

  # get file extension ----

  # get the extension of the input file
  f_ext <- tools::file_ext(x = file)

  # check what type of label the extension of the input matches to
  f_label <- accepted_npx_file_ext[accepted_npx_file_ext == f_ext] |>
    names()

  # check if file extension is applicable ----

  # if the extension of the input file was within the accepted ones it should
  # be a scalar character
  if (!check_is_scalar_character(string = f_label, error = FALSE)) {

    cli::cli_abort(
      message = c(
        "x" = "Unable to recognize the extension of the file {.file {file}}!",
        "i" = "Expected on of {.val {accepted_npx_file_ext}}!"
      ),
      call = NULL,
      wrap = FALSE
    )

  }

  # return ----

  return(f_label)

}
