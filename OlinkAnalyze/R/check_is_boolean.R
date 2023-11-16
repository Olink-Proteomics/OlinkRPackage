#' Help function checking if a variable is a boolean vector.
#'
#' @author Klev Diamanti
#'
#' @param bool Input to check.
#' @param error Boolean to return error or a boolean (default).
#'
#' @return Boolean if the variable is a boolean vector or not, and an error if
#' `error = TRUE`.
#'
#' @seealso
#'   [check_is_character()]
#'   [check_is_integer()]
#'   [check_is_numeric()]
#'
check_is_boolean <- function(bool,
                             error = FALSE) {

  # check that the input is a boolean vector
  if (!rlang::is_logical(bool)
      || any(rlang::are_na(bool))) {

    if (error == TRUE) {

      cli::cli_abort(
        c(
          "x" = "{.arg {rlang::caller_arg(bool)}} must be a boolean vector!"
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
