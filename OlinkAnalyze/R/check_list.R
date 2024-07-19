#' Help function to check if the variable is a list.
#'
#' @author
#'   Klev Diamanti
#'
#' @param lst List to check.
#' @param error Return error or a boolean (default = FALSE).
#'
#' @return Boolean if the object is a list or not, and an error if
#' "error = TRUE".
#'
check_is_list <- function(lst,
                          error = FALSE) {

  # check if input error is boolean vector of length 1
  check_is_scalar_boolean(bool = error,
                          error = TRUE)

  if (!is.list(x = lst)) {

    if (error == TRUE) {

      # error if lst is not a list
      cli::cli_abort(
        c(
          "x" = "{.arg {rlang::caller_arg(lst)}} is not a list!"
        ),
        call = rlang::caller_env(),
        wrap = TRUE
      )

    } else {

      return(FALSE)

    }

  } else {

    return(TRUE)

  }

}
