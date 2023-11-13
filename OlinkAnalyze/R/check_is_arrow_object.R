#' Help function to check if a variable is an R6 ArrowObject.
#'
#' @param var Variable to check.
#' @param error Boolean to return error or a boolean (default).
#'
#' @return Boolean if the file is an R6 ArrowObject or not, and an error if
#' `error = TRUE`.
#'
check_is_arrow_object <- function(var,
                                  error = FALSE) {

  # check if input error is boolean vector of length 1
  check_is_scalar_boolean(bool = error,
                          error = TRUE)

  if (!all(c("ArrowObject", "R6") %in% class(var))) {

    if (error == TRUE) {

      # error if the variable is not a tibble
      cli::cli_abort(
        c(
          "x" = "{.arg {rlang::caller_arg(var)}} is not an R6 ArrowObject!"
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
