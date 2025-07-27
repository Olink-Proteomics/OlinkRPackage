#' Help function checking if a variable is a list.
#'
#' @inherit .check_params params author
#' @param lst Variable to check.
#'
#' @return `TRUE` if the variable is a list, and `FALSE` if not; error if the
#' variable is not a list and `error = TRUE`.
#'
check_is_list <- function(lst,
                          error = FALSE) {

  # check if input error is boolean vector of length 1
  check_is_scalar_boolean(bool = error,
                          error = TRUE)

  if (!rlang::is_bare_list(x = lst)) {

    if (error == TRUE) {

      # error if lst is not a list
      cli::cli_abort( # nolint return_linter
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
