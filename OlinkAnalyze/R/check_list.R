#' Help function checking if a variable is a list.
#'
#' @inherit .check_params params author return seealso
#'
check_is_list <- function(x,
                          error = FALSE) {

  # check if input error is boolean vector of length 1
  check_is_scalar_boolean(x = error,
                          error = TRUE)

  if (!rlang::is_bare_list(x = x)) {

    if (error == TRUE) {

      # error if lst is not a list
      cli::cli_abort( # nolint return_linter
        c(
          "x" = "{.arg {rlang::caller_arg(x)}} is not a list!"
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
