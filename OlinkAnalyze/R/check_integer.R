#' Help function checking if a variable is a vector of integers.
#'
#' @inherit .check_params params author return seealso
#'
check_is_integer <- function(x,
                             error = FALSE) {

  # check if input error is boolean vector of length 1
  check_is_scalar_boolean(x = error,
                          error = TRUE)

  # check if input is an integer vector
  if (!rlang::is_integer(x)
      || any(rlang::are_na(x))) {

    if (error == TRUE) {

      cli::cli_abort( # nolint return_linter
        c(
          "x" = "{.arg {rlang::caller_arg(x)}} must be an integer vector!"
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

#' Help function checking if a variable is a scalar integer.
#'
#' @inherit .check_params params author return seealso
#'
check_is_scalar_integer <- function(x,
                                    error = FALSE) {

  # check if input error is boolean vector of length 1
  check_is_scalar_boolean(x = error,
                          error = TRUE)

  # check if input is an integer vector of length 1
  if (!rlang::is_scalar_integer(x)
      || rlang::is_na(x)) {

    if (error == TRUE) {

      cli::cli_abort( # nolint return_linter
        c(
          "x" = "{.arg {rlang::caller_arg(x)}} must be a scalar integer!"
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
