#' Help function checking if a variable is a vector of numerics.
#'
#' @inherit .check_params params author return seealso
#'
check_is_numeric <- function(x,
                             error = FALSE) {

  # check if input error is boolean vector of length 1
  check_is_scalar_boolean(x = error,
                          error = TRUE)

  # check if input is a numeric vector
  if ((!rlang::is_integer(x) && !is.numeric(x))
      || any(rlang::are_na(x))) {

    if (error == TRUE) {

      cli::cli_abort( # nolint return_linter
        c(
          "x" = "{.arg {rlang::caller_arg(x)}} must be a numeric vector!"
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

#' Help function checking if a variable is a scalar numeric
#'
#' @inherit .check_params params author return seealso
#'
check_is_scalar_numeric <- function(x,
                                    error = FALSE) {

  # check if input error is boolean vector of length 1
  check_is_scalar_boolean(x = error,
                          error = TRUE)

  # check if input is a numeric vector of length 1
  if ((!rlang::is_scalar_double(x) && !rlang::is_scalar_integer(x))
      || rlang::is_na(x)) {

    if (error == TRUE) {

      cli::cli_abort( # nolint return_linter
        c(
          "x" = "{.arg {rlang::caller_arg(x)}} must be a scalar numeric!"
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
