#' Help function checking if a variable is a vector of booleans.
#'
#' @inherit .check_params params author return seealso
#'
check_is_boolean <- function(x,
                             error = FALSE) {

  # check if input is a boolean vector
  if (!rlang::is_logical(x)
      || any(rlang::are_na(x))) {

    if (error == TRUE) {

      cli::cli_abort( # nolint return_linter
        c(
          "x" = "{.arg {rlang::caller_arg(x)}} must be a boolean vector!"
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

#' Help function checking if a variable is a scalar boolean.
#'
#' @inherit .check_params params author return seealso
#'
check_is_scalar_boolean <- function(x,
                                    error = FALSE) {

  # check if input is a boolean vector of length 1
  if (!rlang::is_scalar_logical(x)
      || rlang::is_na(x)) {

    if (error == TRUE) {

      cli::cli_abort( # nolint return_linter
        c(
          "x" = "{.arg {rlang::caller_arg(x)}} must be a scalar boolean!"
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
