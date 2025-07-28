#' Help function checking if a variable is a vector of characters.
#'
#' @inherit .check_params params author return seealso
#'
check_is_character <- function(x,
                               error = FALSE) {

  # check if input error is boolean vector of length 1
  check_is_scalar_boolean(x = error,
                          error = TRUE)

  # check if input is a character vector
  if (!rlang::is_character(x)
      || any(rlang::are_na(x))) {

    if (error == TRUE) {

      cli::cli_abort( # nolint return_linter
        c(
          "x" = "{.arg {rlang::caller_arg(x)}} must be a character vector!"
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

#' Help function checking if a variable is a scalar character.
#'
#' @inherit .check_params params author return seealso
#'
check_is_scalar_character <- function(x,
                                      error = FALSE) {

  # check if input error is boolean vector of length 1
  check_is_scalar_boolean(x = error,
                          error = TRUE)

  # check if input is a character vector of length 1
  if (!rlang::is_scalar_character(x)
      || rlang::is_na(x)) {

    if (error == TRUE) {

      cli::cli_abort( # nolint return_linter
        c(
          "x" = "{.arg {rlang::caller_arg(x)}} must be a scalar character!"
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
