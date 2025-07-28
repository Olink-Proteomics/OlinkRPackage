#' Help function checking if a variable is an R6 ArrowObject.
#'
#' @inherit .check_params params author return seealso
#'
check_is_arrow_object <- function(x,
                                  error = FALSE) {

  # check if input error is boolean vector of length 1
  check_is_scalar_boolean(x = error,
                          error = TRUE)

  # check if input is an ArrowObject
  if (!inherits(x = x,
                what = "ArrowObject")) {

    if (error == TRUE) {

      # error if the variable is not a tibble
      cli::cli_abort( # nolint return_linter
        c(
          "x" = "{.arg {rlang::caller_arg(x)}} is not an R6 ArrowObject!"
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

#' Help function checking if a variable is a tibble dataset.
#'
#' @inherit .check_params params author return seealso
#'
check_is_tibble <- function(x,
                            error = FALSE) {

  # check if input error is boolean vector of length 1
  check_is_scalar_boolean(x = error,
                          error = TRUE)

  # check if input is a tibble
  if (!inherits(x = x,
                what = "tbl_df")) {

    if (error == TRUE) {

      # error if df is not a tibble
      cli::cli_abort( # nolint return_linter
        c(
          "x" = "{.arg {rlang::caller_arg(x)}} is not a tibble dataset!"
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

#' Help function checking if a variable is a tibble or an ArrowObject dataset.
#'
#' @inherit .check_params params author return seealso
#'
check_is_dataset <- function(x,
                             error = FALSE) {

  # check if df is an arrow object or a data.frame/tibble ----

  is_arrow <- check_is_arrow_object(x = x, error = FALSE)
  is_tibble <- check_is_tibble(x = x, error = FALSE)

  if (is_arrow || is_tibble) {

    return(TRUE)

  } else {

    if (error == TRUE) {

      # error if df is not a tibble
      cli::cli_abort( # nolint return_linter
        c(
          "x" = "{.arg {rlang::caller_arg(x)}} is not a tibble or an
          ArrowObject dataset!"
        ),
        call = rlang::caller_env(),
        wrap = TRUE
      )

    } else {

      return(FALSE)

    }

  }

}
