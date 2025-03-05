#' Help function checking if a variable is an R6 ArrowObject.
#'
#' @author
#'   Klev Diamanti
#'
#' @param df Variable to check.
#' @param error Scalar boolean to return an error instead of a `FALSE`
#' (`default = FALSE`).
#'
#' @return `TRUE` if the variable is an ArrowObject, and `FALSE` if not; error
#' if the variable is not an ArrowObject and `error = TRUE`.
#'
#' @seealso
#'   \code{\link{check_is_tibble}}
#'   \code{\link{check_is_dataset}}
#'
check_is_arrow_object <- function(df,
                                  error = FALSE) {

  # check if input error is boolean vector of length 1
  check_is_scalar_boolean(bool = error,
                          error = TRUE)

  # check if input is an ArrowObject
  if (!inherits(x = df,
                what = "ArrowObject")) {

    if (error == TRUE) {

      # error if the variable is not a tibble
      cli::cli_abort(
        c(
          "x" = "{.arg {rlang::caller_arg(df)}} is not an R6 ArrowObject!"
        ),
        call = rlang::caller_env(),
        wrap = FALSE
      )

      return(NULL)

    } else {

      return(FALSE)

    }

  } else {

    return(TRUE)

  }

}

#' Help function checking if a variable is a tibble dataset.
#'
#' @author
#'   Klev Diamanti
#'
#' @param df Variable to check.
#' @param error Scalar boolean to return an error instead of a `FALSE`
#' (`default = FALSE`).
#'
#' @return `TRUE` if the variable is a tibble, and `FALSE` if not; error if the
#' variable is not a tibble and `error = TRUE`.
#'
#' @seealso
#'   \code{\link{check_is_arrow_object}}
#'   \code{\link{check_is_dataset}}
#'
check_is_tibble <- function(df,
                            error = FALSE) {

  # check if input error is boolean vector of length 1
  check_is_scalar_boolean(bool = error,
                          error = TRUE)

  # check if input is a tibble
  if (!inherits(x = df,
                what = "tbl_df")) {

    if (error == TRUE) {

      # error if df is not a tibble
      cli::cli_abort(
        c(
          "x" = "{.arg {rlang::caller_arg(df)}} is not a tibble dataset!"
        ),
        call = rlang::caller_env(),
        wrap = TRUE
      )

      return(NULL)

    } else {

      return(FALSE)

    }

  } else {

    return(TRUE)

  }

}

#' Help function checking if a variable is a tibble or an ArrowObject dataset.
#'
#' @author
#'   Klev Diamanti
#'
#' @param df Variable to check.
#' @param error Scalar boolean to return an error instead of a `FALSE`
#' (`default = FALSE`).
#'
#' @return `TRUE` if the variable is a tibble or an ArrowObject, and `FALSE` if
#' not; error if the variable is not a tibble or an ArrowObject and
#' `error = TRUE`.
#'
#' @seealso
#'   \code{\link{check_is_arrow_object}}
#'   \code{\link{check_is_tibble}}
#'
check_is_dataset <- function(df,
                             error = FALSE) {

  # check input ----

  check_is_scalar_boolean(bool = error,
                          error = TRUE)

  # check if df is an arrow object or a data.frame/tibble ----

  is_arrow <- check_is_arrow_object(df = df, error = FALSE)
  is_tibble <- check_is_tibble(df = df, error = FALSE)

  if (is_arrow || is_tibble) {

    return(TRUE)

  } else {

    if (error == TRUE) {

      # error if df is not a tibble
      cli::cli_abort(
        c(
          "x" = "{.arg {rlang::caller_arg(df)}} is not a tibble or an
          ArrowObject dataset!"
        ),
        call = rlang::caller_env(),
        wrap = TRUE
      )

      return(NULL)

    } else {

      return(FALSE)

    }

  }

}
