#' Help function checking if a data frame is an R6 ArrowObject.
#'
#' @author
#'   Klev Diamanti
#'
#' @param df Data frame to check.
#' @param error `TRUE` to return an error if \var{df} is not an arrow object;
#' `FALSE` (default) to return a boolean.
#'
#' @return A scalar boolean if the object is an arrow object or not, and an
#' error if \var{error} = `TRUE`.
#'
#' @seealso
#'   \code{\link{check_is_tibble}}
#'   \code{\link{check_is_arrow_or_tibble}}
#'
check_is_arrow_object <- function(df,
                                  error = FALSE) {

  # check if input error is boolean vector of length 1
  check_is_scalar_boolean(bool = error,
                          error = TRUE)

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

    } else {

      return(FALSE)

    }

  } else {

    return(TRUE)

  }

}

#' Help function checking if a data frame is a tibble data frame.
#'
#' @author
#'   Klev Diamanti
#'
#' @param df Data frame to check.
#' @param error `TRUE` to return an error if \var{df} is not a tibble; `FALSE`
#' (default) to return a boolean.
#'
#' @return A scalar boolean if the object is a tibble or not, and an error if
#' \var{error} = `TRUE`.
#'
#' @seealso
#'   \code{\link{check_is_arrow_object}}
#'   \code{\link{check_is_arrow_or_tibble}}
#'
check_is_tibble <- function(df,
                            error = FALSE) {

  # check if input error is boolean vector of length 1
  check_is_scalar_boolean(bool = error,
                          error = TRUE)

  if (!inherits(x = df,
                what = "tbl_df")) {

    if (error == TRUE) {

      # error if df is not a tibble
      cli::cli_abort(
        c(
          "x" = "{.arg {rlang::caller_arg(df)}} is not a tibble data frame!"
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

#' Help function checking if a data frame is a tibble or an arrow object.
#'
#' @author
#'   Klev Diamanti
#'
#' @param df Data frame to check.
#' @param error `TRUE` to return an error if \var{df} is not a tibble or an
#' arrow object; `FALSE` (default) to return a boolean.
#'
#' @return A scalar boolean if the object is a tibble or an arrow object or not,
#' and an error if \var{error} = `TRUE`.
#'
#' @seealso
#'   \code{\link{check_is_arrow_object}}
#'   \code{\link{check_is_tibble}}
#'
check_is_arrow_or_tibble <- function(df,
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
          "x" = "{.arg {rlang::caller_arg(df)}} is not a tibble or an arrow data
          frame!"
        ),
        call = rlang::caller_env(),
        wrap = TRUE
      )

    } else {

      return(FALSE)

    }

  }

}
