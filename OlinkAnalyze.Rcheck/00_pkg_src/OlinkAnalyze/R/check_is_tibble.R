#' Help function to check if a data frame is a tibble data frame.
#'
#' @author Klev Diamanti
#'
#' @param df Data frame to check.
#' @param error Boolean to return error or a boolean (default).
#'
#' @return Boolean if the object is a tibble or not, and an error if
#' `error = TRUE`.
#'
#' @seealso
#'   [check_is_arrow_object()]
#'   [check_is_data_frame()]
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
