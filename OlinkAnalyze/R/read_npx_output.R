#' Help function converting the output data frame from a read_npx* function to a
#' tibble or an ArrowObject.
#'
#' @author
#'   Klev Diamanti
#'
#' @param df The data frame to be converted.
#' @param out_df The class of output data frame. One of `tibble` (default) or
#' `arrow` for ArrowObject.
#'
#' @return The data frame in the requested class.
#'
convert_read_npx_output <- function(df,
                                    out_df) {

  # check that out_df is ok
  check_out_df_arg(out_df = out_df)

  if (check_is_arrow_or_tibble(df = df, error = FALSE)) {

    if (out_df == "tibble") {

      return(dplyr::as_tibble(df))

    } else if (out_df == "arrow") {

      return(arrow::as_arrow_table(df))

    }

  } else {

    # if nont of the above throw an error
    cli::cli_abort(
      message = c(
        "x" = "Unexpected input data frame {.arg {rlang::caller_arg(df)}}!",
        "i" = "Expecting: { cli::ansi_collapse(x = read_npx_df_output,
                                               last = \", or \") }"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

}

#' Help function checking that the requested output class of the read_npx*
#' functions is acceptable.
#'
#' @author Klev Diamanti
#'
#' @param out_df The class of output data frame. One of `tibble` (default) or
#' `arrow` for ArrowObject.
#'
#' @return An error if the argument is not as expected.
#'
check_out_df_arg <- function(out_df) {

  # check taht out_df is a string
  check_is_scalar_character(string = out_df,
                            error = TRUE)

  if (!(out_df %in% read_npx_df_output)) {

    cli::cli_abort(
      message = c(
        "x" = "Unknown output argument {.arg {rlang::caller_arg(out_df)}}!",
        "i" = "Acceptable {.arg {rlang::caller_arg(out_df)}}:
        {read_npx_df_output}"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

}
