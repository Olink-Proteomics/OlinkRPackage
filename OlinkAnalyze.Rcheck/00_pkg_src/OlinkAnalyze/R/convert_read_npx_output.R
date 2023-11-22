#' Help function converting the output data frame from the read_npx* function to
#'  a tibble or an ArrowObject.
#'
#' @author Klev Diamanti
#'
#' @param df The data frame to be converted.
#' @param out_df The class of output data frame to be returned. Accepted values
#' are "tibble" and "arrow".
#'
#' @return The data frame in the requested format.
#'
convert_read_npx_output <- function(df,
                                    out_df) {

  # check that out_df is ok
  check_out_df_arg(out_df = out_df)

  if (check_is_arrow_object(df = df, error = FALSE)
      || check_is_tibble(df = df, error = FALSE)
      || check_is_data_frame(df = df, error = FALSE)) {

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
