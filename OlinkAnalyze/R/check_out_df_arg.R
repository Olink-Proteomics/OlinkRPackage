#' Help function checking that the requested output class of the read_npx*
#' functions is acceptable.
#'
#' @author Klev Diamanti
#'
#' @param out_df The class of output data frame to be returned. Accepted values
#' are "tibble" and "arrow".
#'
#' @return `TRUE` if the argument is as expected, and an error otherwise.
#'
#' @seealso
#'   [read_npx()]
#'   [read_npx_delim()]
#'   [read_npx_zip()]
#'   [read_npx_parquet()]
#'   [convert_read_npx_output()]
#'
check_out_df_arg <- function(out_df) {

  # check taht out_df is a string
  check_is_scalar_character(string = out_df,
                            error = TRUE)

  if (out_df %in% read_npx_df_output) {

    return(TRUE)

  } else {

    cli::cli_abort(
      message = c(
        "x" = "Unknown output argument {.arg {rlang::caller_arg(out_df)}}!",
        "i" = "Acceptable output types: {read_npx_df_output}"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

}
