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
