#' Prints options for \var{out_df}.
#'
#' @return A scalar character vector with the input options for \var{out_df}.
#'
get_df_output <- function() {
  paste0("\"", read_npx_df_output, "\"") |>
    cli::ansi_collapse()
}

#' Prints class type output from read_npx* functions.
#'
#' @return A scalar character vector with the class type of outputs from
#' read_npx* functions.
#'
get_df_output_print <- function() {
  paste0("\"", read_npx_df_output, "\"") |>
    (\(.) stringr::str_replace_all(., "arrow", "ArrowObject"))() |>
    cli::ansi_collapse(sep2 = " or ", last = " or ")
}
