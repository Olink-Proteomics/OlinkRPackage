#' Prints options for \var{out_df}.
#'
#' @return A scalar character vector with the input options for \var{out_df}.
#'
get_df_output <- function() {
  ansi_collapse_quot(x = read_npx_df_output, sep = "and")
}

#' Prints class type output from read_npx* functions.
#'
#' @return A scalar character vector with the class type of outputs from
#' read_npx* functions.
#'
get_df_output_print <- function() {
  stringr::str_replace_all(
    string = read_npx_df_output,
    pattern = "arrow",
    replacement = "ArrowObject"
  ) |>
    ansi_collapse_quot(sep = "or")
}
