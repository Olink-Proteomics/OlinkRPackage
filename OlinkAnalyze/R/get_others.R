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
  )
}

#' Describes acceptable file extension for each file type.
#'
#' @return A scalar character vector with one sentence describing the acceptable
#' extensions of each file type.
#'
get_accepted_file_ext_summary <- function() {
  dplyr::tibble(
    ext = accepted_npx_file_ext,
    name = names(accepted_npx_file_ext)
  ) |>
    dplyr::mutate(
      name = strsplit(x = .data[["name"]],
                      split = "_",
                      fixed = TRUE) |>
        lapply(utils::head, 1L) |>
        unlist()
    ) |>
    dplyr::summarise(
      ext = ansi_collapse_quot(x = .data[["ext"]], sep = "or"),
      .by = dplyr::all_of("name")
    ) |>
    dplyr::mutate(
      outname = paste0(.data[["ext"]], " for ", .data[["name"]], " files")
    ) |>
    dplyr::pull(
      .data[["outname"]]
    ) |>
    cli::ansi_collapse()
}
