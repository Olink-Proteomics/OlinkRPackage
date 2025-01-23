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
        unlist(),
      ext = paste0("\"", .data[["ext"]], "\"")
    ) |>
    dplyr::summarise(
      ext = cli::ansi_collapse(
        x = .data[["ext"]],
        sep2 = " or ",
        last = ", or "
      ),
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
