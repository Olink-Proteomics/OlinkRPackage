#' Help function checking for duplicate sample IDs in data
#'
#' @author
#'  Masoumeh Sheikhi
#'
#' @param df A tibble or an arrow object containing
#' the columns "SampleID" and "OlinkID".
#' @param col_names A list of matched column names.
#' This is the output of `check_npx_col_names` function.
#'

### Is it necessary to return anything here?

check_duplicate_sample_ids <- function(df, col_names) {

  # Select relevant columns
  sample_summary <- df  |>
    dplyr::select(dplyr::all_of(c(
      col_names$sample_id,
      col_names$olink_id
    ))) |>
    dplyr::group_by(
      .data[[col_names$sample_id]],
      .data[[col_names$olink_id]]
    ) |>
    dplyr::summarise(freq = dplyr::n(),
                     .groups = "drop") |>
    dplyr::collect()

  # Find duplicates
  duplicates <- character(0L)
  duplicates <- sample_summary |>
    dplyr::filter(.data[["freq"]] > 1) |>
    dplyr::collect() |>
    dplyr::pull(.data[[col_names$sample_id]])

  # Warn if duplicates are found
  if (length(duplicates) > 0L) {
    cli::cli_warn(c(
      "x" = "Duplicate sample ID{?s} detected:
      {duplicates}"
    ))
  }
}
