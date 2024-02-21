#' Help function to identify Olink assays with all quantified values NA.
#'
#' @author
#'  Simon Forsberg
#'  Masoumeh Sheikhi
#'
#' @param df An arrow object containing columns "OlinkID" and
#' either "NPX" or "Quantified_value"
#'
#' @return A character vector containing
#' Olink ID of assays with all quantified values NA,
#' otherwise returns `character(0)`.

check_all_na_assays <- function(df, column_name_df) {

  # Identify assays with only NAs
  all_nas <-
    df |>
    dplyr::select(
      dplyr::all_of(
        c(
        column_name_df$olink_id,
        column_name_df$quant
        )
      )
    ) |>
    dplyr::group_by(
      .data[[column_name_df$olink_id]]
    ) |>
    dplyr::mutate(
      is_na = ifelse(is.na(.data[[column_name_df$quant]]), 1L, 0L)
    ) |>
    dplyr::summarise(
      n = dplyr::n(),
      n_na = sum(is_na),
      .groups = "drop"
    )  |>
    dplyr::filter(
      n == n_na
      )  |>
    dplyr::pull(
      .data[[column_name_df$olink_id]],
      as_vector = TRUE
    )

  # Issue warning if any assays with only NAs are found
  if (length(all_nas) > 0L) {
    cli::cli_warn(c(
      x = "{all_nas} ha{?s/ve} {column_name_df$quant} = NA for all samples."))
  }

  return(all_nas)
}
