#' Help function to identify Olink assays with all quantified values NA.
#'
#' @author
#'  Simon Forsberg
#'  Masoumeh Sheikhi
#'
#' @param df A tibble or an arrow object containing columns "OlinkID" and
#' either "NPX", "Quantified_value" or "Ct"
#'
#' @param col_names A list of matched column names, the output of `check_npx_col_names` function.
#'
#' @return A character vector containing
#' Olink ID of assays with all quantified values NA,
#' otherwise returns `character(0)`.

check_all_na_assays <- function(df, col_names) {

  # required columns: olink_id and quant
  # this seems redundant in presence of col_names that is the output of check_npx_col_names
  # if the olinkid of npx or their equivalent is missing, check_npx_col_names will through an error
  check_columns(df,
                col_list = list(col_names$olink_id,
                                col_names$quant))


  # Identify assays with only NAs
  all_nas <-
    df |>
    dplyr::select(
      dplyr::all_of(
        c(
        col_names$olink_id,
        col_names$quant
        )
      )
    ) |>
    dplyr::group_by(
      .data[[col_names$olink_id]]
    ) |>
    dplyr::mutate(
      is_na = ifelse(is.na(.data[[col_names$quant]]), 1L, 0L)
    ) |>
    dplyr::summarise(
      n = dplyr::n(),
      n_na = sum(is_na),
      .groups = "drop"
    )  |>
    dplyr::filter(
      n == n_na
      )  |>
    dplyr::collect() |>
    dplyr::pull(
      .data[[col_names$olink_id]]) # ,as_vector = TRUE


  # Issue warning if any assays with only NAs are found
  if (length(all_nas) > 0L) {
    cli::cli_warn(c(
      x = "{all_nas} ha{?s/ve} {col_names$quant} = NA for all samples."))
  }

  return(all_nas)
}
