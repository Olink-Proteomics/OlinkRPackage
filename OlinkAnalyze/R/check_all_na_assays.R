#' Help function to identify Olink assays with all quantified values NA.
#'
#' @author
#'  Simon Forsberg
#'  Masoumeh Sheikhi
#'
#' @param df A tibble or an arrow object containing columns "OlinkID" and
#' either "NPX", "Quantified_value" or "Ct"
#'
#' @param name_mask A list of matched column names, the output of `check_npx_col_names` function.
#'
#' @return A character vector containing
#' Olink ID of assays with all quantified values NA,
#' otherwise returns `character(0)`.

check_all_na_assays <- function(df, name_mask) {

  # required columns: olink_id and quant
  # this seems redundant in presence of name_mask that is the output of check_npx_col_names
  # if the olinkid of npx or their equivalent is missing, check_npx_col_names will through an error
  check_columns(df,
                col_list = list(name_mask$olink_id,
                                name_mask$quant))


  # Identify assays with only NAs
  all_nas <-
    df |>
    dplyr::select(
      dplyr::all_of(
        c(
        name_mask$olink_id,
        name_mask$quant
        )
      )
    ) |>
    dplyr::group_by(
      .data[[name_mask$olink_id]]
    ) |>
    dplyr::mutate(
      is_na = ifelse(is.na(.data[[name_mask$quant]]), 1L, 0L)
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
      .data[[name_mask$olink_id]],
      as_vector = TRUE
    )

  # Issue warning if any assays with only NAs are found
  if (length(all_nas) > 0L) {
    cli::cli_warn(c(
      x = "{all_nas} ha{?s/ve} {name_mask$quant} = NA for all samples."))
  }

  return(all_nas)
}
