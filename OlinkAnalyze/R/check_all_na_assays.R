#' Help function to identify assays with Only NAs
#'
#' @author
#'  Simon Forsberg
#'  Masoumeh Sheikhi
#' @param df An arrow object containing columns OlinkID and
#' either NPX or Quantified_value
#' @return A character vector of assays with all NA values.

check_all_na_assays <- function(df) {
  # detect data type, NPX or Quantified_value
  # Q1, since detect_data_type checks whether
  # df is an arrow object or not, can we skip it here?
  # Q2, when writing a test for this function,
  # should we write a test for next line as well?
  # seems like double testing for hidden internal functions..?
  data_type <- detect_data_type(df)

  # Identify assays with only NAs
  all_nas <- df  |>
    dplyr::group_by(OlinkID)  |>
    dplyr::summarise(
      n = dplyr::n(),
      n_na = sum(is.na(!!rlang::ensym(data_type))),
      .groups = "drop"
    )  |>
    dplyr::filter(n == n_na) %>%
    dplyr::pull(OlinkID, as_vector = TRUE)

  # Issue warning if any assays with only NAs are found
  if (length(all_nas) > 0) {
    cli::cli_warn(
      c(x = "{all_nas} ha{?s/ve} {data_type} = NA for all samples
        and will be excluded from the analysis.")
    )
  }

  return(all_nas)
}
