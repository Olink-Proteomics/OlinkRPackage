#' Help function checking whether df contains recognized OIDs
#'
#' @author
#'  Simon Forsberg
#'  Masoumeh Sheikhi
#'
#' @param df An arrow object
#'
#' @return A character vector indicating the invalid OlinkIDs
#'

check_olinkid <- function(df) {
  # check if df is an arrow object
  check_is_arrow_object(df = df, error = TRUE)

  # Extract column names
  df_colnames <- names(df)

  # Check whether df contains recognized OIDs
  if (!("OlinkID" %in% df_colnames)) {
    cli::cli_abort(
      c(
        "x" = "OlinkID column not present in the data."),
      call = rlang::caller_env())
  }

  invalid_oid <- df  |>
    dplyr::distinct(OlinkID)  |>
    dplyr::filter(stringr::str_detect(OlinkID,
                                      "^OID[0-9]{5}$",
                                      negate = TRUE))  |>
    dplyr::pull(OlinkID, as_vector = TRUE)


  if (length(invalid_oid) > 0L) {
    cli::cli_warn(
      c(
        "x" = "Unrecognized Olink ID{?s} detected: {invalid_oid}"),
      call = rlang::caller_env())
  }

  return(invalid_oid)
}
