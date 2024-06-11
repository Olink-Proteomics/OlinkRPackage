#' Help function checking whether df contains unrecognized Olink IDs
#'
#' @author
#'  Masoumeh Sheikhi
#'
#' @param df A tibble or an arrow object containing containing column "OlinkID"
#' @param col_names A list of matched column names.
#' This is the output of `check_npx_col_names` function.
#'
#' @return A character vector including unrecognized Olink IDs
#'

check_olinkid <- function(df, col_names) {

  # check input
  check_is_arrow_or_tibble(df,
                           error = TRUE)

  # extract invalid Olink IDs
  invalid_oid <- df  |>
    dplyr::distinct(.data[[col_names$olink_id]])  |>
    dplyr::filter(stringr::str_detect(.data[[col_names$olink_id]],
                                      "^OID[0-9]{5}$",
                                      negate = TRUE))  |>
    dplyr::collect() |>
    dplyr::pull(.data[[col_names$olink_id]])

  # warning if there is any invalid Olink ID
  if (length(invalid_oid) > 0L) {
    cli::cli_warn(
      c("x" = "Unrecognized Olink ID{?s} detected: {invalid_oid}"),
      call = rlang::caller_env()
    )
  }

  return(invalid_oid)
}
