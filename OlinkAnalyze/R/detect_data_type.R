#' Help function checking whether the input data contains
#' a column named "NPX" or "Quantified_value".
#'
#' @author
#'  Simon Forsberg
#'  Masoumeh Sheikhi
#' @param df An arrow object
#' @return A character string indicating the detected data type
#' ("NPX" or "Quantified_value").
#'
detect_data_type <- function(df) {
  # check if it's an arrow object, stop if not
  check_is_arrow_object(df = df, error = TRUE)

  # Extract column names
  df_colnames <- names(df)

  # Check whether df contains NPX or QUANT ----
  if ("NPX" %in% df_colnames) {
    data_type <- "NPX"
  } else if ("Quantified_value" %in% df_colnames) {
    data_type <- "Quantified_value"
  } else {
    cli::cli_abort(
      c(
        "x" = "Neither NPX nor Quantified_value column present in the data."),
      call = rlang::caller_env())
  }

  return(data_type)
}
