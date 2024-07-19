#' Utility function removing columns with all values NA from a tibble or an
#' arrow object.
#'
#' @author
#'   Klev Diamanti
#'
#' @param df A tibble or an arrow object.
#'
#' @return The same data frame as input without all-NA columns.
#'
remove_all_na_cols <- function(df) {

  # input check ----

  check_is_arrow_or_tibble(df = df,
                           error = TRUE)

  # identify all NA cols ----

  na_cols <- sapply(df, \(x) sum(is.na(x)) == nrow(df))
  na_cols <- na_cols[na_cols == TRUE]
  na_cols <- names(na_cols)

  # remove all NA cols ----

  if (length(na_cols) > 0L) {
    df <- df |>
      dplyr::select(
        -dplyr::all_of(na_cols)
      )
  }

  # return ----

  return(df)
}
