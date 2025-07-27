#' Utility function removing columns with all values NA from a dataset.
#'
#' @author
#'   Klev Diamanti
#'
#' @param df An Olink dataset.
#'
#' @return The input Olink dataset without all-NA columns.
#'
remove_all_na_cols <- function(df) {

  # input check ----

  check_is_dataset(df = df,
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

#' Utility function that adds quotation marks on elements printed by
#' ansi_collapse from cli.
#'
#' @param x Character vector.
#' @param sep One of "or" and "and".
#'
#' @return Scalar character vector collapsed by "and" or "or".
#'
ansi_collapse_quot <- function(x,
                               sep = "and") {
  x_paste <- paste0("\"", x, "\"")

  if (sep == "or") {
    x <- cli::ansi_collapse(x = x_paste, sep2 = " or ", last = ", or ")
  } else {
    x <- cli::ansi_collapse(x = x_paste)
  }
  return(x)
}

#' Common parameters for check functions.
#'
#' @author
#'   Klev Diamanti
#'
#' @param error Scalar boolean to return an error instead of a `FALSE`
#' (`default = FALSE`).
#'
.check_params <- function(error) {}
