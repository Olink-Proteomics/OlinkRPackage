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

#' Common parameters for read_npx-related functions.
#'
#' @author
#'   Klev Diamanti
#'
#' @param filename Path to Olink software output file in wide or long format.
#' Expecting extensions `r get_file_ext_summary()`.
#' @param file Path to Olink software output file in wide or long format.
#' Expecting extensions `r get_file_ext_summary()`.
#' @param out_df The class of the output dataset. One of
#' `r ansi_collapse_quot(x = read_npx_df_output, sep = "or")`. (default =
#' "tibble")
#' @param long_format Boolean marking format of input file. One of `NULL`
#' (default) for auto-detection, or `TRUE` for long format and `FALSE` for
#' wide format files.
#' @param olink_platform Olink platform used to generate the input file.
#' One of `NULL` (default) for auto-detection,
#' or `r ansi_collapse_quot(x = accepted_olink_platforms$name)`.
#' @param data_type Quantification method of the input data. One of `NULL`
#' (default) for auto-detection, `r ansi_collapse_quot(get_olink_data_types())`.
#' @param .ignore_files Character vector of files included in the zip-compressed
#' Olink software output files that should be ignored. Used only for
#' zip-compressed input files (default = \emph{c("README.txt")}).
#' @param quiet Boolean to print a confirmation message when reading the input
#' file. Applies to excel or delimited input only. `TRUE` (default) to not print
#' and `FALSE` to print.
#'
#' @return Dataset,
#' `r ansi_collapse_quot(x = get_df_output_print(), sep = "or")`, with Olink
#' data in long format.
#'
.read_npx_args <- function(filename,
                           file,
                           out_df,
                           long_format,
                           olink_platform,
                           data_type,
                           .ignore_files,
                           quiet) {}
