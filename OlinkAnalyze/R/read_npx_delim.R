#' Help function to read NPX, Ct or absolute quantification data from semicolon-
#' or comma-delimited Olink software output files in R.
#'
#' @description
#' The function can handle delimited files in long and wide format.
#'
#' @author
#'   Klev Diamanti;
#'   Christoffer Cambronero;
#'   Kathleen Nevola
#'
#' @param file Path to Olink software output delimited file in wide or long
#' format. Expecting file extensions `csv` or `txt`.
#' @param out_df The class of output data frame. One of `tibble` (default) or
#' `arrow` for ArrowObject.
#' @param sep Character separator of delimited input file. One of `NULL` for
#' auto-detection (default), `,` for comma or `;` for semicolon. Used only for
#' delimited output files from Olink software.
#'
#' @return Tibble or ArrowObject with Olink data in long format.
#'
#' @seealso
#'   \code{\link{read_npx}}
#'   \code{\link{read_npx_parquet}}
#'   \code{\link{read_npx_zip}}
#'   \code{\link{read_npx_excel}}
#'   \code{\link{read_npx_format}}
#'   \code{\link{read_npx_delim_wide}}
#'   \code{\link{read_npx_delim_long}}
#'
read_npx_delim <- function(file,
                           out_df = "arrow",
                           sep = NULL) {

  # check input ----

  # check if file exists
  check_file_exists(file = file,
                    error = TRUE)

  # check that the requested output df is ok
  check_out_df_arg(out_df = out_df)

  # read file ----

  # using two tryCatch to check if file is long or wide

  # check if long long works
  df_olink <- tryCatch({
    # Try executing long
    read_npx_delim_long(file = file, sep = sep)
  }, warning = function(w) {
    # Handle warnings
    return("warning")
  }, error = function(e) {
    # Handle errors
    return("error")
  })

  # if long failed, try wide
  if (is.character(df_olink) && df_olink %in% c("warning", "error")) {
    df_olink <- tryCatch({
      # Try executing wide
      read_npx_delim_wide(file = file, sep = sep)
    }, warning = function(w) {
      # Handle warnings
      return("warning")
    }, error = function(e) {
      # Handle errors
      return("error")
    })
  }

  # check if any error or warning occurred ----

  if (is.character(df_olink) && df_olink %in% c("warning", "error")) {

    # If both functions throw errors, return an error message
    cli::cli_abort(
      c(
        "x" = "Unable to open delimited file: {.file {file}}",
        "i" = "Check if the input {.arg file} is a delimited file."
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

  # additional checks ----

  if (length(names(df_olink)) == 1L) {

    cli::cli_warn(
      message = c(
        "The delimited file {.file {file}} has only one column.",
        "i" = "Wrong input {.arg sep} = {.val {sep}}?"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

  # convert df class ----

  # if needed convert the object to the requested output
  df_olink <- convert_read_npx_output(df = df_olink,
                                      out_df = out_df)

  # return ----

  return(df_olink)
}

#' @rdname read_npx_delim
read_npx_csv <- read_npx_delim

#' Help function to read NPX, Ct or absolute quantification data from semicolon-
#' or comma-delimited Olink software output files in R.
#'
#' @description
#' The function can handle delimited files in long format.
#'
#' @author
#'   Klev Diamanti;
#'   Christoffer Cambronero;
#'   Kathleen Nevola
#'
#' @param file Path to Olink software output delimited file in long format.
#' Expecting file extensions `csv` or `txt`.
#' @param sep Character separator of delimited input file. One of `NULL` for
#' auto-detection (default), `,` for comma or `;` for semicolon. Used only for
#' delimited output files from Olink software.
#'
#' @return Tibble or ArrowObject with Olink data in long format.
#'
#' @seealso
#'   \code{\link{read_npx_delim_wide}}
#'   \code{\link{read_npx_delim}}
#'
read_npx_delim_long <- function(file,
                                sep = NULL) {

  # check input ----

  # check if file exists
  check_file_exists(file = file,
                    error = TRUE)
  # check separator
  sep <- check_field_separator(file = file,
                               sep = sep)

  # read long file ----

  df_olink <- arrow::open_delim_dataset(
    sources = file,
    delim = sep,
    col_names = TRUE,
    quoted_na = TRUE,
    na = c("", "NA")
  )

  # return ----

  return(df_olink)

}

#' Help function to read NPX, Ct or absolute quantification data from semicolon-
#' or comma-delimited Olink software output files in R.
#'
#' @description
#' The function can handle delimited files in wide format.
#'
#' @author
#'   Klev Diamanti
#'
#' @param file Path to Olink software output delimited file in wide format.
#' Expecting file extensions `csv` or `txt`.
#' @param sep Character separator of delimited input file. One of `NULL` for
#' auto-detection (default), `,` for comma or `;` for semicolon. Used only for
#' delimited output files from Olink software.
#'
#' @return An R6 class ArrowObject or tibble.
#'
#' @seealso
#'   \code{\link{read_npx_delim_long}}
#'   \code{\link{read_npx_delim}}
#'
read_npx_delim_wide <- function(file,
                                sep = NULL) {

  # check input ----

  # check if file exists
  check_file_exists(file = file,
                    error = TRUE)

  # check separator
  sep <- check_field_separator(file = file,
                               sep = sep)

  # read wide file ----

  df_olink <- utils::read.delim(
    file = file,
    header = FALSE,
    sep = sep,
    blank.lines.skip = FALSE,
    na.strings = c("", "NA")
  )
  colnames(df_olink) <- paste0("V", seq_len(ncol(df_olink)))

  df_olink <- dplyr::as_tibble(df_olink)

  # return ----

  return(df_olink)

}

#' Help function to check and get the separator of a delimited file from Olink
#' software.
#'
#' @author
#'   Klev Diamanti
#'
#' @param file Path to Olink software output delimited file in wide or long
#' format. Expecting file extensions `csv` or `txt`.
#' @param sep Character separator of delimited input file. One of `NULL` for
#' auto-detection (default), `,` for comma or `;` for semicolon. Used only for
#' delimited output files from Olink software.
#'
#' @return The file separator comma `,` or semicolon `;`.
#'
#' @seealso
#'   \code{\link{read_npx_delim_long}}
#'   \code{\link{read_npx_delim_wide}}
#'   \code{\link{read_npx_delim}}
#'
check_field_separator <- function(file,
                                  sep = NULL) {

  # check separator
  if (is.null(sep)) {

    sep <- get_field_separator(file = file)

  } else if (!check_is_scalar_character(string = sep, error = FALSE)) {

    cli::cli_abort(
      c(
        "x" = "{.arg sep} should be a string!"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  } else if (!(sep %in% accepted_field_sep)) {

    cli::cli_abort(
      c(
        "x" = "Unexpected separator: {sep}",
        "i" = "Expecting: {accepted_field_sep}"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

  return(sep)
}

#' Help function to get the separator of a delimited file from Olink software.
#'
#' @description
#' This function uses the first line of the provided file to determine the
#' separator of the file.
#'
#' \strong{Note:} The function does not allow presence of commas and semicolons
#' on the same line.
#'
#' @author
#'   Klev Diamanti
#'
#' @param file Path to Olink software output delimited file in wide or long
#' format. Expecting file extensions `csv` or `txt`.
#'
#' @return The file separator comma `,` or semicolon `;`.
#'
#' @seealso
#'   \code{\link{read_npx_delim_long}}
#'   \code{\link{read_npx_delim_wide}}
#'   \code{\link{read_npx_delim}}
#'
get_field_separator <- function(file) {

  # check input is a string
  check_is_scalar_character(string = file,
                            error = TRUE)

  # check if file exists
  check_file_exists(file = file,
                    error = TRUE)

  # get the first line of the file
  file_line_1 <- readLines(file, n = 1L)

  # throw an error if line 1 is empty or the file is empty
  if (!check_is_scalar_character(string = file_line_1, error = FALSE)) {

    cli::cli_abort(
      c(
        "x" = "Unable to read header line from {.file {file}}",
        "i" = "Empty header or empty {.arg file}?"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

  # count fileds for semicolon
  num_fields_semicolon <- utils::count.fields(
    file = textConnection(file_line_1),
    sep = ";"
  )

  # count fileds for comma
  num_fields_comma <- utils::count.fields(
    file = textConnection(file_line_1),
    sep = ","
  )

  if (num_fields_semicolon > 1L && num_fields_comma == 1L) {

    return(";")

  } else if (num_fields_semicolon == 1L && num_fields_comma > 1L) {

    return(",")

  } else if (num_fields_semicolon > 1L && num_fields_comma > 1L) {

    cli::cli_abort(
      c(
        "x" = "Unable to identify the separator of the file: {.file {file}}",
        "i" = "Both semicolon (;) and comma (,) are present in header line."
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  } else {

    cli::cli_abort(
      c(
        "x" = "Unable to identify the separator of the file: {.file {file}}",
        "i" = "Expecting semicolon (;) or comma (,)!"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }
}
