#' Help function to read `r ansi_collapse_quot(get_olink_data_types())` data
#' from delimited Olink software output files in R.
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
#' format. Expecting file extensions
#' `r get_file_ext(name_sub = "delim") |> ansi_collapse_quot()`.
#' @param out_df The class of output data frame. One of
#' `r ansi_collapse_quot(read_npx_df_output)`.
#'
#' @return `r ansi_collapse_quot(x = get_df_output_print(), sep = "or")` with
#' Olink data in long or wide format.
#'
#' @seealso
#'   \code{\link{read_npx}}
#'   \code{\link{read_npx_parquet}}
#'   \code{\link{read_npx_zip}}
#'   \code{\link{read_npx_excel}}
#'   \code{\link{read_npx_format}}
#'
read_npx_delim <- function(file,
                           out_df = "arrow") {

  # check input ----

  # check if file exists
  check_file_exists(file = file,
                    error = TRUE)

  # check that the requested output df is ok
  check_out_df_arg(out_df = out_df)

  # read file ----

  # get filed separator
  sep <- get_field_separator(file = file)

  # Using two tryCatch to check if file can be read.
  # Initially we assume that the file is in long format, so we try to read in
  # the delimited file using `header = TRUE`.
  # We have observed that the approach above throws a warning if the file is in
  # wide format and contains rows with no NPX measurements. The warning contains
  # the sub-string "Stopped early on line". Only when this warning occurs we
  # assume that the file is in wide format, so we return "warning-1" for a
  # special handling later. If another warning, or some error occurs we simply
  # return it and continue.

  # check if long long works
  df_olink <- tryCatch({
    # Try executing long
    read_npx_delim_long(file = file, sep = sep)
  }, warning = function(w) {
    # Handle warnings
    if (grepl("Stopped early on line", w$message)) {
      return("warning-1")
    } else {
      return("warning")
    }
  }, error = function(e) {
    # Handle errors
    return("error")
  })

  # When a file is read as long while it is wide it will have names for the
  # first two columns, while all other columns will have the prefix V and will
  # be consecutive numbers. If we find that all the columns from the 3rd and
  # onward have the prefix V, we can safely assume that the file was actually in
  # wide format instead of long, and we can read it with `header = FALSE`.
  #
  # Also, because of potential rows with no NPX measurements, we have to set the
  # field separator and `fill = TRUE`. `fill = TRUE` has some issues with
  # setting the delimiter, so we provide it.
  #
  # In the special case when the file has uneven number of columns and is in
  # wide format (see relevant test), we also get another warning when the file
  # is re-read as wide because the previous fread did not close properly. In
  # this case, we use the "warning-1" from above as a flag to suppress further
  # warnings.
  #
  # In any case when warning or error persists, we also return an error.
  if (check_is_dataset(df = df_olink, error = FALSE)) {
    num_v_cols <- grepl(
      pattern = "^V\\d*$",
      x = names(df_olink),
      ignore.case = FALSE
    ) |>
      sum()
    num_v_col_min <- ncol(df_olink) - 2L
    df_olink_col_check <- (num_v_cols == num_v_col_min) && (nrow(df_olink) > 2L)
    df_olink_warn_check <- FALSE
  } else {
    df_olink_col_check <- TRUE
    if (df_olink == "warning-1") {
      df_olink_warn_check <- TRUE
    } else {
      df_olink_warn_check <- FALSE
    }
  }

  if (df_olink_col_check == TRUE) {
    df_olink <- tryCatch({
      # Try executing wide
      if (df_olink_warn_check == TRUE) {
        read_npx_delim_wide(file = file, sep = sep) |>
          remove_all_na_cols() |>
          suppressWarnings()
      } else {
        read_npx_delim_wide(file = file, sep = sep) |>
          remove_all_na_cols()
      }
    }, warning = function(w) {
      # Handle warnings
      return("warning")
    }, error = function(e) {
      # Handle errors
      return("error")
    })
  }

  # check if any error or warning occurred ----

  if (check_is_character(string = df_olink, error = FALSE)
      && grepl("warning|error", df_olink)) {

    # If both functions throw errors, return an error message
    cli::cli_abort( # nolint return_linter
      c(
        "x" = "Unable to open delimited file: {.file {file}}!",
        "i" = "Check if {.arg file} is delimited or corrupt."
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

  # additional checks ----

  # too few columns

  if (ncol(df_olink) < 5L) {
    cli::cli_warn(
      message = c(
        "i" = "The delimited file {.file {file}} has too few
        (n={.val {ncol(df_olink)}}) columns!"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )
  }

  # top row is as expected for the corresponding format

  read_npx_format_colnames(df = df_olink, file = file)

  # convert df class ----

  # if needed convert the object to the requested output
  df_olink <- convert_read_npx_output(df = df_olink,
                                      out_df = out_df)

  # return ----

  return(df_olink)
}

#' @rdname read_npx_delim
read_npx_csv <- read_npx_delim

#' Help function to read long format
#' `r ansi_collapse_quot(get_olink_data_types())` data from delimited file
#' exported from Olink software.
#'
#' @description
#' The function can handle delimited files in long format.
#'
#' @author
#'   Klev Diamanti;
#'   Christoffer Cambronero;
#'   Kathleen Nevola;
#'   Ola Caster
#'
#' @param file Path to Olink software output delimited file in long format.
#' Expecting file extensions
#' @param sep Character separator of delimited input file. One of `NULL`
#' (default) for auto-detection, or `r ansi_collapse_quot(accepted_field_sep)`.
#' Used only for delimited output files from Olink software.
#'
#' @return `r ansi_collapse_quot(x = get_df_output_print(), sep = "or")` with
#' Olink data in long format.
#'
#' @seealso
#'   \code{\link{read_npx_delim_wide}}
#'   \code{\link{read_npx_delim}}
#'
read_npx_delim_long <- function(file,
                                sep) {

  # read long file ----

  df_olink <- data.table::fread(
    file = file,
    header = TRUE,
    sep = sep,
    stringsAsFactors = FALSE,
    na.strings = c("NA", ""),
    check.names = FALSE,
    data.table = FALSE
  ) |>
    dplyr::as_tibble()

  # return ----

  return(df_olink)

}

#' Help function to read wide format
#' `r get_file_ext(name_sub = "delim") |> ansi_collapse_quot()` data from
#' delimited file exported from Olink software.
#'
#' @description
#' The function can handle delimited files in wide format.
#'
#' @author
#'   Klev Diamanti
#'
#' @param file Path to Olink software output delimited file in wide format.
#' Expecting file extensions
#' `r get_file_ext(name_sub = "delim") |> ansi_collapse_quot()`.
#' @param sep Character separator of delimited input file. One of `NULL`
#' (default) for auto-detection, or `r ansi_collapse_quot(accepted_field_sep)`.
#' Used only for delimited output files from Olink software.
#'
#' @return `r ansi_collapse_quot(x = get_df_output_print(), sep = "or")` with
#' Olink data in wide format.
#'
#' @seealso
#'   \code{\link{read_npx_delim_long}}
#'   \code{\link{read_npx_delim}}
#'
read_npx_delim_wide <- function(file,
                                sep) {

  # Check if new lines are linux-like "\n" or Windows-like "\r\n" ----

  # if there is windows-line new line separator, we need to replace \r\n with
  # \n so that data.table::fread can read the empty lines. The updated file with
  # is written to a temp file, which is removed when exiting the function!.

  # read first line of the file
  file_line_1 <- readLines(file, n = 1L)
  # read first line plus 10 characters to extend a bit on the second line
  file_line_1_extended <- readChar(con = file,
                                   nchars = nchar(file_line_1) + 10L)
  if (grepl("\r\n", file_line_1_extended)) {
    # temporary directory to extract
    tmp_file <- tempfile()
    # read lines of file
    file_content <- readLines(con = file)
    file_content <- gsub("\r\n", "\n", file_content)
    writeLines(file_content, tmp_file)

    # cleanup temporary file after exiting the function
    on.exit(
      expr = invisible(file.remove(tmp_file)),
      add = TRUE
    )
  } else {
    tmp_file <- file
  }

  # read wide file ----

  df_olink <- data.table::fread(
    file = tmp_file,
    header = FALSE,
    sep = sep,
    stringsAsFactors = FALSE,
    na.strings = c("NA", ""),
    check.names = FALSE,
    data.table = FALSE,
    fill = TRUE
  ) |>
    dplyr::as_tibble() |>
    (\(df) dplyr::rename_with(df, ~ paste0("V", seq_len(ncol(df)))))()

  # return ----

  return(df_olink)

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
#' @param file Path to Olink software output delimited file in wide format.
#' Expecting file extensions
#' `r get_file_ext(name_sub = "delim") |> ansi_collapse_quot()`.
#'
#' @return The file delimiter
#' `r ansi_collapse_quot(x = accepted_field_sep, sep = "or")`.
#'
#' @seealso
#'   \code{\link{read_npx_delim}}
#'
get_field_separator <- function(file) {

  # check if file exists
  check_file_exists(file = file,
                    error = TRUE)

  # get the first 5 lines of the file
  file_lines <- tryCatch({
    readLines(con = file, n = 5L, skipNul = FALSE)
  }, warning = function(w) {
    # convert warning to error
    stop()
  }, error = function(e) {
    cli::cli_abort( # nolint return_linter
      c(
        "x" = "Unable to open delimited file: {.file {file}}!",
        "i" = "Check if {.arg file} is delimited or corrupt."
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )
  })

  # count occurrences of the delimiter in each lines
  count_sep_occur <- accepted_field_sep |>
    sapply(function(d) sum(grepl(pattern = d, x = file_lines, fixed = TRUE)))

  # no match to delimiters
  if (all(count_sep_occur == 0L)) {
    cli::cli_abort( # nolint return_linter
      c(
        "x" = "Unable to identify the separator of the file: {.file {file}}",
        "i" = "Expected one of {.val {accepted_field_sep}}!"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )
  } else {
    # Choose delimiter with most matches
    best_delim <- names(x = which.max(x = count_sep_occur))
  }

  return(best_delim)
}
