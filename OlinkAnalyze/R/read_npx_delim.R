#' Help function to read in Olink data from comma or semicolon delimited files.
#'
#' @author
#'   Klev Diamanti;
#'   Christoffer Cambronero;
#'   Kathleen Nevola
#'
#' @param file Path to Olink software output in txt or csv.
#' @param out_df The class of output data frame to be returned. Accepted values
#' are "tibble" and "arrow" (default).
#' @param sep The separator of the file: NULL (autodetect), comma (,) or
#' semicolon (;).
#'
#' @return An R6 class ArrowObject.
#'
#' @keywords NPX csv txt delim sep
#'
#' @seealso
#'   [read_npx()]
#'   [read_npx_parquet()]
#'   [read_npx_zip()]
#'   [read_npx_excel()]
#'
read_npx_delim <- function(file,
                           out_df = "arrow",
                           sep = NULL) {

  # check if file exists
  check_file_exists(file = file,
                    error = TRUE)

  # check that the requested output df is ok
  check_out_df_arg(out_df = out_df)

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

  # tryCatch in case reading the delimited file fails
  tryCatch(
    {

      df_olink <- arrow::open_delim_dataset(
        sources = file,
        delim = sep,
        col_names = TRUE,
        quoted_na = TRUE,
        na = c("", "NA")
      )

    }, error = function(msg) {

      cli::cli_abort(
        c(
          "x" = "Unable to open delimited file: {.file {file}}",
          "i" = "Check if the input {.arg file} is a delimiter-separated file."
        ),
        call = rlang::caller_env(),
        wrap = FALSE
      )

    }
  )

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

  # if needed convert the object to the requested output
  df_olink <- convert_read_npx_output(df = df_olink,
                                      out_df = out_df)

  return(df_olink)
}

#' @rdname read_npx_delim
read_npx_csv <- read_npx_delim

#' Help function to get the file separator.
#'
#' @author Klev Diamanti
#'
#' @param file Path to Olink software output in txt or csv.
#'
#' @description
#' This function uses the first line of the provided file to determine the
#' separator of the file.
#' _Note_ that the function will not accept the presence of commas and
#' semicolons on the same line.
#'
#' @return The file separator comma (;) or semicolon (;)
#'
#' @seealso
#'   [read_npx_delim()]
#'   [read_npx_csv()]
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
