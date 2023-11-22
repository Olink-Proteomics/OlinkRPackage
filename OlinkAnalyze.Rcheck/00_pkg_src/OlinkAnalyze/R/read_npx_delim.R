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
        "x" = "{.val sep} should be a string!"
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

      df_olink <- arrow::read_delim_arrow(
        file = file,
        delim = sep,
        col_names = TRUE,
        quoted_na = TRUE,
        na = c("", "NA")
      ) |>
        arrow::as_arrow_table()

    }, error = function(msg) {

      cli::cli_abort(
        c(
          "x" = "Unable to open delimited file: {file}",
          "i" = "Check if the input file is a delimiter-separated file."
        ),
        call = rlang::caller_env(),
        wrap = FALSE
      )

    }
  )

  if (length(names(df_olink)) == 1L) {

    cli::cli_warn(
      message = "The delimited file {file} has only one column. Wrong input sep
      \"{sep}\"?",
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
#' @rdname read_npx_delim
