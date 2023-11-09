#' Help function to read in Olink data from comma or semicolon delimited files
#'
#' @param file Path to Olink software output in txt or csv.
#' @param sep The separator of the file: NULL (autodetect), comma (,) or
#' semicolon (;).
#'
#' @keywords NPX csv txt delim sep
#'
read_npx_delim <- function(file,
                         sep = NULL) {

  # check if file is a string
  check_is_string(string = file)

  # check if file exists
  check_file_exists(file = file)

  # check separator
  if (is.null(sep)) {

    sep <- get_field_separator(file = file)

  } else if (!rlang::is_string(sep)) {

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

      df_npx <- arrow::read_delim_arrow(
        file = file,
        delim = sep,
        na = c("NA", ""),
        col_names = TRUE
      )

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

  return(df_npx)
}

#' @rdname summarise_all
read_npx_csv <- read_npx_delim
#' @rdname summarise_all
