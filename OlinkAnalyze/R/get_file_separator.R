#' Help function to get the file separator.
#'
#' @param file Path to Olink software output in txt or csv.
#'
#' @return The file separator comma (;) or semicolon (;)
#'
get_file_separator <- function(file) {

  # check if file exists
  check_file_exists(file = file)

  # get the first line of the file
  file_line_1 <- readLines(file, n = 1L)

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
        "x" = "Unable to identify the separator of the file: {file}",
        "i" = "Both semicolon (;) and comma (,) are present in header line."
      ),
      call = NULL,
      wrap = FALSE
    )

  } else {

    cli::cli_abort(
      c(
        "x" = "Unable to identify the separator of the file: {file}",
        "i" = "Expecting semicolon (;) or comma (,)!"
      ),
      call = NULL,
      wrap = FALSE
    )

  }
}
