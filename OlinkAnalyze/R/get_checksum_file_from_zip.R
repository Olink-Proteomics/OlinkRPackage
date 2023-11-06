#' Title
#'
#' @param files
#'
#' @return
#'
#' @importFrom rlang .env
#'
get_checksum_file_from_zip <- function(files) {

  # if none of the files matches the accepted file names
  if (!any(files %in% .env[["accepted_checksum_files"]])) {
    return(NA_character_)
  }

  # if more than one files are in the accepted file names
  if (sum(files %in% .env[["accepted_checksum_files"]]) > 1) {
    cli::cli_abort(
      c(
        "x" = "Too many checksum files!",
        "i" = "The input *.zip file should contain {.strong only} one checksum
        file: { glue::glue_collapse(x = .env[[\"accepted_checksum_files\"]],
                                    sep = \", \",
                                    last = \" or \") }."
      ),
      call = NULL,
      wrap = FALSE
    )
  }

  # get the checksum file
  checksum_file <- files[files %in% .env[["accepted_checksum_files"]]]

  # return
  return(checksum_file)
}
