#' Help function to get the name of the checksum file from a zip NPX.
#'
#' @param files A vector of file names without any path prefix.
#'
#' @return The name of checksum file or NA if the file is not present.
#'
#' @keywords zip NPX checksum MD5 SHA256
#'
get_checksum_file <- function(files) {

  # if none of the files matches the accepted file names
  if (!any(files %in% accepted_checksum_files)) {

    return(NA_character_)

  }

  # if more than one files are in the accepted file names
  if (sum(files %in% accepted_checksum_files) > 1L) {

    cli::cli_abort(
      c(
        "x" = "The compressed file contains too many checksum files!",
        "i" = "The compressed input file should contain {.strong only} one
          checksum file: { glue::glue_collapse(x = accepted_checksum_files,
                                               sep = \", \",
                                               last = \" or \") }."
      ),
      call = NULL,
      wrap = FALSE
    )

  }

  # get the checksum file
  checksum_file <- files[files %in% accepted_checksum_files]

  # return
  return(checksum_file)
}
