#' Help function comparing the checksum reported by Olink software to the
#' checksum of the delivered NPX file.
#'
#' @param checksum_file The plain file that contains the checksum output from
#' Olink software. The file should contain "MD5" or "SHA256" in the file name.
#' @param npx_file The NPX file accompanying the checksum file.
#'
#' @return A string or NA. If the function return NA then everything worked
#' fine, otherwise there was some sort of error.
#'
check_checksum <- function(checksum_file,
                           npx_file) {

  # We should make it here only if MD5_checksum.txt or checksum_sha256.txt are
  # present in the zip file.

  # make the checksum filename easier to parse
  checksum_file_stripped <- checksum_file |>
    basename() |>
    tools::file_path_sans_ext() |>
    tolower()

  # Get checksum from NPX file
  if (file.exists(npx_file)) {

    if (grepl("md5", checksum_file_stripped)) {

      # MD5 checksum on the NPX file
      npx_file_checksum <- tools::md5sum(npx_file) |>
        unname()

    } else if (grepl("sha256", checksum_file_stripped)) {

      # SHA256 checksum on the NPX file
      npx_file_checksum <- openssl::sha256(file(npx_file)) |>
        stringr::str_replace(pattern = ":",
                             replacement = "")

    }

  } else {

    return(
      paste("The NPX file",
            basename(npx_file),
            "has not been extracted from the zip file")
    )

  }


  if (file.exists(checksum_file)) {

    # check that checksum matches NPX csv file
    checksum_file_read_con <- file(checksum_file, "r")

    # read in the checksum extracted from the compressed file
    checksum_file_content <- readLines(con = checksum_file_read_con,
                                       n = 1L,
                                       warn = FALSE)

    # clean up files
    close(checksum_file_read_con)

  } else {

    return(
      paste("The checksum file",
            basename(checksum_file),
            "has not been extracted from the zip file")
    )

  }

  # check if checksums match
  if (checksum_file_content != npx_file_checksum) {

    # not throwing the error directly from here because we have to clean up
    # the temporary directory in the read_npx_zip.
    return(
      paste("The checksum of the NPX file does not match the one included",
            "in the compressed file")
    )

  } else {

    return(NA_character_)

  }

}
