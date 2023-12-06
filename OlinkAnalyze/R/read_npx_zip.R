#' Helper function to read NPX zip-compressed files with checksum.
#'
#' @author
#'   Klev Diamanti;
#'   Kathleen Nevola;
#'   Pascal Pucholt
#'
#' @param file Path to Olink Software output zip file.
#' @param out_df The class of output data frame to be returned. Accepted values
#' are "tibble" and "arrow" (default).
#' @param sep The separator of the delimited file: NULL (autodetect), comma (,)
#' or semicolon (;). Used only for delimited Olink software output files.
#' @param long_format Boolean marking if input file is in long (TRUE) or wide
#' (FALSE) format. Ignored for non-excel input files.
#' @param olink_platform The Olink platform used to generate the input file.
#' Expecting "Target 96", "Target 48" or "Flex". Ignored for non-excel input
#' files.
#' @param data_type The quantification in which the data comes in, Expecting on
#' of NPX, Quantified or Ct. Ignored for non-excel input files.
#' @param .ignore_files Vector of files to ignore.
#' @param quiet Print a confirmation message after reading in the input file.
#'
#' @return An R6 class ArrowObject.
#'
#' @keywords NPX csv txt delim sep parquet
#'
#' @seealso
#'   [read_npx()]
#'   [read_npx_delim()]
#'   [read_npx_parquet()]
#'   [read_npx_excel()]
#'
read_npx_zip <- function(file,
                         out_df = "arrow",
                         sep = NULL,
                         long_format = NULL,
                         olink_platform = NULL,
                         data_type = NULL,
                         .ignore_files = c("README.txt"),
                         quiet = FALSE) {

  # Check if all required libraries for this function are installed
  check_library_installed(
    libraries = c("zip"),
    error = TRUE
  )

  # check if file exists
  check_file_exists(file = file,
                    error = TRUE)

  # check that the requested output df is ok
  check_out_df_arg(out_df = out_df)

  # check that .ignore_files is a character vector
  check_is_character(string = .ignore_files,
                     error = TRUE)

  # **** Help vars ----

  compressed_file_ext <- c("zip")

  # **** Prep ****

  # tryCatch in case reading the zip file fails
  tryCatch(
    {

      compressed_file_contents <- utils::unzip(
        zipfile = file,
        list = TRUE
      )

    }, error = function(msg) {

      cli::cli_abort(
        c(
          "x" = "Unable to open compressed file: {.file {file}}",
          "i" = "Check if the file is a zip and/or potential file
          corruption."
        ),
        call = rlang::caller_env(),
        wrap = FALSE
      )

    }
  )

  # check contents of the compressed file
  # keep all files but the README.txt
  compressed_file_contents <- compressed_file_contents |>
    dplyr::filter(!(.data[["Name"]] %in% .env[[".ignore_files"]])) |>
    dplyr::pull(.data[["Name"]])

  # check: files vector contains no entries
  if (length(compressed_file_contents) == 0L) {

    cli::cli_abort(
      c(
        "x" = "No NPX and checksum file in the compressed file: {.file {file}}"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

  # Get the name of the checksum file, if available
  compressed_file_checksum <- get_checksum_file(
    files = compressed_file_contents
  )

  # Get the name of the NPX files
  compressed_file_npx <- get_npx_file(
    files = compressed_file_contents,
    compressed_file_ext = compressed_file_ext
  )

  # Array of files to extract
  files_to_extract <- c(compressed_file_checksum,
                        compressed_file_npx) |>
    # Remove NA strings (if NA was returned from checksum function)
    (\(x) x[!is.na(x)])()

  # **** Extract ****

  # temporary directory to extract
  tmp_unzip_dir <- tempfile()

  zip::unzip(zipfile = file,
             files = files_to_extract,
             exdir = tmp_unzip_dir,
             overwrite = TRUE)

  # Extracted NPX csv file
  extracted_file_npx <- file.path(tmp_unzip_dir, compressed_file_npx)

  # **** Checksum ****

  # Checksum of the NPX file
  if (!is.na(compressed_file_checksum)) {

    # Extracted checksum file
    extracted_file_chksm <- file.path(tmp_unzip_dir, compressed_file_checksum)

    # tryCatch in case of errors from the check_checksum
    tryCatch(
      {

        # confirm that the checksum file is available
        check_checksum(
          checksum_file = extracted_file_chksm,
          npx_file = extracted_file_npx
        )

      }, error = function(msg) {

        # cleanup temporary directory with extracted files
        invisible(unlink(x = tmp_unzip_dir, recursive = TRUE))

        # throw the error message from the helper function
        cli::cli_abort(
          c(
            "x" = "{msg$message[1]}",
            "i" = "{msg$body[1]}"
          ),
          call = rlang::caller_env(),
          wrap = FALSE
        )

      }
    )

  }

  # read the NPX file
  df_olink <- read_npx(
    filename = extracted_file_npx,
    out_df = out_df,
    sep = sep,
    long_format = long_format,
    olink_platform = olink_platform,
    data_type = data_type,
    .ignore_files = .ignore_files,
    quiet = quiet
  )

  # cleanup temporary directory with extracted files
  invisible(unlink(x = tmp_unzip_dir, recursive = TRUE))

  # if needed convert the object to the requested output
  df_olink <- convert_read_npx_output(df = df_olink,
                                      out_df = out_df)

  return(df_olink)
}

#' Help function to get the name of the checksum file from a zip NPX.
#'
#' @author Klev Diamanti
#'
#' @param files A vector of file names without any path prefix.
#'
#' @return The name of checksum file or NA if the file is not present.
#'
#' @keywords zip NPX checksum MD5 SHA256
#'
#' @seealso
#'   [check_checksum()]
#'   [get_npx_file()]
#'
get_checksum_file <- function(files) {

  # check that the input is a character vector
  check_is_character(string = files,
                     error = TRUE)

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
          checksum file: { cli::ansi_collapse(x = accepted_checksum_files,
                                              last = \", or \") }."
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

  # get the checksum file
  checksum_file <- files[files %in% accepted_checksum_files]

  # return
  return(checksum_file)
}


#' Help function to get the name of the NPX file from a compresseed NPX.
#'
#' @author Klev Diamanti
#'
#' @param files A vector of file names without any path prefix.
#' @param compressed_file_ext Character vector of file extensions for
#' compressed files.
#'
#' @return The name of the NPX file.
#'
#' @keywords zip NPX
#'
#' @seealso
#'   [check_checksum()]
#'   [get_checksum_file()]
#'
get_npx_file <- function(files,
                         compressed_file_ext = c("zip")) {

  # check that the input is a character vector
  check_is_character(string = files,
                     error = TRUE)

  # remove (if any) checksum files
  files_no_checksum <- files[!(files %in% accepted_checksum_files)]

  # get file extension(s) and keep only those matching the accepted ones
  df_files <- dplyr::tibble(files = files_no_checksum) |>
    dplyr::mutate(
      files_extension = tools::file_ext(.data[["files"]])
    ) |>
    dplyr::filter(
      .data[["files_extension"]] %in% .env[["accepted_npx_file_ext"]]
    )

  # check: no file with the accepted suffix
  if (nrow(df_files) != 1L) {

    # we should not allow the zip extension in this case as the NPX file is
    # already part of the zip we are checking
    cli::cli_abort(
      c(
        "x" = "The compressed file contains
        {ifelse(nrow(df_files) == 0L, \"no\", \"multiple\")}
        acceptable  files!",
        "i" = "The compressed input file should contain {.strong only} one
        file with extension:
        {cli::ansi_collapse(x =
      accepted_npx_file_ext[!(accepted_npx_file_ext %in% compressed_file_ext)],
        last = \", or \")}."
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  } else if (nrow(df_files) == 1L
             & df_files$files_extension %in% compressed_file_ext) {

    cli::cli_abort(
      c(
        "x" = "The compressed file contains another compressed file:
        {df_files$files}!",
        "i" = "Nested compressed files are not allowed to avoid infinite loops"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

  # get the NPX file
  npx_file <- df_files |>
    # we can safely assume that there is only one file here
    dplyr::slice_head(n = 1L) |>
    dplyr::pull(.data[["files"]])

  # return
  return(npx_file)
}


#' Help function comparing the checksum reported by Olink software to the
#' checksum of the delivered NPX file.
#'
#' @description
#' We should make it here only if MD5_checksum.txt or checksum_sha256.txt are
#' present in the zip file.
#'
#' This function does not check whether checksum_file is in acceptable format.
#'
#' @author Klev Diamanti
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

  # check if input is character vectors of length 1
  check_is_scalar_character(string = checksum_file,
                            error = TRUE)
  check_is_scalar_character(string = npx_file,
                            error = TRUE)

  # make the checksum filename easier to parse
  checksum_file_stripped <- checksum_file |>
    basename() |>
    tools::file_path_sans_ext() |>
    tolower()

  # Get checksum from NPX file
  if (check_file_exists(file = npx_file, error = FALSE)) {

    if (grepl("md5", checksum_file_stripped)) {

      # MD5 checksum on the NPX file
      npx_file_checksum <- cli::hash_file_md5(paths = npx_file)

    } else if (grepl("sha256", checksum_file_stripped)) {

      # SHA256 checksum on the NPX file
      npx_file_checksum <- cli::hash_file_sha256(paths = npx_file)

    }

  } else {

    # error message
    cli::cli_abort(
      c(
        "x" = "Unable to open NPX file: {.file {basename(npx_file)}}",
        "i" = "Was it extracted from the compressed file?"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }


  if (check_file_exists(file = checksum_file, error = FALSE)) {

    # check that checksum matches NPX csv file
    checksum_file_read_con <- file(checksum_file, "r")

    # read in the checksum extracted from the compressed file
    checksum_file_content <- readLines(con = checksum_file_read_con,
                                       n = 1L,
                                       warn = FALSE)

    # clean up files
    close(checksum_file_read_con)

  } else {

    # error message
    cli::cli_abort(
      c(
        "x" = "Unable to open checksum file: {.file {basename(checksum_file)}}",
        "i" = "Was it extracted from the compressed file?"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

  # check if checksums match
  if (checksum_file_content != npx_file_checksum) {

    # error message
    cli::cli_abort(
      c(
        "x" = "The checksum of the NPX file does not match the one included in
        the compressed file",
        "i" = "Potential loss of data or corrupt file."
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

}
