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
#' @param .ignore_files Vector of files to ignore.
#'
#' @return An R6 class ArrowObject.
#'
#' @keywords NPX csv txt delim sep parquet
#'
#' @seealso
#'   [read_npx()]
#'   [read_npx_delim()]
#'   [read_npx_parquet()]
#'
read_npx_zip <-
  function(file,
           out_df = "arrow",
           sep = NULL,
           .ignore_files = c("README.txt")) {

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
            "x" = "Unable to open compressed file: {file}",
            "i" = "Check if the file is a zip and or potential file
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
          "x" = "No NPX and checksum file in the compressed file: {file}"
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
      files = compressed_file_contents
    )

    # Array of files to extract
    files_to_extract <- c(compressed_file_checksum,
                          compressed_file_npx)
    # Remove NA strings (if NA was returned from checksum function)
    files_to_extract <- files_to_extract[!is.na(files_to_extract)]

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
      sep = sep
    )

    # cleanup temporary directory with extracted files
    invisible(unlink(x = tmp_unzip_dir, recursive = TRUE))

    # if needed convert the object to the requested output
    df_olink <- convert_read_npx_output(df = df_olink,
                                        out_df = out_df)

    return(df_olink)
  }
