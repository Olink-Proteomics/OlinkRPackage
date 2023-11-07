#' Helper function to read NPX zip-compressed files with checksum.
#'
#' @param file Path to Olink Software output zip file.
#' @param .ignore_files Vector of files to ignore.
#'
#' @return A "tibble" in long format.
#'
#' @importFrom utils zip
#'
read_npx_zip <-
  function(file,
           .ignore_files = c("README.txt")) {

    # **** Prep ****

    # check contents of the compressed file
    # keep all files but the README.txt
    compressed_file_contents <- utils::unzip(
      zipfile = file,
      list = TRUE
    ) |>
      dplyr::filter(!(.data[["Name"]] %in% .env[[".ignore_files"]])) |>
      dplyr::pull(.data[["Name"]])


    # check: files vector contains no entries
    if (length(compressed_file_contents) == 0L) {
      cli::cli_abort(
        c(
          "x" = "The compressed file does not contain checksum or NPX files!"
        ),
        call = NULL,
        wrap = FALSE
      )
    }

    # Get the name of the checksum file, if available
    compressed_file_chksm <- get_checksum_file_from_zip(
      files = compressed_file_contents
    )

    # Get the name of the NPX files
    compressed_file_npx <- get_npx_file_from_zip(
      files = compressed_file_contents
    )

    # Array of files to extract
    files_to_extract <- c(compressed_file_chksm,
                          compressed_file_npx)
    # Remove NA from checksum (if NA was returned)
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
    if (!is.na(compressed_file_chksm)) {

      # Extracted checksum file
      extracted_file_chksm <- file.path(tmp_unzip_dir,
                                        compressed_file_chksm)

      # make the checksum filename easier to parse
      chksm_string <- tools::file_path_sans_ext(compressed_file_chksm) |>
        tolower()

      if (grepl("md5", chksm_string)) {
        # MD5 checksum on the NPX file
        chksm_npx <- tools::md5sum(extracted_file_npx) |>
          unname()
      } else if(grepl("sha256", chksm_string)) {
        # SHA256 checksum on the NPX file
        chksm_npx <- openssl::sha256(file(extracted_file_npx)) |>
          stringr::str_replace(pattern = ":",
                               replacement = "")
      }

      # check that checksum matches NPX csv file
      extracted_file_chksm_con <- file(extracted_file_chksm, "r")

      # read in the checksum extracted from the compressed file
      chksm_extracted <- readLines(con = extracted_file_chksm_con,
                                   n = 1L,
                                   warn = FALSE)

      # clean up files
      close(extracted_file_chksm_con)

      # check if checksums match
      if (chksm_extracted != chksm_npx) {
        # cleanup temporary directory with extracted files
        invisible(
          unlink(x = tmp_unzip_dir,
                 recursive = TRUE)
        )

        # error message
        cli::cli_abort(
          c(
            "x" = "The checksum of the NPX file does not match that of the
                checksum file!",
            "i" = "Potential loss of data or corrupt file."
          ),
          call = NULL,
          wrap = FALSE
        )
      }
    }

    # read the NPX file
    df_npx <- read_npx_csv(
      filename = extracted_file_npx
    )

    # cleanup temporary directory with extracted files
    invisible(
      unlink(x = tmp_unzip_dir,
             recursive = TRUE)
    )

    return(df_npx)
  }
