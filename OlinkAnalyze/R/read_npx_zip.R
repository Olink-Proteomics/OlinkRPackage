#' Help function to read NPX, Ct or absolute quantification data from
#' zip-compressed Olink software output files in R.
#'
#' @description
#' A zip-compressed input file might contain a file from the Olink software
#' containing NPX, Ct or absolute quantification data, a checksum file and one
#' or more files to be ignored.
#'
#' \strong{Note:} The zip-compressed file should strictly contain \strong{one}
#' Olink data file, \strong{none or one} checksum file and
#' \strong{none or one, or more} files that might be ignored.
#'
#' \itemize{
#' \item \strong{Olink file} exported by Olink software in wide or long format.
#' Expecting file extensions `csv`, `txt`, `xls`, `xlsx`, `parquet` or `zip`.
#' This file is subsequently provided as input to \code{\link{read_npx}}.
#' \item \strong{checksum file} named `MD5_checksum.txt` or`checksum_sha256.txt`
#' depending on the checksum algorithm. The file contains only one line with the
#' checksum string of characters.
#' \item \strong{File(s) to be ignored} from the zip file. These files can be
#' named as a character vector in the argument \var{.ignore_files}.
#' }
#'
#' @author
#'   Klev Diamanti;
#'   Kathleen Nevola;
#'   Pascal Pucholt
#'
#' @param file Path to Olink software output zip-compressed file in wide or long
#' format. Expecting file extension `zip`.
#' @param out_df The class of output data frame. One of `tibble` (default) or
#' `arrow` for ArrowObject.
#' @param sep Character separator of delimited input file. One of `NULL` for
#' auto-detection (default), `,` for comma or `;` for semicolon. Used only for
#' delimited output files from Olink software.
#' @param long_format Boolean marking format of input file. One of `NULL`
#' (default) for auto-detection, `TRUE` for long format files or `FALSE` for
#' wide format files.
#' @param olink_platform Olink platform used to generate the input file.
#' One of `NULL` (default), `Explore 3072`, `Explore HT`, `Target 96`,
#' `Target 48`, `Flex` or `Focus`.
#' @param data_type Quantification method of the input data. One of `NULL`
#' (default), `NPX`, `Quantified` or `Ct`.
#' @param .ignore_files Character vector of files included in the zip-compressed
#' Olink software output files that should be ignored. Applies only to
#' zip-compressed input files. `c("README.txt")` (default).
#' @param quiet Boolean to print a confirmation message when reading the input
#' file. Applies to excel or delimited input only. `TRUE` (default) to not print
#' and `FALSE` to print.
#'
#' @return Tibble or ArrowObject with Olink data in long format.
#'
#' @seealso
#'   \code{\link{read_npx}}
#'   \code{\link{read_npx_parquet}}
#'   \code{\link{read_npx_excel}}
#'   \code{\link{read_npx_format}}
#'   \code{\link{read_npx_delim}}
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

  excl_file_ext <- c("zip")

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
    excl_file_ext = excl_file_ext
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

  # cleanup temporary directory with extracted files after exiting the function
  on.exit(
    expr = invisible(unlink(x = tmp_unzip_dir, recursive = TRUE)),
    add = TRUE
  )

  # **** Checksum ****

  # Checksum of the NPX file
  if (!is.na(compressed_file_checksum)) {

    # Extracted checksum file
    extracted_file_chksm <- file.path(tmp_unzip_dir, compressed_file_checksum)

    # confirm that the checksum file is available
    check_checksum(
      checksum_file = extracted_file_chksm,
      npx_file = extracted_file_npx
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

  # if needed convert the object to the requested output
  df_olink <- convert_read_npx_output(df = df_olink,
                                      out_df = out_df)

  return(df_olink)
}

#' Help function to get the file name of the checksum file from the list of
#' contents of a zip-compressed file.
#'
#' @author
#'   Klev Diamanti
#'
#' @param files A character vector listing file names included in the
#' zip-compressed input file.
#'
#' @return The file name of the checksum file, or NA if file is absent.
#'
#' @seealso
#'   \code{\link{read_npx_zip}}
#'   \code{\link{get_npx_file}}
#'   \code{\link{check_checksum}}
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


#' Help function to get the file name of the Olink data file from the list of
#' contents of a zip-compressed file.
#'
#' @author
#'   Klev Diamanti
#'
#' @param files A character vector listing file names included in the
#' zip-compressed input file.
#' @param excl_file_ext Character vector of file extensions that should not be
#' considered as Olink data file. Mainly used to avoid nested compressed files.
#'
#' @return The file name of the Olink data file.
#'
#' @seealso
#'   \code{\link{read_npx_zip}}
#'   \code{\link{get_checksum_file}}
#'   \code{\link{check_checksum}}
#'
get_npx_file <- function(files,
                         excl_file_ext = c("zip")) {

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
      accepted_npx_file_ext[!(accepted_npx_file_ext %in% excl_file_ext)],
        last = \", or \")}."
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  } else if (nrow(df_files) == 1L
             && df_files$files_extension %in% excl_file_ext) {

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
#' checksum of the Olink data file from the input zip-compressed file.
#'
#' @description
#' Runs only if `MD5_checksum.txt` or `checksum_sha256.txt` are present in the
#' input zip-compressed file. This function does not check if the
#' \var{checksum_file} is in acceptable format.
#'
#' @author
#'   Klev Diamanti
#'
#' @param checksum_file Extracted checksum file from the zip-compressed file
#' that contains the checksum file from Olink software.
#' @param npx_file Extracted file from the zip-compressed file that contains the
#' Olink data file from Olink software.
#'
#' @return NULL or an error if the files could not be opened or if checksum did
#' not match.
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
