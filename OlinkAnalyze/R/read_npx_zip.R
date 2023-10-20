#' Helper function to read in Olink Explore zip csv files
#'
#' @param filename Path to Olink Software output zip file.
#'
#' @return A "tibble" in long format. Some of the columns are:
#' \itemize{
#'    \item{SampleID:} Sample ID
#'    \item{Index:} Index
#'    \item{OlinkID:} Olink ID
#'    \item{UniProt:} UniProt ID
#'    \item{Assay:} Protein symbol
#'    \item{MissingFreq:} Proportion of sample below LOD
#'    \item{Panel_Version:} Panel Version
#'    \item{PlateID:} Plate ID
#'    \item{QC_Warning:} QC Warning Status
#'    \item{LOD:} Limit of detection
#'    \item{NPX:} Normalized Protein Expression
#' }
#' Additional columns may be present or missing depending on the platform
#'
#' @keywords NPX
#'
#' @examples
#' \donttest{
#' file <- system.file("extdata", "Example_NPX_Data.csv", package = "OlinkAnalyze")
#' read_NPX(file)
#' }
#'
#' @importFrom tools file_ext md5sum file_path_sans_ext
#' @importFrom dplyr pull filter
#' @importFrom stringr str_detect str_replace
#' @importFrom zip unzip
#'

read_npx_zip <- function(filename) {

  # **** Prep ****

  # check contents of the compressed file
  # keep all files but the README.txt
  compressed_file_contents <- utils::unzip(zipfile = filename,
                                           list = TRUE) |>
    dplyr::filter(Name != "README.txt") |>
    dplyr::pull(Name)

  # Check if checksum file (MD5 or SHA256) exists and prepare list of files to unzip
  if ("MD5_checksum.txt" %in% compressed_file_contents) {

    compressed_file_chksm <- compressed_file_contents[compressed_file_contents == "MD5_checksum.txt"] # md5 txt file
    compressed_file_csv   <- compressed_file_contents[compressed_file_contents != "MD5_checksum.txt"] # NPX csv file
    files_to_extract      <- c(compressed_file_csv, compressed_file_chksm) # array of files to extract

  } else if("checksum_sha256.txt" %in% compressed_file_contents) {

    compressed_file_chksm <- compressed_file_contents[compressed_file_contents == "checksum_sha256.txt"] # sha256 txt file
    compressed_file_csv   <- compressed_file_contents[compressed_file_contents != "checksum_sha256.txt"] # NPX csv file
    files_to_extract      <- c(compressed_file_csv, compressed_file_chksm) # array of files to extract

  } else {

    compressed_file_chksm <- NA_character_
    compressed_file_csv   <- compressed_file_contents
    files_to_extract      <- compressed_file_csv

  }

  # Make sure that there is only one NPX file
  # Make sure that the files to extract have *.csv and *.txt extnsions
  if (length(compressed_file_csv) != 1 ||
      !(tools::file_ext(compressed_file_csv) %in% c("csv", "txt"))) {
    stop("The compressed file does not contain a valid NPX file. Expecting: \"README.txt\", \"MD5_checksum.txt\" or \"checksum_sha256.txt\" and the NPX file.")
  }

  # **** Extract ****

  # temporary directory to extract
  tmp_unzip_dir <- tempfile()

  zip::unzip(zipfile = filename,
             files = files_to_extract,
             exdir = tmp_unzip_dir,
             overwrite = TRUE)

  # Extracted NPX csv file
  extracted_file_csv <- file.path(tmp_unzip_dir, compressed_file_csv)

  # **** Checksum ****

  # Checksum of the NPX file
  if (!is.na(compressed_file_chksm)) {

    # Extracted checksum file
    extracted_file_chksm <- file.path(tmp_unzip_dir, compressed_file_chksm) # MD5 relative path

    # make the checksum filename easier to parse
    chksm_string <- tolower(tools::file_path_sans_ext(compressed_file_chksm))

    # check that checksum matches NPX csv file
    extracted_file_chksm_con <- file(extracted_file_chksm, "r")

    if (stringr::str_detect(chksm_string, "md5")) {

      chksm <- tools::md5sum(extracted_file_csv) |>
        unname()

    } else if(stringr::str_detect(chksm_string, "sha256")) {

      chksm <- openssl::sha256(file(extracted_file_csv)) |>
        stringr::str_replace(pattern = ":",
                             replacement = "")

    }

    if (readLines(con = extracted_file_chksm_con,
                  n = 1L,
                  warn = FALSE) != chksm) { # check if checksum matches

      # clean up files
      close(extracted_file_chksm_con)
      invisible(
        unlink(x = tmp_unzip_dir,
               recursive = TRUE)
      )
      stop(paste("Checksum of NPX file does not match the one from \"",
                 compressed_file_chksm, "\"! Loss of data?", sep = ""))

    }

    close(extracted_file_chksm_con)

  }

  df_npx <- read_npx_csv(
    filename = extracted_file_csv
  )

  # cleanup temporary directory with extracted files
  invisible(
    unlink(x = tmp_unzip_dir,
           recursive = TRUE)
  ) # remove files

  return(df_npx)
}
