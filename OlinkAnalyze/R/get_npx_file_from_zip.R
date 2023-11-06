#' Helper function to get the name of the NPX file from a zip NPX.
#'
#' @param files A vector of file names without any path prefix.
#' @param accepted_npx_files Vector of accepted extensions of NPX files.
#'
#' @return The name of the NPX file.
#'
get_npx_file_from_zip <-
  function(files,
           accepted_npx_files = c("xls",
                                  "xlsx",
                                  "csv",
                                  "txt",
                                  "parquet")) {

    # remove (if any) checksum files
    files_no_checksum <- files[!grepl("(MD5|sha256).*\\.txt$", files)]

    # get file extension(s)
    df_files <- dplyr::tibble(files = files_no_checksum) |>
      dplyr::mutate(
        files_extension = tools::file_ext(.data[["files"]])
      )

    # check: no file with the accepted suffix
    if (sum(df_files$files_extension %in% accepted_npx_files) != 1) {
      cli::cli_abort(
        c(
          "x" = "Unknown NPX files!",
          "i" = "The input *.zip file should contain {.strong only} one NPX
          file with extsnsion: { glue::glue_collapse(x = accepted_npx_files,
                                                     sep = \", \",
                                                     last = \" or \") }."
        ),
        call = NULL,
        wrap = FALSE
      )
    }

    # get the NPX file
    npx_file <- files_no_checksum[files_no_checksum %in% accepted_npx_files]

    # return
    return(npx_file)
  }
