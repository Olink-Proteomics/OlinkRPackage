#' Help function to get the name of the NPX file from a compresseed NPX.
#'
#' @param files A vector of file names without any path prefix.
#'
#' @return The name of the NPX file.
#'
#' @keywords zip NPX
#'
get_npx_file <- function(files) {

  # check that the input is a character vector
  check_is_character(string = files,
                     error = TRUE)

  # remove (if any) checksum files
  files_no_checksum <- files[!(files %in% accepted_checksum_files)]

  # get file extension(s)
  df_files <- dplyr::tibble(files = files_no_checksum) |>
    dplyr::mutate(
      files_extension = tools::file_ext(.data[["files"]])
    )

  # check: no file with the accepted suffix
  if (sum(df_files$files_extension %in% accepted_npx_file_ext) != 1L) {

    if (sum(df_files$files_extension %in% accepted_npx_file_ext) == 0L) {
      err_msg <- "no" # nolint object_usage_linter
    } else if (sum(df_files$files_extension %in% accepted_npx_file_ext) > 1L) {
      err_msg <- "multiple" # nolint object_usage_linter
    }

    # we should not allow the zip extension in this case as the NPX file is
    # already part of the zip we are checking
    npx_ext_no_zip <- accepted_npx_file_ext[!(accepted_npx_file_ext == "zip")] # nolint object_usage_linter

    cli::cli_abort(
      c(
        "x" = "The compressed file contains {err_msg} NPX files!",
        "i" = "The compressed input file should contain {.strong only} one NPX
          file with extension: { glue::glue_collapse(x = npx_ext_no_zip,
                                                     sep = \", \",
                                                     last = \" or \") }."
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )
  }

  # get the NPX file
  npx_file <- df_files |>
    dplyr::filter(
      .data[["files_extension"]] %in% .env[["accepted_npx_file_ext"]]
    ) |>
    # we can safely assume that there is only one file here
    dplyr::slice_head(n = 1L) |>
    dplyr::pull(.data[["files"]])

  # return
  return(npx_file)
}
