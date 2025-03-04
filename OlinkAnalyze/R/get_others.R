#' Prints class type output from read_npx* functions.
#'
#' @return A scalar character vector with the class type of outputs from
#' read_npx* functions.
#'
get_df_output_print <- function() {
  stringr::str_replace_all(
    string = read_npx_df_output,
    pattern = "arrow",
    replacement = "ArrowObject"
  )
}

#' Describes acceptable file extension for each file type.
#'
#' @return A scalar character vector with one sentence describing the acceptable
#' extensions of each file type.
#'
get_file_ext_summary <- function() {
  sapply(
    get_file_formats(),
    function(ff) {
      paste0(
        get_file_ext(name_sub = ff) |> ansi_collapse_quot(sep = "or"),
        " for ", ff, " files"
      )

    }
  ) |>
    cli::ansi_collapse()
}

#' Get all acceptable file formats.
#'
#' @return A character vector with the acceptable file formats.
#'
get_file_formats <- function() {
  file_ext_nm <- names(accepted_npx_file_ext) |>
    strsplit(split = "_", fixed = TRUE) |>
    sapply(utils::head, 1L) |>
    unique()
  return(file_ext_nm)
}

#' Gets all file extensions based on the file format.
#'
#' @param name_sub Substring of file format. One of
#' `r ansi_collapse_quot(c(get_file_formats(), "NULL"))`. If `NULL` all
#' file extensions are returned.
#'
#' @return Character vector with accepted file extensions.
#'
get_file_ext <- function(name_sub = NULL) {

  if (!is.null(name_sub)) {

    check_is_scalar_character(string = name_sub,
                              error = TRUE)

    if (!(name_sub %in% get_file_formats())) {
      cli::cli_abort(
        c(
          "x" = "{.val {name_sub}} does not reflect an acceptable file format!",
          "i" = "Expected one of: {.val {get_file_formats()}}"
        ),
        call = rlang::caller_env(),
        wrap = FALSE
      )
    }

  } else {
    name_sub <- get_file_formats() |>
      paste(collapse = "|")
  }

  f_ext <- accepted_npx_file_ext[grepl(name_sub, names(accepted_npx_file_ext))]
  return(f_ext)
}
