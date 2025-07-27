#' Prints class type output from read_npx* functions.
#'
#' @author
#'   Klev Diamanti
#'
#' @return A scalar character vector with the class type of outputs from
#' read_npx* functions.
#'
get_df_output_print <- function() {
  x <- stringr::str_replace_all(
    string = read_npx_df_output,
    pattern = "arrow",
    replacement = "ArrowObject"
  )
  return(x)
}

#' Describes acceptable file extension for each file type.
#'
#' @author
#'   Klev Diamanti
#'
#' @return A scalar character vector with one sentence describing the acceptable
#' extensions of each file type.
#'
get_file_ext_summary <- function() {
  x <- sapply(
    get_file_formats(),
    function(ff) {
      paste0( # nolint return_linter
        get_file_ext(name_sub = ff) |> ansi_collapse_quot(sep = "or"),
        " for ", ff, " files"
      )

    }
  ) |>
    cli::ansi_collapse()
  return(x)
}

#' Get all acceptable file formats.
#'
#' @author
#'   Klev Diamanti
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
#' @author
#'   Klev Diamanti
#'
#' @param name_sub Substring of file format. One of
#' `r ansi_collapse_quot(c(get_file_formats(), "NULL"))`. If `NULL` all
#' file extensions are returned.
#'
#' @return Character vector with accepted file extensions.
#'
get_file_ext <- function(name_sub = NULL) {

  if (!is.null(name_sub)) {

    check_is_character(string = name_sub,
                       error = TRUE)

    if (!all(name_sub %in% get_file_formats())) {
      no_overlap <- name_sub[!(name_sub %in% get_file_formats())] # nolint object_usage_linter
      cli::cli_abort(
        c(
          "x" = "{.val {no_overlap}} {?does/do} not reflect an acceptable file
          format!",
          "i" = "Expected one of: {.val {get_file_formats()}}"
        ),
        call = rlang::caller_env(),
        wrap = FALSE
      )
    }

    if (length(name_sub) > 1L) {
      name_sub <- paste(name_sub, collapse = "|")
    }

  } else {
    name_sub <- get_file_formats() |>
      paste(collapse = "|")
  }

  f_ext <- accepted_npx_file_ext[grepl(name_sub, names(accepted_npx_file_ext))]
  return(f_ext)
}
