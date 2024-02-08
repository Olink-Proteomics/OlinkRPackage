#' Help function checking for duplicate sample IDs in data
#'
#' @author Masoumeh Sheikhi
#'
#' @param df An arrow object containing the columns "SampleID" and "OlinkID".
#'
#' @return Invisible NULL.

check_duplicate_sample_ids <- function(df) {
  # check if it's an arrow object, stop if not
  check_is_arrow_object(df = df, error = TRUE)

  # required columns: OlinkID
  # replace this chunck with check_columns and dictionary functions
  # check column OlinkID exists
  olink_id <- "OlinkID"
  sample_id <- "SampleID"
  if (!olink_id %in% names(df)) {
    cli::cli_abort(
      c("x" = "The data frame {.arg {rlang::caller_arg(df)}} is missing the
        required column: {olinkid}!"),
      call = rlang::caller_env(),
      wrap = TRUE
    )
  }

  # check column sample_id exists
  if (!sample_id %in% names(df)) {
    cli::cli_abort(
      c("x" = "The data frame {.arg {rlang::caller_arg(df)}} is missing the
        required column: {sample_id}!"),
      call = rlang::caller_env(),
      wrap = TRUE
    )
  }

  # Select relevant columns
  sample_summary <- df  |>
    dplyr::select(dplyr::all_of(c(sample_id, olink_id)))  |>
    dplyr::group_by(.data[[sample_id]], .data[[olink_id]]) |>
    dplyr::summarise(freq = dplyr::n(), .groups = "drop") |>
    dplyr::collect()

  # Find duplicates
  duplicates <- character(0L)
  duplicates <- sample_summary |>
    dplyr::filter(freq > 1) |>
    dplyr::pull(.data[[sample_id]])

  # Warn if duplicates are found
  if (length(duplicates) > 0L) {
    cli::cli_warn(c(
      "x" = "Duplicate sample ID{?s} detected:
      {duplicates}"))
  }
  return(invisible(NULL))
}
