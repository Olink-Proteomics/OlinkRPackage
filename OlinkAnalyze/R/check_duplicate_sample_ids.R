#' Help function checking for duplicate sample IDs in data
#'
#' @author Masoumeh Sheikhi
#' @param df An arrow object containing the columns "SampleID" and "OlinkID".
#' @return Invisible NULL.

check_duplicate_sample_ids <- function(df) {
  # check if it's an arrow object, stop if not
  check_is_arrow_object(df = df, error = TRUE)

  # Select relevant columns
  selected_columns <- df  |>
    dplyr::select(SampleID, OlinkID)  |>
    dplyr::collect()

  # remove df, Q: does it matter rm or not?
  rm(df)

  # Find duplicates
  duplicates <- selected_columns |>
    duplicated()

  # Warn if duplicates are found
  if (any(duplicates)) {
    duplicate_samples <- unique(selected_columns$SampleID[duplicates]) # nolint object_usage_linter
    cli::cli_warn(c(
      "x" = "Duplicate sample ID{?s} detected:
      {duplicate_samples}"))
  }
  return(invisible(NULL))
}
