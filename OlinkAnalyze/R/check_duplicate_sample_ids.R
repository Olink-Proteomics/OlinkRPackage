#' Help function checking for duplicate sample IDs in data
#'
#' @author Masoumeh Sheikhi
#'
#' @param df An arrow object containing the columns "SampleID" and "OlinkID".
#' @param name_mask A list of matched column names. This is the output of `check_npx_col_names` function.
#'
#' @return Invisible NULL.

### Is it necessary to return anything here?

check_duplicate_sample_ids <- function(df, name_mask) {

  # required columns: olink_id and sample_id
  check_columns(df,
                col_list = list(name_mask$sample_id,
                                name_mask$olink_id))


  # replace this chunck with check_columns and dictionary functions
  # check column OlinkID exists
  # olink_id <- "OlinkID"
  # sample_id <- "SampleID"
  # if (!olink_id %in% names(df)) {
  #   cli::cli_abort(
  #     c("x" = "The data frame {.arg {rlang::caller_arg(df)}} is missing the
  #       required column: {olinkid}!"),
  #     call = rlang::caller_env(),
  #     wrap = TRUE
  #   )
  # }

  # # check column sample_id exists
  # if (!sample_id %in% names(df)) {
  #   cli::cli_abort(
  #     c("x" = "The data frame {.arg {rlang::caller_arg(df)}} is missing the
  #       required column: {sample_id}!"),
  #     call = rlang::caller_env(),
  #     wrap = TRUE
  #   )
  # }
  #


  ### thoughts ###
  # i think it's best if we use check_is_arrow_object and check_columns once
  # in the check_npx wrap function.
  # for check_columns, we can check all the columns necessary
  # for the subsequent help functions at once.


  # Select relevant columns
  sample_summary <- df  |>
    dplyr::select(dplyr::all_of(c(
      name_mask$sample_id,
      name_mask$olink_id)))  |>
    dplyr::group_by(.data[[name_mask$sample_id]], .data[[name_mask$olink_id]]) |>
    dplyr::summarise(freq = dplyr::n(), .groups = "drop") |>
    dplyr::collect()

  # Find duplicates
  duplicates <- character(0L)
  duplicates <- sample_summary |>
    dplyr::filter(freq > 1) |>
    dplyr::pull(.data[[name_mask$sample_id]])

  # Warn if duplicates are found
  if (length(duplicates) > 0L) {
    cli::cli_warn(c(
      "x" = "Duplicate sample ID{?s} detected:
      {duplicates}"))
  }
  return(invisible(NULL))
}
