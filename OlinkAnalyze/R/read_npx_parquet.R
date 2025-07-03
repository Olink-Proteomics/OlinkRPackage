#' Help function to read NPX data from long format parquet Olink software output
#' file in R.
#'
#' @author
#'   Klev Diamanti;
#'   Kathleen Nevola;
#'   Pascal Pucholt
#'
#' @param file Path to Olink software output parquet file in long format.
#' Expecting file extension
#' `r ansi_collapse_quot(get_file_ext(name_sub = "parquet"))`.
#' @param out_df The class of the output dataset. One of
#' `r ansi_collapse_quot(read_npx_df_output)`. (default = "tibble")
#'
#' @return `r ansi_collapse_quot(x = get_df_output_print(), sep = "or")` with
#' Olink data in long or wide format.
#'
#' @seealso
#'   \code{\link{read_npx}}
#'   \code{\link{read_npx_zip}}
#'   \code{\link{read_npx_excel}}
#'   \code{\link{read_npx_format}}
#'   \code{\link{read_npx_delim}}
#'
read_npx_parquet <- function(file,
                             out_df = "arrow") {

  # Read parquet file ----

  # check if file exists
  check_file_exists(file = file,
                    error = TRUE)

  # check that the requested output df is ok
  check_out_df_arg(out_df = out_df)

  # tryCatch in case reading the file fails
  tryCatch(
    {

      df_olink <- arrow::open_dataset(
        sources = file
      )

    }, error = function(msg) {

      cli::cli_abort( # nolint return_linter
        c(
          "x" = "Unable to read parquet file: {.file {file}}",
          "i" = "Check if the file is in parquet format and/or potential file
          corruption."
        ),
        call = rlang::caller_env(),
        wrap = FALSE
      )

    }
  )

  # Check metadata ----

  # Check that all required parquet metadata is in place
  if (!all(olink_parquet_spec$parquet_metadata %in% names(df_olink$metadata))) {
    missing_fields <- olink_parquet_spec$parquet_metadata[ # nolint object_usage_linter
      !(olink_parquet_spec$parquet_metadata %in% names(df_olink$metadata))
    ]

    cli::cli_abort(
      c(
        "x" = "Missing required field{?s} in the metadata of the parquet file:
        {.val {missing_fields}}"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

  # Check specific fields of metadata ----

  # Product
  olink_parquet_product <- olink_parquet_spec$parquet_metadata[
    names(olink_parquet_spec$parquet_metadata) == "product"
  ]

  if (!(df_olink$metadata[[olink_parquet_product]] %in% olink_parquet_spec$parquet_platforms)) { # nolint line_length_linter

    cli::cli_abort(
      c(
        "x" = "Unsupported product:
        {.val {df_olink$metadata[[olink_parquet_product]]}}",
        "i" = "Metadata field {.val {olink_parquet_product}} expects one of:
        {.val {olink_parquet_spec$parquet_platforms}}."
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

  # Data file type
  olink_parquet_files <- olink_parquet_spec$parquet_metadata[
    names(olink_parquet_spec$parquet_metadata) == "data_file_type"
  ]

  if (!(df_olink$metadata[[olink_parquet_files]] %in% olink_parquet_spec$parquet_files)) { # nolint line_length_linter

    cli::cli_abort(
      c(
        "x" = "Unsupported file:
        {.val {df_olink$metadata[[olink_parquet_files]]}}",
        "i" = "Metadata field {.val {olink_parquet_files}} expects one of:
        {.val {olink_parquet_spec$parquet_files}}."
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

  # Research use only
  olink_ruo <- olink_parquet_spec$optional_metadata[
    names(olink_parquet_spec$optional_metadata) == "ruo"
  ] |>
    unname()

  if (olink_ruo %in% names(df_olink$metadata)) {

    cli::cli_alert_info(
      "This parquet file is for research use only:
      {.val {df_olink$metadata[[olink_ruo]]}}!"
    )

  }

  # Return ----

  # if needed convert the object to the requested output
  df_olink <- convert_read_npx_output(
    df = df_olink,
    out_df = out_df
  )

  return(df_olink)
}
