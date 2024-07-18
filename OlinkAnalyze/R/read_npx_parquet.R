#' Help function to read NPX data from long format parquet Olink software output
#' files in R.
#'
#' @author
#'   Klev Diamanti;
#'   Kathleen Nevola;
#'   Pascal Pucholt
#'
#' @param file Path to Olink software output parquet file in long format.
#' Expecting file extension
#' `r cli::ansi_collapse(x = accepted_npx_file_ext[grepl("parquet", names(accepted_npx_file_ext))], sep = ", ", last = " or ")`. # nolint
#' @param out_df The class of output data frame. One of "tibble" (default) or
#' "arrow" for ArrowObject.
#'
#' @return Tibble or ArrowObject with Olink data in long format.
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

      cli::cli_abort(
        c(
          "x" = "Unable to read parquet file: {.file {file}}",
          "i" = "Check the file is in parquet format and potential file
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
    missing_fields <- olink_parquet_spec$parquet_metadata[ # nolint
      !(olink_parquet_spec$parquet_metadata %in% names(df_olink$metadata))
    ]

    cli::cli_abort(
      c(
        "x" = "Missing required field{?s} in metadata:
        {missing_fields}"
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

  if (!(df_olink$metadata[[olink_parquet_product]] %in% olink_parquet_spec$parquet_platforms)) { # nolint

    cli::cli_abort(
      c(
        "x" = "Unsupported product:
        {.val {df_olink$metadata[[olink_parquet_product]]}}",
        "i" = "Metadata field {.val {olink_parquet_product}} expects:
        {olink_parquet_spec$parquet_platforms}"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

  # Data file type
  olink_parquet_files <- olink_parquet_spec$parquet_metadata[
    names(olink_parquet_spec$parquet_metadata) == "data_file_type"
  ]

  if (!(df_olink$metadata[[olink_parquet_files]] %in% olink_parquet_spec$parquet_files)) { # nolint

    cli::cli_abort(
      c(
        "x" = "Unsupported file:
        {.val {df_olink$metadata[[olink_parquet_files]]}}",
        "i" = "Metadata field {.val {olink_parquet_files}} expects:
        {olink_parquet_spec$parquet_files}"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

  # Return ----

  # if needed convert the object to the requested output
  df_olink <- convert_read_npx_output(df = df_olink,
                                      out_df = out_df)

  return(df_olink)
}
