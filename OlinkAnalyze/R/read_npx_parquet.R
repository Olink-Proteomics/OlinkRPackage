#' Help function to read Olink parquet files.
#'
#' @author
#'   Klev Diamanti;
#'   Kathleen Nevola;
#'   Pascal Pucholt
#'
#' @param file Path to Olink Software parquet output file.
#' @param out_df The class of output data frame to be returned. Accepted values
#' are "tibble" and "arrow" (default).
#'
#' @return An R6 class ArrowObject.
#'
#' @keywords NPX parquet
#'
#' @seealso
#'   [read_npx()]
#'   [read_npx_delim()]
#'   [read_npx_zip()]
#'   [read_npx_excel()]
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
          "x" = "Unable to read parquet file: {file}",
          "i" = "Check the file is in parquet format and potential file
          corruption."
        ),
        call = rlang::caller_env(),
        wrap = FALSE
      )

    }
  )

  # Check metadata ----

  olink_parquet_metadata <- list(
    file_version = "FileVersion",
    project_name = "ProjectName",
    sample_matrix = "SampleMatrix",
    product = "Product",
    data_file_type = "DataFileType"
  )

  # Check that all required parquet metadata is in place
  if (!all(unlist(olink_parquet_metadata) %in% names(df_olink$metadata))) {
    missing_fields <- unlist(olink_parquet_metadata)[ # nolint object_usage_linter
      !(unlist(olink_parquet_metadata) %in% names(df_olink$metadata))
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

  # We allow only Olink Explore HT parquet files for now.
  # If other platforms are to be reported in parquet format, we need to add
  # them to this array.
  olink_parquet_platforms <- c("ExploreHT")

  if (!(df_olink$metadata[[olink_parquet_metadata$product]] %in% olink_parquet_platforms)) { # nolint object_usage_linter

    cli::cli_abort(
      c(
        "x" = "Unsupported platform:
        {.val {df_olink$metadata[[olink_parquet_metadata$product]]}}",
        "i" =
          "Metadata field {.val {olink_parquet_metadata$product}} expects:
        {olink_parquet_platforms}"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

  # We allow only  NPX file for now.
  # If other files are to be accepted, we need to add them to this array.
  olink_parquet_files <- c("NPX File",
                           "Extended NPX File",
                           "CLI Data Export File",
                           "Internal CLI Data Export File",
                           "R Package Export File")

  if (!(df_olink$metadata[[olink_parquet_metadata$data_file_type]] %in% olink_parquet_files)) { # nolint object_usage_linter

    cli::cli_abort(
      c(
        "x" = "Unsupported file:
        {.val {df_olink$metadata[[olink_parquet_metadata$data_file_type]]}}",
        "i" =
          "Metadata field {.val {olink_parquet_metadata$data_file_type}}
          expects: {olink_parquet_files}"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

  # Return the ArrowObject ----

  # if needed convert the object to the requested output
  df_olink <- convert_read_npx_output(df = df_olink,
                                      out_df = out_df)

  return(df_olink)
}
