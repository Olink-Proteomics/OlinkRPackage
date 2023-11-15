#' Help function to read Olink parquet files.
#'
#' @param file Path to Olink Software parquet output file.
#'
#' @return An R6 class ArrowObject.
#'
#' @keywords NPX parquet
#'
read_npx_parquet <- function(file) {

  # Read parquet file ----

  # check if file exists
  check_file_exists(file = file,
                    error = TRUE)

  # tryCatch in case reading the file fails
  tryCatch(
    {

      parquet_npx <- arrow::open_dataset(
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
  if (!all(unlist(olink_parquet_metadata) %in% names(parquet_npx$metadata))) {
    missing_fields <- unlist(olink_parquet_metadata)[ # nolint object_usage_linter
      !(unlist(olink_parquet_metadata) %in% names(parquet_npx$metadata))
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

  if (!(parquet_npx$metadata[[olink_parquet_metadata$product]] %in% olink_parquet_platforms)) { # nolint object_usage_linter

    cli::cli_abort(
      c(
        "x" = "Unsupported platform:
        {.val {parquet_npx$metadata[[olink_parquet_metadata$product]]}}",
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
  olink_parquet_files <- c("NPX File")

  if (!(parquet_npx$metadata[[olink_parquet_metadata$data_file_type]] %in% olink_parquet_files)) { # nolint object_usage_linter

    cli::cli_abort(
      c(
        "x" = "Unsupported file:
        {.val {parquet_npx$metadata[[olink_parquet_metadata$data_file_type]]}}",
        "i" =
          "Metadata field {.val {olink_parquet_metadata$data_file_type}}
          expects: {olink_parquet_files}"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

  # Return the ArrowObject ----

  return(parquet_npx)
}
