#' Helper function to read in Olink Explore parquet output files
#'
#' @param filename Path to Olink Software parquet output file.
#'
#' @return A "tibble" in long format. Some of the columns are:
#' \itemize{
#'    \item{SampleID:} Sample ID
#'    \item{OlinkID:} Olink ID
#'    \item{UniProt:} UniProt ID
#'    \item{Assay:} Protein symbol
#'    \item{PlateID:} Plate ID
#'    \item{Count:} Counts from sequences
#'    \item{ExtNPX:} External control normalized counts
#'    \item{NPX:} Normalized Protein Expression
#' }
#' Additional columns may be present or missing depending on the platform
#'
#' @keywords NPX
#'
#' @examples
#' \donttest{
#' file <- system.file("extdata", "Example_NPX_Data.csv", package = "OlinkAnalyze")
#' read_NPX(file)
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr as_tibble select
#'

read_npx_parquet <- function (filename) {

  if(!requireNamespace("arrow", quietly = TRUE) ) {
    stop("Reading parquet files requires the arrow package.
         Please install arrow before continuing.

         install.packages(\"arrow\")")
  }

  # pointer to parquet file
  parquet_file <- arrow::open_dataset(
    sources = filename
  )

  # Check that all required parquet metadata is in place
  olink_parquet_metadata <- c("DataFileType",
                              "Product")
  if (!all(olink_parquet_metadata %in% names(parquet_file$metadata))) {

    stop(
      paste("Missing required fileds",
            paste(
              paste0("\"", olink_parquet_metadata, "\""),
              collapse = " and "),
            "in parquet file's metadata."
            )
      )

  }

  # We allow only Olink Explore HT parquet files for now
  # If other platforms are to be reported as parquet too, we have to add
  # them to this array
  olink_platforms <- c("ExploreHT", "Explore3072")
  if (!(parquet_file$metadata$Product %in% olink_platforms)) {

    stop("Only \"Olink Explore HT\" or \"Olink Explore 3072\" parquet files are currently supported.")

  }


  # Check if it is an NPX file
  olink_files <- c("NPX File",
                   "Extended NPX File",
                   "CLI Data Export File",
                   "Internal CLI Data Export File",
                   "R Package Export File",
                   "Olink Analyze Export File")
  if (parquet_file$metadata$DataFileType %in% olink_files) {

    # Check that required columns are present
    required_cols <- c("SampleID",
                       "OlinkID",
                       "UniProt",
                       "Assay",
                       "Panel",
                       "PlateID",
                       "SampleQC",
                       "NPX")

    missing_cols <- setdiff(required_cols,
                            names(parquet_file))

    if(length(missing_cols) != 0) {

      stop(paste("The following columns are missing:",
                 paste(missing_cols, collapse = ", ")))

    }

  } else {

    stop("Only \"NPX\" parquet files are currently supported.")

  }

  df_npx <- parquet_file %>%
    dplyr::collect() %>%
    dplyr::as_tibble()

  return(df_npx)
}
