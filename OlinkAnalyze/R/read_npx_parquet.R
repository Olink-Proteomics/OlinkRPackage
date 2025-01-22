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
  p_file <- arrow::open_dataset(
    sources = filename
  )
  p_file_meta <- names(p_file$metadata)

  # Check that all required parquet metadata is in place
  exp_p_meta <- list(
    dtftp = "DataFileType",
    prod = "Product"
  )
  if (!all(unlist(exp_p_meta) %in% p_file_meta)) {
    cli::cli_warn( # nolint
      "Missing required field{?s}
      {.val {unlist(exp_p_meta)[!(unlist(exp_p_meta) %in% p_file_meta)]}}
      in parquet file's metadata."
    )
  }

  # If other platforms are to be reported as parquet too, we have to add
  # them to this array
  olink_platforms <- c("ExploreHT", "Explore3072", "Reveal")
  if (exp_p_meta$prod %in% p_file_meta
      & !(p_file$metadata[[exp_p_meta$prod]] %in% olink_platforms)) {
    cli::cli_warn(
      "Only parquet file from {.val {olink_platforms}} are currently supported."
    )
  }

  # Print RUO message if present
  if (("RUO" %in% names(p_file$metadata))) {
    cli::cli_alert_info(
      "This parquet file is for research use only:
      {.val {p_file$metadata$RUO}}!")
  }

  # Check if it is an NPX file
  olink_files <- c("NPX File",
                   "Extended NPX File",
                   "CLI Data Export File",
                   "Internal CLI Data Export File",
                   "R Package Export File",
                   "Olink Analyze Export File")
  if (exp_p_meta$dtftp %in% p_file_meta
      & !(p_file$metadata[[exp_p_meta$dtftp]] %in% olink_files)) {
    cli::cli_warn("Only {.val {\"NPX\"}} parquet files are currently
                  supported.")
  }

  # Check that required columns are present
  required_cols <- c("SampleID",
                     "OlinkID",
                     "UniProt",
                     "Assay",
                     "Panel",
                     "PlateID",
                     "SampleQC",
                     "NPX")
  missing_cols <- setdiff(x = required_cols,
                          y = names(p_file))
  if(length(missing_cols) != 0L) {
    cli::cli_abort(
      "The following columns are missing: {.val {missing_cols}}"
    )
  }


  df_npx <- parquet_file %>%
    dplyr::collect() %>%
    dplyr::as_tibble()

  return(df_npx)
}
