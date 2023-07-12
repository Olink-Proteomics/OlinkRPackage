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
#' @importFrom arrow open_dataset
#'

read_npx_parquet <- function (filename) {

  # pointer to parquet file
  parquet_file <- arrow::open_dataset(
    sources = filename
  )

  # We allow only Olink Explore HT parquet files for now
  # If other platforms are to be reported as parquet too, we have to add
  # them to this array
  olink_platforms <- c("ExploreHT")
  if (!(parquet_file$metadata$ProductType %in% olink_platforms)) {
    stop("Only \"Olink Explore HT\" parquet files are allowed!")
  }

  # Check if it is an NPX file
  olink_files <- c("NPX File")
  if (parquet_file$metadata$DataFileType %in% olink_files) {

    # NPX and EXTENDED NPX files share a large fraction of columns
    parquet_file <- parquet_file %>%
      dplyr::select(
        # same as E3072
        SampleID,
        WellID,
        PlateID,
        OlinkID,
        UniProt,
        Assay,
        Panel,
        NPX,
        Normalization,
        ExploreVersion,

        # New entry
        Block,
        AssayType,
        Count,
        ExtNPX,
        PCNormalizedNPX,

        # It was Sample_Type in EXTNDED NPX E3072
        # Not used in OA, so we keep it as is
        SampleType,
        # Corresponds to Panel_Lot_Nr
        # Should consider keeping it unchanged
        Panel_Lot_Nr = DataAnalysisRefID,
        # Corresponds to Assay_Warning
        # Should consider keeping it unchanged
        Assay_Warning = AssayQC,
        # Corresponds to QC_Warning
        # Should consider keeping it unchanged
        QC_Warning = SampleQC
      )

  } else {
    stop("Only \"NPX\" and \"EXTENDED NPX\" parquet files are allowed!")
  }

  df_npx <- parquet_file %>%
    dplyr::collect() %>%
    dplyr::as_tibble()

  return(df_npx)
}
