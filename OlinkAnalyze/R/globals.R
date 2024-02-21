# To prevent lint errors when using .data and .env
utils::globalVariables(
  c(
    ".env",
    ".data"
  )
)

# Global variables ----

## Acceptable Olink platforms ----

# Used in:
#  - read_npx_excel
accepted_olink_platforms <- dplyr::tibble(
  full_name = c(
    "Olink Target 48",
    "Olink Flex",
    "Olink Target 96",
    "Olink Explore 3072",
    "Olink Explore HT",
    "Olink Focus"
  ),
  name = c(
    "Target 48",
    "Flex",
    "Target 96",
    "Explore 3072",
    "Explore HT",
    "Focus"
  ),
  code_friendly_name = c(
    "Target_48",
    "Flex",
    "Target_96",
    "Explore_3072",
    "Explore_HT",
    "Focus"
  ),
  short_name = c(
    "T48",
    "Flex",
    "T96",
    "3k",
    "HT",
    "Focus"
  ),
  broader_platform = c(
    "qPCR",
    "qPCR",
    "qPCR",
    "NGS",
    "NGS",
    "qPCR"
  ),
  regexp = c(
    "Target 48",
    "[A-Z]{4}-[A-Z]{4}|Flex",
    "Target 96",
    NA_character_,
    NA_character_,
    NA_character_
  ),
  quant_method = list(
    c("NPX", "Quantified", "Ct"),
    c("NPX", "Quantified", "Ct"),
    c("NPX", "Ct"),
    c("NPX"),
    c("NPX"),
    c("NPX", "Quantified", "Ct")
  ),
  quant_type = list(
    c("relative", "absolute", "relative"),
    c("relative", "absolute"),
    c("relative", "relative"),
    c("relative"),
    c("relative"),
    c("relative", "absolute")
  ),
  base_index = c(
    NA_integer_,
    NA_integer_,
    92L,
    NA_integer_,
    NA_integer_,
    NA_integer_
  ),
  wide_format_plate_info = c(
    TRUE,
    TRUE,
    FALSE,
    NA,
    NA,
    TRUE
  )
)

## Specifications for quantification types in Olink wide excel files ----

# Used in :
#   - read_npx_wide
olink_wide_spec <- dplyr::tibble(
  data_type = c(
    "NPX",
    "Ct",
    "Quantified"
  ),
  has_qc_data = c(
    TRUE,
    FALSE,
    TRUE
  ),
  n_na_rows = c(
    2L,
    1L,
    2L
  ),
  top_matrix_v1 = list(
    c("Panel", "Assay", "Uniprot ID", "OlinkID"),
    c("Panel", "Assay", "Uniprot ID", "OlinkID"),
    c("Panel", "Assay", "Uniprot ID", "OlinkID", "Unit")
  ),
  top_matrix_assay_labels = list(
    c("plate" = "Plate ID", "qc_warn" = "QC Warning"),
    c("plate" = "Plate ID"),
    c("plate" = "Plate ID", "qc_warn" = "QC Warning")
  ),
  top_matrix_assay_int_ctrl = list(
    c("Inc Ctrl", "Inc Ctrl 1", "Inc Ctrl 2", "Det Ctrl", "Ext Ctrl"),
    c("Inc Ctrl", "Inc Ctrl 1", "Inc Ctrl 2", "Det Ctrl", "Ext Ctrl"),
    c("Inc Ctrl", "Inc Ctrl 1", "Inc Ctrl 2", "Det Ctrl", "Ext Ctrl")
  ),
  top_matrix_assay_dev_int_ctrl = list(
    c("QC Deviation from median"),
    character(0L),
    c("QC Deviation from median")
  ),
  top_matrix_uniprot_dev_int_ctrl = list(
    c("Inc Ctrl", "Inc Ctrl 1", "Inc Ctrl 2", "Det Ctrl"),
    character(0L),
    c("Inc Ctrl", "Inc Ctrl 1", "Inc Ctrl 2", "Det Ctrl")
  )
)

olink_wide_bottom_matrix <-
  readRDS("inst/extdata/npx_signature_bottom_mat_v1_long.rds")

olink_wide_rename_npxs <- readRDS("inst/extdata/olink_wide_rename_npxs.rds")

## Acceptable checksum file names ----

# Used in:
#   - get_checksum_file
#   - get_npx_file
accepted_checksum_files <- c(
  "MD5_checksum.txt",
  "checksum_sha256.txt"
)

## Acceptable extensions of NPX files ----

# Used in:
#   - read_npx
#   - get_npx_file
accepted_npx_file_ext <- c(
  excel_1       = "xls",
  excel_2       = "xlsx",
  delim_1      = "csv",
  delim_2      = "txt",
  parquet_1    = "parquet",
  compressed_1 = "zip"
)

## Acceptable separators for Olink software files in long format ----

# Used in:
#   - read_npx_delim
accepted_field_sep <- c(
  ";",
  ","
)

## Acceptable classes of data frames that read_npx* functions may output ----

# Used in:
#   - check_out_df_arg
read_npx_df_output <- c(
  "tibble",
  "arrow"
)
