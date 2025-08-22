## code to prepare internal dataset goes here
## based on https://r-pkgs.org/data.html#sec-data-sysdata

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
  excel_1      = "xls",
  excel_2      = "xlsx",
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
  ",",
  "\t",
  "|",
  ":"
)

## Acceptable classes of data frames that read_npx* functions may output ----

# Used in:
#   - check_out_df_arg
read_npx_df_output <- c(
  "tibble",
  "arrow"
)

## Acceptable samples types ----

olink_sample_types <- list(
  "sample" = c("SAMPLE"),
  "sc" = c("CONTROL", "SAMPLE_CONTROL"),
  "pc" = c("PLATE_CONTROL"),
  "nc" = c("NEGATIVE_CONTROL")
)

## Acceptable samples types ----

olink_assay_types <- list(
  "assay" = c("assay"),
  "inc" = c("inc_ctrl"),
  "det" = c("det_ctrl"),
  "ext" = c("ext_ctrl"),
  "amp" = c("amp_ctrl")
)
