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

## check_npx output list names ----

check_npx_lst_names <- c(
  "col_names",
  "oid_invalid",
  "assay_na",
  "sample_id_dups",
  "sample_id_na",
  "col_class",
  "assay_qc",
  "non_unique_uniprot",
  "darid_invalid"
)

## Rules defining outdated DARID and panel archive combinations
## Used in check_darid(), part of check_npx().
outdated_darid_panel_archive <- dplyr::tibble(
  darid_list = c(
    "D10007", "D20007", "D30007", "D40007",
    "D50007", "D60007", "D70007", "D80007",
    "D10008", "D20008", "D30008", "D40008",
    "D50008", "D60008", "D70008", "D80008",
    "D10010", "D20010", "D30010", "D40010",
    "D50010", "D60010", "D70010", "D80010",
    "D10014", "D20014", "D30014", "D40014",
    "D50014", "D60014", "D70014", "D80014"
  ),
  min_version = "1.5.0"
)
