# To prevent lint errors when using .data and .env
utils::globalVariables(
  c(
    ".env",
    ".data"
  )
)

# Global variables ----

## Acceptable checksum file names ----

# Used in:
#   - get_checksum_file()
#   - get_npx_file()
accepted_checksum_files <- c(
  "MD5_checksum.txt",
  "checksum_sha256.txt"
)

## Acceptable extensions of NPX files ----

# Used in:
#   - read_npx()
#   - get_npx_file()
accepted_npx_file_ext <- c(
  wide_1       = "xls",
  wide_2       = "xlsx",
  delim_1      = "csv",
  delim_2      = "txt",
  parquet_1    = "parquet",
  compressed_1 = "zip"
)

## Acceptable separators for Olink software files in long format ----

# Used in:
#   - read_npx_delim()
accepted_field_sep <- c(
  ";",
  ","
)

## Acceptable classes of data frames that read_npx* functions may output ----

# Used in:
#   - check_out_df_arg()
read_npx_df_output <- c(
  "tibble",
  "arrow"
)
