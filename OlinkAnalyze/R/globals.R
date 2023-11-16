# To prevent lint errors when using .data and .env
utils::globalVariables(
  c(
    ".env",
    ".data"
  )
)

# Global variables ----

# the checksum files we accept
accepted_checksum_files <- c(
  "MD5_checksum.txt",
  "checksum_sha256.txt"
)

# the extensions of NPX files that we accept
accepted_npx_file_ext <- c(
  wide_1       = "xls",
  wide_2       = "xlsx",
  delim_1      = "csv",
  delim_2      = "txt",
  parquet_1    = "parquet",
  compressed_1 = "zip"
)

# the separators of the Olink long files
accepted_field_sep <- c(
  ";",
  ","
)
