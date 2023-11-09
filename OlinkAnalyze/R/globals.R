# To prevent lint errors when using .data and .env
utils::globalVariables(c(".env", ".data"))

# Basic global variables ----

# the checksum files we accept
accepted_checksum_files <- c("MD5_checksum.txt",
                             "checksum_sha256.txt")

# the extensions of NPX files that we accept
accepted_npx_file_ext <- c("xls",
                           "xlsx",
                           "csv",
                           "txt",
                           "parquet",
                           "zip")

# the separators of the Olink long files
accepted_field_sep <- c(";",
                        ",")
