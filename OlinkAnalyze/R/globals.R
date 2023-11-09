# To prevent lint errors when using .data and .env
utils::globalVariables(c(".env", ".data"))

# Basic global variables
accepted_checksum_files <- c("MD5_checksum.txt",
                            "checksum_sha256.txt")
