test_that(
  "get checksum file from zip works",
  {
    # realistic scenario with checksum_sha256.txt
    expect_equal(
      get_checksum_file_from_zip(
        files = c("checksum_sha256.txt", "test.csv")
      ),
      "checksum_sha256.txt"
    )

    # realistic scenario with MD5_checksum.txt
    expect_equal(
      get_checksum_file_from_zip(
        files = c("MD5_checksum.txt", "test.csv")
      ),
      "MD5_checksum.txt"
    )
  }
)

test_that(
  "get checksum file from zip - No checksum file",
  {
    # missing checksum file
    expect_identical(
      get_checksum_file_from_zip(
        files = c("MD51_checksum.txt", "test.csv")
      ),
      NA_character_
    )
  }
)

test_that(
  "get checksum file from zip - 2 MD5 checksums",
  {
    # multiple checksum files
    expect_error(
      get_checksum_file_from_zip(
        files = c("MD5_checksum.txt", "MD5_checksum.txt")
      ),
      regexp = "The compressed file contains too many checksum files!"
    )
  }
)

test_that(
  "get checksum file from zip - 2 sha256 checksums",
  {
    expect_error(
      get_checksum_file_from_zip(
        files = c("checksum_sha256.txt", "checksum_sha256.txt")
      ),
      regexp = "The compressed file contains too many checksum files!"
    )

    expect_error(
      get_checksum_file_from_zip(
        files = c("MD5_checksum.txt", "checksum_sha256.txt")
      ),
      regexp = "The compressed file contains too many checksum files!"
    )
  }
)

test_that(
  "get checksum file from zip - 1 sha256 and 1 MD5 checksum",
  {
    expect_error(
      get_checksum_file_from_zip(
        files = c("MD5_checksum.txt", "checksum_sha256.txt")
      ),
      regexp = "The compressed file contains too many checksum files!"
    )
  }
)
