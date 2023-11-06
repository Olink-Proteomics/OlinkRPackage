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

    # missing checksum file
    expect_identical(
      get_checksum_file_from_zip(
        files = c("test.csv")
      ),
      NA_character_
    )

    expect_identical(
      get_checksum_file_from_zip(
        files = c("MD51_checksum.txt", "test.csv")
      ),
      NA_character_
    )

    # multiple checksum files
    expect_error(
      get_checksum_file_from_zip(
        files = c("MD5_checksum.txt", "MD5_checksum.txt")
      ),
      regexp = "The compressed file contains an unknown NPX file!"
    )

    expect_error(
      get_checksum_file_from_zip(
        files = c("checksum_sha256.txt", "checksum_sha256.txt")
      ),
      regexp = "The compressed file contains an unknown NPX file!"
    )

    expect_error(
      get_checksum_file_from_zip(
        files = c("MD5_checksum.txt", "checksum_sha256.txt")
      ),
      regexp = "The compressed file contains an unknown NPX file!"
    )
  }
)
