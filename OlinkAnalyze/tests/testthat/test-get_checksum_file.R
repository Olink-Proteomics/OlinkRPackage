# Test that the function returns the correct checksum file
test_that(
  "get checksum file from zip works",
  {
    # realistic scenario with checksum_sha256.txt
    expect_equal(
      object = get_checksum_file(
        files = c("checksum_sha256.txt", "test.csv")
      ),
      expected = "checksum_sha256.txt"
    )

    # realistic scenario with MD5_checksum.txt
    expect_equal(
      object = get_checksum_file(
        files = c("MD5_checksum.txt", "test.csv")
      ),
      expected = "MD5_checksum.txt"
    )
  }
)

# Test that NA is returned when there is no acceptable checksum file name
test_that(
  "get checksum file from zip - No checksum file",
  {
    # wrong checksum file
    expect_identical(
      object = get_checksum_file(
        files = c("MD51_checksum.txt", "test.csv")
      ),
      expected = NA_character_
    )
  }
)

# Test that a relevant error messahe os thrown when 2 acceptable checksum files
# are provided.
test_that(
  "get checksum file from zip - 2 checksums",
  {
    # multiple checksum files
    expect_error(
      object = get_checksum_file(
        files = c("MD5_checksum.txt", "MD5_checksum.txt")
      ),
      regexp = "The compressed file contains too many checksum files!"
    )

    expect_error(
      object = get_checksum_file(
        files = c("checksum_sha256.txt", "checksum_sha256.txt")
      ),
      regexp = "The compressed file contains too many checksum files!"
    )

    expect_error(
      object = get_checksum_file(
        files = c("MD5_checksum.txt", "checksum_sha256.txt")
      ),
      regexp = "The compressed file contains too many checksum files!"
    )
  }
)
