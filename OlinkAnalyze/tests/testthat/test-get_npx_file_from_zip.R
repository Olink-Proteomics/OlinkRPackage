# test all realistic scenario combos of checksum files and NPX files ----

test_accepted_npx_files <- c("xls",
                             "xlsx",
                             "csv",
                             "txt",
                             "parquet")

test_accepted_checksum_files <- c("checksum_sha256.txt",
                                  "MD5_checksum.txt")

# Test get_npx_file_from_zip for slightly different inputs. Specifically,
# checking for all combos of checksum file and NPX file
test_checksum_npx_combo <- function(c_file, n_file) {
  test_that(
    paste("Testing get NPX file from zip with:", c_file, "and", n_file), {
      expect_equal(
        get_npx_file_from_zip(files = c(c_file, n_file)),
        n_file
      )
    }
  )
}

invisible(
  sapply(
    test_accepted_checksum_files,
    function(checksum_file) {
      sapply(
        paste("test", test_accepted_npx_files, sep = "."),
        function(npx_file) {
          test_checksum_npx_combo(c_file = checksum_file, n_file = npx_file)
        }
      )
    }
  )
)

rm(test_accepted_checksum_files,
   test_checksum_npx_combo)

# test all realistic scenario of NPX files input only ----

# Test get_npx_file_from_zip for slightly different inputs. Specifically,
# checking for all NPX files.
test_npx_input <- function(n_file) {
  test_that(
    paste("Testing get NPX file from zip with:", n_file), {
      expect_equal(
        get_npx_file_from_zip(files = n_file),
        n_file
      )
    }
  )
}

invisible(
  sapply(
    paste("test", test_accepted_npx_files, sep = "."),
    function(npx_file) {
      test_npx_input(n_file = npx_file)
    }
  )
)

rm(test_accepted_npx_files,
   test_npx_input)

# Test edge cases ----

# Test that relevant error is thrown when only checksum files are provided.
test_that(
  "get NPX file from zip - only checksums",
  {
    # one MD5 only
    expect_error(
      get_npx_file_from_zip(
        files = c("MD5_checksum.txt")
      ),
      regexp = "The compressed file contains no NPX files!"
    )

    # two MD5
    expect_error(
      get_npx_file_from_zip(
        files = c("MD5_checksum.txt", "MD5_checksum.txt")
      ),
      regexp = "The compressed file contains no NPX files!"
    )

    # one SHA256 only
    expect_error(
      get_npx_file_from_zip(
        files = c("checksum_sha256.txt")
      ),
      regexp = "The compressed file contains no NPX files!"
    )

    # two SHA256
    expect_error(
      get_npx_file_from_zip(
        files = c("checksum_sha256.txt", "checksum_sha256.txt")
      ),
      regexp = "The compressed file contains no NPX files!"
    )
  }
)

# Test that relevant error is thrown when unknown files are provided.
test_that(
  "get NPX file from zip - no known file",
  {
    # one unknown file
    expect_error(
      get_npx_file_from_zip(
        files = c("test.xml")
      ),
      regexp = "The compressed file contains no NPX files!"
    )

    # two unknown files
    expect_error(
      get_npx_file_from_zip(
        files = c("test.xml", "test.yaml")
      ),
      regexp = "The compressed file contains no NPX files!"
    )
  }
)

# Test that relevant error is thrown when 2 acceptable files are provided.
test_that(
  "get NPX file from zip - many known files v1",
  {
    # A txt and a csv file
    expect_error(
      get_npx_file_from_zip(
        files = c("test.txt", "test.csv")
      ),
      regexp = "The compressed file contains multiple NPX files!"
    )

    # A parquet and a csv file
    expect_error(
      get_npx_file_from_zip(
        files = c("test.parquet", "test.csv")
      ),
      regexp = "The compressed file contains multiple NPX files!"
    )

    # An xlsx and a csv file
    expect_error(
      get_npx_file_from_zip(
        files = c("test.xlsx", "test.csv")
      ),
      regexp = "The compressed file contains multiple NPX files!"
    )

    # misAn xls and a csv file
    expect_error(
      get_npx_file_from_zip(
        files = c("test.xls", "test.csv")
      ),
      regexp = "The compressed file contains multiple NPX files!"
    )
  }
)
