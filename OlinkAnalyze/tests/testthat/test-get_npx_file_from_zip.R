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
# checking for all combos of checksum file and NPX file
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

test_that(
  "get NPX file from zip - 1 MD5 checksum",
  {
    # missing checksum file
    expect_error(
      get_npx_file_from_zip(
        files = c("MD5_checksum.txt")
      ),
      regexp = "The compressed file contains an unknown NPX file!"
    )
  }
)

test_that(
  "get NPX file from zip - 2 MD5 checksums",
  {
    # missing checksum file
    expect_error(
      get_npx_file_from_zip(
        files = c("MD5_checksum.txt", "MD5_checksum.txt")
      ),
      regexp = "The compressed file contains an unknown NPX file!"
    )
  }
)

test_that(
  "get NPX file from zip - 1 sha256 checksum",
  {
    # missing checksum file
    expect_error(
      get_npx_file_from_zip(
        files = c("checksum_sha256.txt")
      ),
      regexp = "The compressed file contains an unknown NPX file!"
    )
  }
)

test_that(
  "get NPX file from zip - 2 sha256 checksums",
  {
    # missing checksum file
    expect_error(
      get_npx_file_from_zip(
        files = c("checksum_sha256.txt", "checksum_sha256.txt")
      ),
      regexp = "The compressed file contains an unknown NPX file!"
    )
  }
)

test_that(
  "get NPX file from zip - no known file",
  {
    # missing checksum file
    expect_error(
      get_npx_file_from_zip(
        files = c("test.xml")
      ),
      regexp = "The compressed file contains an unknown NPX file!"
    )
  }
)

test_that(
  "get NPX file from zip - no known files",
  {
    # missing checksum file
    expect_error(
      get_npx_file_from_zip(
        files = c("test.xml", "test.yaml")
      ),
      regexp = "The compressed file contains an unknown NPX file!"
    )
  }
)

test_that(
  "get NPX file from zip - many known files v1",
  {
    # missing checksum file
    expect_error(
      get_npx_file_from_zip(
        files = c("test.txt", "test.csv")
      ),
      regexp = "The compressed file contains an unknown NPX file!"
    )
  }
)

test_that(
  "get NPX file from zip - many known files v2",
  {
    # missing checksum file
    expect_error(
      get_npx_file_from_zip(
        files = c("test.parquet", "test.csv")
      ),
      regexp = "The compressed file contains an unknown NPX file!"
    )
  }
)

test_that(
  "get NPX file from zip - many known files v3",
  {
    # missing checksum file
    expect_error(
      get_npx_file_from_zip(
        files = c("test.xlsx", "test.csv")
      ),
      regexp = "The compressed file contains an unknown NPX file!"
    )
  }
)

test_that(
  "get NPX file from zip - many known files v4",
  {
    # missing checksum file
    expect_error(
      get_npx_file_from_zip(
        files = c("test.xls", "test.csv")
      ),
      regexp = "The compressed file contains an unknown NPX file!"
    )
  }
)
