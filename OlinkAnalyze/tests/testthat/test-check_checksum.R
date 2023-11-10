# Test that when the MD5 checksum of the file matches the reported MD5 checksum,
# then the function works -> returns NULL
test_that(
  "checksum matches works - MD5",
  {
    withr::with_tempfile(
      new = c("text_file_test", "MD5_check"),
      pattern = "MD5",
      fileext = ".txt",
      code = {

        # write a random data frame to file
        dplyr::tibble("A" = c(1, 2.2, 3.14),
                      "B" = c("a", "b", "c"),
                      "C" = c(TRUE, TRUE, FALSE),
                      "D" = c("NA", "B", NA_character_),
                      "E" = c(1L, 2L, 3L)) |>
          utils::write.table(file = text_file_test,
                             append = FALSE,
                             quote = FALSE,
                             sep = ";",
                             eol = "\n",
                             na = "",
                             dec = ".",
                             row.names = FALSE,
                             col.names = TRUE)

        # check that the comma delimited file exists
        expect_true(file.exists(text_file_test))

        # MD5 checksum on the NPX file
        text_file_checksum <- tools::md5sum(text_file_test) |>
          unname()

        # write some text in a txt file
        writeLines(text_file_checksum, MD5_check)

        # check that the parquet file was created
        expect_true(file.exists(MD5_check))

        # check that relevant error is thrown
        expect_no_condition(
          check_checksum(checksum_file = MD5_check,
                         npx_file = text_file_test)
        )

      }
    )

  }
)

# Test that when the SHA256 checksum of the file matches the reported SHA256
# checksum, then the function works -> returns NULL
test_that(
  "checksum matches works - SHA256",
  {
    withr::with_tempfile(
      new = c("text_file_test", "SHA256_check"),
      pattern = "SHA256",
      fileext = ".txt",
      code = {

        # write a random data frame to file
        dplyr::tibble("A" = c(1, 2.2, 3.14),
                      "B" = c("a", "b", "c"),
                      "C" = c(TRUE, TRUE, FALSE),
                      "D" = c("NA", "B", NA_character_),
                      "E" = c(1L, 2L, 3L)) |>
          utils::write.table(file = text_file_test,
                             append = FALSE,
                             quote = FALSE,
                             sep = ";",
                             eol = "\n",
                             na = "",
                             dec = ".",
                             row.names = FALSE,
                             col.names = TRUE)

        # check that the comma delimited file exists
        expect_true(file.exists(text_file_test))

        # SHA256 checksum on the NPX file
        text_file_checksum <- openssl::sha256(file(text_file_test)) |>
          stringr::str_replace(pattern = ":",
                               replacement = "")

        # write some text in a txt file
        writeLines(text_file_checksum, SHA256_check)

        # check that the parquet file was created
        expect_true(file.exists(SHA256_check))

        # check that relevant error is thrown
        expect_no_condition(
          check_checksum(checksum_file = SHA256_check,
                         npx_file = text_file_test)
        )

      }
    )

  }
)

# Test that a relevant error is thrown when the NPX file does not exist.
test_that(
  "checksum matches works - missing NPX file",
  {
    withr::with_tempfile(
      new = "nfile_test",
      pattern = "npx",
      fileext = ".parquet",
      code = {

        # check that the parquet file was created
        expect_false(file.exists(nfile_test))

        withr::with_tempfile(
          new = "SHA256_check",
          pattern = "SHA256",
          fileext = ".txt",
          code = {

            # write in the checksum file
            writeLines("I_AM_A_R4ND0M_ChEcKuP", SHA256_check)

            # check that the parquet file was created
            expect_true(file.exists(SHA256_check))

            # check that relevant string is returned
            expect_error(
              check_checksum(checksum_file = SHA256_check,
                             npx_file = nfile_test),
              regexp = "Unable to open NPX file"
            )

          }
        )

      }
    )

  }
)

# Test that a relevant error is thrown when the checksum file does not exist.
test_that(
  "checksum matches works - missing checksum file",
  {
    withr::with_tempfile(
      new = c("text_file_test", "SHA256_check"),
      pattern = "SHA256",
      fileext = ".txt",
      code = {

        # write a random data frame to file
        dplyr::tibble("A" = c(1, 2.2, 3.14),
                      "B" = c("a", "b", "c"),
                      "C" = c(TRUE, TRUE, FALSE),
                      "D" = c("NA", "B", NA_character_),
                      "E" = c(1L, 2L, 3L)) |>
          utils::write.table(file = text_file_test,
                             append = FALSE,
                             quote = FALSE,
                             sep = ";",
                             eol = "\n",
                             na = "",
                             dec = ".",
                             row.names = FALSE,
                             col.names = TRUE)

        # check that the comma delimited file exists
        expect_true(file.exists(text_file_test))

        # check that the parquet file was created
        expect_false(file.exists(SHA256_check))

        # check that relevant string is returned
        expect_error(
          check_checksum(checksum_file = SHA256_check,
                         npx_file = text_file_test),
          regexp = "Unable to open checksum file"
        )

      }
    )

  }
)

# Test that a relevant error is thrown when the checksum of the file does not
# match the one reported.
test_that(
  "checksum matches works - not matching checksum",
  {
    withr::with_tempfile(
      new = c("text_file_test", "SHA256_check"),
      pattern = "SHA256",
      fileext = ".txt",
      code = {

        # write a random data frame to file
        dplyr::tibble("A" = c(1, 2.2, 3.14),
                      "B" = c("a", "b", "c"),
                      "C" = c(TRUE, TRUE, FALSE),
                      "D" = c("NA", "B", NA_character_),
                      "E" = c(1L, 2L, 3L)) |>
          utils::write.table(file = text_file_test,
                             append = FALSE,
                             quote = FALSE,
                             sep = ";",
                             eol = "\n",
                             na = "",
                             dec = ".",
                             row.names = FALSE,
                             col.names = TRUE)

        # check that the comma delimited file exists
        expect_true(file.exists(text_file_test))

        # write some text in a txt file
        writeLines("I_AM_A_R4ND0M_ChEcKuP", SHA256_check)

        # check that the parquet file was created
        expect_true(file.exists(SHA256_check))

        # check that relevant string is returned
        expect_error(
          check_checksum(checksum_file = SHA256_check,
                         npx_file = text_file_test),
          regexp = "The checksum of the NPX file does not match the one"
        )

      }
    )

  }
)
