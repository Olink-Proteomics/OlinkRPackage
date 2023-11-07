test_that(
  "checksum matches works - MD5",
  {
    parquet_file <- system.file("extdata",
                                "npx_data_ext.parquet",
                                package = "OlinkAnalyze",
                                mustWork = TRUE)
    checksum_tmp_file <- character()

    withr::with_tempfile(
      new = "MD5_check",
      pattern = "MD5",
      fileext = ".txt",
      code = {

        # MD5 checksum on the NPX file
        npx_file_checksum <- tools::md5sum(parquet_file) |>
          unname()

        # write some text in a txt file
        writeLines(npx_file_checksum, MD5_check)

        # check that the parquet file was created
        expect_true(file.exists(MD5_check))

        # check that relevant error is thrown
        expect_identical(
          check_checksum_matches(checksum_file = MD5_check,
                                 npx_file = parquet_file),
          NA_character_
        )

        checksum_tmp_file <<- MD5_check
      }
    )

    expect_false(file.exists(checksum_tmp_file))
  }
)

test_that(
  "checksum matches works - SHA256",
  {
    parquet_file <- system.file("extdata",
                                "npx_data_ext.parquet",
                                package = "OlinkAnalyze",
                                mustWork = TRUE)
    checksum_tmp_file <- character()

    withr::with_tempfile(
      new = "SHA256_check",
      pattern = "SHA256",
      fileext = ".txt",
      code = {

        # SHA256 checksum on the NPX file
        npx_file_checksum <- openssl::sha256(file(parquet_file)) |>
          stringr::str_replace(pattern = ":",
                               replacement = "")

        # write some text in a txt file
        writeLines(npx_file_checksum, SHA256_check)

        # check that the parquet file was created
        expect_true(file.exists(SHA256_check))

        # check that relevant error is thrown
        expect_identical(
          check_checksum_matches(checksum_file = SHA256_check,
                                 npx_file = parquet_file),
          NA_character_
        )

        checksum_tmp_file <<- SHA256_check
      }
    )

    expect_false(file.exists(checksum_tmp_file))
  }
)

test_that(
  "checksum matches works - not matching checksum",
  {
    parquet_file <- system.file("extdata",
                                "npx_data_ext.parquet",
                                package = "OlinkAnalyze",
                                mustWork = TRUE)
    checksum_tmp_file <- character()

    withr::with_tempfile(
      new = "SHA256_check",
      pattern = "SHA256",
      fileext = ".txt",
      code = {

        # write some text in a txt file
        writeLines("I_AM_A_R4ND0M_ChEcKuP", SHA256_check)

        # check that the parquet file was created
        expect_true(file.exists(SHA256_check))

        # check that relevant string is returned
        expect_match(
          check_checksum_matches(checksum_file = SHA256_check,
                                 npx_file = parquet_file),
          regexp = "The checksum of the NPX file does not match the one",
          fixed = TRUE
        )

        checksum_tmp_file <<- SHA256_check
      }
    )

    expect_false(file.exists(checksum_tmp_file))
  }
)
