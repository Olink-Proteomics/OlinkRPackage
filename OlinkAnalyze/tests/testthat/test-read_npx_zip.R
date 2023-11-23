# Test that if the input file has been misidentified as zip compressed but it is
# not (e.g. .txt), then a relevant error is thrown
test_that(
  "Non-zip input is handled - random file",
  {
    withr::with_tempfile(
      new = "txtfile_z",
      pattern = "txt-file_as_zip-file",
      fileext = ".txt",
      code = {

        # write some text in a txt file
        writeLines("foo", txtfile_z)

        # check that the parquet file was created
        expect_true(object = file.exists(txtfile_z))

        # check that relevant error is thrown
        expect_error(
          object = read_npx_zip(file = txtfile_z),
          regexp = "Unable to open compressed file: "
        )

      }
    )

  }
)

# Test that if the input file has been correctly identified as zip compressed
# but it is not (e.g. it was just named as .zip but it is a .txt file), then a
# relevant error is thrown
test_that(
  "Non-zip input is handled - corrupt zip file",
  {
    withr::with_tempfile(
      new = "txtfile_zcorrupt",
      pattern = "txt-file_as_corrupt-zip-file",
      fileext = ".parquet",
      code = {

        # write some text in a txt file
        writeLines("foo", txtfile_zcorrupt)

        # check that the parquet file was created
        expect_true(object = file.exists(txtfile_zcorrupt))

        # check that relevant error is thrown
        expect_error(
          object = read_npx_zip(file = txtfile_zcorrupt),
          regexp = "Unable to open compressed file: "
        )

      }
    )

  }
)

# Test that relevant error is thrown when both NPX and checksum are missing from
# the compressed file.
# Note that if NPX is missing then a relevant error is thrown from the
# get_npx_from_zip function
test_that(
  "read NPX zip - no checsum or NPX file",
  {
    readme_file <- file.path(tempdir(),
                             "README.txt")
    writeLines("foo", readme_file)
    expect_true(object = file.exists(readme_file))

    withr::with_tempfile(
      new = "zip_test",
      pattern = "zip_test",
      fileext = ".zip",
      code = {

        utils::zip(
          zipfile = zip_test,
          files = c(readme_file),
          flags = "-jq"
        )

        expect_true(object = file.exists(zip_test))

        expect_error(
          object = read_npx_zip(file = zip_test),
          regexp = "No NPX and checksum file in the compressed file"
        )

        file.remove(readme_file)
      }
    )

    expect_false(object = file.exists(readme_file))
  }
)

# Test that the function works when the compressed file contains only the NPX
# file.
test_that(
  "read NPX zip - NPX file only",
  {
    withr::with_tempfile(
      new = "nfile_test",
      pattern = "npx",
      fileext = ".csv",
      code = {

        # write the coma-delimited file
        dplyr::tibble(
          "A" = c(1, 2.2, 3.14),
          "B" = c("a", "b", "c"),
          "C" = c(TRUE, TRUE, FALSE),
          "D" = c("NA", "B", NA_character_),
          "E" = c(1L, 2L, 3L)
        ) |>
          utils::write.table(
            file = nfile_test,
            append = FALSE,
            quote = FALSE,
            sep = ";",
            eol = "\n",
            na = "",
            dec = ".",
            row.names = FALSE,
            col.names = TRUE
          )

        # check that the parquet file was created
        expect_true(object = file.exists(nfile_test))

        withr::with_tempfile(
          new = "zfile_test",
          pattern = "npx",
          fileext = ".zip",
          code = {

            # write zip file
            utils::zip(
              zipfile = zfile_test,
              files = c(nfile_test),
              flags = "-jq"
            )

            # check that the zip file was created
            expect_true(object = file.exists(zfile_test))

            # check that this works
            expect_no_condition(
              object = read_npx_zip(file = zfile_test)
            )

          }
        )

      }
    )

  }
)

# Test that the function works when the compressed file contains the NPX
# file and the README file.
test_that(
  "read NPX zip - NPX and README files only",
  {
    readmefile_test <- file.path(tempdir(),
                                 "README.txt")
    writeLines("foo", readmefile_test)
    expect_true(object = file.exists(readmefile_test))

    withr::with_tempfile(
      new = "nfile_test",
      pattern = "npx",
      fileext = ".csv",
      code = {

        # write the coma-delimited file
        dplyr::tibble(
          "A" = c(1, 2.2, 3.14),
          "B" = c("a", "b", "c"),
          "C" = c(TRUE, TRUE, FALSE),
          "D" = c("NA", "B", NA_character_),
          "E" = c(1L, 2L, 3L)
        ) |>
          utils::write.table(
            file = nfile_test,
            append = FALSE,
            quote = FALSE,
            sep = ";",
            eol = "\n",
            na = "",
            dec = ".",
            row.names = FALSE,
            col.names = TRUE
          )

        # check that the parquet file was created
        expect_true(object = file.exists(nfile_test))

        withr::with_tempfile(
          new = "zfile_test",
          pattern = "npx",
          fileext = ".zip",
          code = {

            # write zip file
            utils::zip(
              zipfile = zfile_test,
              files = c(readmefile_test, nfile_test),
              flags = "-jq"
            )

            # check that the zip file was created
            expect_true(object = file.exists(zfile_test))

            # check that this works
            expect_no_condition(
              object = read_npx_zip(file = zfile_test)
            )
          }
        )

        file.remove(readmefile_test)
      }
    )

    expect_false(object = file.exists(readmefile_test))
  }
)

# Test that the function works when the compressed file contains the NPX
# file, the README file and another file that does not have the extension xlsx,
# xlsx, csv, txt or parquet.
# Note that if there are multiple NPX files in the compressed file then the
# function get_npx_file_from_zip throws a relevant error.
test_that(
  "read NPX zip - NPX, README and random non-npx",
  {
    readmefile_test <- file.path(tempdir(),
                                 "README.txt")
    writeLines("foo", readmefile_test)
    expect_true(object = file.exists(readmefile_test))

    withr::with_tempfile(
      new = "nfile_test",
      pattern = "npx",
      fileext = ".csv",
      code = {

        # write the coma-delimited file
        dplyr::tibble(
          "A" = c(1, 2.2, 3.14),
          "B" = c("a", "b", "c"),
          "C" = c(TRUE, TRUE, FALSE),
          "D" = c("NA", "B", NA_character_),
          "E" = c(1L, 2L, 3L)
        ) |>
          utils::write.table(
            file = nfile_test,
            append = FALSE,
            quote = FALSE,
            sep = ";",
            eol = "\n",
            na = "",
            dec = ".",
            row.names = FALSE,
            col.names = TRUE
          )

        # check that the parquet file was created
        expect_true(object = file.exists(nfile_test))

        withr::with_tempfile(
          new = "zfile_test",
          pattern = "npx",
          fileext = ".zip",
          code = {

            withr::with_tempfile(
              new = "rfile_test",
              pattern = "random",
              fileext = ".yaml",
              code = {

                # create random file
                writeLines("foo", rfile_test)
                expect_true(object = file.exists(rfile_test))

                # write zip file
                utils::zip(
                  zipfile = zfile_test,
                  files = c(readmefile_test, nfile_test, rfile_test),
                  flags = "-jq"
                )

                # check that the zip file was created
                expect_true(object = file.exists(zfile_test))

                # check that this works
                expect_no_condition(
                  object = read_npx_zip(file = zfile_test)
                )

              }
            )

          }
        )

        file.remove(readmefile_test)

      }
    )

    expect_false(object = file.exists(readmefile_test))

  }
)

# Test that the function works when the compressed file contains the NPX
# file, the README file and another file that does not have the extension xlsx,
# xlsx, csv, txt or parquet. In this case the random file has been added to the
# argument .ignore_files to test that it works.
test_that(
  "read NPX zip - NPX, README and random non-npx - .ignore_files works v1",
  {
    readmefile_test <- file.path(tempdir(),
                                 "README.txt")
    writeLines("foo", readmefile_test)
    expect_true(object = file.exists(readmefile_test))

    withr::with_tempfile(
      new = "nfile_test",
      pattern = "npx",
      fileext = ".csv",
      code = {

        # write the coma-delimited file
        dplyr::tibble(
          "A" = c(1, 2.2, 3.14),
          "B" = c("a", "b", "c"),
          "C" = c(TRUE, TRUE, FALSE),
          "D" = c("NA", "B", NA_character_),
          "E" = c(1L, 2L, 3L)
        ) |>
          utils::write.table(
            file = nfile_test,
            append = FALSE,
            quote = FALSE,
            sep = ";",
            eol = "\n",
            na = "",
            dec = ".",
            row.names = FALSE,
            col.names = TRUE
          )

        # check that the parquet file was created
        expect_true(object = file.exists(nfile_test))

        withr::with_tempfile(
          new = "zfile_test",
          pattern = "npx",
          fileext = ".zip",
          code = {

            withr::with_tempfile(
              new = "rfile_test",
              pattern = "random",
              fileext = ".yaml",
              code = {

                # create random file
                writeLines("foo", rfile_test)
                expect_true(object = file.exists(rfile_test))

                # write zip file
                utils::zip(
                  zipfile = zfile_test,
                  files = c(readmefile_test, nfile_test, rfile_test),
                  flags = "-jq"
                )

                # check that the zip file was created
                expect_true(object = file.exists(zfile_test))

                # check that this works
                expect_no_condition(
                  object = read_npx_zip(
                    file = zfile_test,
                    .ignore_files = c(basename(readmefile_test),
                                      basename(rfile_test))
                  )
                )

              }
            )

          }
        )

        file.remove(readmefile_test)

      }
    )

    expect_false(object = file.exists(readmefile_test))

  }
)

# Test that the function works when the compressed file contains the NPX
# file, the README file and another file that has the extension xlsx, xlsx, csv,
# txt or parquet. In this case the random NPX file has been added to the
# argument .ignore_files to test that it works.
test_that(
  "read NPX zip - NPX, README and random npx - .ignore_files works v2",
  {
    readmefile_test <- file.path(tempdir(),
                                 "README.txt")
    writeLines("foo", readmefile_test)
    expect_true(object = file.exists(readmefile_test))

    withr::with_tempfile(
      new = "nfile_test",
      pattern = "npx",
      fileext = ".csv",
      code = {

        # write the coma-delimited file
        dplyr::tibble(
          "A" = c(1, 2.2, 3.14),
          "B" = c("a", "b", "c"),
          "C" = c(TRUE, TRUE, FALSE),
          "D" = c("NA", "B", NA_character_),
          "E" = c(1L, 2L, 3L)
        ) |>
          utils::write.table(
            file = nfile_test,
            append = FALSE,
            quote = FALSE,
            sep = ";",
            eol = "\n",
            na = "",
            dec = ".",
            row.names = FALSE,
            col.names = TRUE
          )

        # check that the parquet file was created
        expect_true(object = file.exists(nfile_test))

        withr::with_tempfile(
          new = "zfile_test",
          pattern = "npx",
          fileext = ".zip",
          code = {

            withr::with_tempfile(
              new = "rfile_test",
              pattern = "npx",
              fileext = ".csv",
              code = {

                # create random file
                writeLines("foo", rfile_test)
                expect_true(object = file.exists(rfile_test))

                # write zip file
                utils::zip(
                  zipfile = zfile_test,
                  files = c(readmefile_test, nfile_test, rfile_test),
                  flags = "-jq"
                )

                # check that the zip file was created
                expect_true(object = file.exists(zfile_test))

                # check that this works
                expect_no_condition(
                  object = read_npx_zip(
                    file = zfile_test,
                    .ignore_files = c(basename(readmefile_test),
                                      basename(rfile_test))
                  )
                )
              }
            )

          }
        )

        file.remove(readmefile_test)

      }
    )

    expect_false(object = file.exists(readmefile_test))

  }
)

# Test that the function works when the compressed file contains the NPX
# file, the README file and an MD5 checksum file.
test_that(
  "read NPX zip - NPX, README and MD5 checksum",
  {
    readmefile_test <- file.path(tempdir(),
                                 "README.txt")
    writeLines("foo", readmefile_test)
    expect_true(object = file.exists(readmefile_test))

    checksumfile_test <- file.path(tempdir(),
                                   "MD5_checksum.txt")

    withr::with_tempfile(
      new = "nfile_test",
      pattern = "npx",
      fileext = ".csv",
      code = {

        # write the coma-delimited file
        dplyr::tibble(
          "A" = c(1, 2.2, 3.14),
          "B" = c("a", "b", "c"),
          "C" = c(TRUE, TRUE, FALSE),
          "D" = c("NA", "B", NA_character_),
          "E" = c(1L, 2L, 3L)
        ) |>
          utils::write.table(
            file = nfile_test,
            append = FALSE,
            quote = FALSE,
            sep = ";",
            eol = "\n",
            na = "",
            dec = ".",
            row.names = FALSE,
            col.names = TRUE
          )

        # check that the parquet file was created
        expect_true(object = file.exists(nfile_test))

        # compute MD5 checksum
        cli::hash_file_md5(paths = nfile_test) |>
          writeLines(checksumfile_test)

        # check that the checksum file was created
        expect_true(object = file.exists(checksumfile_test))

        withr::with_tempfile(
          new = "zfile_test",
          pattern = "npx",
          fileext = ".zip",
          code = {

            # write zip file
            utils::zip(
              zipfile = zfile_test,
              files = c(readmefile_test,
                        nfile_test,
                        checksumfile_test),
              flags = "-jq"
            )

            # check that the zip file was created
            expect_true(object = file.exists(zfile_test))

            # check that this works
            expect_no_condition(
              object = df_out <- read_npx_zip(file = zfile_test,
                                              out_df = "arrow",
                                              sep = NULL)
            )

            expect_true(exists("df_out"))

            expect_true(inherits(x = df_out, what = "ArrowObject"))

            expect_true(
              read_npx_zip(file = zfile_test,
                           out_df = "tibble",
                           sep = NULL) |>
                inherits(what = "tbl_df")
            )

          }
        )

        file.remove(readmefile_test)
        file.remove(checksumfile_test)

      }
    )

    expect_false(object = file.exists(readmefile_test))
    expect_false(object = file.exists(checksumfile_test))
  }
)

# Test that the function works when the compressed file contains the NPX
# file and an MD5 checksum file.
test_that(
  "read NPX zip - NPX and MD5 checksum",
  {
    checksumfile_test <- file.path(tempdir(),
                                   "MD5_checksum.txt")

    withr::with_tempfile(
      new = "nfile_test",
      pattern = "npx",
      fileext = ".csv",
      code = {

        # write the coma-delimited file
        dplyr::tibble(
          "A" = c(1, 2.2, 3.14),
          "B" = c("a", "b", "c"),
          "C" = c(TRUE, TRUE, FALSE),
          "D" = c("NA", "B", NA_character_),
          "E" = c(1L, 2L, 3L)
        ) |>
          utils::write.table(
            file = nfile_test,
            append = FALSE,
            quote = FALSE,
            sep = ";",
            eol = "\n",
            na = "",
            dec = ".",
            row.names = FALSE,
            col.names = TRUE
          )

        # check that the parquet file was created
        expect_true(object = file.exists(nfile_test))

        # compute MD5 checksum
        cli::hash_file_md5(paths = nfile_test) |>
          writeLines(checksumfile_test)

        # check that the checksum file was created
        expect_true(object = file.exists(checksumfile_test))

        withr::with_tempfile(
          new = "zfile_test",
          pattern = "npx",
          fileext = ".zip",
          code = {

            # write zip file
            utils::zip(
              zipfile = zfile_test,
              files = c(nfile_test,
                        checksumfile_test),
              flags = "-jq"
            )

            # check that the zip file was created
            expect_true(object = file.exists(zfile_test))

            # check that this works
            expect_no_condition(
              object = read_npx_zip(file = zfile_test)
            )
          }
        )

        file.remove(checksumfile_test)

      }
    )

    expect_false(object = file.exists(checksumfile_test))

  }
)

# Test that the function works when the compressed file contains the NPX
# file, the README file and an SHA256 checksum file.
test_that(
  "read NPX zip - NPX, README and SHA256 checksum",
  {
    readmefile_test <- file.path(tempdir(),
                                 "README.txt")
    writeLines("foo", readmefile_test)
    expect_true(object = file.exists(readmefile_test))

    checksumfile_test <- file.path(tempdir(),
                                   "checksum_sha256.txt")

    withr::with_tempfile(
      new = "nfile_test",
      pattern = "npx",
      fileext = ".csv",
      code = {

        # write the coma-delimited file
        dplyr::tibble(
          "A" = c(1, 2.2, 3.14),
          "B" = c("a", "b", "c"),
          "C" = c(TRUE, TRUE, FALSE),
          "D" = c("NA", "B", NA_character_),
          "E" = c(1L, 2L, 3L)
        ) |>
          utils::write.table(
            file = nfile_test,
            append = FALSE,
            quote = FALSE,
            sep = ";",
            eol = "\n",
            na = "",
            dec = ".",
            row.names = FALSE,
            col.names = TRUE
          )

        # check that the parquet file was created
        expect_true(object = file.exists(nfile_test))

        # compute SHA256 checksum
        openssl::sha256(file(nfile_test)) |>
          stringr::str_replace(pattern = ":",
                               replacement = "") |>
          writeLines(checksumfile_test)

        # check that the checksum file was created
        expect_true(object = file.exists(checksumfile_test))

        withr::with_tempfile(
          new = "zfile_test",
          pattern = "npx",
          fileext = ".zip",
          code = {

            # write zip file
            utils::zip(
              zipfile = zfile_test,
              files = c(readmefile_test,
                        nfile_test,
                        checksumfile_test),
              flags = "-jq"
            )

            # check that the zip file was created
            expect_true(object = file.exists(zfile_test))

            # check that this works
            expect_no_condition(
              object = df_out <- read_npx_zip(file = zfile_test,
                                              out_df = "arrow",
                                              sep = NULL)
            )

            expect_true(exists("df_out"))

            expect_true(inherits(x = df_out, what = "ArrowObject"))

            expect_true(
              read_npx_zip(file = zfile_test,
                           out_df = "tibble",
                           sep = NULL) |>
                inherits(what = "tbl_df")
            )
          }
        )

        file.remove(readmefile_test)
        file.remove(checksumfile_test)

      }
    )

    expect_false(object = file.exists(readmefile_test))
    expect_false(object = file.exists(checksumfile_test))

  }
)

# Test that the function works when the compressed file contains the NPX
# file and an SHA256 checksum file.
test_that(
  "read NPX zip - NPX and SHA256 checksum",
  {
    checksumfile_test <- file.path(tempdir(),
                                   "checksum_sha256.txt")

    withr::with_tempfile(
      new = "nfile_test",
      pattern = "npx",
      fileext = ".csv",
      code = {

        # write the coma-delimited file
        dplyr::tibble(
          "A" = c(1, 2.2, 3.14),
          "B" = c("a", "b", "c"),
          "C" = c(TRUE, TRUE, FALSE),
          "D" = c("NA", "B", NA_character_),
          "E" = c(1L, 2L, 3L)
        ) |>
          utils::write.table(
            file = nfile_test,
            append = FALSE,
            quote = FALSE,
            sep = ";",
            eol = "\n",
            na = "",
            dec = ".",
            row.names = FALSE,
            col.names = TRUE
          )

        # check that the parquet file was created
        expect_true(object = file.exists(nfile_test))

        # compute SHA256 checksum
        openssl::sha256(file(nfile_test)) |>
          stringr::str_replace(pattern = ":",
                               replacement = "") |>
          writeLines(checksumfile_test)

        # check that the checksum file was created
        expect_true(object = file.exists(checksumfile_test))

        withr::with_tempfile(
          new = "zfile_test",
          pattern = "npx",
          fileext = ".zip",
          code = {

            # write zip file
            utils::zip(
              zipfile = zfile_test,
              files = c(nfile_test,
                        checksumfile_test),
              flags = "-jq"
            )

            # check that the zip file was created
            expect_true(object = file.exists(zfile_test))

            # check that this works
            expect_no_condition(
              object = read_npx_zip(file = zfile_test)
            )

          }
        )

        file.remove(checksumfile_test)

      }
    )

    expect_false(object = file.exists(checksumfile_test))

  }
)

# Test that the function throws a relevant error when the compressed file
# contains the NPX file, the README file and a checksum file, when the value of
# the latter does not match the checksum of the NPX file.
test_that(
  "read NPX zip - NPX, README and wrong checksum",
  {
    readmefile_test <- file.path(tempdir(),
                                 "README.txt")
    writeLines("foo", readmefile_test)
    expect_true(object = file.exists(readmefile_test))

    checksumfile_test <- file.path(tempdir(),
                                   "checksum_sha256.txt")

    withr::with_tempfile(
      new = "nfile_test",
      pattern = "npx",
      fileext = ".csv",
      code = {

        # write the coma-delimited file
        dplyr::tibble("A" = c(1, 2.2, 3.14),
                      "B" = c("a", "b", "c"),
                      "C" = c(TRUE, TRUE, FALSE),
                      "D" = c("NA", "B", NA_character_),
                      "E" = c(1L, 2L, 3L)) |>
          utils::write.table(file = nfile_test,
                             append = FALSE,
                             quote = FALSE,
                             sep = ";",
                             eol = "\n",
                             na = "",
                             dec = ".",
                             row.names = FALSE,
                             col.names = TRUE)

        # check that the parquet file was created
        expect_true(object = file.exists(nfile_test))

        # write something random in checksum
        writeLines("foo", checksumfile_test)

        # check that the checksum file was created
        expect_true(object = file.exists(checksumfile_test))

        withr::with_tempfile(
          new = "zfile_test",
          pattern = "npx",
          fileext = ".zip",
          code = {

            # write zip file
            utils::zip(
              zipfile = zfile_test,
              files = c(readmefile_test,
                        nfile_test,
                        checksumfile_test),
              flags = "-jq"
            )

            # check that the zip file was created
            expect_true(object = file.exists(zfile_test))

            # check that this throws an error. It expexts a sha256 checksum
            # but it gets something else
            expect_error(
              object = read_npx_zip(file = zfile_test),
              regexp = "The checksum of the NPX file does not match the one"
            )

          }
        )

        file.remove(readmefile_test)
        file.remove(checksumfile_test)

      }
    )

    expect_false(object = file.exists(readmefile_test))
    expect_false(object = file.exists(checksumfile_test))

  }
)

# Test that the function throws a relevant error when the compressed file
# contains the NPX file and a checksum file, when the value of the latter does
# not match the checksum of the NPX file.
test_that(
  "read NPX zip - NPX, README and wrong checksum",
  {
    checksumfile_test <- file.path(tempdir(),
                                   "checksum_sha256.txt")

    withr::with_tempfile(
      new = "nfile_test",
      pattern = "npx",
      fileext = ".csv",
      code = {

        # write the coma-delimited file
        dplyr::tibble(
          "A" = c(1, 2.2, 3.14),
          "B" = c("a", "b", "c"),
          "C" = c(TRUE, TRUE, FALSE),
          "D" = c("NA", "B", NA_character_),
          "E" = c(1L, 2L, 3L)
        ) |>
          utils::write.table(
            file = nfile_test,
            append = FALSE,
            quote = FALSE,
            sep = ";",
            eol = "\n",
            na = "",
            dec = ".",
            row.names = FALSE,
            col.names = TRUE
          )

        # check that the parquet file was created
        expect_true(object = file.exists(nfile_test))

        # write something random in checksum
        writeLines("foo", checksumfile_test)

        # check that the checksum file was created
        expect_true(object = file.exists(checksumfile_test))

        withr::with_tempfile(
          new = "zfile_test",
          pattern = "npx",
          fileext = ".zip",
          code = {

            # write zip file
            utils::zip(
              zipfile = zfile_test,
              files = c(nfile_test,
                        checksumfile_test),
              flags = "-jq"
            )

            # check that the zip file was created
            expect_true(object = file.exists(zfile_test))

            # check that this throws an error. It expexts a sha256 checksum
            # but it gets something else
            expect_error(
              object = read_npx_zip(file = zfile_test),
              regexp = "The checksum of the NPX file does not match the one"
            )

          }
        )

        file.remove(checksumfile_test)

      }
    )

    expect_false(object = file.exists(checksumfile_test))

  }
)
