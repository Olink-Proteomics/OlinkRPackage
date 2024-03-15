# Test read_npx_zip ----

# Test that if the input file has been misidentified as zip compressed but it is
# not (e.g. .txt), then a relevant error is thrown
test_that(
  "Non-zip input is handled - random file",
  {
    skip_if_not_installed("zip")

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
    skip_if_not_installed("zip")

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
    skip_if_not_installed("zip")

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
    skip_if_not_installed("zip")

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
            # warning comes from read_npx_format because the long file is
            # totally made up.
            expect_warning(
              object = read_npx_zip(file = zfile_test,
                                    out_df = "arrow",
                                    long_format = TRUE,
                                    quiet = TRUE),
              regexp = "Unable to confirm the \"long\" format from the input"
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
    skip_if_not_installed("zip")

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
            # warning comes from read_npx_format because the long file is
            # totally made up.
            expect_warning(
              object = read_npx_zip(file = zfile_test,
                                    out_df = "arrow",
                                    long_format = TRUE,
                                    quiet = TRUE),
              regexp = "Unable to confirm the \"long\" format from the input"
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
    skip_if_not_installed("zip")

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
                # warning comes from read_npx_format because the long file is
                # totally made up.
                expect_warning(
                  object = read_npx_zip(file = zfile_test,
                                        out_df = "arrow",
                                        long_format = TRUE,
                                        quiet = TRUE),
                  regexp = "Unable to confirm the \"long\" format from the inpu"
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
    skip_if_not_installed("zip")

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
                # warning comes from read_npx_format because the long file is
                # totally made up.
                expect_warning(
                  object = read_npx_zip(file = zfile_test,
                                        out_df = "arrow",
                                        long_format = TRUE,
                                        .ignore_files =
                                          c(basename(readmefile_test),
                                            basename(rfile_test)),
                                        quiet = TRUE),
                  regexp = "Unable to confirm the \"long\" format from the inpu"
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
    skip_if_not_installed("zip")

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
                # warning comes from read_npx_format because the long file is
                # totally made up.
                expect_warning(
                  object = read_npx_zip(file = zfile_test,
                                        out_df = "arrow",
                                        long_format = TRUE,
                                        .ignore_files =
                                          c(basename(readmefile_test),
                                            basename(rfile_test)),
                                        quiet = TRUE),
                  regexp = "Unable to confirm the \"long\" format from the inpu"
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
    skip_if_not_installed("zip")

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
            # warning comes from read_npx_format because the long file is
            # totally made up.
            expect_warning(
              object = df_out_arrow <- read_npx_zip(file = zfile_test,
                                                    out_df = "arrow",
                                                    sep = NULL,
                                                    long_format = TRUE,
                                                    quiet = TRUE),
              regexp = "Unable to confirm the \"long\" format from the inpu"
            )

            expect_true(exists("df_out_arrow"))

            expect_true(inherits(x = df_out_arrow, what = "ArrowObject"))

            expect_warning(
              object = df_out_tibble <- read_npx_zip(file = zfile_test,
                                                     out_df = "tibble",
                                                     sep = NULL,
                                                     long_format = TRUE,
                                                     quiet = TRUE),
              regexp = "Unable to confirm the \"long\" format from the inpu"
            )

            expect_true(exists("df_out_tibble"))

            expect_true(inherits(x = df_out_tibble, what = "tbl_df"))

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
    skip_if_not_installed("zip")

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
            # warning comes from read_npx_format because the long file is
            # totally made up.
            expect_warning(
              object = df_out_arrow <- read_npx_zip(file = zfile_test,
                                                    out_df = "arrow",
                                                    long_format = TRUE,
                                                    quiet = TRUE),
              regexp = "Unable to confirm the \"long\" format from the inpu"
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
    skip_if_not_installed("zip")

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
        cli::hash_file_sha256(paths = nfile_test) |>
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
            # warning comes from read_npx_format because the long file is
            # totally made up.
            expect_warning(
              object = df_out_arrow <- read_npx_zip(file = zfile_test,
                                                    out_df = "arrow",
                                                    sep = NULL,
                                                    long_format = TRUE,
                                                    quiet = TRUE),
              regexp = "Unable to confirm the \"long\" format from the inpu"
            )

            expect_true(exists("df_out_arrow"))

            expect_true(inherits(x = df_out_arrow, what = "ArrowObject"))

            expect_warning(
              object = df_out_tibble <- read_npx_zip(file = zfile_test,
                                                     out_df = "tibble",
                                                     sep = NULL,
                                                     long_format = TRUE,
                                                     quiet = TRUE),
              regexp = "Unable to confirm the \"long\" format from the inpu"
            )

            expect_true(exists("df_out_tibble"))

            expect_true(inherits(x = df_out_tibble, what = "tbl_df"))

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
    skip_if_not_installed("zip")

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
        cli::hash_file_sha256(paths = nfile_test) |>
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
            # warning comes from read_npx_format because the long file is
            # totally made up.
            expect_warning(
              object = read_npx_zip(file = zfile_test,
                                    out_df = "arrow",
                                    long_format = TRUE,
                                    quiet = TRUE),
              regexp = "Unable to confirm the \"long\" format from the inpu"
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
    skip_if_not_installed("zip")

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
    skip_if_not_installed("zip")

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

# Test check_checksum ----

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
        expect_true(object = file.exists(text_file_test))

        # MD5 checksum on the NPX file
        text_file_checksum <- cli::hash_file_md5(paths = text_file_test)

        # write some text in a txt file
        writeLines(text_file_checksum, MD5_check)

        # check that the parquet file was created
        expect_true(object = file.exists(MD5_check))

        # check that relevant error is thrown
        expect_no_condition(
          object = check_checksum(checksum_file = MD5_check,
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
        expect_true(object = file.exists(text_file_test))

        # write sha256 in the txt file
        cli::hash_file_sha256(paths = text_file_test) |>
          writeLines(SHA256_check)

        # check that the parquet file was created
        expect_true(object = file.exists(SHA256_check))

        # check that relevant error is thrown
        expect_no_condition(
          object = check_checksum(checksum_file = SHA256_check,
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
        expect_false(object = file.exists(nfile_test))

        withr::with_tempfile(
          new = "SHA256_check",
          pattern = "SHA256",
          fileext = ".txt",
          code = {

            # write in the checksum file
            writeLines("I_AM_A_R4ND0M_ChEcKuP", SHA256_check)

            # check that the parquet file was created
            expect_true(object = file.exists(SHA256_check))

            # check that relevant string is returned
            expect_error(
              object = check_checksum(checksum_file = SHA256_check,
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
        expect_true(object = file.exists(text_file_test))

        # check that the parquet file was created
        expect_false(object = file.exists(SHA256_check))

        # check that relevant string is returned
        expect_error(
          object = check_checksum(checksum_file = SHA256_check,
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
        expect_true(object = file.exists(text_file_test))

        # write some text in a txt file
        writeLines("I_AM_A_R4ND0M_ChEcKuP", SHA256_check)

        # check that the parquet file was created
        expect_true(object = file.exists(SHA256_check))

        # check that relevant string is returned
        expect_error(
          object = check_checksum(checksum_file = SHA256_check,
                                  npx_file = text_file_test),
          regexp = "The checksum of the NPX file does not match the one"
        )

      }
    )

  }
)

# Test get_checksum_file ----

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

# Test get_npx_file ----

## test all realistic scenario combos of checksum files and NPX files ----

# Test get_npx_file for slightly different inputs. Specifically,
# checking for all combos of checksum file and NPX file
test_checksum_npx_combo <- function(c_file, n_file) {
  test_that(
    paste("Testing get NPX file from zip with:", c_file, "and", n_file), {
      expect_equal(
        object = get_npx_file(files = c(c_file, n_file)),
        expected = n_file
      )
    }
  )
}

invisible(
  sapply(
    accepted_checksum_files,
    function(checksum_file) {
      sapply(
        paste0("test.", accepted_npx_file_ext[accepted_npx_file_ext != "zip"]),
        function(npx_file) {
          test_checksum_npx_combo(c_file = checksum_file, n_file = npx_file)
        }
      )
    }
  )
)

rm(test_checksum_npx_combo)

## test all realistic scenario of NPX files input only ----

# Test get_npx_file for slightly different inputs. Specifically,
# checking for all NPX files.
test_npx_input <- function(n_file) {
  test_that(
    paste("Testing get NPX file from zip with:", n_file), {
      expect_equal(
        object = get_npx_file(files = n_file),
        expected = n_file
      )
    }
  )
}

invisible(
  sapply(
    paste0("test.", accepted_npx_file_ext[accepted_npx_file_ext != "zip"]),
    function(npx_file) {
      test_npx_input(n_file = npx_file)
    }
  )
)

rm(test_npx_input)

## Test edge cases ----

# Test that relevant error is thrown when only checksum files are provided.
test_that(
  "get NPX file from zip - only checksums",
  {
    # one MD5 only
    expect_error(
      object = get_npx_file(
        files = c("MD5_checksum.txt")
      ),
      regexp = "The compressed file contains no acceptable files!"
    )

    # two MD5
    expect_error(
      object = get_npx_file(
        files = c("MD5_checksum.txt", "MD5_checksum.txt")
      ),
      regexp = "The compressed file contains no acceptable files!"
    )

    # one SHA256 only
    expect_error(
      object = get_npx_file(
        files = c("checksum_sha256.txt")
      ),
      regexp = "The compressed file contains no acceptable files!"
    )

    # two SHA256
    expect_error(
      object = get_npx_file(
        files = c("checksum_sha256.txt", "checksum_sha256.txt")
      ),
      regexp = "The compressed file contains no acceptable files!"
    )
  }
)

# Test that relevant error is thrown when unknown files are provided.
test_that(
  "get NPX file from zip - no known file",
  {
    # one unknown file
    expect_error(
      object = get_npx_file(
        files = c("test.xml")
      ),
      regexp = "The compressed file contains no acceptable files!"
    )

    # two unknown files
    expect_error(
      object = get_npx_file(
        files = c("test.xml", "test.yaml")
      ),
      regexp = "The compressed file contains no acceptable files!"
    )
  }
)

# Test that relevant error is thrown when 2 acceptable files are provided.
test_that(
  "get NPX file from zip - many known files v1",
  {
    # A txt and a csv file
    expect_error(
      object = get_npx_file(
        files = c("test.txt", "test.csv")
      ),
      regexp = "The compressed file contains multiple acceptable files!"
    )

    # A parquet and a csv file
    expect_error(
      object = get_npx_file(
        files = c("test.parquet", "test.csv")
      ),
      regexp = "The compressed file contains multiple acceptable files!"
    )

    # An xlsx and a csv file
    expect_error(
      object = get_npx_file(
        files = c("test.xlsx", "test.csv")
      ),
      regexp = "The compressed file contains multiple acceptable files!"
    )

    # An xls and a csv file
    expect_error(
      object = get_npx_file(
        files = c("test.xls", "test.csv")
      ),
      regexp = "The compressed file contains multiple acceptable files!"
    )

    # An zip and a csv file
    expect_error(
      object = get_npx_file(
        files = c("test.zip", "test.csv")
      ),
      regexp = "The compressed file contains multiple acceptable files!"
    )
  }
)

test_that(
  "get NPX file from zip - nested compressed file",
  {

    # A zip file only
    expect_error(
      object = get_npx_file(
        files = c("test.zip")
      ),
      regexp = "The compressed file contains another compressed file:"
    )

  }
)
