test_that(
  "Non-zip input is handled - random file",
  {
    textfile_zip <- character()

    withr::with_tempfile(
      new = "txtfile_z",
      pattern = "txt-file_as_zip-file",
      fileext = ".txt",
      code = {

        # write some text in a txt file
        writeLines("foo", txtfile_z)

        # check that the parquet file was created
        expect(
          file.exists(txtfile_z),
          failure_message = "Text file was not created!"
        )

        # check that relevant error is thrown
        expect_error(
          read_npx_zip(file = txtfile_z),
          regexp = "Unable to open zip file: "
        )

        textfile_zip <<- txtfile_z
      }
    )

    expect_false(file.exists(textfile_zip))
  }
)

test_that(
  "Non-zip input is handled - corrupt zip file",
  {
    txtfile_zipcorrupt <- character()

    withr::with_tempfile(
      new = "txtfile_zcorrupt",
      pattern = "txt-file_as_corrupt-zip-file",
      fileext = ".parquet",
      code = {

        # write some text in a txt file
        writeLines("foo", txtfile_zcorrupt)

        # check that the parquet file was created
        expect(
          file.exists(txtfile_zcorrupt),
          failure_message = "Corrupt zip file was not created!"
        )

        # check that relevant error is thrown
        expect_error(
          read_npx_zip(file = txtfile_zcorrupt),
          regexp = "Unable to open zip file: "
        )

        txtfile_zipcorrupt <<- txtfile_zcorrupt
      }
    )

    expect_false(file.exists(txtfile_zipcorrupt))
  }
)

test_that(
  "read NPX zip - no checsum or NPX file",
  {
    readme_file <- file.path(tempdir(),
                             "README.txt")

    writeLines("foo", readme_file)

    expect_true(file.exists(readme_file))

    zip_file <- character()

    withr::with_tempfile(
      new = "zip_test",
      pattern = "zip_test",
      fileext = ".zip",
      code = {
        utils::zip(zipfile = zip_test,
                   files = c(readme_file),
                   flags = "-jq")

        expect_true(file.exists(zip_test))

        expect_error(
          read_npx_zip(
            file = zip_test
          ),
          regexp = "The compressed file does not contain checksum or NPX files!"
        )

        zip_file <<- zip_test
      }
    )

    file.remove(readme_file)

    expect_false(file.exists(zip_file))
    expect_false(file.exists(readme_file))
  }
)
