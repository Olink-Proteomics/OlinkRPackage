# Test check_file_exists ----
test_that(
  "check_file_exists - works - file present", {

    withr::with_tempfile(
      new = "tfile_test",
      pattern = "text-file-test",
      fileext = ".csv",
      code = {

        # write soemthing to file
        writeLines("foo", tfile_test)

        # check if file exists
        expect_true(object = file.exists(tfile_test))

        # expect TRUE to return
        expect_true(
          object = check_file_exists(
            file = tfile_test,
            error = FALSE
          )
        )

        # expect TRUE to return
        expect_true(
          object = check_file_exists(
            file = tfile_test,
            error = TRUE
          )
        )

      }
    )

  }
)

test_that(
  "check_file_exists - error - file missing", {

    withr::with_tempfile(
      new = "tfile_test",
      pattern = "text-file-test",
      fileext = ".csv",
      code = {

        # check if file exists
        expect_false(object = file.exists(tfile_test))

        # expect TRUE to return
        expect_false(
          object = check_file_exists(
            file = tfile_test,
            error = FALSE
          )
        )

        # expect TRUE to return
        expect_error(
          object = check_file_exists(
            file = tfile_test,
            error = TRUE
          ),
          regexp = "Unable to locate file"
        )

      }
    )

  }
)

# Test check_file_extension ----

test_that(
  "check_file_extension - works",
  {
    expect_equal(
      object = check_file_extension(file = "file.xls"),
      expected = "excel_1"
    )

    expect_equal(
      object = check_file_extension(file = "file.xlsx"),
      expected = "excel_2"
    )

    expect_equal(
      object = check_file_extension(file = "file.csv"),
      expected = "delim_1"
    )

    expect_equal(
      object = check_file_extension(file = "file.txt"),
      expected = "delim_2"
    )

    expect_equal(
      object = check_file_extension(file = "file.parquet"),
      expected = "parquet_1"
    )

    expect_equal(
      object = check_file_extension(file = "file.zip"),
      expected = "compressed_1"
    )
  }
)

test_that(
  "check_file_extension - error",
  {
    expect_error(
      object = check_file_extension(file = "file.yaml"),
      regexp = "Unable to recognize the extension of the file"
    )

    expect_error(
      object = check_file_extension(file = "file"),
      regexp = "Unable to recognize the extension of the file"
    )
  }
)
