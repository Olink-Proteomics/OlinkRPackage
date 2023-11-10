# Test that the function returns TRUE when the file exists
test_that(
  "check file exists works - file present", {

    withr::with_tempfile(
      new = "tfile_test",
      pattern = "text-file-test",
      fileext = ".csv",
      code = {

        # write soemthing to file
        writeLines("foo", tfile_test)

        # check if file exists
        expect_true(file.exists(tfile_test))

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

# Test that relevant error is thrown when file is missing
test_that(
  "check file exists works - file missing", {

    withr::with_tempfile(
      new = "tfile_test",
      pattern = "text-file-test",
      fileext = ".csv",
      code = {

        # check if file exists
        expect_false(file.exists(tfile_test))

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
