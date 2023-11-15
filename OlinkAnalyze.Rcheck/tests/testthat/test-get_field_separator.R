# Test that the header line of the file is not empty or the file is empty
test_that(
  "check get_field_separator works - check empty haeder or file",
  {

    withr::with_tempfile(
      new = "tfile_empty",
      pattern = "txt-file_semicolon",
      fileext = ".txt",
      code = {

        # write some text in a txt file
        writeLines(character(), tfile_empty)

        # check that the file was created
        expect_true(object = file.exists(tfile_empty))

        # check that relevant error is thrown
        expect_error(
          object = get_field_separator(file = tfile_empty),
          regexp = "Unable to read header line from"
        )

      }
    )

  }
)

# Test that the function returns a semicolon when the file is separated by
# semicolon.
test_that(
  "check get_field_separator works - semicolon",
  {

    withr::with_tempfile(
      new = "tfile_semicolon",
      pattern = "txt-file_semicolon",
      fileext = ".txt",
      code = {

        # write some text in a txt file
        writeLines("1;2;3;4;5", tfile_semicolon)

        # check that the file was created
        expect_true(object = file.exists(tfile_semicolon))

        # check that relevant error is thrown
        expect_identical(
          object = get_field_separator(file = tfile_semicolon),
          expected = ";"
        )

      }
    )

  }
)

# Test that the function returns a comma when the file is separated by comma.
test_that(
  "check get_field_separator works - comma",
  {

    withr::with_tempfile(
      new = "tfile_comma",
      pattern = "txt-file_comma",
      fileext = ".txt",
      code = {

        # write some text in a txt file
        writeLines("1,2,3,4,5", tfile_comma)

        # check that the file was created
        expect_true(object = file.exists(tfile_comma))

        # check that relevant error is thrown
        expect_identical(
          object = get_field_separator(file = tfile_comma),
          expected = ","
        )

      }
    )

  }
)

# Test that the relevant error is thrown when unexpected separator in file.
test_that(
  "check get_field_separator works - hashtag",
  {

    withr::with_tempfile(
      new = "tfile_hashtag",
      pattern = "txt-file_hashtag",
      fileext = ".txt",
      code = {

        # write some text in a txt file
        writeLines("1#2#3#4#5", tfile_hashtag)

        # check that the file was created
        expect_true(object = file.exists(tfile_hashtag))

        # check that relevant error is thrown
        expect_error(
          object = get_field_separator(file = tfile_hashtag),
          regexp = "Expecting semicolon.*.or comma.*.!"
        )

      }
    )

  }
)

# Test that the relevant error is thrown when unexpected separator in file.
test_that(
  "check get_field_separator works - both comma and semicolon",
  {

    withr::with_tempfile(
      new = "tfile_mixed",
      pattern = "txt-file_mixed",
      fileext = ".txt",
      code = {

        # write some text in a txt file
        writeLines("1,2;3,4;5", tfile_mixed)

        # check that the file was created
        expect_true(object = file.exists(tfile_mixed))

        # check that relevant error is thrown
        expect_error(
          object = get_field_separator(file = tfile_mixed),
          regexp = "Both semicolon.*.and comma.*.are present in header line."
        )

      }
    )

  }
)
