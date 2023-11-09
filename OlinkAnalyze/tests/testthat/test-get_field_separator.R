# Test that the function returns a semicolon when the file is separated by
# semicolon.
test_that(
  "check get_field_separator works - semicolon",
  {

    textfile_semicolon <- character()

    withr::with_tempfile(
      new = "tfile_semicolon",
      pattern = "txt-file_semicolon",
      fileext = ".txt",
      code = {

        # write some text in a txt file
        writeLines("1;2;3;4;5", tfile_semicolon)

        # check that the file was created
        expect_true(file.exists(tfile_semicolon))

        # check that relevant error is thrown
        expect_identical(
          get_field_separator(file = tfile_semicolon),
          ";"
        )

        textfile_semicolon <<- tfile_semicolon
      }
    )

    expect_false(file.exists(textfile_semicolon))

  }
)

# Test that the function returns a comma when the file is separated by comma.
test_that(
  "check get_field_separator works - comma",
  {

    textfile_comma <- character()

    withr::with_tempfile(
      new = "tfile_comma",
      pattern = "txt-file_comma",
      fileext = ".txt",
      code = {

        # write some text in a txt file
        writeLines("1,2,3,4,5", tfile_comma)

        # check that the file was created
        expect_true(file.exists(tfile_comma))

        # check that relevant error is thrown
        expect_identical(
          get_field_separator(file = tfile_comma),
          ","
        )

        textfile_comma <<- tfile_comma
      }
    )

    expect_false(file.exists(textfile_comma))

  }
)

# Test that the relevant error is thrown when unexpected separator in file.
test_that(
  "check get_field_separator works - hashtag",
  {

    textfile_hashtag <- character()

    withr::with_tempfile(
      new = "tfile_hashtag",
      pattern = "txt-file_hashtag",
      fileext = ".txt",
      code = {

        # write some text in a txt file
        writeLines("1#2#3#4#5", tfile_hashtag)

        # check that the file was created
        expect_true(file.exists(tfile_hashtag))

        # check that relevant error is thrown
        expect_error(
          get_field_separator(file = tfile_hashtag),
          regexp = "Expecting semicolon.*.or comma.*.!"
        )

        textfile_hashtag <<- tfile_hashtag
      }
    )

    expect_false(file.exists(textfile_hashtag))

  }
)

# Test that the relevant error is thrown when unexpected separator in file.
test_that(
  "check get_field_separator works - both comma and semicolon",
  {

    textfile_mixed <- character()

    withr::with_tempfile(
      new = "tfile_mixed",
      pattern = "txt-file_mixed",
      fileext = ".txt",
      code = {

        # write some text in a txt file
        writeLines("1,2;3,4;5", tfile_mixed)

        # check that the file was created
        expect_true(file.exists(tfile_mixed))

        # check that relevant error is thrown
        expect_error(
          get_field_separator(file = tfile_mixed),
          regexp = "Both semicolon.*.and comma.*.are present in header line."
        )

        textfile_mixed <<- tfile_mixed
      }
    )

    expect_false(file.exists(textfile_mixed))

  }
)
