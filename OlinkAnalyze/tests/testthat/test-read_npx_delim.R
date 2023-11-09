# Test that relevant error is thrown when file is not a string
test_that(
  "read NPX csv error - file not a string or missing",
  {

    expect_error(
      read_npx_delim(file = c("A", "B")),
      regexp = "`file` must be a string!"
    )

    expect_error(
      read_npx_delim(file = character()),
      regexp = "`file` must be a string!"
    )

    expect_error(
      read_npx_delim(file = 1),
      regexp = "`file` must be a string!"
    )


    expect_error(
      read_npx_delim(file = TRUE),
      regexp = "`file` must be a string!"
    )

    csvfile_test <- character()

    withr::with_tempfile(
      new = "cfile_test",
      pattern = "csv_temp_file",
      fileext = ".csv",
      code = {

        expect_false(file.exists(cfile_test))

        expect_error(
          read_npx_delim(file = cfile_test),
          regexp = "Unable to locate file:"
        )

        csv_file_test <<- cfile_test
      }
    )

    expect_false(file.exists(csv_file_test))
  }
)

# Test that relevant error is thrown when sep is not a string
test_that(
  "read NPX csv error - sep not a string",
  {

    csvfile_test <- character()

    withr::with_tempfile(
      new = "cfile_test",
      pattern = "csv_temp_file",
      fileext = ".csv",
      code = {

        writeLines("foo", cfile_test)
        expect_true(file.exists(cfile_test))

        expect_error(
          read_npx_delim(file = cfile_test,
                       sep = c(",", ";")),
          regexp = "\"sep\" should be a string!"
        )

        expect_error(
          read_npx_delim(file = cfile_test,
                       sep = character()),
          regexp = "\"sep\" should be a string!"
        )

        expect_error(
          read_npx_delim(file = cfile_test,
                       sep = 1),
          regexp = "\"sep\" should be a string!"
        )


        expect_error(
          read_npx_delim(file = cfile_test,
                       sep = TRUE),
          regexp = "\"sep\" should be a string!"
        )

        csv_file_test <<- cfile_test
      }
    )

    expect_false(file.exists(csv_file_test))
  }
)

# Test that relevant error is thrown when sep is not a string
test_that(
  "read NPX csv error - sep not accepted",
  {

    npx_csv_file <- system.file("extdata",
                                "Example_NPX_Data2_1.csv",
                                package = "OlinkAnalyze",
                                mustWork = TRUE)

    expect_error(
      read_npx_delim(file = npx_csv_file,
                   sep = "I_Am_Unaccepted"),
      regexp = "Unexpected separator:"
    )

    expect_error(
      read_npx_delim(file = npx_csv_file,
                   sep = "A"),
      regexp = "Unexpected separator:"
    )

    expect_error(
      read_npx_delim(file = npx_csv_file,
                   sep = "#"),
      regexp = "Unexpected separator:"
    )


    expect_error(
      read_npx_delim(file = npx_csv_file,
                   sep = "|"),
      regexp = "Unexpected separator:"
    )
  }
)


