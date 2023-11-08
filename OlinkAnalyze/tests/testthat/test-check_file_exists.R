# Test that the function throws no errors, warning or messages when the file
# exists
test_that(
  "check file exists works - file present", {

    parquet_file <- system.file("extdata",
                                "npx_data_ext.parquet",
                                package = "OlinkAnalyze",
                                mustWork = TRUE)

    expect_no_condition(
      object = check_file_exists(
        file = parquet_file
      )
    )
  }
)

# Test that relevant error is thrown when file is NULL or NA
test_that(
  "check file exists works - file NA or NULL", {

    expect_error(
      object = check_file_exists(
        file = NULL
      ),
      regexp = "File cannot be NULL"
    )

    expect_error(
      object = check_file_exists(
        file = NA
      ),
      regexp = "File cannot be NA"
    )
  }
)

# Test that relevant error is thrown when file(s) is/are missing
test_that(
  "check file exists works - file NA or NULL", {

    missing_file_1 <- file.path(tempdir(),
                                "I_Am_A_MissinG_FilE_1")

    missing_file_2 <- file.path(tempdir(),
                                "I_Am_A_MissinG_FilE_2")

    parquet_file <- system.file("extdata",
                                "npx_data_ext.parquet",
                                package = "OlinkAnalyze",
                                mustWork = TRUE)

    expect_error(
      object = check_file_exists(
        file = missing_file_1
      ),
      regexp = "Unable to locate file"
    )

    expect_error(
      object = check_file_exists(
        file = c(missing_file_1, missing_file_2)
      ),
      regexp = "Unable to locate files"
    )

    expect_error(
      object = check_file_exists(
        file = c(missing_file_1, missing_file_2, parquet_file)
      ),
      regexp = "Unable to locate files"
    )

  }
)
