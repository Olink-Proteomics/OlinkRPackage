# Test that the function throws no errors, warning or messages when the file
# exists
test_that(
  "check file exists works - file present", {

    # path to the sample parquet file
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

# Test that relevant error is thrown when file is missing
test_that(
  "check file exists works - file missing", {

    missing_file <- file.path(tempdir(),
                              "I_Am_A_MissinG_FilE")

    expect_error(
      object = check_file_exists(
        file = missing_file
      ),
      regexp = "Unable to locate file"
    )

  }
)
