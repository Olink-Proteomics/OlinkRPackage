test_that(
  "data loads correctly - long - parquet",
  {
    withr::with_tempfile(
      new = "tmp_long_parquet",
      pattern = "parquet-long-",
      fileext = ".parquet",
      code = {

        # get the npx data file
        expect_no_error(
          object = npx_file <- system.file("extdata", "npx_data_ext.parquet",
                                           package = "OlinkAnalyze",
                                           mustWork = TRUE)
        )

        # check that the variable was created
        expect_true(object = exists("npx_file"))

        # check that xlsx file can by copied without issues
        expect_no_condition(
          object = file.copy(npx_file, tmp_long_parquet)
        )

        # check that data can be loaded
        expect_no_condition(
          object = npx_df <- read_NPX(filename = tmp_long_parquet,
                                      out_df = "tibble")
        )

        expect_no_condition(
          object = npx_arrow <- read_NPX(filename = tmp_long_parquet,
                                         out_df = "arrow")
        )

        # check that data frame exists
        expect(ok = exists("npx_df"),
               failure_message = "failed to read long paruqet in tibble")
        expect(ok = exists("npx_arrow"),
               failure_message = "failed to read long paruqet in arrow")

        # check that data set has correct number of rows and columns
        expect_equal(object = nrow(npx_df), expected = 1L)
        expect_equal(object = ncol(npx_df), expected = 19L)
        expect_equal(object = nrow(npx_arrow), expected = 1L)
        expect_equal(object = ncol(npx_arrow), expected = 19L)

        # check that dataset has the correct column names
        expect_identical(
          object = colnames(npx_df),
          expected = c("SampleID", "SampleType", "WellID", "PlateID",
                       "DataAnalysisRefID", "OlinkID", "UniProt", "Assay",
                       "AssayType", "Panel", "Block", "Count", "ExtNPX", "NPX",
                       "Normalization", "PCNormalizedNPX", "AssayQC",
                       "SampleQC", "ExploreVersion")
        )
        expect_identical(
          object = names(npx_arrow),
          expected = c("SampleID", "SampleType", "WellID", "PlateID",
                       "DataAnalysisRefID", "OlinkID", "UniProt", "Assay",
                       "AssayType", "Panel", "Block", "Count", "ExtNPX", "NPX",
                       "Normalization", "PCNormalizedNPX", "AssayQC",
                       "SampleQC", "ExploreVersion")
        )

      }
    )
  }
)

test_that(
  "data loads correctly - long - csv",
  {
    withr::with_tempfile(
      new = "tmp_long_csv",
      pattern = "csv-long-",
      fileext = ".csv",
      code = {

        # get the npx data file
        expect_no_error(
          object = npx_file <- system.file("extdata",
                                           "npx_data_long_csv.csv",
                                           package = "OlinkAnalyze",
                                           mustWork = TRUE)
        )

        # check that the variable was created
        expect_true(object = exists("npx_file"))

        # check that xlsx file can by copied without issues
        expect_no_condition(
          object = file.copy(npx_file, tmp_long_csv)
        )

        # check that data can be loaded
        expect_no_condition(
          object = npx_df <- read_NPX(filename = tmp_long_csv,
                                      out_df = "tibble")
        )
        expect_no_condition(
          object = npx_arrow <- read_NPX(filename = tmp_long_csv,
                                         out_df = "arrow")
        )

        # check that data frame exists
        expect(ok = exists("npx_df"),
               failure_message = "failed to read long csv in tibble")
        expect(ok = exists("npx_arrow"),
               failure_message = "failed to read long csv in arrow")

        # check that data set has correct number of rows and columns
        expected_rows <- 1000L
        expected_cols <- 16L
        expect_equal(object = nrow(npx_df), expected = expected_rows)
        expect_equal(object = ncol(npx_df), expected = expected_cols)
        expect_equal(object = nrow(npx_arrow), expected = expected_rows)
        expect_equal(object = ncol(npx_arrow), expected = expected_cols)

        # check that dataset has the correct column names
        expected_colnames <- c("SampleID", "Index", "OlinkID", "UniProt",
                               "Assay", "MissingFreq", "Panel", "Panel_Lot_Nr",
                               "PlateID", "QC_Warning", "LOD", "NPX",
                               "Normalization", "Assay_Warning", "Sample_Type",
                               "ExploreVersion")
        expect_identical(
          object = colnames(npx_df),
          expected = expected_colnames
        )
        expect_identical(
          object = names(npx_arrow),
          expected = expected_colnames
        )

      }
    )
  }
)

test_that(
  "data loads correctly - long - zip",
  {
    skip_if_not_installed("zip")

    withr::with_tempfile(
      new = "tmp_long_csv_zip",
      pattern = "csv-zip-long-",
      fileext = ".zip",
      code = {

        # get the npx data file
        expect_no_error(
          object = npx_file <- system.file("extdata",
                                           "npx_data_long_zip.zip",
                                           package = "OlinkAnalyze",
                                           mustWork = TRUE)
        )

        # check that the variable was created
        expect_true(object = exists("npx_file"))

        # check that xlsx file can by copied without issues
        expect_no_condition(
          object = file.copy(npx_file, tmp_long_csv_zip)
        )

        # check that data can be loaded
        expect_no_condition(
          object = npx_df <- read_NPX(filename = tmp_long_csv_zip,
                                      out_df = "tibble")
        )
        expect_no_condition(
          object = npx_arrow <- read_NPX(filename = tmp_long_csv_zip,
                                         out_df = "arrow")
        )

        # check that data frame exists
        expect(ok = exists("npx_df"),
               failure_message = "failed to read long zip csv in tibble")
        expect(ok = exists("npx_arrow"),
               failure_message = "failed to read long zip csv in arrow")

        # check that data set has correct number of rows and columns
        expected_rows <- 1000L
        expected_cols <- 16L
        expect_equal(object = nrow(npx_df), expected = expected_rows)
        expect_equal(object = ncol(npx_df), expected = expected_cols)
        expect_equal(object = nrow(npx_arrow), expected = expected_rows)
        expect_equal(object = ncol(npx_arrow), expected = expected_cols)

        # check that dataset has the correct column names
        expected_colnames <- c("SampleID", "Index", "OlinkID", "UniProt",
                               "Assay", "MissingFreq", "Panel", "Panel_Lot_Nr",
                               "PlateID", "QC_Warning", "LOD", "NPX",
                               "Normalization", "Assay_Warning", "Sample_Type",
                               "ExploreVersion")
        expect_identical(
          object = colnames(npx_df),
          expected = expected_colnames
        )
        expect_identical(
          object = names(npx_arrow),
          expected = expected_colnames
        )

      }
    )
  }
)

test_that(
  "data loads correctly - wide - xlsx",
  {
    withr::with_tempfile(
      new = "tmp_wide_xlsx",
      pattern = "xlsx-wide-",
      fileext = ".xlsx",
      code = {

        # get the npx data file
        expect_no_error(
          object = npx_file <- system.file("extdata",
                                           "npx_data1.xlsx",
                                           package = "OlinkAnalyze",
                                           mustWork = TRUE)
        )

        # check that the variable was created
        expect_true(object = exists("npx_file"))

        # check that xlsx file can by copied without issues
        expect_no_condition(
          object = file.copy(npx_file, tmp_wide_xlsx)
        )

        # check that data load fails because we cannot determine platform
        expect_error(
          object = read_NPX(filename = tmp_wide_xlsx,
                            out_df = "tibble"),
          regexp = "Unable to recognize the Olink platform from the input file"
        )

        # check that data can be loaded
        expect_message(
          object = expect_warning(
            object = npx_df <- read_NPX(filename = tmp_wide_xlsx,
                                        olink_platform = "Target 96",
                                        out_df = "tibble"),
            regexp = "Unable to recognize the Olink platform from the input"
          ),
          regexp = "Identified 2 duplicates!"
        )
        expect_message(
          object = expect_warning(
            object = npx_arrow <- read_NPX(filename = tmp_wide_xlsx,
                                           olink_platform = "Target 96",
                                           out_df = "arrow"),
            regexp = "Unable to recognize the Olink platform from the input"
          ),
          regexp = "Identified 2 duplicates!"
        )

        # check that data frame exists
        expect(ok = exists("npx_df"),
               failure_message = "failed to read wide xlsx in tibble")
        expect(ok = exists("npx_arrow"),
               failure_message = "failed to read wide xlsx in arrow")

        # check that data set has correct number of rows and columns
        expected_rows <- 176640L
        expected_cols <- 12L
        expect_equal(object = nrow(npx_df), expected = expected_rows)
        expect_equal(object = ncol(npx_df), expected = expected_cols)
        expect_equal(object = nrow(npx_arrow), expected = expected_rows)
        expect_equal(object = ncol(npx_arrow), expected = expected_cols)

        # check that dataset has the correct column names
        expected_colnames <- c("SampleID", "NPX", "Panel", "Assay", "UniProt",
                               "OlinkID", "Panel_Version", "PlateID",
                               "QC_Warning", "LOD", "MissingFreq",
                               "Olink NPX Signature Version")
        expect_identical(
          object = colnames(npx_df),
          expected = expected_colnames
        )
        expect_identical(
          object = names(npx_arrow),
          expected = expected_colnames
        )

      }
    )
  }
)

test_that(
  "data loads correctly - wide - csv",
  {
    withr::with_tempfile(
      new = "tmp_wide_csv",
      pattern = "csv-wide-",
      fileext = ".csv",
      code = {

        # get the npx data file
        expect_no_error(
          object = npx_file <- system.file("extdata",
                                           "npx_data_wide_csv.csv",
                                           package = "OlinkAnalyze",
                                           mustWork = TRUE)
        )

        # check that the variable was created
        expect_true(object = exists("npx_file"))

        # check that xlsx file can by copied without issues
        expect_no_condition(
          object = file.copy(npx_file, tmp_wide_csv)
        )

        # check that data load fails because we cannot determine platform
        expect_error(
          object = read_NPX(filename = tmp_wide_csv,
                            out_df = "tibble"),
          regexp = "Unable to recognize the Olink platform from the input file"
        )

        # check that data can be loaded
        expect_message(
          object = expect_warning(
            object = npx_df <- read_NPX(filename = tmp_wide_csv,
                                        olink_platform = "Target 96",
                                        out_df = "tibble"),
            regexp = "Unable to recognize the Olink platform from the input"
          ),
          regexp = "Identified 2 duplicates!"
        )
        expect_message(
          object = expect_warning(
            object = npx_arrow <- read_NPX(filename = tmp_wide_csv,
                                           olink_platform = "Target 96",
                                           out_df = "arrow"),
            regexp = "Unable to recognize the Olink platform from the input"
          ),
          regexp = "Identified 2 duplicates!"
        )

        # check that data frame exists
        expect(ok = exists("npx_df"),
               failure_message = "failed to read wide csv in tibble")
        expect(ok = exists("npx_arrow"),
               failure_message = "failed to read wide csv in arrow")

        # check that data set has correct number of rows and columns
        expected_rows <- 29440L
        expected_cols <- 12L
        expect_equal(object = nrow(npx_df), expected = expected_rows)
        expect_equal(object = ncol(npx_df), expected = expected_cols)
        expect_equal(object = nrow(npx_arrow), expected = expected_rows)
        expect_equal(object = ncol(npx_arrow), expected = expected_cols)

        # check that dataset has the correct column names
        expected_colnames <- c("SampleID", "NPX", "Panel", "Assay", "UniProt",
                               "OlinkID", "Panel_Version", "PlateID",
                               "QC_Warning", "LOD", "MissingFreq",
                               "Olink NPX Signature Version")
        expect_identical(
          object = colnames(npx_df),
          expected = expected_colnames
        )
        expect_identical(
          object = names(npx_arrow),
          expected = expected_colnames
        )

      }
    )
  }
)

test_that(
  "data loads correctly - legacy - wide - xlsx",
  {
    withr::with_tempfile(
      new = "tmp_wide_xlsx",
      pattern = "xlsx-wide-",
      fileext = ".xlsx",
      code = {

        # get the npx data file
        expect_no_error(
          object = npx_file <- system.file("extdata",
                                           "npx_data1.xlsx",
                                           package = "OlinkAnalyze",
                                           mustWork = TRUE)
        )

        # check that the variable was created
        expect_true(object = exists("npx_file"))

        # check that xlsx file can by copied without issues
        expect_no_condition(
          object = file.copy(npx_file, tmp_wide_xlsx)
        )

        # check that data can be loaded
        expect_warning(
          object = npx_df <- read_NPX(filename = tmp_wide_xlsx,
                                      olink_platform = "Target 96",
                                      out_df = "tibble",
                                      long_format = FALSE,
                                      data_type = "NPX",
                                      legacy = TRUE,
                                      quiet = TRUE),
          regexp = "Unable to recognize the Olink platform from the input"
        )

        # check that data frame exists
        expect(ok = exists("npx_df"),
               failure_message = "failed to read wide xlsx in tibble")

        # check that data set has correct number of rows and columns
        expected_rows <- 176640L
        expected_cols <- 12L
        expect_equal(object = nrow(npx_df), expected = expected_rows)
        expect_equal(object = ncol(npx_df), expected = expected_cols)

        # check that dataset has the correct column names
        expected_colnames <- c("SampleID", "Index", "OlinkID", "UniProt",
                               "Assay", "MissingFreq", "Panel", "Panel_Version",
                               "PlateID", "QC_Warning", "LOD", "NPX")
        expect_identical(
          object = colnames(npx_df),
          expected = expected_colnames
        )

      }
    )
  }
)
