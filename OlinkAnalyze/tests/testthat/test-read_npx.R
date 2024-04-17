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
          object = npx_file <- system.file("extdata", "Example_NPX_Data2_1.csv",
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
        expect_equal(object = nrow(npx_df), expected = 1L)
        expect_equal(object = ncol(npx_df), expected = 13L)
        expect_equal(object = nrow(npx_arrow), expected = 1L)
        expect_equal(object = ncol(npx_arrow), expected = 13L)

        # check that dataset has the correct column names
        expect_identical(
          object = colnames(npx_df),
          expected = c("SampleID", "Index", "OlinkID", "UniProt", "Assay",
                       "MissingFreq", "Panel", "Panel_Lot_Nr", "PlateID",
                       "QC_Warning", "LOD", "NPX", "Normalization")
        )
        expect_identical(
          object = names(npx_arrow),
          expected = c("SampleID", "Index", "OlinkID", "UniProt", "Assay",
                       "MissingFreq", "Panel", "Panel_Lot_Nr", "PlateID",
                       "QC_Warning", "LOD", "NPX", "Normalization")
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
          object = npx_file <- system.file("extdata", "npx_data_v3.zip",
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
        expect_equal(object = nrow(npx_df), expected = 1000L)
        expect_equal(object = ncol(npx_df), expected = 16L)
        expect_equal(object = nrow(npx_arrow), expected = 1000L)
        expect_equal(object = ncol(npx_arrow), expected = 16L)

        # check that dataset has the correct column names
        expect_identical(
          object = colnames(npx_df),
          expected = c("SampleID", "Index", "OlinkID", "UniProt", "Assay",
                       "MissingFreq", "Panel", "Panel_Lot_Nr", "PlateID",
                       "QC_Warning", "LOD", "NPX", "Normalization",
                       "Assay_Warning", "Sample_Type", "ExploreVersion")
        )
        expect_identical(
          object = names(npx_arrow),
          expected = c("SampleID", "Index", "OlinkID", "UniProt", "Assay",
                       "MissingFreq", "Panel", "Panel_Lot_Nr", "PlateID",
                       "QC_Warning", "LOD", "NPX", "Normalization",
                       "Assay_Warning", "Sample_Type", "ExploreVersion")
        )

      }
    )
  }
)

test_that(
  "data does not load - unrecognizable file extension",
  {
    withr::with_tempfile(
      new = "tmp_unknown_file",
      pattern = "test-random-file",
      fileext = ".yaml",
      code = {

        # write in the file
        writeLines("foo", tmp_unknown_file)

        # check that file exists
        expect_true(object = file.exists(tmp_unknown_file))

        # check that data can be loaded
        expect_error(
          object = read_NPX(filename = tmp_unknown_file),
          regexp = "Unable to recognize format from file extension!"
        )

      }
    )
  }
)
