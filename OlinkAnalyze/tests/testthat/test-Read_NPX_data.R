test_that("Data loads correctly with 'read_NPX()'", {
  #Read data
  npx_file <- system.file("extdata", "npx_data1.xlsx", package = "OlinkAnalyze", mustWork = TRUE)
  manifest_file <- system.file("extdata", "npx_data1_meta.csv", package = "OlinkAnalyze", mustWork = TRUE)
  parquet_file <- system.file("extdata", "npx_data_ext.parquet",
                              package = "OlinkAnalyze", mustWork = TRUE)
  df_1 <- read_NPX(filename = npx_file) # load dataset 1
  manifest_1 <- read.delim(manifest_file, header = TRUE, sep = ';')

  # load manifest 1
  df_2_2 <- read_NPX(system.file("extdata", "Example_NPX_Data2_1.csv", package = "OlinkAnalyze", mustWork = TRUE))
  if (requireNamespace("openssl", quietly = TRUE)
      & requireNamespace("zip", quietly = TRUE)) {
    npx_file_v3 <- system.file("extdata", "npx_data_v3.zip", package = "OlinkAnalyze", mustWork = TRUE)
    df_v3 <- read_NPX(filename = npx_file_v3)
    npx_file_ext_v1 <- system.file("extdata", "npx_data_ext_v1.zip", package = "OlinkAnalyze", mustWork = TRUE)
    df_ext_v1 <- read_NPX(filename = npx_file_ext_v1)
    npx_file_ext_v2 <- system.file("extdata", "npx_data_ext_v2.zip", package = "OlinkAnalyze", mustWork = TRUE)
    df_ext_v2 <- read_NPX(filename = npx_file_ext_v2)
  }
  if (requireNamespace("arrow", quietly = TRUE) ){
    df_parquet <- read_NPX(filename = parquet_file)
  }


  #NPX read ok?
  expect(exists("df_1"), failure_message = "read_NPX failed on dataset 1")
  expect_s3_class(df_1, class = "tbl_df")
  if (requireNamespace("openssl", quietly = TRUE)
      & requireNamespace("zip", quietly = TRUE)) {
    expect(exists("df_v3"), failure_message = "read_NPX failed on dataset v3")
    expect(exists("df_ext_v1"), failure_message = "read_NPX failed on extended dataset v1")
    expect(exists("df_ext_v2"), failure_message = "read_NPX failed on extended dataset v2")
  }
  if (requireNamespace("arrow", quietly = TRUE) ){
    expect(exists("df_parquet"), failure_message = "read_NPX failed on parquet dataset")
  }

  #NPX zip read ok?
  if (requireNamespace("openssl", quietly = TRUE)
      & requireNamespace("zip", quietly = TRUE)) {
    zip_npx_file_fail_1 <- system.file("extdata", "Example_NPX_Data_zip.zip", package = "OlinkAnalyze", mustWork = TRUE)
    expect_error(read_NPX(filename = zip_npx_file_fail_1), "Checksum of NPX file does not match the one from \"MD5_checksum.txt\"! Loss of data?")

    zip_npx_file_fail_2 <- system.file("extdata", "Example_NPX_Data_empty.zip", package = "OlinkAnalyze", mustWork = TRUE)
    expect_error(read_NPX(filename = zip_npx_file_fail_2), "The compressed file does not contain a valid NPX file. Expecting: \"README.txt\", \"MD5_checksum.txt\" or \"checksum_sha256.txt\" and the NPX file.")

    zip_npx_file_success <- system.file("extdata", "Example_NPX_Data_3K.zip", package = "OlinkAnalyze", mustWork = TRUE)
    df_2 <- read_NPX(filename = zip_npx_file_success)

    zip_npx_file_success_sha <- system.file("extdata", "Example_NPX_Data_sha256.zip", package = "OlinkAnalyze", mustWork = TRUE)
    expect_snapshot(read_NPX(filename = zip_npx_file_success_sha))
  }

  #Manifest read ok?
  expect(exists("manifest_1"), failure_message = "Failed to read manifest_1.")
  expect_s3_class(manifest_1, class = "data.frame")

  #Correct number of cols and rows?
  expect_equal(nrow(df_1), 29440)
  expect_equal(ncol(df_1), 12)
  if (requireNamespace("openssl", quietly = TRUE)
      & requireNamespace("zip", quietly = TRUE)) {
    expect_equal(nrow(df_2), 11772)
    expect_equal(ncol(df_2), 14)
    expect_equal(nrow(df_v3), 1000)
    expect_equal(ncol(df_v3), 16)
    expect_equal(nrow(df_ext_v1), 1000)
    expect_equal(ncol(df_ext_v1), 24)
    expect_equal(nrow(df_ext_v2), 1000)
    expect_equal(ncol(df_ext_v2), 22)
  }
  if (requireNamespace("arrow", quietly = TRUE) ) {
    expect_equal(nrow(df_parquet), 1)
    expect_equal(ncol(df_parquet),19)
  }

  #Correct col names?
  expect_identical(colnames(df_1),
                   c("SampleID", "Index", "OlinkID", "UniProt", "Assay",
                     "MissingFreq", "Panel","Panel_Version", "PlateID", "QC_Warning", "LOD",
                     "NPX"))
  expect_identical(colnames(df_2_2),
                   c("SampleID", "Index", "OlinkID", "UniProt", "Assay",
                     "MissingFreq", "Panel","Panel_Lot_Nr", "PlateID", "QC_Warning", "LOD",
                     "NPX", "Normalization"))
  if (requireNamespace("openssl", quietly = TRUE)
      & requireNamespace("zip", quietly = TRUE)) {
    expect_identical(colnames(df_2),
                     c("SampleID", "Index", "OlinkID", "UniProt", "Assay",
                       "MissingFreq", "Panel","Panel_Lot_Nr", "PlateID", "QC_Warning", "LOD",
                       "NPX", "Normalization", "Assay_Warning"))
    expect_identical(colnames(df_v3),
                     c("SampleID", "Index", "OlinkID", "UniProt", "Assay",
                       "MissingFreq", "Panel", "Panel_Lot_Nr", "PlateID",
                       "QC_Warning", "LOD", "NPX", "Normalization",
                       "Assay_Warning", "Sample_Type", "ExploreVersion"))
    expect_identical(colnames(df_ext_v1),
                     c("SampleID", "Index", "OlinkID", "UniProt", "Assay",
                       "MissingFreq", "Panel", "Panel_Lot_Nr", "PlateID",
                       "QC_Warning", "LOD", "NPX", "Normalization",
                       "Assay_Warning", "Sample_Type", "WellID", "IntraCV",
                       "InterCV", "Processing_StartDate", "Processing_EndDate",
                       "AnalyzerID", "INC_Warning", "AMP_Warning",
                       "Count_Warning"))
    expect_identical(colnames(df_ext_v2),
                     c("SampleID", "Index", "OlinkID", "UniProt", "Assay",
                       "MissingFreq", "Panel", "Panel_Lot_Nr", "PlateID",
                       "QC_Warning", "LOD", "NPX", "Normalization",
                       "Assay_Warning", "Sample_Type", "ExploreVersion", "WellID",
                       "IntraCV", "InterCV", "Processing_StartDate",
                       "Processing_EndDate", "AnalyzerID"))
  }
  if (requireNamespace("arrow", quietly = TRUE) ){
    expect_identical(colnames(df_parquet),
                   c("SampleID", "SampleType", "WellID", "PlateID",
                     "DataAnalysisRefID", "OlinkID", "UniProt", "Assay",
                     "AssayType", "Panel", "Block", "Count", "ExtNPX", "NPX",
                     "Normalization", "PCNormalizedNPX", "AssayQC", "SampleQC",
                     "ExploreVersion"))
  }

  #All samples in the manifest?
  sample_names <- df_1 %>%
    dplyr::filter(!stringr::str_detect(SampleID, "CONTROL*.")) %>%
    dplyr::distinct(SampleID)
  expect(all(dplyr::pull(sample_names) %in% manifest_1$SampleID), failure_message = "Some samples not in manifest.")
})



test_that("data completeness check", {
  expect_warning(
    npx_data1 %>%
       mutate(NPX = if_else(SampleID == "A1" & Panel == "Olink Cardiometabolic",
                            NA_real_,
                            NPX)) %>%
       check_data_completeness(),
    "The following samples have NA NPX values for the following panels"
  )

  expect_warning(
    npx_data1 %>%
      check_data_completeness(),
    NA
  )

})


# Sample ID with # --------------------------------------------------------


test_that("# in SampleID", {
      input <- suppressWarnings(read_NPX(testthat::test_path("refs/mock_sampleID_hashes.csv")))

      expect_equal(input$SampleID, c("Sample#1", "Sample_#31"))
})



# Flex long format QUANT xlsx ---------------------------------------------


test_that("# in SampleID", {
  expect_message(
    input <- read_NPX(
      testthat::test_path("refs/Flex_test_data_long_quant.xlsx")),
    "Flex data in long form detected"
  )

  expect_equal(input$Quantified_value, c(0.19169, 336.12903))
})

# No warning for extra column ----------------

test_that("extra column", {
  skip_if_not(condition = getRversion() >= "4.2.0",
              message = "Skipping for R < 4.2.0")

  expect_no_warning(read_NPX(testthat::test_path("refs/mock_sampleID_hashes.csv")))
})

# RUO parquet file ----

test_that(
  "read_npx_parquet - RUO file",
  {
    skip_if_not_installed(c("withr", "arrow"))

    withr::with_tempfile(
      new = "pfile_ruo",
      pattern = "parquet-file_ruo",
      fileext = ".parquet",
      code = {

        # random data frame
        df <- dplyr::tibble(
          "SampleID" = c(1, 2.2, 3.14),
          "OlinkID" = c("a", "b", "c"),
          "UniProt" = c(TRUE, TRUE, FALSE),
          "Assay" = c("NA", "B", NA_character_),
          "Panel" = c(1L, 2L, 3L),
          "PlateID" = c(1, 2.2, 3.14),
          "SampleQC" = c("a", "b", "c"),
          "NPX" = c(1L, 2L, 3L)
        ) |>
          arrow::as_arrow_table(df)

        # modify metadata
        df_metadata_list <- list(
          "DataFileType" = "NPX File",
          "Product" = "ExploreHT",
          "RUO" = "I am for reasearch only!"
        )
        df$metadata <- df_metadata_list

        # write parquet
        arrow::write_parquet(
          x = df,
          sink = pfile_ruo,
          compression = "gzip"
        )

        # check that the parquet file was created
        expect_true(object = file.exists(pfile_ruo))

        # check that relevant error is thrown
        expect_message(
          object = read_npx_parquet(filename = pfile_ruo),
          regexp = "I am for reasearch only!"
        )

      }
    )

  }
)
