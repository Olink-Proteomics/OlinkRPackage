test_that("Data loads correctly with 'read_NPX()'", {
  #Read data
  npx_file <- system.file("extdata", "npx_data1.xlsx", package = "OlinkAnalyze", mustWork = TRUE)
  manifest_file <- system.file("extdata", "npx_data1_meta.csv", package = "OlinkAnalyze", mustWork = TRUE)
  df_1 <- read_NPX(filename = npx_file) # load dataset 1
  manifest_1 <- read.delim(manifest_file, header = TRUE, sep = ';') # load manifest 1

  #NPX read ok?
  expect(exists("df_1"), failure_message = "read_NPX failed on dataset 1")
  expect_s3_class(df_1, class = "tbl_df")

  #NPX zip read ok?
  zip_npx_file_fail_1 <- system.file("extdata", "Example_NPX_Data_zip.zip", package = "OlinkAnalyze", mustWork = TRUE)
  expect_error(read_NPX(filename = zip_npx_file_fail_1), "Checksum of NPX file does not match the one from \"MD5_checksum.txt\"! Loss of data?")

  zip_npx_file_fail_2 <- system.file("extdata", "Example_NPX_Data_empty.zip", package = "OlinkAnalyze", mustWork = TRUE)
  expect_error(read_NPX(filename = zip_npx_file_fail_2), "The compressed file does not contain a valid NPX file. Expecting: \"README.txt\", \"MD5_checksum.txt\" or \"checksum_sha256.txt\" and the NPX file.")

  # zip_npx_file_success <- system.file("extdata", "Example_NPX_Data_3K.zip", package = "OlinkAnalyze", mustWork = TRUE)
  # df_2 <- read_NPX(filename = zip_npx_file_success)

  zip_npx_file_success_sha <- system.file("extdata", "Example_NPX_Data_sha256.zip", package = "OlinkAnalyze", mustWork = TRUE)
  expect_snapshot(read_NPX(filename = zip_npx_file_success_sha))


  #Manifest read ok?
  expect(exists("manifest_1"), failure_message = "Failed to read manifest_1.")
  expect_s3_class(manifest_1, class = "data.frame")

  #Correct number of cols and rows?
  expect_equal(nrow(df_1), 29440)
  expect_equal(ncol(df_1), 12)
#
#   expect_equal(nrow(df_2), 264870)
#   expect_equal(ncol(df_2), 14)

  #Correct col names?
  expect_identical(colnames(df_1),
                   c("SampleID", "Index", "OlinkID", "UniProt", "Assay",
                     "MissingFreq", "Panel","Panel_Version", "PlateID", "QC_Warning", "LOD",
                     "NPX"))
  # expect_identical(colnames(df_2),
  #                  c("SampleID", "Index", "OlinkID", "UniProt", "Assay",
  #                    "MissingFreq", "Panel","Panel_Lot_Nr", "PlateID", "QC_Warning", "LOD",
  #                    "NPX", "Normalization", "Assay_Warning"))

  #All samples in the manifest?
  sample_names <- df_1 %>%
    dplyr::filter(!stringr::str_detect(SampleID, "CONTROL*.")) %>%
    dplyr::distinct(SampleID)
  expect(all(dplyr::pull(sample_names) %in% manifest_1$SampleID), failure_message = "Some samples not in manifest.")
})
