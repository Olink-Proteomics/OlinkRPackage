test_that("Data loads correctly with 'read_NPX()'", {
  #Read data
  npx_file <- system.file("extdata", "npx_data1.xlsx", package = "OlinkAnalyze", mustWork = T)
  manifest_file <- system.file("extdata", "npx_data1_meta.csv", package = "OlinkAnalyze", mustWork = T)
  df_1 <- read_NPX(filename = npx_file) # load dataset 1
  manifest_1 <- read.delim(manifest_file, header = T, sep = ';') # load manifest 1

  #NPX read ok?
  expect(exists("df_1"), failure_message = "read_NPX failed on dataset 1")
  expect_s3_class(df_1, class = "tbl_df")

  #Manifest read ok?
  expect(exists("manifest_1"), failure_message = "Failed to read manifest_1.")
  expect_s3_class(manifest_1, class = "data.frame")

  #Correct number of cols and rows?
  expect_equal(nrow(df_1), 29440)
  expect_equal(ncol(df_1), 12)

  #Correct col names?
  expect_identical(colnames(df_1),
                   c("SampleID", "Index", "OlinkID", "UniProt", "Assay",
                     "MissingFreq", "Panel","Panel_Version", "PlateID", "QC_Warning", "LOD",
                     "NPX"))

  #All samples in the manifest?
  sample_names <- df_1 %>%
    dplyr::filter(!stringr::str_detect(SampleID, "CONTROL*.")) %>%
    dplyr::distinct(SampleID)
  expect(all(dplyr::pull(sample_names) %in% manifest_1$SampleID), failure_message = "Some samples not in manifest.")
})
