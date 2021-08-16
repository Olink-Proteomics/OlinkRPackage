test_that("Data loads correctly with 'read_NPX()'", {
  npx_file <- system.file("extdata", "Example_NPX_Data.csv", package = "OlinkAnalyze", mustWork = T)
  df_1 <- read_NPX(filename = npx_file) # load dataset 1

  expect(exists("df_1"), failure_message = "read_NPX failed on dataset 1")
  expect_s3_class(df_1, class = "tbl_df")

  expect_equal(nrow(df_1), 1)
  expect_equal(ncol(df_1), 18)

  expect_setequal(colnames(df_1),
                   c("X","SampleID", "Index", "OlinkID", "UniProt", "Assay",
                     "MissingFreq", "Panel","Panel_Version", "PlateID", "QC_Warning", "LOD",
                     "NPX", "Subject", "Treatment", "Site", "Time", "Project") )

})
