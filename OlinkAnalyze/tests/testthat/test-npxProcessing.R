#Load reference results
refRes_file <- '../data/refResults.RData'
load(refRes_file)

procData <- npxProcessing_forDimRed(df = {npx_data1 %>% mutate(SampleID = paste(SampleID, "_", Index, sep = ""))},
                                    color_g = 'QC_Warning',
                                    drop_assays = F,
                                    drop_samples = F,
                                    verbose = T)

test_that("npxProcessing_forDimRed works", {
  expect_equal(procData$df_wide, ref_results$procData$df_wide)
  expect_equal(procData$df_wide_matrix, ref_results$procData$df_wide_matrix)
})
