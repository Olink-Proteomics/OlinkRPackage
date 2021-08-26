test_that("T-test function works", {
  #Load reference results
  refRes_file <- system.file("extdata", "refResults.RData", package = "OlinkAnalyze", mustWork = T)
  load(refRes_file)

  #Run t-tests
  t.test_results <- olink_ttest(npx_data1, 'Treatment')

  #Tests
  expect_equal(t.test_results, ref_results$ttestresults)  # compare results to ref
  expect_error(olink_ttest(df = npx_data1)) # no input data
  expect_error(olink_ttest(npx_data1, "Time")) # more than 2 levels in the grouping variable
})
