#Load reference results
refRes_file <- '../data/refResults.RData'
load(refRes_file)

#Run olink_ttest
t.test_results <- olink_ttest(npx_data1, 'Treatment')

test_that("T-test function works", {
  expect_equal(t.test_results, ref_results$ttestresults)  # compare results to ref
  expect_error(olink_ttest(df = npx_data1)) # no input data
  expect_error(olink_ttest(npx_data1, "Time")) # more than 2 levels in the grouping variable
})
