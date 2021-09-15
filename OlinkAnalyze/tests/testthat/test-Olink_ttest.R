#Load reference results
refRes_file <- '../data/refResults.RData'
load(refRes_file)

#Run olink_ttest
t.test_results <- olink_ttest(npx_data1, 'Treatment')

t.test_results_paired <- npx_data1 %>% #Paired t-test
  filter(Time %in% c("Baseline","Week.6")) %>%
  olink_ttest(variable = "Time", pair_id = "Subject")

test_that("T-test function works", {
  expect_equal(t.test_results, ref_results$t.test_results)  # compare results to ref
  expect_equal(as.matrix(t.test_results_paired), as.matrix(ref_results$t.test_results_paired))  # compare results to ref
  expect_error(olink_ttest(df = npx_data1)) # no input data
  expect_error(olink_ttest(npx_data1, "Time")) # more than 2 levels in the grouping variable
})
