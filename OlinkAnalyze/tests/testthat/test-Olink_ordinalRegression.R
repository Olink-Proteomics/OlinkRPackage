#Load reference results
refRes_file <- '../data/refResults.RData'
load(refRes_file)

#Two-way Ordinal Regression
ordinalRegression_results <- olink_ordinalRegression(df = npx_data1,
                                                     variable="Treatment:Time") %>% 
  mutate(id = as.character(OlinkID)) %>%
  arrange(id, Assay) %>% #Just for consistency. Not actually needed in this case
  select(-id)

#Posthoc
ordinalRegression_results_posthoc_results <- olink_ordinalRegression_posthoc(npx_data1,
                                                                             variable=c("Treatment:Time"),
                                                                             covariates="Site",
                                                                             olinkid_list = ordinalRegression_results %>%
                                                                               filter(Threshold == 'Significant' & term == 'Treatment:Time') %>%
                                                                               dplyr::select(OlinkID) %>%
                                                                               distinct() %>%
                                                                               pull(),
                                                                             effect = "Treatment:Time") %>% 
  mutate(id = as.character(OlinkID)) %>%
  arrange(id, contrast) %>% #Just for consistency. Not actually needed in this case
  select(-id)




test_that("olink_ordinalRegression works", {
  expect_equal(ordinalRegression_results, ref_results$ordinalRegression_results, tolerance = 1e-4)
  expect_error(olink_ordinalRegression(npx_data1))
})

test_that("olink_ordinalRegression_posthoc works", {
  expect_equal(ordinalRegression_results_posthoc_results, ref_results$ordinalRegression_results_posthoc_results)
  expect_error(olink_ordinalRegression_posthoc(df = npx_data1,
                                  variable = c('Treatment', "Time"),
                                  random = "Subject",
                                  olinkid_list = {ordinalRegression_results %>%
                                      dplyr::filter(term == 'Treatment:Time') %>%
                                      dplyr::filter(Threshold == 'Significant') %>%
                                      dplyr::pull(OlinkID)})) # no effect specified
})
