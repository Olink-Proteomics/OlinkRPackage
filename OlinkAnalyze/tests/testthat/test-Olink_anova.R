test_that("olink_anova function works", {
  #Load reference results
  refRes_file <- system.file("extdata", "refResults.RData", package = "OlinkAnalyze", mustWork = T)
  load(refRes_file)

  #Fit the ANOVA models
  anova_results_1_site <- olink_anova(npx_data1, 'Site') %>%
    mutate(id = as.character(OlinkID)) %>%
    arrange(id) %>%
    select(-id)
  anova_results_1_time <- olink_anova(npx_data1, 'Time') %>%
    mutate(id = as.character(OlinkID)) %>%
    arrange(id) %>%
    select(-id)
  anova_results_1_siteTime <- olink_anova(npx_data1, c('Site', 'Time')) %>%
    mutate(id = as.character(OlinkID)) %>%
    arrange(id, term) %>% #Since OlinkID is not unique here (=> ties), term is used to break the ties
    select(-id)

  #Tests
  expect_equal(anova_results_1_site, ref_results$anova_results_1_site)  ##result equal to testfile
  expect_equal(anova_results_1_time, ref_results$anova_results_1_time)  ##result equal to testfile
  expect_equal(anova_results_1_siteTime, ref_results$anova_results_1_siteTime)  ##result equal to testfile

  expect_equal(nrow(anova_results_1_siteTime), 3312)
  expect_equal(ncol(anova_results_1_siteTime), 12)

  expect_error(olink_anova(npx_data1,)) ##no input data
})
