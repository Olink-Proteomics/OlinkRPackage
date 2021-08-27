#Load reference results
refRes_file <- '../data/refResults.RData'
load(refRes_file)

test_that("olink_anova function works", {
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

test_that("olink_anova_posthoc function works", {
  #Run function
  anova_posthoc_1_site <- olink_anova_posthoc(npx_data1,
                                              variable = 'Site',
                                              olinkid_list =  {ref_results$anova_results_1_site %>%
                                                  dplyr::filter(Threshold == 'Significant') %>%
                                                  dplyr::pull(OlinkID)},
                                              effect = 'Site') %>%
    mutate(id = as.character(OlinkID)) %>%
    arrange(id, contrast) %>% #Since OlinkID is not unique here (=> ties), contrast is used to break the ties
    select(-id)
  anova_posthoc_1_time <- olink_anova_posthoc(npx_data1,
                                              variable = 'Time',
                                              {ref_results$anova_results_1_time %>%
                                                  dplyr::filter(Threshold == 'Significant') %>%
                                                  dplyr::pull(OlinkID)},
                                              effect = 'Time') %>%
    mutate(id = as.character(OlinkID)) %>%
    arrange(id, contrast) %>% #Just for consistency. Not actually needed in this case
    select(-id)

  #Tests
  expect_equal(anova_posthoc_1_site, ref_results$anova_posthoc_1_site) ## result equal to testfile - posthoc
  expect_equal(nrow(anova_posthoc_1_site), 1410) ## check nr of rows
  expect_error(olink_anova_posthoc(npx_data1, 'Site')) ##no olinkid list
  expect_equal(anova_posthoc_1_site %>%
                 dplyr::select(contrast) %>%
                 unique() %>%
                 nrow(),10)

  expect_equal(anova_posthoc_1_time, ref_results$anova_posthoc_1_time)
  expect_equal(nrow(anova_posthoc_1_time), 3)
  expect_equal(anova_posthoc_1_time %>% ncol(), 11)
})
