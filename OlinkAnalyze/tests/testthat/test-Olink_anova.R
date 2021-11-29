#Load reference results
refRes_file <- '../data/refResults.RData'
load(refRes_file)

#Run olink_anova
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

#Run olink_anova_posthoc
anova_posthoc_1_site <- olink_anova_posthoc(npx_data1,
                                            variable = 'Site',
                                            olinkid_list =  {anova_results_1_site %>%
                                                head(10) %>%
                                                dplyr::pull(OlinkID)},
                                            effect = 'Site') %>%
  mutate(id = as.character(OlinkID)) %>%
  arrange(id, contrast) %>% #Since OlinkID is not unique here (=> ties), contrast is used to break the ties
  mutate(contrast = as.character(contrast)) %>% # In R 3.6.1 we get factors, but reference is characters
  select(-id)


anova_posthoc_1_time <- olink_anova_posthoc(npx_data1,
                                            variable = 'Time',
                                            {anova_results_1_time %>%
                                                head(10) %>%
                                                dplyr::pull(OlinkID)},
                                            effect = 'Time') %>%
  mutate(id = as.character(OlinkID)) %>%
  arrange(id, contrast) %>% #Just for consistency. Not actually needed in this case
  mutate(contrast = as.character(contrast)) %>% # In R 3.6.1 we get factors, but reference is characters
  select(-id)

test_that("olink_anova function works", {
  expect_equal(anova_results_1_site, ref_results$anova_results_1_site)  ##result equal to testfile
  expect_equal(anova_results_1_time, ref_results$anova_results_1_time)  ##result equal to testfile
  expect_equal(anova_results_1_siteTime, ref_results$anova_results_1_siteTime)  ##result equal to testfile

  expect_equal(nrow(anova_results_1_siteTime), 552)
  expect_equal(ncol(anova_results_1_siteTime), 12)

  expect_error(olink_anova(npx_data1,)) ##no input data
})

test_that("olink_anova_posthoc function works", {
  expect_equal(anova_posthoc_1_site, ref_results$anova_posthoc_1_site) ## result equal to testfile - posthoc
  expect_equal(nrow(anova_posthoc_1_site), 100) ## check nr of rows
  expect_error(olink_anova_posthoc(npx_data1, 'Site')) ##no olinkid list
  expect_equal(anova_posthoc_1_site %>%
                 dplyr::select(contrast) %>%
                 unique() %>%
                 nrow(),10)

  expect_equal(anova_posthoc_1_time, ref_results$anova_posthoc_1_time)
  expect_equal(nrow(anova_posthoc_1_time), 30)
  expect_equal(anova_posthoc_1_time %>% ncol(), 11)
})
