# This script creates reference results for usage in the unit tests

#### t-test ####
t.test_results <- olink_ttest(npx_data1, 'Treatment')

#### ANOVA ####
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

#### anova posthoc ####
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

#### lmer ####
lmer_results_1 <- olink_lmer(df = npx_data1,
                             variable = c('Treatment', "Time"),
                             random = "Subject") %>%
  mutate(id = as.character(OlinkID)) %>%
  arrange(id, term) %>% #Since OlinkID is not unique here (=> ties), term is used to break the ties
  select(-id)

#### lmer posthoc ####
lmer_results_1_posthoc <- olink_lmer_posthoc(df = npx_data1,
                                             variable = c('Treatment', "Time"),
                                             random = "Subject",
                                             olinkid_list = {lmer_results_1 %>%
                                                 dplyr::filter(term == 'Treatment:Time') %>%
                                                 dplyr::filter(Threshold == 'Significant') %>%
                                                 dplyr::pull(OlinkID)} ,
                                             effect = c('Treatment', "Time")) %>%
  mutate(id = as.character(OlinkID)) %>%
  arrange(id, contrast) %>%
  select(-id)

#### Wrap up the results ####
ref_results <- list(ttestresults = t.test_results,
                    anova_results_1_site = anova_results_1_site,
                    anova_results_1_time = anova_results_1_time,
                    anova_results_1_siteTime = anova_results_1_siteTime,
                    anova_posthoc_1_site = anova_posthoc_1_site,
                    anova_posthoc_1_time = anova_posthoc_1_time,
                    lmer_results_1 = lmer_results_1,
                    lmer_results_1_posthoc = lmer_results_1_posthoc)
save(ref_results, file = 'refResults.RData')

