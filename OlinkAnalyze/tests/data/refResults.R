# This script creates reference results for usage in the unit tests

#### t-test ####
t.test_results <- olink_ttest(npx_data1, 'Treatment')
#paired t-test
t.test_results_paired <- npx_data1 %>%
  filter(Time %in% c("Baseline","Week.6")) %>%
  olink_ttest(variable = "Time", pair_id = "Subject")

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
                                            olinkid_list =  {anova_results_1_site %>%
                                                head(10)%>%
                                                dplyr::pull(OlinkID)},
                                            effect = 'Site') %>%
  mutate(id = as.character(OlinkID)) %>%
  arrange(id, contrast) %>% #Since OlinkID is not unique here (=> ties), contrast is used to break the ties
  select(-id)
anova_posthoc_1_time <- olink_anova_posthoc(npx_data1,
                                            variable = 'Time',
                                            {anova_results_1_time %>%
                                                head(10) %>%
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

#### olink_normalization ####
# Output this subset of samples to reduce the file size
sampleSubset <- c("A6", "A38","B47","B22","A43","D75","D79","C66","B43","B70","D52","A58","B71","A50","D1", "B8")

# Bridging normalization
overlap_samples <- intersect(npx_data1$SampleID, npx_data2$SampleID) %>%
  data.frame() %>%
  filter(!str_detect(., 'CONTROL_SAMPLE')) %>% #Remove control samples
  pull(.)
normalization_results.bridged <- olink_normalization(df1 = npx_data1,
                                                     df2 = npx_data2,
                                                     overlapping_samples_df1 = overlap_samples,
                                                     df1_project_nr = '20200001',
                                                     df2_project_nr = '20200002',
                                                     reference_project = '20200001') %>%
  filter(SampleID %in% sampleSubset)

# Intensity normalization
normalization_results.intensity <- olink_normalization(df1 = npx_data1,
                                                       df2 = npx_data2,
                                                       overlapping_samples_df1 = npx_data1$SampleID,
                                                       overlapping_samples_df2 = npx_data2$SampleID) %>%
  filter(SampleID %in% sampleSubset)

# Subset normalization
#NOTE: this subset is just a random sample in order to test the function
sampleSubset.adj <- c("C6", "C21","C28","C50","C19","D5", "A30","C52","D77","D3", "D16","C72","A52","D67","C77","C22","D62","D39","C34","C13")
normalization_results.subset <- olink_normalization(df1 = npx_data1,
                                                    df2 = npx_data2,
                                                    overlapping_samples_df1 = npx_data1$SampleID,
                                                    overlapping_samples_df2 = sampleSubset.adj,
                                                    df1_project_nr = '20200001',
                                                    df2_project_nr = '20200002',
                                                    reference_project = '20200001') %>%
  filter(SampleID %in% sampleSubset)

#### olink_plate_randomizer ####
randomized_result1 <- olink_plate_randomizer(manifest,
                                             seed=12345)
randomized_result2 <- olink_plate_randomizer(manifest,
                                             SubjectColumn="SubjectID",
                                             available.spots=c(88,88),
                                             seed=12345)

#### npxProcessing_forDimRed ####
npx_data1.uniqIDs <- npx_data1 %>%
  mutate(SampleID = paste(SampleID, "_", Index, sep = ""))

procData <- npxProcessing_forDimRed(df = npx_data1.uniqIDs,
                                    color_g = 'QC_Warning',
                                    drop_assays = F,
                                    drop_samples = F,
                                    verbose = T)

#With missing data
samples <- sample({npx_data1.uniqIDs$SampleID %>% unique()},
               size = {ceiling( (npx_data1.uniqIDs$SampleID %>% unique() %>% length())*.15 )})

npx_data1.uniqIDs_missingData <- npx_data1.uniqIDs %>%
  mutate(NPX = ifelse(SampleID %in% samples & OlinkID %in% c('OID00482', 'OID00483', 'OID00484', 'OID00485'), NA, NPX)) %>% #These should be removed due to to high missingness
  mutate(NPX = ifelse(SampleID %in% c('A18_19', 'B8_87') & OlinkID %in% c('OID00562', 'OID01213', 'OID05124'), NA, NPX))    #These should be median imputed

procData_missingData <- npxProcessing_forDimRed(df = npx_data1.uniqIDs_missingData,
                                    color_g = 'QC_Warning',
                                    drop_assays = F,
                                    drop_samples = F,
                                    verbose = T)

#### Wrap up the results ####
ref_results <- list(t.test_results = t.test_results,
                    t.test_results_paired = t.test_results_paired,
                    anova_results_1_site = anova_results_1_site,
                    anova_results_1_time = anova_results_1_time,
                    anova_results_1_siteTime = anova_results_1_siteTime,
                    anova_posthoc_1_site = anova_posthoc_1_site,
                    anova_posthoc_1_time = anova_posthoc_1_time,
                    lmer_results_1 = lmer_results_1,
                    lmer_results_1_posthoc = lmer_results_1_posthoc,
                    normalization_results.bridged = normalization_results.bridged,
                    normalization_results.intensity = normalization_results.intensity,
                    normalization_results.subset = normalization_results.subset,
                    randomized_result1 = randomized_result1,
                    randomized_result2 = randomized_result2,
                    procData = procData,
                    procData_missingData = procData_missingData)
save(ref_results, file = 'tests/data/refResults.RData')

