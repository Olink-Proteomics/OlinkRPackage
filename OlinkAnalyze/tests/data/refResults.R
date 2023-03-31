# This script creates reference results for usage in the unit tests

#### t-test ####
t.test_results <- olink_ttest(npx_data1, 'Treatment')
#paired t-test
t.test_results_paired <- npx_data1 %>%
  filter(Time %in% c("Baseline","Week.6")) %>%
  olink_ttest(variable = "Time", pair_id = "Subject")

#### Mann-Whitney U Test ####
wilcox.test_results <- olink_wilcox(npx_data1, 'Treatment')
#paired Mann-Whitney U Test
wilcox.test_results_paired <- npx_data1 %>%
  filter(Time %in% c("Baseline","Week.6")) %>%
  olink_wilcox(variable = "Time", pair_id = "Subject")

#### Kruskal and Friedman test ####
# One-way Kruskal-Wallis Test
kruskal_results <- olink_one_non_parametric(df = npx_data1,
                                            variable = "Site")
# One-way Friedman Test
friedman_results <- olink_one_non_parametric(df = npx_data1,
                                             variable = "Time",
                                             subject = "Subject",
                                             dependence = TRUE)

#Posthoc test for the results from Kruskal-Wallis Test
kruskal_posthoc_results <- olink_one_non_parametric_posthoc(npx_data1,
                                                            variable = "Site",
                                                            test = "kruskal",
                                                            olinkid_list = {kruskal_results %>%
                                                                filter(Threshold == 'Significant') %>%
                                                                dplyr::select(OlinkID) %>%
                                                                distinct() %>%
                                                                pull()}) %>%
  mutate(id = as.character(OlinkID)) %>%
  arrange(id, contrast) %>% #Just for consistency. Not actually needed in this case
  select(-id)

#Posthoc test for the results from Friedman Test
friedman_posthoc_results <- olink_one_non_parametric_posthoc(npx_data1,
                                                             variable = "Time",
                                                             test = "friedman",
                                                             olinkid_list = {friedman_results %>%
                                                                 filter(Threshold == 'Significant') %>%
                                                                 dplyr::select(OlinkID) %>%
                                                                 distinct() %>%
                                                                 pull()}) %>%
  mutate(id = as.character(OlinkID)) %>%
  arrange(id, contrast) %>% #Just for consistency. Not actually needed in this case
  select(-id)

#### Ordinal regression ####
#Two-way Ordinal Regression with CLM.
ordinalRegression_results <- olink_ordinalRegression(df = npx_data1,
                             variable="Treatment:Time") %>%
  mutate(id = as.character(OlinkID)) %>%
  arrange(id, Assay) %>% #Just for consistency. Not actually needed in this case
  select(-id)

#Posthoc
ordinalRegression_results_posthoc_results <- olink_ordinalRegression_posthoc(npx_data1,
                                                                             variable=c("Treatment:Time"),
                                                                             covariates="Site",
                                                                             olinkid_list = {ordinalRegression_results %>%
                                                                                 filter(Threshold == 'Significant' & term == 'Treatment:Time') %>%
                                                                                 dplyr::select(OlinkID) %>%
                                                                                 distinct() %>%
                                                                                 pull()},
                                                                             effect = "Treatment:Time") %>%
  mutate(id = as.character(OlinkID)) %>%
  arrange(id, contrast) %>% #Just for consistency. Not actually needed in this case
  select(-id)


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

# Multi-batch normalization
npx_multi_df1 <- npx_data1 |>
  dplyr::filter(!stringr::str_detect(SampleID, "CONTROL_")) |>
  dplyr::select(-Project) |>
  dplyr::mutate(Normalization = "Intensity")

npx_multi_df2 <- npx_data2 |>
  dplyr::filter(!stringr::str_detect(SampleID, "CONTROL_")) |>
  dplyr::select(-Project) |>
  dplyr::mutate(Normalization = "Intensity")

# manipulating the sample NPX datasets to create another two random ones
npx_multi_df3 <- npx_data2 |>
  dplyr::mutate(SampleID = paste(SampleID, "_mod", sep = ""),
                PlateID = paste(PlateID, "_mod", sep = "")) |>
  dplyr::filter(!stringr::str_detect(SampleID, "CONTROL_")) |>
  dplyr::select(-Project) |>
  dplyr::mutate(Normalization = "Intensity")

npx_multi_df4 <- npx_data1 |>
  dplyr::mutate(SampleID = paste(SampleID, "_mod2", sep = ""),
                PlateID = paste(PlateID, "_mod2", sep = "")) |>
  dplyr::filter(!stringr::str_detect(SampleID, "CONTROL_")) |>
  dplyr::select(-Project) |>
  dplyr::mutate(Normalization = "Intensity")

## samples to use for normalization
# Bridge samples with same identifiers between npx_df1 and npx_df2
overlap_samples_df1_df2 <- list("DF1" = overlap_samples,
                                "DF2" = overlap_samples)

# Bridge samples with different identifiers between npx_df2 and npx_df3
overlap_samples_df2_df3 <- list("DF1" =
                                  {
                                    npx_multi_df2 |>
                                      dplyr::filter(stringr::str_detect(string = SampleID,
                                                                        pattern = "^A")) |>
                                      dplyr::pull(SampleID) |>
                                      unique() |>
                                      sort()
                                  },
                                "DF2" =
                                  {
                                    npx_multi_df3 |>
                                      dplyr::filter(stringr::str_detect(string = SampleID,
                                                                        pattern = "^C")) |>
                                      dplyr::pull(SampleID) |>
                                      unique() |>
                                      sort() |>
                                      head(10)
                                  })

# Samples to use for intensity normalization between npx_df4 and the
# normalized dataset of npx_df1 and npx_df2
overlap_samples_df13_df4 <- list("DF1" =
                                   {
                                     npx_multi_df1 |>
                                       dplyr::bind_rows(npx_multi_df3) |>
                                       dplyr::filter(stringr::str_detect(string = SampleID,
                                                                         pattern = "^A")) |>
                                       dplyr::pull(SampleID) |>
                                       unique() |>
                                       sort()
                                   },
                                 "DF2" =
                                   {
                                     npx_multi_df4 |>
                                       dplyr::filter(stringr::str_detect(string = SampleID,
                                                                         pattern = "^B")) |>
                                       dplyr::pull(SampleID) |>
                                       unique() |>
                                       sort()
                                   })

# create tibble for input
norm_schema_npxMulti <- dplyr::tibble(
  order              = c(1, 2, 3, 4),
  name               = c("NPX_DF1", "NPX_DF2", "NPX_DF3", "NPX_DF4"),
  data               = list("NPX_DF1" = npx_multi_df1,
                            "NPX_DF2" = npx_multi_df2,
                            "NPX_DF3" = npx_multi_df3,
                            "NPX_DF4" = npx_multi_df4),
  samples            = list("NPX_DF1" = NA_character_,
                            "NPX_DF2" = overlap_samples_df1_df2,
                            "NPX_DF3" = overlap_samples_df2_df3,
                            "NPX_DF4" = overlap_samples_df13_df4),
  normalization_type = c(NA_character_, "Bridge", "Bridge", "Subset"),
  normalize_to       = c(NA_character_, "1", "2", "1,3")
)

normalization_results.multi <-
  olink_normalization_n(norm_schema = norm_schema_npxMulti) |>
  dplyr::mutate(SampleID_tmp = stringr::str_split_i(string = SampleID,
                                                    pattern = "_",
                                                    i = 1)) |>
  dplyr::filter(SampleID_tmp %in% sampleSubset) |>
  dplyr::select(-SampleID_tmp)

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
                    wilcox.test_results = wilcox.test_results,
                    wilcox.test_results_paired = wilcox.test_results_paired,
                    kruskal_results = kruskal_results,
                    friedman_results = friedman_results,
                    kruskal_posthoc_results = kruskal_posthoc_results,
                    friedman_posthoc_results = friedman_posthoc_results,
                    ordinalRegression_results = ordinalRegression_results,
                    ordinalRegression_results_posthoc_results = ordinalRegression_results_posthoc_results,
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

