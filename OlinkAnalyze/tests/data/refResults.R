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
  dplyr::filter(!stringr::str_detect(SampleID, "CONTROL_")) |>
  dplyr::mutate(SampleID = paste(SampleID, "_mod", sep = ""),
                PlateID = paste(PlateID, "_mod", sep = "")) |>
  dplyr::select(-Project) |>
  dplyr::mutate(Normalization = "Intensity")

npx_multi_df4 <- npx_data1 |>
  dplyr::filter(!stringr::str_detect(SampleID, "CONTROL_")) |>
  dplyr::mutate(SampleID = paste(SampleID, "_mod2", sep = ""),
                PlateID = paste(PlateID, "_mod2", sep = "")) |>
  dplyr::select(-Project) |>
  dplyr::mutate(Normalization = "Intensity")

## samples to use for normalization
# Bridge samples with same identifiers between npx_df1 and npx_df2
overlap_samples_df1_df2 <- list("DF1" = overlap_samples,
                                "DF2" = overlap_samples)

# Bridge samples with different identifiers between npx_df2 and npx_df3
overlap_samples_df2_df3 <- list("DF1" = c("A13", "A29", "A30", "A36", "A45", "A46", "A52", "A63", "A71", "A73"),
                                "DF2" = c("C1_mod", "C10_mod", "C11_mod", "C12_mod", "C13_mod", "C14_mod", "C15_mod", "C16_mod", "C17_mod", "C18_mod"))

# Samples to use for intensity normalization between npx_df4 and the
# normalized dataset of npx_df1 and npx_df2
overlap_samples_df13_df4 <- list("DF1" = c("A1", "A10", "A11", "A12", "A13", "A13_mod", "A14", "A15", "A16", "A17", "A18", "A19", "A2", "A20", "A21", "A22", "A23", "A24", "A25", "A26", "A27", "A28", "A29", "A29_mod", "A3", "A30", "A30_mod", "A31", "A32", "A33", "A34", "A35", "A36", "A36_mod", "A37", "A38", "A39", "A4", "A40", "A41", "A42", "A43", "A44", "A45", "A45_mod", "A46", "A46_mod", "A47", "A48", "A49", "A5", "A50", "A51", "A52", "A52_mod", "A53", "A54", "A55", "A56", "A57", "A58", "A59", "A6", "A60", "A61", "A62", "A63", "A63_mod", "A64", "A65", "A66", "A67", "A68", "A69", "A7", "A70", "A71", "A71_mod", "A72", "A73", "A73_mod", "A74", "A75", "A76", "A77", "A8", "A9"),
                                 "DF2" = c("B1_mod2", "B10_mod2", "B11_mod2", "B12_mod2", "B13_mod2", "B14_mod2", "B15_mod2", "B16_mod2", "B17_mod2", "B18_mod2", "B19_mod2", "B2_mod2", "B20_mod2", "B21_mod2", "B22_mod2", "B23_mod2", "B24_mod2", "B25_mod2", "B26_mod2", "B27_mod2", "B28_mod2", "B29_mod2", "B3_mod2", "B30_mod2", "B31_mod2", "B32_mod2", "B33_mod2", "B34_mod2", "B35_mod2", "B36_mod2", "B37_mod2", "B38_mod2", "B39_mod2", "B4_mod2", "B40_mod2", "B41_mod2", "B42_mod2", "B43_mod2", "B44_mod2", "B45_mod2", "B46_mod2", "B47_mod2", "B48_mod2", "B49_mod2", "B5_mod2", "B50_mod2", "B51_mod2", "B52_mod2", "B53_mod2", "B54_mod2", "B55_mod2", "B56_mod2", "B57_mod2", "B58_mod2", "B59_mod2", "B6_mod2", "B60_mod2", "B61_mod2", "B62_mod2", "B63_mod2", "B64_mod2", "B65_mod2", "B66_mod2", "B67_mod2", "B68_mod2", "B69_mod2", "B7_mod2", "B70_mod2", "B71_mod2", "B72_mod2", "B73_mod2", "B74_mod2", "B75_mod2", "B76_mod2", "B77_mod2", "B78_mod2", "B79_mod2", "B8_mod2", "B9_mod2"))

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
  dplyr::mutate(SampleID_tmp =
                  {
                    SampleID |>
                      stringr::str_split(pattern = "_") |>
                      lapply(head, 1) |>
                      unlist()
                  }) |>
  dplyr::filter(SampleID_tmp %in% sampleSubset) |>
  dplyr::select(-SampleID_tmp) |>
  dplyr::arrange(Project, Panel, OlinkID, SampleID)

#### olink_plate_randomizer ####
randomized_result1 <- olink_plate_randomizer(manifest,
                                             seed=12345)
randomized_result2 <- olink_plate_randomizer(manifest,
                                             SubjectColumn="SubjectID",
                                             available.spots=c(88,88),
                                             seed=12345)
randomized_result3 <- olink_plate_randomizer({manifest |> dplyr::mutate(
  study = ifelse(Site == "Site1", "study1", "study2"))},
  SubjectColumn="SubjectID",
  available.spots=c(88,88),
  seed=12345)
randomized_result4 <- olink_plate_randomizer({manifest |> dplyr::mutate(
  study = ifelse(Site == "Site1", "study1", "study2"))},
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
                    normalization_results.multi = normalization_results.multi,
                    randomized_result1 = randomized_result1,
                    randomized_result2 = randomized_result2,
                    procData = procData,
                    procData_missingData = procData_missingData)
ref_results$randomized_result3 <- randomized_result3
ref_results$randomized_result4 <- randomized_result4

save(ref_results, file = 'tests/data/refResults.RData')
