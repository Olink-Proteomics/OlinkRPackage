#Load reference results
refRes_file <- '../data/refResults.RData'
load(refRes_file)

#Load data with hidden/excluded assays (all NPX=NA)
load(file = '../data/npx_data_format221010.RData')

# Sample subset used to reduce file size of the ref results
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


# Warning if column names arent the same but results still match
npx_df2 <- OlinkAnalyze::npx_data2 %>% dplyr::mutate(Project = 'P2')
npx_df_test<-OlinkAnalyze::npx_data1 %>%
  dplyr::mutate(Sample_type = "Sample") %>%
  dplyr::select(SampleID, Sample_type, Index, OlinkID, UniProt, Assay, MissingFreq, Panel_Version, PlateID, QC_Warning, LOD, NPX, Subject)
overlap_samples2 <- intersect(npx_data1$SampleID, npx_data2$SampleID) %>%
  data.frame() %>%
  filter(!str_detect(., 'CONTROL_SAMPLE')) %>% #Remove control samples
  pull(.)
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

# Bridge normalization with excluded assays
excludedOIDs.proj1 <-
  npx_data_format221010 %>%
  group_by(OlinkID) %>%
  summarise(fracNA = sum(is.na(NPX))/n()) %>%
  filter(fracNA == 1) %>%
  pull(OlinkID)

excludedOIDs.proj2<-
  npx_data_format221010.project2 %>%
  group_by(OlinkID) %>%
  summarise(fracNA = sum(is.na(NPX))/n()) %>%
  filter(fracNA == 1) %>%
  pull(OlinkID)

overlap_samples <- intersect(npx_data_format221010$SampleID, npx_data_format221010.project2$SampleID)
npxBridged <- olink_normalization(df1 = npx_data_format221010,
                                  df2 = npx_data_format221010.project2,
                                  overlapping_samples_df1 = overlap_samples,
                                  reference_project = 'P1')

npxBridged_proj2ref <- olink_normalization(df1 = npx_data_format221010,
                                           df2 = npx_data_format221010.project2,
                                           overlapping_samples_df1 = overlap_samples,
                                           reference_project = 'P2')

test_that("olink_normalization works", {
  expect_warning(expect_warning(olink_normalization(df1 = npx_df_test,
                                     df2 = npx_df2,
                                     overlapping_samples_df1 = overlap_samples2,
                                     df1_project_nr = 'P1',
                                     df2_project_nr = 'P2',
                                     reference_project = 'P1')))
  expect_equal(normalization_results.bridged, ref_results$normalization_results.bridged)
  expect_equal(normalization_results.intensity, ref_results$normalization_results.intensity)
  expect_equal(normalization_results.subset, ref_results$normalization_results.subset)
  expect_error(olink_normalization(df1 = npx_data1,
                                   df2 = npx_data2)) # No samples specified
  expect_error(olink_normalization(df1 = npx_data1,
                                   df2 = npx_data2,
                                   overlapping_samples_df1 = c("B64", "B36", "A77", "B7", "A24", "A49", "B76"))) # Non overlapping samples for bridging

  ### Testing the excluded assay bridging ###
  ## With P1 as the reference
  #Test that all excluded assays from project 1 remain NA after bridging
  expect_true(npxBridged %>%
                filter(Project == 'P1' & OlinkID %in% excludedOIDs.proj1) %>%
                pull(NPX) %>%
                is.na() %>%
                all())

  #Test that the non-excluded assays in project 2 remain unchanged
  expect_true(npxBridged %>%
                filter(Project == 'P2' & OlinkID %in% setdiff(excludedOIDs.proj1, excludedOIDs.proj2)) %>%
                left_join(npx_data_format221010.project2, by = c('SampleID', 'OlinkID')) %>%
                mutate(match = NPX.x == NPX.y) %>%
                pull(match) %>%
                all())

  ## With P2 as the reference
  #Test that all excluded assays from project 1 remain NA after bridging
  expect_true(npxBridged_proj2ref %>%
                filter(Project == 'P1' & OlinkID %in% excludedOIDs.proj1) %>%
                pull(NPX) %>%
                is.na() %>%
                all())

  #Test that the non-excluded assays in project 2 remain unchanged
  expect_true(npxBridged_proj2ref %>%
                filter(Project == 'P2' & OlinkID %in% setdiff(excludedOIDs.proj1, excludedOIDs.proj2)) %>%
                left_join(npx_data_format221010.project2, by = c('SampleID', 'OlinkID')) %>%
                mutate(match = NPX.x == NPX.y) %>%
                pull(match) %>%
                all())
})
