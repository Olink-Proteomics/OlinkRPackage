#Load reference results
refRes_file <- '../data/refResults.RData'
load(refRes_file)

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

#New function olink_bridging
normalization_results.bridged_olink_bridging <- olink_bridging(df1 = npx_data1,
                                                               df2 = npx_data2,
                                                               overlapping_samples = overlap_samples,
                                                               df1_project_nr = '20200001',
                                                               df2_project_nr = '20200002',
                                                               reference_project = '20200001') %>%
  filter(SampleID %in% sampleSubset)

#New function olink_normalization_v2
# Intensity normalization
normalization_results.intensity_v2 <- olink_normalization_v2(df1 = npx_data1,
                                                             df2 = npx_data2,
                                                             normalization_samples_df1 = npx_data1$SampleID,
                                                             normalization_samples_df2 = npx_data2$SampleID) %>%
  filter(SampleID %in% sampleSubset)

# Subset normalization
#NOTE: this subset is just a random sample in order to test the function
sampleSubset.adj <- c("C6", "C21","C28","C50","C19","D5", "A30","C52","D77","D3", "D16","C72","A52","D67","C77","C22","D62","D39","C34","C13")
normalization_results.subset_v2 <- olink_normalization_v2(df1 = npx_data1,
                                                          df2 = npx_data2,
                                                          normalization_samples_df1 = npx_data1$SampleID,
                                                          normalization_samples_df2 = sampleSubset.adj,
                                                          df1_project_nr = '20200001',
                                                          df2_project_nr = '20200002',
                                                          reference_project = '20200001') %>%
  filter(SampleID %in% sampleSubset)

test_that("olink_normalization works", {
  expect_equal(normalization_results.bridged, ref_results$normalization_results.bridged)
  expect_equal(normalization_results.intensity, ref_results$normalization_results.intensity)
  expect_equal(normalization_results.subset, ref_results$normalization_results.subset)
  expect_error(olink_normalization(df1 = npx_data1,
                                   df2 = npx_data2)) # No samples specified
  expect_error(olink_normalization(df1 = npx_data1,
                                   df2 = npx_data2,
                                   overlapping_samples_df1 = c("B64", "B36", "A77", "B7", "A24", "A49", "B76"))) # Non overlapping samples for bridging

  #New functions
  expect_equal(normalization_results.bridged_olink_bridging, ref_results$normalization_results.bridged)
  expect_equal(normalization_results.intensity_v2, ref_results$normalization_results.intensity)
  expect_equal(normalization_results.subset_v2, ref_results$normalization_results.subset)
})
