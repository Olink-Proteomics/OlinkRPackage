#Load reference results
refRes_file <- test_path('data','refResults.RData')
load(refRes_file)

#Load data with hidden/excluded assays (all NPX=NA)
load(file = test_path('data','npx_data_format221010.RData'))

# One-way Kruskal-Wallis Test
kruskal_results <- olink_one_non_parametric(df = npx_data1,
                                            variable = "Site")
# One-way Friedman Test
friedman_results <- olink_one_non_parametric(df = npx_data1,
                                             variable = "Time",
                                             subject = "Subject",
                                             dependence = TRUE)

#Posthoc test for the results from Kruskal-Wallis Test
if (requireNamespace("FSA", quietly = TRUE) ){
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
}
#Posthoc test for the results from Friedman Test
friedman_posthoc_results <- olink_one_non_parametric_posthoc(npx_data1,
                                                             variable = "Time",
                                                             test = "friedman",
                                                             subject = "Subject",
                                                             olinkid_list = {friedman_results %>%
                                                               filter(Threshold == 'Significant') %>%
                                                               dplyr::select(OlinkID) %>%
                                                               distinct() %>%
                                                               pull()}) %>%
  mutate(id = as.character(OlinkID)) %>%
  arrange(id, contrast) %>% #Just for consistency. Not actually needed in this case
  select(-id)

test_that("olink_one_non_parametric function works", {
  if (requireNamespace("FSA", quietly = TRUE) ){
    expect_equal(kruskal_results, ref_results$kruskal_results)  ##result equal to testfile
  }
  expect_equal(friedman_results, ref_results$friedman_results)  ##result equal to testfile
  if (requireNamespace("FSA", quietly = TRUE) ){
    expect_equal(nrow(kruskal_results), 184)
    expect_equal(ncol(kruskal_results), 11)
  }

  expect_equal(nrow(friedman_results), 184)
  expect_equal(ncol(friedman_results), 11)

  expect_error(olink_one_non_parametric(npx_data1,)) ##no input data

  expect_warning(olink_one_non_parametric(npx_data_format221010, 'treatment2')) # data with all NPX=NA for some assays
})

test_that("olink_one_non_parametric_posthoc function works", {
  if (requireNamespace("FSA", quietly = TRUE) ){
    expect_equal(kruskal_posthoc_results, ref_results$kruskal_posthoc_results) ## result equal to testfile - posthoc
    expect_equal(nrow(kruskal_posthoc_results), 190) ## check nr of rows
    expect_equal(kruskal_posthoc_results %>%
                   dplyr::select(contrast) %>%
                   unique() %>%
                   nrow(),10)
    expect_error(olink_one_non_parametric_posthoc(npx_data1, 'Site')) ##no olinkid list

    expect_warning(olink_one_non_parametric_posthoc(npx_data_format221010, variable = 'treatment2')) # data with all NPX=NA for some assays

  }
  expect_equal(friedman_posthoc_results, ref_results$friedman_posthoc_results) ## result equal to testfile - posthoc


  expect_equal(nrow(friedman_posthoc_results), 3) ## check nr of rows


  expect_equal(friedman_posthoc_results %>%
                 dplyr::select(contrast) %>%
                 unique() %>%
                 nrow(),3)

})

