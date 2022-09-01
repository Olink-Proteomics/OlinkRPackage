#Load reference results
refRes_file <- '../data/refResults.RData'
load(refRes_file)

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
                                                            olinkid_list = kruskal_results %>%
                                                              filter(Threshold == 'Significant') %>%
                                                              dplyr::select(OlinkID) %>%
                                                              distinct() %>%
                                                              pull())

#Posthoc test for the results from Friedman Test
friedman_posthoc_results <- olink_one_non_parametric_posthoc(npx_data1,
                                                             variable = "Time",
                                                             test = "friedman",
                                                             olinkid_list = friedman_results %>%
                                                               filter(Threshold == 'Significant') %>%
                                                               dplyr::select(OlinkID) %>%
                                                               distinct() %>%
                                                               pull())

test_that("olink_one_non_parametric function works", {
  expect_equal(kruskal_results, ref_results$kruskal_results)  ##result equal to testfile
  expect_equal(friedman_results, ref_results$friedman_results)  ##result equal to testfile

  expect_equal(nrow(kruskal_results), 184)
  expect_equal(ncol(kruskal_results), 11)

  expect_equal(nrow(friedman_results), 184)
  expect_equal(ncol(friedman_results), 11)

  expect_error(olink_anova(npx_data1,)) ##no input data
})

test_that("olink_one_non_parametric_posthoc function works", {
  expect_equal(kruskal_posthoc_results, ref_results$kruskal_posthoc_results) ## result equal to testfile - posthoc
  expect_equal(friedman_posthoc_results, ref_results$friedman_posthoc_results) ## result equal to testfile - posthoc

  expect_equal(nrow(kruskal_posthoc_results), 190) ## check nr of rows
  expect_equal(nrow(friedman_posthoc_results), 3) ## check nr of rows

  expect_error(olink_one_non_parametric_posthoc(npx_data1, 'Site')) ##no olinkid list
  expect_equal(friedman_posthoc_results %>%
                 dplyr::select(contrast) %>%
                 unique() %>%
                 nrow(),3)
  expect_equal(kruskal_posthoc_results %>%
                 dplyr::select(contrast) %>%
                 unique() %>%
                 nrow(),10)
})
