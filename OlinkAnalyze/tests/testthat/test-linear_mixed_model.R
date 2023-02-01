skip_on_cran()
skip_if_not_installed("ggplot2", minimum_version = "3.4.0")

# Suppress messages
sink(file = file(tempfile(), open = "wt"), type = "message")

#Load reference results
refRes_file <- '../data/refResults.RData'
load(refRes_file)
load(file = '../data/npx_data_format221010.RData')
npx_Check <- suppressWarnings(npxCheck(npx_data_format221010))

#Load data with hidden/excluded assays (all NPX=NA)
load(file = '../data/npx_data_format221010.RData')

#Run olink_lmer
lmer_results_1 <- olink_lmer(df = npx_data1,
                             variable = c('Treatment', "Time"),
                             random = "Subject") %>%
  mutate(id = as.character(OlinkID)) %>%
  arrange(id, term) %>% #Since OlinkID is not unique here (=> ties), term is used to break the ties
  select(-id)

#Run olink_lmer_posthoc
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
  mutate(contrast = as.character(contrast)) %>% # In R 3.6.1 we get factors, but reference is characters
  select(-id)

#Run olink_lmer_plot
lmer_plot <- olink_lmer_plot(df = npx_data1,
                             variable = c('Treatment', 'Time'),
                             random = "Subject",
                             olinkid_list = {ref_results$lmer_results_1 %>%
                                 dplyr::filter(term == 'Treatment:Time' & Threshold == 'Significant') %>%
                                 head(6) %>%
                                 dplyr::pull(OlinkID)},
                             x_axis_variable = "Time",
                             col_variable = "Treatment")

lmer_plot_moreProts <- olink_lmer_plot(df = npx_data1,
                                       variable = c('Treatment', 'Time'),
                                       random = "Subject",
                                       olinkid_list = {ref_results$lmer_results_1 %>%
                                           dplyr::filter(term == 'Treatment:Time' & Threshold == 'Significant') %>%
                                           head(10) %>%
                                           dplyr::pull(OlinkID)},
                                       x_axis_variable = "Time",
                                       col_variable = "Treatment", number_of_proteins_per_plot = 5)

test_that("olink_lmer works", {
  expect_equal(lmer_results_1, ref_results$lmer_results_1, tolerance = 1e-4)
  expect_error(olink_lmer(npx_data1))
  expect_warning(olink_lmer(npx_data_format221010, variable = 'treatment1', random = 'SubjectDummy')) # data with all NPX=NA for some assays
})

test_that("olink_lmer_posthoc works", {
  expect_equal(lmer_results_1_posthoc, ref_results$lmer_results_1_posthoc)
  expect_error(olink_lmer_posthoc(df = npx_data1,
                                  variable = c('Treatment', "Time"),
                                  random = "Subject",
                                  olinkid_list = {lmer_results_1 %>%
                                      dplyr::filter(term == 'Treatment:Time') %>%
                                      dplyr::filter(Threshold == 'Significant') %>%
                                      dplyr::pull(OlinkID)})) # no effect specified

  expect_warning(olink_lmer_posthoc(df = npx_data_format221010,
                                    variable = 'treatment1',
                                    effect = 'treatment1',
                                    random = 'SubjectDummy')) # data with all NPX=NA for some assays
})

lmer_plot_excludedids<- suppressWarnings(olink_lmer_plot(df = npx_data_format221010,
               variable = c('treatment1'),
               random = "SubjectDummy",
               olinkid_list = c(npx_Check$all_nas[1:5],"OID30538"),
               x_axis_variable = "treatment1", number_of_proteins_per_plot = 5))

test_that("olink_lmer_plot works", {
  vdiffr::expect_doppelganger('lmer plot', lmer_plot)
  vdiffr::expect_doppelganger('lmer plot more prots than space', lmer_plot_moreProts[[2]])
  expect_length(unique(lmer_plot_excludedids[[1]]$data$OlinkID), 1)
})
