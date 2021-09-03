#Load reference results
refRes_file <- '../data/refResults.RData'
load(refRes_file)

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
  expect_equal(lmer_results_1, ref_results$lmer_results_1)
  expect_error(olink_lmer(npx_data1))
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
})

test_that("olink_lmer_plot works", {
  skip_on_ci()
  vdiffr::expect_doppelganger('lmer plot', lmer_plot)
  vdiffr::expect_doppelganger('lmer plot more prots than space', lmer_plot_moreProts[[2]])
})
