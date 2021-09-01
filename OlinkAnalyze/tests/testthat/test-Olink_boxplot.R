#Load reference results
refRes_file <- '../data/refResults.RData'
load(refRes_file)

boxplot_site_2prots <- npx_data1 %>%
  na.omit() %>% # removing missing values which exists for Site
  olink_boxplot(variable = "Site",
                olinkid_list = {ref_results$anova_results_1_site %>%
                    filter(Threshold == 'Significant') %>%
                    head(2) %>%
                    pull(OlinkID)})

boxplot_site_10prots <- npx_data1 %>%
  na.omit() %>% # removing missing values which exists for Site
  olink_boxplot(variable = "Site",
                olinkid_list = {ref_results$anova_results_1_site %>%
                    filter(Threshold == 'Significant') %>%
                    head(10) %>%
                    pull(OlinkID)},
                number_of_proteins_per_plot = 5)

boxplot_time <- npx_data1 %>%
  olink_boxplot(variable = "Time",
                olinkid_list = {ref_results$anova_results_1_time %>%
                    filter(Threshold == 'Significant') %>%
                    pull(OlinkID)})

test_that("olink_boxplot works", {
  vdiffr::expect_doppelganger('boxplot site 2prots', boxplot_site_2prots)
  vdiffr::expect_doppelganger('boxplot site 10prots', boxplot_site_10prots[[2]])
  vdiffr::expect_doppelganger('boxplot time', boxplot_time)
})
