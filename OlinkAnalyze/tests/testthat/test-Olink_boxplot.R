skip_on_cran()
skip_if_not_installed("ggplot2", minimum_version = "3.4.0")

#Load reference results
refRes_file <- '../data/refResults.RData'
load(refRes_file)
load(file = '../data/npx_data_format221010.RData')
npx_Check <- suppressWarnings(npxCheck(npx_data_format221010))

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
                    head(10) %>%
                    pull(OlinkID)})

boxplot_time_coloroption <- npx_data1 %>%
  olink_boxplot(variable = "Time",
                olinkid_list = {ref_results$anova_results_1_time %>%
                    head(10) %>%
                    pull(OlinkID)},
                coloroption = c("teal","pink","orange","turqoise"))

boxplot_time_site <- npx_data1 %>%
  na.omit() %>%
  olink_boxplot(variable = c("Time","Site"),
                olinkid_list = {ref_results$anova_results_1_time %>%
                    head(10) %>%
                    pull(OlinkID)})


test_that("olink_boxplot works", {
  if (requireNamespace("vdiffr", quietly = TRUE) ){
    vdiffr::expect_doppelganger('boxplot site 2prots', boxplot_site_2prots)
    vdiffr::expect_doppelganger('boxplot site 10prots', boxplot_site_10prots[[2]])
    vdiffr::expect_doppelganger('boxplot time', boxplot_time)
    vdiffr::expect_doppelganger('boxplot time with coloroption', boxplot_time_coloroption)
    vdiffr::expect_doppelganger('boxplot time and site', boxplot_time_site)
  }
  expect_warning(npx_data_format221010 %>%
                   olink_boxplot(variable = "treatment2",
                                 olinkid_list = c(npx_Check$all_nas[1:5],"OID30538")))

  boxplot_npxcheck <- suppressWarnings(olink_boxplot(npx_data_format221010, variable = "treatment2",
                                   olinkid_list = c(npx_Check$all_nas[1:5],"OID30538")))
  expect_length(unique(boxplot_npxcheck[[1]]$data$Name_OID), 1)
})
