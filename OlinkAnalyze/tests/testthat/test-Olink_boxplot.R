skip_on_cran()
skip_if_not_installed("ggplot2", minimum_version = "3.4.0")

#Load reference results
refRes_file <- test_path('data','refResults.RData')
load(refRes_file)
load(file = test_path('data','npx_data_format221010.RData'))
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
  expect_warning(npx_data_format221010 %>%
                   olink_boxplot(variable = "treatment2",
                                 olinkid_list = c(npx_Check$all_nas[1:5],"OID30538")))

  boxplot_npxcheck <- suppressWarnings(olink_boxplot(npx_data_format221010, variable = "treatment2",
                                                     olinkid_list = c(npx_Check$all_nas[1:5],"OID30538")))
  expect_length(unique(boxplot_npxcheck[[1]]$data$Name_OID), 1)
})

test_that("olink_boxplot works - vdiffr", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")

  boxplot_site_2prots_name <- "boxplot site 2prots"
  check_snap_exist(test_dir_name = "Olink_boxplot", snap_name = boxplot_site_2prots_name)
  vdiffr::expect_doppelganger(boxplot_site_2prots_name, boxplot_site_2prots)

  boxplot_site_10prots_name <- "boxplot site 10prots"
  check_snap_exist(test_dir_name = "Olink_boxplot", snap_name = boxplot_site_10prots_name)
  vdiffr::expect_doppelganger(boxplot_site_10prots_name, boxplot_site_10prots[[2]])

  boxplot_time_name <- "boxplot time"
  check_snap_exist(test_dir_name = "Olink_boxplot", snap_name = boxplot_time_name)
  vdiffr::expect_doppelganger(boxplot_time_name, boxplot_time)

  boxplot_time_coloroption_name <- "boxplot time with coloroption"
  check_snap_exist(test_dir_name = "Olink_boxplot", snap_name = boxplot_time_coloroption_name)
  vdiffr::expect_doppelganger(boxplot_time_coloroption_name, boxplot_time_coloroption)

  boxplot_time_site_name <- "boxplot time and site"
  check_snap_exist(test_dir_name = "Olink_boxplot", snap_name = boxplot_time_site_name)
  vdiffr::expect_doppelganger(boxplot_time_site_name, boxplot_time_site)
})
