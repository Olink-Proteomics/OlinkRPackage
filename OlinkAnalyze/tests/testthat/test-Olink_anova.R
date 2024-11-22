#Load reference results
refRes_file <- test_path('data','refResults.RData')
load(refRes_file)

#Load data with hidden/excluded assays (all NPX=NA)
load(file = test_path('data','npx_data_format221010.RData'))

# Remove assays with NPX == NA from npx_data_format221010 for testing
npx_data_format221010_no_NA <- npx_data_format221010 |>
  dplyr::filter(!is.na(NPX))

# Add dummy Extension control assays
extension_control <- npx_data_format221010_no_NA |>
  dplyr::filter(stringr::str_detect(Assay, "Incubation control")) |>
  dplyr::mutate(Assay = gsub("Incubation", "Extension", Assay)) |>
  dplyr::mutate(UniProt = gsub("INC", "EXT", UniProt))

npx_data_format221010_ext_ctrl <- rbind(
  npx_data_format221010_no_NA,
  extension_control
)

# Add AssayType
npx_data_format221010_AssayType <- npx_data_format221010_ext_ctrl |>
  dplyr::mutate(AssayType = dplyr::case_when(
    stringr::str_detect(Assay, "Incubation control") ~ "inc_ctrl",
    stringr::str_detect(Assay, "Amplification control") ~ "amp_ctrl",
    stringr::str_detect(Assay, "Extension control") ~ "ext_ctrl",
    TRUE ~ "assay"
  ))

# Remove control assays from npx_data_format221010 for warning tests
npx_data_format221010_no_ctrl <- npx_data_format221010 |>
  dplyr::filter(!str_detect(Assay, "control"))

# Remove sample controls from npx_data1 to preserve test results
npx_data1 <- npx_data1 |>
  dplyr::filter(!stringr::str_detect(npx_data1$SampleID, 
                                     stringr::regex("control|ctrl", 
                                                    ignore_case = TRUE)))


#Run olink_anova
anova_results_1_site <- olink_anova(npx_data1, 'Site') %>%
  dplyr::mutate(id = as.character(OlinkID)) %>%
  dplyr::arrange(id) %>%
  dplyr::select(-id)
anova_results_1_time <- olink_anova(npx_data1, 'Time') %>%
  dplyr::mutate(id = as.character(OlinkID)) %>%
  dplyr::arrange(id) %>%
  dplyr::select(-id)
anova_results_1_siteTime <- olink_anova(npx_data1, c('Site', 'Time')) %>%
  dplyr::mutate(id = as.character(OlinkID)) %>%
  dplyr::arrange(id, term) %>% #Since OlinkID is not unique here (=> ties), term is used to break the ties
  dplyr::select(-id)

#Run olink_anova_posthoc
anova_posthoc_1_site <- olink_anova_posthoc(npx_data1,
                                            variable = 'Site',
                                            olinkid_list =  {anova_results_1_site %>%
                                                head(10) %>%
                                                dplyr::pull(OlinkID)},
                                            effect = 'Site') %>%
  dplyr::mutate(id = as.character(OlinkID)) %>%
  dplyr::arrange(id, contrast) %>% #Since OlinkID is not unique here (=> ties), contrast is used to break the ties
  dplyr::mutate(contrast = as.character(contrast)) %>% # In R 3.6.1 we get factors, but reference is characters
  dplyr::select(-id)


anova_posthoc_1_time <- olink_anova_posthoc(npx_data1,
                                            variable = 'Time',
                                            {anova_results_1_time %>%
                                                head(10) %>%
                                                dplyr::pull(OlinkID)},
                                            effect = 'Time') %>%
  dplyr::mutate(id = as.character(OlinkID)) %>%
  dplyr::arrange(id, contrast) %>% #Just for consistency. Not actually needed in this case
  dplyr::mutate(contrast = as.character(contrast)) %>% # In R 3.6.1 we get factors, but reference is characters
  dplyr::select(-id)

test_that("olink_anova function works", {
  expect_equal(anova_results_1_site, ref_results$anova_results_1_site)  ##result equal to testfile
  expect_equal(anova_results_1_time, ref_results$anova_results_1_time)  ##result equal to testfile
  expect_equal(anova_results_1_siteTime, ref_results$anova_results_1_siteTime)  ##result equal to testfile

  expect_equal(nrow(anova_results_1_siteTime), 552)
  expect_equal(ncol(anova_results_1_siteTime), 12)

  expect_error(olink_anova(npx_data1,)) ##no input data
  
  expect_warning(olink_anova(npx_data_format221010_no_ctrl, 'treatment2')) # data with all NPX=NA for some assays
  
  expect_error(olink_anova(npx_data_format221010_AssayType, variable = "Site")) # Assay controls not removed, AssayType present
  expect_error(olink_anova(npx_data_format221010_ext_ctrl, variable = "Site")) # Assay controls not removed, no AssayType
  })

test_that("olink_anova_posthoc function works", {
  expect_equal(anova_posthoc_1_site, ref_results$anova_posthoc_1_site) ## result equal to testfile - posthoc
  expect_equal(nrow(anova_posthoc_1_site), 100) ## check nr of rows
  expect_error(olink_anova_posthoc(npx_data1, 'Site')) ##no olinkid list
  expect_equal(anova_posthoc_1_site %>%
                 dplyr::select(contrast) %>%
                 unique() %>%
                 nrow(),10)

  expect_equal(anova_posthoc_1_time, ref_results$anova_posthoc_1_time)
  expect_equal(nrow(anova_posthoc_1_time), 30)
  expect_equal(anova_posthoc_1_time %>% ncol(), 11)

  expect_warning(olink_anova_posthoc(npx_data_format221010_no_ctrl, variable = 'treatment2', effect = 'treatment2')) # data with all NPX=NA for some assays

  expect_error(olink_anova_posthoc(npx_data_format221010_AssayType, 
                                   variable = 'Site', 
                                   effect = 'Site')) # Assay controls not removed, AssayType present
  expect_error(olink_anova_posthoc(npx_data_format221010_ext_ctrl, 
                                   variable = 'Site', 
                                   effect = 'Site')) # Assay controls not removed, no AssayType
  
})
