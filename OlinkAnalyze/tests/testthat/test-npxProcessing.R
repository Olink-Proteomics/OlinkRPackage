#Load reference results
refRes_file <- testthat::test_path("../data/refResults.RData")
load(refRes_file)

npx_data1.uniqIDs <- npx_data1 %>%
  mutate(SampleID = paste(SampleID, "_", Index, sep = ""))
procData <- npxProcessing_forDimRed(df = npx_data1.uniqIDs,
                                    color_g = 'QC_Warning',
                                    drop_assays = F,
                                    drop_samples = F,
                                    verbose = T)

#With missing data
samples <- sample({npx_data1.uniqIDs$SampleID %>% unique()},
                  size = {ceiling( (npx_data1.uniqIDs$SampleID %>% unique() %>% length())*.15 )})

npx_data1.uniqIDs_missingData <- npx_data1.uniqIDs %>%
  mutate(NPX = ifelse(SampleID %in% samples & OlinkID %in% c('OID00482', 'OID00483', 'OID00484', 'OID00485'), NA, NPX)) %>% #These should be removed due to to high missingness
  mutate(NPX = ifelse(SampleID %in% c('A18_19', 'B8_87') & OlinkID %in% c('OID00562', 'OID01213', 'OID05124'), NA, NPX))    #These should be median imputed

w <- testthat::capture_warnings(
  procData_missingData <- npxProcessing_forDimRed(df = npx_data1.uniqIDs_missingData,
                                                  color_g = 'QC_Warning',
                                                  drop_assays = F,
                                                  drop_samples = F,
                                                  verbose = T)
)


test_that("npxProcessing_forDimRed works", {
  expect_equal(procData$df_wide %>% arrange(SampleID), ref_results$procData$df_wide %>% arrange(SampleID))
  expect_equal(procData$df_wide_matrix[sort(row.names(procData$df_wide_matrix)),],
               ref_results$procData$df_wide_matrix[sort(row.names(ref_results$procData$df_wide_matrix)),])
  expect_null(procData$dropped_assays.na)
  expect_null(procData$dropped_assays.missingness)

  #With missing data
  expect_equal(w, c("There are 4 assay(s) dropped due to high missingness (>10%).", "There are 3 assay(s) that were imputed by their medians."))
  expect_equal(procData_missingData$df_wide %>% arrange(SampleID), ref_results$procData_missingData$df_wide  %>% arrange(SampleID))
  expect_equal(procData_missingData$df_wide_matrix[sort(row.names(procData_missingData$df_wide_matrix)),],
               ref_results$procData_missingData$df_wide_matrix[sort(row.names(ref_results$procData_missingData$df_wide_matrix)),])
  expect_null(procData_missingData$dropped_assays.na)
  expect_equal(procData_missingData$dropped_assays.missingness, c('OID00482', 'OID00483', 'OID00484', 'OID00485'))
})

#Load data with hidden/excluded assays (all NPX=NA)
load(file = '../data/npx_data_format221010.RData')
npx_Check <- suppressWarnings(npxCheck(npx_data_format221010))

test_that("Assays with NA are removed in NPX check", {
  na_oids<-npx_data_format221010 %>%
    dplyr::filter(is.na(NPX)) %>%
    dplyr::select(OlinkID) %>%
    dplyr::distinct()
  expect_equal(sort(na_oids$OlinkID), sort(npx_Check$all_nas))
})

