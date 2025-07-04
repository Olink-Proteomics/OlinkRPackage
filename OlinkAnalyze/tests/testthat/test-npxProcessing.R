#Load reference results
refRes_file <- testthat::test_path("data","refResults.RData")
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
load(file = test_path('data','npx_data_format221010.RData'))
load(file = test_path('data','npx_data_format221121.RData'))

npx_Check <- suppressWarnings(npxCheck(npx_data_format221010))

test_that("Assays with NA are removed in NPX check", {
  na_oids<-npx_data_format221010 %>%
    dplyr::group_by(OlinkID) %>%
    dplyr::filter(all(is.na(NPX))) %>%
    dplyr::select(OlinkID) %>%
    dplyr::distinct()
  expect_equal(sort(na_oids$OlinkID), sort(npx_Check$all_nas))
})

npx_Check <- suppressWarnings(npxCheck(npx_data_format221121))

test_that("Assays with NA are removed in NPX check", {
  na_oids<-npx_data_format221121 %>%
    dplyr::group_by(OlinkID) %>%
    dplyr::filter(all(is.na(NPX))) %>%
    dplyr::select(OlinkID) %>%
    dplyr::distinct()
  expect_equal(sort(na_oids$OlinkID), sort(npx_Check$all_nas))
})

test_that("No error if missing data from 1st OID",{
  skip_if_not(condition = getRversion() >= "4.2.0",
              message = "Skipping for R < 4.2.0")

  expect_no_error(suppressWarnings(suppressWarnings(npx_data1.uniqIDs %>%
    mutate(QC_Warning = ifelse(OlinkID == "OID01216" & stringr::str_detect(SampleID, "2"),
                               "Warn", QC_Warning)) %>%
    filter(QC_Warning == "Pass") %>%
    olink_pca_plot())))
  expect_no_error(suppressWarnings(suppressWarnings(npx_data1.uniqIDs %>%
                                                      mutate(QC_Warning = ifelse(OlinkID == "OID01216" & stringr::str_detect(SampleID, "A"),
                                                                                 "Warn", QC_Warning)) %>%
                                                      filter(QC_Warning == "Pass") %>%
                                                      olink_pca_plot())))


})

# data with SampleQC instead of QC_Warning
w2 <- testthat::capture_error(
  procData_missingData <- npx_data1.uniqIDs %>% dplyr::rename(SampleQC = QC_Warning) %>%
    npxProcessing_forDimRed(color_g = 'QC_Warning',
                            drop_assays = F,
                            drop_samples = F,
                            verbose = T)
  )

test_that("npxProcessing_forDimRed does not recognize QC_Warning", {
  expect_equal(w2, simpleError("In color_g = \"QC_Warning\", QC_Warning was not found. Did you mean color_g = \"SampleQC\"?"))
})

# test that npxCheck detects duplicate sample IDs
npx_Check <- suppressMessages(npxCheck(npx_data1))

test_that("npxCheck detects duplicate sample IDs.",
         {expect_equal(npx_Check$duplicate_samples, c("CONTROL_SAMPLE_AS 1", "CONTROL_SAMPLE_AS 2"))})

# test for duplicate UniProts for 1 OlinkID
npx_data_dup <- npx_data1
npx_data_dup$UniProt[5] <- "new_uniprot"
npx_data_dup$UniProt[10] <- "another_new_uniprot"
npx_data_dup <- npx_data_dup |>
  dplyr::filter(!stringr::str_detect(SampleID,"CONTROL"))

test_that("npxCheck detects duplicated UniProt IDs.", {
  expect_warning(assay_identifiers(npx_data_dup),
                 regexp = "OlinkID has multiple unique UniProt IDs.")
  expect_equal(unique(suppressWarnings(assay_identifiers(npx_data_dup)$OlinkID)),
               "OID01216")
  expect_equal(unique(suppressWarnings(
    assay_identifiers(npx_data_dup)$new_UniProt)),
               "O00533")
  expect_equal(unique(suppressWarnings(
    assay_identifiers(npx_data_dup)$UniProt)),
                      c("new_uniprot",
                      "another_new_uniprot"))
})

# Test that UniProts are replaced correctly
test_that("UniProts replaced correctly",
          {
            npx_check <- suppressWarnings(npxCheck(df = npx_data_dup))
            df <- uniprot_replace(df = npx_data_dup, npx_check = npx_check)
            expect_equal(df, npx_data1 |>
                           dplyr::filter(!stringr::str_detect(SampleID,"CONTROL")))
          })

# npxProcessing_forDimRed snapshot ----------------------------------------

test_that("npxProcessing_forDimRed snapshot", {
  oids_to_use <- sort(unique(npx_data1$OlinkID))[1:10]
  sids_to_use <- sort(unique(npx_data1$SampleID))[1:10]

  df <- npx_data1 %>%
    filter(SampleID %in% sids_to_use & OlinkID %in% oids_to_use) %>%
    mutate(SampleID = paste(SampleID, "_", Index, sep = "")) %>%
    npxProcessing_forDimRed()

  expect_snapshot_value(df, style = "deparse")
})
