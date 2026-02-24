# skip_if_not_installed("dplyr")
# skip_if_not_installed("tibble")

set.seed(123)  # reproducible randomization

osi_data <- dplyr::tibble(
  OSITimeToCentrifugation = c(
    0.3012289,0.060720572,0.94772694,0.720596273,0.142294296,
    0.549284656,0.954091239,0.585483353,0.404510282,0.647893479,
    0.319820617,0.307720011,0.219767631,0.369488866,0.984219203,
    0.154202301,0.091044,0.141906908,0.690007102,0.619256483,
    0.891394117,0.672999093,0.737077738,0.521135726,0.65983845,
    NA_real_,0.786281552,0.979821917,0.439431536,0.311702202,
    0.409474953,0.010467112,0.183849524,0.842729319,0.231161782,
    0.239099956,0.076691165,0.245723678,0.732135206,0.847453165,
    0.497527267,0.38790903,0.246448994,0.111096461,0.389994435,
    0.571935314,0.216892763,0.444768002,0.217990669,0.502299563,
    0.353904572,0.649985159,0.374713957,0.355445381,0.533687945,
    0.74033436,0.221102938,0.412746119,0.265686687,0.629973053,
    0.183828491,0.863644111,0.746568004,0.66828465,0.618017873,
    0.37223806,0.529835686,0.874682343,0.5817501,0.839767765,
    0.312448165,0.708290322,0.265017806,0.594343194,0.4812898,
    0.265032731,0.564590435,0.913188223,0.901874389,0.274166622,
    0.321482756,0.985640884,0.61999331,0.937314089,0.466532702,
    0.406832593,0.659230324,0.152346617,0.572867058,0.238726027,
    0.962358936,0.601365726,0.515029727,0.402573342,0.880246541,
    0.364091865,0.288239281,0.170645235,0.172171746,0.482042606
  ),

  OSIPreparationTemperature = c(
    0.252964929,0.21625479,0.674376388,0.047663627,0.700853087,
    0.351888638,0.408943998,0.820951324,0.918857348,0.28252833,
    0.961104794,0.728394428,0.686375082,0.052843943,0.395220135,
    0.47784538,0.560253264,0.698261595,0.915683538,0.618351227,
    0.428421509,0.542080367,0.058478489,0.260856857,0.397151953,
    0.197744737,0.831927563,0.152887223,0.803418542,0.546826157,
    0.662317642,0.171698494,0.63305536,0.311869747,0.724554346,
    0.398939825,0.969356411,0.967398371,0.726702539,0.257216746,
    NA_real_,0.593045652,0.267521432,0.531070399,0.785291671,
    0.168060811,0.404399181,0.471576278,0.868106807,0.925707956,
    0.881977559,0.674186843,0.950166979,0.516444894,0.576519021,
    0.336331206,0.347324631,0.020024301,0.502813046,0.871043414,
    0.006300784,0.072057124,0.164211225,0.770334074,0.735184306,
    0.971875636,0.466472377,0.074384513,0.648818124,0.75859317,
    0.137106081,0.396584595,0.224985329,0.057958561,0.395892688,
    0.0649283,0.225886433,0.054629109,0.67028204,0.297741783,
    0.100721582,0.071904097,0.880440569,0.754247402,0.816605888,
    0.982140374,0.103599645,0.099041829,0.798831611,0.784575267,
    0.009429905,0.779065883,0.729390652,0.630131853,0.48091083,
    0.156636851,0.00821552,0.452458394,0.492293329,0.389587112
  ),

  OSISummary = c(
    0.354783098,0.802812273,0.835708837,0.237749405,0.353986103,
    0.85688542,0.853763374,0.295895455,0.147048325,0.703992061,
    0.103806688,0.033727773,0.999404528,0.034874804,0.338391284,
    0.915063762,0.61723527,0.286285349,0.737797403,0.834054309,
    0.31427078,0.492566548,0.697373766,0.641462354,0.643922915,
    NA_real_,0.414735334,0.11940478,0.52602966,0.225073351,
    0.486411764,0.370214798,0.983350181,0.388319115,0.22924484,
    0.623297546,0.136540197,0.967469494,0.515071808,0.163070329,
    NA_real_,0.985954165,0.668771517,0.418915897,0.323344993,
    0.83525532,0.143817044,0.192815947,0.896738683,0.308119554,
    0.363300544,0.783946479,0.19337868,0.017765812,0.406607866,
    0.483167669,0.42184495,0.342808802,0.866483315,0.455108051,
    0.533764874,0.963843332,0.774591542,0.208876349,0.308786833,
    0.97134245,0.584900093,0.760823625,0.372709394,0.769193911,
    0.537677183,0.91399545,0.185296442,0.282218417,0.094962413,
    0.210487079,0.977098996,0.296302175,0.725983027,0.785687834,
    0.105417746,0.239594629,0.270544872,0.101058494,0.117913841,
    0.991236556,0.986054297,0.137067471,0.905309582,0.576301838,
    0.395448859,0.449802484,0.706501901,0.082502746,0.33931258,
    0.680787551,0.316949248,0.831568598,0.215172083,0.497948936
  ),

  OSICategory = c(
    2,4,4,1,2,4,4,2,1,3,
    1,1,4,1,2,4,3,2,3,4,
    2,2,3,3,3,0,2,1,3,1,
    2,2,4,2,1,3,1,4,3,1,
    0,4,3,2,2,4,1,1,4,2,
    2,4,1,1,2,2,2,2,4,2,
    3,4,4,1,2,4,3,4,2,4,
    3,4,1,2,1,1,4,2,3,4,
    1,1,2,1,1,4,4,1,4,3,
    2,2,3,1,2,3,2,4,1,2
  )
)

new_ids <- c(paste0("A", 1:77), paste0("B", 1:23))
stopifnot(nrow(osi_data) == length(new_ids))

osi_data <- osi_data |>
  mutate(SampleID = sample(new_ids, size = n(), replace = FALSE))

data1 <- OlinkAnalyze::npx_data1 |>
  dplyr::right_join(
    osi_data |>
      select(all_of(c(
        "SampleID",
        "OSITimeToCentrifugation",
        "OSIPreparationTemperature",
        "OSISummary",
         "OSICategory"
      ))),
    by = "SampleID"
  ) |>
  dplyr::filter(
    !grepl("CONTROL", SampleID)
    ) |>
  dplyr::mutate(
    SampleType = "SAMPLE",
    AssayType = "assay",
    AssayQC = "PASS"
  )

test_that("OSI errors: using npx_data1 and OSI columns", {
  # ----------------------------
  # OSICategory invalid value
  # ----------------------------
  df_bad_cat <- data1 |>
    mutate(OSICategory = if_else(dplyr::row_number() == 1, "7",
                                 as.character(OSICategory)))

  testthat::expect_error(
    olink_pca_plot(df_bad_cat, quiet = TRUE),
    regexp = "Invalid values detected in OSICategory\\. Expected only 0, 1, 2, 3, or 4\\. Found: 7"
  )

  # ----------------------------
  # OSICategory all NA
  # ----------------------------
  df_cat_all_na <- data1 %>% mutate(OSICategory = NA)

  testthat::expect_error(
    olink_pca_plot(df_cat_all_na, quiet = TRUE),
    regexp = "All values are NA in OSICategory\\. Please provide at least one non-missing value\\."
  )

  # ------------------------------------------
  # Continuous OSI column non-numeric value
  # ------------------------------------------
  df_bad_cont_nonnum <- data1 %>%
    mutate(
      OSITimeToCentrifugation = if_else(dplyr::row_number() == 1,
                                        "oops",
                                        as.character(OSITimeToCentrifugation)))

  testthat::expect_error(
    olink_pca_plot(df_bad_cont_nonnum, quiet = TRUE),
    regexp = paste0(
      "Invalid values detected in OSITimeToCentrifugation\\.",
      " Expected continuous numeric values between 0 and 1\\.",
      " Found non-numeric value\\(s\\): oops"
    )
  )

  # ------------------------------------
  # Continuous OSI column out of range
  # ------------------------------------
  df_bad_cont_oor <- data1 %>%
    mutate(OSIPreparationTemperature = if_else(dplyr::row_number() == 1, 1.2,
                                               OSIPreparationTemperature))

  testthat::expect_error(
    olink_pca_plot(df_bad_cont_oor, quiet = TRUE),
    regexp = paste0(
      "Invalid values detected in OSIPreparationTemperature\\.",
      " Expected continuous numeric values between 0 and 1\\.",
      " Found out-of-range value\\(s\\): 1\\.2"
    )
  )

  # ----------------------------
  # Continuous OSI column all NA
  # ----------------------------
  df_cont_all_na <- data1 %>% mutate(OSISummary = NA)

  testthat::expect_error(
    olink_pca_plot(df_cont_all_na, quiet = TRUE),
    regexp = "All values are NA in OSISummary\\. Please provide at least one non-missing value\\."
  )

  # ------------------------------------------------------
  # Valid OSI values should NOT trigger OSI error strings
  # ------------------------------------------------------
  testthat::expect_no_error(
    olink_pca_plot(data1, quiet = TRUE)
  )

})

# -------------------------------------------------------
skip_if_not_installed("ggplot2", minimum_version = "3.4.0")
skip_on_cran()

# Skip in newer versions of R
skip_if(
  R.version$major > 4 ||
    (R.version$major == 4 && as.numeric(R.version$minor) > 2.3)
)

set.seed(10)
#Load reference results
refRes_file <- testthat::test_path('data','refResults.RData')
load(refRes_file)

#Load data with hidden/excluded assays (all NPX=NA)
load(file = testthat::test_path('data','npx_data_format221010.RData'))
load(file = testthat::test_path('data','npx_data_format221121.RData'))

pca_plot <- npx_data1 %>%
  mutate(SampleID = paste(SampleID, "_", Index, sep = "")) %>%
  olink_pca_plot(quiet = TRUE)

pca_plot_treatCol <- npx_data1 %>%
  mutate(SampleID = paste(SampleID, "_", Index, sep = "")) %>%
  filter(!is.na(Treatment)) %>% #Or else, a warning shows up in the test results
  olink_pca_plot(color_g = 'Treatment', quiet = TRUE)

pca_plot_treatCol_topLoadings <- npx_data1 %>%
  mutate(SampleID = paste(SampleID, "_", Index, sep = "")) %>%
  filter(!is.na(Treatment)) %>% #Or else, a warning shows up in the test results
  olink_pca_plot(color_g = 'Treatment',
                 loadings_list = {ref_results$t.test_results %>%
                     head(5) %>%
                     pull(OlinkID)},
                 quiet = TRUE)

#PCA by panel
pca_plot_byPanel <- npx_data1 %>%
  mutate(SampleID = paste(SampleID, "_", Index, sep = "")) %>%
  olink_pca_plot(byPanel = TRUE, quiet = TRUE)

#Label outliers
pca_plot_byPanel_outliers <- npx_data1 %>%
  mutate(SampleID = paste(SampleID, "_", Index, sep = "")) %>%
  olink_pca_plot(byPanel = TRUE, outlierDefX = 4, outlierDefY = 2.5, quiet = TRUE)
outliers <- lapply(pca_plot_byPanel_outliers, function(x){x$data}) %>%
  bind_rows() %>%
  filter(Outlier == 1)


test_that("olink_pca_plot works", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")

  # Two Warnings thrown: for dropped assays and dropped samples
  expect_warning(
    expect_warning(
      pca_plot_drop <- npx_data1 %>%
      mutate(SampleID = paste(SampleID, "_", Index, sep = "")) %>%
      olink_pca_plot(drop_assays = TRUE, drop_samples = TRUE, quiet = TRUE)
    )
  )

  expect_equal(outliers$SampleID, c("B4_83", "A14_15", "A15_16", "A19_21"))
  expect_equal(outliers$Panel, c("Cardiometabolic", "Inflammation", "Inflammation", "Inflammation"))

  # data with all NPX=NA for some assays
  expect_warning(
    olink_pca_plot(npx_data_format221010, quiet = TRUE),
    "have NPX = NA for all samples")
  expect_warning(
    olink_pca_plot(npx_data_format221121, quiet = TRUE),
    "have NPX = NA for all samples")
  expect_warning(
    olink_pca_plot(npx_data_extended_format221121, quiet = TRUE),
    "have NPX = NA for all samples")

  pca_plot_name <- "PCA plot"
  check_snap_exist(test_dir_name = "pca_plot", snap_name = pca_plot_name)
  vdiffr::expect_doppelganger(pca_plot_name, pca_plot[[1]])

  pca_plot_treatCol_name <- "PCA plot color by treatment"
  check_snap_exist(test_dir_name = "pca_plot", snap_name = pca_plot_treatCol_name)
  vdiffr::expect_doppelganger(pca_plot_treatCol_name, pca_plot_treatCol[[1]])

  pca_plot_treatCol_topLoadings_name <- "PCA plot with loadings"
  check_snap_exist(test_dir_name = "pca_plot", snap_name = pca_plot_treatCol_topLoadings_name)
  vdiffr::expect_doppelganger(pca_plot_treatCol_topLoadings_name, pca_plot_treatCol_topLoadings[[1]])

  pca_plot_drop_name <- "PCA plot drop_assays and drop_samples"
  check_snap_exist(test_dir_name = "pca_plot", snap_name = pca_plot_drop_name)
  vdiffr::expect_doppelganger(pca_plot_drop_name, pca_plot_drop[[1]])

  pca_plot_byPanel_name1 <- "PCA plot panel 1"
  check_snap_exist(test_dir_name = "pca_plot", snap_name = pca_plot_byPanel_name1)
  vdiffr::expect_doppelganger(pca_plot_byPanel_name1, pca_plot_byPanel[[1]])

  pca_plot_byPanel_name2 <- "PCA plot panel 2"
  check_snap_exist(test_dir_name = "pca_plot", snap_name = pca_plot_byPanel_name2)
  vdiffr::expect_doppelganger(pca_plot_byPanel_name2, pca_plot_byPanel[[2]])
})


# PCA plot internal -------------------------------------------------------

test_that("PCA plot internal", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")

  pca_p2 <- npx_data1 %>%
    mutate(SampleID = paste(SampleID, "_", Index, sep = "")) %>%
    olink_pca_plot.internal(outlierDefX = NA,
                            outlierDefY = NA,
                            label_outliers = FALSE,
                            outlierLines = FALSE)

  pca_p2_name <- "PCA plot internal"
  check_snap_exist(test_dir_name = "pca_plot", snap_name = pca_p2_name)
  vdiffr::expect_doppelganger(pca_p2_name, pca_p2)


  pca_p3 <- npx_data1 %>%
    mutate(SampleID = paste(SampleID, "_", Index, sep = "")) %>%
    olink_pca_plot.internal(outlierDefX = NA,
                            outlierDefY = NA,
                            label_outliers = TRUE,
                            outlierLines = FALSE)

  pca_p3_name <- "PCA plot internal 2"
  check_snap_exist(test_dir_name = "pca_plot", snap_name = pca_p3_name)
  vdiffr::expect_doppelganger(pca_p3_name, pca_p3)


  pca_p4 <- list(npx_data1 %>%
    mutate(SampleID = paste(SampleID, "_", Index, sep = "")) %>%
    olink_pca_plot.internal(outlierDefX = NA,
                            outlierDefY = NA,
                            label_outliers = TRUE,
                            outlierLines = FALSE))

  pca_p4_name <- "PCA plot internal 3"
  check_snap_exist(test_dir_name = "pca_plot", snap_name = pca_p4_name)
  vdiffr::expect_doppelganger(pca_p4_name, pca_p4[[1]])


  pca_p5 <- npx_data1 %>%
    dplyr::filter(stringr::str_detect(OlinkID, "OID[0-9]{5}")) %>%
    mutate(SampleID = paste(SampleID, "_", Index, sep = "")) %>%
    olink_pca_plot.internal(outlierDefX = NA,
                            outlierDefY = NA,
                            label_outliers = TRUE,
                            outlierLines = FALSE)

  pca_p5_name <- "PCA plot internal 4"
  check_snap_exist(test_dir_name = "pca_plot", snap_name = pca_p5_name)
  vdiffr::expect_doppelganger(pca_p5_name, pca_p5)

})

# PCA calculation ---------------------------------------------------------

## Order of output

### With set locale -----

old_collate <- Sys.getlocale("LC_COLLATE")
Sys.setlocale("LC_COLLATE", "C")

locale_outside <- Sys.getlocale(category = "LC_ALL")

pca_outside <- npx_data1 %>%
  mutate(SampleID = paste(SampleID, "_", Index, sep = "")) %>%
  npxProcessing_forDimRed() %>%
  olink_calculate_pca()

locale_outside <- Sys.getlocale (category = "LC_ALL")

Sys.setlocale("LC_COLLATE", old_collate)

test_that("PCA calculation - output order 2", {
  pca <- npx_data1 %>%
    mutate(SampleID = paste(SampleID, "_", Index, sep = "")) %>%
    npxProcessing_forDimRed() %>%
    olink_calculate_pca()

  locale_inside <- Sys.getlocale (category = "LC_ALL")

  expect_equal(locale_outside, locale_inside)
  expect_equal(rownames(pca$scores), rownames(pca_outside$scores))

  expect_snapshot_value(pca$scores, style = "deparse")
  expect_snapshot_value(pca$loadings, style = "deparse")
})

### Without set locale -----

pca_outside <- npx_data1 %>%
  mutate(SampleID = paste(SampleID, "_", Index, sep = "")) %>%
  npxProcessing_forDimRed() %>%
  olink_calculate_pca()

test_that("PCA calculation - output order 2", {
  pca <- npx_data1 %>%
    mutate(SampleID = paste(SampleID, "_", Index, sep = "")) %>%
    npxProcessing_forDimRed() %>%
    olink_calculate_pca()

  expect_equal(rownames(pca$scores), rownames(pca_outside$scores))
})


pca_outside <- npx_data1 %>%
  mutate(SampleID = paste(SampleID, "_", Index, sep = "")) %>%
  npxProcessing_forDimRed() %>%
  olink_calculate_pca()

test_that("PCA calculation - output values", {
  pca <- npx_data1 %>%
    mutate(SampleID = paste(SampleID, "_", Index, sep = "")) %>%
    npxProcessing_forDimRed() %>%
    olink_calculate_pca()

  expect_equal(pca$loadings_scaling_factor, pca_outside$loadings_scaling_factor)
  expect_equal(pca$loadings, pca_outside$loadings)
  expect_equal(pca$PoV, pca_outside$PoV)
  expect_equal(pca$scores, pca_outside$scores)

})


test_that("PCA basic plotting", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")

  pca_input <- npx_data1 %>%
    mutate(SampleID = paste(SampleID, "_", Index, sep = "")) %>%
    npxProcessing_forDimRed()
  pca <- pca_input %>%
    olink_calculate_pca()

  pca_p1 <- ggplot(pca$scores, aes(x = PCX, y = PCY)) +
    geom_point()

  pca_p1_name <- "PCA basic plotting"
  check_snap_exist(test_dir_name = "pca_plot", snap_name = pca_p1_name)
  vdiffr::expect_doppelganger(pca_p1_name, pca_p1)
})



# PCA plot function -------------------------------------------------------

test_that("minimal PCA plot", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")

  pca_plot <- npx_data1 %>%
    mutate(SampleID = paste(SampleID, "_", Index, sep = "")) %>%
    olink_pca_plot(quiet = TRUE,
                   label_outliers = FALSE)
  pca_plot_name <- "PCA plot - not label outliers"
  check_snap_exist(test_dir_name = "pca_plot", snap_name = pca_plot_name)
  vdiffr::expect_doppelganger(pca_plot_name, pca_plot[[1]])

  pca_plot_outliers <- npx_data1 %>%
    mutate(SampleID = paste(SampleID, "_", Index, sep = "")) %>%
    olink_pca_plot(quiet = TRUE, label_outliers = TRUE)
  pca_plot_outliers_name <- "PCA plot - label outliers"
  check_snap_exist(test_dir_name = "pca_plot", snap_name = pca_plot_outliers_name)
  vdiffr::expect_doppelganger(pca_plot_outliers_name, pca_plot_outliers[[1]])

  #Removing Index dependence in PCA plot
  pca_rem_index <- npx_data1 %>%
    mutate(SampleID = paste(SampleID, "_", Index, sep = ""),
           Index=if_else(Panel == "Olink Cardiometabolic", Index+1L, Index)) %>%
    olink_pca_plot(quiet = TRUE)

  expect_true(all(abs(sort(pca_rem_index[[1]]$data$PCX) -
                        sort(pca_plot[[1]]$data$PCX)) == 0))
  expect_true(all(abs(sort(pca_rem_index[[1]]$data$PCY) -
                        sort(pca_plot[[1]]$data$PCY)) == 0))
})


# prcomp ------------------------------------------------------------------

C <- chol(S <- toeplitz(.9 ^ (0:31))) # Cov.matrix and its root
set.seed(17)
X <- matrix(rnorm(32000), 1000, 32)
Z <- X %*% C  ## ==>  cov(Z) ~=  C'C = S

pZ_outside <- prcomp(Z, tol = 0.1)

test_that("prcomp", {
  pZ_inside <- prcomp(Z, tol = 0.1)

  expect_equal(pZ_outside, pZ_inside)

})


# locale ------------------------------------------------------------------

old_collate <- Sys.getlocale("LC_COLLATE")
Sys.setlocale("LC_COLLATE", "C")

locale_outside <- Sys.getlocale(category = "LC_ALL")

Sys.setlocale("LC_COLLATE", old_collate)

#----

test_that("PCA calculation", {

  locale_inside <- Sys.getlocale (category = "LC_ALL")

  expect_equal(locale_outside, locale_inside)

})
