testthat::skip_if_not_installed(pkg = "dplyr")
testthat::skip_if_not_installed(pkg = "tibble")
testthat::skip_if_not_installed(pkg = "ggrepel")

set.seed(123)  # reproducible randomization

osi_data <- dplyr::tibble(
  OSITimeToCentrifugation = c(
    0.3012289, 0.060720572, 0.94772694, 0.720596273, 0.142294296,
    0.549284656, 0.954091239, 0.585483353, 0.404510282, 0.647893479,
    0.319820617, 0.307720011, 0.219767631, 0.369488866, 0.984219203,
    0.154202301, 0.091044, 0.141906908, 0.690007102, 0.619256483,
    0.891394117, 0.672999093, 0.737077738, 0.521135726, 0.65983845,
    NA_real_, 0.786281552, 0.979821917, 0.439431536, 0.311702202,
    0.409474953, 0.010467112, 0.183849524, 0.842729319, 0.231161782,
    0.239099956, 0.076691165, 0.245723678, 0.732135206, 0.847453165,
    0.497527267, 0.38790903, 0.246448994, 0.111096461, 0.389994435,
    0.571935314, 0.216892763, 0.444768002, 0.217990669, 0.502299563,
    0.353904572, 0.649985159, 0.374713957, 0.355445381, 0.533687945,
    0.74033436, 0.221102938, 0.412746119, 0.265686687, 0.629973053,
    0.183828491, 0.863644111, 0.746568004, 0.66828465, 0.618017873,
    0.37223806, 0.529835686, 0.874682343, 0.5817501, 0.839767765,
    0.312448165, 0.708290322, 0.265017806, 0.594343194, 0.4812898,
    0.265032731, 0.564590435, 0.913188223, 0.901874389, 0.274166622,
    0.321482756, 0.985640884, 0.61999331, 0.937314089, 0.466532702,
    0.406832593, 0.659230324, 0.152346617, 0.572867058, 0.238726027,
    0.962358936, 0.601365726, 0.515029727, 0.402573342, 0.880246541,
    0.364091865, 0.288239281, 0.170645235, 0.172171746, 0.482042606
  ),

  OSIPreparationTemperature = c(
    0.252964929, 0.21625479, 0.674376388, 0.047663627, 0.700853087,
    0.351888638, 0.408943998, 0.820951324, 0.918857348, 0.28252833,
    0.961104794, 0.728394428, 0.686375082, 0.052843943, 0.395220135,
    0.47784538, 0.560253264, 0.698261595, 0.915683538, 0.618351227,
    0.428421509, 0.542080367, 0.058478489, 0.260856857, 0.397151953,
    0.197744737, 0.831927563, 0.152887223, 0.803418542, 0.546826157,
    0.662317642, 0.171698494, 0.63305536, 0.311869747, 0.724554346,
    0.398939825, 0.969356411, 0.967398371, 0.726702539, 0.257216746,
    NA_real_, 0.593045652, 0.267521432, 0.531070399, 0.785291671,
    0.168060811, 0.404399181, 0.471576278, 0.868106807, 0.925707956,
    0.881977559, 0.674186843, 0.950166979, 0.516444894, 0.576519021,
    0.336331206, 0.347324631, 0.020024301, 0.502813046, 0.871043414,
    0.006300784, 0.072057124, 0.164211225, 0.770334074, 0.735184306,
    0.971875636, 0.466472377, 0.074384513, 0.648818124, 0.75859317,
    0.137106081, 0.396584595, 0.224985329, 0.057958561, 0.395892688,
    0.0649283, 0.225886433, 0.054629109, 0.67028204, 0.297741783,
    0.100721582, 0.071904097, 0.880440569, 0.754247402, 0.816605888,
    0.982140374, 0.103599645, 0.099041829, 0.798831611, 0.784575267,
    0.009429905, 0.779065883, 0.729390652, 0.630131853, 0.48091083,
    0.156636851, 0.00821552, 0.452458394, 0.492293329, 0.389587112
  ),

  OSISummary = c(
    0.354783098, 0.802812273, 0.835708837, 0.237749405, 0.353986103,
    0.85688542, 0.853763374, 0.295895455, 0.147048325, 0.703992061,
    0.103806688, 0.033727773, 0.999404528, 0.034874804, 0.338391284,
    0.915063762, 0.61723527, 0.286285349, 0.737797403, 0.834054309,
    0.31427078, 0.492566548, 0.697373766, 0.641462354, 0.643922915,
    NA_real_, 0.414735334, 0.11940478, 0.52602966, 0.225073351,
    0.486411764, 0.370214798, 0.983350181, 0.388319115, 0.22924484,
    0.623297546, 0.136540197, 0.967469494, 0.515071808, 0.163070329,
    NA_real_, 0.985954165, 0.668771517, 0.418915897, 0.323344993,
    0.83525532, 0.143817044, 0.192815947, 0.896738683, 0.308119554,
    0.363300544, 0.783946479, 0.19337868, 0.017765812, 0.406607866,
    0.483167669, 0.42184495, 0.342808802, 0.866483315, 0.455108051,
    0.533764874, 0.963843332, 0.774591542, 0.208876349, 0.308786833,
    0.97134245, 0.584900093, 0.760823625, 0.372709394, 0.769193911,
    0.537677183, 0.91399545, 0.185296442, 0.282218417, 0.094962413,
    0.210487079, 0.977098996, 0.296302175, 0.725983027, 0.785687834,
    0.105417746, 0.239594629, 0.270544872, 0.101058494, 0.117913841,
    0.991236556, 0.986054297, 0.137067471, 0.905309582, 0.576301838,
    0.395448859, 0.449802484, 0.706501901, 0.082502746, 0.33931258,
    0.680787551, 0.316949248, 0.831568598, 0.215172083, 0.497948936
  ),

  OSICategory = c(
    2, 4, 4, 1, 2, 4, 4, 2, 1, 3,
    1, 1, 4, 1, 2, 4, 3, 2, 3, 4,
    2, 2, 3, 3, 3, 0, 2, 1, 3, 1,
    2, 2, 4, 2, 1, 3, 1, 4, 3, 1,
    0, 4, 3, 2, 2, 4, 1, 1, 4, 2,
    2, 4, 1, 1, 2, 2, 2, 2, 4, 2,
    3, 4, 4, 1, 2, 4, 3, 4, 2, 4,
    3, 4, 1, 2, 1, 1, 4, 2, 3, 4,
    1, 1, 2, 1, 1, 4, 4, 1, 4, 3,
    2, 2, 3, 1, 2, 3, 2, 4, 1, 2
  )
)

new_ids <- c(paste0("A", 1:77), paste0("B", 1:23))
stopifnot(nrow(osi_data) == length(new_ids))

osi_data <- osi_data |>
  dplyr::mutate(SampleID = sample(new_ids, size = dplyr::n(), replace = FALSE))

data1 <- OlinkAnalyze::npx_data1 |>
  dplyr::right_join(
    osi_data |>
      dplyr::select(SampleID,
                    OSITimeToCentrifugation,
                    OSIPreparationTemperature,
                    OSISummary,
                    OSICategory),
    by = "SampleID"
  ) |>
  dplyr::filter(!grepl("CONTROL", SampleID))

test_that("OSI errors: using npx_data1 and OSI columns", {

  # ----------------------------
  # OSICategory invalid value
  # ----------------------------
  df_bad_cat <- data1 |>
    dplyr::mutate(OSICategory = ifelse(dplyr::row_number() == 1, "7",
                                       as.character(OSICategory)))

  # No error when not using OSI column
  testthat::expect_no_error(
    testthat::expect_message(
      olink_qc_plot(df_bad_cat, color_g = "QC_Warning"),
      regexp = "`check_log` not provided. Running `check_npx()`.",
      fixed = TRUE
    )
  )

  testthat::expect_error(
    testthat::expect_message(olink_qc_plot(df_bad_cat, color_g = "OSICategory"),
      regexp = paste0("`check_log` not provided. ",
                      "Running `check_npx()`."),
      fixed = TRUE
    ),
    regexp = paste0('Invalid values detected in OSICategory\\.', #nolint quotes_linter
                    ' Expected only 0, 1, 2, 3, or 4\\. Found: "7".') #nolint quotes_linter
  )

  # ----------------------------
  # OSICategory all NA
  # ----------------------------
  df_cat_all_na <- data1 |>
    dplyr::mutate(OSICategory = NA)

  testthat::expect_error(
    testthat::expect_message(olink_qc_plot(df_cat_all_na,
                                           color_g = "OSICategory"),
                             regexp = paste0("`check_log` not provided. ",
                                             "Running `check_npx()`."),
                             fixed = TRUE),
    regexp = paste0("All values are NA in OSICategory\\. ",
                    "Please check your data to confirm OSI data is present\\.")
  )

  # ------------------------------------------
  # Continuous OSI column non-numeric value
  # ------------------------------------------
  df_bad_cont_nonnum <- data1 |>
    dplyr::mutate(OSITimeToCentrifugation = ifelse(
      dplyr::row_number() == 1,
      "oops",
      as.character(OSITimeToCentrifugation)
    )
    )

  testthat::expect_error(
    testthat::expect_message(olink_qc_plot(df_bad_cont_nonnum, color_g = "OSITimeToCentrifugation"), #nolint line_length_linter
                             regexp = paste0("`check_log` not provided. ",
                                             "Running `check_npx()`."),
                             fixed = TRUE),
    regexp = paste0(
      'Invalid values detected in OSITimeToCentrifugation\\.', #nolint quotes_linter
      ' Expected continuous numeric values between 0 and 1\\.', #nolint quotes_linter
      ' Found non-numeric value\\(s\\): "oops".' #nolint quotes_linter
    )
  )

  # ------------------------------------
  # Continuous OSI column out of range
  # ------------------------------------
  df_bad_cont_oor <- data1 |>
    dplyr::mutate(OSIPreparationTemperature = ifelse(dplyr::row_number() == 1, 1.2, OSIPreparationTemperature)) #nolint line_length_linter

  testthat::expect_error(
    testthat::expect_message(olink_qc_plot(df_bad_cont_oor, color_g = "OSIPreparationTemperature"), #nolint line_length_linter
                             regexp = paste0("`check_log` not provided. ",
                                             "Running `check_npx()`."),
                             fixed = TRUE),
    regexp = paste0(
      "Invalid values detected in OSIPreparationTemperature\\.",
      " Expected continuous numeric values between 0 and 1\\.",
      " Found out-of-range value\\(s\\): 1\\.2"
    )
  )

  # ----------------------------
  # Continuous OSI column all NA
  # ----------------------------
  df_cont_all_na <- data1 |>
    dplyr::mutate(OSISummary = NA)

  testthat::expect_error(
    testthat::expect_message(olink_qc_plot(df_cont_all_na, color_g = "OSISummary"), #nolint line_length_linter
                             regexp = paste0("`check_log` not provided. ",
                                             "Running `check_npx()`."),
                             fixed = TRUE),
    regexp = paste0("All values are NA in OSISummary\\.",
                    " Please check your data to confirm OSI data is present\\.")
  )

  # ------------------------------------------------------
  # Valid OSI values should NOT trigger OSI error strings
  # ------------------------------------------------------
  testthat::expect_no_error(
    testthat::expect_message(olink_qc_plot(data1, color_g = "OSICategory"),
                             regexp = paste0("`check_log` not provided. ",
                                             "Running `check_npx()`."),
                             fixed = TRUE)
  )

  testthat::expect_no_error(
    testthat::expect_message(olink_qc_plot(data1, color_g = "OSISummary"),
                             regexp = paste0("`check_log` not provided. ",
                                             "Running `check_npx()`."),
                             fixed = TRUE)
  )

  testthat::expect_no_error(
    testthat::expect_message(olink_qc_plot(data1, color_g = "OSIPreparationTemperature"), #nolint line_length_linter
                             regexp = paste0("`check_log` not provided. ",
                                             "Running `check_npx()`."),
                             fixed = TRUE)
  )

  testthat::expect_no_error(
    testthat::expect_message(olink_qc_plot(data1, color_g = "OSITimeToCentrifugation"), #nolint line_length_linter
                             regexp = paste0("`check_log` not provided. ",
                                             "Running `check_npx()`."),
                             fixed = TRUE)
  )

})

# -------------------------------------------------------

#Load data with hidden/excluded assays (all NPX=NA)
load(file = test_path("data", "npx_data_format221010.RData"))
load(file = test_path("data", "npx_data_format221121.RData"))


qc_plot <- npx_data1 |>
  dplyr::mutate(SampleID = paste(SampleID, "_", Index, sep = "")) |>
  olink_qc_plot(label_outliers = FALSE) |>
  suppressMessages()

qc_plot2 <- npx_data1 |>
  dplyr::mutate(SampleID = paste(SampleID, "_", Index, sep = "")) |>
  olink_qc_plot(coloroption =  c("teal", "pink"), label_outliers = FALSE) |>
  suppressMessages()

test_that("olink_qc_plot works", {

  testthat::expect_message(
    testthat::expect_message(
      testthat::expect_warning(olink_qc_plot(npx_data_format221010),
                               regex = paste0('"OID31309", and "OID31325" have', #nolint quotes_linter
                                              ' "NPX" = NA for all samples.') #nolint quotes_linter
      ),
      regexp = "`check_log` not provided. Running `check_npx()`.",
      fixed = TRUE
    ),
    regexp = paste0("8 assays exhibited assay QC warnings in column ",
                    "`Assay_Warning` of the dataset"),
    fixed = TRUE
  ) # data with all NPX=NA for some assays

  testthat::expect_message(
    testthat::expect_message(
      testthat::expect_warning(olink_qc_plot(npx_data_format221121),
                               regex = paste0('"OID21069", and "OID21125" have', #nolint quotes_linter
                                              ' "NPX" = NA for all samples.') #nolint quotes_linter
      ),
      regexp = "`check_log` not provided. Running `check_npx()`.",
      fixed = TRUE
    ),
    regexp = paste0("326 assays exhibited assay QC warnings in column ",
                    "`Assay_Warning` of the dataset"),
    fixed = TRUE
  ) # data with all NPX=NA for some assays

  testthat::expect_message(
    testthat::expect_message(
      testthat::expect_warning(olink_qc_plot(npx_data_extended_format221121),
                               regex = paste0('"OID21069", and "OID21125" have', #nolint quotes_linter
                                              ' "NPX" = NA for all samples.') #nolint quotes_linter
      ),
      regexp = "`check_log` not provided. Running `check_npx()`.",
      fixed = TRUE
    ),
    regexp = paste0("326 assays exhibited assay QC warnings in column ",
                    "`Assay_Warning` of the dataset"),
    fixed = TRUE
  ) # data with all NPX=NA for some assays

})

test_that("olink_qc_plot works - vdiffr", {

  skip_on_cran()
  skip_if_not_installed("vdiffr")

  qc_plot_name <- "QC plot"
  check_snap_exist(test_dir_name = "olink_qc_plot", snap_name = qc_plot_name)
  vdiffr::expect_doppelganger(qc_plot_name, qc_plot)

  qc_plot2_name <- "QC plot with coloroption"
  check_snap_exist(test_dir_name = "olink_qc_plot", snap_name = qc_plot2_name)
  vdiffr::expect_doppelganger(qc_plot2_name, qc_plot2)
})
