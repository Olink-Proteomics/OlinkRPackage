# mapping.R ----

eHT_e3072_mapping_file <- system.file("data-raw",
                                      "mapping.R",
                                      package = "OlinkAnalyze",
                                      mustWork = TRUE)
source(eHT_e3072_mapping_file)
rm(eHT_e3072_mapping_file)

# read in normalization utilities ----

normalization_utilities_file <- system.file("data-raw",
                                            "normalization_utils.R",
                                            package = "OlinkAnalyze",
                                            mustWork = TRUE)
source(normalization_utilities_file)
rm(normalization_utilities_file)

# save to R/sysdata.rda ----

usethis::use_data(eHT_e3072_mapping,
                  olink_norm_mode_combos,
                  olink_norm_ref_median_cols,
                  olink_norm_modes,
                  overwrite = TRUE,
                  internal = TRUE,
                  compress = "xz",
                  version = 2L)
