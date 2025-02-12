# mapping.R ----

eHT_e3072_mapping_file <- system.file("data-raw",
                                      "mapping.R",
                                      package = "OlinkAnalyze",
                                      mustWork = TRUE)
source(eHT_e3072_mapping_file)
rm(eHT_e3072_mapping_file)

# example_HT_data_small.R ----

data_ht_small_file <- system.file("data-raw",
                                  "example_HT_data_small.R",
                                  package = "OlinkAnalyze",
                                  mustWork = TRUE)
source(data_ht_small_file)
rm(data_ht_small_file)

# example_3k_data_small.R ----

data_3k_small_file <- system.file("data-raw",
                            "example_3k_data_small.R",
                            package = "OlinkAnalyze",
                            mustWork = TRUE)
source(data_3k_small_file)
rm(data_3k_small_file)

# read in normalization utilities ----

normalization_utilities_file <- system.file("data-raw",
                                            "normalization_utils.R",
                                            package = "OlinkAnalyze",
                                            mustWork = TRUE)
source(normalization_utilities_file)
rm(normalization_utilities_file)

# save to R/sysdata.rda ----

usethis::use_data(eHT_e3072_mapping,
                  reveal_e3072_mapping,
                  data_ht_small,
                  data_3k_small,
                  olink_norm_mode_combos,
                  olink_norm_ref_median_cols,
                  olink_norm_modes,
                  overwrite = TRUE,
                  internal = TRUE,
                  compress = "xz",
                  version = 2L)
