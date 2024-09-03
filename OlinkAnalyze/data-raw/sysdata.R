# mapping.R ----

e3k_eHT_mapping_file <- system.file("data-raw", # nolint
                                    "mapping.R",
                                    package = "OlinkAnalyze",
                                    mustWork = TRUE)
source(e3k_eHT_mapping_file)
rm(e3k_eHT_mapping_file)

# example HT data ----

data_ht_file <- system.file("data-raw",
                            "example_HT_data.R",
                            package = "OlinkAnalyze",
                            mustWork = TRUE)
source(data_ht_file)
rm(data_ht_file)

# example 3k data ----

data_3k_file <- system.file("data-raw",
                            "example_3k_data.R",
                            package = "OlinkAnalyze",
                            mustWork = TRUE)
source(data_3k_file)
rm(data_3k_file)

# save to R/R/sysdata.rda ----

usethis::use_data(eHT_e3072_mapping,
                  data_ht,
                  data_3k,
                  overwrite = TRUE,
                  internal = TRUE,
                  compress = "xz",
                  version = 2L)
