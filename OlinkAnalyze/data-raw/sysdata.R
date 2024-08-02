# mapping.R ----

e3k_eHT_mapping_file <- system.file("data-raw",
                                    "mapping.R",
                                    package = "OlinkAnalyze",
                                    mustWork = TRUE)
source(e3k_eHT_mapping_file)
rm(e3k_eHT_mapping_file)

# save to R/R/sysdata.rda ----

usethis::use_data(e3k_eHT_mapping,
                  overwrite = TRUE,
                  internal = TRUE,
                  compress = "xz",
                  version = 2L)
