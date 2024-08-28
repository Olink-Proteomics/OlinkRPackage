# mapping.R ----

eHT_e3072_mapping_file <- system.file("data-raw",
                                      "mapping.R",
                                      package = "OlinkAnalyze",
                                      mustWork = TRUE)
source(eHT_e3072_mapping_file)
rm(eHT_e3072_mapping_file)

# save to R/R/sysdata.rda ----

usethis::use_data(eHT_e3072_mapping,
                  overwrite = TRUE,
                  internal = TRUE,
                  compress = "xz",
                  version = 2L)
