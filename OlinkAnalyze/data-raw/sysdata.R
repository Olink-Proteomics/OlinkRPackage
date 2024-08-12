# mapping.R ----

e3k_eHT_mapping_file <- system.file("data-raw",
                                    "mapping.R",
                                    package = "OlinkAnalyze",
                                    mustWork = TRUE)
source(e3k_eHT_mapping_file)
rm(e3k_eHT_mapping_file)

source(system.file("data-raw",
            "example_HT_data.R",
            package = "OlinkAnalyze",
            mustWork = TRUE))
source(system.file("data-raw",
                   "example_3k_data.R",
                   package = "OlinkAnalyze",
                   mustWork = TRUE))

# save to R/R/sysdata.rda ----

usethis::use_data(e3k_eHT_mapping,
                  overwrite = TRUE,
                  internal = TRUE,
                  compress = "xz",
                  version = 2L)
usethis::use_data(data_ht,
                  overwrite = TRUE,
                  internal = TRUE,
                  compress = "xz",
                  version = 2L)

usethis::use_data(data_3k,
                  overwrite = TRUE,
                  internal = TRUE,
                  compress = "xz",
                  version = 2L)
