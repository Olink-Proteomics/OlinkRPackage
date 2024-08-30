# code to prepare `mapping` dataset goes here
# Raw data is generated here:

e3k_eHT_mapping_rds <- system.file("extdata", # nolint
                                   "OlinkIDMapping.rds",
                                   package = "OlinkAnalyze",
                                   mustWork = TRUE)
e3k_eHT_mapping <- readRDS(file = e3k_eHT_mapping_rds) # nolint
rm(e3k_eHT_mapping_rds)
