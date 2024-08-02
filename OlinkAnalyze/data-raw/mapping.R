# code to prepare `mapping` dataset goes here
# Raw data is generated here:
# Data_Science/Internal Projects/3k_to_HT_bridging/R code/Bridgeable/mapping_file.R

e3k_eHT_mapping_rds <- system.file("extdata",
                                   "OlinkIDMapping.rds",
                                   package = "OlinkAnalyze",
                                   mustWork = TRUE)
e3k_eHT_mapping <- readRDS(file = e3k_eHT_mapping_rds)
rm(e3k_eHT_mapping_rds)
