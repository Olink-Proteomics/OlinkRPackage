## code to prepare `mapping` dataset goes here

# See Data_Science/Internal Projects/3k_to_HT_bridging/R code/Bridgeable/mapping_file.R for generating from raw data

load("inst/extdata/OlinkIDMapping.rds")

usethis::use_data(mapping, overwrite = TRUE)

