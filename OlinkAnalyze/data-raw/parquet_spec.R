## code to prepare internal dataset goes here
## based on https://r-pkgs.org/data.html#sec-data-sysdata

## Specifications for Olink parquet files ----

olink_parquet_spec <- list(
  parquet_metadata = c(
    file_version = "FileVersion",
    project_name = "ProjectName",
    sample_matrix = "SampleMatrix",
    product = "Product",
    data_file_type = "DataFileType"
  ),
  optional_metadata = c(
    "RUO"
  ),
  parquet_platforms = c(
    "ExploreHT",
    "Explore3072",
    "Reveal"
  ),
  parquet_files = c(
    "NPX File",
    "Extended NPX File",
    "CLI Data Export File",
    "Internal CLI Data Export File",
    "R Package Export File"
  )
)
