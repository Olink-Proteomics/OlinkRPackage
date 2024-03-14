# olink_platforms.R ----

olink_platforms_file <- system.file("data-raw",
                                    "olink_platforms.R",
                                    package = "OlinkAnalyze",
                                    mustWork = TRUE)
source(olink_platforms_file)
rm(olink_platforms_file)

# olink_wide_top_mat.R ----

olink_wide_top_mat_file <- system.file("data-raw",
                                       "olink_wide_top_mat.R",
                                       package = "OlinkAnalyze",
                                       mustWork = TRUE)
source(olink_wide_top_mat_file)
rm(olink_wide_top_mat_file)

# parquet_spec.R ----

parquet_spec_file <- system.file("data-raw",
                                 "parquet_spec.R",
                                 package = "OlinkAnalyze",
                                 mustWork = TRUE)
source(parquet_spec_file)
rm(parquet_spec_file)

# utils_spec.R ----

utils_spec_file <- system.file("data-raw",
                               "utils_spec.R",
                               package = "OlinkAnalyze",
                               mustWork = TRUE)
source(utils_spec_file)
rm(utils_spec_file)

# others ----

olink_wide_bottom_matrix <-
  readRDS("inst/extdata/npx_signature_bottom_mat_v1_long.rds")

olink_wide_rename_npxs <-
  readRDS("inst/extdata/olink_wide_rename_npxs.rds")

# save to R/syssata.rda ----

usethis::use_data(accepted_olink_platforms,
                  olink_wide_spec,
                  olink_parquet_spec,
                  accepted_checksum_files,
                  accepted_field_sep,
                  accepted_npx_file_ext,
                  read_npx_df_output,
                  olink_wide_bottom_matrix,
                  olink_wide_rename_npxs,
                  overwrite = TRUE,
                  internal = TRUE,
                  compress = "gzip",
                  version = 2L)
