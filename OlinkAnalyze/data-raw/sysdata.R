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

# olink_wide_rename_npxs.R ----

olink_wide_rename_npxs_file <- system.file("data-raw",
                                           "olink_wide_rename_npxs.R",
                                           package = "OlinkAnalyze",
                                           mustWork = TRUE)
source(olink_wide_rename_npxs_file)
rm(olink_wide_rename_npxs_file)

# utils_spec.R ----

utils_spec_file <- system.file("data-raw",
                               "utils_spec.R",
                               package = "OlinkAnalyze",
                               mustWork = TRUE)
source(utils_spec_file)
rm(utils_spec_file)

# olink_wide_bottom_mat_spec.R ----

olink_wide_bottom_mat_spec_fil <- system.file("data-raw",
                                              "olink_wide_bottom_mat_spec.R",
                                              package = "OlinkAnalyze",
                                              mustWork = TRUE)
source(olink_wide_bottom_mat_spec_fil)
rm(olink_wide_bottom_mat_spec_fil)

# column_name_dict.R ----

column_name_dict_file <- system.file("data-raw",
                                     "column_name_dict.R",
                                     package = "OlinkAnalyze",
                                     mustWork = TRUE)
source(column_name_dict_file)
rm(column_name_dict_file)

# normalization ----

## mapping.R ----

olinkid_mapping_file <- system.file("data-raw",
                                    "norm_olinkid_mapping.R",
                                    package = "OlinkAnalyze",
                                    mustWork = TRUE)
source(olinkid_mapping_file)
rm(olinkid_mapping_file)

## example_HT_data_small.R ----

data_ht_small_file <- system.file("data-raw",
                                  "example_HT_data_small.R",
                                  package = "OlinkAnalyze",
                                  mustWork = TRUE)
source(data_ht_small_file)
rm(data_ht_small_file)

## example_3k_data_small.R ----

data_3k_small_file <- system.file("data-raw",
                                  "example_3k_data_small.R",
                                  package = "OlinkAnalyze",
                                  mustWork = TRUE)
source(data_3k_small_file)
rm(data_3k_small_file)

## read in normalization utilities ----

normalization_utilities_file <- system.file("data-raw",
                                            "normalization_utils.R",
                                            package = "OlinkAnalyze",
                                            mustWork = TRUE)
source(normalization_utilities_file)
rm(normalization_utilities_file)

# save to R/syssata.rda ----

usethis::use_data(accepted_olink_platforms,
                  olink_wide_spec,
                  olink_parquet_spec,
                  olink_wide_rename_npxs,
                  accepted_checksum_files,
                  accepted_field_sep,
                  accepted_npx_file_ext,
                  read_npx_df_output,
                  olink_wide_bottom_matrix,
                  column_name_dict,
                  olink_sample_types,
                  olink_assay_types,
                  check_npx_lst_names,
                  eHT_e3072_mapping,
                  reveal_e3072_mapping,
                  reveal_eht_mapping,
                  data_ht_small,
                  data_3k_small,
                  olink_norm_mode_combos,
                  olink_norm_ref_median_cols,
                  olink_norm_modes,
                  olink_norm_recalc,
                  overwrite = TRUE,
                  internal = TRUE,
                  compress = "xz",
                  version = 2L)

rm(accepted_olink_platforms,
   olink_wide_spec,
   olink_parquet_spec,
   olink_wide_rename_npxs,
   accepted_checksum_files,
   accepted_field_sep,
   accepted_npx_file_ext,
   read_npx_df_output,
   olink_wide_bottom_matrix,
   column_name_dict,
   olink_sample_types,
   olink_assay_types,
   check_npx_lst_names,
   eHT_e3072_mapping,
   reveal_e3072_mapping,
   reveal_eht_mapping,
   data_ht_small,
   data_3k_small,
   olink_norm_mode_combos,
   olink_norm_ref_median_cols,
   olink_norm_modes,
   olink_norm_recalc)
