# This script uses the original first version of the cross-product normalization
# for each pair of Olink products. Specifically, we will use:
# - OlinkAnalyze v 4.0.1 for Olink Explore HT - Olink Explore 3072
# - OlinkAnalyze v 4.2.0 for Olink Reveal - Olink Explore 3072
# - OlinkAnalyze v 4.4.0 for Olink Explore HT - Olink Reveal
#
# Note, that the versions above are meant to generate normalized unformatted
# data (format = FALSE). The feature for formatted normalized data
# (format = TRUE) was introduced in OlinkAnalyze v 4.5.0, and thus, the
# reference results for formatted normalized data will be generated using
# OlinkAnalyze v 4.5.0 for all pairs of Olink products. The reference results
# for formatted normalized data will be generated at the bottom of this script.
#
# Note, that when switching between versions of OA, we need to restart the R
# session to avoid conflicts between versions.

# Storage list ----

lst_norm_cp <- list(
  "QQ" = list(
    "HT_3K" = NULL,
    "Reveal_3K" = NULL,
    "HT_Reveal" = NULL,
    "Reveal_HT" = NULL
  ),
  "Reference" = list(
    "NoFormat" = list(
      "HT_3K" = NULL,
      "Reveal_3K" = NULL,
      "HT_Reveal" = NULL,
      "Reveal_HT" = NULL
    ),
    "Format" = list(
      "HT_3K" = NULL,
      "Reveal_3K" = NULL,
      "HT_Reveal" = NULL,
      "Reveal_HT" = NULL
    )
  )
)

# Set seed ----

set.seed(123)

# Data ----

## Olink Explore HT ----

dt_ht_file <- testthat::test_path("data", "example_HT_data.rds")
stopifnot(file.exists(dt_ht_file))
dt_ht <- readRDS(file = dt_ht_file)
rm(dt_ht_file)

## Olink Explore 3072 ----

dt_3k_file <- testthat::test_path("data", "example_3k_data.rds")
stopifnot(file.exists(dt_3k_file))
dt_3k <- readRDS(file = dt_3k_file)
rm(dt_3k_file)

## Olink Reveal ----

dt_reveal_file <- testthat::test_path("data", "example_Reveal_data.rds")
stopifnot(file.exists(dt_reveal_file))
dt_reveal <- readRDS(file = dt_reveal_file)
rm(dt_reveal_file)

# Cross-product normalization - No formatting ----

## Olink Explore HT - Olink Explore 3072 ----

### Install OA ----

remotes::install_version(
  package = "OlinkAnalyze",
  version = "4.0.2",
  repos = "http://cran.us.r-project.org"
)

### Bridge samples ----

bridge_sample_ht_3k <- intersect(
  x = unique(dt_ht$SampleID),
  y = unique(dt_3k$SampleID)
) |>
  (\(.) .[!grepl("CONTROL", .)])()
# 52 bridge samples

### Normalization ----

dt_norm_ht_3k <- OlinkAnalyze::olink_normalization(
  df1 = dt_ht,
  df2 = dt_3k,
  overlapping_samples_df1 = bridge_sample_ht_3k,
  df1_project_nr = "df_ht",
  df2_project_nr = "df_3k",
  reference_project = "df_ht"
)
# Cross-product normalization will be performed!
# Warning message:
# 2 assays are not shared across products.
# ℹ 2 assays will be removed from normalization.

### Generate QQ reference ----

qq_ht_3k <- dt_norm_ht_3k |>
  dplyr::filter(
    .data[["Project"]] == "df_3k" &
      # only samples that start with "Sample_A" or "Sample_" followed by a
      # single capital letter
      grepl(pattern = "^Sample_[A-Z]$|^Sample_A", x = .data[["SampleID"]])
  ) |>
  dplyr::mutate(
    OlinkID = paste(.data[["OlinkID"]], .data[["OlinkID_E3072"]], sep = "_")
  ) |>
  dplyr::select(
    dplyr::all_of(
      c("OlinkID", "QSNormalizedNPX", "SampleID")
    )
  )
# this was tested against the legacy file
# "qq_normalization_reference_result.rds" and it will be replacing it for better
# reproducibility and transparency.

### Store in list ----

lst_norm_cp$QQ$HT_3K <- qq_ht_3k
lst_norm_cp$Reference$NoFormat$HT_3K <- dt_norm_ht_3k

## Olink Reveal - Olink Explore 3072 ----

### Install OA ----

remotes::install_version(
  package = "OlinkAnalyze",
  version = "4.2.0",
  repos = "http://cran.us.r-project.org"
)

### Bridge samples ----

bridge_sample_reveal_3k <- intersect(
  x = unique(dt_reveal$SampleID),
  y = unique(dt_3k$SampleID)
) |>
  (\(.) .[!grepl("CONTROL", .)])()
# 52 bridge samples

### Normalization ----

dt_norm_reveal_3k <- OlinkAnalyze::olink_normalization(
  df1 = dt_reveal,
  df2 = dt_3k,
  overlapping_samples_df1 = bridge_sample_reveal_3k,
  df1_project_nr = "df_reveal",
  df2_project_nr = "df_3k",
  reference_project = "df_reveal"
)
# Cross-product normalization will be performed!
# Warning message:
# 85 assays are not shared across products.
# ℹ 85 assays will be removed from normalization.

### Generate QQ reference ----

qq_reveal_3k <- dt_norm_reveal_3k |>
  dplyr::filter(
    .data[["Project"]] == "df_3k" &
      # only samples that start with "Sample_A" or "Sample_" followed by a
      # single capital letter
      grepl(pattern = "^Sample_[A-Z]$|^Sample_A", x = .data[["SampleID"]])
  ) |>
  dplyr::mutate(
    OlinkID = paste(.data[["OlinkID"]], .data[["OlinkID_E3072"]], sep = "_")
  ) |>
  dplyr::select(
    dplyr::all_of(
      c("OlinkID", "QSNormalizedNPX", "SampleID")
    )
  )
# this was tested against the legacy file
# "qq_normalization_reference_result_reveal.rds" and it will be replacing it for
# better reproducibility and transparency.

### Store in list ----

lst_norm_cp$QQ$Reveal_3K <- qq_reveal_3k
lst_norm_cp$Reference$NoFormat$Reveal_3K <- dt_norm_reveal_3k

## Olink Explore HT - Olink Reveal ----

### Install OA ----

remotes::install_version(
  package = "OlinkAnalyze",
  version = "4.4.0",
  repos = "http://cran.us.r-project.org"
)

### Bridge samples ----

bridge_sample_reveal_ht <- intersect(
  x = unique(dt_reveal$SampleID),
  y = unique(dt_ht$SampleID)
) |>
  (\(.) .[!grepl("CONTROL", .)])()
# 52 bridge samples

### Normalization ----

#### Reveal as reference ----

dt_norm_reveal_ht <- OlinkAnalyze::olink_normalization(
  df1 = dt_reveal,
  df2 = dt_ht,
  overlapping_samples_df1 = bridge_sample_reveal_ht,
  df1_project_nr = "df_reveal",
  df2_project_nr = "df_ht",
  reference_project = "df_reveal"
)
# 0 assay(s) exhibited assay QC warning. For more information see the AssayQC column. # nolint: line_length_linter
# 0 assay(s) exhibited assay QC warning. For more information see the AssayQC column. # nolint: line_length_linter
# Cross-product normalization will be performed!
# Warning message:
# 80 assays are not shared across products.
# ℹ 80 assays will be removed from normalization

#### HT as reference ----

dt_norm_ht_reveal <- OlinkAnalyze::olink_normalization(
  df1 = dt_reveal,
  df2 = dt_ht,
  overlapping_samples_df1 = bridge_sample_reveal_ht,
  df1_project_nr = "df_reveal",
  df2_project_nr = "df_ht",
  reference_project = "df_ht"
)
# 0 assay(s) exhibited assay QC warning. For more information see the AssayQC column. # nolint: line_length_linter
# 0 assay(s) exhibited assay QC warning. For more information see the AssayQC column. # nolint: line_length_linter
# Cross-product normalization will be performed!
# Warning message:
# 80 assays are not shared across products.
# ℹ 80 assays will be removed from normalization

### Generate QQ reference ----

#### Reveal as reference ----

qq_reveal_ht <- dt_norm_reveal_ht |>
  dplyr::filter(
    .data[["Project"]] == "df_ht" &
      # only samples that start with "Sample_A" or "Sample_" followed by a
      # single capital letter
      grepl(pattern = "^Sample_[A-Z]$|^Sample_A", x = .data[["SampleID"]])
  ) |>
  dplyr::mutate(
    OlinkID = paste(.data[["OlinkID"]], .data[["OlinkID_HT"]], sep = "_")
  ) |>
  dplyr::select(
    dplyr::all_of(
      c("OlinkID", "QSNormalizedNPX", "SampleID")
    )
  )
# No legacy file to compare with, but this will be used as the reference for the
# QQ normalization of the Reveal - HT pair.

#### HT as reference ----

qq_ht_reveal <- dt_norm_ht_reveal |>
  dplyr::filter(
    .data[["Project"]] == "df_reveal" &
      # only samples that start with "Sample_A" or "Sample_" followed by a
      # single capital letter
      grepl(pattern = "^Sample_[A-Z]$|^Sample_A", x = .data[["SampleID"]])
  ) |>
  dplyr::mutate(
    OlinkID = paste(.data[["OlinkID"]], .data[["OlinkID_Reveal"]], sep = "_")
  ) |>
  dplyr::select(
    dplyr::all_of(
      c("OlinkID", "QSNormalizedNPX", "SampleID")
    )
  )

### Store in list ----

#### Reveal as reference ----

lst_norm_cp$QQ$Reveal_HT <- qq_reveal_ht
lst_norm_cp$Reference$NoFormat$Reveal_HT <- dt_norm_reveal_ht

#### HT as reference ----

lst_norm_cp$QQ$HT_Reveal <- qq_ht_reveal
lst_norm_cp$Reference$NoFormat$HT_Reveal <- dt_norm_ht_reveal

# Cross-product normalization - With formatting ----

### Install OA ----

remotes::install_version(
  package = "OlinkAnalyze",
  version = "4.5.0",
  repos = "http://cran.us.r-project.org"
)

## Olink Explore HT - Olink Explore 3072 ----

### Bridge samples ----

bridge_sample_ht_3k <- intersect(
  x = unique(dt_ht$SampleID),
  y = unique(dt_3k$SampleID)
) |>
  (\(.) .[!grepl("CONTROL", .)])()
# 52 bridge samples

### Normalization ----

dt_norm_ht_3k_format <- OlinkAnalyze::olink_normalization(
  df1 = dt_ht,
  df2 = dt_3k,
  overlapping_samples_df1 = bridge_sample_ht_3k,
  df1_project_nr = "df_ht",
  df2_project_nr = "df_3k",
  reference_project = "df_ht",
  format = TRUE
)
# 0 assay(s) exhibited assay QC warning. For more information see the AssayQC column. # nolint: line_length_linter
# 0 assay(s) exhibited assay QC warning. For more information see the AssayQC column. # nolint: line_length_linter
# Cross-product normalization will be performed!
# ℹ Output includes two sets of bridging samples.
# We recommend retaining only bridge samples from the reference dataset for downstream analysis. # nolint: line_length_linter
# ℹ 2 non-overlapping assays are included in the normalized dataset without adjustment. Assays found in only one project will have decreased statistical power due to the lower number of samples. # nolint: line_length_linter
# ℹ 2 not bridgeable assays are included in the bridged dataset without adjustment. # nolint: line_length_linter
# ℹ 6 Negative Controls were removed from dataset: "NEGATIVE_CONTROL_1", "NEGATIVE_CONTROL_2", "NEGATIVE_CONTROL_3", "NEGATIVE_CONTROL_4", "NEGATIVE_CONTROL_5", and "NEGATIVE_CONTROL_6" # nolint: line_length_linter
# ℹ 10 Plate Controls were removed from dataset: "PLATE_CONTROL_1", "PLATE_CONTROL_2", "PLATE_CONTROL_3", "PLATE_CONTROL_4", "PLATE_CONTROL_5", "PLATE_CONTROL_6", "PLATE_CONTROL_7", "PLATE_CONTROL_8", # nolint: line_length_linter
# "PLATE_CONTROL_9", and "PLATE_CONTROL_10"
# Warning message:
# 2 assays are not shared across products.
# ℹ 2 assays will be removed from normalization.

### Store in list ----

lst_norm_cp$Reference$Format$HT_3K <- dt_norm_ht_3k_format |>
  # there is a grouped data frame that is generated as part of the
  # normalization. This has been fixed in the OA code, but for the reference
  # results, we need to ungroup the data frame.
  dplyr::ungroup()

## Olink Reveal - Olink Explore 3072 ----

### Bridge samples ----

bridge_sample_reveal_3k <- intersect(
  x = unique(dt_reveal$SampleID),
  y = unique(dt_3k$SampleID)
) |>
  (\(.) .[!grepl("CONTROL", .)])()
# 52 bridge samples

### Normalization ----

dt_norm_reveal_3k_format <- OlinkAnalyze::olink_normalization(
  df1 = dt_reveal,
  df2 = dt_3k,
  overlapping_samples_df1 = bridge_sample_reveal_3k,
  df1_project_nr = "df_reveal",
  df2_project_nr = "df_3k",
  reference_project = "df_reveal",
  format = TRUE
)
# 0 assay(s) exhibited assay QC warning. For more information see the AssayQC column. # nolint: line_length_linter
# 0 assay(s) exhibited assay QC warning. For more information see the AssayQC column. # nolint: line_length_linter
# Cross-product normalization will be performed!
# ℹ Output includes two sets of bridging samples.
# We recommend retaining only bridge samples from the reference dataset for downstream analysis. # nolint: line_length_linter
# ℹ 85 non-overlapping assays are included in the normalized dataset without adjustment. Assays found in only one project will have decreased statistical power due to the lower number of samples. # nolint: line_length_linter
# ℹ 24 not bridgeable assays are included in the bridged dataset without adjustment. # nolint: line_length_linter
# ℹ 6 Negative Controls were removed from dataset: "NEGATIVE_CONTROL_1", "NEGATIVE_CONTROL_2", "NEGATIVE_CONTROL_3", "NEGATIVE_CONTROL_4", "NEGATIVE_CONTROL_5", and "NEGATIVE_CONTROL_6" # nolint: line_length_linter
# ℹ 10 Plate Controls were removed from dataset: "PLATE_CONTROL_1", "PLATE_CONTROL_2", "PLATE_CONTROL_3", "PLATE_CONTROL_4", "PLATE_CONTROL_5", "PLATE_CONTROL_6", "PLATE_CONTROL_7", "PLATE_CONTROL_8", # nolint: line_length_linter
# "PLATE_CONTROL_9", and "PLATE_CONTROL_10"
# Warning message:
# 85 assays are not shared across products.
# ℹ 85 assays will be removed from normalization.

### Store in list ----

lst_norm_cp$Reference$Format$Reveal_3K <- dt_norm_reveal_3k_format |>
  # there is a grouped data frame that is generated as part of the
  # normalization. This has been fixed in the OA code, but for the reference
  # results, we need to ungroup the data frame.
  dplyr::ungroup()

## Olink Explore HT - Olink Reveal ----

### Bridge samples ----

bridge_sample_reveal_ht <- intersect(
  x = unique(dt_reveal$SampleID),
  y = unique(dt_ht$SampleID)
) |>
  (\(.) .[!grepl("CONTROL", .)])()
# 52 bridge samples

### Normalization ----

#### Reveal as reference ----

dt_norm_reveal_ht_format <- OlinkAnalyze::olink_normalization(
  df1 = dt_reveal,
  df2 = dt_ht,
  overlapping_samples_df1 = bridge_sample_reveal_ht,
  df1_project_nr = "df_reveal",
  df2_project_nr = "df_ht",
  reference_project = "df_reveal",
  format = TRUE
)
# 0 assay(s) exhibited assay QC warning. For more information see the AssayQC column. # nolint: line_length_linter
# 0 assay(s) exhibited assay QC warning. For more information see the AssayQC column. # nolint: line_length_linter
# Cross-product normalization will be performed!
# ℹ Output includes two sets of bridging samples.
# We recommend retaining only bridge samples from the reference dataset for downstream analysis. # nolint: line_length_linter
# ℹ 80 non-overlapping assays are included in the normalized dataset without adjustment. Assays found in only one project will have decreased statistical power due to the lower number of samples. # nolint: line_length_linter
# ℹ 24 not bridgeable assays are included in the bridged dataset without adjustment. # nolint: line_length_linter
# ℹ 4 Negative Controls were removed from dataset: "NEGATIVE_CONTROL_1", "NEGATIVE_CONTROL_2", "NEGATIVE_CONTROL_3", and "NEGATIVE_CONTROL_4" # nolint: line_length_linter
# ℹ 10 Plate Controls were removed from dataset: "PLATE_CONTROL_1", "PLATE_CONTROL_2", "PLATE_CONTROL_3", "PLATE_CONTROL_4", "PLATE_CONTROL_5", "PLATE_CONTROL_6", "PLATE_CONTROL_7", "PLATE_CONTROL_8", # nolint: line_length_linter
# "PLATE_CONTROL_9", and "PLATE_CONTROL_10"
# Warning message:
# 80 assays are not shared across products.
# ℹ 80 assays will be removed from normalization.

#### HT as reference ----

dt_norm_ht_reveal_format <- OlinkAnalyze::olink_normalization(
  df1 = dt_reveal,
  df2 = dt_ht,
  overlapping_samples_df1 = bridge_sample_reveal_ht,
  df1_project_nr = "df_reveal",
  df2_project_nr = "df_ht",
  reference_project = "df_ht",
  format = TRUE
)
# 0 assay(s) exhibited assay QC warning. For more information see the AssayQC column. # nolint: line_length_linter
# 0 assay(s) exhibited assay QC warning. For more information see the AssayQC column. # nolint: line_length_linter
# Cross-product normalization will be performed!
#   ℹ Output includes two sets of bridging samples.
# We recommend retaining only bridge samples from the reference dataset for downstream analysis. # nolint: line_length_linter
# ℹ 80 non-overlapping assays are included in the normalized dataset without adjustment. Assays found in only one project will have decreased statistical power due to the lower number of samples. # nolint: line_length_linter
# ℹ 24 not bridgeable assays are included in the bridged dataset without adjustment. # nolint: line_length_linter
# ℹ 4 Negative Controls were removed from dataset: "NEGATIVE_CONTROL_1", "NEGATIVE_CONTROL_2", "NEGATIVE_CONTROL_3", and "NEGATIVE_CONTROL_4" # nolint: line_length_linter
# ℹ 10 Plate Controls were removed from dataset: "PLATE_CONTROL_1", "PLATE_CONTROL_2", "PLATE_CONTROL_3", "PLATE_CONTROL_4", "PLATE_CONTROL_5", "PLATE_CONTROL_6", "PLATE_CONTROL_7", "PLATE_CONTROL_8", # nolint: line_length_linter
# "PLATE_CONTROL_9", and "PLATE_CONTROL_10"
# Warning message:
#   80 assays are not shared across products.
# ℹ 80 assays will be removed from normalization.

### Store in list ----

#### Reveal as reference ----

lst_norm_cp$Reference$Format$Reveal_HT <- dt_norm_reveal_ht_format |>
  # there is a grouped data frame that is generated as part of the
  # normalization. This has been fixed in the OA code, but for the reference
  # results, we need to ungroup the data frame.
  dplyr::ungroup()

#### HT as reference ----

lst_norm_cp$Reference$Format$HT_Reveal <- dt_norm_ht_reveal_format |>
  # there is a grouped data frame that is generated as part of the
  # normalization. This has been fixed in the OA code, but for the reference
  # results, we need to ungroup the data frame.
  dplyr::ungroup()
