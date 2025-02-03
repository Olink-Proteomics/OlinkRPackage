# This script generated reference data for OlinkAnalyze::olink_normalization using
# OlinkAnalyze v3.8.2. To revert to OA version OA 3.8.2 please use the following
# command:
# remotes::install_version("OlinkAnalyze", version = "3.8.2")
# and check the packageVersion using:
# packageVersion("OlinkAnalyze")

# datasets ----

lst_df <- list()

## npx_data1 ----

# npx_data1 does not contain column Normalization
lst_df$df1_no_norm <- OlinkAnalyze::npx_data1 |>
  dplyr::select(
    -dplyr::all_of(
      c("Subject", "Treatment", "Site", "Time", "Project")
    )
  )

# npx_data1 with Normalization column
lst_df$df1_norm <- OlinkAnalyze::npx_data1 |>
  dplyr::select(
    -dplyr::all_of(
      c("Subject", "Treatment", "Site", "Time", "Project")
    )
  ) |>
  dplyr::mutate(
    Normalization = "Intensity"
  )

# npx_data1 with Normalization column, but no LOD column
lst_df$df1_no_lod <- OlinkAnalyze::npx_data1 |>
  dplyr::select(
    -dplyr::all_of(
      c("Subject", "Treatment", "Site", "Time", "Project", "LOD")
    )
  ) |>
  dplyr::mutate(
    Normalization = "Intensity"
  )

# npx_data1 with Normalization column, and PlateLOD+MaxLOD instead of LOD
lst_df$df1_multiple_lod <- OlinkAnalyze::npx_data1 |>
  dplyr::select(
    -dplyr::all_of(
      c("Subject", "Treatment", "Site", "Time", "Project")
    )
  ) |>
  dplyr::rename(
    "PlateLOD" = "LOD"
  ) |>
  dplyr::mutate(
    Normalization = "Intensity",
    MaxLOD = .data[["PlateLOD"]] + 0.5
  )

## npx_data2 ----

# npx_data2 does not contain column Normalization
lst_df$df2_no_norm <- OlinkAnalyze::npx_data2 |>
  dplyr::select(
    -dplyr::all_of(
      c("Subject", "Treatment", "Site", "Time", "Project")
    )
  )

# npx_data2 with Normalization column
lst_df$df2_norm <- OlinkAnalyze::npx_data2 |>
  dplyr::select(
    -dplyr::all_of(
      c("Subject", "Treatment", "Site", "Time", "Project")
    )
  ) |>
  dplyr::mutate(
    Normalization = "Intensity"
  )

# npx_data2 with Normalization column, but no LOD column
lst_df$df2_no_lod <- OlinkAnalyze::npx_data2 |>
  dplyr::select(
    -dplyr::all_of(
      c("Subject", "Treatment", "Site", "Time", "Project", "LOD")
    )
  ) |>
  dplyr::mutate(
    Normalization = "Intensity"
  )

# npx_data2 with Normalization column, and PlateLOD+MaxLOD instead of LOD
lst_df$df2_multiple_lod <- OlinkAnalyze::npx_data2 |>
  dplyr::select(
    -dplyr::all_of(
      c("Subject", "Treatment", "Site", "Time", "Project")
    )
  ) |>
  dplyr::rename(
    "PlateLOD" = "LOD"
  ) |>
  dplyr::mutate(
    Normalization = "Intensity",
    MaxLOD = .data[["PlateLOD"]] - 0.2
  )

## reference_medians ----

lst_df$ref_med <- OlinkAnalyze::npx_data1 |>
  dplyr::group_by(
    dplyr::pick(
      c("OlinkID")
    )
  ) |>
  dplyr::summarise(
    mu_npx = mean(x = .data[["NPX"]], na.rm = TRUE) / 3L,
    mu_lod = mean(x = .data[["LOD"]], na.rm = TRUE),
    Reference_NPX = dplyr::if_else(
      .data[["mu_lod"]] == 0,
      .data[["mu_npx"]],
      .data[["mu_lod"]]
    ),
    .groups = "drop"
  ) |>
  dplyr::select(
    dplyr::all_of(
      c("OlinkID", "Reference_NPX")
    )
  )

# samples ----

lst_sample <- list()

# sample subset to reduce file size ----

lst_sample$sample_subset <- c("A6", "A38", "B47", "B22", "A43", "D75", "D79",
                              "C66", "B43", "B70", "D52", "A58", "B71", "A50",
                              "D1", "B8")

## bridge samples ----

lst_sample$bridge_samples <- intersect(
  x = OlinkAnalyze::npx_data1$SampleID,
  y = OlinkAnalyze::npx_data2$SampleID
) |>
  (\(x) x[!grepl(pattern = "CONTROL_SAMPLE", x = x)])()

## npx_data1 samples ----

lst_sample$df1_all <- OlinkAnalyze::npx_data1$SampleID |>
  unique() |>
  (\(x) x[!grepl(pattern = "CONTROL_SAMPLE", x = x)])()

lst_sample$df1_subset <- OlinkAnalyze::npx_data1$SampleID |>
  unique() |>
  sort() |>
  (\(x) x[!grepl(pattern = "CONTROL_SAMPLE", x = x)])() |>
  head(20L)

## npx_data2 samples ----

lst_sample$df2_all <- OlinkAnalyze::npx_data2$SampleID |>
  unique() |>
  (\(x) x[!grepl(pattern = "CONTROL_SAMPLE", x = x)])()

lst_sample$df2_subset <- OlinkAnalyze::npx_data2$SampleID |>
  unique() |>
  sort() |>
  (\(x) x[!grepl(pattern = "CONTROL_SAMPLE", x = x)])() |>
  head(15L)

# normalize ----

lst_norm <- list()

## bridge normalization ----

lst_norm$bridge_norm <- list()

### df1_no_norm + df2_no_norm ----

lst_norm$bridge_norm$no_norm <- OlinkAnalyze::olink_normalization(
  df1 = lst_df$df1_no_norm,
  df2 = lst_df$df2_no_norm,
  overlapping_samples_df1 = lst_sample$bridge_samples,
  df1_project_nr = "df1_no_norm",
  df2_project_nr = "df2_no_norm",
  reference_project = "df1_no_norm"
) |>
  dplyr::filter(
    .data[["SampleID"]] %in% lst_sample$sample_subset
  )

### df1_norm + df2_norm ----

lst_norm$bridge_norm$norm <- OlinkAnalyze::olink_normalization(
  df1 = lst_df$df1_norm,
  df2 = lst_df$df2_norm,
  overlapping_samples_df1 = lst_sample$bridge_samples,
  df1_project_nr = "df1_norm",
  df2_project_nr = "df2_norm",
  reference_project = "df1_norm"
) |>
  dplyr::filter(
    .data[["SampleID"]] %in% lst_sample$sample_subset
  )

### df1_no_lod + df2_no_lod ----

lst_norm$bridge_norm$no_lod <- OlinkAnalyze::olink_normalization(
  df1 = lst_df$df1_no_lod,
  df2 = lst_df$df2_no_lod,
  overlapping_samples_df1 = lst_sample$bridge_samples,
  df1_project_nr = "df1_no_lod",
  df2_project_nr = "df2_no_lod",
  reference_project = "df1_no_lod"
) |>
  dplyr::filter(
    .data[["SampleID"]] %in% lst_sample$sample_subset
  )

### df1_multiple_lod + df2_multiple_lod ----

lst_norm$bridge_norm$multiple_lod <- OlinkAnalyze::olink_normalization(
  df1 = lst_df$df1_multiple_lod,
  df2 = lst_df$df2_multiple_lod,
  overlapping_samples_df1 = lst_sample$bridge_samples,
  df1_project_nr = "df1_multiple_lod",
  df2_project_nr = "df2_multiple_lod",
  reference_project = "df1_multiple_lod"
) |>
  dplyr::filter(
    .data[["SampleID"]] %in% lst_sample$sample_subset
  )

## subset normalization ----

lst_norm$subset_norm <- list()

### df1_no_norm + df2_no_norm ----

lst_norm$subset_norm$no_norm <- OlinkAnalyze::olink_normalization(
  df1 = lst_df$df1_no_norm,
  df2 = lst_df$df2_no_norm,
  overlapping_samples_df1 = lst_sample$df1_subset,
  overlapping_samples_df2 = lst_sample$df2_subset,
  df1_project_nr = "df1_no_norm",
  df2_project_nr = "df2_no_norm",
  reference_project = "df1_no_norm"
) |>
  dplyr::filter(
    .data[["SampleID"]] %in% lst_sample$sample_subset
  )

### df1_norm + df2_norm ----

lst_norm$subset_norm$norm <- OlinkAnalyze::olink_normalization(
  df1 = lst_df$df1_norm,
  df2 = lst_df$df2_norm,
  overlapping_samples_df1 = lst_sample$df1_subset,
  overlapping_samples_df2 = lst_sample$df2_subset,
  df1_project_nr = "df1_norm",
  df2_project_nr = "df2_norm",
  reference_project = "df1_norm"
) |>
  dplyr::filter(
    .data[["SampleID"]] %in% lst_sample$sample_subset
  )

### df1_no_lod + df2_no_lod ----

lst_norm$subset_norm$no_lod <- OlinkAnalyze::olink_normalization(
  df1 = lst_df$df1_no_lod,
  df2 = lst_df$df2_no_lod,
  overlapping_samples_df1 = lst_sample$df1_subset,
  overlapping_samples_df2 = lst_sample$df2_subset,
  df1_project_nr = "df1_no_lod",
  df2_project_nr = "df2_no_lod",
  reference_project = "df1_no_lod"
) |>
  dplyr::filter(
    .data[["SampleID"]] %in% lst_sample$sample_subset
  )

### df1_multiple_lod + df2_multiple_lod ----

lst_norm$subset_norm$multiple_lod <- OlinkAnalyze::olink_normalization(
  df1 = lst_df$df1_multiple_lod,
  df2 = lst_df$df2_multiple_lod,
  overlapping_samples_df1 = lst_sample$df1_subset,
  overlapping_samples_df2 = lst_sample$df2_subset,
  df1_project_nr = "df1_multiple_lod",
  df2_project_nr = "df2_multiple_lod",
  reference_project = "df1_multiple_lod"
) |>
  dplyr::filter(
    .data[["SampleID"]] %in% lst_sample$sample_subset
  )

## intensity normalization ----

lst_norm$intensity_norm <- list()

### df1_no_norm + df2_no_norm ----

lst_norm$intensity_norm$no_norm <- OlinkAnalyze::olink_normalization(
  df1 = lst_df$df1_no_norm,
  df2 = lst_df$df2_no_norm,
  overlapping_samples_df1 = lst_sample$df1_all,
  overlapping_samples_df2 = lst_sample$df2_all,
  df1_project_nr = "df1_no_norm",
  df2_project_nr = "df2_no_norm",
  reference_project = "df1_no_norm"
) |>
  dplyr::filter(
    .data[["SampleID"]] %in% lst_sample$sample_subset
  )

### df1_norm + df2_norm ----

lst_norm$intensity_norm$norm <- OlinkAnalyze::olink_normalization(
  df1 = lst_df$df1_norm,
  df2 = lst_df$df2_norm,
  overlapping_samples_df1 = lst_sample$df1_all,
  overlapping_samples_df2 = lst_sample$df2_all,
  df1_project_nr = "df1_norm",
  df2_project_nr = "df2_norm",
  reference_project = "df1_norm"
) |>
  dplyr::filter(
    .data[["SampleID"]] %in% lst_sample$sample_subset
  )

### df1_no_lod + df2_no_lod ----

lst_norm$intensity_norm$no_lod <- OlinkAnalyze::olink_normalization(
  df1 = lst_df$df1_no_lod,
  df2 = lst_df$df2_no_lod,
  overlapping_samples_df1 = lst_sample$df1_all,
  overlapping_samples_df2 = lst_sample$df2_all,
  df1_project_nr = "df1_no_lod",
  df2_project_nr = "df2_no_lod",
  reference_project = "df1_no_lod"
) |>
  dplyr::filter(
    .data[["SampleID"]] %in% lst_sample$sample_subset
  )

### df1_multiple_lod + df2_multiple_lod ----

lst_norm$intensity_norm$multiple_lod <- OlinkAnalyze::olink_normalization(
  df1 = lst_df$df1_multiple_lod,
  df2 = lst_df$df2_multiple_lod,
  overlapping_samples_df1 = lst_sample$df1_all,
  overlapping_samples_df2 = lst_sample$df2_all,
  df1_project_nr = "df1_multiple_lod",
  df2_project_nr = "df2_multiple_lod",
  reference_project = "df1_multiple_lod"
) |>
  dplyr::filter(
    .data[["SampleID"]] %in% lst_sample$sample_subset
  )

## reference median normalization ----

lst_norm$ref_med_norm <- list()

### df1_no_norm ----

lst_norm$ref_med_norm$no_norm <- OlinkAnalyze::olink_normalization(
  df1 = lst_df$df1_no_norm,
  overlapping_samples_df1 = lst_sample$df1_subset,
  reference_medians = lst_df$ref_med
) |>
  dplyr::filter(
    .data[["SampleID"]] %in% lst_sample$sample_subset
  ) |>
  dplyr::mutate(
    Project = "df1_no_norm"
  ) |>
  dplyr::select(
    dplyr::all_of(
      c(colnames(lst_df$df1_no_norm), "Adj_factor", "Project")
    )
  )

### df1_norm ----

lst_norm$ref_med_norm$norm <- OlinkAnalyze::olink_normalization(
  df1 = lst_df$df1_norm,
  overlapping_samples_df1 = lst_sample$df1_subset,
  reference_medians = lst_df$ref_med
) |>
  dplyr::filter(
    .data[["SampleID"]] %in% lst_sample$sample_subset
  ) |>
  dplyr::mutate(
    Project = "df1_norm"
  ) |>
  dplyr::select(
    dplyr::all_of(
      c(colnames(lst_df$df1_norm), "Adj_factor", "Project")
    )
  )

### df1_no_lod ----

lst_norm$ref_med_norm$no_lod <- OlinkAnalyze::olink_normalization(
  df1 = lst_df$df1_no_lod,
  overlapping_samples_df1 = lst_sample$df1_subset,
  reference_medians = lst_df$ref_med
) |>
  dplyr::filter(
    .data[["SampleID"]] %in% lst_sample$sample_subset
  ) |>
  dplyr::mutate(
    Project = "df1_no_lod"
  ) |>
  dplyr::select(
    dplyr::all_of(
      c(colnames(lst_df$df1_no_lod), "Adj_factor", "Project")
    )
  )

### df1_multiple_lod ----

lst_norm$ref_med_norm$multiple_lod <- OlinkAnalyze::olink_normalization(
  df1 = lst_df$df1_multiple_lod,
  overlapping_samples_df1 = lst_sample$df1_subset,
  reference_medians = lst_df$ref_med
) |>
  dplyr::filter(
    .data[["SampleID"]] %in% lst_sample$sample_subset
  ) |>
  dplyr::mutate(
    Project = "df1_multiple_lod"
  ) |>
  dplyr::select(
    dplyr::all_of(
      c(colnames(lst_df$df1_multiple_lod), "Adj_factor", "Project")
    )
  )

# save data ----

saveRDS(
  object = list(
    lst_df = lst_df,
    lst_sample = lst_sample,
    lst_norm = lst_norm
  ),
  file = "tests/data/ref_results_norm.rds",
  version = 2L,
  compress = "gzip"
)
