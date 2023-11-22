# This script creates reference results for usage in the unit tests

# t-test ----

# unpaired t-test
t_test <- OlinkAnalyze::olink_ttest(
  df = OlinkAnalyze::npx_data1,
  variable = "Treatment"
)

# paired t-test
t_test_paired <- OlinkAnalyze::npx_data1 |>
  dplyr::filter(
    .data[["Time"]] %in% c("Baseline", "Week.6")
  ) |>
  OlinkAnalyze::olink_ttest(
    variable = "Time",
    pair_id = "Subject"
  )

# Mann-Whitney U Test ----

# unpaired Mann-Whitney U Test
wilcox_test <- OlinkAnalyze::olink_wilcox(
  df = OlinkAnalyze::npx_data1,
  variable = "Treatment"
)

# paired Mann-Whitney U Test
wilcox_test_paired <- OlinkAnalyze::npx_data1 |>
  dplyr::filter(
    .data[["Time"]] %in% c("Baseline", "Week.6")
  ) |>
  OlinkAnalyze::olink_wilcox(
    variable = "Time",
    pair_id = "Subject"
  )

# One-way non-parametric test ----

## Kruskal-Wallis test ----

# One-way Kruskal-Wallis test
kruskal <- OlinkAnalyze::olink_one_non_parametric(
  df = OlinkAnalyze::npx_data1,
  variable = "Site"
)

kruskal_sign <- kruskal |>
  dplyr::filter(
    .data[["Threshold"]] == "Significant"
  ) |>
  dplyr::pull(
    .data[["OlinkID"]]
  ) |>
  unique()

# Posthoc test for the results from Kruskal-Wallis test
kruskal_posthoc <-
  OlinkAnalyze::olink_one_non_parametric_posthoc(
    df = OlinkAnalyze::npx_data1,
    variable = "Site",
    test = "kruskal",
    olinkid_list = kruskal_sign
  ) |>
  dplyr::mutate(
    id = as.character(.data[["OlinkID"]])
  ) |>
  # Just for consistency. Not actually needed in this case
  dplyr::arrange(
    .data[["id"]],
    .data[["contrast"]]
  ) |>
  dplyr::select(
    -dplyr::all_of("id")
  )

rm(kruskal_sign)

## Friedman test ----

# One-way Friedman Test
friedman <- OlinkAnalyze::olink_one_non_parametric(
  df = OlinkAnalyze::npx_data1,
  variable = "Time",
  subject = "Subject",
  dependence = TRUE
)

friedman_sign <- friedman |>
  dplyr::filter(
    .data[["Threshold"]] == "Significant"
  ) |>
  dplyr::pull(
    .data[["OlinkID"]]
  ) |>
  unique()

# Posthoc test for the results from Friedman test
friedman_posthoc <- OlinkAnalyze::olink_one_non_parametric_posthoc(
  df = OlinkAnalyze::npx_data1,
  variable = "Time",
  test = "friedman",
  olinkid_list = friedman_sign
) |>
  dplyr::mutate(
    id = as.character(.data[["OlinkID"]])
  ) |>
  # Just for consistency. Not actually needed in this case
  dplyr::arrange(
    .data[["id"]],
    .data[["contrast"]]
  ) |>
  dplyr::select(
    -dplyr::all_of("id")
  )

rm(friedman_sign)

# Ordinal regression ----

# Two-way Ordinal Regression with CLM
ordinal_regression <- OlinkAnalyze::olink_ordinalRegression(
  df = OlinkAnalyze::npx_data1,
  variable = "Treatment:Time"
) |>
  dplyr::mutate(
    id = as.character(.data[["OlinkID"]])
  ) |>
  # Just for consistency. Not actually needed in this case
  dplyr::arrange(
    .data[["id"]],
    .data[["Assay"]]
  ) |>
  dplyr::select(
    -dplyr::all_of("id")
  )

ord_reg_sign <- ordinal_regression |>
  dplyr::filter(.data[["Threshold"]] == "Significant"
                & .data[["term"]] == "Treatment:Time") |>
  dplyr::pull(.data[["OlinkID"]]) |>
  unique()

# Posthoc test for the results from Two-way Ordinal Regression with CLM
ordinal_regression_posthoc <-
  OlinkAnalyze::olink_ordinalRegression_posthoc(
    df = OlinkAnalyze::npx_data1,
    variable = c("Treatment:Time"),
    covariates = "Site",
    olinkid_list = ord_reg_sign,
    effect = "Treatment:Time"
  ) |>
  dplyr::mutate(
    id = as.character(.data[["OlinkID"]])
  ) |>
  # Just for consistency. Not actually needed in this case
  dplyr::arrange(
    .data[["id"]],
    .data[["contrast"]]
  ) |>
  dplyr::select(
    -dplyr::all_of("id")
  )

rm(ord_reg_sign)

# ANOVA ----

## ANOVA - site ----

# ANOVA
anova_site <- OlinkAnalyze::olink_anova(
  df = OlinkAnalyze::npx_data1,
  variable = "Site"
) |>
  dplyr::mutate(
    id = as.character(.data[["OlinkID"]])
  ) |>
  dplyr::arrange(
    .data[["id"]]
  ) |>
  dplyr::select(
    -dplyr::all_of("id")
  )

anova_site_sign <- anova_site |>
  dplyr::slice_head(
    n = 10L
  ) |>
  dplyr::pull(
    .data[["OlinkID"]]
  )

# Posthoc ANOVA
anova_site_posthoc <- OlinkAnalyze::olink_anova_posthoc(
  df = OlinkAnalyze::npx_data1,
  variable = "Site",
  olinkid_list = anova_site_sign,
  effect = "Site"
) |>
  dplyr::mutate(
    id = as.character(.data[["OlinkID"]])
  ) |>
  # Since OlinkID is not unique here (=> ties), contrast is used to break them
  dplyr::arrange(
    .data[["id"]],
    .data[["contrast"]]
  ) |>
  dplyr::select(
    -dplyr::all_of("id")
  )

rm(anova_site_sign)

## ANOVA - time ----

# ANOVA
anova_time <- OlinkAnalyze::olink_anova(
  df = OlinkAnalyze::npx_data1,
  variable = "Time"
) |>
  dplyr::mutate(
    id = as.character(.data[["OlinkID"]])
  ) |>
  dplyr::arrange(
    .data[["id"]]
  ) |>
  dplyr::select(
    -dplyr::all_of("id")
  )

anova_time_sign <- anova_time |>
  dplyr::slice_head(
    n = 10L
  ) |>
  dplyr::pull(
    .data[["OlinkID"]]
  )

# Posthoc ANOVA
anova_time_posthoc <- OlinkAnalyze::olink_anova_posthoc(
  df = OlinkAnalyze::npx_data1,
  variable = "Time",
  olinkid_list = anova_time_sign,
  effect = "Time"
) |>
  dplyr::mutate(
    id = as.character(.data[["OlinkID"]])
  ) |>
  # Just for consistency. Not actually needed in this case
  dplyr::arrange(
    .data[["id"]],
    .data[["contrast"]]
  ) |>
  dplyr::select(
    -dplyr::all_of("id")
  )

rm(anova_time_sign)

## ANOVA - site*time ----

# ANOVA
anova_site_time <- OlinkAnalyze::olink_anova(
  df = OlinkAnalyze::npx_data1,
  variable = c("Site", "Time")
) |>
  dplyr::mutate(
    id = as.character(.data[["OlinkID"]])
  ) |>
  # Since OlinkID is not unique here (=> ties), term is used to break them.
  dplyr::arrange(
    .data[["id"]],
    .data[["term"]]
  ) |>
  dplyr::select(
    -dplyr::all_of("id")
  )

# LMER ----

# lmer
lmer <- OlinkAnalyze::olink_lmer(
  df = OlinkAnalyze::npx_data1,
  variable = c("Treatment", "Time"),
  random = "Subject"
) |>
  dplyr::mutate(
    id = as.character(.data[["OlinkID"]])
  ) |>
  # Since OlinkID is not unique here (=> ties), term is used to break them
  dplyr::arrange(
    .data[["id"]],
    .data[["term"]]
  ) |>
  dplyr::select(
    -dplyr::all_of("id")
  )

lmer_sign <- lmer |>
  dplyr::filter(
    .data[["term"]] == "Treatment:Time"
    & .data[["Threshold"]] == "Significant"
  ) |>
  dplyr::pull(
    .data[["OlinkID"]]
  )

# lmer posthoc
lmer_posthoc <- OlinkAnalyze::olink_lmer_posthoc(
  df = OlinkAnalyze::npx_data1,
  variable = c("Treatment", "Time"),
  random = "Subject",
  olinkid_list = lmer_sign,
  effect = c("Treatment", "Time")
) |>
  dplyr::mutate(
    id = as.character(.data[["OlinkID"]])
  ) |>
  dplyr::arrange(
    .data[["id"]],
    .data[["contrast"]]
  ) |>
  dplyr::select(
    -dplyr::all_of("id")
  )

rm(lmer_sign)

# Olink normalization ----

# Output this subset of samples to reduce the file size
norm_out_sample_set <- c("A6", "A38", "B47", "B22", "A43", "D75", "D79", "C66",
                         "B43", "B70", "D52", "A58", "B71", "A50", "D1", "B8")

## Bridge normalization ----

norm_overlap_samples <- intersect(
  x = OlinkAnalyze::npx_data1$SampleID,
  y = OlinkAnalyze::npx_data2$SampleID
) |>
  (\(x) x[!grepl("CONTROL_SAMPLE", x)])()

normalization_bridge <- OlinkAnalyze::olink_normalization(
  df1 = OlinkAnalyze::npx_data1,
  df2 = OlinkAnalyze::npx_data2,
  overlapping_samples_df1 = norm_overlap_samples,
  df1_project_nr = "20200001",
  df2_project_nr = "20200002",
  reference_project = "20200001"
) |>
  dplyr::filter(
    .data[["SampleID"]] %in% .env[["norm_out_sample_set"]]
  )

## Intensity normalization ----

normalization_intensity <- OlinkAnalyze::olink_normalization(
  df1 = OlinkAnalyze::npx_data1,
  df2 = OlinkAnalyze::npx_data2,
  overlapping_samples_df1 = unique(OlinkAnalyze::npx_data1$SampleID),
  overlapping_samples_df2 = unique(OlinkAnalyze::npx_data2$SampleID)
) |>
  dplyr::filter(
    .data[["SampleID"]] %in% .env[["norm_out_sample_set"]]
  )

## Subset normalization ----

normalization_subset <- OlinkAnalyze::olink_normalization(
  df1 = OlinkAnalyze::npx_data1,
  df2 = OlinkAnalyze::npx_data2,
  overlapping_samples_df1 = unique(OlinkAnalyze::npx_data1$SampleID),
  # NOTE: this subset is just a random sample to test the function
  overlapping_samples_df2 = c("C6", "C21", "C28", "C50", "C19", "D5", "A30",
                              "C52", "D77", "D3", "D16", "C72", "A52", "D67",
                              "C77", "C22", "D62", "D39", "C34", "C13"),
  df1_project_nr = "20200001",
  df2_project_nr = "20200002",
  reference_project = "20200001"
) |>
  dplyr::filter(
    .data[["SampleID"]] %in% .env[["norm_out_sample_set"]]
  )

## Multi-batch normalization ----

### Prep data ----

# Prep df1 from npx_data1
norm_multibatch_df1 <- OlinkAnalyze::npx_data1 |>
  dplyr::filter(
    !stringr::str_detect(.data[["SampleID"]], "CONTROL_")
  ) |>
  dplyr::select(
    -dplyr::all_of("Project")
  ) |>
  dplyr::mutate(
    Normalization = "Intensity"
  )

# Prep df2 from npx_data2
norm_multibatch_df2 <- OlinkAnalyze::npx_data2 |>
  dplyr::filter(
    !stringr::str_detect(.data[["SampleID"]], "CONTROL_")
  ) |>
  dplyr::select(
    -dplyr::all_of("Project")
  ) |>
  dplyr::mutate(
    Normalization = "Intensity"
  )

# Prep df3 from npx_data2
# manipulating the sample NPX datasets to create another two random ones
norm_multibatch_df3 <- OlinkAnalyze::npx_data2 |>
  dplyr::filter(
    !stringr::str_detect(.data[["SampleID"]], "CONTROL_")
  ) |>
  dplyr::mutate(
    SampleID = paste0(.data[["SampleID"]], "_mod"),
    PlateID = paste0(.data[["PlateID"]], "_mod")
  ) |>
  dplyr::select(
    -dplyr::all_of("Project")
  ) |>
  dplyr::mutate(
    Normalization = "Intensity"
  )

# Prep df4 from npx_data1
norm_multibatch_df4 <- OlinkAnalyze::npx_data1 |>
  dplyr::filter(
    !stringr::str_detect(.data[["SampleID"]], "CONTROL_")
  ) |>
  dplyr::mutate(
    SampleID = paste0(.data[["SampleID"]], "_mod2"),
    PlateID = paste0(.data[["PlateID"]], "_mod2")
  ) |>
  dplyr::select(
    -dplyr::all_of("Project")
  ) |>
  dplyr::mutate(
    Normalization = "Intensity"
  )

### Samples for normalization ----

# Bridge samples for npx_multi_df1 and npx_multi_df2
norm_overlap_samples_df1_df2 <- list(
  "DF1" = norm_overlap_samples,
  "DF2" = norm_overlap_samples
)

# Bridge samples for npx_multi_df2 and npx_multi_df3
norm_overlap_samples_df2_df3 <- list(
  "DF1" = c("A13", "A29", "A30", "A36", "A45", "A46", "A52", "A63", "A71",
            "A73"),
  "DF2" = c("C1_mod", "C10_mod", "C11_mod", "C12_mod", "C13_mod", "C14_mod",
            "C15_mod", "C16_mod", "C17_mod", "C18_mod")
)

# Samples to use for intensity normalization between npx_df4 and the
# normalized dataset of npx_df1 and npx_df2
norm_overlap_samples_df13_df4 <- list(
  "DF1" = c("A1", "A10", "A11", "A12", "A13", "A13_mod", "A14", "A15", "A16",
            "A17", "A18", "A19", "A2", "A20", "A21", "A22", "A23", "A24",
            "A25", "A26", "A27", "A28", "A29", "A29_mod", "A3", "A30",
            "A30_mod", "A31", "A32", "A33", "A34", "A35", "A36", "A36_mod",
            "A37", "A38", "A39", "A4", "A40", "A41", "A42", "A43", "A44", "A45",
            "A45_mod", "A46", "A46_mod", "A47", "A48", "A49", "A5", "A50",
            "A51", "A52", "A52_mod", "A53", "A54", "A55", "A56", "A57", "A58",
            "A59", "A6", "A60", "A61", "A62", "A63", "A63_mod", "A64", "A65",
            "A66", "A67", "A68", "A69", "A7", "A70", "A71", "A71_mod", "A72",
            "A73", "A73_mod", "A74", "A75", "A76", "A77", "A8", "A9"),
  "DF2" = c("B1_mod2", "B10_mod2", "B11_mod2", "B12_mod2", "B13_mod2",
            "B14_mod2", "B15_mod2", "B16_mod2", "B17_mod2", "B18_mod2",
            "B19_mod2", "B2_mod2", "B20_mod2", "B21_mod2", "B22_mod2",
            "B23_mod2", "B24_mod2", "B25_mod2", "B26_mod2", "B27_mod2",
            "B28_mod2", "B29_mod2", "B3_mod2", "B30_mod2", "B31_mod2",
            "B32_mod2", "B33_mod2", "B34_mod2", "B35_mod2", "B36_mod2",
            "B37_mod2", "B38_mod2", "B39_mod2", "B4_mod2", "B40_mod2",
            "B41_mod2", "B42_mod2", "B43_mod2", "B44_mod2", "B45_mod2",
            "B46_mod2", "B47_mod2", "B48_mod2", "B49_mod2", "B5_mod2",
            "B50_mod2", "B51_mod2", "B52_mod2", "B53_mod2", "B54_mod2",
            "B55_mod2", "B56_mod2", "B57_mod2", "B58_mod2", "B59_mod2",
            "B6_mod2", "B60_mod2", "B61_mod2", "B62_mod2", "B63_mod2",
            "B64_mod2", "B65_mod2", "B66_mod2", "B67_mod2", "B68_mod2",
            "B69_mod2", "B7_mod2", "B70_mod2", "B71_mod2", "B72_mod2",
            "B73_mod2", "B74_mod2", "B75_mod2", "B76_mod2", "B77_mod2",
            "B78_mod2", "B79_mod2", "B8_mod2", "B9_mod2")
)

### Create tibble for input and normalize ----

norm_multibatch_schema <- dplyr::tibble(
  order = c(1L, 2L, 3L, 4L),
  name = c("NPX_DF1", "NPX_DF2", "NPX_DF3", "NPX_DF4"),
  data = list("NPX_DF1" = norm_multibatch_df1,
              "NPX_DF2" = norm_multibatch_df2,
              "NPX_DF3" = norm_multibatch_df3,
              "NPX_DF4" = norm_multibatch_df4),
  samples = list("NPX_DF1" = NA_character_,
                 "NPX_DF2" = norm_overlap_samples_df1_df2,
                 "NPX_DF3" = norm_overlap_samples_df2_df3,
                 "NPX_DF4" = norm_overlap_samples_df13_df4),
  normalization_type = c(NA_character_, "Bridge", "Bridge", "Subset"),
  normalize_to = c(NA_character_, "1", "2", "1,3")
)

normalization_multibatch <-
  OlinkAnalyze::olink_normalization_n(
    norm_schema = norm_multibatch_schema
  ) |>
  dplyr::mutate(
    SampleID_tmp = {
      .data[["SampleID"]] |>
        stringr::str_split(pattern = "_") |>
        lapply(head, 1L) |>
        unlist()
    }
  ) |>
  dplyr::filter(
    .data[["SampleID_tmp"]] %in% .env[["norm_out_sample_set"]]
  ) |>
  dplyr::select(
    -dplyr::all_of("SampleID_tmp")
  ) |>
  dplyr::arrange(
    .data[["Project"]],
    .data[["Panel"]],
    .data[["OlinkID"]],
    .data[["SampleID"]]
  )

rm(
  norm_out_sample_set,
  norm_overlap_samples,
  norm_overlap_samples_df1_df2,
  norm_overlap_samples_df2_df3,
  norm_overlap_samples_df13_df4,
  norm_multibatch_df1,
  norm_multibatch_df2,
  norm_multibatch_df3,
  norm_multibatch_df4,
  norm_multibatch_schema
)

# Plate randomization ----

randomized_samples <- OlinkAnalyze::olink_plate_randomizer(
  Manifest = OlinkAnalyze::manifest,
  seed = 12345L
)

randomized_subjects <- OlinkAnalyze::olink_plate_randomizer(
  Manifest = OlinkAnalyze::manifest,
  SubjectColumn = "SubjectID",
  available.spots = c(88L, 88L),
  seed = 12345L
)

randomized_subjects_spots <- OlinkAnalyze::manifest |>
  dplyr::mutate(
    study = dplyr::if_else(
      .data[["Site"]] == "Site1", "study1", "study2"
    )
  ) |>
  OlinkAnalyze::olink_plate_randomizer(
    SubjectColumn = "SubjectID",
    available.spots = c(88L, 88L),
    seed = 12345L
  )

randomized_samples_spots <- OlinkAnalyze::manifest |>
  dplyr::mutate(
    study = dplyr::if_else(
      .data[["Site"]] == "Site1", "study1", "study2"
    )
  ) |>
  OlinkAnalyze::olink_plate_randomizer(
    available.spots = c(88L, 88L),
    seed = 12345L
  )

# NPX pre-processing for dimansionality reduction ----

## Pre-processing without missing data ----

preproc_npx_data1 <- OlinkAnalyze::npx_data1 |>
  dplyr::mutate(
    SampleID = paste0(.data[["SampleID"]], "_", Index)
  )

preprocessing_dim_red <- OlinkAnalyze:::npxProcessing_forDimRed(
  df = preproc_npx_data1,
  color_g = "QC_Warning",
  drop_assays = FALSE,
  drop_samples = FALSE,
  verbose = TRUE
)
# keep only the data frame
preprocessing_dim_red <- preprocessing_dim_red$df_wide

## Pre-processing with missing data ----

preproc_samples_miss <- preproc_npx_data1$SampleID |>
  unique() |>
  sample(
    size = {
      preproc_npx_data1$SampleID |>
        (\(x) dplyr::n_distinct(x) * 0.15 |> ceiling())()
    }
  )

preproc_npx_data1_missing_data <- preproc_npx_data1 |>
  # These should be removed due to to high missingness
  dplyr::mutate(
    NPX = dplyr::if_else(.data[["SampleID"]] %in% .env[["preproc_samples_miss"]]
                         & .data[["OlinkID"]] %in% c("OID00482",
                                                     "OID00483",
                                                     "OID00484",
                                                     "OID00485"),
                         NA_real_,
                         .data[["NPX"]])
  ) |>
  # These should be median imputed
  dplyr::mutate(
    NPX = dplyr::if_else(.data[["SampleID"]] %in% c("A18_19",
                                                    "B8_87")
                         & .data[["OlinkID"]] %in% c("OID00562",
                                                     "OID01213",
                                                     "OID05124"),
                         NA_real_,
                         .data[["NPX"]])
  )

preprocessing_dim_red_miss <- OlinkAnalyze:::npxProcessing_forDimRed(
  df = preproc_npx_data1_missing_data,
  color_g = "QC_Warning",
  drop_assays = FALSE,
  drop_samples = FALSE,
  verbose = TRUE
)
# keep only the data frame
preprocessing_dim_red_miss <- preprocessing_dim_red_miss$df_wide

rm(
  preproc_npx_data1,
  preproc_samples_miss,
  preproc_npx_data1_missing_data
)

# Wrap up the results ----

reference_results <- list(
  t_test = t_test,
  t_test_paired = t_test_paired,

  wilcox_test = wilcox_test,
  wilcox_test_paired = wilcox_test_paired,

  kruskal = kruskal,
  kruskal_posthoc = kruskal_posthoc,

  friedman = friedman,
  friedman_posthoc = friedman_posthoc,

  ordinal_regression = ordinal_regression,
  ordinal_regression_posthoc = ordinal_regression_posthoc,

  anova_site = anova_site,
  anova_site_posthoc = anova_site_posthoc,

  anova_time = anova_time,
  anova_time_posthoc = anova_time_posthoc,

  anova_site_time = anova_site_time,

  lmer = lmer,
  lmer_posthoc = lmer_posthoc,

  normalization_bridge = normalization_bridge,
  normalization_intensity = normalization_intensity,
  normalization_subset = normalization_subset,
  normalization_multibatch = normalization_multibatch,

  randomized_samples = randomized_samples,
  randomized_subjects = randomized_subjects,
  randomized_subjects_spots = randomized_subjects_spots,
  randomized_samples_spots = randomized_samples_spots,

  preprocessing_dim_red = preprocessing_dim_red,
  preprocessing_dim_red_miss = preprocessing_dim_red_miss
)

save(
  reference_results,
  file = "../../reference_results.RData"
)
