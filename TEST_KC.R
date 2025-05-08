# npx_df1 and npx_df2 have NPX, Quantified_value, Ct columns
npx_df1 <- OlinkAnalyze::npx_data1 |>
  mutate(Ct = NPX,
  Quantified_value = NPX) |>
  mutate(Normalization = "Calibrator Normalized")

npx_df2 <- OlinkAnalyze::npx_data2 |>
  mutate(Ct = NPX,
         Quantified_value = NPX) |>
  mutate(Normalization = "Calibrator Normalized")

# Remove columns from df(s) as needed for testing

# npx_df1 <- npx_df1 |> select(-Quantified_value) # Has Ct in common
npx_df2 <- npx_df2 |> select(-NPX)

# overlapping samples - exclude control samples
overlap_samples <- intersect(x = npx_df1$SampleID,
                             y = npx_df2$SampleID) |>
  (\(x) x[!grepl("^CONTROL_SAMPLE", x)])()


# bridge normalization
test <- olink_normalization(
  df1 = npx_df1,
  df2 = npx_df2,
  overlapping_samples_df1 = overlap_samples,
  df1_project_nr = "DF1",
  df2_project_nr = "DF2",
  reference_project = "DF1"
)


# subset normalization
# find a suitable subset of samples from each dataset:
# exclude control samples
# exclude samples that do not pass QC
df1_samples <- npx_df1 |>
  dplyr::group_by(
    dplyr::pick(
      dplyr::all_of("SampleID")
    )
  )|>
  dplyr::filter(
    all(.data[["QC_Warning"]] == 'Pass')
  ) |>
  dplyr::ungroup() |>
  dplyr::filter(
    !grepl(pattern = "^CONTROL_SAMPLE", x = .data[["SampleID"]])
  ) |>
  dplyr::pull(
    .data[["SampleID"]]
  ) |>
  unique()

df2_samples <- npx_df2 |>
  dplyr::group_by(
    dplyr::pick(
      dplyr::all_of("SampleID")
    )
  )|>
  dplyr::filter(
    all(.data[["QC_Warning"]] == 'Pass')
  ) |>
  dplyr::ungroup() |>
  dplyr::filter(
    !grepl(pattern = "^CONTROL_SAMPLE", x = .data[["SampleID"]])
  ) |>
  dplyr::pull(
    .data[["SampleID"]]
  ) |>
  unique()

# select a subset of samples from each set from above
df1_subset <- sample(x = df1_samples, size = 16L)
df2_subset <- sample(x = df2_samples, size = 20L)

# normalize
test <- olink_normalization(
  df1 = npx_df1,
  df2 = npx_df2,
  overlapping_samples_df1 = df1_subset,
  overlapping_samples_df2 = df2_subset,
  df1_project_nr = "P1",
  df2_project_nr = "P2",
  reference_project = "P1"
)

# special case of subset normalization using all samples
test <- olink_normalization(
  df1 = npx_df1,
  df2 = npx_df2,
  overlapping_samples_df1 = df1_samples,
  overlapping_samples_df2 = df2_samples,
  df1_project_nr = "P1",
  df2_project_nr = "P2",
  reference_project = "P1"
)

# reference median normalization

# For the sake of this example, set the reference median to 1
ref_med_df <- npx_data1 |>
  dplyr::select(
    dplyr::all_of(
      c("OlinkID")
    )
  ) |>
  dplyr::distinct() |>
  dplyr::mutate(
    Reference_NPX = runif(n = dplyr::n(),
                          min = -1,
                          max = 1)
  )

# normalize
test <- olink_normalization(
  df1 = npx_df1,
  overlapping_samples_df1 = df1_subset,
  reference_medians = ref_med_df
)

