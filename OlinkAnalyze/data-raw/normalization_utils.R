# modes that olink normalization functions may have
olink_norm_modes <- list(
  "bridge" = "bridge",
  "subset" = "subset",
  "ref_median" = "ref_median",
  "norm_ht_3k" = "norm_ht_3k"
)

# pre-populated dataset with column names and classes that the reference medians
# input dataset may have
olink_norm_ref_median_cols <- dplyr::tibble(
  cols = c("OlinkID", "Reference_NPX"),
  class = c("character", "numeric"),
  name = c("olink_id", "ref_med")
)

# columns tha should be re-calculated post-normalization
olink_norm_recalc <- list(
  max_lod = c("Max LOD", "Max_LOD", "MaxLOD")
)

# dataset with all possible combinations of inputs that olink_normalization may
# take and the error, warning or messages that should be printed in each case.
# The dataset also marks which olink_norm_mode is returned in each case.
olink_norm_mode_combos <- expand.grid(df1 = c(FALSE, TRUE),
                                      df2 = c(FALSE, TRUE),
                                      overlapping_samples_df1 = c(FALSE, TRUE),
                                      overlapping_samples_df2 = c(FALSE, TRUE),
                                      reference_medians = c(FALSE, TRUE)) |>
  dplyr::mutate(
    error_msg = dplyr::case_when(
      df1 == FALSE ~ "Required {.var df1} is missing!",
      df1 == TRUE & df2 == FALSE & reference_medians == FALSE ~ "When {.var df1} is provided, either {.var df2} or {.var reference_medians} is required!", # nolint
      df1 == TRUE & df2 == FALSE & reference_medians == TRUE & overlapping_samples_df1 == FALSE ~ "When {.var df1} and {.var reference_medians} are provided, {.var overlapping_samples_df1} is required!", # nolint
      df1 == TRUE & df2 == FALSE & reference_medians == TRUE & overlapping_samples_df1 == TRUE & overlapping_samples_df2 == FALSE ~ NA_character_, # nolint
      df1 == TRUE & df2 == FALSE & reference_medians == TRUE & overlapping_samples_df1 == TRUE & overlapping_samples_df2 == TRUE ~ NA_character_, # nolint
      df1 == TRUE & df2 == TRUE & overlapping_samples_df1 == FALSE ~ "When {.var df1} and {.var df2} are provided, at least {.var overlapping_samples_df1} is required!", # nolint
      df1 == TRUE & df2 == TRUE & overlapping_samples_df1 == TRUE & overlapping_samples_df2 == FALSE & reference_medians == FALSE ~ NA_character_, # nolint
      df1 == TRUE & df2 == TRUE & overlapping_samples_df1 == TRUE & overlapping_samples_df2 == FALSE & reference_medians == TRUE ~ NA_character_, # nolint
      df1 == TRUE & df2 == TRUE & overlapping_samples_df1 == TRUE & overlapping_samples_df2 == TRUE & reference_medians == FALSE ~ NA_character_, # nolint
      df1 == TRUE & df2 == TRUE & overlapping_samples_df1 == TRUE & overlapping_samples_df2 == TRUE & reference_medians == TRUE ~ NA_character_, # nolint
      TRUE ~ NA_character_,
      .default = NA_character_
    ),
    warning_msg = dplyr::case_when(
      df1 == FALSE ~ NA_character_,
      df1 == TRUE & df2 == FALSE & reference_medians == FALSE ~ NA_character_,
      df1 == TRUE & df2 == FALSE & reference_medians == TRUE & overlapping_samples_df1 == FALSE ~ NA_character_, # nolint
      df1 == TRUE & df2 == FALSE & reference_medians == TRUE & overlapping_samples_df1 == TRUE & overlapping_samples_df2 == FALSE ~ NA_character_, # nolint
      df1 == TRUE & df2 == FALSE & reference_medians == TRUE & overlapping_samples_df1 == TRUE & overlapping_samples_df2 == TRUE ~ "{.var overlapping_samples_df2} will be ignored", # nolint
      df1 == TRUE & df2 == TRUE & overlapping_samples_df1 == FALSE ~ NA_character_, # nolint
      df1 == TRUE & df2 == TRUE & overlapping_samples_df1 == TRUE & overlapping_samples_df2 == FALSE & reference_medians == FALSE ~ NA_character_, # nolint
      df1 == TRUE & df2 == TRUE & overlapping_samples_df1 == TRUE & overlapping_samples_df2 == FALSE & reference_medians == TRUE ~ "{.var reference_medians} will be ignored", # nolint
      df1 == TRUE & df2 == TRUE & overlapping_samples_df1 == TRUE & overlapping_samples_df2 == TRUE & reference_medians == FALSE ~ NA_character_, # nolint
      df1 == TRUE & df2 == TRUE & overlapping_samples_df1 == TRUE & overlapping_samples_df2 == TRUE & reference_medians == TRUE ~ "{.var reference_medians} will be ignored", # nolint
      TRUE ~ NA_character_,
      .default = NA_character_
    ),
    inform_msg = dplyr::case_when(
      df1 == FALSE ~ NA_character_,
      df1 == TRUE & df2 == FALSE & reference_medians == FALSE ~ NA_character_,
      df1 == TRUE & df2 == FALSE & reference_medians == TRUE & overlapping_samples_df1 == FALSE ~ NA_character_, # nolint
      df1 == TRUE & df2 == FALSE & reference_medians == TRUE & overlapping_samples_df1 == TRUE & overlapping_samples_df2 == FALSE ~ "Reference median normalization will be performed!", # nolint
      df1 == TRUE & df2 == FALSE & reference_medians == TRUE & overlapping_samples_df1 == TRUE & overlapping_samples_df2 == TRUE ~ "Reference median normalization will be performed!", # nolint
      df1 == TRUE & df2 == TRUE & overlapping_samples_df1 == FALSE ~ NA_character_, # nolint
      df1 == TRUE & df2 == TRUE & overlapping_samples_df1 == TRUE & overlapping_samples_df2 == FALSE & reference_medians == FALSE ~ "Bridge normalization will be performed!", # nolint
      df1 == TRUE & df2 == TRUE & overlapping_samples_df1 == TRUE & overlapping_samples_df2 == FALSE & reference_medians == TRUE ~ "Bridge normalization will be performed!", # nolint
      df1 == TRUE & df2 == TRUE & overlapping_samples_df1 == TRUE & overlapping_samples_df2 == TRUE & reference_medians == FALSE ~ "Subset normalization will be performed!", # nolint
      df1 == TRUE & df2 == TRUE & overlapping_samples_df1 == TRUE & overlapping_samples_df2 == TRUE & reference_medians == TRUE ~ "Subset normalization will be performed!", # nolint
      TRUE ~ NA_character_,
      .default = NA_character_
    ),
    norm_mode = dplyr::case_when(
      df1 == FALSE ~ NA_character_,
      df1 == TRUE & df2 == FALSE & reference_medians == FALSE ~ NA_character_,
      df1 == TRUE & df2 == FALSE & reference_medians == TRUE & overlapping_samples_df1 == FALSE ~ NA_character_, # nolint
      df1 == TRUE & df2 == FALSE & reference_medians == TRUE & overlapping_samples_df1 == TRUE & overlapping_samples_df2 == FALSE ~ olink_norm_modes$ref_median, # nolint
      df1 == TRUE & df2 == FALSE & reference_medians == TRUE & overlapping_samples_df1 == TRUE & overlapping_samples_df2 == TRUE ~ olink_norm_modes$ref_median, # nolint
      df1 == TRUE & df2 == TRUE & overlapping_samples_df1 == FALSE ~ NA_character_, # nolint
      df1 == TRUE & df2 == TRUE & overlapping_samples_df1 == TRUE & overlapping_samples_df2 == FALSE & reference_medians == FALSE ~ olink_norm_modes$bridge, # nolint
      df1 == TRUE & df2 == TRUE & overlapping_samples_df1 == TRUE & overlapping_samples_df2 == FALSE & reference_medians == TRUE ~ olink_norm_modes$bridge, # nolint
      df1 == TRUE & df2 == TRUE & overlapping_samples_df1 == TRUE & overlapping_samples_df2 == TRUE & reference_medians == FALSE ~ olink_norm_modes$subset, # nolint
      df1 == TRUE & df2 == TRUE & overlapping_samples_df1 == TRUE & overlapping_samples_df2 == TRUE & reference_medians == TRUE ~ olink_norm_modes$subset, # nolint
      TRUE ~ NA_character_,
      .default = NA_character_
    )
  ) |>
  dplyr::arrange(
    .data[["df1"]],
    .data[["df2"]],
    .data[["overlapping_samples_df1"]],
    .data[["overlapping_samples_df2"]],
    .data[["reference_medians"]]
  )
