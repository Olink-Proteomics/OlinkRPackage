# modes that olink normalization functions may have
olink_norm_modes <- list(
  "bridge" = "bridge",
  "subset" = "subset",
  "ref_median" = "ref_median",
  "norm_cross_product" = "norm_cross_product"
)

# pre-populated dataset with column names and classes that the reference medians
# input dataset may have
olink_norm_ref_median_cols <- dplyr::tibble(
  cols = c("OlinkID", "Reference_NPX"),
  class = c("character", "numeric"),
  name = c("olink_id", "ref_med")
)

olink_norm_product_n_samples <- dplyr::tibble(
  product_1 = c("E3072", "HT", "E3072", "Reveal", "HT", "Reveal"),
  product_2 = c("HT", "E3072", "Reveal", "E3072", "Reveal", "HT"),
  num_samples = c(40L, 40L, 32L, 32L, 24L, 24L),
  ref = list(
    list("HT"),
    list("HT"),
    list("Reveal"),
    list("Reveal"),
    list("HT", "Reveal"),
    list("HT", "Reveal")
  )
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
      .data[["df1"]] == FALSE ~ "Required {.arg df1} is missing!",
      .data[["df1"]] == TRUE &
        .data[["df2"]] == FALSE &
        .data[["reference_medians"]] == FALSE ~ "When {.arg df1} is provided, either {.arg df2} or {.arg reference_medians} is required!", # nolint: line_length_linter
      .data[["df1"]] == TRUE &
        .data[["df2"]] == FALSE &
        .data[["reference_medians"]] == TRUE &
        .data[["overlapping_samples_df1"]] == FALSE ~ "When {.arg df1} and {.arg reference_medians} are provided, {.arg overlapping_samples_df1} is required!", # nolint: line_length_linter
      .data[["df1"]] == TRUE &
        .data[["df2"]] == FALSE &
        .data[["reference_medians"]] == TRUE &
        .data[["overlapping_samples_df1"]] == TRUE &
        .data[["overlapping_samples_df2"]] == FALSE ~ NA_character_,
      .data[["df1"]] == TRUE &
        .data[["df2"]] == FALSE &
        .data[["reference_medians"]] == TRUE &
        .data[["overlapping_samples_df1"]] == TRUE &
        .data[["overlapping_samples_df2"]] == TRUE ~ NA_character_,
      .data[["df1"]] == TRUE &
        .data[["df2"]] == TRUE &
        .data[["overlapping_samples_df1"]] == FALSE ~ "When {.arg df1} and {.arg df2} are provided, at least {.arg overlapping_samples_df1} is required!", # nolint: line_length_linter
      .data[["df1"]] == TRUE &
        .data[["df2"]] == TRUE &
        .data[["overlapping_samples_df1"]] == TRUE &
        .data[["overlapping_samples_df2"]] == FALSE &
        .data[["reference_medians"]] == FALSE ~ NA_character_,
      .data[["df1"]] == TRUE &
        .data[["df2"]] == TRUE &
        .data[["overlapping_samples_df1"]] == TRUE &
        .data[["overlapping_samples_df2"]] == FALSE &
        .data[["reference_medians"]] == TRUE ~ NA_character_,
      .data[["df1"]] == TRUE &
        .data[["df2"]] == TRUE &
        .data[["overlapping_samples_df1"]] == TRUE &
        .data[["overlapping_samples_df2"]] == TRUE &
        .data[["reference_medians"]] == FALSE ~ NA_character_,
      .data[["df1"]] == TRUE &
        .data[["df2"]] == TRUE &
        .data[["overlapping_samples_df1"]] == TRUE &
        .data[["overlapping_samples_df2"]] == TRUE &
        .data[["reference_medians"]] == TRUE ~ NA_character_,
      TRUE ~ NA_character_,
      .default = NA_character_
    ),
    warning_msg = dplyr::case_when(
      .data[["df1"]] == FALSE ~ NA_character_,
      .data[["df1"]] == TRUE &
        .data[["df2"]] == FALSE &
        .data[["reference_medians"]] == FALSE ~ NA_character_,
      .data[["df1"]] == TRUE &
        .data[["df2"]] == FALSE &
        .data[["reference_medians"]] == TRUE &
        .data[["overlapping_samples_df1"]] == FALSE ~ NA_character_,
      .data[["df1"]] == TRUE &
        .data[["df2"]] == FALSE &
        .data[["reference_medians"]] == TRUE &
        .data[["overlapping_samples_df1"]] == TRUE &
        .data[["overlapping_samples_df2"]] == FALSE ~ NA_character_,
      .data[["df1"]] == TRUE &
        .data[["df2"]] == FALSE &
        .data[["reference_medians"]] == TRUE &
        .data[["overlapping_samples_df1"]] == TRUE &
        .data[["overlapping_samples_df2"]] == TRUE ~ "{.arg overlapping_samples_df2} will be ignored", # nolint: line_length_linter
      .data[["df1"]] == TRUE &
        .data[["df2"]] == TRUE &
        .data[["overlapping_samples_df1"]] == FALSE ~ NA_character_,
      .data[["df1"]] == TRUE &
        .data[["df2"]] == TRUE &
        .data[["overlapping_samples_df1"]] == TRUE &
        .data[["overlapping_samples_df2"]] == FALSE &
        .data[["reference_medians"]] == FALSE ~ NA_character_,
      .data[["df1"]] == TRUE &
        .data[["df2"]] == TRUE &
        .data[["overlapping_samples_df1"]] == TRUE &
        .data[["overlapping_samples_df2"]] == FALSE &
        .data[["reference_medians"]] == TRUE ~ "{.arg reference_medians} will be ignored", # nolint: line_length_linter
      .data[["df1"]] == TRUE &
        .data[["df2"]] == TRUE &
        .data[["overlapping_samples_df1"]] == TRUE &
        .data[["overlapping_samples_df2"]] == TRUE &
        .data[["reference_medians"]] == FALSE ~ NA_character_,
      .data[["df1"]] == TRUE &
        .data[["df2"]] == TRUE &
        .data[["overlapping_samples_df1"]] == TRUE &
        .data[["overlapping_samples_df2"]] == TRUE &
        .data[["reference_medians"]] == TRUE ~ "{.arg reference_medians} will be ignored", # nolint: line_length_linter
      TRUE ~ NA_character_,
      .default = NA_character_
    ),
    inform_msg = dplyr::case_when(
      .data[["df1"]] == FALSE ~ NA_character_,
      .data[["df1"]] == TRUE &
        .data[["df2"]] == FALSE &
        .data[["reference_medians"]] == FALSE ~ NA_character_,
      .data[["df1"]] == TRUE &
        .data[["df2"]] == FALSE &
        .data[["reference_medians"]] == TRUE &
        .data[["overlapping_samples_df1"]] == FALSE ~ NA_character_,
      .data[["df1"]] == TRUE &
        .data[["df2"]] == FALSE &
        .data[["reference_medians"]] == TRUE &
        .data[["overlapping_samples_df1"]] == TRUE &
        .data[["overlapping_samples_df2"]] == FALSE ~ "Reference median normalization will be performed!", # nolint: line_length_linter
      .data[["df1"]] == TRUE &
        .data[["df2"]] == FALSE &
        .data[["reference_medians"]] == TRUE &
        .data[["overlapping_samples_df1"]] == TRUE &
        .data[["overlapping_samples_df2"]] == TRUE ~ "Reference median normalization will be performed!", # nolint: line_length_linter
      .data[["df1"]] == TRUE &
        .data[["df2"]] == TRUE &
        .data[["overlapping_samples_df1"]] == FALSE ~ NA_character_,
      .data[["df1"]] == TRUE &
        .data[["df2"]] == TRUE &
        .data[["overlapping_samples_df1"]] == TRUE &
        .data[["overlapping_samples_df2"]] == FALSE &
        .data[["reference_medians"]] == FALSE ~ "Bridge normalization will be performed!", # nolint: line_length_linter
      .data[["df1"]] == TRUE &
        .data[["df2"]] == TRUE &
        .data[["overlapping_samples_df1"]] == TRUE &
        .data[["overlapping_samples_df2"]] == FALSE &
        .data[["reference_medians"]] == TRUE ~ "Bridge normalization will be performed!", # nolint: line_length_linter
      .data[["df1"]] == TRUE &
        .data[["df2"]] == TRUE &
        .data[["overlapping_samples_df1"]] == TRUE &
        .data[["overlapping_samples_df2"]] == TRUE &
        .data[["reference_medians"]] == FALSE ~ "Subset normalization will be performed!", # nolint: line_length_linter
      .data[["df1"]] == TRUE &
        .data[["df2"]] == TRUE &
        .data[["overlapping_samples_df1"]] == TRUE &
        .data[["overlapping_samples_df2"]] == TRUE &
        .data[["reference_medians"]] == TRUE ~ "Subset normalization will be performed!", # nolint: line_length_linter
      TRUE ~ NA_character_,
      .default = NA_character_
    ),
    norm_mode = dplyr::case_when(
      .data[["df1"]] == FALSE ~ NA_character_,
      .data[["df1"]] == TRUE &
        .data[["df2"]] == FALSE &
        .data[["reference_medians"]] == FALSE ~ NA_character_,
      .data[["df1"]] == TRUE &
        .data[["df2"]] == FALSE &
        .data[["reference_medians"]] == TRUE &
        .data[["overlapping_samples_df1"]] == FALSE ~ NA_character_,
      .data[["df1"]] == TRUE &
        .data[["df2"]] == FALSE &
        .data[["reference_medians"]] == TRUE &
        .data[["overlapping_samples_df1"]] == TRUE &
        .data[["overlapping_samples_df2"]] == FALSE ~ olink_norm_modes$ref_median, # nolint: line_length_linter
      .data[["df1"]] == TRUE &
        .data[["df2"]] == FALSE &
        .data[["reference_medians"]] == TRUE &
        .data[["overlapping_samples_df1"]] == TRUE &
        .data[["overlapping_samples_df2"]] == TRUE ~ olink_norm_modes$ref_median, # nolint: line_length_linter
      .data[["df1"]] == TRUE &
        .data[["df2"]] == TRUE &
        .data[["overlapping_samples_df1"]] == FALSE ~ NA_character_,
      .data[["df1"]] == TRUE &
        .data[["df2"]] == TRUE &
        .data[["overlapping_samples_df1"]] == TRUE &
        .data[["overlapping_samples_df2"]] == FALSE &
        .data[["reference_medians"]] == FALSE ~ olink_norm_modes$bridge,
      .data[["df1"]] == TRUE &
        .data[["df2"]] == TRUE &
        .data[["overlapping_samples_df1"]] == TRUE &
        .data[["overlapping_samples_df2"]] == FALSE &
        .data[["reference_medians"]] == TRUE ~ olink_norm_modes$bridge,
      .data[["df1"]] == TRUE &
        .data[["df2"]] == TRUE &
        .data[["overlapping_samples_df1"]] == TRUE &
        .data[["overlapping_samples_df2"]] == TRUE &
        .data[["reference_medians"]] == FALSE ~ olink_norm_modes$subset,
      .data[["df1"]] == TRUE &
        .data[["df2"]] == TRUE &
        .data[["overlapping_samples_df1"]] == TRUE &
        .data[["overlapping_samples_df2"]] == TRUE &
        .data[["reference_medians"]] == TRUE ~ olink_norm_modes$subset,
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
