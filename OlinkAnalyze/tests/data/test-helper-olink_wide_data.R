## Generate sets of all possible wide matrix combinations ----

df_wide_summary <- dplyr::tibble(
  olink_platform = c(rep(x = "Target 96", times = 3L),
                     rep(x = "Target 48", times = 3L),
                     rep(x = "Flex", times = 3L),
                     rep(x = "Focus", times = 3L)),
  data_type = rep(x = c("Ct", "NPX", "Quantified"), times = 4L),
  n_panels = rep(x = list(c(1L, 3L)), times = 12L),
  n_assays = c(rep(x = 92L, times = 3L),
               rep(x = 45L, times = 3L),
               rep(x = 20L, times = 3L),
               rep(x = 33L, times = 3L)),
  n_samples = rep(x = list(c(88L, 100L)), times = 12L),
  show_dev_int_ctrl = rep(x = list(FALSE, c(TRUE, FALSE), c(TRUE, FALSE)),
                          times = 4L),
  show_int_ctrl = rep(x = list(c(TRUE, FALSE)), times = 12L),
  version = rep(x = list(0L, c(1L, 2L), 0L),
                times = 4L)
) |>
  # remove platform - data_type combos that do not apply
  dplyr::inner_join(
    accepted_olink_platforms |>
      dplyr::filter(
        .data[["broader_platform"]] == "qPCR"
      ) |>
      dplyr::select(
        dplyr::all_of(c("name", "quant_method"))
      ) |>
      tidyr::unnest_longer(
        dplyr::all_of("quant_method")
      ),
    by = c("olink_platform" = "name",
           "data_type" = "quant_method")
  )

# all combination of parameters for synthetic wide data
df_wide_combos <- lapply(seq_len(nrow(df_wide_summary)), function(i) {
  expand.grid(
    olink_platform = unlist(df_wide_summary$olink_platform[i]),
    data_type = unlist(df_wide_summary$data_type[i]),
    n_panels = unlist(df_wide_summary$n_panels[i]),
    n_assays = unlist(df_wide_summary$n_assays[i]),
    n_samples = unlist(df_wide_summary$n_samples[i]),
    show_dev_int_ctrl = unlist(df_wide_summary$show_dev_int_ctrl[i]),
    show_int_ctrl = unlist(df_wide_summary$show_int_ctrl[i]),
    version = unlist(df_wide_summary$version[i])
  ) |>
    dplyr::as_tibble()
}) |>
  dplyr::bind_rows() |>
  # remove combinations that are not part of the data
  dplyr::inner_join(
    olink_wide_excel_bottom_matrix |>
      dplyr::select(
        dplyr::all_of(c("olink_platform", "version"))
      ) |>
      dplyr::distinct(),
    by = c("olink_platform", "version")
  ) |>
  dplyr::mutate(
    dplyr::across(
      where(is.factor), ~ as.character(.x)
    ),
    dplyr::across(
      where(is.numeric), ~ as.integer(.x)
    )
  )

# generate a dataset for each combination
df_wide_combos$df <- lapply(seq_len(nrow(df_wide_combos)), function(i) {
  olink_wide_synthetic(olink_platform = df_wide_combos$olink_platform[i],
                       data_type = df_wide_combos$data_type[i],
                       n_panels = df_wide_combos$n_panels[i],
                       n_assays = df_wide_combos$n_assays[i],
                       n_samples = df_wide_combos$n_samples[i],
                       show_dev_int_ctrl = df_wide_combos$show_dev_int_ctrl[i],
                       show_int_ctrl = df_wide_combos$show_int_ctrl[i],
                       version = df_wide_combos$version[i])
})
