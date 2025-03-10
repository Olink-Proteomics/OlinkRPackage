#based on:
#-https://testthat.r-lib.org/articles/test-fixtures.html
#-https://r-pkgs.org/testing-advanced.html#sec-testing-advanced-concrete-fixture

olink_wide_synthetic_data <- test_path("data",
                                       "synthetic_dt_wide",
                                       "synthetic_dt_wide.R")
source(olink_wide_synthetic_data)

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
  n_samples = rep(x = list(c(88L, 99L)), times = 12L),
  show_dev_int_ctrl = rep(x = list(FALSE, c(TRUE, FALSE), c(TRUE, FALSE)),
                          times = 4L),
  show_int_ctrl = rep(x = list(c(TRUE, FALSE)), times = 12L),
  version = rep(x = list(0L, c(1L, 2L, 3L), 0L),
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
  expand.grid( # nolint return_linter
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
  # Focus has only version 0 on bottom matrix
  dplyr::mutate(
    version = dplyr::if_else(.data[["olink_platform"]] == "Focus"
                             & .data[["data_type"]] == "NPX",
                             .data[["version"]] - 1L,
                             .data[["version"]])
  ) |>
  # remove combinations that are not part of the data
  dplyr::inner_join(
    olink_wide_bottom_matrix |>
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
  olink_wide_synthetic( # nolint return_linter
    olink_platform = df_wide_combos$olink_platform[i],
    data_type = df_wide_combos$data_type[i],
    n_panels = df_wide_combos$n_panels[i],
    n_assays = df_wide_combos$n_assays[i],
    n_samples = df_wide_combos$n_samples[i],
    show_dev_int_ctrl = df_wide_combos$show_dev_int_ctrl[i],
    show_int_ctrl = df_wide_combos$show_int_ctrl[i],
    version = df_wide_combos$version[i]
  )
})

# modify df_wide_combos to shorten file names
df_wide_combos <- df_wide_combos |>
  dplyr::mutate(
    olink_platform = lapply(.data[["olink_platform"]], function(.x) {
      accepted_olink_platforms |> # nolint return_linter
        dplyr::filter(
          .data[["name"]] == .env[[".x"]]
        ) |>
        dplyr::pull(
          .data[["short_name"]]
        )
    }) |>
      unlist(),
    data_type = dplyr::if_else(.data[["data_type"]] == "Quantified",
                               "quant",
                               tolower(.data[["data_type"]])),
    dplyr::across(
      dplyr::starts_with("show"),
      ~ dplyr::if_else(.x == FALSE, "F", "T")
    )
  )

# save each dataset in an rds file
lapply(seq_len(nrow(df_wide_combos)), function(i) {
  df_wide_combos_tmp <- df_wide_combos |>
    dplyr::slice(i)

  # create olink_platform dir
  olink_platform_dir <- file.path(dirname(olink_wide_synthetic_data),
                                  df_wide_combos_tmp$olink_platform[1L])
  if (!dir.exists(olink_platform_dir)) {
    dir.create(olink_platform_dir)
  }

  # create data_type dir
  data_type_dir <- file.path(olink_platform_dir,
                             df_wide_combos_tmp$data_type[1L])
  if (!dir.exists(data_type_dir)) {
    dir.create(data_type_dir)
  }

  # file name
  file_name <- file.path(
    data_type_dir,
    paste0(
      "p=", df_wide_combos_tmp$n_panels[1L], "_",
      "a=", df_wide_combos_tmp$n_assays[1L], "_",
      "s=", df_wide_combos_tmp$n_samples[1L], "_",
      "ic=", df_wide_combos_tmp$show_int_ctrl[1L], "_",
      "dic=", df_wide_combos_tmp$show_dev_int_ctrl[1L], "_",
      "v=", df_wide_combos_tmp$version[1L],
      ".rds"
    )
  )
  saveRDS(object = df_wide_combos_tmp$df[1L][[1L]], # nolint return_linter
          file = file_name,
          version = 2L,
          compress = "gzip")
})
