skip_on_cran()
skip_if_not_installed("ggplot2", minimum_version = "3.4.0")

# -------------------------------------------------------------------------
# Load reference results and NPX example data
# -------------------------------------------------------------------------

ref_results <- get_example_data(
  filename = "reference_results.rds"
)

npx_data_format221010 <- get_example_data(
  filename = "npx_data_format-Oct-2022.rds"
)
npx_check <- check_npx(npx_data_format221010) |>
  suppressWarnings() |>
  suppressMessages()
# -------------------------------------------------------------------------
# Create plots for testing
# -------------------------------------------------------------------------
npx_data1_check <- check_npx(df = npx_data1) |>
  suppressWarnings() |>
  suppressMessages()

boxplot_site_2prots <- npx_data1 |>
  na.omit() |>
  olink_boxplot(
    variable = "Site",
    olinkid_list = ref_results$anova_site |>
      dplyr::filter(Threshold == "Significant") |>
      head(2) |>
      dplyr::pull(OlinkID),
    check_log = npx_data1_check
  ) |>
  suppressMessages()

boxplot_site_10prots <- npx_data1 |>
  na.omit() |>
  olink_boxplot(
    variable = "Site",
    olinkid_list = ref_results$anova_site |>
      dplyr::filter(Threshold == "Significant") |>
      head(10) |>
      dplyr::pull(OlinkID),
    number_of_proteins_per_plot = 5L,
    check_log = npx_data1_check
  ) |>
  suppressMessages()

boxplot_time <- npx_data1 |>
  olink_boxplot(
    variable = "Time",
    olinkid_list = ref_results$anova_time |>
      head(10) |>
      dplyr::pull(OlinkID),
    check_log = npx_data1_check
  ) |>
  suppressMessages()

ref_results$anova_time_posthoc[1, "Threshold"] <- "Significant"
ref_results$anova_time_posthoc[1, "Adjusted_pval"] <- 0.0003

boxplot_time_posthoc <- npx_data1 |>
  olink_boxplot(
    variable = "Time",
    olinkid_list = ref_results$anova_time |>
      head(10) |>
      dplyr::pull(OlinkID),
    posthoc_results = ref_results$anova_time_posthoc,
    check_log = npx_data1_check
  ) |>
  suppressMessages()

boxplot_time_coloroption <- npx_data1 |>
  olink_boxplot(
    variable = "Time",
    olinkid_list = ref_results$anova_time |>
      head(10) |>
      dplyr::pull(OlinkID),
    coloroption = c("teal", "pink", "orange", "turqoise"),
    check_log = npx_data1_check
  ) |>
  suppressMessages()

boxplot_time_site <- npx_data1 |>
  na.omit() |>
  olink_boxplot(
    variable = c("Time", "Site"),
    olinkid_list = ref_results$anova_time |>
      head(10) |>
      dplyr::pull(OlinkID),
    check_log = npx_data1_check
  ) |>
  suppressMessages()

boxplot_treatment_ttest <- npx_data1 |>
  olink_boxplot(
    variable = "Treatment",
    olinkid_list = ref_results$t_test |>
      head(10) |>
      dplyr::pull(OlinkID),
    ttest_result = ref_results$t_test,
    check_log = npx_data1_check
  ) |>
  suppressMessages()

# -------------------------------------------------------------------------
# Tests
# -------------------------------------------------------------------------

test_that("olink_boxplot works", {
  expect_message(
    object = expect_message(
      object = expect_warning(
        npx_data_format221010 |>
          olink_boxplot(
            variable = "treatment2",
            olinkid_list = c(
              npx_check$assay_na[1:5],
              "OID30538"
            )
          )
      ),
      regexp = "check_log` not provided. Running `check_npx()`",
      fixed = TRUE
    ),
    regexp = paste("8 assays exhibited assay QC warnings in column",
                   "`Assay_Warning` of the dataset")
  )

  expect_message(
    object = expect_message(
      object = boxplot_npxcheck <- suppressWarnings(
        olink_boxplot(
          df = npx_data_format221010,
          variable = "treatment2",
          olinkid_list = c(
            npx_check$assay_na[1:5],
            "OID30538"
          )
        )
      ),
      regexp = "check_log` not provided. Running `check_npx()`",
      fixed = TRUE
    ),
    regexp = paste("8 assays exhibited assay QC warnings in column",
                   "`Assay_Warning` of the dataset")
  )
  expect_length(
    unique(boxplot_npxcheck[[1]]$data$Name_OID),
    1
  )
})

# -------------------------------------------------------------------------
# Visual regression: vdiffr tests
# -------------------------------------------------------------------------

test_that("olink_boxplot works - vdiffr", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  # ---- 2 proteins ----
  boxplot_site_2prots_name <- "boxplot site 2prots"
  check_snap_exist(
    test_dir_name = "olink_boxplot",
    snap_name = boxplot_site_2prots_name
  )
  vdiffr::expect_doppelganger(
    boxplot_site_2prots_name,
    boxplot_site_2prots[[1L]]
  )
  # ---- 10 proteins ----
  boxplot_site_10prots_name <- "boxplot site 10prots"
  check_snap_exist(
    test_dir_name = "olink_boxplot",
    snap_name = boxplot_site_10prots_name
  )
  vdiffr::expect_doppelganger(
    boxplot_site_10prots_name,
    boxplot_site_10prots[[2L]]
  )
  # ---- Time ----
  boxplot_time_name <- "boxplot time"
  check_snap_exist(
    test_dir_name = "olink_boxplot",
    snap_name = boxplot_time_name
  )
  vdiffr::expect_doppelganger(
    boxplot_time_name,
    boxplot_time[[2L]]
  )
  # ---- Time w/ coloroption ----
  boxplot_time_coloroption_name <- "boxplot time with coloroption"
  check_snap_exist(
    test_dir_name = "olink_boxplot",
    snap_name = boxplot_time_coloroption_name
  )
  vdiffr::expect_doppelganger(
    boxplot_time_coloroption_name,
    boxplot_time_coloroption[[2L]]
  )
  # ---- Time + Site ----
  boxplot_time_site_name <- "boxplot time and site"
  check_snap_exist(
    test_dir_name = "olink_boxplot",
    snap_name = boxplot_time_site_name
  )
  vdiffr::expect_doppelganger(
    boxplot_time_site_name,
    boxplot_time_site[[2L]]
  )
  # ---- Time + Posthoc ----
  boxplot_time_posthoc_name <- "boxplot time and posthoc"
  check_snap_exist(
    test_dir_name = "olink_boxplot",
    snap_name = boxplot_time_posthoc_name
  )
  vdiffr::expect_doppelganger(
    boxplot_time_posthoc_name,
    boxplot_time_posthoc[[2L]]
  )
  # ---- Treatment + ttest ----
  boxplot_treatment_ttest_name <- "boxplot treatment and ttest"
  check_snap_exist(
    test_dir_name = "olink_boxplot",
    snap_name = boxplot_treatment_ttest_name
  )
  vdiffr::expect_doppelganger(
    boxplot_treatment_ttest_name,
    boxplot_treatment_ttest[[2L]]
  )
})
