test_that(
  "olink_boxplot - works",
  {
    skip_on_cran()
    skip_if_not_installed("ggplot2", minimum_version = "3.4.0")

    npx_data_format221010 <- get_example_data(
      filename = "npx_data_format-Oct-2022.rds"
    )
    npx_check <- check_npx(df = npx_data_format221010) |>
      suppressWarnings() |>
      suppressMessages()

    expect_no_error(
      object = expect_no_warning(
        object = expect_message(
          object = boxplot_npxcheck <- olink_boxplot(
            df = npx_data_format221010,
            variable = "treatment2",
            olinkid_list = c(npx_check$assay_na[1L:5L], "OID30538"),
            check_log = npx_check
          ),
          regexp = paste("2530 entries removed by `clean_npx()` from the input",
                         "dataset `df`. Run `clean_npx()` on your dataset with",
                         "`verbose = TRUE` to inspect which rows were removed"),
          fixed = TRUE
        )
      )
    )

    expect_length(
      object = unique(boxplot_npxcheck[[1L]]$data$Name_OID),
      n = 1L
    )
  }
)

test_that(
  "olink_boxplot - works - vdiffr",
  {
    skip_on_cran()
    skip_if_not_installed("ggplot2", minimum_version = "3.4.0")
    skip_if_not_installed("vdiffr")

    ref_results <- get_example_data(
      filename = "reference_results.rds"
    )

    npx_data1_check <- check_npx(df = npx_data1) |>
      suppressWarnings() |>
      suppressMessages()

    npx_data1_clean <- clean_npx(df = npx_data1,
                                 check_log = npx_data1_check) |>
      suppressMessages() |>
      suppressWarnings()

    npx_data1_clean_check <- check_npx(df = npx_data1_clean) |>
      suppressWarnings() |>
      suppressMessages()

    npx_data1_clean <- strip_check_log(df = npx_data1_clean)

    # ---- 2 proteins ----

    expect_no_error(
      object = expect_no_warning(
        object = expect_message(
          object = boxplot_site_2prots <- olink_boxplot(
            df = npx_data1_clean,
            variable = "Site",
            olinkid_list = ref_results$anova_site |>
              dplyr::filter(.data[["Threshold"]] == "Significant") |>
              dplyr::slice_head(n = 2L) |>
              dplyr::pull(.data[["OlinkID"]]),
            check_log = npx_data1_clean_check
          ),
          regexp = paste("No sample type column detected in input `df`.",
                         "Control samples may not be filtered out."),
          fixed = TRUE
        )
      )
    )

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

    expect_no_error(
      object = expect_no_warning(
        object = expect_message(
          object = boxplot_site_10prots <- olink_boxplot(
            df = npx_data1_clean,
            variable = "Site",
            olinkid_list = ref_results$anova_site |>
              dplyr::filter(.data[["Threshold"]] == "Significant") |>
              dplyr::slice_head(n = 10L) |>
              dplyr::pull(.data[["OlinkID"]]),
            number_of_proteins_per_plot = 5L,
            check_log = npx_data1_clean_check
          ),
          regexp = paste("No sample type column detected in input `df`.",
                         "Control samples may not be filtered out."),
          fixed = TRUE
        )
      )
    )

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

    expect_no_error(
      object = expect_no_warning(
        object = expect_message(
          object = expect_message(
            object = boxplot_time <- olink_boxplot(
              df = npx_data1,
              variable = "Time",
              olinkid_list = ref_results$anova_time |>
                dplyr::slice_head(n = 10L) |>
                dplyr::pull(.data[["OlinkID"]]),
              check_log = npx_data1_check
            ),
            regexp = paste("736 entries removed by `clean_npx()` from the",
                           "input dataset `df`. Run `clean_npx()` on your",
                           "dataset with `verbose = TRUE` to inspect which",
                           "rows were removed."),
            fixed = TRUE
          ),
          regexp = paste("No sample type column detected in input `df`.",
                         "Control samples may not be filtered out."),
          fixed = TRUE
        )
      )
    )

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

    ref_results$anova_time_posthoc[1L, "Threshold"] <- "Significant"
    ref_results$anova_time_posthoc[1L, "Adjusted_pval"] <- 0.0003

    expect_no_error(
      object = expect_no_warning(
        object = expect_message(
          object = expect_message(
            object = boxplot_time_coloroption <- olink_boxplot(
              df = npx_data1,
              variable = "Time",
              olinkid_list = ref_results$anova_time |>
                dplyr::slice_head(n = 10L) |>
                dplyr::pull(.data[["OlinkID"]]),
              coloroption = c("teal", "pink", "orange", "turquoise"),
              check_log = npx_data1_check
            ),
            regexp = paste("736 entries removed by `clean_npx()` from the",
                           "input dataset `df`. Run `clean_npx()` on your",
                           "dataset with `verbose = TRUE` to inspect which",
                           "rows were removed."),
            fixed = TRUE
          ),
          regexp = paste("No sample type column detected in input `df`.",
                         "Control samples may not be filtered out."),
          fixed = TRUE
        )
      )
    )

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

    expect_no_error(
      object = expect_no_warning(
        object = expect_message(
          object = boxplot_time_site <- olink_boxplot(
            df = npx_data1_clean,
            variable = c("Time", "Site"),
            olinkid_list = ref_results$anova_time |>
              dplyr::slice_head(n = 10L) |>
              dplyr::pull(.data[["OlinkID"]]),
            check_log = npx_data1_clean_check
          ),
          regexp = paste("No sample type column detected in input `df`.",
                         "Control samples may not be filtered out."),
          fixed = TRUE
        )
      )
    )

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

    ref_results$anova_time_posthoc[1:6, "Threshold"] <- "Significant"
    ref_results$anova_time_posthoc[1:4, "Adjusted_pval"] <- 0.0003
    ref_results$anova_time_posthoc[5, "Adjusted_pval"] <- 0.049
    ref_results$anova_time_posthoc[6, "Adjusted_pval"] <- 0.01

    expect_no_error(
      object = expect_no_warning(
        object = expect_message(
          object = expect_message(
            object = boxplot_time_posthoc <- olink_boxplot(
              df = npx_data1,
              variable = "Time",
              olinkid_list = ref_results$anova_time |>
                dplyr::slice_head(n = 10L) |>
                dplyr::pull(.data[["OlinkID"]]),
              posthoc_results = ref_results$anova_time_posthoc,
              check_log = npx_data1_check
            ),
            regexp = paste("736 entries removed by `clean_npx()` from the",
                           "input dataset `df`. Run `clean_npx()` on your",
                           "dataset with `verbose = TRUE` to inspect which",
                           "rows were removed."),
            fixed = TRUE
          ),
          regexp = paste("No sample type column detected in input `df`.",
                         "Control samples may not be filtered out."),
          fixed = TRUE
        )
      )
    )

    boxplot_time_posthoc_name <- "boxplot time and posthoc"
    check_snap_exist(
      test_dir_name = "olink_boxplot",
      snap_name = boxplot_time_posthoc_name
    )
    vdiffr::expect_doppelganger(
      boxplot_time_posthoc_name,
      boxplot_time_posthoc[[1L]]
    )

    # ---- Treatment + ttest ----

    expect_no_error(
      object = expect_no_warning(
        object = expect_message(
          object = expect_message(
            object = boxplot_treatment_ttest <- olink_boxplot(
              df = npx_data1,
              variable = "Treatment",
              olinkid_list = ref_results$t_test |>
                dplyr::slice_head(n = 10L) |>
                dplyr::pull(.data[["OlinkID"]]),
              ttest_result = ref_results$t_test,
              check_log = npx_data1_check
            ),
            regexp = paste("736 entries removed by `clean_npx()` from the",
                           "input dataset `df`. Run `clean_npx()` on your",
                           "dataset with `verbose = TRUE` to inspect which",
                           "rows were removed."),
            fixed = TRUE
          ),
          regexp = paste("No sample type column detected in input `df`.",
                         "Control samples may not be filtered out."),
          fixed = TRUE
        )
      )
    )

    boxplot_treatment_ttest_name <- "boxplot treatment and ttest"
    check_snap_exist(
      test_dir_name = "olink_boxplot",
      snap_name = boxplot_treatment_ttest_name
    )
    vdiffr::expect_doppelganger(
      boxplot_treatment_ttest_name,
      boxplot_treatment_ttest[[2L]]
    )
  }
)

test_that(
  "olink_boxplot - works - olink_class and arrow",
  {
    skip_on_cran()
    skip_if_not_installed("ggplot2", minimum_version = "3.4.0")

    ref_results <- get_example_data(
      filename = "reference_results.rds"
    )

    npx_data1_check <- check_npx(df = npx_data1) |>
      suppressWarnings() |>
      suppressMessages()

    npx_data1_clean <- clean_npx(df = npx_data1,
                                 check_log = npx_data1_check) |>
      suppressMessages() |>
      suppressWarnings()

    npx_data1_clean_check <- check_npx(df = npx_data1_clean) |>
      suppressWarnings() |>
      suppressMessages()

    npx_data1_clean <- strip_check_log(df = npx_data1_clean)

    # tibble ----

    ## ---- 2 proteins ----

    boxplot_site_2prots <- olink_boxplot(
      df = npx_data1_clean,
      variable = "Site",
      olinkid_list = ref_results$anova_site |>
        dplyr::filter(.data[["Threshold"]] == "Significant") |>
        dplyr::slice_head(n = 2L) |>
        dplyr::pull(.data[["OlinkID"]]),
      check_log = npx_data1_clean_check
    ) |>
      suppressMessages()

    ## ---- 10 proteins ----

    boxplot_site_10prots <- olink_boxplot(
      df = npx_data1_clean,
      variable = "Site",
      olinkid_list = ref_results$anova_site |>
        dplyr::filter(.data[["Threshold"]] == "Significant") |>
        dplyr::slice_head(n = 10L) |>
        dplyr::pull(.data[["OlinkID"]]),
      number_of_proteins_per_plot = 5L,
      check_log = npx_data1_clean_check
    ) |>
      suppressMessages()

    ## ---- Time ----

    boxplot_time <- olink_boxplot(
      df = npx_data1,
      variable = "Time",
      olinkid_list = ref_results$anova_time |>
        dplyr::slice_head(n = 10L) |>
        dplyr::pull(.data[["OlinkID"]]),
      check_log = npx_data1_check
    ) |>
      suppressMessages()

    ## ---- Time w/ coloroption ----

    boxplot_time_coloroption <- olink_boxplot(
      df = npx_data1,
      variable = "Time",
      olinkid_list = ref_results$anova_time |>
        dplyr::slice_head(n = 10L) |>
        dplyr::pull(.data[["OlinkID"]]),
      coloroption = c("teal", "pink", "orange", "turquoise"),
      check_log = npx_data1_check
    ) |>
      suppressMessages()

    ## ---- Time + Site ----

    boxplot_time_site <- olink_boxplot(
      df = npx_data1_clean,
      variable = c("Time", "Site"),
      olinkid_list = ref_results$anova_time |>
        dplyr::slice_head(n = 10L) |>
        dplyr::pull(.data[["OlinkID"]]),
      check_log = npx_data1_clean_check
    ) |>
      suppressMessages()

    ## ---- Time + Posthoc ----

    boxplot_time_posthoc <- olink_boxplot(
      df = npx_data1,
      variable = "Time",
      olinkid_list = ref_results$anova_time |>
        dplyr::slice_head(n = 10L) |>
        dplyr::pull(.data[["OlinkID"]]),
      posthoc_results = ref_results$anova_time_posthoc,
      check_log = npx_data1_check
    ) |>
      suppressMessages()

    ## ---- Treatment + ttest ----

    boxplot_treatment_ttest <- olink_boxplot(
      df = npx_data1,
      variable = "Treatment",
      olinkid_list = ref_results$t_test |>
        dplyr::slice_head(n = 10L) |>
        dplyr::pull(.data[["OlinkID"]]),
      ttest_result = ref_results$t_test,
      check_log = npx_data1_check
    ) |>
      suppressMessages()

    # olink_class ----

    npx_data1_obj <- attach_check_log(
      df = npx_data1,
      out_df = "tibble",
      preferred_names = NULL
    ) |>
      suppressWarnings()

    npx_data1_clean_obj <- attach_check_log(
      df = npx_data1_clean,
      out_df = "tibble",
      preferred_names = NULL
    )

    ## ---- 2 proteins ----

    boxplot_site_2prots_obj <- olink_boxplot(
      df = npx_data1_clean_obj,
      variable = "Site",
      olinkid_list = ref_results$anova_site |>
        dplyr::filter(.data[["Threshold"]] == "Significant") |>
        dplyr::slice_head(n = 2L) |>
        dplyr::pull(.data[["OlinkID"]])
    ) |>
      suppressMessages()

    expect_equal_ggplot(
      object = boxplot_site_2prots_obj[[1L]],
      expected = boxplot_site_2prots[[1L]]
    )

    ## ---- 10 proteins ----

    boxplot_site_10prots_obj <- olink_boxplot(
      df = npx_data1_clean_obj,
      variable = "Site",
      olinkid_list = ref_results$anova_site |>
        dplyr::filter(.data[["Threshold"]] == "Significant") |>
        dplyr::slice_head(n = 10L) |>
        dplyr::pull(.data[["OlinkID"]]),
      number_of_proteins_per_plot = 5L
    ) |>
      suppressMessages()

    expect_equal_ggplot(
      object = boxplot_site_10prots_obj[[2L]],
      expected = boxplot_site_10prots[[2L]]
    )

    ## ---- Time ----

    boxplot_time_obj <- olink_boxplot(
      df = npx_data1_obj,
      variable = "Time",
      olinkid_list = ref_results$anova_time |>
        dplyr::slice_head(n = 10L) |>
        dplyr::pull(.data[["OlinkID"]])
    ) |>
      suppressMessages()

    expect_equal_ggplot(
      object = boxplot_time_obj[[2L]],
      expected = boxplot_time[[2L]]
    )

    ## ---- Time w/ coloroption ----

    boxplot_time_coloroption_obj <- olink_boxplot(
      df = npx_data1_obj,
      variable = "Time",
      olinkid_list = ref_results$anova_time |>
        dplyr::slice_head(n = 10L) |>
        dplyr::pull(.data[["OlinkID"]]),
      coloroption = c("teal", "pink", "orange", "turquoise")
    ) |>
      suppressMessages()

    expect_equal_ggplot(
      object = boxplot_time_coloroption_obj[[2L]],
      expected = boxplot_time_coloroption[[2L]]
    )

    ## ---- Time + Site ----

    boxplot_time_site_obj <- olink_boxplot(
      df = npx_data1_clean_obj,
      variable = c("Time", "Site"),
      olinkid_list = ref_results$anova_time |>
        dplyr::slice_head(n = 10L) |>
        dplyr::pull(.data[["OlinkID"]])
    ) |>
      suppressMessages()

    expect_equal_ggplot(
      object = boxplot_time_site_obj[[2L]],
      expected = boxplot_time_site[[2L]]
    )

    ## ---- Time + Posthoc ----

    boxplot_time_posthoc_obj <- olink_boxplot(
      df = npx_data1_obj,
      variable = "Time",
      olinkid_list = ref_results$anova_time |>
        dplyr::slice_head(n = 10L) |>
        dplyr::pull(.data[["OlinkID"]]),
      posthoc_results = ref_results$anova_time_posthoc
    ) |>
      suppressMessages()

    expect_equal_ggplot(
      object = boxplot_time_posthoc_obj[[1L]],
      expected = boxplot_time_posthoc[[1L]]
    )

    ## ---- Treatment + ttest ----

    boxplot_treatment_ttest_obj <- olink_boxplot(
      df = npx_data1_obj,
      variable = "Treatment",
      olinkid_list = ref_results$t_test |>
        dplyr::slice_head(n = 10L) |>
        dplyr::pull(.data[["OlinkID"]]),
      ttest_result = ref_results$t_test
    ) |>
      suppressMessages()

    expect_equal_ggplot(
      object = boxplot_treatment_ttest_obj[[2L]],
      expected = boxplot_treatment_ttest[[2L]]
    )

    # arrow ----

    npx_data1_arrow <- attach_check_log(
      df = npx_data1,
      out_df = "arrow",
      preferred_names = NULL
    ) |>
      suppressWarnings()

    npx_data1_clean_arrow <- attach_check_log(
      df = npx_data1_clean,
      out_df = "arrow",
      preferred_names = NULL
    )

    ## ---- 2 proteins ----

    boxplot_site_2prots_arrow <- olink_boxplot(
      df = npx_data1_clean_arrow,
      variable = "Site",
      olinkid_list = ref_results$anova_site |>
        dplyr::filter(.data[["Threshold"]] == "Significant") |>
        dplyr::slice_head(n = 2L) |>
        dplyr::pull(.data[["OlinkID"]])
    ) |>
      suppressMessages()

    expect_equal_ggplot(
      object = boxplot_site_2prots_arrow[[1L]],
      expected = boxplot_site_2prots[[1L]]
    )

    ## ---- 10 proteins ----

    boxplot_site_10prots_arrow <- olink_boxplot(
      df = npx_data1_clean_arrow,
      variable = "Site",
      olinkid_list = ref_results$anova_site |>
        dplyr::filter(.data[["Threshold"]] == "Significant") |>
        dplyr::slice_head(n = 10L) |>
        dplyr::pull(.data[["OlinkID"]]),
      number_of_proteins_per_plot = 5L
    ) |>
      suppressMessages()

    expect_equal_ggplot(
      object = boxplot_site_10prots_arrow[[2L]],
      expected = boxplot_site_10prots[[2L]]
    )

    ## ---- Time ----

    boxplot_time_arrow <- olink_boxplot(
      df = npx_data1_arrow,
      variable = "Time",
      olinkid_list = ref_results$anova_time |>
        dplyr::slice_head(n = 10L) |>
        dplyr::pull(.data[["OlinkID"]])
    ) |>
      suppressMessages()

    expect_equal_ggplot(
      object = boxplot_time_arrow[[2L]],
      expected = boxplot_time[[2L]]
    )

    ## ---- Time w/ coloroption ----

    boxplot_time_coloroption_arrow <- olink_boxplot(
      df = npx_data1_arrow,
      variable = "Time",
      olinkid_list = ref_results$anova_time |>
        dplyr::slice_head(n = 10L) |>
        dplyr::pull(.data[["OlinkID"]]),
      coloroption = c("teal", "pink", "orange", "turquoise")
    ) |>
      suppressMessages()

    expect_equal_ggplot(
      object = boxplot_time_coloroption_arrow[[2L]],
      expected = boxplot_time_coloroption[[2L]]
    )

    ## ---- Time + Site ----

    boxplot_time_site_arrow <- olink_boxplot(
      df = npx_data1_clean_arrow,
      variable = c("Time", "Site"),
      olinkid_list = ref_results$anova_time |>
        dplyr::slice_head(n = 10L) |>
        dplyr::pull(.data[["OlinkID"]])
    ) |>
      suppressMessages()

    expect_equal_ggplot(
      object = boxplot_time_site_arrow[[2L]],
      expected = boxplot_time_site[[2L]]
    )

    ## ---- Time + Posthoc ----

    boxplot_time_posthoc_arrow <- olink_boxplot(
      df = npx_data1_arrow,
      variable = "Time",
      olinkid_list = ref_results$anova_time |>
        dplyr::slice_head(n = 10L) |>
        dplyr::pull(.data[["OlinkID"]]),
      posthoc_results = ref_results$anova_time_posthoc
    ) |>
      suppressMessages()

    expect_equal_ggplot(
      object = boxplot_time_posthoc_arrow[[1L]],
      expected = boxplot_time_posthoc[[1L]]
    )

    ## ---- Treatment + ttest ----

    boxplot_treatment_ttest_arrow <- olink_boxplot(
      df = npx_data1_arrow,
      variable = "Treatment",
      olinkid_list = ref_results$t_test |>
        dplyr::slice_head(n = 10L) |>
        dplyr::pull(.data[["OlinkID"]]),
      ttest_result = ref_results$t_test
    ) |>
      suppressMessages()

    expect_equal_ggplot(
      object = boxplot_treatment_ttest_arrow[[2L]],
      expected = boxplot_treatment_ttest[[2L]]
    )

  }
)
