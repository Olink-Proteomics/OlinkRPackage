# Test olink_lmer_plot ----

test_that(
  "olink_lmer_plot - works - 6 assays, 1 page",
  {
    skip_on_cran()
    skip_if_not_installed("vdiffr")
    skip_if_not_installed(pkg = "lme4")
    skip_if_not_installed(pkg = "lmerTest")
    skip_if_not_installed(pkg = "broom")
    skip_if_not_installed(pkg = "emmeans")

    # Load reference results
    # tests are skipped if files are absent
    reference_results <- get_example_data(filename = "reference_results.rds")

    npx_data1_check <- check_npx(df = npx_data1) |>
      suppressMessages() |>
      suppressWarnings()

    expect_no_error(
      object = expect_no_warning(
        object = expect_no_message(
          object = lmer_plot <- olink_lmer_plot(
            df = npx_data1,
            check_log = npx_data1_check,
            variable = c("Treatment", "Time"),
            random = "Subject",
            olinkid_list = reference_results$lmer |>
              dplyr::filter(
                .data[["term"]] == "Treatment:Time"
                & .data[["Threshold"]] == "Significant"
              ) |>
              dplyr::pull(
                .data[["OlinkID"]]
              ) |>
              utils::head(6L),
            x_axis_variable = "Time",
            col_variable = "Treatment"
          )
        )
      )
    )

    expect_type(object = lmer_plot, type = "list")
    expect_length(object = lmer_plot, n = 1L)
    expect_s3_class(object = lmer_plot[[1L]], class = "ggplot")

    lmer_plot_name <- "lmer_plot_6_assays"
    check_snap_exist(test_dir_name = "plot_point_range",
                     snap_name = lmer_plot_name)
    vdiffr::expect_doppelganger(title = lmer_plot_name,
                                fig = lmer_plot[[1L]])
  }
)

test_that(
  "olink_lmer_plot - works - 10 assays, 2 pages",
  {
    skip_on_cran()
    skip_if_not_installed("vdiffr")
    skip_if_not_installed(pkg = "lme4")
    skip_if_not_installed(pkg = "lmerTest")
    skip_if_not_installed(pkg = "broom")
    skip_if_not_installed(pkg = "emmeans")

    # Load reference results
    # tests are skipped if files are absent
    reference_results <- get_example_data(filename = "reference_results.rds")

    npx_data1_check <- check_npx(df = npx_data1) |>
      suppressMessages() |>
      suppressWarnings()

    expect_no_error(
      object = expect_no_warning(
        object = expect_no_message(
          object = lmer_plot <- olink_lmer_plot(
            df = npx_data1,
            check_log = npx_data1_check,
            variable = c("Treatment", "Time"),
            random = "Subject",
            olinkid_list = reference_results$lmer |>
              dplyr::filter(
                .data[["term"]] == "Treatment:Time"
                & .data[["Threshold"]] == "Significant"
              ) |>
              dplyr::pull(
                .data[["OlinkID"]]
              ) |>
              utils::head(10L),
            x_axis_variable = "Time",
            col_variable = "Treatment",
            number_of_proteins_per_plot = 5L
          )
        )
      )
    )

    expect_type(object = lmer_plot, type = "list")
    expect_length(object = lmer_plot, n = 2L)
    expect_s3_class(object = lmer_plot[[1L]], class = "ggplot")
    expect_s3_class(object = lmer_plot[[2L]], class = "ggplot")

    lmer_plot_name <- "lmer_plot_10_assays"
    check_snap_exist(test_dir_name = "plot_point_range",
                     snap_name = lmer_plot_name)
    vdiffr::expect_doppelganger(title = lmer_plot_name,
                                fig = lmer_plot[[2L]])
  }
)

test_that(
  "olink_lmer_plot - works - no check_log, 6 assays, 1 page",
  {
    skip_on_cran()
    skip_if_not_installed("vdiffr")
    skip_if_not_installed(pkg = "lme4")
    skip_if_not_installed(pkg = "lmerTest")
    skip_if_not_installed(pkg = "broom")
    skip_if_not_installed(pkg = "emmeans")

    # Load reference results
    # tests are skipped if files are absent
    reference_results <- get_example_data(filename = "reference_results.rds")

    expect_no_error(
      object = expect_no_warning(
        object = expect_message(
          lmer_plot <- olink_lmer_plot(
            df = npx_data1,
            variable = c("Treatment", "Time"),
            random = "Subject",
            olinkid_list = reference_results$lmer |>
              dplyr::filter(
                .data[["term"]] == "Treatment:Time"
                & .data[["Threshold"]] == "Significant"
              ) |>
              dplyr::pull(
                .data[["OlinkID"]]
              ) |>
              utils::head(6L),
            x_axis_variable = "Time",
            col_variable = "Treatment"
          ),
          regexp = "`check_log` not provided. Running `check_npx()`.",
          fixed = TRUE
        )
      )
    )

    expect_type(object = lmer_plot, type = "list")
    expect_length(object = lmer_plot, n = 1L)
    expect_s3_class(object = lmer_plot[[1L]], class = "ggplot")
  }
)

test_that(
  "olink_lmer_plot - works - edge case, assays with missing values only",
  {
    skip_on_cran()
    skip_if_not_installed(pkg = "lme4")
    skip_if_not_installed(pkg = "lmerTest")
    skip_if_not_installed(pkg = "broom")
    skip_if_not_installed(pkg = "emmeans")

    # data with all NPX=NA for some assays
    dt_edge_case <- get_example_data(filename = "npx_data_format-Oct-2022.rds")
    dt_edge_case_check <- check_npx(df = dt_edge_case) |>
      suppressMessages() |>
      suppressWarnings()

    expect_no_error(
      object = expect_no_warning(
        object = expect_no_message(
          object = lmer_plot <- olink_lmer_plot(
            df = dt_edge_case,
            check_log = dt_edge_case_check,
            variable = c("treatment1"),
            random = "SubjectDummy",
            olinkid_list = c(
              utils::head(x = dt_edge_case_check$assay_na, n = 5L),
              "OID30538"
            ),
            x_axis_variable = "treatment1",
            number_of_proteins_per_plot = 5L
          )
        )
      )
    )

    expect_type(object = lmer_plot, type = "list")
    expect_length(object = lmer_plot, n = 1L)
    expect_s3_class(object = lmer_plot[[1L]], class = "ggplot")
    expect_identical(
      object = unique(lmer_plot[[1L]]$data$OlinkID),
      expected = c("OID30538")
    )
  }
)
