test_that(
  "olink_one_non_parametric - works - match reference results",
  {
    # load reference results
    ref_results <- get_example_data(filename = "reference_results.rds")

    # npx_data1 check_log
    check_log <- check_npx(npx_data1) |>
      suppressMessages() |>
      suppressWarnings()

    # get data without control
    npx_data1_noctrl <- clean_control_sample_id(
      df = npx_data1,
      check_log = check_log,
      control_sample_ids = c("CONTROL_SAMPLE_AS 1", "CONTROL_SAMPLE_AS 2")
    ) |>
      suppressMessages() |>
      suppressWarnings()

    check_log_noctrl <- check_npx(npx_data1) |>
      suppressMessages() |>
      suppressWarnings()

    # ---- test kruskal with reference file ----
    skip_if_not_installed("FSA")
    expect_message(
      object = expect_message(
        object = kruskal_results <- olink_one_non_parametric(
          df = npx_data1_noctrl,
          check_log = check_log_noctrl,
          variable = "Site"
        ),
        regexp = "Variables converted from character to factors: \"Site\"",
        fixed = TRUE
      ),
      regexp = "Kruskal model fit to each assay: `NPX~Site`",
      fixed = TRUE
    )

    expect_equal(
      object = kruskal_results,
      expected = ref_results$kruskal
    )

    expect_equal(
      object = nrow(kruskal_results),
      expected = 184
    )

    expect_equal(
      object = ncol(kruskal_results),
      expected = 11
    )

    # ---- test posthoc test for the results from Kruskal-Wallis test ----
    sig_oids <- kruskal_results |>
      dplyr::filter(Threshold == 'Significant') |>
      dplyr::select(OlinkID) |>
      dplyr::distinct() |>
      dplyr::pull()

    expect_message(
      object = expect_message(
        object = kruskal_posthoc_results <- olink_one_non_parametric_posthoc(
          df = npx_data1_noctrl,
          check_log = check_log_noctrl,
          variable = "Site",
          test = "kruskal",
          olinkid_list = sig_oids
        ) |>
          dplyr::mutate(id = as.character(OlinkID)) |>
          dplyr::arrange(id, contrast) |>  # for consistency.
          dplyr::select(-id),
        regexp = "Variables converted from character to factors: \"Site\"",
        fixed = TRUE
      ),
      regexp = paste0(
      "Pairwise comparisons for Kruskal-Wallis test using ",
      "Dunn test were performed"
      ),
      fixed = TRUE
    )

    expect_equal(
      object = kruskal_posthoc_results,
      expected = ref_results$kruskal_posthoc
      )

    expect_equal(
      object = nrow(kruskal_posthoc_results),
      expected = 190
      )

    expect_equal(
      object = kruskal_posthoc_results |>
        dplyr::select(contrast) |>
        unique() |>
        nrow(),
      expected = 10
      )

    # ---- test friedman with reference file ----
    expect_message(
      object = expect_message(
        object = friedman_results <- olink_one_non_parametric(
          df = npx_data1_noctrl,
          check_log = check_log_noctrl,
          variable = "Time",
          subject = "Subject",
          dependence = TRUE
        ),
        regexp = "Variables converted from character to factors: \"Time\"",
        fixed = TRUE
      ),
      regexp = "Friedman model fit to each assay: `NPX~Time`",
      fixed = TRUE
    )

    expect_equal(
      object = friedman_results,
      expected = ref_results$friedman
    )

    expect_equal(
      object = nrow(friedman_results),
      expected = 184
    )
    expect_equal(
      object = ncol(friedman_results),
      expected = 11
    )

    # ---- test posthoc test for the results from Friedman test ----
    sig_oids <- friedman_results |>
      dplyr::filter(Threshold == 'Significant') |>
      dplyr::select(OlinkID) |>
      dplyr::distinct() |>
      dplyr::pull()

    expect_message(
      object = expect_message(
        object = friedman_posthoc_results <- olink_one_non_parametric_posthoc(
          df = npx_data1_noctrl,
          check_log = check_log_noctrl,
          variable = "Time",
          test = "friedman",
          subject = "Subject",
          olinkid_list = sig_oids
        ) |>
          dplyr::mutate(id = as.character(OlinkID)) |>
          dplyr::arrange(id, contrast) |>  # for consistency.
          dplyr::select(-id),
        regexp = "Variables converted from character to factors: \"Time\"",
        fixed = TRUE
      ),
      regexp = paste0(
        "Pairwise comparisons for Friedman test using ",
        "paired Wilcoxon signed-rank test were performed"
      ),
      fixed = TRUE
    )

    expect_equal(
      object = friedman_posthoc_results,
      expected = ref_results$friedman_posthoc
      )

    expect_equal(
      object = nrow(friedman_posthoc_results),
      expected = 3
      )

    expect_equal(
      object = friedman_posthoc_results |>
                   dplyr::select(contrast) |>
                   unique() %>%
                   nrow(),
      expected = 3
      )
  }
)

test_that(
  "olink_one_non_parametric - error - missing input",
  {
    # npx_data1 check_log
    check_log <- check_npx(npx_data1) |>
      suppressMessages() |>
      suppressWarnings()

    # test for no input data or variable
    expect_error(
      object = olink_one_non_parametric(df = NULL),
      regexp = "`df` and `variable` must be specified."
    )

    expect_error(
      object = olink_one_non_parametric(df = npx_data1,
                                        check_log = check_log),
      regexp = "`df` and `variable` must be specified."
    )

  }
)
