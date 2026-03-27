# Test reference results from reference_results.rds against refResults.RData

testthat::test_that(
  "reference results match",
  {
    # environments to avoid name conflicts ----

    # new version
    env_new_v <- new.env()
    # get data if available, otherwise skip the test
    lst_new_v <- get_example_data("reference_results.rds")
    assign(x = "lst_new_v", value = lst_new_v, envir = env_new_v)
    rm(lst_new_v)

    # legacy version
    env_legacy_v <- new.env()
    file_legacy_v <- test_path("data", "refResults.RData")
    skip_if_not(file.exists(file_legacy_v))
    load(file = file_legacy_v, envir = env_legacy_v)

    # matching names between reference datasets
    match_names <- dplyr::tribble(
      ~legacy_v,                                   ~new_v,
      "t.test_results",                            "t_test",
      "t.test_results_paired",                     "t_test_paired",
      "wilcox.test_results",                       "wilcox_test",
      "wilcox.test_results_paired",                "wilcox_test_paired",
      "kruskal_results",                           "kruskal",
      "kruskal_posthoc_results",                   "kruskal_posthoc",
      "friedman_results",                          "friedman",
      "friedman_posthoc_results",                  "friedman_posthoc",
      "ordinalRegression_results",                 "ordinal_regression",
      "ordinalRegression_results_posthoc_results", "ordinal_regression_posthoc",
      "anova_results_1_site",                      "anova_site",
      "anova_posthoc_1_site",                      "anova_site_posthoc",
      "anova_results_1_time",                      "anova_time",
      "anova_posthoc_1_time",                      "anova_time_posthoc",
      "anova_results_1_siteTime",                  "anova_site_time",
      "lmer_results_1",                            "lmer",
      "lmer_results_1_posthoc",                    "lmer_posthoc",
      "normalization_results.bridged",             "normalization_bridge",
      "normalization_results.intensity",           "normalization_intensity",
      "normalization_results.subset",              "normalization_subset",
      "normalization_results.multi",               "normalization_multibatch",
      "randomized_result1",                        "randomized_samples",
      "randomized_result2",                        "randomized_subjects",
      "randomized_result3",                        "randomized_subjects_spots",
      "randomized_result4",                        "randomized_samples_spots",
      "procData",                                  "preprocessing_dim_red",
      "procData_missingData",                      "preprocessing_dim_red_miss",
      "bridge_samples_npx_data1",                  "bridge_samples_npx_data1",
      "bridge_samples_npx_data2",                  "bridge_samples_npx_data2"
    )

    # read NPX ----

    # This is tested within the tests for read_npx. Nothing to check here.

    # t-test ----

    # unpaired t-test
    row_n <- 1L
    expect_identical(
      object = match_names |>
        dplyr::slice(
          .env[["row_n"]]
        ) |>
        dplyr::pull(
          .data[["new_v"]]
        ),
      expected = "t_test"
    )
    expect_equal(
      object = env_new_v$lst_new_v[[match_names$new_v[row_n]]],
      expected = env_legacy_v$ref_results[[match_names$legacy_v[row_n]]]
    )

    # paired t-test
    row_n <- 2L
    expect_identical(
      object = match_names |>
        dplyr::slice(
          .env[["row_n"]]
        ) |>
        dplyr::pull(
          .data[["new_v"]]
        ),
      expected = "t_test_paired"
    )
    testthat::expect_equal(
      object = env_new_v$lst_new_v[[match_names$new_v[row_n]]],
      expected = env_legacy_v$ref_results[[match_names$legacy_v[row_n]]]
    )

    # Mann-Whitney U Test ----

    # unpaired Mann-Whitney U Test
    row_n <- 3L
    expect_identical(
      object = match_names |>
        dplyr::slice(
          .env[["row_n"]]
        ) |>
        dplyr::pull(
          .data[["new_v"]]
        ),
      expected = "wilcox_test"
    )
    testthat::expect_equal(
      object = env_new_v$lst_new_v[[match_names$new_v[row_n]]],
      expected = env_legacy_v$ref_results[[match_names$legacy_v[row_n]]]
    )

    # paired Mann-Whitney U Test
    row_n <- 4L
    expect_identical(
      object = match_names |>
        dplyr::slice(
          .env[["row_n"]]
        ) |>
        dplyr::pull(
          .data[["new_v"]]
        ),
      expected = "wilcox_test_paired"
    )
    testthat::expect_equal(
      object = env_new_v$lst_new_v[[match_names$new_v[row_n]]],
      expected = env_legacy_v$ref_results[[match_names$legacy_v[row_n]]]
    )

    # One-way non-parametric test ----

    ## Kruskal-Wallis test ----

    # One-way Kruskal-Wallis test
    row_n <- 5L
    expect_identical(
      object = match_names |>
        dplyr::slice(
          .env[["row_n"]]
        ) |>
        dplyr::pull(
          .data[["new_v"]]
        ),
      expected = "kruskal"
    )
    testthat::expect_equal(
      object = env_new_v$lst_new_v[[match_names$new_v[row_n]]],
      expected = env_legacy_v$ref_results[[match_names$legacy_v[row_n]]]
    )

    # Posthoc test for the results from Kruskal-Wallis test
    row_n <- 6L
    expect_identical(
      object = match_names |>
        dplyr::slice(
          .env[["row_n"]]
        ) |>
        dplyr::pull(
          .data[["new_v"]]
        ),
      expected = "kruskal_posthoc"
    )
    testthat::expect_equal(
      object = env_new_v$lst_new_v[[match_names$new_v[row_n]]],
      expected = env_legacy_v$ref_results[[match_names$legacy_v[row_n]]]
    )

    ## Friedman test ----

    # One-way Friedman Test
    row_n <- 7L
    expect_identical(
      object = match_names |>
        dplyr::slice(
          .env[["row_n"]]
        ) |>
        dplyr::pull(
          .data[["new_v"]]
        ),
      expected = "friedman"
    )
    testthat::expect_equal(
      object = env_new_v$lst_new_v[[match_names$new_v[row_n]]],
      expected = env_legacy_v$ref_results[[match_names$legacy_v[row_n]]]
    )

    # Posthoc test for the results from Friedman test
    row_n <- 8L
    expect_identical(
      object = match_names |>
        dplyr::slice(
          .env[["row_n"]]
        ) |>
        dplyr::pull(
          .data[["new_v"]]
        ),
      expected = "friedman_posthoc"
    )
    testthat::expect_equal(
      object = env_new_v$lst_new_v[[match_names$new_v[row_n]]],
      expected = env_legacy_v$ref_results[[match_names$legacy_v[row_n]]]
    )

    # Ordinal regression ----

    # Two-way Ordinal Regression with CLM
    row_n <- 9L
    expect_identical(
      object = match_names |>
        dplyr::slice(
          .env[["row_n"]]
        ) |>
        dplyr::pull(
          .data[["new_v"]]
        ),
      expected = "ordinal_regression"
    )
    testthat::expect_equal(
      object = env_new_v$lst_new_v[[match_names$new_v[row_n]]],
      expected = env_legacy_v$ref_results[[match_names$legacy_v[row_n]]]
    )

    # Posthoc test for the results from Two-way Ordinal Regression with CLM
    row_n <- 10L
    expect_identical(
      object = match_names |>
        dplyr::slice(
          .env[["row_n"]]
        ) |>
        dplyr::pull(
          .data[["new_v"]]
        ),
      expected = "ordinal_regression_posthoc"
    )
    testthat::expect_equal(
      object = env_new_v$lst_new_v[[match_names$new_v[row_n]]],
      expected = env_legacy_v$ref_results[[match_names$legacy_v[row_n]]]
    )

    # ANOVA ----

    ## ANOVA - site ----

    # ANOVA
    row_n <- 11L
    expect_identical(
      object = match_names |>
        dplyr::slice(
          .env[["row_n"]]
        ) |>
        dplyr::pull(
          .data[["new_v"]]
        ),
      expected = "anova_site"
    )
    testthat::expect_equal(
      object = env_new_v$lst_new_v[[match_names$new_v[row_n]]],
      expected = env_legacy_v$ref_results[[match_names$legacy_v[row_n]]]
    )

    # Posthoc ANOVA
    row_n <- 12L
    expect_identical(
      object = match_names |>
        dplyr::slice(
          .env[["row_n"]]
        ) |>
        dplyr::pull(
          .data[["new_v"]]
        ),
      expected = "anova_site_posthoc"
    )
    testthat::expect_equal(
      object = env_new_v$lst_new_v[[match_names$new_v[row_n]]],
      expected = env_legacy_v$ref_results[[match_names$legacy_v[row_n]]]
    )

    ## ANOVA - time ----

    # ANOVA
    row_n <- 13L
    expect_identical(
      object = match_names |>
        dplyr::slice(
          .env[["row_n"]]
        ) |>
        dplyr::pull(
          .data[["new_v"]]
        ),
      expected = "anova_time"
    )
    testthat::expect_equal(
      object = env_new_v$lst_new_v[[match_names$new_v[row_n]]],
      expected = env_legacy_v$ref_results[[match_names$legacy_v[row_n]]]
    )

    # Posthoc ANOVA
    row_n <- 14L
    expect_identical(
      object = match_names |>
        dplyr::slice(
          .env[["row_n"]]
        ) |>
        dplyr::pull(
          .data[["new_v"]]
        ),
      expected = "anova_time_posthoc"
    )
    testthat::expect_equal(
      object = env_new_v$lst_new_v[[match_names$new_v[row_n]]],
      expected = env_legacy_v$ref_results[[match_names$legacy_v[row_n]]]
    )

    ## ANOVA - site*time ----

    # ANOVA
    row_n <- 15L
    expect_identical(
      object = match_names |>
        dplyr::slice(
          .env[["row_n"]]
        ) |>
        dplyr::pull(
          .data[["new_v"]]
        ),
      expected = "anova_site_time"
    )
    testthat::expect_equal(
      object = env_new_v$lst_new_v[[match_names$new_v[row_n]]],
      expected = env_legacy_v$ref_results[[match_names$legacy_v[row_n]]]
    )

    # LMER ----

    # lmer
    row_n <- 16L
    expect_identical(
      object = match_names |>
        dplyr::slice(
          .env[["row_n"]]
        ) |>
        dplyr::pull(
          .data[["new_v"]]
        ),
      expected = "lmer"
    )
    testthat::expect_equal(
      object = env_new_v$lst_new_v[[match_names$new_v[row_n]]],
      expected = env_legacy_v$ref_results[[match_names$legacy_v[row_n]]]
    )

    # lmer posthoc
    row_n <- 17L
    expect_identical(
      object = match_names |>
        dplyr::slice(
          .env[["row_n"]]
        ) |>
        dplyr::pull(
          .data[["new_v"]]
        ),
      expected = "lmer_posthoc"
    )
    testthat::expect_equal(
      object = env_new_v$lst_new_v[[match_names$new_v[row_n]]],
      expected = env_legacy_v$ref_results[[match_names$legacy_v[row_n]]]
    )

    # Olink normalization ----

    ## Bridge normalization ----

    row_n <- 18L
    expect_identical(
      object = match_names |>
        dplyr::slice(
          .env[["row_n"]]
        ) |>
        dplyr::pull(
          .data[["new_v"]]
        ),
      expected = "normalization_bridge"
    )
    testthat::expect_equal(
      object = env_new_v$lst_new_v[[match_names$new_v[row_n]]],
      expected = env_legacy_v$ref_results[[match_names$legacy_v[row_n]]]
    )

    ## Intensity normalization ----

    row_n <- 19L
    expect_identical(
      object = match_names |>
        dplyr::slice(
          .env[["row_n"]]
        ) |>
        dplyr::pull(
          .data[["new_v"]]
        ),
      expected = "normalization_intensity"
    )
    testthat::expect_equal(
      object = env_new_v$lst_new_v[[match_names$new_v[row_n]]],
      expected = env_legacy_v$ref_results[[match_names$legacy_v[row_n]]]
    )

    ## Subset normalization ----

    row_n <- 20L
    expect_identical(
      object = match_names |>
        dplyr::slice(
          .env[["row_n"]]
        ) |>
        dplyr::pull(
          .data[["new_v"]]
        ),
      expected = "normalization_subset"
    )
    testthat::expect_equal(
      object = env_new_v$lst_new_v[[match_names$new_v[row_n]]],
      expected = env_legacy_v$ref_results[[match_names$legacy_v[row_n]]]
    )

    ## Multi-batch normalization ----

    row_n <- 21L
    expect_identical(
      object = match_names |>
        dplyr::slice(
          .env[["row_n"]]
        ) |>
        dplyr::pull(
          .data[["new_v"]]
        ),
      expected = "normalization_multibatch"
    )
    testthat::expect_equal(
      object = env_new_v$lst_new_v[[match_names$new_v[row_n]]],
      expected = env_legacy_v$ref_results[[match_names$legacy_v[row_n]]]
    )

    # Plate randomization ----

    row_n <- 22L
    expect_identical(
      object = match_names |>
        dplyr::slice(
          .env[["row_n"]]
        ) |>
        dplyr::pull(
          .data[["new_v"]]
        ),
      expected = "randomized_samples"
    )
    testthat::expect_equal(
      object = env_new_v$lst_new_v[[match_names$new_v[row_n]]],
      expected = env_legacy_v$ref_results[[match_names$legacy_v[row_n]]]
    )

    row_n <- 23L
    expect_identical(
      object = match_names |>
        dplyr::slice(
          .env[["row_n"]]
        ) |>
        dplyr::pull(
          .data[["new_v"]]
        ),
      expected = "randomized_subjects"
    )
    testthat::expect_equal(
      object = env_new_v$lst_new_v[[match_names$new_v[row_n]]],
      expected = env_legacy_v$ref_results[[match_names$legacy_v[row_n]]]
    )

    row_n <- 24L
    expect_identical(
      object = match_names |>
        dplyr::slice(
          .env[["row_n"]]
        ) |>
        dplyr::pull(
          .data[["new_v"]]
        ),
      expected = "randomized_subjects_spots"
    )
    testthat::expect_equal(
      object = env_new_v$lst_new_v[[match_names$new_v[row_n]]],
      expected = env_legacy_v$ref_results[[match_names$legacy_v[row_n]]]
    )

    row_n <- 25L
    expect_identical(
      object = match_names |>
        dplyr::slice(
          .env[["row_n"]]
        ) |>
        dplyr::pull(
          .data[["new_v"]]
        ),
      expected = "randomized_samples_spots"
    )
    testthat::expect_equal(
      object = env_new_v$lst_new_v[[match_names$new_v[row_n]]],
      expected = env_legacy_v$ref_results[[match_names$legacy_v[row_n]]]
    )

    # NPX pre-processing for dimansionality reduction ----

    ## Pre-processing without missing data ----

    row_n <- 26L
    expect_identical(
      object = match_names |>
        dplyr::slice(
          .env[["row_n"]]
        ) |>
        dplyr::pull(
          .data[["new_v"]]
        ),
      expected = "preprocessing_dim_red"
    )
    testthat::expect_equal(
      object = env_new_v$lst_new_v[[match_names$new_v[row_n]]],
      expected = env_legacy_v$ref_results[[match_names$legacy_v[row_n]]]$df_wide
    )

    ## Pre-processing with missing data ----

    row_n <- 27L
    expect_identical(
      object = match_names |>
        dplyr::slice(
          .env[["row_n"]]
        ) |>
        dplyr::pull(
          .data[["new_v"]]
        ),
      expected = "preprocessing_dim_red_miss"
    )
    testthat::expect_equal(
      object = env_new_v$lst_new_v[[match_names$new_v[row_n]]],
      expected = env_legacy_v$ref_results[[match_names$legacy_v[row_n]]]$df_wide
    )

    # Bridge sample selection ----

    ## Bridge sample selection npx_data1 ----

    row_n <- 28L
    expect_identical(
      object = match_names |>
        dplyr::slice(
          .env[["row_n"]]
        ) |>
        dplyr::pull(
          .data[["new_v"]]
        ),
      expected = "bridge_samples_npx_data1"
    )
    testthat::expect_equal(
      object = env_new_v$lst_new_v[[match_names$new_v[row_n]]],
      expected = env_legacy_v$ref_results[[match_names$legacy_v[row_n]]]
    )

    ## Bridge sample selection npx_data2 ----

    row_n <- 29L
    expect_identical(
      object = match_names |>
        dplyr::slice(
          .env[["row_n"]]
        ) |>
        dplyr::pull(
          .data[["new_v"]]
        ),
      expected = "bridge_samples_npx_data2"
    )
    testthat::expect_equal(
      object = env_new_v$lst_new_v[[match_names$new_v[row_n]]],
      expected = env_legacy_v$ref_results[[match_names$legacy_v[row_n]]]
    )
  }
)

testthat::test_that(
  "reference manifest matches",
  {
    # legacy version
    env_legacy_v <- new.env()
    file_legacy_v <- test_path("..", "..", "data-raw", "ref_manifest.rds")
    skip_if_not(file.exists(file_legacy_v))
    env_legacy_v$manifest <- readRDS(file = file_legacy_v)

    testthat::expect_identical(
      object = manifest,
      expected = env_legacy_v$manifest
    )
  }
)

testthat::test_that(
  "reference npx_data1 matches",
  {
    # legacy version
    env_legacy_v <- new.env()
    file_legacy_v <- test_path("..", "..", "data-raw", "ref_npx_data1.rds")
    skip_if_not(file.exists(file_legacy_v))
    env_legacy_v$npx_data1 <- readRDS(file = file_legacy_v)

    testthat::expect_identical(
      object = npx_data1,
      expected = env_legacy_v$npx_data1
    )
  }
)

testthat::test_that(
  "reference npx_data2 matches",
  {
    # legacy version
    env_legacy_v <- new.env()
    file_legacy_v <- test_path("..", "..", "data-raw", "ref_npx_data2.rds")
    skip_if_not(file.exists(file_legacy_v))
    env_legacy_v$npx_data2 <- readRDS(file = file_legacy_v)

    testthat::expect_identical(
      object = npx_data2,
      expected = env_legacy_v$npx_data2
    )
  }
)
