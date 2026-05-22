# Test olink_lmer ----

test_that(
  "olink_lmer - works - reference",
  {
    skip_if_not_installed(pkg = "lme4") |> suppressPackageStartupMessages()
    skip_if_not_installed(pkg = "lmerTest")
    skip_if_not_installed(pkg = "broom")
    skip_on_cran()

    # Load reference results
    # tests are skipped if files are absent
    reference_results <- get_example_data(filename = "reference_results.rds")

    npx_data1_check <- check_npx(df = npx_data1) |>
      suppressMessages() |>
      suppressWarnings()

    expect_no_error(
      object = expect_no_warning(
        object = expect_message(
          object = expect_message(
            object = expect_message(
              object = lmer_result <- olink_lmer(
                df = npx_data1,
                check_log = npx_data1_check,
                variable = c("Treatment", "Time"),
                random = "Subject"
              ),
              regexp = paste("Samples removed due to missing variable or",
                             "covariate levels: CONTROL_SAMPLE_AS 1,",
                             "CONTROL_SAMPLE_AS 2")
            ),
            regexp = paste("Variables and covariates converted from character",
                           "to factors: Treatment, Time, Subject")
          ),
          regexp = paste("Linear mixed effects model fit to each",
                         "assay:NPX~Treatment*Time+(1|Subject)"),
          fixed = TRUE
        )
      )
    )

    lmer_result <- lmer_result |>
      dplyr::mutate(
        id = as.character(.data[["OlinkID"]])
      ) |>
      # Since OlinkID is not unique here (=> ties), term is used to break ties
      dplyr::arrange(
        .data[["id"]], .data[["term"]]
      ) |>
      dplyr::select(
        -dplyr::all_of("id")
      )

    expect_equal(
      object = lmer_result,
      expected = reference_results$lmer,
      tolerance = 1e-4
    )
  }
)

test_that(
  "olink_lmer - works - assays with all NA",
  {
    skip_if_not_installed(pkg = "lme4") |> suppressPackageStartupMessages()
    skip_if_not_installed(pkg = "lmerTest")
    skip_if_not_installed(pkg = "broom")
    skip_on_cran()

    # data with all NPX=NA for some assays
    dt_edge_case <- get_example_data(filename = "npx_data_format-Oct-2022.rds")
    dt_edge_case_check <- check_npx(df = dt_edge_case) |>
      suppressMessages() |>
      suppressWarnings()

    expect_no_error(
      object = expect_no_warning(
        object = expect_message(
          object = expect_message(
            object = olink_lmer(
              df = dt_edge_case,
              check_log = dt_edge_case_check,
              variable = "treatment1",
              random = "SubjectDummy"
            ),
            regexp = paste("Variables and covariates converted from character",
                           "to factors: treatment1, SubjectDummy")
          ),
          regexp = paste("Linear mixed effects model fit to each",
                         "assay:NPX~treatment1+(1|SubjectDummy)"),
          fixed = TRUE
        )
      )
    )
  }
)

test_that(
  "olink_lmer - works - column 'Index' is missing",
  {
    skip_if_not_installed(pkg = "lme4") |> suppressPackageStartupMessages()
    skip_if_not_installed(pkg = "lmerTest")
    skip_if_not_installed(pkg = "broom")
    skip_on_cran()

    npx_data1_mod <- npx_data1 |>
      dplyr::select(
        -dplyr::all_of("Index")
      )

    npx_data1_mod_check = check_npx(df = npx_data1_mod) |>
      suppressMessages() |>
      suppressWarnings()

    expect_no_error(
      object = expect_no_warning(
        object = expect_message(
          object = expect_message(
            object = expect_message(
              object = olink_lmer(
                df = npx_data1_mod,
                check_log = npx_data1_mod_check,
                variable = c("Treatment", "Time"),
                random = "Subject"
              ),
              regexp = paste("Samples removed due to missing variable or",
                             "covariate levels: CONTROL_SAMPLE_AS 1,",
                             "CONTROL_SAMPLE_AS 2")
            ),
            regexp = paste("Variables and covariates converted from character",
                           "to factors: Treatment, Time, Subject")
          ),
          regexp = paste("Linear mixed effects model fit to each",
                         "assay:NPX~Treatment*Time+(1|Subject)"),
          fixed = TRUE
        )
      )
    )
  }
)

test_that(
  "olink_lmer - error - 'df','variable' or 'random ' not provided",
  {
    skip_if_not_installed(pkg = "lme4") |> suppressPackageStartupMessages()
    skip_if_not_installed(pkg = "lmerTest")
    skip_if_not_installed(pkg = "broom")
    skip_on_cran()

    expect_error(
      object = olink_lmer(),
      regexp = "The df and variable and random arguments need to be specified."
    )

    expect_error(
      object = olink_lmer(df = npx_data1),
      regexp = "The df and variable and random arguments need to be specified."
    )

    expect_error(
      object = olink_lmer(variable = "Site"),
      regexp = "The df and variable and random arguments need to be specified."
    )

    expect_error(
      object = olink_lmer(df = npx_data1,
                          variable = "Site"),
      regexp = "The df and variable and random arguments need to be specified."
    )
  }
)

test_that(
  "olink_lmer - covariate provided with model formula",
  {
    skip_if_not_installed(pkg = "lme4") |> suppressPackageStartupMessages()
    skip_if_not_installed(pkg = "lmerTest")
    skip_if_not_installed(pkg = "broom")
    skip_on_cran()

    # Small version of dataset
    oid_list <- npx_data1 |>
      dplyr::distinct(.data[["OlinkID"]]) |>
      utils::head(20) |>
      dplyr::pull()

    npx_data1_small <- npx_data1 |>
      dplyr::filter(.data[["OlinkID"]] %in% .env[["oid_list"]])

    npx_data1_check <- check_npx(df = npx_data1_small) |>
      suppressMessages() |>
      suppressWarnings()

    lmer_result_args <- olink_lmer(
      df = npx_data1_small,
      check_log = npx_data1_check,
      variable = "Treatment",
      random = "Subject",
      covariates = "Site"
    ) |>
      suppressMessages()

    lmer_result_formula <- olink_lmer(
      df = npx_data1_small,
      check_log = npx_data1_check,
      model_formula = "NPX~Treatment+Site+(1|Subject)"
    )|>
      suppressMessages()

    lmer_result_formula_covariate <- olink_lmer(
      df = npx_data1_small,
      check_log = npx_data1_check,
      model_formula = "NPX~Treatment+Site+(1|Subject)",
      covariates = "Site"
    )|>
      suppressMessages()

    # Check that results are the same when covariate arg is included
    expect_equal(
      object = lmer_result_formula_covariate,
      expected = lmer_result_args
    )

    # Check that covariate is included in results when not specified
    expect_true(
      "Site" %in% unique(lmer_result_formula$term)
    )

    # Check that covariate is not included in results when specified
    expect_false(
      "Site" %in% unique(lmer_result_formula_covariate$term)
    )

    # Check that error is thrown when covariate specified does not exist
    expect_error(
      object = expect_message(
        object = expect_message(
          object = expect_message(
            object = lmer_result_error <- olink_lmer(
              df = npx_data1_small,
              check_log = npx_data1_check,
              model_formula = "NPX~Treatment+(1|Subject)",
              covariates = "Site"
            ),
            regexp = paste("Samples removed due to missing variable or",
                           "covariate levels: CONTROL_SAMPLE_AS 1,",
                           "CONTROL_SAMPLE_AS 2")
          ),
          regexp = paste("Variables and covariates converted from character",
                         "to factors: Treatment, Subject")
        ),
        regexp = paste("Linear mixed effects model fit to each",
                       "assay:NPX~Treatment+(1|Subject)"),
        fixed = TRUE
      ),
      regexp = paste("Covariate \"Site\" is not present in the model formula!")
    )

  }
)

# Test olink_lmer_posthoc ----

test_that(
  "olink_lmer_posthoc - works reference",
  {
    skip_if_not_installed(pkg = "lme4") |> suppressPackageStartupMessages()
    skip_if_not_installed(pkg = "lmerTest")
    skip_if_not_installed(pkg = "broom")
    skip_if_not_installed(pkg = "emmeans")
    skip_on_cran()

    # Load reference results
    # tests are skipped if files are absent
    reference_results <- get_example_data(filename = "reference_results.rds")

    npx_data1_check <- check_npx(df = npx_data1) |>
      suppressMessages() |>
      suppressWarnings()

    # olink_lmer result was tested against the referebce, so there is no need
    # to retest it here. Just extract the list of OlinkIDs with significant
    # Treatment:Time interaction from the reference results.
    oid_list <- reference_results$lmer |>
      dplyr::filter(
        .data[["term"]] == "Treatment:Time"
      ) |>
      dplyr::filter(
        .data[["Threshold"]] == "Significant"
      ) |>
      dplyr::pull(
        .data[["OlinkID"]]
      )

    expect_no_error(
      object = expect_no_warning(
        object = expect_message(
          object = expect_message(
            object = expect_message(
              object = lmer_posthoc_result <- olink_lmer_posthoc(
                df = npx_data1,
                check_log = npx_data1_check,
                variable = c("Treatment", "Time"),
                random = "Subject",
                olinkid_list = oid_list,
                effect = c("Treatment", "Time")
              ),
              regexp = paste("Samples removed due to missing variable or",
                             "covariate levels: CONTROL_SAMPLE_AS 1,",
                             "CONTROL_SAMPLE_AS 2")
            ),
            regexp = paste("Variables and covariates converted from character",
                           "to factors: Treatment, Time")
          ),
          regexp = paste("Means estimated for each assay from linear mixed",
                         "effects model: NPX~Treatment*Time+(1|Subject)"),
          fixed = TRUE
        )
      )
    )

    # Run olink_lmer_posthoc
    lmer_posthoc_result <- lmer_posthoc_result |>
      dplyr::mutate(
        id = as.character(.data[["OlinkID"]])
      ) |>
      dplyr::arrange(
        .data[["id"]], .data[["contrast"]]
      ) |>
      # In R 3.6.1 we get factors, but reference is characters
      dplyr::mutate(
        contrast = as.character(.data[["contrast"]])
      ) |>
      dplyr::select(
        -dplyr::all_of("id")
      )

    expect_equal(
      object = lmer_posthoc_result,
      expected = reference_results$lmer_posthoc
    )
  }
)

test_that(
  "olink_lmer_posthoc - works - assays with all NA",
  {
    skip_if_not_installed(pkg = "lme4") |> suppressPackageStartupMessages()
    skip_if_not_installed(pkg = "lmerTest")
    skip_if_not_installed(pkg = "broom")
    skip_if_not_installed(pkg = "emmeans")
    skip_on_cran()

    # data with all NPX=NA for some assays
    dt_edge_case <- get_example_data(filename = "npx_data_format-Oct-2022.rds")
    dt_edge_case_check <- check_npx(df = dt_edge_case) |>
      suppressMessages() |>
      suppressWarnings()

    expect_no_error(
      object = expect_no_warning(
        object = expect_message(
          object = expect_message(
            object = olink_lmer_posthoc(
              df = dt_edge_case,
              check_log = dt_edge_case_check,
              variable = "treatment1",
              effect = "treatment1",
              random = "SubjectDummy"
            ),
            regexp = paste("Variables and covariates converted from character",
                           "to factors: treatment1")
          ),
          regexp = paste("Means estimated for each assay from linear mixed",
                         "effects model: NPX~treatment1+(1|SubjectDummy)"),
          fixed = TRUE
        )
      )
    )
  }
)

test_that(
  "olink_lmer_posthoc - error - 'df','variable' or 'random ' not provided",
  {
    skip_if_not_installed(pkg = "lme4") |> suppressPackageStartupMessages()
    skip_if_not_installed(pkg = "lmerTest")
    skip_if_not_installed(pkg = "broom")
    skip_on_cran()

    expect_error(
      object = olink_lmer_posthoc(),
      regexp = paste("The df, variable, random and effect arguments need to be",
                     "specified.")
    )

    expect_error(
      object = olink_lmer_posthoc(df = npx_data1),
      regexp = paste("The df, variable, random and effect arguments need to be",
                     "specified.")
    )

    expect_error(
      object = olink_lmer_posthoc(variable = "Site"),
      regexp = paste("The df, variable, random and effect arguments need to be",
                     "specified.")
    )

    expect_error(
      object = olink_lmer_posthoc(df = npx_data1,
                                  variable = c("Treatment", "Time"),
                                  random = "Subject"),
      regexp = paste("The df, variable, random and effect arguments need to be",
                     "specified.")
    )
  }
)

test_that(
  "olink_lmer_posthoc - covariate provided with model formula",
  {
    skip_if_not_installed(pkg = "lme4") |> suppressPackageStartupMessages()
    skip_if_not_installed(pkg = "lmerTest")
    skip_if_not_installed(pkg = "broom")
    skip_on_cran()

    # Small version of dataset
    oid_list <- npx_data1 |>
      dplyr::distinct(.data[["OlinkID"]]) |>
      utils::head(20) |>
      dplyr::pull()

    npx_data1_small <- npx_data1 |>
      dplyr::filter(.data[["OlinkID"]] %in% .env[["oid_list"]])

    npx_data1_check <- check_npx(df = npx_data1_small) |>
      suppressMessages() |>
      suppressWarnings()

    lmer_posthoc_result_args <- olink_lmer_posthoc(
      df = npx_data1_small,
      check_log = npx_data1_check,
      variable = "Treatment",
      random = "Subject",
      covariates = "Site",
      effect = "Treatment"
    ) |>
      suppressMessages()

    lmer_posthoc_result_formula <- olink_lmer_posthoc(
      df = npx_data1_small,
      check_log = npx_data1_check,
      model_formula = "NPX~Treatment+Site+(1|Subject)",
      effect = "Treatment"
    )|>
      suppressMessages()

    lmer_posthoc_result_formula_covariate <- olink_lmer_posthoc(
      df = npx_data1_small,
      check_log = npx_data1_check,
      model_formula = "NPX~Treatment+Site+(1|Subject)",
      covariates = "Site",
      effect = "Treatment"
    )|>
      suppressMessages()

    # Check that results are the same in all cases
    # Only the effect results are returned
    expect_equal(
      object = lmer_posthoc_result_formula_covariate,
      expected = lmer_posthoc_result_args
    )

    expect_equal(
      object = lmer_posthoc_result_formula_covariate,
      expected = lmer_posthoc_result_formula
    )

    expect_equal(
      object = lmer_posthoc_result_formula,
      expected = lmer_posthoc_result_args
    )

    # Check that covariate is not included in any results
    expect_false(
      "Site" %in% unique(lmer_posthoc_result_args$term)
    )

    expect_false(
      "Site" %in% unique(lmer_posthoc_result_formula$term)
    )

    expect_false(
      "Site" %in% unique(lmer_posthoc_result_formula_covariate$term)
    )

    # Check that error is thrown when covariate specified does not exist
    expect_error(
      object = expect_message(
        object = expect_message(
          object = expect_message(
            object = lmer_posthoc_result_error <- olink_lmer_posthoc(
              df = npx_data1_small,
              check_log = npx_data1_check,
              model_formula = "NPX~Treatment+(1|Subject)",
              covariates = "Site",
              effect = "Treatment"
            ),
            regexp = paste("Samples removed due to missing variable or",
                           "covariate levels: CONTROL_SAMPLE_AS 1,",
                           "CONTROL_SAMPLE_AS 2")
          ),
          regexp = paste("Variables and covariates converted from character",
                         "to factors: Treatment, Subject")
        ),
        regexp = paste("Linear mixed effects model fit to each",
                       "assay:NPX~Treatment+(1|Subject)"),
        fixed = TRUE
      ),
      regexp = paste("Covariate \"Site\" is not present in the model formula!"),
    )

  }
)
