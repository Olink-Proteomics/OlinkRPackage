# Create datasets for testing ----

## use npx_data1 ----

# Remove sample controls from npx_data1 to preserve test results
npx_data1_mod <- npx_data1 |>
  dplyr::filter(
    !stringr::str_detect(
      string = .data[["SampleID"]],
      pattern = stringr::regex(
        pattern = "control|ctrl",
        ignore_case = TRUE
      )
    )
  )
npx_data1_mod_check_log <- check_npx(df = npx_data1_mod)

## use npx data edge cases ----

dt_edge_case <- get_example_data(filename = "npx_data_format-Oct-2022.rds")

# Remove control assays from npx_data_format221010 for warning tests,
# but data with all NPX=NA for some assays
dt_edge_case_no_ctrl <- dt_edge_case |>
  dplyr::filter(
    !stringr::str_detect(
      string = .data[["Assay"]],
      pattern = "control"
    )
  )

# Remove assays with NPX == NA from npx_data_format-Oct-2022.rds for testing
dt_edge_case_no_na <- dt_edge_case |>
  dplyr::filter(
    !is.na(.data[["NPX"]])
  )

# Add dummy Extension control assays
dt_edge_case_ext_ctrl <- dt_edge_case_no_na |>
  dplyr::filter(
    stringr::str_detect(
      string = .data[["Assay"]],
      pattern = "Incubation control"
    )
  ) |>
  dplyr::mutate(
    Assay = gsub(pattern = "Incubation",
                 replacement = "Extension",
                 x = .data[["Assay"]]),
    UniProt = gsub(pattern = "INC",
                   replacement = "EXT",
                   x = .data[["UniProt"]])
  )

# Add dummy CTRL assay using the data for ACY3
dt_edge_case_assay_ctrl <- dt_edge_case_no_na |>
  dplyr::filter(
    stringr::str_detect(
      string = .data[["Assay"]],
      pattern = "ACY3"
    )
  ) |>
  dplyr::mutate(
    Assay = gsub(pattern = "ACY3",
                 replacement = "CTRL",
                 x = .data[["Assay"]]),
    UniProt = gsub(pattern = "Q96HD9",
                   replacement = "P40313",
                   x = .data[["UniProt"]]),
    OlinkID = gsub(pattern = "OID30086",
                   replacement = "OID12345",
                   x = .data[["OlinkID"]])
  )

dt_edge_case_ctrl <- dt_edge_case_no_na |>
  dplyr::bind_rows(dt_edge_case_ext_ctrl) |>
  dplyr::bind_rows(dt_edge_case_assay_ctrl)

dt_edge_case_ctrl_check <- check_npx(df = dt_edge_case_ctrl) |>
  suppressMessages() |>
  suppressWarnings()

# Add AssayType
dt_edge_case_assaytype <- dt_edge_case_ctrl |>
  dplyr::mutate(
    AssayType = dplyr::case_when(
      stringr::str_detect(string = .data[["Assay"]],
                          pattern = "Incubation control") ~ "inc_ctrl",
      stringr::str_detect(string = .data[["Assay"]],
                          pattern = "Amplification control") ~ "amp_ctrl",
      stringr::str_detect(string = .data[["Assay"]],
                          pattern = "Extension control") ~ "ext_ctrl",
      TRUE ~ "assay",
      .default = NA_character_
    )
  )

dt_edge_case_assaytype_check <- check_npx(df = dt_edge_case_assaytype) |>
  suppressMessages() |>
  suppressWarnings()

# Dataset with no AssayType column, no internal controls but includes CTRL assay
dt_edge_case_ctrl_assay <- dt_edge_case_ctrl |>
  dplyr::filter(
    !stringr::str_detect(
      string = .data[["Assay"]],
      pattern = "Incubation control|Amplification control|Extension control"
    )
  )

dt_edge_case_ctrl_assay_check <- check_npx(df = dt_edge_case_ctrl_assay) |>
  suppressMessages() |>
  suppressWarnings()

# Test olink_anova ----

test_that(
  "olink_anova - works - site",
  {
    # Load reference results
    # tests are skipped if files are absent
    reference_results <- get_example_data(filename = "reference_results.rds")

    skip_if_not_installed(pkg = "car")
    skip_if_not_installed(pkg = "broom")
    skip_on_cran()

    expect_no_warning(
      object = expect_no_error(
        object = expect_message(
          object = expect_message(
            object = anova_res_site <- olink_anova(
              df = npx_data1_mod,
              variable = "Site",
              check_log = npx_data1_mod_check_log
            ) |>
              dplyr::mutate(
                id = as.character(.data[["OlinkID"]])
              ) |>
              dplyr::arrange(
                .data[["id"]]
              ) |>
              dplyr::select(
                -dplyr::all_of("id")
              ),
            regexp = paste("Variables and covariates converted from character",
                           "to factors: Site")
          ),
          regexp = "ANOVA model fit to each assay: NPX~Site"
        )
      )
    )

    expect_equal(
      object = anova_res_site,
      expected = reference_results$anova_site
    )
  }
)

test_that(
  "olink_anova - works - time",
  {
    # Load reference results
    # tests are skipped if files are absent
    reference_results <- get_example_data(filename = "reference_results.rds")

    skip_if_not_installed(pkg = "car")
    skip_if_not_installed(pkg = "broom")
    skip_on_cran()

    expect_no_warning(
      object = expect_no_error(
        object = expect_message(
          object = expect_message(
            object = anova_res_time <- olink_anova(
              df = npx_data1_mod,
              variable = "Time",
              check_log = npx_data1_mod_check_log
            ) |>
              dplyr::mutate(
                id = as.character(.data[["OlinkID"]])
              ) |>
              dplyr::arrange(
                .data[["id"]]
              ) |>
              dplyr::select(
                -dplyr::all_of("id")
              ),
            regexp = paste("Variables and covariates converted from character",
                           "to factors: Time")
          ),
          regexp = "ANOVA model fit to each assay: NPX~Time"
        )
      )
    )

    expect_equal(
      object = anova_res_time,
      expected = reference_results$anova_time
    )
  }
)

test_that(
  "olink_anova - works - site*time",
  {
    # Load reference results
    # tests are skipped if files are absent
    reference_results <- get_example_data(filename = "reference_results.rds")

    skip_if_not_installed(pkg = "car")
    skip_if_not_installed(pkg = "broom")
    skip_on_cran()

    expect_no_warning(
      object = expect_no_error(
        object = expect_message(
          object = expect_message(
            object = anova_res_site_time <- olink_anova(
              df = npx_data1_mod,
              variable = c("Site", "Time"),
              check_log = npx_data1_mod_check_log
            ) |>
              dplyr::mutate(
                id = as.character(.data[["OlinkID"]])
              ) |>
              dplyr::arrange(
                .data[["id"]],
                .data[["term"]]
              ) |>
              dplyr::select(
                -dplyr::all_of("id")
              ),
            regexp = paste("Variables and covariates converted from character",
                           "to factors: Site, Time")
          ),
          regexp = "ANOVA model fit to each assay: NPX~Site*Time",
          fixed = TRUE
        )
      )
    )

    expect_equal(
      object = anova_res_site_time,
      expected = reference_results$anova_site_time
    )
  }
)

test_that(
  "olink_anova - works - no check_log",
  {
    skip_if_not_installed(pkg = "car")
    skip_if_not_installed(pkg = "broom")
    skip_on_cran()

    expect_warning(
      object = expect_message(
        object = expect_message(
          object = expect_message(
            object = expect_message(
              object = olink_anova(
                df = dt_edge_case_no_ctrl,
                variable = "treatment2"
              ),
              regexp = "`check_log` not provided. Running `check_npx()`.",
              fixed = TRUE
            ),
            regexp = paste("8 assays exhibited assay QC warnings in column",
                           "`Assay_Warning` of the dataset")
          ),
          regexp = paste("Variables and covariates converted from character",
                         "to factors: treatment2")
        ),
        regexp = "ANOVA model fit to each assay: NPX~treatment2"
      ),
      regexp = paste("\"OID30136\", \"OID30144\", \"OID30166\", \"OID30168\",",
                     "\"OID30438\", \"OID30544\", \"OID30626\", \"OID30695\",",
                     "\"OID30748\", \"OID30866\", \"OID30899\", \"OID31054\",",
                     "\"OID31113\", \"OID31186\", \"OID31225\", \"OID31309\",",
                     "and \"OID31325\" have \"NPX\" = NA for all samples.")
    )
  }
)

test_that(
  "olink_anova - error - 'df' and/or 'variable' not provided",
  {
    skip_if_not_installed(pkg = "car")
    skip_if_not_installed(pkg = "broom")
    skip_on_cran()

    expect_error(
      object = olink_anova(),
      regexp = "The df and variable arguments need to be specified."
    )

    expect_error(
      object = olink_anova(df = npx_data1_mod),
      regexp = "The df and variable arguments need to be specified."
    )

    expect_error(
      object = olink_anova(variable = "Site"),
      regexp = "The df and variable arguments need to be specified."
    )
  }
)

test_that(
  "olink_anova - error - control assays in dataset",
  {
    skip_if_not_installed(pkg = "car")
    skip_if_not_installed(pkg = "broom")
    skip_on_cran()

    expect_error(
      object = olink_anova(
        df = dt_edge_case_ctrl,
        variable = "treatment2",
        check_log = dt_edge_case_ctrl_check
      ),
      # Assay controls not removed, no AssayType
      regexp = "Control assays have not been removed from the dataset"
    )

    expect_error(
      object = olink_anova(
        df = dt_edge_case_assaytype,
        variable = "treatment2",
        check_log = dt_edge_case_assaytype_check
      ),
      # Assay controls not removed, AssayType present
      regexp = "Control assays have not been removed from the dataset"
    )
  }
)

test_that(
  "olink_anova - works - when edge cases are cleaned up",
  {
    skip_if_not_installed(pkg = "car")
    skip_if_not_installed(pkg = "broom")
    skip_on_cran()

    expect_no_error(
      object = expect_no_warning(
        object = expect_message(
          object = expect_message(
            object = olink_anova(
              df = dt_edge_case_ctrl_assay,
              variable = "treatment1",
              check_log = dt_edge_case_ctrl_assay_check
            ),
            regexp = paste("Variables and covariates converted from character",
                           "to factors: treatment1")
          ),
          regexp = "ANOVA model fit to each assay: NPX~treatment1"
        )
      )
    )
  }
)

# Test olink_anova_posthoc ----

test_that(
  "olink_anova_posthoc - works - site",
  {
    # Load reference results
    # tests are skipped if files are absent
    reference_results <- get_example_data(filename = "reference_results.rds")

    skip_if_not_installed(pkg = "car")
    skip_if_not_installed(pkg = "broom")
    skip_if_not_installed(pkg = "emmeans")
    skip_on_cran()

    # this has been tested earlier
    anova_res_site <- olink_anova(
      df = npx_data1_mod,
      variable = "Site",
      check_log = npx_data1_mod_check_log
    ) |>
      suppressMessages() |>
      suppressWarnings()

    anova_res_site <- anova_res_site |>
      dplyr::mutate(
        id = as.character(.data[["OlinkID"]])
      ) |>
      dplyr::arrange(
        .data[["id"]]
      ) |>
      dplyr::select(
        -dplyr::all_of("id")
      )

    anova_res_site_oid <- anova_res_site |>
      dplyr::slice_head(
        n = 10L
      ) |>
      dplyr::pull(
        .data[["OlinkID"]]
      )

    expect_no_error(
      object = expect_no_warning(
        object = expect_message(
          object = expect_message(
            object = anova_posthoc_res_site <- olink_anova_posthoc(
              df = npx_data1_mod,
              check_log = npx_data1_mod_check_log,
              variable = "Site",
              olinkid_list = anova_res_site_oid,
              effect = "Site"
            ) |>
              dplyr::mutate(
                id = as.character(.data[["OlinkID"]]),
                # In R 3.6.1 we get factors, but reference is characters
                contrast = as.character(.data[["contrast"]])
              ) |>
              # Since OlinkID is not unique here (=> ties), contrast is used to
              # break the ties
              dplyr::arrange(
                .data[["id"]], .data[["contrast"]]
              ) |>
              dplyr::select(
                -dplyr::all_of("id")
              ),
            regexp = paste("Variables and covariates converted from character",
                           "to factors: Site")
          ),
          regexp = "Means estimated for each assay from ANOVA model: NPX~Site"
        )
      )
    )

    expect_equal(
      object = anova_posthoc_res_site,
      expected = reference_results$anova_site_posthoc
    )
  }
)

test_that(
  "olink_anova_posthoc - works - time",
  {
    # Load reference results
    # tests are skipped if files are absent
    reference_results <- get_example_data(filename = "reference_results.rds")

    skip_if_not_installed(pkg = "car")
    skip_if_not_installed(pkg = "broom")
    skip_if_not_installed(pkg = "emmeans")
    skip_on_cran()

    # this has been tested earlier
    anova_res_time <- olink_anova(
      df = npx_data1_mod,
      variable = "Time",
      check_log = npx_data1_mod_check_log
    ) |>
      suppressMessages() |>
      suppressWarnings()

    anova_res_time <- anova_res_time |>
      dplyr::mutate(
        id = as.character(.data[["OlinkID"]])
      ) |>
      dplyr::arrange(
        .data[["id"]]
      ) |>
      dplyr::select(
        -dplyr::all_of("id")
      )

    anova_res_time_oid <- anova_res_time |>
      dplyr::slice_head(
        n = 10L
      ) |>
      dplyr::pull(
        .data[["OlinkID"]]
      )

    expect_no_error(
      object = expect_no_warning(
        object = expect_message(
          object = expect_message(
            object = anova_posthoc_res_time <- olink_anova_posthoc(
              df = npx_data1_mod,
              check_log = npx_data1_mod_check_log,
              variable = "Time",
              olinkid_list = anova_res_time_oid,
              effect = "Time"
            ) |>
              dplyr::mutate(
                id = as.character(.data[["OlinkID"]]),
                # In R 3.6.1 we get factors, but reference is characters
                contrast = as.character(.data[["contrast"]])
              ) |>
              # Since OlinkID is not unique here (=> ties), contrast is used to
              # break the ties
              dplyr::arrange(
                .data[["id"]], .data[["contrast"]]
              ) |>
              dplyr::select(
                -dplyr::all_of("id")
              ),
            regexp = paste("Variables and covariates converted from character",
                           "to factors: Time")
          ),
          regexp = "Means estimated for each assay from ANOVA model: NPX~Time"
        )
      )
    )

    expect_equal(
      object = anova_posthoc_res_time,
      expected = reference_results$anova_time_posthoc
    )
  }
)

test_that(
  "olink_anova_posthoc - works - site*time",
  {
    skip_if_not_installed(pkg = "car")
    skip_if_not_installed(pkg = "broom")
    skip_if_not_installed(pkg = "emmeans")
    skip_on_cran()

    # this has been tested earlier
    anova_res_site_time <- olink_anova(
      df = npx_data1_mod,
      variable = c("Site", "Time"),
      check_log = npx_data1_mod_check_log
    ) |>
      suppressMessages() |>
      suppressWarnings()

    anova_res_site_time_oid <- anova_res_site_time |>
      dplyr::filter(
        .data[["Threshold"]] == "Significant"
      ) |>
      dplyr::pull(
        .data[["OlinkID"]]
      )

    expect_no_error(
      object = expect_no_warning(
        object = olink_anova_posthoc_msg <- testthat::capture_messages(
          {
            anova_posthoc_res_site_time <- olink_anova_posthoc(
              df = npx_data1_mod,
              check_log = npx_data1_mod_check_log,
              variable = c("Site", "Time"),
              olinkid_list = anova_res_site_time_oid,
              effect = "Time"
            )
          }
        )
      )
    )

    # check messages
    expect_true(
      object = grepl(
        pattern = paste("Variables and covariates converted from character to",
                        "factors: Site, Time"),
        x = olink_anova_posthoc_msg[1L],
        fixed = TRUE
      )
    )
    expect_true(
      object = grepl(
        pattern = paste("Means estimated for each assay from ANOVA model:",
                        "NPX~Site*Time"),
        x = olink_anova_posthoc_msg[2L],
        fixed = TRUE
      )
    )
    expect_equal(
      object = grepl(
        pattern = paste("NOTE: Results may be misleading due to involvement",
                        "in interactions"),
        x = olink_anova_posthoc_msg[3L:7L],
        fixed = TRUE
      ) |>
        sum(),
      expected = 5L
    )

    # check that results match
    expect_identical(
      object = dim(anova_posthoc_res_site_time),
      expected = c(15L, 11L)
    )

    expect_equal(
      object = anova_posthoc_res_site_time |>
        dplyr::filter(
          .data[["Threshold"]] == "Significant"
        ) |>
        dplyr::select(
          dplyr::all_of(
            c("OlinkID", "term", "contrast", "estimate",
              "conf.low", "conf.high", "Adjusted_pval")
          )
        ) |>
        dplyr::mutate(
          dplyr::across(
            dplyr::all_of(
              c("OlinkID", "term", "contrast")
            ),
            ~ as.character(.x)
          )
        ),
      expected = dplyr::tibble(
        OlinkID = c("OID01276", "OID00525"),
        term = rep(x = "Time", times = 2L),
        contrast = rep(x = "Baseline - Week.6", times = 2L),
        estimate = c(-2.311703, -1.060433),
        conf.low = c(-4.346567, -2.053885),
        conf.high = c(-0.2768394, -0.0669805),
        Adjusted_pval = c(0.02164654, 0.03340609)
      ),
      tolerance = 1e-6
    )
  }
)

test_that(
  "olink_anova_posthoc - works - no check_log",
  {
    skip_if_not_installed(pkg = "car")
    skip_if_not_installed(pkg = "broom")
    skip_if_not_installed(pkg = "emmeans")
    skip_on_cran()

    expect_warning(
      object = expect_message(
        object = expect_message(
          object = expect_message(
            object = expect_message(
              object = anova_res <- olink_anova(
                df = dt_edge_case_no_ctrl,
                variable = "treatment2"
              ),
              regexp = "`check_log` not provided. Running `check_npx()`.",
              fixed = TRUE
            ),
            regexp = paste("8 assays exhibited assay QC warnings in column",
                           "`Assay_Warning` of the dataset")
          ),
          regexp = paste("Variables and covariates converted from character",
                         "to factors: treatment2")
        ),
        regexp = "ANOVA model fit to each assay: NPX~treatment2"
      ),
      regexp = paste("\"OID30136\", \"OID30144\", \"OID30166\", \"OID30168\",",
                     "\"OID30438\", \"OID30544\", \"OID30626\", \"OID30695\",",
                     "\"OID30748\", \"OID30866\", \"OID30899\", \"OID31054\",",
                     "\"OID31113\", \"OID31186\", \"OID31225\", \"OID31309\",",
                     "and \"OID31325\" have \"NPX\" = NA for all samples.")
    )

    anova_res_oid <- anova_res |>
      dplyr::slice_head(
        n = 10L
      ) |>
      dplyr::pull(
        .data[["OlinkID"]]
      )

    expect_warning(
      object = expect_message(
        object = expect_message(
          object = expect_message(
            object = expect_message(
              object = olink_anova_posthoc(
                df = dt_edge_case_no_ctrl,
                variable = "treatment2",
                olinkid_list = anova_res_oid,
                effect = "treatment2"
              ),
              regexp = "`check_log` not provided. Running `check_npx()`.",
              fixed = TRUE
            ),
            regexp = paste("8 assays exhibited assay QC warnings in column",
                           "`Assay_Warning` of the dataset")
          ),
          regexp = paste("Variables and covariates converted from character",
                         "to factors: treatment2")
        ),
        regexp = paste("Means estimated for each assay from ANOVA model:",
                       "NPX~treatment2")
      ),
      regexp = paste("\"OID30136\", \"OID30144\", \"OID30166\", \"OID30168\",",
                     "\"OID30438\", \"OID30544\", \"OID30626\", \"OID30695\",",
                     "\"OID30748\", \"OID30866\", \"OID30899\", \"OID31054\",",
                     "\"OID31113\", \"OID31186\", \"OID31225\", \"OID31309\",",
                     "and \"OID31325\" have \"NPX\" = NA for all samples.")
    )
  }
)

test_that(
  "olink_anova_posthoc - error - 'df', 'variable' or 'effect' not provided",
  {
    skip_if_not_installed(pkg = "car")
    skip_if_not_installed(pkg = "broom")
    skip_if_not_installed(pkg = "emmeans")
    skip_on_cran()

    expect_error(
      object = olink_anova_posthoc(),
      regexp = "The df and variable and effect arguments need to be specified."
    )

    expect_error(
      object = olink_anova_posthoc(df = npx_data1_mod),
      regexp = "The df and variable and effect arguments need to be specified."
    )

    expect_error(
      object = olink_anova_posthoc(variable = "Site"),
      regexp = "The df and variable and effect arguments need to be specified."
    )
  }
)

test_that(
  "olink_anova_posthoc - error - control assays in dataset",
  {
    skip_if_not_installed(pkg = "car")
    skip_if_not_installed(pkg = "broom")
    skip_if_not_installed(pkg = "emmeans")
    skip_on_cran()

    expect_error(
      object = olink_anova_posthoc(
        df = dt_edge_case_ctrl,
        variable = "treatment2",
        check_log = dt_edge_case_ctrl_check,
        effect = "treatment2"
      ),
      # Assay controls not removed, no AssayType
      regexp = "Control assays have not been removed from the dataset"
    )

    expect_error(
      object = olink_anova_posthoc(
        df = dt_edge_case_assaytype,
        variable = "treatment2",
        check_log = dt_edge_case_assaytype_check,
        effect = "treatment2"
      ),
      # Assay controls not removed, AssayType present
      regexp = "Control assays have not been removed from the dataset"
    )
  }
)

test_that(
  "olink_anova_posthoc - works - when edge cases are cleaned up",
  {
    skip_if_not_installed(pkg = "car")
    skip_if_not_installed(pkg = "broom")
    skip_if_not_installed(pkg = "emmeans")
    skip_on_cran()

    expect_no_error(
      object = expect_no_warning(
        object = expect_message(
          object = expect_message(
            object = olink_anova_posthoc(
              df = dt_edge_case_ctrl_assay,
              variable = "treatment2",
              check_log = dt_edge_case_ctrl_assay_check,
              effect = "treatment2"
            ),
            regexp = paste("Variables and covariates converted from character",
                           "to factors: treatment2")
          ),
          regexp = paste("Means estimated for each assay from ANOVA model:",
                         "NPX~treatment2")
        )
      )
    )
  }
)

# Additional tests for comprehensive coverage ----

test_that(
  "olink_anova - works - model_formula string override",
  {
    skip_if_not_installed(pkg = "car")
    skip_if_not_installed(pkg = "broom")
    skip_on_cran()

    expect_message(
      object = expect_message(
        object = expect_no_error(
          object = expect_no_warning(
            object = olink_anova(
              df = npx_data1_mod,
              check_log = npx_data1_mod_check_log,
              model_formula = "NPX~Site",
              variable = "Time"
            )
          )
        ),
        regexp = "model_formula overriding variable and covariate arguments."
      ),
      regexp = "ANOVA model fit to each assay: NPX~Site"
    )
  }
)

test_that(
  "olink_anova - works - model_formula object override",
  {
    skip_if_not_installed(pkg = "car")
    skip_if_not_installed(pkg = "broom")
    skip_on_cran()

    expect_message(
      object = expect_no_error(
        object = expect_no_warning(
          object = olink_anova(
            df = npx_data1_mod,
            check_log = npx_data1_mod_check_log,
            model_formula = stats::as.formula("NPX~Time")
          )
        )
      ),
      regexp = "ANOVA model fit to each assay: NPX~Time"
    )
  }
)

test_that(
  "olink_anova - error - invalid model_formula",
  {
    skip_if_not_installed(pkg = "car")
    skip_if_not_installed(pkg = "broom")
    skip_on_cran()

    expect_error(
      object = olink_anova(
        df = npx_data1_mod,
        check_log = npx_data1_mod_check_log,
        model_formula = "this is not a valid formula"
      ),
      regexp = "is not a recognized formula"
    )
  }
)

test_that(
  "olink_anova - works - with single covariate",
  {
    skip_if_not_installed(pkg = "car")
    skip_if_not_installed(pkg = "broom")
    skip_on_cran()

    expect_no_error(
      object = expect_no_warning(
        object = expect_message(
          object = expect_message(
            object = anova_res <- olink_anova(
              df = npx_data1_mod,
              check_log = npx_data1_mod_check_log,
              variable = "Time",
              covariates = "Site"
            ),
            regexp = paste("Variables and covariates converted from character",
                           "to factors: Time, Site")
          ),
          regexp = "ANOVA model fit to each assay: NPX~Time\\+Site",
          fixed = TRUE
        )
      )
    )

    # Verify that results include both Time and Site terms
    expect_true(
      object = "Time" %in% unique(anova_res$term)
    )
    expect_true(
      object = "Site" %in% unique(anova_res$term)
    )
  }
)

test_that(
  "olink_anova - works - with interaction covariate",
  {
    skip_if_not_installed(pkg = "car")
    skip_if_not_installed(pkg = "broom")
    skip_on_cran()

    # Create a dataset with a third variable for testing
    npx_test_data <- npx_data1_mod |>
      dplyr::mutate(
        Treatment = dplyr::case_when(
          Site == "Site_A" ~ "Treated",
          TRUE ~ "Untreated"
        )
      )

    npx_test_check <- check_npx(df = npx_test_data) |>
      suppressMessages() |>
      suppressWarnings()

    expect_no_error(
      object = expect_message(
        object = expect_message(
          object = anova_res <- olink_anova(
            df = npx_test_data,
            check_log = npx_test_check,
            variable = "Time",
            covariates = "Site:Treatment"
          ),
          regexp = "Missing main effects added to the model formula: Site, Treatment"
        ),
        regexp = "ANOVA model fit to each assay: NPX~Time\\+Site:Treatment\\+Site\\+Treatment",
        fixed = TRUE
      )
    )
  }
)

test_that(
  "olink_anova - works - return.covariates = TRUE",
  {
    skip_if_not_installed(pkg = "car")
    skip_if_not_installed(pkg = "broom")
    skip_on_cran()

    anova_res_with_cov <- olink_anova(
      df = npx_data1_mod,
      check_log = npx_data1_mod_check_log,
      variable = "Time",
      covariates = "Site",
      return.covariates = TRUE
    ) |>
      suppressMessages() |>
      suppressWarnings()

    # Verify that Site covariate is in results
    expect_true(
      object = "Site" %in% unique(anova_res_with_cov$term)
    )

    # Verify that covariate has NA adjusted p-value
    site_results <- anova_res_with_cov |>
      dplyr::filter(term == "Site")

    expect_true(
      object = all(is.na(site_results$Adjusted_pval))
    )
  }
)

test_that(
  "olink_anova - works - verbose = FALSE",
  {
    skip_if_not_installed(pkg = "car")
    skip_if_not_installed(pkg = "broom")
    skip_on_cran()

    # Capture all messages
    msgs <- testthat::capture_messages(
      {
        anova_res <- olink_anova(
          df = npx_data1_mod,
          check_log = npx_data1_mod_check_log,
          variable = "Site",
          verbose = FALSE
        )
      }
    )

    # Should have no messages when verbose = FALSE
    expect_equal(
      object = length(msgs),
      expected = 0L
    )
  }
)

test_that(
  "olink_anova_posthoc - works - model_formula override",
  {
    skip_if_not_installed(pkg = "car")
    skip_if_not_installed(pkg = "broom")
    skip_if_not_installed(pkg = "emmeans")
    skip_on_cran()

    # Get some OlinkIDs for testing
    anova_res_site <- olink_anova(
      df = npx_data1_mod,
      variable = "Site",
      check_log = npx_data1_mod_check_log
    ) |>
      suppressMessages() |>
      suppressWarnings()

    anova_res_site_oid <- anova_res_site |>
      dplyr::slice_head(n = 5L) |>
      dplyr::pull(.data[["OlinkID"]])

    expect_message(
      object = expect_no_error(
        object = expect_no_warning(
          object = olink_anova_posthoc(
            df = npx_data1_mod,
            check_log = npx_data1_mod_check_log,
            model_formula = "NPX~Site",
            variable = "Time",
            olinkid_list = anova_res_site_oid,
            effect = "Site"
          )
        )
      ),
      regexp = "model_formula overriding variable and covariate arguments."
    )
  }
)

test_that(
  "olink_anova_posthoc - works - effect_formula",
  {
    skip_if_not_installed(pkg = "car")
    skip_if_not_installed(pkg = "broom")
    skip_if_not_installed(pkg = "emmeans")
    skip_on_cran()

    # Two-way ANOVA for testing effect_formula
    anova_res_site_time <- olink_anova(
      df = npx_data1_mod,
      variable = c("Site", "Time"),
      check_log = npx_data1_mod_check_log
    ) |>
      suppressMessages() |>
      suppressWarnings()

    anova_res_oid <- anova_res_site_time |>
      dplyr::slice_head(n = 5L) |>
      dplyr::pull(.data[["OlinkID"]])

    expect_message(
      object = expect_no_error(
        object = expect_no_warning(
          object = posthoc_res <- olink_anova_posthoc(
            df = npx_data1_mod,
            check_log = npx_data1_mod_check_log,
            variable = c("Site", "Time"),
            olinkid_list = anova_res_oid,
            effect = "Time",
            effect_formula = "pairwise~Time|Site"
          )
        )
      ),
      regexp = "effect_formula overriding effect argument."
    )

    # Verify results have the expected structure
    expect_true(
      object = "contrast" %in% names(posthoc_res)
    )
  }
)

test_that(
  "olink_anova_posthoc - works - mean_return = TRUE",
  {
    skip_if_not_installed(pkg = "car")
    skip_if_not_installed(pkg = "broom")
    skip_if_not_installed(pkg = "emmeans")
    skip_on_cran()

    anova_res_site <- olink_anova(
      df = npx_data1_mod,
      variable = "Site",
      check_log = npx_data1_mod_check_log
    ) |>
      suppressMessages() |>
      suppressWarnings()

    anova_res_site_oid <- anova_res_site |>
      dplyr::slice_head(n = 5L) |>
      dplyr::pull(.data[["OlinkID"]])

    posthoc_res_mean <- olink_anova_posthoc(
      df = npx_data1_mod,
      check_log = npx_data1_mod_check_log,
      variable = "Site",
      olinkid_list = anova_res_site_oid,
      effect = "Site",
      mean_return = TRUE
    ) |>
      suppressMessages() |>
      suppressWarnings()

    # Verify that emmean column is present
    expect_true(
      object = "emmean" %in% names(posthoc_res_mean)
    )

    # Verify that contrast and p-value columns are NOT present
    expect_false(
      object = "contrast" %in% names(posthoc_res_mean)
    )
    expect_false(
      object = "Adjusted_pval" %in% names(posthoc_res_mean)
    )
  }
)

test_that(
  "olink_anova_posthoc - works - post_hoc_padjust_method sidak",
  {
    skip_if_not_installed(pkg = "car")
    skip_if_not_installed(pkg = "broom")
    skip_if_not_installed(pkg = "emmeans")
    skip_on_cran()

    anova_res_time <- olink_anova(
      df = npx_data1_mod,
      variable = "Time",
      check_log = npx_data1_mod_check_log
    ) |>
      suppressMessages() |>
      suppressWarnings()

    anova_res_time_oid <- anova_res_time |>
      dplyr::slice_head(n = 5L) |>
      dplyr::pull(.data[["OlinkID"]])

    expect_no_error(
      object = expect_no_warning(
        object = posthoc_res <- olink_anova_posthoc(
          df = npx_data1_mod,
          check_log = npx_data1_mod_check_log,
          variable = "Time",
          olinkid_list = anova_res_time_oid,
          effect = "Time",
          post_hoc_padjust_method = "sidak"
        ) |>
          suppressMessages()
      )
    )

    expect_true(
      object = "Adjusted_pval" %in% names(posthoc_res)
    )
  }
)

test_that(
  "olink_anova_posthoc - works - post_hoc_padjust_method bonferroni",
  {
    skip_if_not_installed(pkg = "car")
    skip_if_not_installed(pkg = "broom")
    skip_if_not_installed(pkg = "emmeans")
    skip_on_cran()

    anova_res_time <- olink_anova(
      df = npx_data1_mod,
      variable = "Time",
      check_log = npx_data1_mod_check_log
    ) |>
      suppressMessages() |>
      suppressWarnings()

    anova_res_time_oid <- anova_res_time |>
      dplyr::slice_head(n = 5L) |>
      dplyr::pull(.data[["OlinkID"]])

    expect_no_error(
      object = expect_no_warning(
        object = posthoc_res <- olink_anova_posthoc(
          df = npx_data1_mod,
          check_log = npx_data1_mod_check_log,
          variable = "Time",
          olinkid_list = anova_res_time_oid,
          effect = "Time",
          post_hoc_padjust_method = "bonferroni"
        ) |>
          suppressMessages()
      )
    )

    expect_true(
      object = "Adjusted_pval" %in% names(posthoc_res)
    )
  }
)

test_that(
  "olink_anova_posthoc - works - post_hoc_padjust_method none",
  {
    skip_if_not_installed(pkg = "car")
    skip_if_not_installed(pkg = "broom")
    skip_if_not_installed(pkg = "emmeans")
    skip_on_cran()

    anova_res_site <- olink_anova(
      df = npx_data1_mod,
      variable = "Site",
      check_log = npx_data1_mod_check_log
    ) |>
      suppressMessages() |>
      suppressWarnings()

    anova_res_site_oid <- anova_res_site |>
      dplyr::slice_head(n = 5L) |>
      dplyr::pull(.data[["OlinkID"]])

    posthoc_res <- olink_anova_posthoc(
      df = npx_data1_mod,
      check_log = npx_data1_mod_check_log,
      variable = "Site",
      olinkid_list = anova_res_site_oid,
      effect = "Site",
      post_hoc_padjust_method = "none"
    ) |>
      suppressMessages() |>
      suppressWarnings()

    # When method is "none", column should be named "pvalue" not "Adjusted_pval"
    expect_true(
      object = "pvalue" %in% names(posthoc_res)
    )
    expect_false(
      object = "Adjusted_pval" %in% names(posthoc_res)
    )
  }
)

test_that(
  "olink_anova_posthoc - works - olinkid_list NULL uses all assays",
  {
    skip_if_not_installed(pkg = "car")
    skip_if_not_installed(pkg = "broom")
    skip_if_not_installed(pkg = "emmeans")
    skip_on_cran()

    posthoc_res_all <- olink_anova_posthoc(
      df = npx_data1_mod,
      check_log = npx_data1_mod_check_log,
      variable = "Site",
      olinkid_list = NULL,
      effect = "Site"
    ) |>
      suppressMessages() |>
      suppressWarnings()

    # Get unique OlinkIDs from input data
    all_oids <- unique(npx_data1_mod$OlinkID)

    # Verify that posthoc was run on all OlinkIDs
    posthoc_oids <- unique(posthoc_res_all$OlinkID)

    # Should have many OlinkIDs (all from the dataset)
    expect_gt(
      object = length(posthoc_oids),
      expected = 50L
    )
  }
)

test_that(
  "olink_anova_posthoc - works - verbose = FALSE",
  {
    skip_if_not_installed(pkg = "car")
    skip_if_not_installed(pkg = "broom")
    skip_if_not_installed(pkg = "emmeans")
    skip_on_cran()

    anova_res_site <- olink_anova(
      df = npx_data1_mod,
      variable = "Site",
      check_log = npx_data1_mod_check_log
    ) |>
      suppressMessages() |>
      suppressWarnings()

    anova_res_site_oid <- anova_res_site |>
      dplyr::slice_head(n = 5L) |>
      dplyr::pull(.data[["OlinkID"]])

    # Capture all messages
    msgs <- testthat::capture_messages(
      {
        posthoc_res <- olink_anova_posthoc(
          df = npx_data1_mod,
          check_log = npx_data1_mod_check_log,
          variable = "Site",
          olinkid_list = anova_res_site_oid,
          effect = "Site",
          verbose = FALSE
        )
      }
    )

    # Should have no messages when verbose = FALSE
    expect_equal(
      object = length(msgs),
      expected = 0L
    )
  }
)

test_that(
  "olink_anova_posthoc - error - effect not in variable",
  {
    skip_if_not_installed(pkg = "car")
    skip_if_not_installed(pkg = "broom")
    skip_if_not_installed(pkg = "emmeans")
    skip_on_cran()

    expect_error(
      object = olink_anova_posthoc(
        df = npx_data1_mod,
        check_log = npx_data1_mod_check_log,
        variable = "Site",
        effect = "Time"
      ),
      regexp = "All effect terms must be included in the variable argument"
    )
  }
)

test_that(
  "olink_anova_posthoc - error - invalid effect_formula",
  {
    skip_if_not_installed(pkg = "car")
    skip_if_not_installed(pkg = "broom")
    skip_if_not_installed(pkg = "emmeans")
    skip_on_cran()

    expect_error(
      object = olink_anova_posthoc(
        df = npx_data1_mod,
        check_log = npx_data1_mod_check_log,
        variable = "Site",
        effect = "Site",
        effect_formula = c("pairwise~Site", "another formula")
      ),
      regexp = "Unrecognized effect formula"
    )
  }
)

test_that(
  "olink_anova - works - with numeric variable",
  {
    skip_if_not_installed(pkg = "car")
    skip_if_not_installed(pkg = "broom")
    skip_on_cran()

    # Create a dataset with a numeric variable
    npx_numeric_data <- npx_data1_mod |>
      dplyr::mutate(
        Age = as.numeric(dplyr::case_when(
          Site == "Site_A" ~ 30,
          Site == "Site_B" ~ 45,
          Site == "Site_C" ~ 60,
          TRUE ~ 50
        ))
      )

    npx_numeric_check <- check_npx(df = npx_numeric_data) |>
      suppressMessages() |>
      suppressWarnings()

    expect_message(
      object = expect_message(
        object = expect_no_error(
          object = expect_no_warning(
            object = anova_res <- olink_anova(
              df = npx_numeric_data,
              check_log = npx_numeric_check,
              variable = "Age"
            )
          )
        ),
        regexp = "Variables and covariates treated as numeric: Age"
      ),
      regexp = "ANOVA model fit to each assay: NPX~Age"
    )

    # Verify results exist
    expect_true(
      object = nrow(anova_res) > 0L
    )
  }
)

test_that(
  "olink_anova_posthoc - works - with numeric variable",
  {
    skip_if_not_installed(pkg = "car")
    skip_if_not_installed(pkg = "broom")
    skip_if_not_installed(pkg = "emmeans")
    skip_on_cran()

    # Create a dataset with a numeric variable
    npx_numeric_data <- npx_data1_mod |>
      dplyr::mutate(
        Age = as.numeric(dplyr::case_when(
          Site == "Site_A" ~ 30,
          Site == "Site_B" ~ 45,
          Site == "Site_C" ~ 60,
          TRUE ~ 50
        ))
      )

    npx_numeric_check <- check_npx(df = npx_numeric_data) |>
      suppressMessages() |>
      suppressWarnings()

    anova_res <- olink_anova(
      df = npx_numeric_data,
      check_log = npx_numeric_check,
      variable = "Age"
    ) |>
      suppressMessages() |>
      suppressWarnings()

    anova_res_oid <- anova_res |>
      dplyr::slice_head(n = 5L) |>
      dplyr::pull(.data[["OlinkID"]])

    expect_message(
      object = expect_message(
        object = expect_message(
          object = expect_no_error(
            object = expect_no_warning(
              object = posthoc_res <- olink_anova_posthoc(
                df = npx_numeric_data,
                check_log = npx_numeric_check,
                variable = "Age",
                olinkid_list = anova_res_oid,
                effect = "Age"
              )
            )
          ),
          regexp = "Variables and covariates treated as numeric: Age"
        ),
        regexp = "Numeric variables post-hoc performed using Mean and Mean \\+ 1SD: Age"
      ),
      regexp = "Means estimated for each assay from ANOVA model: NPX~Age"
    )

    # Verify results exist
    expect_true(
      object = nrow(posthoc_res) > 0L
    )
  }
)

test_that(
  "olink_anova - message - removed samples with NA variables",
  {
    skip_if_not_installed(pkg = "car")
    skip_if_not_installed(pkg = "broom")
    skip_on_cran()

    # Create a dataset with NA values in a variable
    npx_na_data <- npx_data1_mod |>
      dplyr::mutate(
        Site = dplyr::if_else(
          dplyr::row_number() <= 5L,
          NA_character_,
          Site
        )
      )

    npx_na_check <- check_npx(df = npx_na_data) |>
      suppressMessages() |>
      suppressWarnings()

    expect_message(
      object = olink_anova(
        df = npx_na_data,
        check_log = npx_na_check,
        variable = "Site"
      ) |>
        suppressMessages(),
      regexp = "Samples removed due to missing variable or covariate levels:"
    )
  }
)
