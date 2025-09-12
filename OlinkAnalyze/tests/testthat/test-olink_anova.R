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
  "olink_anova_posthoc - works - no check_log",
  {
    skip_if_not_installed(pkg = "car")
    skip_if_not_installed(pkg = "broom")
    skip_if_not_installed(pkg = "emmeans")

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
