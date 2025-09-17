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

# Test olink_ordinalRegression ----

test_that(
  "olink_ordinalRegression - works - site",
  {
    skip_if_not_installed(pkg = "broom")
    skip_if_not_installed(pkg = "ordinal")

    expect_no_warning(
      object = expect_no_error(
        object = expect_message(
          object = expect_message(
            object = ord_regs_res_site <- olink_ordinalRegression(
              df = npx_data1_mod,
              variable = "Site",
              check_log = npx_data1_mod_check_log
            ),
            regexp = paste("Variables and covariates converted from character",
                           "to factors: Site")
          ),
          regexp = "Cumulative Link Model (CLM) fit to each assay: NPX~Site",
          fixed = TRUE
        )
      )
    )

    expect_identical(
      object = dim(ord_regs_res_site),
      expected = c(184L, 10L)
    )

    expect_identical(
      object = ord_regs_res_site |>
        dplyr::filter(.data[["Threshold"]] == "Significant") |>
        nrow(),
      expected = 21L
    )

    expect_equal(
      object = ord_regs_res_site |>
        dplyr::filter(.data[["Threshold"]] == "Significant") |>
        dplyr::select(
          dplyr::all_of(
            c("OlinkID", "term", "df", "statistic",
              "Adjusted_pval", "Threshold")
          )
        ),
      expected = dplyr::tibble(
        OlinkID = c("OID01218", "OID01276", "OID00488", "OID00484", "OID00472",
                    "OID01297", "OID00549", "OID00485", "OID00525", "OID00541",
                    "OID01228", "OID01253", "OID01296", "OID01305", "OID01267",
                    "OID00561", "OID01226", "OID01250", "OID00481", "OID00532",
                    "OID01268"),
        term = rep(x = "Site", times = 21L),
        df = rep(x = 4L, times = 21L),
        statistic = c(26.75414, 25.31355, 24.81096, 23.81637, 21.15660,
                      20.63518, 20.20273, 19.49613, 18.54173, 18.41350,
                      17.89335, 17.75377, 16.84813, 16.56931, 16.47307,
                      15.30990, 14.92483, 14.91469, 14.58520, 14.57997,
                      14.57055),
        Adjusted_pval = c(0.003367815, 0.003367815, 0.003367815, 0.003999119,
                          0.010849136, 0.011469572, 0.011970962, 0.014438676,
                          0.018847985, 0.018847985, 0.021138556, 0.021138556,
                          0.029281088, 0.030002231, 0.030002231, 0.047147683,
                          0.049768005, 0.049768005, 0.049768005, 0.049768005,
                          0.049768005),
        Threshold = rep(x = "Significant", times = 21L)
      ),
      tolerance = 1e-6
    )
  }
)

test_that(
  "olink_ordinalRegression - works - time",
  {
    skip_if_not_installed(pkg = "broom")
    skip_if_not_installed(pkg = "ordinal")

    expect_no_warning(
      object = expect_no_error(
        object = expect_message(
          object = expect_message(
            object = ord_regs_res_time <- olink_ordinalRegression(
              df = npx_data1_mod,
              variable = "Time",
              check_log = npx_data1_mod_check_log
            ),
            regexp = paste("Variables and covariates converted from character",
                           "to factors: Time")
          ),
          regexp = "Cumulative Link Model (CLM) fit to each assay: NPX~Time",
          fixed = TRUE
        )
      )
    )

    expect_identical(
      object = dim(ord_regs_res_time),
      expected = c(184L, 10L)
    )

    expect_identical(
      object = ord_regs_res_time |>
        dplyr::filter(.data[["Threshold"]] == "Significant") |>
        nrow(),
      expected = 0L
    )

    expect_equal(
      object = ord_regs_res_time |>
        dplyr::slice_head(n = 20L) |>
        dplyr::select(
          dplyr::all_of(
            c("OlinkID", "term", "df", "statistic",
              "Adjusted_pval", "Threshold")
          )
        ),
      expected = dplyr::tibble(
        OlinkID = c("OID00534", "OID01294", "OID01265", "OID00493", "OID01248",
                    "OID01276", "OID00499", "OID00491", "OID01247", "OID00525",
                    "OID01264", "OID01266", "OID00523", "OID00544", "OID00471",
                    "OID01232", "OID01213", "OID01225", "OID01252", "OID01219"),
        term = rep(x = "Time", times = 20L),
        df = rep(x = 2L, times = 20L),
        statistic = c(11.517852, 10.501171, 9.995991, 9.011922, 8.884133,
                      7.552191, 7.120552, 6.604045, 5.421028, 5.409309,
                      4.700308, 4.427255, 4.425728, 4.159592, 3.954284,
                      3.893117, 3.849188, 3.719878, 3.676861, 3.565180),
        Adjusted_pval = c(0.4140899, 0.4140899, 0.4140899, 0.4331945, 0.4331945,
                          0.7026338, 0.7473284, 0.8465990, 0.9985855, 0.9985855,
                          0.9985855, 0.9985855, 0.9985855, 0.9985855, 0.9985855,
                          0.9985855, 0.9985855, 0.9985855, 0.9985855,
                          0.9985855),
        Threshold = rep(x = "Non-significant", times = 20L)
      ),
      tolerance = 1e-6
    )
  }
)

test_that(
  "olink_ordinalRegression - works - treatment*time",
  {
    # Load reference results
    # tests are skipped if files are absent
    reference_results <- get_example_data(filename = "reference_results.rds")

    skip_if_not_installed(pkg = "broom")
    skip_if_not_installed(pkg = "ordinal")

    expect_no_warning(
      object = expect_no_error(
        object = expect_message(
          object = expect_message(
            object = ord_regs_res_treat_time <- olink_ordinalRegression(
              df = npx_data1_mod,
              variable = "Treatment:Time",
              check_log = npx_data1_mod_check_log
            ) |>
              dplyr::mutate(
                id = as.character(.data[["OlinkID"]])
              ) |>
              dplyr::arrange(
                .data[["id"]],
                .data[["Assay"]]
              ) |>
              dplyr::select(
                -dplyr::all_of("id")
              ),
            regexp = paste("Variables and covariates converted from character",
                           "to factors: Treatment, Time")
          ),
          regexp = paste("Cumulative Link Model (CLM) fit to each assay:",
                         "NPX~Treatment*Time"),
          fixed = TRUE
        )
      )
    )

    expect_equal(
      object = ord_regs_res_treat_time,
      expected = reference_results$ordinal_regression
    )
  }
)

test_that(
  "olink_ordinalRegression - works - no check_log",
  {
    skip_if_not_installed(pkg = "broom")
    skip_if_not_installed(pkg = "ordinal")

    expect_warning(
      object = expect_message(
        object = expect_message(
          object = expect_message(
            object = expect_message(
              object = olink_ordinalRegression(
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
        regexp = paste("Cumulative Link Model (CLM) fit to each assay:",
                       "NPX~treatment2"),
        fixed = TRUE
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
  "olink_ordinalRegression - error - 'df' and/or 'variable' not provided",
  {
    skip_if_not_installed(pkg = "ordinal")
    skip_if_not_installed(pkg = "broom")

    expect_error(
      object = olink_ordinalRegression(),
      regexp = "The df and variable arguments need to be specified."
    )

    expect_error(
      object = olink_ordinalRegression(df = npx_data1_mod),
      regexp = "The df and variable arguments need to be specified."
    )

    expect_error(
      object = olink_ordinalRegression(variable = "Site"),
      regexp = "The df and variable arguments need to be specified."
    )
  }
)

test_that(
  "olink_ordinalRegression - works - when edge cases are cleaned up",
  {
    skip_if_not_installed(pkg = "ordinal")
    skip_if_not_installed(pkg = "broom")

    expect_no_error(
      object = expect_no_warning(
        object = expect_message(
          object = expect_message(
            object = olink_ordinalRegression(
              df = dt_edge_case_ctrl_assay,
              variable = "treatment1",
              check_log = dt_edge_case_ctrl_assay_check
            ),
            regexp = paste("Variables and covariates converted from character",
                           "to factors: treatment1")
          ),
          regexp = paste("Cumulative Link Model (CLM) fit to each assay:",
                         "NPX~treatment1"),
          fixed = TRUE
        )
      )
    )
  }
)

# Test olink_ordinalRegression ----

test_that(
  "olink_ordinalRegression - works - site",
  {
    skip_if_not_installed(pkg = "broom")
    skip_if_not_installed(pkg = "ordinal")
    skip_if_not_installed(pkg = "emmeans")

    ord_regs_res_site <- olink_ordinalRegression(
      df = npx_data1_mod,
      variable = "Site",
      check_log = npx_data1_mod_check_log
    ) |>
      suppressMessages() |>
      suppressWarnings()

    ord_regs_res_site_oid <- ord_regs_res_site |>
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
            object = ord_regs_posthoc_res_site <-
              olink_ordinalRegression_posthoc(
                df = npx_data1_mod,
                check_log = npx_data1_mod_check_log,
                variable = "Site",
                olinkid_list = ord_regs_res_site_oid,
                effect = "Site"
              ),
            regexp = paste("Variables and covariates converted from character",
                           "to factors: Site")
          ),
          regexp = paste("Estimated marginal means for each assay computed",
                         "from the cumulative link model (CLM): NPX~Site"),
          fixed = TRUE
        )
      )
    )

    expect_identical(
      object = dim(ord_regs_posthoc_res_site),
      expected = c(210L, 9L)
    )

    expect_identical(
      object = ord_regs_posthoc_res_site |>
        dplyr::filter(.data[["Threshold"]] == "Significant") |>
        nrow(),
      expected = 54L
    )

    expect_equal(
      object = ord_regs_posthoc_res_site |>
        dplyr::filter(.data[["Threshold"]] == "Significant"
                      & grepl(pattern = "Site_A", x = .data[["contrast"]])) |>
        dplyr::select(
          dplyr::all_of(
            c("OlinkID", "term", "contrast", "estimate",
              "Adjusted_pval", "Threshold")
          )
        ) |>
        dplyr::mutate(
          dplyr::across(
            dplyr::all_of(c("OlinkID", "term", "contrast", "Threshold")),
            ~ as.character(.x)
          )
        ) |>
        dplyr::arrange(
          .data[["contrast"]], .data[["Adjusted_pval"]]
        ),
      expected = dplyr::tibble(
        OlinkID = c("OID01253", "OID00472", "OID01296", "OID01218", "OID00484",
                    "OID01228", "OID00485", "OID00484", "OID00485", "OID01250",
                    "OID01218", "OID00481", "OID00541", "OID01253", "OID01305",
                    "OID01297", "OID01228", "OID00484", "OID00532", "OID00541",
                    "OID01253", "OID00525", "OID00561"),
        term = rep(x = "Site", times = 23L),
        contrast = c(
          rep(x = "Site_A - Site_B", times = 7L),
          rep(x = "Site_A - Site_C", times = 3L),
          rep(x = "Site_A - Site_D", times = 6L),
          rep(x = "Site_A - Site_E", times = 7L)
        ),
        estimate = c(-1.909381, 1.868598, -1.779329, 1.495099, 1.538627,
                     1.350004, -1.338967, 2.219389, -1.862798, 1.802456,
                     1.814209, 1.652638, 1.548481, -1.442087, 1.422823,
                     -1.359331, 1.765493, 1.805050, 1.629207, 1.640337,
                     -1.594448, 1.446509, -1.274167),
        Adjusted_pval = c(0.0009047646, 0.0009262498, 0.0012594739,
                          0.0138088429, 0.0172293627, 0.0207905362,
                          0.0407860177, 0.0001103561, 0.0010905634,
                          0.0032021741, 0.0014308923, 0.0050188403,
                          0.0151847548, 0.0248466262, 0.0334173482,
                          0.0395936948, 0.0010840233, 0.0026291929,
                          0.0037286287, 0.0055363121, 0.0072898574,
                          0.0218196701, 0.0414924552),
        Threshold = rep(x = "Significant", times = 23L)
      ),
      tolerance = 1e-6
    )
  }
)

test_that(
  "olink_ordinalRegression - works - treatment*time",
  {
    # Load reference results
    # tests are skipped if files are absent
    reference_results <- get_example_data(filename = "reference_results.rds")

    skip_if_not_installed(pkg = "broom")
    skip_if_not_installed(pkg = "emmeans")
    skip_if_not_installed(pkg = "ordinal")

    ord_regs_res_treat_time <- olink_ordinalRegression(
      df = npx_data1_mod,
      variable = "Treatment:Time",
      check_log = npx_data1_mod_check_log
    ) |>
      suppressMessages() |>
      suppressWarnings()

    ord_regs_res_treat_time_oid <- ord_regs_res_treat_time |>
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
              object = ord_reg_ph_res_treat_time <-
                olink_ordinalRegression_posthoc(
                  df = npx_data1_mod,
                  check_log = npx_data1_mod_check_log,
                  variable = "Treatment:Time",
                  olinkid_list = ord_regs_res_treat_time_oid,
                  effect = "Time"
                ) |>
                dplyr::mutate(
                  id = as.character(.data[["OlinkID"]]),
                  # In R 3.6.1 we get factors, but reference is characters
                  contrast = as.character(.data[["contrast"]])
                ) |>
                # Since OlinkID is not unique here (=> ties), contrast is
                # used to break the ties
                dplyr::arrange(
                  .data[["id"]], .data[["contrast"]]
                ) |>
                dplyr::select(
                  -dplyr::all_of("id")
                ),
              regexp = paste("Variables and covariates converted from",
                             "character to factors: Treatment, Time")
            ),
            regexp = paste("Estimated marginal means for each assay computed",
                           "from the cumulative link model (CLM):",
                           "NPX~Treatment*Time"),
            fixed = TRUE
          ),
          regexp = paste("NOTE: Results may be misleading due to involvement",
                         "in interactions")
        )
      )
    )

    expect_equal(
      object = ord_reg_ph_res_treat_time,
      expected = reference_results$ordinal_regression_posthoc
    )
  }
)

test_that(
  "olink_ordinalRegression - works - no check_log",
  {
    skip_if_not_installed(pkg = "broom")
    skip_if_not_installed(pkg = "ordinal")
    skip_if_not_installed(pkg = "emmeans")

    expect_warning(
      object = expect_message(
        object = expect_message(
          object = expect_message(
            object = expect_message(
              object = ord_regs_res_treat <- olink_ordinalRegression(
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
        regexp = paste("Cumulative Link Model (CLM) fit to each assay:",
                       "NPX~treatment2"),
        fixed = TRUE
      ),
      regexp = paste("\"OID30136\", \"OID30144\", \"OID30166\", \"OID30168\",",
                     "\"OID30438\", \"OID30544\", \"OID30626\", \"OID30695\",",
                     "\"OID30748\", \"OID30866\", \"OID30899\", \"OID31054\",",
                     "\"OID31113\", \"OID31186\", \"OID31225\", \"OID31309\",",
                     "and \"OID31325\" have \"NPX\" = NA for all samples.")
    )

    ord_regs_res_treat_oid <- ord_regs_res_treat |>
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
              object = olink_ordinalRegression_posthoc(
                df = dt_edge_case_no_ctrl,
                variable = "treatment2",
                olinkid_list = ord_regs_res_treat_oid,
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
        regexp = paste("Estimated marginal means for each assay computed from",
                       "the cumulative link model (CLM): NPX~treatment2"),
        fixed = TRUE
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
  "olink_ordinalRegression_posthoc - error - required input not provided",
  {
    skip_if_not_installed(pkg = "broom")
    skip_if_not_installed(pkg = "ordinal")
    skip_if_not_installed(pkg = "emmeans")

    expect_error(
      object = olink_ordinalRegression_posthoc(),
      regexp = "The df, variable and effect arguments need to be specified."
    )

    expect_error(
      object = olink_ordinalRegression_posthoc(df = npx_data1_mod),
      regexp = "The df, variable and effect arguments need to be specified."
    )

    expect_error(
      object = olink_ordinalRegression_posthoc(variable = "Site"),
      regexp = "The df, variable and effect arguments need to be specified."
    )

    expect_error(
      object = olink_ordinalRegression_posthoc(
        df = npx_data1_mod,
        variable = "Site"
      ),
      regexp = "The df, variable and effect arguments need to be specified."
    )
  }
)

test_that(
  "olink_anova_posthoc - works - when edge cases are cleaned up",
  {
    skip_if_not_installed(pkg = "broom")
    skip_if_not_installed(pkg = "ordinal")
    skip_if_not_installed(pkg = "emmeans")

    expect_no_error(
      object = expect_no_warning(
        object = expect_message(
          object = expect_message(
            object = olink_ordinalRegression_posthoc(
              df = dt_edge_case_ctrl_assay,
              variable = "treatment2",
              check_log = dt_edge_case_ctrl_assay_check,
              effect = "treatment2"
            ),
            regexp = paste("Variables and covariates converted from character",
                           "to factors: treatment2")
          ),
          regexp = paste("Estimated marginal means for each assay computed",
                         "from the cumulative link model (CLM):",
                         "NPX~treatment2"),
          fixed = TRUE
        )
      )
    )
  }
)
