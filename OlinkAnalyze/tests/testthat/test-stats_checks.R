test_that(
  "check_stats_required validates required inputs",
  {
    expect_invisible(check_stats_required(c(TRUE, TRUE), "missing"))
    expect_error(
      check_stats_required(c(TRUE, FALSE), "required input missing"),
      "required input missing"
    )
  }
)

test_that(
  "check_stats_formula validates and normalizes formulas",
  {
    expect_identical(
      check_stats_formula("NPX ~ Treatment + Time"),
      "NPX~Treatment+Time"
    )
    expect_identical(
      check_stats_formula(NPX ~ Treatment),
      "NPX~Treatment"
    )
    expect_error(
      check_stats_formula("NPX ~ Treatment +"),
      "not a recognized formula"
    )
  }
)

test_that(
  "parse_stats_effect_formula handles supported effect formats",
  {
    expect_identical(parse_stats_effect_formula("Treatment"), "Treatment")
    expect_identical(
      parse_stats_effect_formula("pairwise ~ Treatment + Time"),
      c(" Treatment ", " Time")
    )
    expect_identical(
      parse_stats_effect_formula(~ Treatment * Time),
      c("Treatment ", " Time")
    )
    expect_message(
      parse_stats_effect_formula(
        "Treatment",
        effect = "Time",
        effect_supplied = TRUE
      ),
      "overriding effect argument"
    )
    expect_error(
      parse_stats_effect_formula(c("Treatment", "Time")),
      "character string of length1"
    )
  }
)

test_that(
  "check_stats_effect_terms validates model membership",
  {
    expect_invisible(
      check_stats_effect_terms("Treatment:Time", "Treatment*Time")
    )
    expect_error(
      check_stats_effect_terms("Site", "Treatment"),
      "variable argument"
    )
    expect_error(
      check_stats_effect_terms(
        "Site",
        "Treatment",
        include_model_formula = TRUE
      ),
      "variable argument or model formula"
    )
  }
)

test_that(
  "check_stats_control_assays supports both control conventions",
  {
    expect_invisible(
      check_stats_control_assays(
        data.frame(Assay = "Protein", AssayType = "assay")
      )
    )
    expect_error(
      check_stats_control_assays(
        data.frame(
          Assay = c("Protein", "Extension control"),
          AssayType = c("assay", "control")
        )
      ),
      "Control assays have not been removed"
    )
    expect_invisible(
      check_stats_control_assays(data.frame(Assay = "Protein"))
    )
    expect_error(
      check_stats_control_assays(
        data.frame(Assay = c("Protein", "Incubation control"))
      ),
      "Control assays have not been removed"
    )
  }
)

test_that(
  "check_stats_effect_data detects incomplete assay levels",
  {
    check_log <- list(
      assay_na = character(),
      col_names = list(
        sample_id = "SampleID",
        olink_id = "OlinkID"
      )
    )
    effect_data <- tibble::tibble(
      SampleID = paste0("S", 1:4),
      OlinkID = rep("OID00001", 4L),
      Group = c("A", "A", "B", "B"),
      NPX = c(NA_real_, NA_real_, 1, 2)
    )

    expect_warning(
      incomplete <- check_stats_effect_data(
        df = effect_data,
        effects = "Group",
        outcome = "NPX",
        check_log = check_log
      ),
      "only NA values"
    )
    expect_identical(incomplete, "OID00001")

    effect_data[["NPX"]] <- seq_len(nrow(effect_data))
    expect_identical(
      check_stats_effect_data(
        df = effect_data,
        effects = "Group",
        outcome = "NPX",
        check_log = check_log
      ),
      character()
    )

    check_log$assay_na <- "OID00001"
    effect_data[["NPX"]] <- NA_real_
    expect_identical(
      check_stats_effect_data(
        df = effect_data,
        effects = "Group",
        outcome = "NPX",
        check_log = check_log
      ),
      character()
    )
  }
)

test_that(
  "check_stats_effect_data requires one effect level per sample",
  {
    check_log <- list(
      assay_na = character(),
      col_names = list(
        sample_id = "SampleID",
        olink_id = "OlinkID"
      )
    )
    invalid_data <- tibble::tibble(
      SampleID = c("S1", "S1"),
      OlinkID = c("OID00001", "OID00002"),
      Group = c("A", "B"),
      NPX = c(1, 2)
    )

    expect_error(
      check_stats_effect_data(
        df = invalid_data,
        effects = "Group",
        outcome = "NPX",
        check_log = check_log
      ),
      "do not have a unique level"
    )
  }
)

test_that(
  "inform_stats_model emits each applicable message",
  {
    messages <- capture_messages(
      inform_stats_model(
        add_main_effects = "Treatment",
        removed_sampleids = "S1",
        converted_vars = "Site",
        numeric_vars = c("Age", "Time"),
        variable = c("Treatment", "Time"),
        model_message = "Model fitted.",
        posthoc = TRUE
      )
    )

    expect_true(any(grepl("Missing main effects", messages)))
    expect_true(any(grepl("Samples removed", messages)))
    expect_true(any(grepl("converted from character", messages)))
    expect_true(any(grepl("treated as numeric", messages)))
    expect_true(any(grepl("Numeric variables post-hoc", messages)))
    expect_true(any(grepl("Model fitted", messages)))
    expect_no_message(inform_stats_model())
  }
)

test_that(
  "prepare_two_group_stats validates columns and grouping variables",
  {
    base_data <- data.frame(
      SampleID = c("S1", "S2"),
      OlinkID = rep("OID00001", 2L),
      NPX = c(1, 2),
      Group = c("A", "B")
    )

    expect_error(
      prepare_two_group_stats(
        df = base_data,
        variable = "Group",
        pair_id = "Pair",
        test_name = "Test"
      ),
      "Column.*Pair.*not found"
    )

    numeric_data <- base_data
    numeric_data[["Group"]] <- c(1, 2)
    expect_error(
      prepare_two_group_stats(
        df = numeric_data,
        variable = "Group",
        test_name = "Test"
      ),
      "neither factor nor character"
    )

    three_level_data <- rbind(
      base_data,
      data.frame(
        SampleID = "S3",
        OlinkID = "OID00001",
        NPX = 3,
        Group = "C"
      )
    )
    expect_error(
      suppressMessages(
        prepare_two_group_stats(
          df = three_level_data,
          variable = "Group",
          test_name = "Test"
        )
      ),
      "factor has 3 levels"
    )

    invalid_sample_data <- data.frame(
      SampleID = c("S1", "S1"),
      OlinkID = c("OID00001", "OID00002"),
      NPX = c(1, 2),
      Group = factor(c("A", "B"))
    )
    expect_error(
      prepare_two_group_stats(
        df = invalid_sample_data,
        variable = "Group",
        test_name = "Test"
      ),
      "do not have a unique level"
    )
  }
)

test_that(
  "prepare_two_group_stats filters missing values and invalid assays",
  {
    mock_get_check_npx <- function(df,
                                   check_log = NULL,
                                   preferred_names = NULL) {
      if (is.null(check_log)) {
        return(list(assay_na = character()))
      }
      return(check_log)
    }
    helper_env <- environment(prepare_two_group_stats)
    if (environmentIsLocked(helper_env)) {
      local_mocked_bindings(
        get_check_npx = mock_get_check_npx,
        .package = "OlinkAnalyze"
      )
    } else {
      rlang::local_bindings(
        get_check_npx = mock_get_check_npx,
        .env = helper_env
      )
    }

    missing_data <- data.frame(
      SampleID = c("S1", "S2", "S3"),
      OlinkID = rep("OID00001", 3L),
      NPX = c(1, 2, 3),
      Group = c("A", "B", NA_character_)
    )
    messages <- capture_messages(
      result <- prepare_two_group_stats(
        df = missing_data,
        variable = "Group",
        test_name = "Test"
      )
    )
    expect_true(any(grepl("missing variable levels", messages)))
    expect_true(any(grepl("converted from character", messages)))
    expect_identical(result$removed_sampleids, "S3")
    expect_true(is.factor(result$df$Group))
    expect_identical(result$variable_levels, c("A", "B"))

    assay_data <- tibble::tibble(
      SampleID = rep(c("S1", "S2"), 2L),
      OlinkID = rep(c("OID00001", "OID00002"), each = 2L),
      NPX = c(NA_real_, 2, 1, 2),
      Group = factor(rep(c("A", "B"), 2L))
    )
    expect_warning(
      result <- prepare_two_group_stats(
        df = assay_data,
        variable = "Group",
        check_log = list(assay_na = character()),
        test_name = "Test"
      ),
      "too few datapoints"
    )
    expect_identical(unique(result$df$OlinkID), "OID00002")

    expect_error(
      suppressWarnings(
        prepare_two_group_stats(
          df = assay_data[assay_data$OlinkID == "OID00001", ],
          variable = "Group",
          check_log = list(assay_na = character()),
          test_name = "Test"
        )
      ),
      "No assays pass the initial check"
    )

    result <- prepare_two_group_stats(
      df = assay_data,
      variable = "Group",
      check_log = list(assay_na = "OID00001"),
      test_name = "Test"
    )
    expect_identical(unique(result$df$OlinkID), "OID00002")
  }
)

test_that(
  "prepare_two_group_stats validates paired observations",
  {
    mock_get_check_npx <- function(df,
                                   check_log = NULL,
                                   preferred_names = NULL) {
      return(check_log)
    }
    helper_env <- environment(prepare_two_group_stats)
    if (environmentIsLocked(helper_env)) {
      local_mocked_bindings(
        get_check_npx = mock_get_check_npx,
        .package = "OlinkAnalyze"
      )
    } else {
      rlang::local_bindings(
        get_check_npx = mock_get_check_npx,
        .env = helper_env
      )
    }

    paired_data <- data.frame(
      SampleID = c("S1", "S2", "S3"),
      OlinkID = rep("OID00001", 3L),
      NPX = c(1, 2, 3),
      Group = factor(c("A", "B", "A")),
      Pair = c("P1", "P1", NA_character_)
    )
    messages <- capture_messages(
      result <- prepare_two_group_stats(
        df = paired_data,
        variable = "Group",
        pair_id = "Pair",
        check_log = list(assay_na = character()),
        test_name = "Test"
      )
    )
    expect_true(any(grepl("missing pair ID", messages)))
    expect_true(any(grepl("Converting data frame to tibble", messages)))
    expect_s3_class(result$df, "tbl_df")
    expect_identical(result$removed_sampleids, "S3")

    invalid_pairs <- paired_data
    invalid_pairs$Pair <- "P1"
    expect_error(
      suppressMessages(
        prepare_two_group_stats(
          df = invalid_pairs,
          variable = "Group",
          pair_id = "Pair",
          check_log = list(assay_na = character()),
          test_name = "Test"
        )
      ),
      "no more than 2 unique samples"
    )
  }
)

test_that(
  "check_lmer_plot_args validates plotting arguments",
  {
    expect_invisible(
      check_lmer_plot_args(
        variable = c("Treatment", "Time"),
        x_axis_variable = "Time",
        col_variable = "Treatment",
        dots = list()
      )
    )
    expect_invisible(
      check_lmer_plot_args(
        variable = "Treatment:Time",
        x_axis_variable = "Time",
        col_variable = NULL,
        dots = list(coloroption = list())
      )
    )
    expect_error(
      check_lmer_plot_args("Treatment", "Time", NULL, list()),
      "x axis variable"
    )
    expect_error(
      check_lmer_plot_args("Treatment", "Treatment", "Time", list()),
      "color variable"
    )
    expect_error(
      check_lmer_plot_args(
        "Treatment",
        "Treatment",
        NULL,
        list(coloroption = list(), other = TRUE)
      ),
      "accepts only one argument"
    )
    expect_error(
      check_lmer_plot_args(
        "Treatment",
        "Treatment",
        NULL,
        list(other = TRUE)
      ),
      "only the.*coloroption.*argument"
    )
  }
)

test_that(
  "prepare_stats_model_terms expands interactions and removes overlap",
  {
    expect_identical(
      prepare_stats_model_terms("Treatment", "Age"),
      list(
        variable = "Treatment",
        covariates = "Age",
        add_main_effects = NULL
      )
    )

    terms <- prepare_stats_model_terms(
      variable = "Treatment*Time",
      covariates = c("Site*Age", "Treatment")
    )
    expect_identical(terms$variable, c("Treatment", "Time"))
    expect_identical(terms$covariates, c("Site:Age", "Site", "Age"))
    expect_identical(terms$add_main_effects, c("Site", "Age"))
  }
)

test_that(
  "filter_stats_missing removes rows and tracks sample IDs",
  {
    input <- tibble::tibble(
      SampleID = c("S1", "S2", "S3"),
      Group = c("A", NA_character_, "B"),
      Age = c(NA_real_, 20, 30)
    )
    result <- filter_stats_missing(
      df = input,
      columns = c("Group", "Age"),
      sample_id = "SampleID"
    )
    expect_identical(result$df$SampleID, "S3")
    expect_identical(result$removed_sampleids, c("S2", "S1"))

    unchanged <- filter_stats_missing(input, character(), "SampleID")
    expect_identical(unchanged$df, input)
    expect_null(unchanged$removed_sampleids)
  }
)

test_that(
  "convert_stats_predictors records converted predictor types",
  {
    input <- tibble::tibble(
      Character = c("A", "B"),
      Factor = factor(c("A", "B")),
      Numeric = c(1, 2),
      Logical = c(TRUE, FALSE)
    )
    result <- convert_stats_predictors(input, names(input))

    expect_true(is.factor(result$df$Character))
    expect_identical(result$converted_vars, "Character")
    expect_identical(result$numeric_vars, "Numeric")
    expect_identical(result$factor_vars, c("Character", "Factor"))

    empty <- convert_stats_predictors(input, character())
    expect_null(empty$converted_vars)
    expect_null(empty$numeric_vars)
    expect_identical(empty$factor_vars, character())
  }
)

test_that(
  "build_stats_formula supports custom, fixed, and mixed formulas",
  {
    expect_identical(
      build_stats_formula(
        outcome = "NPX",
        variable = "Treatment",
        model_formula = "NPX~Site"
      ),
      "NPX~Site"
    )
    expect_identical(
      build_stats_formula("NPX", c("Treatment", "Time")),
      "NPX~Treatment*Time"
    )
    expect_identical(
      build_stats_formula(
        outcome = "NPX",
        variable = "Treatment",
        covariates = c("Age", "Site"),
        random = c("Subject", "Plate")
      ),
      "NPX~Treatment+Age+Site+(1|Subject)+(1|Plate)"
    )
  }
)

test_that(
  "get_stats_fixed_effects includes factor covariates",
  {
    input <- tibble::tibble(
      Treatment = factor(c("A", "B")),
      Site = factor(c("A", "B")),
      Age = c(20, 30)
    )
    expect_identical(
      get_stats_fixed_effects(input, "Treatment"),
      "Treatment"
    )
    expect_identical(
      get_stats_fixed_effects(
        input,
        variable = "Treatment",
        covariates = c("Site", "Age")
      ),
      c("Treatment", "Site")
    )
  }
)

test_that(
  "get_stats_covariate_terms includes reversed interactions",
  {
    expect_null(get_stats_covariate_terms(NULL))
    expect_identical(get_stats_covariate_terms("Age"), "Age")
    expect_identical(
      get_stats_covariate_terms(c("Age", "Site:Batch")),
      c("Age", "Site:Batch", "Batch:Site")
    )
  }
)

test_that(
  "filter_stats_olink_ids handles patterns and explicit exclusions",
  {
    input <- tibble::tibble(
      OlinkID = c("OID00001", "invalid", "OID00002"),
      value = 1:3
    )
    expect_identical(
      filter_stats_olink_ids(input)$OlinkID,
      c("OID00001", "OID00002")
    )
    expect_identical(
      filter_stats_olink_ids(input, invalid_ids = "OID00001")$OlinkID,
      c("invalid", "OID00002")
    )

    renamed <- dplyr::rename(input, ID = "OlinkID")
    expect_identical(
      filter_stats_olink_ids(renamed, olink_id = "ID")$ID,
      c("OID00001", "OID00002")
    )
  }
)

test_that(
  "resolve_stats_olink_ids supplies defaults only when needed",
  {
    input <- tibble::tibble(ID = c("OID00001", "OID00001", "OID00002"))
    expect_identical(
      resolve_stats_olink_ids(input, NULL, olink_id = "ID"),
      c("OID00001", "OID00002")
    )
    expect_identical(
      resolve_stats_olink_ids(input, character(), olink_id = "ID"),
      c("OID00001", "OID00002")
    )
    expect_identical(
      resolve_stats_olink_ids(input, "OID00002", olink_id = "ID"),
      "OID00002"
    )
  }
)

test_that(
  "filter_stats_assays applies inclusion, exclusion, and ordering",
  {
    input <- tibble::tibble(
      OlinkID = c("OID00001", "OID00002", "OID00003"),
      value = 1:3
    )
    expect_identical(filter_stats_assays(input), input)

    result <- filter_stats_assays(
      input,
      olinkid_list = c("OID00003", "OID00002"),
      assay_na = "OID00002",
      preserve_order = TRUE
    )
    expect_identical(as.character(result$OlinkID), "OID00003")
    expect_identical(
      levels(result$OlinkID),
      c("OID00003", "OID00002")
    )

    expect_identical(
      filter_stats_assays(input, assay_na = "OID00001")$OlinkID,
      c("OID00002", "OID00003")
    )
  }
)

test_that(
  "build_stats_effect_formula creates requested output type",
  {
    generated <- build_stats_effect_formula(c("Treatment", "Time"))
    expect_s3_class(generated, "formula")
    expect_identical(deparse(generated), "pairwise ~ Treatment + Time")

    expect_identical(
      build_stats_effect_formula(
        effect = "Treatment",
        effect_formula = "pairwise~Site",
        as_formula = FALSE
      ),
      "pairwise~Site"
    )
    expect_s3_class(
      build_stats_effect_formula(
        effect = "Treatment",
        effect_formula = "pairwise~Site"
      ),
      "formula"
    )
  }
)

test_that(
  "run_stats_emmeans returns means and contrasts",
  {
    skip_if_not_installed("emmeans")

    model_data <- transform(mtcars, cyl = factor(cyl))
    model <- stats::lm(mpg ~ cyl + wt, data = model_data)
    effect <- stats::as.formula("pairwise ~ cyl")

    means <- run_stats_emmeans(
      model = model,
      effect = effect,
      mean_return = TRUE,
      padjust_method = "tukey"
    )
    contrasts <- run_stats_emmeans(
      model = model,
      effect = effect,
      mean_return = FALSE,
      padjust_method = "tukey",
      lmer_df = "satterthwaite"
    )

    expect_s3_class(means, "data.frame")
    expect_true("emmean" %in% names(means))
    expect_s3_class(contrasts, "data.frame")
    expect_true("contrast" %in% names(contrasts))
  }
)

test_that(
  "significance helpers adjust p-values and label thresholds",
  {
    input <- tibble::tibble(
      probability = c(0.01, 0.2),
      method = c("first", "second")
    )
    result <- add_stats_significance(
      input,
      p_column = "probability",
      method = "BH"
    )

    expect_equal(
      result$Adjusted_pval,
      stats::p.adjust(input$probability, method = "BH")
    )
    expect_identical(
      result$Threshold,
      c("Significant", "Non-significant")
    )

    custom <- add_stats_threshold(input, p_column = "probability")
    expect_identical(
      custom$Threshold,
      c("Significant", "Non-significant")
    )
  }
)

test_that(
  "muffle_stats_warning suppresses only matching warnings",
  {
    expect_no_warning(
      withCallingHandlers(
        warning("known warning"),
        warning = function(warning) {
          return(muffle_stats_warning(warning, "known"))
        }
      )
    )
    expect_warning(
      withCallingHandlers(
        warning("keep warning"),
        warning = function(warning) {
          return(muffle_stats_warning(warning, "known"))
        }
      ),
      "keep warning"
    )
  }
)
