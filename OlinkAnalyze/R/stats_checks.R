#' Shared checks and messages for statistical functions
#'
#' @keywords internal
#' @noRd
check_stats_required <- function(present, message) {
  if (!all(present)) {
    cli::cli_abort(message, call = rlang::caller_env())
  }

  return(invisible(NULL))
}

#' @rdname check_stats_required
#' @keywords internal
#' @noRd
check_stats_formula <- function(model_formula) {
  if (inherits(model_formula, "formula")) {
    model_formula <- deparse(model_formula)
  }

  tryCatch(
    stats::as.formula(model_formula),
    error = function(error) {
      cli::cli_abort( # nolint: return_linter
        "{.code {model_formula}} is not a recognized formula.",
        call = rlang::caller_env()
      )
    }
  )

  return(gsub(pattern = " ", replacement = "", x = model_formula))
}

#' @rdname check_stats_required
#' @keywords internal
#' @noRd
parse_stats_effect_formula <- function(effect_formula,
                                       effect = NULL,
                                       effect_supplied = FALSE) {
  if (inherits(effect_formula, "formula")) {
    effect_formula <- deparse(effect_formula)
  }

  if (length(effect_formula) != 1L) {
    cli::cli_abort(
      paste0(
        "Unrecognized effect formula. Should be a character string of length",
        "1. If listing in the form c('A','B'), use the effects argument."
      ),
      call = rlang::caller_env()
    )
  }

  if (effect_supplied) {
    cli::cli_inform("effect_formula overriding effect argument.")
  }
  if (grepl(pattern = "~", x = effect_formula)) {
    effect_formula <- strsplit(x = effect_formula, split = "~")[[1L]][2L]
  }
  if (grepl(pattern = "\\||\\+|\\*", x = effect_formula)) {
    effect_formula <- strsplit(
      x = effect_formula,
      split = "\\||\\+|\\*"
    )[[1L]]
  }

  return(effect_formula)
}

#' @rdname check_stats_required
#' @keywords internal
#' @noRd
check_stats_effect_terms <- function(effect,
                                     variable,
                                     include_model_formula = FALSE) {
  effect_terms <- unique(unlist(strsplit(x = effect, split = ":")))
  variable_terms <- unique(unlist(strsplit(x = variable, split = "[\\*:]")))

  if (!all(effect_terms %in% variable_terms)) {
    suffix <- if (include_model_formula) { # nolint: object_usage_linter
      " or model formula"
    } else {
      ""
    }
    cli::cli_abort(
      "All effect terms must be included in the variable argument{suffix}.",
      call = rlang::caller_env()
    )
  }

  return(invisible(NULL))
}

#' @rdname check_stats_required
#' @keywords internal
#' @noRd
check_stats_control_assays <- function(df) {
  if ("AssayType" %in% names(df)) {
    control_assays <- df[df[["AssayType"]] != "assay", , drop = FALSE]
    reason <- "Assays with AssayType != \"assay\" should be excluded."
  } else {
    is_control <- stringr::str_detect(
      df[["Assay"]],
      stringr::regex("(?i)(?=.*\\bcontrol|ctrl\\b)(?!.*\\bCTRL\\b)")
    )
    control_assays <- df[is_control, , drop = FALSE]
    reason <- paste0(
      "Assays with \"control\" in their Assay field should be excluded."
    )
  }

  if (nrow(control_assays) > 0L) {
    assays <- unique(control_assays[["Assay"]]) # nolint: object_usage_linter
    cli::cli_abort(
      c(
        "Control assays have not been removed from the dataset.",
        "x" = reason,
        "i" = paste0(
          "The following {length(assays)} control assay{?s} {?was/were} ",
          "found: {.val {assays}}"
        )
      ),
      call = rlang::caller_env(),
      wrap = TRUE
    )
  }

  return(invisible(NULL))
}

#' @rdname check_stats_required
#' @keywords internal
#' @noRd
check_stats_effect_data <- function(df,
                                    effects,
                                    outcome,
                                    check_log,
                                    sample_id = check_log$col_names$sample_id,
                                    olink_id = check_log$col_names$olink_id) {
  assays_with_missing_level <- character()

  for (effect in effects) {
    current_assays <- df |>
      dplyr::filter(!(.data[[olink_id]] %in% check_log$assay_na)) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(c(olink_id, effect)))) |>
      dplyr::summarise(
        n = dplyr::n(),
        n_na = sum(is.na(.data[[outcome]])),
        .groups = "drop"
      ) |>
      dplyr::filter(.data[["n"]] == .data[["n_na"]]) |>
      dplyr::distinct(.data[[olink_id]]) |>
      dplyr::pull(.data[[olink_id]])

    if (length(current_assays) > 0L) {
      assays_with_missing_level <- c(
        assays_with_missing_level,
        current_assays
      )
      cli::cli_warn(
        paste0(
          "The assay(s) {.val {current_assays}} have only NA values in at ",
          "least one level of {.var {effect}} and will not be tested."
        )
      )
    }

    invalid_samples <- df |>
      dplyr::group_by(.data[[sample_id]]) |>
      dplyr::summarise(
        n_levels = dplyr::n_distinct(.data[[effect]], na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::filter(.data[["n_levels"]] > 1L) |>
      nrow()

    if (invalid_samples > 0L) {
      cli::cli_abort(
        paste0(
          "There are {invalid_samples} samples that do not have a unique ",
          "level for the effect {.var {effect}}. Only one level per sample ",
          "is allowed."
        ),
        call = rlang::caller_env()
      )
    }
  }

  return(unique(assays_with_missing_level))
}

#' @rdname check_stats_required
#' @keywords internal
#' @noRd
inform_stats_model <- function(add_main_effects = NULL,
                               removed_sampleids = NULL,
                               converted_vars = NULL,
                               numeric_vars = NULL,
                               variable = NULL,
                               model_message = NULL,
                               posthoc = FALSE) {
  if (length(add_main_effects) > 0L) {
    cli::cli_inform(
      paste0(
        "Missing main effects added to the model formula: ",
        "{paste(add_main_effects, collapse = ', ')}"
      )
    )
  }
  if (length(removed_sampleids) > 0L) {
    cli::cli_inform(
      paste0(
        "Samples removed due to missing variable or covariate levels: ",
        "{paste(removed_sampleids, collapse = ', ')}"
      )
    )
  }
  if (length(converted_vars) > 0L) {
    cli::cli_inform(
      paste0(
        "Variables and covariates converted from character to factors: ",
        "{paste(converted_vars, collapse = ', ')}"
      )
    )
  }
  if (length(numeric_vars) > 0L) {
    cli::cli_inform(
      paste0(
        "Variables and covariates treated as numeric: ",
        "{paste(numeric_vars, collapse = ', ')}"
      )
    )
  }
  if (posthoc && any(variable %in% numeric_vars)) {
    posthoc_vars <- numeric_vars[ # nolint: object_usage_linter
      numeric_vars %in% variable
    ]
    cli::cli_inform(
      paste0(
        "Numeric variables post-hoc performed using Mean and Mean + 1SD: ",
        "{paste(posthoc_vars, collapse = ', ')}"
      )
    )
  }
  if (!is.null(model_message)) {
    cli::cli_inform(model_message)
  }

  return(invisible(NULL))
}

#' @rdname check_stats_required
#' @keywords internal
#' @noRd
prepare_two_group_stats <- function(df,
                                    variable,
                                    pair_id = NULL,
                                    check_log = NULL,
                                    test_name) {
  if (!is.null(pair_id) && !(pair_id %in% names(df))) {
    cli::cli_abort("Column {.var {pair_id}} not found.")
  }

  df <- filter_stats_olink_ids(df)

  removed_sampleids <- unique(df[["SampleID"]][is.na(df[[variable]])])
  if (length(removed_sampleids) > 0L) {
    cli::cli_inform(
      paste0(
        "Samples removed due to missing variable levels: ",
        "{paste(removed_sampleids, collapse = ', ')}"
      )
    )
  }
  df <- df[!is.na(df[[variable]]), , drop = FALSE]

  if (!is.null(pair_id)) {
    missing_pair <- unique(df[["SampleID"]][is.na(df[[pair_id]])])
    if (length(missing_pair) > 0L) {
      cli::cli_inform(
        paste0(
          "Samples removed due to missing pair ID: ",
          "{paste(missing_pair, collapse = ', ')}"
        )
      )
    }
    df <- df[!is.na(df[[pair_id]]), , drop = FALSE]
    removed_sampleids <- unique(c(removed_sampleids, missing_pair))
  }

  if (is.character(df[[variable]])) {
    df[[variable]] <- factor(df[[variable]])
    cli::cli_inform(
      "Variable converted from character to factor: {variable}"
    )
  } else if (!is.factor(df[[variable]])) {
    cli::cli_abort(
      paste0(
        "The grouping variable {.var {variable}} is neither factor nor ",
        "character. Only character and factor variable types are allowed."
      )
    )
  }

  variable_levels <- levels(df[[variable]])
  if (length(variable_levels) != 2L) {
    cli::cli_abort(
      paste0(
        "The number of levels in the factor needs to be 2. Your factor has ",
        "{length(variable_levels)} levels."
      )
    )
  }

  invalid_samples <- df |>
    dplyr::group_by(.data[["SampleID"]]) |>
    dplyr::summarise(
      n_levels = dplyr::n_distinct(.data[[variable]], na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::filter(.data[["n_levels"]] > 1L) |>
    nrow()
  if (invalid_samples > 0L) {
    cli::cli_abort(
      paste0(
        "There are {invalid_samples} samples that do not have a unique level ",
        "for your variable. Only one level per sample is allowed."
      )
    )
  }

  check_log <- get_check_npx(df = df, check_log = check_log)
  assays_with_missing_level <- df |>
    dplyr::filter(!(.data[["OlinkID"]] %in% check_log$assay_na)) |>
    dplyr::group_by(.data[["OlinkID"]], .data[[variable]]) |>
    dplyr::summarise(
      n = dplyr::n(),
      n_na = sum(is.na(.data[["NPX"]])),
      .groups = "drop"
    ) |>
    dplyr::filter(.data[["n"]] == .data[["n_na"]]) |>
    dplyr::pull(.data[["OlinkID"]])
  if (length(assays_with_missing_level) > 0L) {
    cli::cli_warn(
      paste0(
        "The assays {.val {assays_with_missing_level}} have too few ",
        "datapoints in one level of the factor. They will not be tested."
      )
    )
  }

  df <- df |>
    dplyr::filter(
      !(.data[["OlinkID"]] %in% check_log$assay_na),
      !(.data[["OlinkID"]] %in% assays_with_missing_level)
    )
  if (nrow(df) == 0L) {
    cli::cli_abort(
      "No assays pass the initial check. {test_name} will not be performed."
    )
  }

  if (!is.null(pair_id)) {
    if (!check_is_tibble(x = df, error = FALSE)) {
      cli::cli_inform("Converting data frame to tibble.")
      df <- dplyr::as_tibble(df)
    }
    pair_counts <- df |>
      dplyr::group_by(.data[["OlinkID"]], .data[[pair_id]]) |>
      dplyr::summarise(n = dplyr::n(), .groups = "drop")
    if (!all(pair_counts[["n"]] <= 2L)) {
      invalid_pairs <- unique( # nolint: object_usage_linter
        pair_counts[[pair_id]][pair_counts[["n"]] > 2L]
      )
      cli::cli_abort(
        paste0(
          "Each pair identifier must identify no more than 2 unique samples. ",
          "Check pairs: {.val {invalid_pairs}}"
        )
      )
    }
  }

  return(list(
    df = df,
    variable_levels = variable_levels,
    check_log = check_log,
    removed_sampleids = removed_sampleids
  ))
}

#' @rdname check_stats_required
#' @keywords internal
#' @noRd
check_lmer_plot_args <- function(variable,
                                 x_axis_variable,
                                 col_variable,
                                 dots) {
  variable_terms <- unique(unlist(strsplit(variable, "[\\*:]")))
  if (!all(x_axis_variable %in% variable_terms)) {
    cli::cli_abort(
      "The x axis variable must be included in the {.arg variable} argument."
    )
  }
  if (!is.null(col_variable) && !all(col_variable %in% variable_terms)) {
    cli::cli_abort(
      "The color variable must be included in the {.arg variable} argument."
    )
  }
  if (length(dots) > 1L) {
    cli::cli_abort(
      "{.arg ...} accepts only one argument; received {.val {names(dots)}}."
    )
  }
  if (length(dots) == 1L && !identical(names(dots), "coloroption")) {
    cli::cli_abort(
      paste0(
        "{.arg ...} accepts only the {.arg coloroption} argument; ",
        "received {.arg {names(dots)}}."
      )
    )
  }

  return(invisible(NULL))
}

#' @rdname check_stats_required
#' @keywords internal
#' @noRd
prepare_stats_model_terms <- function(variable, covariates = NULL) {
  variable <- gsub(pattern = "\\*", replacement = ":", x = variable)
  if (!is.null(covariates)) {
    covariates <- gsub(
      pattern = "\\*",
      replacement = ":",
      x = covariates
    )
  }

  add_main_effects <- NULL
  if (any(grepl(pattern = ":", x = covariates))) {
    covariate_terms <- unlist(strsplit(x = covariates, split = ":"))
    add_main_effects <- c(
      add_main_effects,
      setdiff(x = covariate_terms, y = covariates)
    )
    covariates <- union(x = covariates, y = add_main_effects)
  }
  if (any(grepl(pattern = ":", x = variable))) {
    variable_terms <- unlist(strsplit(x = variable, split = ":"))
    add_main_effects <- c(
      add_main_effects,
      setdiff(x = variable_terms, y = variable)
    )
    variable <- union(x = variable, y = variable_terms)
    variable <- variable[!grepl(pattern = ":", x = variable)]
  }

  covariates <- setdiff(x = covariates, y = variable)
  add_main_effects <- setdiff(x = add_main_effects, y = variable)

  return(list(
    variable = variable,
    covariates = covariates,
    add_main_effects = add_main_effects
  ))
}

#' @rdname check_stats_required
#' @keywords internal
#' @noRd
filter_stats_missing <- function(df, columns, sample_id) {
  removed_sampleids <- NULL

  for (column in columns) {
    removed_sampleids <- unique(c(
      removed_sampleids,
      df[[sample_id]][is.na(df[[column]])]
    ))
    df <- df[!is.na(df[[column]]), ]
  }

  return(list(df = df, removed_sampleids = removed_sampleids))
}

#' @rdname check_stats_required
#' @keywords internal
#' @noRd
convert_stats_predictors <- function(df, columns) {
  converted_vars <- NULL
  numeric_vars <- NULL

  for (column in columns) {
    if (is.character(df[[column]])) {
      df[[column]] <- factor(df[[column]])
      converted_vars <- c(converted_vars, column)
    } else if (is.numeric(df[[column]])) {
      numeric_vars <- c(numeric_vars, column)
    }
  }

  factor_vars <- columns[vapply(
    columns,
    function(column) is.factor(df[[column]]),
    logical(1L)
  )]

  return(list(
    df = df,
    converted_vars = converted_vars,
    numeric_vars = numeric_vars,
    factor_vars = factor_vars
  ))
}

#' @rdname check_stats_required
#' @keywords internal
#' @noRd
build_stats_formula <- function(outcome,
                                variable,
                                covariates = NULL,
                                random = NULL,
                                model_formula = NULL) {
  if (!is.null(model_formula)) {
    return(model_formula)
  }

  formula_terms <- paste(variable, collapse = "*")
  if (length(covariates) > 0L) {
    formula_terms <- paste(
      formula_terms,
      paste(covariates, collapse = "+"),
      sep = "+"
    )
  }
  if (length(random) > 0L) {
    random_terms <- paste0("(1|", random, ")", collapse = "+")
    formula_terms <- paste(formula_terms, random_terms, sep = "+")
  }

  return(paste0(outcome, "~", formula_terms))
}

#' @rdname check_stats_required
#' @keywords internal
#' @noRd
get_stats_fixed_effects <- function(df, variable, covariates = NULL) {
  if (is.null(covariates)) {
    return(variable)
  }

  factor_vars <- names(df)[vapply(df, is.factor, logical(1L))]
  return(c(variable, intersect(x = covariates, y = factor_vars)))
}

#' @rdname check_stats_required
#' @keywords internal
#' @noRd
get_stats_covariate_terms <- function(covariates) {
  if (is.null(covariates) || !any(grepl(pattern = ":", x = covariates))) {
    return(covariates)
  }

  interactions <- covariates[grepl(pattern = ":", x = covariates)]
  reversed_interactions <- sub(
    pattern = "(.*)\\:(.*)$",
    replacement = "\\2:\\1",
    x = interactions
  )
  return(c(covariates, reversed_interactions))
}

#' @rdname check_stats_required
#' @keywords internal
#' @noRd
filter_stats_olink_ids <- function(df,
                                   olink_id = "OlinkID",
                                   invalid_ids = NULL) {
  if (length(invalid_ids) > 0L) {
    return(df[!(df[[olink_id]] %in% invalid_ids), ])
  }

  return(df[
    stringr::str_detect(df[[olink_id]], pattern = "OID[0-9]{5}"),
  ])
}

#' @rdname check_stats_required
#' @keywords internal
#' @noRd
resolve_stats_olink_ids <- function(df, olinkid_list, olink_id = "OlinkID") {
  if (is.null(olinkid_list) || length(olinkid_list) == 0L) {
    return(unique(df[[olink_id]]))
  }

  return(olinkid_list)
}

#' @rdname check_stats_required
#' @keywords internal
#' @noRd
filter_stats_assays <- function(df,
                                olinkid_list = NULL,
                                assay_na = character(),
                                olink_id = "OlinkID",
                                preserve_order = FALSE) {
  if (!is.null(olinkid_list)) {
    df <- df[df[[olink_id]] %in% olinkid_list, ]
  }
  if (length(assay_na) > 0L) {
    df <- df[!(df[[olink_id]] %in% assay_na), ]
  }
  if (preserve_order && !is.null(olinkid_list)) {
    df[[olink_id]] <- factor(df[[olink_id]], levels = olinkid_list)
  }

  return(df)
}

#' @rdname check_stats_required
#' @keywords internal
#' @noRd
build_stats_effect_formula <- function(effect,
                                       effect_formula = NULL,
                                       as_formula = TRUE) {
  if (is.null(effect_formula)) {
    effect_formula <- paste0("pairwise~", paste(effect, collapse = "+"))
  }
  if (as_formula) {
    return(stats::as.formula(effect_formula))
  }

  return(effect_formula)
}

#' @rdname check_stats_required
#' @keywords internal
#' @noRd
run_stats_emmeans <- function(model,
                              effect,
                              mean_return,
                              padjust_method,
                              lmer_df = NULL) {
  emmeans_args <- list(
    object = model,
    specs = effect,
    cov.reduce = function(x) {
      return(round(
        x = c(mean(x), mean(x) + stats::sd(x)),
        digits = 4L
      ))
    },
    infer = c(TRUE, TRUE),
    adjust = padjust_method
  )
  if (!is.null(lmer_df)) {
    emmeans_args$lmer.df <- lmer_df
  }

  emmeans_result <- rlang::exec(emmeans::emmeans, !!!emmeans_args)
  result_type <- if (mean_return) "emmeans" else "contrasts"
  return(data.frame(
    emmeans_result[[result_type]],
    stringsAsFactors = FALSE
  ))
}

#' @rdname check_stats_required
#' @keywords internal
#' @noRd
add_stats_significance <- function(df,
                                   p_column = "p.value",
                                   method = "fdr") {
  df <- df |>
    dplyr::mutate(
      Adjusted_pval = stats::p.adjust(
        p = .data[[p_column]],
        method = !!method
      )
    ) |>
    add_stats_threshold()

  return(df)
}

#' @rdname check_stats_required
#' @keywords internal
#' @noRd
add_stats_threshold <- function(df, p_column = "Adjusted_pval") {
  df <- df |>
    dplyr::mutate(
      Threshold = dplyr::if_else(
        .data[[p_column]] < 0.05,
        "Significant",
        "Non-significant"
      )
    )

  return(df)
}

#' @rdname check_stats_required
#' @keywords internal
#' @noRd
muffle_stats_warning <- function(warning, patterns) {
  should_muffle <- any(vapply(
    patterns,
    function(pattern) grepl(pattern = pattern, x = warning),
    logical(1L)
  ))
  if (should_muffle) {
    invokeRestart("muffleWarning")
  }

  return(invisible(NULL))
}
