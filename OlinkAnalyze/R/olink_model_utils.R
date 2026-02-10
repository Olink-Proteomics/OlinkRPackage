#' Utility functions for olink_anova and olink_lmer
#'
#' @description
#' Internal helper functions to reduce code duplication between olink_anova and
#' olink_lmer. These functions handle common input validation, data processing,
#' and formula manipulation tasks.
#'
#' @keywords internal
#' @name olink_model_utils

#' Expand crossed formula terms and add main effects
#'
#' @description
#' Processes variable and covariate strings to handle crossed analysis notation
#' (`:` and `*`). Expands interaction terms and adds necessary main effects.
#'
#' @param variable Character vector of variable names.
#' @param covariates Character vector of covariate names or NULL.
#'
#' @return A list with three elements:
#'   \item{variable}{Processed variable vector with main effects}
#'   \item{covariates}{Processed covariates vector with main effects}
#'   \item{add.main.effects}{Vector of main effects that were added}
#'
#' @keywords internal
#'
expand_crossed_formula_terms <- function(variable, covariates = NULL) {

  # Allow for :/* notation in covariates
  variable <- gsub(pattern = "\\*", replacement = ":", x = variable)
  if (!is.null(covariates)) {
    covariates <- gsub(pattern = "\\*", replacement = ":", x = covariates)
  }

  add.main.effects <- NULL # nolint: object_name_linter
  if (any(grepl(":", covariates))) {
    tmp <- unlist(strsplit(covariates, ":"))
    add.main.effects <- c(add.main.effects, setdiff(tmp, covariates)) # nolint: object_name_linter
    covariates <- union(covariates, add.main.effects)
  }
  if (any(grepl(":", variable))) {
    tmp <- unlist(strsplit(variable, ":"))
    add.main.effects <- c(add.main.effects, setdiff(tmp, variable)) # nolint: object_name_linter
    variable <- union(variable, unlist(strsplit(variable, ":")))
    variable <- variable[!grepl(":", variable)]
  }
  # If variable is in both variable and covariate, keep it in variable or
  # will get removed from final table
  covariates <- setdiff(x = covariates, y = variable)
  add.main.effects <- setdiff(x = add.main.effects, y = variable) # nolint: object_name_linter

  return(list(
    variable = variable,
    covariates = covariates,
    add.main.effects = add.main.effects
  ))
}

#' Remove samples with NA values in specified variables
#'
#' @description
#' Removes rows from the dataframe where any of the specified variables have NA
#' values. Tracks which SampleIDs were removed.
#'
#' @param df Data frame containing the data.
#' @param variable_testers Character vector of variable names to check for NAs.
#'
#' @return A list with two elements:
#'   \item{df}{The filtered data frame}
#'   \item{removed.sampleids}{Vector of SampleIDs that were removed}
#'
#' @keywords internal
#'
remove_na_samples <- function(df, variable_testers) {

  # Remove rows where variables or covariate is NA (can't include in
  # analysis anyway)
  removed.sampleids <- NULL # nolint: object_name_linter
  for (i in variable_testers) {
    removed.sampleids <- c(removed.sampleids, # nolint: object_name_linter
                           df$SampleID[is.na(df[[i]])]) |>
      unique()
    df <- df[!is.na(df[[i]]), ]
  }

  return(list(
    df = df,
    removed.sampleids = removed.sampleids
  ))
}

#' Convert character variables to factors
#'
#' @description
#' Converts character columns to factors and tracks which variables were
#' converted and which are numeric.
#'
#' @param df Data frame containing the data.
#' @param variable_testers Character vector of variable names to check.
#'
#' @return A list with three elements:
#'   \item{df}{The data frame with converted factors}
#'   \item{converted.vars}{Vector of variable names that were converted}
#'   \item{num.vars}{Vector of variable names that are numeric}
#'
#' @keywords internal
#'
convert_vars_to_factors <- function(df, variable_testers) {

  # Convert character vars to factor
  converted.vars <- NULL # nolint: object_name_linter
  num.vars <- NULL # nolint: object_name_linter
  for (i in variable_testers) {
    if (is.character(df[[i]])) {
      df[[i]] <- factor(df[[i]])
      converted.vars <- c(converted.vars, i) # nolint: object_name_linter
    } else if (is.numeric(df[[i]])) {
      num.vars <- c(num.vars, i) # nolint: object_name_linter
    }
  }

  return(list(
    df = df,
    converted.vars = converted.vars,
    num.vars = num.vars
  ))
}

#' Detect assays with all NAs in at least one level of a factor
#'
#' @description
#' Identifies assays that have all NA values for the outcome variable in at
#' least one level of a factor variable. Issues warnings for such assays.
#'
#' @param df Data frame containing the data.
#' @param single_fixed_effects Character vector of factor variable names.
#' @param check_log Named list from check_npx().
#' @param outcome Character name of the outcome variable.
#'
#' @return Character vector of OlinkIDs that have all NAs in at least one level.
#'
#' @keywords internal
#'
detect_assays_with_na_levels <- function(df,
                                          single_fixed_effects,
                                          check_log,
                                          outcome = "NPX") {

  # Not testing assays that have all NA:s in one level
  # Every sample needs to have a unique level of the factor

  nas_in_var <- character(0L)

  for (effect in single_fixed_effects) {

    current_nas <- df |>
      dplyr::filter( # Exclude assays that have all NA:s
        !(.data[["OlinkID"]] %in% check_log$assay_na)
      ) |>
      dplyr::group_by(
        dplyr::across(
          dplyr::all_of(
            c("OlinkID", effect)
          )
        )
      ) |>
      dplyr::summarise(
        n = dplyr::n(),
        n_na = sum(is.na(.data[[outcome]])),
        .groups = "drop"
      ) |>
      dplyr::filter(
        .data[["n"]] == .data[["n_na"]]
      ) |>
      dplyr::distinct(
        .data[["OlinkID"]]
      ) |>
      dplyr::pull(
        .data[["OlinkID"]]
      )

    if (length(current_nas) > 0L) {

      nas_in_var <- c(nas_in_var, current_nas)

      warning(
        paste0(
          "The assay(s) ", current_nas,
          " has only NA:s in at least one level of ", effect,
          ". It will not be tested."
        ),
        call. = FALSE
      )
    }
  }

  return(nas_in_var)
}

#' Validate that samples have unique factor levels
#'
#' @description
#' Checks that each sample has only one level for each factor variable. Throws
#' an error if any sample has more than one level.
#'
#' @param df Data frame containing the data.
#' @param single_fixed_effects Character vector of factor variable names.
#'
#' @return NULL (invisibly). Throws an error if validation fails.
#'
#' @keywords internal
#'
validate_unique_levels_per_sample <- function(df, single_fixed_effects) {

  for (effect in single_fixed_effects) {

    n_samples_w_more_than_1_level <- df |>
      dplyr::group_by(
        dplyr::across(
          dplyr::all_of(
            c("SampleID")
          )
        )
      ) |>
      dplyr::summarise(
        n_levels = dplyr::n_distinct(.data[[effect]], na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::filter(
        .data[["n_levels"]] > 1L
      ) |>
      nrow()

    if (n_samples_w_more_than_1_level > 0L) {
      stop(
        paste0(
          "There are ", n_samples_w_more_than_1_level,
          " samples that do not have a unique level for the effect ",
          effect, ". Only one level per sample is allowed."
        )
      )
    }
  }

  invisible(NULL)
}

#' Build covariate filter string with reversed interaction terms
#'
#' @description
#' Processes covariate strings to include reversed interaction terms for
#' filtering results.
#'
#' @param covariates Character vector of covariate names or NULL.
#'
#' @return Character vector with original and reversed interaction terms.
#'
#' @keywords internal
#'
build_covariate_filter_string <- function(covariates) {

  if (!is.null(covariates) && any(grepl(":", covariates))) {
    covariate_filter_string <- covariates[stringr::str_detect(covariates, ":")]
    covariate_filter_string <- sub(
      pattern = "(.*)\\:(.*)$",
      replacement = "\\2:\\1",
      x = covariate_filter_string
    )
    covariate_filter_string <- c(covariates, covariate_filter_string)
  } else {
    covariate_filter_string <- covariates
  }

  return(covariate_filter_string)
}

#' Print verbose messages about model processing
#'
#' @description
#' Prints informative messages about data processing steps if verbose is TRUE.
#'
#' @param verbose Boolean indicating whether to print messages.
#' @param add.main.effects Character vector of added main effects or NULL.
#' @param removed.sampleids Character vector of removed sample IDs or NULL.
#' @param converted.vars Character vector of converted variable names or NULL.
#' @param num.vars Character vector of numeric variable names or NULL.
#' @param formula_string Character string of the final model formula.
#' @param model_type Character string indicating model type (e.g., "ANOVA", "lmer").
#'
#' @return NULL (invisibly). Prints messages as side effect.
#'
#' @keywords internal
#'
print_verbose_messages <- function(verbose,
                                    add.main.effects,
                                    removed.sampleids,
                                    converted.vars,
                                    num.vars,
                                    formula_string,
                                    model_type = "model") {

  if (verbose) {
    if (!is.null(add.main.effects) && length(add.main.effects) > 0L) {
      message(
        "Missing main effects added to the model formula: ",
        paste(add.main.effects, collapse = ", ")
      )
    }
    if (!is.null(removed.sampleids) && length(removed.sampleids) > 0L) {
      message(
        "Samples removed due to missing variable or covariate levels: ",
        paste(removed.sampleids, collapse = ", ")
      )
    }
    if (!is.null(converted.vars)) {
      message(
        "Variables and covariates converted from character to factors: ",
        paste(converted.vars, collapse = ", ")
      )
    }
    if (!is.null(num.vars)) {
      message(
        "Variables and covariates treated as numeric: ",
        paste(num.vars, collapse = ", ")
      )
    }
    message(model_type, formula_string)
  }

  invisible(NULL)
}
