#' Function that performs a linear mixed model per protein.
#'
#' @description
#' Fits a linear mixed effects model for every protein (by OlinkID) in every
#' panel, using `lmerTest::lmer` and `stats::anova`. The function handles both
#' factor and numerical variables and potential covariates.
#'
#' @details
#' Samples that have no variable information or missing factor levels are
#' automatically removed from the analysis (specified in a message if
#' `verbose = TRUE`). Character columns in the input dataset are automatically
#' converted to factors (specified in a message if `verbose = TRUE`). Numerical
#' variables are not converted to factors. If a numerical variable is to be used
#' as a factor, this conversion needs to be done on the dataset before the
#' function call.
#'
#' Crossed analysis, i.e. `A*B` formula notation, is inferred from the variable
#' argument in the following cases:
#' \itemize{
#'   \item c('A','B')
#'   \item c('A:B')
#'   \item c('A:B', 'B') or c('A:B', 'A')
#' }
#'
#' Inference is specified in a message if `verbose = TRUE`.
#'
#' For covariates, crossed analyses need to be specified explicitly, i.e. two
#' main effects will not be expanded with a `c('A','B')` notation. Main effects
#' present in the variable takes precedence.  The random variable only takes
#' main effects. The formula notation of the final model is specified in a
#' message if `verbose = TRUE`.
#'
#' Output p-values are adjusted by `stats::p.adjust` according to the
#' Benjamini-Hochberg method (“fdr”). Adjusted p-values are logically evaluated
#' towards adjusted `p-value<0.05`. Model terms specified as covariates are
#' not included in the adjusted p-value calculation and are not evaluated
#' towards the significance threshold, but are included in the output table
#' if `return.covariates = TRUE`.
#'
#' If the `model_formula` argument is used, all model terms will be tested
#' and included in the results. When using a model formula, the `covariates`
#' argument can be set to specify terms that should be excluded from the
#' adjusted p-value calculation and significance threshold evaluation.
#'
#' @param df NPX data frame in long format with at least protein name (Assay),
#' OlinkID, UniProt, 1-2 variables with at least 2 levels.
#' @param variable Single character value or character array. Variables to test.
#' If `length > 1`, the included variable names will be used in crossed
#' analyses. Also takes ':' or '*' notation.
#' @param check_log Optional named list returned by [`check_npx()`]. If `NULL`,
#' an attached `check_log` is used when present; otherwise [`check_npx()`] will
#' be run internally using `df`.
#' @param outcome Character. The dependent variable. Default: NPX.
#' @param random Single character value or character array.
#' @param covariates Single character value or character array. Default: `NULL`.
#' Covariates to include. Takes ':' or '*' notation. Crossed analysis will not
#' be inferred from main effects.
#' @param model_formula (optional) Symbolic description of the model to be
#' fitted in standard formula notation (e.g. `NPX~A*B + (1|ID)`). If provided,
#' this will override the `outcome`, `variable` and `covariates`.
#' arguments. Can be a string or of class \code{stats::formula()}.
#' @param return.covariates Boolean. Default: FALSE. Returns results for the
#' covariates. Note: Adjusted p-values will be NA for the covariates.
#' @param verbose Boolean. Default: TRUE. If information about removed samples,
#'  factor conversion and final model formula is to be printed to the console.
#'
#' @return A "tibble" containing the results of fitting the linear mixed effects
#' model to every protein by OlinkID, ordered by ascending p-value. Columns
#' include:
#' \itemize{
#'   \item{Assay:} "character" Protein symbol
#'   \item{OlinkID:} "character" Olink specific ID
#'   \item{UniProt:} "character" UniProt ID
#'   \item{Panel:} "character" Name of Olink Panel
#'   \item{term:} "character" term in model
#'   \item{sumsq:} "numeric" sum of square
#'   \item{meansq:} "numeric" mean of square
#'   \item{NumDF:} "integer" numerator of degrees of freedom
#'   \item{DenDF:} "numeric" denominator of decrees of freedom
#'   \item{statistic:} "numeric" value of the statistic
#'   \item{p.value:} "numeric" nominal p-value
#'   \item{Adjusted_pval:} "numeric" adjusted p-value for the test
#'   (Benjamini&Hochberg)
#'   \item{Threshold:} "character" if adjusted p-value is significant or not
#'   (< 0.05)
#' }
#'
#' @export
#'
#' @examples
#' \donttest{
#' if (rlang::is_installed(pkg = c("lme4", "lmerTest", "broom"))) {
#'   #data
#'   npx_df <- OlinkAnalyze::npx_data1 |>
#'     dplyr::filter(
#'       !grepl(
#'         pattern = "control|ctrl",
#'         x = .data[["SampleID"]],
#'         ignore.case = TRUE
#'       )
#'     )
#'
#'   # Results in model NPX ~ Time * Treatment + (1 | Subject) + (1 | Site)
#'   lmer_results <- OlinkAnalyze::olink_lmer(
#'     df = npx_df,
#'     variable = c("Time", "Treatment"),
#'     random = c("Subject", "Site")
#'   )
#' }
#' }
#'
olink_lmer <- function(df,
                       variable,
                       check_log = NULL,
                       outcome = "NPX",
                       random,
                       covariates = NULL,
                       model_formula,
                       return.covariates = FALSE, # nolint: object_name_linter
                       verbose = TRUE) {

  # Check if all required libraries for this function are installed
  rlang::check_installed(
    pkg = c("lme4", "lmerTest", "broom"),
    call = rlang::caller_env()
  )


  if (!missing(model_formula)) {
    model_formula <- check_stats_formula(model_formula)

    # If variable or random were included, message that they will
    # not be used as model_formula is provided
    if (!missing(variable) || !missing(random)) {
      cli::cli_inform(
        paste("model_formula overriding variable and random",
              "arguments.")
      )
    }

    # Parse formula so checks on the variable, outcome and random objects can
    # continue as usual
    # Random portion of formula
    splt_random <- stringr::str_extract(string = model_formula,
                                        pattern = "(?<=\\().*(?=\\))")
    splt_random <- strsplit(x = splt_random, split = "\\+|~|\\*|:|\\|")[[1L]]

    if (any(grepl(pattern = "-1|1", x = splt_random))) {
      splt_random <- splt_random[-grep(pattern = "-1|1", x = splt_random)]
    }
    random <- splt_random

    # Fixed effects portion of formula
    splt_form <- gsub(pattern = "\\s*\\([^\\)]+\\)",
                      replacement = "",
                      x = model_formula)
    splt_form <- strsplit(x = splt_form, split = c("\\+|~|\\*|:"))[[1L]]

    if ("-1" %in% splt_form) {
      splt_form <- splt_form[-which(splt_form == "-1")]
    }

    # Check if covariate was specified and remove from variables
    if (!missing(covariates)) {

      # Check if covariate exists in formula
      if (all(covariates %in% splt_form[-1L])) {

        outcome <- splt_form[1L]
        variable <- setdiff(splt_form[-1L], covariates)

      } else {

        cli::cli_abort(
          c(
            "x" = "Covariate{?s} {.val {setdiff(covariates,splt_form[-1L])}}
            {?is/are} not present in the model formula!",
            "i" = "Expected {.or {.val {splt_form[-1L]}}}."
          ),
          call = rlang::caller_env(),
          wrap = FALSE
        )
      }

    } else {
      # If no covariate specified, treat all terms as variables
      outcome <- splt_form[1L]
      variable <- splt_form[-1L]
      covariates <- NULL
    }


  }

  check_stats_required(
    present = c(!missing(df), !missing(variable), !missing(random)),
    message = "The df and variable and random arguments need to be specified."
  )

  # Check data format
  check_log <- get_check_npx(df = df, check_log = check_log)

  lmer_result <- withCallingHandlers(
    {
      df <- filter_stats_olink_ids(
        df = df,
        olink_id = check_log$col_names$olink_id,
        invalid_ids = check_log$oid_invalid
      )

      model_terms <- prepare_stats_model_terms(
        variable = variable,
        covariates = covariates
      )
      variable <- model_terms$variable
      covariates <- model_terms$covariates
      add_main_effects <- model_terms$add_main_effects

      # Variables to be checked
      variable_testers <- intersect(x = c(variable, covariates, random),
                                    y = names(df))

      filtered_data <- filter_stats_missing(
        df = df,
        columns = variable_testers,
        sample_id = check_log$col_names$sample_id
      )
      df <- filtered_data$df
      removed_sampleids <- filtered_data$removed_sampleids

      converted_data <- convert_stats_predictors(df, variable_testers)
      df <- converted_data$df
      converted_vars <- converted_data$converted_vars
      num_vars <- converted_data$numeric_vars

      single_fixed_effects <- get_stats_fixed_effects(
        df = df,
        variable = variable,
        covariates = covariates
      )
      nas_in_var <- check_stats_effect_data(
        df = df,
        effects = single_fixed_effects,
        outcome = outcome,
        check_log = check_log
      )

      formula_string <- build_stats_formula(
        outcome = outcome,
        variable = variable,
        covariates = covariates,
        random = random,
        model_formula = if (missing(model_formula)) NULL else model_formula
      )

      if (verbose) {
        inform_stats_model(
          add_main_effects = add_main_effects,
          removed_sampleids = removed_sampleids,
          converted_vars = converted_vars,
          numeric_vars = num_vars,
          model_message = paste0(
            "Linear mixed effects model fit to each assay:", formula_string
          )
        )
      }

      covariate_filter_string <- get_stats_covariate_terms(covariates)

      ##make LMM
      lmer_model <- df |>
        # Exclude assays that have all NA:s
        dplyr::filter(
          !(.data[[check_log$col_names$olink_id]] %in% check_log$assay_na)
        ) |>
        dplyr::filter(
          !(.data[[check_log$col_names$olink_id]] %in% .env[["nas_in_var"]])
        ) |>
        dplyr::group_by(
          dplyr::across(
            dplyr::all_of(
              c(check_log$col_names$assay,
                check_log$col_names$olink_id,
                check_log$col_names$uniprot,
                check_log$col_names$panel)
            )
          )
        ) |>
        dplyr::group_modify(
          .f = ~ broom::tidy(
            x = stats::anova(
              object = single_lmer(
                data = .x,
                formula_string = formula_string
              ),
              type = "III",
              ddf = "Satterthwaite"
            )
          )
        ) |>
        dplyr::ungroup() |>
        dplyr::mutate(
          covariates = .data[["term"]] %in% .env[["covariate_filter_string"]]
        ) |>
        dplyr::group_by(
          .data[["covariates"]]
        ) |>
        add_stats_significance() |>
        dplyr::mutate(
          dplyr::across(
            dplyr::all_of(
              c("Adjusted_pval", "Threshold")
            ),
            ~ dplyr::if_else(.data[["covariates"]], NA, .x)
          )
        ) |>
        dplyr::ungroup() |>
        dplyr::select(
          -dplyr::all_of(
            c("covariates")
          )
        ) |>
        dplyr::arrange(
          .data[["p.value"]]
        )

      if (return.covariates) {
        return(lmer_model)
      } else {
        return(
          lmer_model |>
            dplyr::filter(
              !(.data[["term"]] %in% .env[["covariate_filter_string"]])
            )
        )
      }

    },
    warning = function(w) {
      return(muffle_stats_warning(
        warning = w,
        patterns = c(
          "not\\s+recognized\\s+or\\s+transformed",
          utils::glob2rx("*contains implicit NA, consider using*")
        )
      ))
    }
  )

  return(lmer_result)
}

#' Internal LMER function
#'
#' @param data grouped data frame
#' @param formula_string anova formula
#'
#' @return lmer results
#'
#' @noRd
#'
single_lmer <- function(data, formula_string) {

  out_model <- tryCatch(
    lmerTest::lmer(
      formula = stats::as.formula(formula_string),
      data = data,
      REML = FALSE,
      control = lme4::lmerControl(
        check.conv.singular = "ignore"
      )
    ),
    warning = function(w) {
      return(
        lmerTest::lmer(
          formula = stats::as.formula(object = formula_string),
          data = data,
          REML = FALSE,
          control = lme4::lmerControl(
            optimizer = "Nelder_Mead",
            check.conv.singular = "ignore"
          )
        )
      )
    }
  )

  if (inherits(out_model, "lmerModLmerTest")) {
    return(out_model)
  } else {
    cli::cli_abort( # nolint: return_linter
      "Convergence issue not caught by {.fn single_lmer}."
    )
  }
}

#' Function which performs a linear mixed model posthoc per protein.
#'
#' @description
#' Similar to \code{\link{olink_lmer}} but performs a post-hoc analysis based on
#' a linear mixed model effects model using \code{lmerTest::lmer} and
#' \code{emmeans::emmeans} on proteins. See \code{\link{olink_lmer}} for details
#' of input notation.
#'
#' @details
#' The function handles both factor and numerical variables and/or covariates.
#' Differences in estimated marginal means are calculated for all pairwise
#' levels of a given variable. Degrees of freedom are estimated using
#' Satterthwaite’s approximation. The posthoc test for a numerical variable
#' compares the difference in means of the outcome variable (default:
#' \code{NPX}) for 1 standard deviation difference in the numerical variable,
#' e.g. mean NPX at mean(numerical variable) versus mean NPX at mean(numerical
#' variable) + 1*SD(numerical variable). The output tibble is arranged by
#' ascending Tukey adjusted p-values.
#'
#' The `variable`, `covariates`, `random`, and/or `model_formula` arguments can
#' be used in the same way as in `\code{\link{olink_lmer}} to specify the model
#' to be fitted.
#'
#' @param df NPX data frame in long format with at least protein name (Assay),
#' OlinkID, UniProt, 1-2 variables with at least 2 levels and subject
#' identifier.
#' @param check_log Optional named list returned by [`check_npx()`]. If `NULL`,
#' an attached `check_log` is used when present; otherwise [`check_npx()`] will
#' be run internally using `df`.
#' @param olinkid_list Character vector of OlinkID's on which to perform post
#' hoc analysis. If not specified, all assays in df are used.
#' @param variable Single character value or character array. Variables to test.
#' If `length > 1`, the included variable names will be used in crossed
#' analyses. Also takes ':' or '*' notation.
#' @param outcome Character. The dependent variable. Default: NPX.
#' @param random Single character value or character array.
#' @param model_formula (optional) Symbolic description of the model to be
#' fitted in standard formula notation (e.g. `NPX~A*B + (1|ID)`). If provided,
#' this will override the `outcome`, `variable` and `covariates`.
#' arguments. Can be a string or of class \code{stats::formula()}.
#' @param effect Term on which to perform post-hoc. Character vector. Must be
#' subset of or identical to variable.
#' @param effect_formula (optional) A character vector specifying the names of
#' the predictors over which estimated marginal means are desired as defined in
#' the \code{emmeans} package. May also be a formula. If provided, this will
#' override the \code{effect} argument. See \code{?emmeans::emmeans()} for more
#' information.
#' @param covariates Single character value or character array. Default: `NULL`.
#' Covariates to include. Takes ':' or '*' notation. Crossed analysis will not
#' be inferred from main effects.
#' @param mean_return Boolean. If true, returns the mean of each factor level
#' rather than the difference in means (default). Note that no p-value is
#' returned for mean_return = TRUE and no adjustment is performed.
#' @param post_hoc_padjust_method P-value adjustment method to use for post-hoc
#' comparisons within an assay. Options include \code{tukey}, \code{sidak},
#' \code{bonferroni} and \code{none}.
#' @param verbose Boolean. Default: True. If information about removed samples,
#' factor conversion and final model formula is to be printed to the console.
#'
#' @return A "tibble" containing the results of the pairwise comparisons between
#' given variable levels for proteins specified in olinkid_list (or full df).
#' Columns include:
#' \itemize{
#'  \item{Assay:} "character" Protein symbol
#'  \item{OlinkID:} "character" Olink specific ID
#'  \item{UniProt:} "character" UniProt ID
#'  \item{Panel:} "character" Name of Olink Panel
#'  \item{term:} "character" term in model
#'  \item{contrast:} "character" the groups that were compared
#'  \item{estimate:} "numeric" difference in mean NPX between groups
#'  \item{conf.low:} "numeric" confidence interval for the mean (lower end)
#'  \item{conf.high:} "numeric" confidence interval for the mean (upper end)
#'  \item{Adjusted_pval:} "numeric" adjusted p-value for the test
#'  \item{Threshold:} "character" if adjusted p-value is significant or not
#'  (< 0.05)
#' }
#'
#' @export
#'
#' @examples
#' \donttest{
#' if (rlang::is_installed(pkg = c("lme4", "lmerTest", "emmeans", "broom"))) {
#'   #data
#'   npx_df <- OlinkAnalyze::npx_data1 |>
#'     dplyr::filter(
#'       !grepl(
#'         pattern = "control|ctrl",
#'         x = .data[["SampleID"]],
#'         ignore.case = TRUE
#'       )
#'     )
#'
#'   # Results in model NPX ~ Time * Treatment + (1 | Subject)
#'   lmer_results <- OlinkAnalyze::olink_lmer(
#'     df = npx_df,
#'     variable = c("Time", "Treatment"),
#'     random = c("Subject")
#'   )
#'
#'   # List of significant proteins for the interaction effect Time:Treatment
#'   assay_list <- lmer_results |>
#'     dplyr::filter(
#'     .data[["Threshold"]] == "Significant"
#'     & .data[["term"]] == "Time:Treatment"
#'   ) |>
#'     dplyr::distinct(.data[["OlinkID"]]) |>
#'     dplyr::pull()
#'
#'   # Run lmer posthoc on significant proteins
#'   results_lmer_posthoc <- OlinkAnalyze::olink_lmer_posthoc(
#'     df = npx_df,
#'     olinkid_list = assay_list,
#'     variable = c("Time", "Treatment"),
#'     effect = "Time:Treatment",
#'     random = "Subject",
#'     verbose = TRUE
#'   )
#'
#'   # Estimate treated vs untreated at each timepoint
#'   results_lmer_posthoc <- OlinkAnalyze::olink_lmer_posthoc(
#'     df = npx_df,
#'     olinkid_list = assay_list,
#'     model_formula = "NPX~Time*Treatment+(1|Subject)",
#'     effect_formula = "pairwise~Treatment|Time",
#'     verbose = TRUE
#'   )
#' }
#' }
#'
olink_lmer_posthoc <- function(df,
                               check_log = NULL,
                               olinkid_list = NULL,
                               variable,
                               outcome = "NPX",
                               random,
                               model_formula,
                               effect,
                               effect_formula,
                               covariates = NULL,
                               mean_return = FALSE,
                               post_hoc_padjust_method = "tukey",
                               verbose = TRUE) {

  # Check if all required libraries for this function are installed
  rlang::check_installed(
    pkg = c("lme4", "lmerTest", "emmeans", "broom"),
    call = rlang::caller_env()
  )

  if (!missing(model_formula)) {
    model_formula <- check_stats_formula(model_formula)

    # If variable or random were included, throw a message that they will
    # not be used as model_formula is provided
    if (!missing(variable) || !missing(random)) {
      cli::cli_inform(
        paste("model_formula overriding variable and",
              "random arguments.")
      )
    }

    # Parse formula so checks on the  variable and outcome objects can continue
    # as usual
    # Random portion of formula
    splt_random <- stringr::str_extract(string = model_formula,
                                        pattern = "(?<=\\().*(?=\\))")
    splt_random <- strsplit(x = splt_random, split = "\\+|~|\\*|:|\\|")[[1L]]
    if (any(grepl(pattern = "-1|1", x = splt_random))) {
      splt_random <- splt_random[-grep(pattern = "-1|1", x = splt_random)]
    }
    random <- splt_random

    # Fixed effects portion
    splt_form <- gsub(pattern = "\\s*\\([^\\)]+\\)",
                      replacement = "",
                      x = model_formula)
    splt_form <- strsplit(x = splt_form, split = c("\\+|~|\\*|:"))[[1L]]
    if ("-1" %in% splt_form) {
      splt_form <- splt_form[-which(splt_form == "-1")]
    }

    # Check if covariate was specified and remove from variables
    if (!missing(covariates)) {

      # Check if covariate exists in formula
      if (all(covariates %in% splt_form[-1L])) {

        outcome <- splt_form[1L]
        variable <- setdiff(splt_form[-1L], covariates)

      } else {

        cli::cli_abort(
          c(
            "x" = "Covariate{?s} {.val {setdiff(covariates,splt_form[-1L])}}
            {?is/are} not present in the model formula!",
            "i" = "Expected {.or {.val {splt_form[-1L]}}}."
          ),
          call = rlang::caller_env(),
          wrap = FALSE
        )
      }

    } else {
      # If no covariate specified, treat all terms as variables
      outcome <- splt_form[1L]
      variable <- splt_form[-1L]
      covariates <- NULL
    }
  }

  if (!missing(effect_formula)) {
    effect <- parse_stats_effect_formula(
      effect_formula = effect_formula,
      effect = if (missing(effect)) NULL else effect,
      effect_supplied = !missing(effect)
    )
  }

  check_stats_required(
    present = c(
      !missing(df), !missing(variable), !missing(random), !missing(effect)
    ),
    message = paste(
      "The df, variable, random and effect arguments need to be specified."
    )
  )
  check_stats_effect_terms(effect = effect, variable = variable)

  # Check data format
  check_log <- get_check_npx(df = df, check_log = check_log)

  lmer_posthoc_result <- withCallingHandlers(
    {
      df <- filter_stats_olink_ids(
        df = df,
        olink_id = check_log$col_names$olink_id,
        invalid_ids = check_log$oid_invalid
      )
      olinkid_list <- resolve_stats_olink_ids(
        df = df,
        olinkid_list = olinkid_list,
        olink_id = check_log$col_names$olink_id
      )

      model_terms <- prepare_stats_model_terms(
        variable = variable,
        covariates = covariates
      )
      variable <- model_terms$variable
      covariates <- model_terms$covariates
      add_main_effects <- model_terms$add_main_effects

      variable_testers <- intersect(x = c(variable, covariates), y = names(df))
      filtered_data <- filter_stats_missing(
        df = df,
        columns = variable_testers,
        sample_id = check_log$col_names$sample_id
      )
      df <- filtered_data$df
      removed_sampleids <- filtered_data$removed_sampleids

      converted_data <- convert_stats_predictors(df, variable_testers)
      df <- converted_data$df
      converted_vars <- converted_data$converted_vars
      num_vars <- converted_data$numeric_vars

      formula_string <- build_stats_formula(
        outcome = outcome,
        variable = variable,
        covariates = covariates,
        random = random,
        model_formula = if (missing(model_formula)) NULL else model_formula
      )
      e_form <- build_stats_effect_formula( # nolint: object_usage_linter
        effect = effect,
        effect_formula = if (missing(effect_formula)) NULL else effect_formula,
        as_formula = FALSE
      )

      if (verbose) {
        inform_stats_model(
          add_main_effects = add_main_effects,
          removed_sampleids = removed_sampleids,
          converted_vars = converted_vars,
          numeric_vars = num_vars,
          variable = variable,
          model_message = paste(
            "Means estimated for each assay from linear mixed effects model:",
            formula_string
          ),
          posthoc = TRUE
        )
      }


      output_df <- df |>
        filter_stats_assays(
          olinkid_list = olinkid_list,
          assay_na = check_log$assay_na,
          olink_id = check_log$col_names$olink_id
        ) |>
        dplyr::group_by(
          dplyr::across(
            dplyr::all_of(
              c(check_log$col_names$assay,
                check_log$col_names$olink_id,
                check_log$col_names$uniprot,
                check_log$col_names$panel)
            )
          )
        ) |>
        dplyr::group_modify(
          .f = ~ single_posthoc(
            data = .x,
            formula_string = formula_string,
            effect = e_form,
            mean_return = mean_return,
            padjust_method = post_hoc_padjust_method
          )
        ) |>
        dplyr::ungroup() |>
        dplyr::mutate(
          term = paste(.env[["effect"]], collapse = ":")
        ) |>
        dplyr::select(
          dplyr::all_of(
            c(check_log$col_names$assay,
              check_log$col_names$olink_id,
              check_log$col_names$uniprot,
              check_log$col_names$panel,
              "term")
          ),
          dplyr::everything()
        )

      if ("Adjusted_pval" %in% colnames(output_df)) {
        output_df <- output_df |>
          dplyr::arrange(
            .data[["Adjusted_pval"]]
          )
      }

      return(output_df)
    },
    warning = function(w) {
      return(muffle_stats_warning(
        warning = w,
        patterns = utils::glob2rx("*contains implicit NA, consider using*")
      ))
    }
  )

  return(lmer_posthoc_result)
}

#' Internal LMER posthoc function
#'
#' @param data grouped data frame
#' @param formula_string anova formula
#' @param effect Term on which to perform post-hoc. Character vector. Must be
#' subset of or identical to variable.
#' @param mean_return Boolean. If true, returns the mean of each factor level
#' rather than the difference in means (default). Note that no p-value is
#' returned for mean_return = TRUE and no adjustment is performed.
#' @param padjust_method P-value adjustment method to use for post-hoc
#' comparisons within an assay. Options include \code{tukey}, \code{sidak},
#' \code{bonferroni} and \code{none}.
#'
#' @return lmer posthoc results
#'
#' @noRd
#'
single_posthoc <- function(data,
                           formula_string,
                           effect,
                           mean_return,
                           padjust_method = "tukey") {

  if (!is.character(effect)) {
    cli::cli_abort("{.arg effect} must be a character string.")
  }

  posthoc_result <- run_stats_emmeans(
    model = single_lmer(
      data = data,
      formula_string = formula_string
    ),
    effect = stats::as.formula(effect),
    mean_return = mean_return,
    padjust_method = padjust_method,
    lmer_df = "satterthwaite"
  )

  if (mean_return) {
    return(
      posthoc_result |>
        dplyr::as_tibble() |>
        dplyr::rename(
          "conf.low" = "lower.CL",
          "conf.high" = "upper.CL"
        ) |>
        dplyr::select(
          -dplyr::all_of(
            c("SE", "df", "t.ratio", "p.value")
          )
        )
    )
  } else {
    out_df <- posthoc_result |>
      dplyr::as_tibble() |>
      dplyr::rename(
        "Adjusted_pval" = "p.value",
        "conf.low" = "lower.CL",
        "conf.high" = "upper.CL"
      ) |>
      add_stats_threshold() |>
      dplyr::select(
        -dplyr::all_of(
          c("SE", "df", "t.ratio")
        )
      ) |>
      dplyr::arrange(
        .data[["Adjusted_pval"]]
      )

    if (padjust_method == "none") {
      out_df <- out_df |>
        dplyr::rename(
          "pvalue" = "Adjusted_pval"
        )
    }

    return(out_df)
  }
}
