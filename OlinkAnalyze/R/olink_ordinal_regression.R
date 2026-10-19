#' Function that performs a two-way ordinal analysis.
#'
#' @description
#' Function that performs a two-way ordinal analysis of variance can address an
#' experimental design with two independent variables, each of which is a factor
#' variable. The main effect of each independent variable can be tested, as well
#' as the effect of the interaction of the two factors.
#'
#' @details
#' Performs an ANOVA F-test for each assay (by OlinkID) in every panel using
#' stats::Anova and Type III sum of squares. Dependent variable will be treated
#' as ordered factor. The function handles only factor and/or covariates.
#'
#' Samples that have no variable information or missing factor levels are
#' automatically removed from the analysis (specified in a message if verbose =
#' T). Character columns in the input dataframe are automatically converted to
#' factors (specified in a message if verbose = T). Crossed analysis, i.e. A*B
#' formula notation, is inferred from the variable argument in the following
#' cases:
#' \itemize{
#'   \item c('A','B')
#'   \item c('A: B')
#'   \item c('A: B', 'B') or c('A: B', 'A')
#' }
#'
#' Inference and the formula notation of the final model are specified in a
#' message if verbose = T.
#'
#' Adjusted p-values are calculated by stats::p.adjust according to the
#' Benjamini & Hochberg (1995) method (“fdr”). The threshold is determined by
#' logic evaluation of Adjusted_pval < 0.05. Covariates are not included in the
#' p-value adjustment.
#'
#' @param df NPX or Quantified_value data frame in long format with at least
#' protein name (Assay), OlinkID, UniProt, Panel and a factor with at least
#' 3 levels.
#' @param variable Single character value or character array. Variable(s) to
#' test. If length > 1, the included variable names will be used in crossed
#' analyses. Also takes ':'/'*' notation.
#' @param covariates Single character value or character array. Default: NULL.
#' Covariates to include. Takes ':'/'*' notation. Crossed analysis will not be
#' inferred from main effects.
#' @param return.covariates Logical. Default: False. Returns F-test results for
#' the covariates. Note: Adjusted p-values will be NA for the covariates.
#' @param check_log Optional named list returned by [`check_npx()`]. If `NULL`,
#' an attached `check_log` is used when present; otherwise [`check_npx()`] will
#' be run internally using `df`.
#' @param verbose Logical. Default: True. If information about removed samples,
#' factor conversion and final model formula is to be printed to the console.
#'
#' @export
#'
#' @return A tibble containing the ANOVA results for every protein. The tibble
#' is arranged by ascending p-values. Columns include:#'
#' \itemize{
#'   \item{Assay:} "character" Protein symbol
#'   \item{OlinkID:} "character" Olink specific ID
#'   \item{UniProt:} "character" UniProt ID
#'   \item{Panel:} "character" Name of Olink Panel
#'   \item{term:} "character" term in model
#'   \item{statistic:} "numeric" value of the statistic
#'   \item{p.value:} "numeric" nominal p-value
#'   \item{Adjusted_pval:} "numeric" adjusted p-value for the test
#'   \item{Threshold:} "character" if adjusted p-value is significant or not
#'   (< 0.05)
#' }
#'
#' @examples
#' \donttest{
#' if (rlang::is_installed(pkg = c("ordinal", "broom"))) {
#'   npx_df <- OlinkAnalyze::npx_data1 |>
#'     dplyr::filter(
#'     !grepl(
#'       pattern = "control",
#'       x = .data[["SampleID"]],
#'       ignore.case = TRUE
#'     )
#'   )
#'
#'   # Two-way Ordinal Regression with CLM.
#'   # Results in model NPX~Treatment+Time+Treatment:Time.
#'   ordinalRegression_results <- OlinkAnalyze::olink_ordinal_regression(
#'     df = npx_df,
#'     variable = "Treatment:Time"
#'   )
#' }
#' }
#'
olink_ordinal_regression <- function(df,
                                     variable,
                                     covariates = NULL,
                                     return.covariates = FALSE, # nolint: object_name_linter
                                     check_log = NULL,
                                     verbose = TRUE) {
  # Check if all required libraries for this function are installed
  rlang::check_installed(
    pkg = c("ordinal", "broom"),
    call = rlang::caller_env()
  )

  check_stats_required(
    present = c(!missing(df), !missing(variable)),
    message = "The df and variable arguments need to be specified."
  )

  ord_regr_results <- withCallingHandlers(
    {
      df <- filter_stats_olink_ids(df)

      model_terms <- prepare_stats_model_terms(
        variable = variable,
        covariates = covariates
      )
      variable <- model_terms$variable
      covariates <- model_terms$covariates
      add.main.effects <- model_terms$add_main_effects # nolint: object_name_linter

      # Variables to check
      variable_testers <- intersect(x = c(variable, covariates),
                                    y = names(df))

      filtered_data <- filter_stats_missing(
        df = df,
        columns = variable_testers,
        sample_id = "SampleID"
      )
      df <- filtered_data$df
      removed.sampleids <- filtered_data$removed_sampleids # nolint: object_name_linter

      # check data format
      check_log <- get_check_npx(df = df, check_log = check_log)
      data_type <- check_log$col_names$quant

      # Convert outcome to factor
      df <- df |>
        dplyr::mutate(
          !!data_type := factor(x = .data[[data_type]], ordered = TRUE)
        )

      converted_data <- convert_stats_predictors(df, variable_testers)
      df <- converted_data$df
      converted.vars <- converted_data$converted_vars # nolint: object_name_linter
      num.vars <- converted_data$numeric_vars # nolint: object_name_linter
      fact.vars <- converted_data$factor_vars # nolint: object_name_linter

      single_fixed_effects <- get_stats_fixed_effects(
        df = df,
        variable = variable,
        covariates = covariates
      )
      nas_in_var <- check_stats_effect_data(
        df = df,
        effects = single_fixed_effects,
        outcome = data_type,
        check_log = check_log
      )

      formula_string <- build_stats_formula(
        outcome = data_type,
        variable = variable,
        covariates = covariates
      )

      if (verbose) {
        inform_stats_model(
          add_main_effects = add.main.effects,
          removed_sampleids = removed.sampleids,
          converted_vars = converted.vars,
          numeric_vars = num.vars,
          model_message = paste(
            "Cumulative Link Model (CLM) fit to each assay:", formula_string
          )
        )
      }

      covariate_filter_str <- get_stats_covariate_terms(covariates)

      p.val <- df |> # nolint: object_name_linter
        dplyr::filter(
          !(.data[["OlinkID"]] %in% check_log$assay_na)
          & !(.data[["OlinkID"]] %in% .env[["nas_in_var"]])
        ) |>
        dplyr::group_by(
          .data[["Assay"]], .data[["OlinkID"]],
          .data[["UniProt"]], .data[["Panel"]]
        ) |>
        dplyr::mutate(
          !!data_type := rank(x = .data[[data_type]]) |>
            factor()
        ) |>
        dplyr::group_modify(~ {
          broom::tidy(
            stats::anova(
              object = ordinal::clm(
                formula = stats::as.formula(formula_string),
                data = .x,
                threshold = "flexible"
              ),
              type = "marginal"
            )
          )
        }) |>
        dplyr::ungroup() |>
        dplyr::filter(
          !(.data[["term"]] %in% c("(Intercept)", "Residuals"))
        ) |>
        dplyr::mutate(
          covariates = .data[["term"]] %in% .env[["covariate_filter_str"]]
        ) |>
        dplyr::group_by(
          .data[["covariates"]]
        ) |>
        add_stats_significance() |>
        dplyr::ungroup() |>
        dplyr::mutate(
          dplyr::across(
            dplyr::all_of(c("Adjusted_pval", "Threshold")),
            ~ dplyr::if_else(.data[["covariates"]] == TRUE, NA, .x)
          )
        ) |>
        dplyr::select(
          dplyr::all_of(
            c("Assay", "OlinkID", "UniProt", "Panel", "term", "df",
              "statistic", "p.value", "Adjusted_pval", "Threshold")
          )
        ) |>
        dplyr::arrange(
          .data[["p.value"]]
        )

      if (return.covariates == FALSE) {
        p.val <- p.val |> # nolint: object_name_linter
          dplyr::filter(
            !(.data[["term"]] %in% .env[["covariate_filter_str"]])
          )
      }

      return(p.val)

    }, warning = function(w) {
      return(muffle_stats_warning(
        warning = w,
        patterns = utils::glob2rx("*contains implicit NA, consider using*")
      ))
    }
  )

  return(ord_regr_results)
}

#' @rdname olink_ordinal_regression
#' @export
olink_ordinalRegression <- olink_ordinal_regression  # nolint: object_name_linter

#' Function which performs an posthoc test per protein.
#'
#' @description
#' Performs a post hoc ANOVA test using emmeans::emmeans with Tukey p-value
#' adjustment per assay (by OlinkID) for each panel at confidence level 0.95.
#' See \code{olink_anova} for details of input notation.
#'
#' The function handles both factor and numerical variables and/or covariates.
#' The posthoc test for a numerical variable compares the difference in means of
#' the ordinal outcome variable (default: NPX) for 1 standard deviation
#' difference in the numerical variable, e.g. mean ordinal NPX at mean(numerical
#' variable) versus mean NPX at mean(numerical variable) + 1*SD(numerical
#' variable).
#'
#' @param df NPX data frame in long format with at least protein name (Assay),
#' OlinkID, UniProt, Panel and a factor with at least 3 levels.
#' @param olinkid_list Character vector of OlinkID's on which to perform post
#' hoc analysis. If not specified, all assays in df are used.
#' @param variable Single character value or character array. Variable(s) to
#' test. If length > 1, the included variable names will be used in crossed
#' analyses. Also takes ':' notation.
#' @param covariates Single character value or character array. Default: NULL.
#' Covariates to include. Takes ':'/'*' notation. Crossed analysis will not be
#' inferred from main effects.
#' @param effect Term on which to perform post-hoc. Character vector. Must be
#' subset of or identical to variable.
#' @param effect_formula (optional) A character vector specifying the names of
#' the predictors over which estimated marginal means are desired as defined in
#' the \code{emmeans} package. May also be a formula. If provided, this will
#' override the \code{effect} argument. See \code{?emmeans::emmeans()} for more
#' information.
#' @param mean_return Boolean. If true, returns the mean of each factor level
#' rather than the difference in means (default). Note that no p-value is
#' returned for mean_return = TRUE and no adjustment is performed.
#' @param post_hoc_padjust_method P-value adjustment method to use for post-hoc
#' comparisons within an assay. Options include \code{tukey}, \code{sidak},
#' \code{bonferroni} and \code{none}.
#' @param check_log Optional named list returned by [`check_npx()`]. If `NULL`,
#' an attached `check_log` is used when present; otherwise [`check_npx()`] will
#' be run internally using `df`.
#' @param verbose Boolean. Default: True. If information about removed samples,
#' factor conversion and final model formula is to be printed to the console.
#'
#' @export
#'
#' @return Tibble of posthoc tests for specified effect, arranged by ascending
#' adjusted p-values. Columns include:
#' \itemize{
#'   \item{Assay:} "character" Protein symbol
#'   \item{OlinkID:} "character" Olink specific ID
#'   \item{UniProt:} "character" UniProt ID
#'   \item{Panel:} "character" Name of Olink Panel
#'   \item{term:} "character" term in model
#'   \item{contrast:} "character" the groups that were compared
#'   \item{estimate:} "numeric" difference in mean of the ordinal NPX between
#'   groups
#'   \item{Adjusted_pval:} "numeric" adjusted p-value for the test
#'   \item{Threshold:} "character" if adjusted p-value is significant or not
#'   (< 0.05)
#' }
#'
#' @examples
#' \donttest{
#' if (rlang::is_installed(pkg = c("ordinal", "emmeans"))) {
#'   npx_df <- OlinkAnalyze::npx_data1 |>
#'     dplyr::filter(
#'     !grepl(
#'       pattern = "control",
#'       x = .data[["SampleID"]],
#'       ignore.case = TRUE
#'     )
#'   )
#'
#'   # Two-way Ordinal Regression with CLM.
#'   # Results in model NPX~Treatment+Time+Treatment:Time.
#'   ordinalRegression_results <- OlinkAnalyze::olink_ordinal_regression(
#'     df = npx_df,
#'     variable = "Treatment:Time"
#'   )
#'
#'   significant_assays <- ordinalRegression_results |>
#'     dplyr::filter(
#'       .data[["Threshold"]] == "Significant"
#'       & .data[["term"]] == "Time"
#'     ) |>
#'     dplyr::pull(
#'       .data[["OlinkID"]]
#'     ) |>
#'     unique()
#'
#'   # Posthoc test
#'   ordRegr_results_posthoc <- OlinkAnalyze::olink_ordinal_regression_posthoc(
#'     df = npx_df,
#'     variable = c("Treatment:Time"),
#'     olinkid_list = significant_assays,
#'     effect = "Time"
#'   )
#' }
#' }
#'
olink_ordinal_regression_posthoc <- function(df, # nolint: object_length_linter
                                             olinkid_list = NULL,
                                             variable,
                                             covariates = NULL,
                                             effect,
                                             effect_formula,
                                             mean_return = FALSE,
                                             post_hoc_padjust_method = "tukey",
                                             check_log = NULL,
                                             verbose = TRUE) {

  # Check if all required libraries for this function are installed
  rlang::check_installed(
    pkg = c("ordinal", "emmeans"),
    call = rlang::caller_env()
  )

  check_stats_required(
    present = c(!missing(df), !missing(variable), !missing(effect)),
    message = "The df, variable and effect arguments need to be specified."
  )
  check_stats_effect_terms(effect = effect, variable = variable)

  ord_regr_posthoc_result <- withCallingHandlers(
    {
      df <- filter_stats_olink_ids(df)
      olinkid_list <- resolve_stats_olink_ids(df, olinkid_list)

      # check data format
      check_log <- get_check_npx(df = df, check_log = check_log)
      data_type <- check_log$col_names$quant

      model_terms <- prepare_stats_model_terms(
        variable = variable,
        covariates = covariates
      )
      variable <- model_terms$variable
      covariates <- model_terms$covariates
      add.main.effects <- model_terms$add_main_effects # nolint: object_name_linter

      # Variables to check
      variable_testers <- intersect(x = c(variable, covariates),
                                    y = names(df))

      filtered_data <- filter_stats_missing(
        df = df,
        columns = variable_testers,
        sample_id = "SampleID"
      )
      df <- filtered_data$df
      removed.sampleids <- filtered_data$removed_sampleids # nolint: object_name_linter

      # Convert outcome to factor
      df <- df |>
        dplyr::mutate(
          !!data_type := factor(x = .data[[data_type]], ordered = TRUE)
        )

      converted_data <- convert_stats_predictors(df, variable_testers)
      df <- converted_data$df
      converted.vars <- converted_data$converted_vars # nolint: object_name_linter
      num.vars <- converted_data$numeric_vars # nolint: object_name_linter

      formula_string <- build_stats_formula(
        outcome = data_type,
        variable = variable,
        covariates = covariates
      )

      if (verbose) {
        inform_stats_model(
          add_main_effects = add.main.effects,
          removed_sampleids = removed.sampleids,
          converted_vars = converted.vars,
          numeric_vars = num.vars,
          variable = variable,
          model_message = paste(
            "Estimated marginal means for each assay computed from the",
            "cumulative link model (CLM):", formula_string
          ),
          posthoc = TRUE
        )
      }

      e_form <- build_stats_effect_formula( # nolint: object_usage_linter
        effect = effect,
        effect_formula = if (missing(effect_formula)) NULL else effect_formula
      )

      ord_regr_posthoc_results <- df |>
        filter_stats_assays(
          olinkid_list = olinkid_list,
          assay_na = check_log$assay_na,
          preserve_order = TRUE
        ) |>
        dplyr::group_by(
          .data[["Assay"]], .data[["OlinkID"]],
          .data[["UniProt"]], .data[["Panel"]]
        ) |>
        dplyr::mutate(
          !!data_type := rank(x = .data[[data_type]]) |>
            factor()
        ) |>
        dplyr::group_modify(~ {
          model <- ordinal::clm(
            formula = stats::as.formula(object = formula_string),
            data = .x
          )
          run_stats_emmeans(
            model = model,
            effect = e_form,
            mean_return = mean_return,
            padjust_method = post_hoc_padjust_method
          )
        }) |>
        dplyr::ungroup() |>
        dplyr::mutate(
          term = paste(.env[["effect"]], collapse = ":")
        ) |>
        dplyr::rename(
          "Adjusted_pval" = "p.value"
        ) |>
        dplyr::arrange(
          .data[["Adjusted_pval"]]
        ) |>
        add_stats_threshold() |>
        dplyr::select(
          dplyr::all_of(
            c("Assay", "OlinkID", "UniProt", "Panel", "term", "contrast",
              "estimate", "Adjusted_pval", "Threshold")
          )
        )

      return(ord_regr_posthoc_results)

    }, warning = function(w) {
      return(muffle_stats_warning(
        warning = w,
        patterns = utils::glob2rx("*contains implicit NA, consider using*")
      ))
    }
  )

  return(ord_regr_posthoc_result)
}

#' @rdname olink_ordinal_regression_posthoc
#' @export
olink_ordinalRegression_posthoc <- olink_ordinal_regression_posthoc  # nolint: object_name_linter
