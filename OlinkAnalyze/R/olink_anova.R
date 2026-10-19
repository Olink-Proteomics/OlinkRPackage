#' Function which performs an ANOVA per protein.
#'
#' @description
#' Performs an ANOVA F-test for each assay (by OlinkID) in every panel using
#' car::Anova and Type III sum of squares. The function handles both factor and
#' numerical variables and/or covariates.
#'
#' @details
#' Samples that have no variable information or missing factor levels are
#' automatically removed from the analysis (specified in a message if verbose =
#' TRUE). Character columns in the input dataframe are automatically converted
#' to factors (specified in a message if verbose = TRUE). Numerical variables
#' are not converted to factors. Control samples should be removed before using
#' this function. Control assays (AssayType is not "assay", or Assay contains
#' "control" or "ctrl") should be removed before using this function. If a
#' numerical variable is to be used as a factor, this conversion needs to be
#' done on the dataframe before the function call.
#'
#' Crossed analysis, i.e. A*B formula notation, is inferred from the variable
#' argument in the following cases:
#' \itemize{
#'   \item c('A','B')
#'   \item c('A: B')
#'   \item c('A: B', 'B') or c('A: B', 'A')
#' }
#'
#' Inference is specified in a message if verbose = TRUE.
#'
#' For covariates, crossed analyses need to be specified explicitly, i.e. two
#' main effects will not be expanded with a c('A','B') notation. Main effects
#' present in the variable takes precedence. The formula notation of the final
#' model is specified in a message if verbose = TRUE.
#'
#' Adjusted p-values are calculated by stats::p.adjust according to the
#' Benjamini & Hochberg (1995) method (“fdr”). The threshold is determined by
#' logic evaluation of Adjusted_pval < 0.05. Covariates are not included in the
#' p-value adjustment.
#'
#' @param df NPX data frame in long format with at least protein name (Assay),
#' OlinkID, UniProt, Panel and a factor with at least 3 levels.
#' @param variable Single character value or character array. Variable(s) to
#' test. If length > 1, the included variable names will be used in crossed
#' analyses. Also takes ':' or '*' notation.
#' @param check_log Optional named list returned by [`check_npx()`]. If `NULL`,
#' an attached `check_log` is used when present; otherwise [`check_npx()`] will
#' be run internally using `df`.
#' @param outcome Character. The dependent variable. Default: NPX.
#' @param covariates Single character value or character array. Default: NULL.
#' Covariates to include. Takes ':' or '*' notation. Crossed analysis will not
#' be inferred from main effects.
#' @param model_formula (optional) Symbolic description of the model to be
#' fitted in standard formula notation (e.g. "NPX~A*B"). If provided, this will
#' override the \code{outcome}, \code{variable} and \code{covariates} arguments.
#' Can be a string or of class \code{stats::formula()}.
#' @param return.covariates Boolean. Default: False. Returns F-test results for
#' the covariates. Note: Adjusted p-values will be NA for the covariates.
#' @param verbose Boolean. Default: True. If information about removed samples,
#' factor conversion and final model formula is to be printed to the console.
#'
#' @return A "tibble" containing the ANOVA results for every protein. The tibble
#' is arranged by ascending p-values. Columns include:
#'
#' \itemize{
#'  \item{Assay:} "character" Protein symbol
#'  \item{OlinkID:} "character" Olink specific ID
#'  \item{UniProt:} "character" UniProt ID
#'  \item{Panel:} "character" Name of Olink Panel
#'  \item{term:} "character" term in model
#'  \item{df:} "numeric" degrees of freedom
#'  \item{sumsq:} "numeric" sum of square
#'  \item{meansq:} "numeric" mean of square
#'  \item{statistic:} "numeric" value of the statistic
#'  \item{p.value:} "numeric" nominal p-value
#'  \item{Adjusted_pval:} "numeric" adjusted p-value for the test
#'  (Benjamini&Hochberg)
#'  \item{Threshold:} "character" if adjusted p-value is significant or not
#'  (< 0.05)
#' }
#'
#' @export
#'
#' @examples
#' \donttest{
#' if (rlang::is_installed(pkg = c("broom", "car"))) {
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
#'   # One-way ANOVA, no covariates.
#'   # Results in a model NPX~Time
#'   anova_results <- OlinkAnalyze::olink_anova(
#'     df = npx_df,
#'     variable = "Time"
#'   )
#'
#'   # Two-way ANOVA, one main effect covariate.
#'   # Results in model NPX~Treatment*Time+Site.
#'   anova_results <- OlinkAnalyze::olink_anova(
#'     df = npx_df,
#'     variable = c("Treatment:Time"),
#'     covariates = "Site"
#'   )
#'
#'   # One-way ANOVA, interaction effect covariate.
#'   # Results in model NPX~Treatment+Site:Time+Site+Time.
#'   anova_results <- OlinkAnalyze::olink_anova(
#'     df = npx_df,
#'     variable = "Treatment",
#'     covariates = "Site:Time"
#'   )
#' }
#' }
#'
olink_anova <- function(df,
                        variable,
                        check_log = NULL,
                        outcome = "NPX",
                        covariates = NULL,
                        model_formula,
                        return.covariates = FALSE, # nolint: object_name_linter
                        verbose = TRUE) {

  # Check if all required libraries for this function are installed
  rlang::check_installed(
    pkg = c("broom", "car"),
    call = rlang::caller_env()
  )

  if (!missing(model_formula)) {
    model_formula <- check_stats_formula(model_formula)

    # If variable and covariates were included, throw a message that they will
    # not be used as model_formula is provided
    if (!missing(variable) || !is.null(covariates)) {
      cli::cli_inform(
        "model_formula overriding variable and covariate arguments."
      )
    }

    # Parse formula so checks on the variable and outcome objects can continue
    # as usual
    splt_form <- strsplit(x = model_formula, split = c("\\+|~|\\*|:"))[[1L]]

    if ("-1" %in% splt_form) {
      splt_form <- splt_form[-which(splt_form == "-1")]
    }

    outcome <- splt_form[1L]
    variable <- splt_form[-1L]
    covariates <- NULL
  }

  check_stats_required(
    present = c(!missing(df), !missing(variable)),
    message = "The df and variable arguments need to be specified."
  )
  check_stats_control_assays(df)

  anova_result <- withCallingHandlers(
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
      variable_testers <- intersect(x = c(variable, covariates), y = names(df))

      filtered_data <- filter_stats_missing(
        df = df,
        columns = variable_testers,
        sample_id = "SampleID"
      )
      df <- filtered_data$df
      removed.sampleids <- filtered_data$removed_sampleids # nolint: object_name_linter

      # Check data format
      check_log <- get_check_npx(df = df, check_log = check_log)

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
        outcome = "NPX",
        check_log = check_log
      )

      formula_string <- build_stats_formula(
        outcome = outcome,
        variable = variable,
        covariates = covariates,
        model_formula = if (missing(model_formula)) NULL else model_formula
      )


      if (verbose) {
        inform_stats_model(
          add_main_effects = add.main.effects,
          removed_sampleids = removed.sampleids,
          converted_vars = converted.vars,
          numeric_vars = num.vars,
          model_message = paste(
            "ANOVA model fit to each assay:", formula_string
          )
        )
      }

      covariate_filter_string <- get_stats_covariate_terms(covariates)

      p.val <- df |> # nolint: object_name_linter
        # Exclude assays that have all NA:s
        dplyr::filter(
          !(.data[["OlinkID"]] %in% check_log$assay_na)
        ) |>
        dplyr::filter(
          !(.data[["OlinkID"]] %in% .env[["nas_in_var"]])
        ) |>
        dplyr::group_by(
          .data[["Assay"]],
          .data[["OlinkID"]],
          .data[["UniProt"]],
          .data[["Panel"]]
        ) |>
        dplyr::group_modify(
          .f = ~ internal_anova(
            x = .x,
            formula_string = formula_string,
            fact.vars = fact.vars
          )
        ) |>
        dplyr::ungroup() |>
        dplyr::filter(
          !(.data[["term"]] %in% c("(Intercept)", "Residuals"))
        ) |>
        dplyr::mutate(
          covariates = .data[["term"]] %in% .env[["covariate_filter_string"]]
        ) |>
        dplyr::group_by(
          .data[["covariates"]]
        ) |>
        add_stats_significance() |>
        dplyr::ungroup() |>
        dplyr::mutate(
          dplyr::across(
            dplyr::all_of(
              c("Adjusted_pval", "Threshold")
            ),
            ~ dplyr::if_else(.data[["covariates"]] == TRUE, NA, .x)
          )
        ) |>
        dplyr::mutate(
          meansq = .data[["sumsq"]] / .data[["df"]]
        ) |>
        dplyr::select(
          dplyr::all_of(
            c("Assay", "OlinkID", "UniProt", "Panel", "term", "df", "sumsq",
              "meansq", "statistic", "p.value", "Adjusted_pval", "Threshold")
          )
        ) |>
        dplyr::arrange(
          .data[["Adjusted_pval"]]
        )

      if (return.covariates == FALSE) {
        p.val <- p.val |> # nolint: object_name_linter
          dplyr::filter(
            !(.data[["term"]] %in% .env[["covariate_filter_string"]])
          )
      }

      return(p.val)
    },
    warning = function(w) {
      return(muffle_stats_warning(
        warning = w,
        patterns = utils::glob2rx("*contains implicit NA, consider using*")
      ))
    }
  )

  return(anova_result)
}

#' Function which performs an ANOVA posthoc test per protein.
#'
#' @description
#' Performs a post hoc ANOVA test using emmeans::emmeans with Tukey p-value
#' adjustment per assay (by OlinkID) for each panel at confidence level 0.95.
#' See \code{olink_anova} for details of input notation.
#'
#' @details
#' The function handles both factor and numerical variables and/or covariates.
#' Control samples should be removed before using this function. Control assays
#' (AssayType is not "assay", or Assay contains "control" or "ctrl") should be
#' removed before using this function. The posthoc test for a numerical variable
#' compares the difference in means of the outcome variable (default: NPX) for 1
#' standard deviation difference in the numerical variable, e.g. mean NPX at
#' mean(numerical variable) versus mean NPX at mean(numerical variable) +
#' 1*SD(numerical variable).
#'
#' @param df NPX data frame in long format with at least protein name (Assay),
#' OlinkID, UniProt, Panel and a factor with at least 3 levels.
#' @param check_log Optional named list returned by [`check_npx()`]. If `NULL`,
#' an attached `check_log` is used when present; otherwise [`check_npx()`] will
#' be run internally using `df`.
#' @param olinkid_list Character vector of OlinkID's on which to perform post
#' hoc analysis. If not specified, all assays in df are used.
#' @param variable Single character value or character array. Variable(s) to
#' test. If length > 1, the included variable names will be used in crossed
#' analyses. Also takes ':' notation.
#' @param covariates Single character value or character array. Default: NULL.
#' Covariates to include. Takes ':' or '*' notation. Crossed analysis will not
#' be inferred from main effects.
#' @param outcome Character. The dependent variable. Default: NPX.
#' @param model_formula (optional) Symbolic description of the model to be
#' fitted in standard formula notation (e.g. "NPX~A*B"). If provided, this will
#' override the \code{outcome}, \code{variable} and \code{covariates} arguments.
#' Can be a string or of class \code{stats::formula()}.
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
#' @param verbose Boolean. Default: True. If information about removed samples,
#' factor conversion and final model formula is to be printed to the console.
#'
#' @return A "tibble" of posthoc tests for specified effect, arranged by
#' ascending adjusted p-values. Columns include:
#' \itemize{
#'   \item{Assay:} "character" Protein symbol
#'   \item{OlinkID:} "character" Olink specific ID
#'   \item{UniProt:} "character" UniProt ID
#'   \item{Panel:} "character" Name of Olink Panel
#'   \item{term:} "character" term in model
#'   \item{contrast:} "character" the groups that were compared
#'   \item{estimate:} "numeric" difference in mean NPX between groups
#'   \item{conf.low:} "numeric" confidence interval for the mean (lower end)
#'   \item{conf.high:} "numeric" confidence interval for the mean (upper end)
#'   \item{Adjusted_pval:} "numeric" adjusted p-value for the test
#'   \item{Threshold:} "character" if adjusted p-value is significant or not
#'   (< 0.05)
#' }
#'
#' @export
#'
#' @examples
#' \donttest{
#' if (rlang::is_installed(pkg = c("car", "emmeans"))) {
#'   # data
#'   npx_df <- OlinkAnalyze::npx_data1 |>
#'     dplyr::filter(
#'       !grepl(
#'         pattern = "control|ctrl",
#'         x = .data[["SampleID"]],
#'         ignore.case = TRUE
#'        )
#'     )
#'
#'   # Two-way ANOVA, one main effect (Site) covariate.
#'   # Results in model NPX~Treatment*Time+Site.
#'   anova_results <- OlinkAnalyze::olink_anova(
#'     df = npx_df,
#'     variable = c("Treatment:Time"),
#'     covariates = "Site"
#'   )
#'
#'   # Posthoc test for the model NPX~Treatment*Time+Site,
#'   # on the interaction effect Treatment:Time with covariate Site.
#'
#'   # Filtering out significant and relevant results.
#'   significant_assays <- anova_results |>
#'     dplyr::filter(
#'       .data[["Threshold"]] == "Significant"
#'       & .data[["term"]] == "Treatment:Time"
#'     ) |>
#'     dplyr::select(
#'       dplyr::all_of("OlinkID")
#'     ) |>
#'     dplyr::distinct() |>
#'     dplyr::pull()
#'
#'   # Posthoc, all pairwise comparisons
#'   anova_posthoc_results <- OlinkAnalyze::olink_anova_posthoc(
#'     df = npx_df,
#'     variable = c("Treatment:Time"),
#'     covariates = "Site",
#'     olinkid_list = significant_assays,
#'     effect = "Treatment:Time"
#'   )
#'
#'   # Posthoc, treated vs untreated at each timepoint, adjusted for Site effect
#'   anova_posthoc_results <- OlinkAnalyze::olink_anova_posthoc(
#'     df = npx_df,
#'     model_formula = "NPX~Treatment*Time+Site",
#'     olinkid_list = significant_assays,
#'     effect_formula = "pairwise~Treatment|Time"
#'   )
#' }
#' }
#'
olink_anova_posthoc <- function(df,
                                check_log = NULL,
                                olinkid_list = NULL,
                                variable,
                                covariates = NULL,
                                outcome = "NPX",
                                model_formula,
                                effect,
                                effect_formula,
                                mean_return = FALSE,
                                post_hoc_padjust_method = "tukey",
                                verbose = TRUE) {

  # Check if all required libraries for this function are installed
  rlang::check_installed(
    pkg = c("car", "emmeans"),
    call = rlang::caller_env()
  )

  if (!missing(model_formula)) {
    model_formula <- check_stats_formula(model_formula)

    # If variable and covariates were included, throw a message that they will
    # not be used as model_formula is provided
    if (!missing(variable) || !is.null(covariates)) {
      cli::cli_inform(
        "model_formula overriding variable and covariate arguments."
      )
    }

    # Parse formula so checks on the  variable and outcome objects can continue
    # as usual
    splt_form <- strsplit(x = model_formula, split = c("\\+|~|\\*|:"))[[1L]]

    if ("-1" %in% splt_form) {
      splt_form <- splt_form[-which(splt_form == "-1")]
    }

    outcome <- splt_form[1L]
    variable <- splt_form[-1L]
    covariates <- NULL
  }

  if (!missing(effect_formula)) {
    effect <- parse_stats_effect_formula(
      effect_formula = effect_formula,
      effect = if (missing(effect)) NULL else effect,
      effect_supplied = !missing(effect)
    )
  }

  check_stats_required(
    present = c(!missing(df), !missing(variable), !missing(effect)),
    message = "The df and variable and effect arguments need to be specified."
  )
  check_stats_effect_terms(
    effect = effect,
    variable = variable,
    include_model_formula = TRUE
  )
  check_stats_control_assays(df)

  anova_posthoc_result <- withCallingHandlers(
    {
      df <- filter_stats_olink_ids(df)
      olinkid_list <- resolve_stats_olink_ids(df, olinkid_list)

      model_terms <- prepare_stats_model_terms(
        variable = variable,
        covariates = covariates
      )
      variable <- model_terms$variable
      covariates <- model_terms$covariates
      add.main.effects <- model_terms$add_main_effects # nolint: object_name_linter

      # Variables to check
      variable_testers <- intersect(x = c(variable, covariates), y = names(df))

      filtered_data <- filter_stats_missing(
        df = df,
        columns = variable_testers,
        sample_id = "SampleID"
      )
      df <- filtered_data$df
      removed.sampleids <- filtered_data$removed_sampleids # nolint: object_name_linter

      # Check data format
      check_log <- get_check_npx(df = df, check_log = check_log)

      converted_data <- convert_stats_predictors(df, variable_testers)
      df <- converted_data$df
      converted.vars <- converted_data$converted_vars # nolint: object_name_linter
      num.vars <- converted_data$numeric_vars # nolint: object_name_linter

      formula_string <- build_stats_formula(
        outcome = outcome,
        variable = variable,
        covariates = covariates,
        model_formula = if (missing(model_formula)) NULL else model_formula
      )

      # Print verbose message
      if (verbose) {
        inform_stats_model(
          add_main_effects = add.main.effects,
          removed_sampleids = removed.sampleids,
          converted_vars = converted.vars,
          numeric_vars = num.vars,
          variable = variable,
          model_message = paste(
            "Means estimated for each assay from ANOVA model:",
            formula_string
          ),
          posthoc = TRUE
        )
      }

      e_form <- build_stats_effect_formula( # nolint: object_usage_linter
        effect = effect,
        effect_formula = if (missing(effect_formula)) NULL else effect_formula
      )

      anova_posthoc_results <- df |>
        filter_stats_assays(
          olinkid_list = olinkid_list,
          assay_na = check_log$assay_na,
          preserve_order = TRUE
        ) |>
        dplyr::group_by(
          .data[["Assay"]],
          .data[["OlinkID"]],
          .data[["UniProt"]],
          .data[["Panel"]]
        ) |>
        dplyr::group_modify(~ {
          model <- lm(
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
          "conf.low" = "lower.CL",
          "conf.high" = "upper.CL"
        )

      if (mean_return == TRUE) {
        anova_posthoc_results <- anova_posthoc_results |>
          dplyr::select(
            dplyr::all_of(
              c("Assay", "OlinkID", "UniProt", "Panel", "term",
                effect, "emmean", "conf.low", "conf.high")
            )
          )
      } else if (mean_return == FALSE) {
        anova_posthoc_results <- anova_posthoc_results |>
          dplyr::rename(
            "Adjusted_pval" = "p.value"
          ) |>
          dplyr::arrange(
            .data[["Adjusted_pval"]]
          ) |>
          add_stats_threshold() |>
          dplyr::select(
            dplyr::any_of(
              c("Assay", "OlinkID", "UniProt", "Panel", "term",
                "contrast", effect, "estimate", "conf.low",
                "conf.high", "Adjusted_pval", "Threshold")
            )
          )

        if (post_hoc_padjust_method == "none") {
          anova_posthoc_results <- anova_posthoc_results |>
            dplyr::rename(
              "pvalue" = "Adjusted_pval"
            )
        }
      }

      return(anova_posthoc_results)

    },
    warning = function(w) {
      return(muffle_stats_warning(
        warning = w,
        patterns = utils::glob2rx("*contains implicit NA, consider using*")
      ))
    }
  )

  return(anova_posthoc_result)
}

#' Internal Anova function
#'
#' @param x grouped data frame
#' @param formula_string anova formula
#' @param fact.vars variables in factor form
#'
#' @return anova results
#'
#' @noRd
#'
internal_anova <- function(x,
                           formula_string,
                           fact.vars) {  # nolint: object_name_linter
  anova_out <- broom::tidy(
    x = car::Anova(
      stats::lm(
        formula = stats::as.formula(formula_string),
        data = x,
        contrasts = sapply(
          X = fact.vars,
          FUN = function(x) return(stats::contr.sum),
          simplify = FALSE
        )
      ),
      type = 3
    )
  )
  return(anova_out)
}
