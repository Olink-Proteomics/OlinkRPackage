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
#' towards adjusted `p-value<0.05`.
#'
#' @param df NPX data frame in long format with at least protein name (Assay),
#' OlinkID, UniProt, 1-2 variables with at least 2 levels.
#' @param variable Single character value or character array. Variables to test.
#' If `length > 1`, the included variable names will be used in crossed
#' analyses. Also takes ':' or '*' notation.
#' @param check_log A named list returned by [`check_npx()`]. If `NULL`,
#' [`check_npx()`] will be run internally using `df`.
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
#'   # check data
#'   npx_df_check_log <- OlinkAnalyze::check_npx(
#'     df = npx_df
#'   )
#'
#'   # Results in model NPX ~ Time * Treatment + (1 | Subject) + (1 | Site)
#'   lmer_results <- OlinkAnalyze::olink_lmer(
#'     df = npx_df,
#'     check_log = npx_df_check_log,
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
    if ("formula" %in% class(model_formula)) {
      model_formula <- deparse(model_formula) # Convert to string if is formula
    }
    tryCatch(
      stats::as.formula(object = model_formula),
      # If cannot be coerced into formula, error
      error = function(e) {
        stop(paste0(model_formula, " is not a recognized formula."))
      }
    )

    # If variable, random or covariates were included, message that they will
    # not be used as model_formula is provided
    if (!missing(variable) || !is.null(covariates) || !missing(random)) {
      message(
        paste("model_formula overriding variable, covariate and random",
              "arguments.")
      )
    }

    # Parse formula so checks on the variable, outcome and random objects can
    # continue as usual
    model_formula <- gsub(pattern = " ", replacement = "", x = model_formula)

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

    outcome <- splt_form[1L]
    variable <- splt_form[-1L]
    covariates <- NULL
  }

  if (missing(df) || missing(variable) || missing(random)) {
    stop("The df and variable and random arguments need to be specified.")
  }

  lmer_result <- withCallingHandlers(
    {
      # Filtering on valid OlinkID
      df <- df |>
        dplyr::filter(
          stringr::str_detect(
            string = .data[["OlinkID"]],
            pattern = "OID[0-9]{5}"
          )
        )

      # Allow for :/* notation in covariates and expand crossed terms
      expanded_terms <- expand_crossed_formula_terms(
        variable = variable,
        covariates = covariates
      )
      variable <- expanded_terms$variable
      covariates <- expanded_terms$covariates
      add.main.effects <- expanded_terms$add.main.effects # nolint: object_name_linter

      # Variables to be checked
      variable_testers <- intersect(x = c(variable, covariates, random),
                                    y = names(df))

      # Remove rows where variables or covariate is NA (can't include in
      # analysis anyway)
      na_removed <- remove_na_samples(df = df, variable_testers = variable_testers)
      df <- na_removed$df
      removed.sampleids <- na_removed$removed.sampleids # nolint: object_name_linter

      # Check data format
      check_log <- run_check_npx(df = df, check_log = check_log)

      # Convert character vars to factor
      conversion_result <- convert_vars_to_factors(
        df = df,
        variable_testers = variable_testers
      )
      df <- conversion_result$df
      converted.vars <- conversion_result$converted.vars # nolint: object_name_linter
      num.vars <- conversion_result$num.vars # nolint: object_name_linter

      # Not testing assays that have all NA:s in one level
      # Every sample needs to have a unique level of the factor

      # Identify single fixed effects to check
      if (!is.null(covariates)) {
        factors_in_df <- names(df)[sapply(df, is.factor)]
        single_fixed_effects <- c(variable,
                                  intersect(x = covariates,
                                            y = factors_in_df))
      } else {
        single_fixed_effects <- variable
      }

      # Detect assays with all NAs in at least one level
      nas_in_var <- detect_assays_with_na_levels(
        df = df,
        single_fixed_effects = single_fixed_effects,
        check_log = check_log,
        outcome = outcome
      )

      # Validate that samples have unique factor levels
      validate_unique_levels_per_sample(
        df = df,
        single_fixed_effects = single_fixed_effects
      )

      if (missing(model_formula)) {
        if (!is.null(covariates)) {
          formula_string <- paste0(
            outcome, "~", paste(variable, collapse = "*"), "+",
            paste(covariates, sep = "", collapse = "+"), "+",
            paste(paste0("(1|", random, ")"), collapse = "+")
          )
        } else {
          formula_string <- paste0(
            outcome, "~", paste(variable, collapse = "*"), "+",
            paste(paste0("(1|", random, ")"), collapse = "+")
          )
        }
      } else if (!missing(model_formula)) {
        formula_string <- model_formula
      }

      #Get factors
      fact.vars <- sapply(variable_testers, function(x) is.factor(df[[x]])) # nolint: object_name_linter
      fact.vars <- names(fact.vars)[fact.vars] # nolint: object_name_linter

      # Print verbose messages
      print_verbose_messages(
        verbose = verbose,
        add.main.effects = add.main.effects,
        removed.sampleids = removed.sampleids,
        converted.vars = converted.vars,
        num.vars = num.vars,
        formula_string = formula_string,
        # Note: no trailing space maintains backward compatibility with original
        model_type = "Linear mixed effects model fit to each assay:"
      )

      # Build covariate filter string
      covariate_filter_string <- build_covariate_filter_string(covariates = covariates)

      ##make LMM
      lmer_model <- df |>
        # Exclude assays that have all NA:s
        dplyr::filter(
          !(.data[["OlinkID"]] %in% check_log$assay_na)
        ) |>
        dplyr::filter(
          !(.data[["OlinkID"]] %in% .env[["nas_in_var"]])
        ) |>
        dplyr::group_by(
          dplyr::across(
            dplyr::all_of(
              c("Assay", "OlinkID", "UniProt", "Panel")
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
        dplyr::mutate(
          Adjusted_pval = stats::p.adjust(
            p = .data[["p.value"]],
            method = "fdr"
          ),
          Threshold = dplyr::if_else(
            .data[["Adjusted_pval"]] < 0.05,
            "Significant",
            "Non-significant"
          )
        ) |>
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
      restart_if_spec_warn <- grepl(
        x = w,
        pattern = "not\\s+recognized\\s+or\\s+transformed"
      ) |
        grepl(
          x = w,
          pattern = utils::glob2rx("*contains implicit NA, consider using*")
        )

      if (restart_if_spec_warn == TRUE) {
        invokeRestart("muffleWarning")
      }
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

  out.model <- tryCatch( # nolint: object_name_linter
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

  if (inherits(out.model, "lmerModLmerTest")) {
    return(out.model)
  } else {
    stop("Convergence issue not caught by single_lmer")
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
#' @param df NPX data frame in long format with at least protein name (Assay),
#' OlinkID, UniProt, 1-2 variables with at least 2 levels and subject
#' identifier.
#' @param check_log A named list returned by [`check_npx()`]. If `NULL`,
#' [`check_npx()`] will be run internally using `df`.
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
#' if (rlang::is_installed(pkg = c("lme4", "lmerTest", "emmeans"))) {
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
#'   # check data
#'   npx_df_check_log <- OlinkAnalyze::check_npx(
#'     df = npx_df
#'   )
#'
#'   # Results in model NPX ~ Time * Treatment + (1 | Subject)
#'   lmer_results <- OlinkAnalyze::olink_lmer(
#'     df = npx_df,
#'     check_log = npx_df_check_log,
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
#'     check_log = npx_df_check_log,
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
#'     check_log = npx_df_check_log,
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
    if ("formula" %in% class(model_formula)) {
      model_formula <- deparse(model_formula) # Convert to string if is formula
    }
    tryCatch(
      stats::as.formula(object = model_formula),
      # If cannot be coerced into formula, error
      error = function(e) {
        stop(paste0(model_formula, " is not a recognized formula."))
      }
    )

    # If variable and covariates were included, throw a message that they will
    # not be used as model_formula is provided
    if (!missing(variable) || !is.null(covariates) || !missing(random)) {
      message(
        paste("model_formula overriding variable and covariate and",
              "random arguments.")
      )
    }

    # Parse formula so checks on the  variable and outcome objects can continue
    # as usual
    model_formula <- gsub(pattern = " ", replacement = "", x = model_formula)

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
    outcome <- splt_form[1L]
    variable <- splt_form[-1L]
    covariates <- NULL
  }

  if (!missing(effect_formula)) {
    if (length(effect_formula) == 1L) {
      # Parse effect formula so the check on the effect object can continue as
      # usual
      if (!missing(effect)) {
        message("effect_formula overriding effect argument.")
      }
      if ("formula" %in% class(effect_formula)) {
        effect_formula <- deparse(effect_formula)
      }
      splt_effect <- effect_formula
      if (grepl(pattern = "~", x = splt_effect)) {
        # Pull out variables from right hand side of formula.
        # e.g. pairwise~A+B|C = "A+B|C"
        splt_effect <- strsplit(x = splt_effect, split = "~")[[1L]][2L]
      }
      if (grepl(pattern = "\\||+|\\*", x = splt_effect)) {
        # Split rhs of formula into vector of variables.
        # e.g. "A+B|C"=c("A","B","C")
        splt_effect <- strsplit(x = splt_effect, split = "\\||\\+|\\*")[[1L]]
      }
      effect <- splt_effect
    } else {
      stop(
        paste0(
          "Unrecognized effect formula. Should be a character string of length",
          "1. If listing in the form c('A','B'), use the effects argument."
        )
      )
    }
  }

  if (missing(df) || missing(variable) || missing(effect) || missing(random)) {
    stop("The df, variable, random and effect arguments need to be specified.")
  }

  tmp_efect <- strsplit(x = effect, split = ":") |>
    unlist() |>
    unique()
  tmp_variable <- strsplit(x = variable, split = "[\\*:]") |>
    unlist() |>
    unique()
  if (!all(tmp_efect %in% tmp_variable)) {
    stop("All effect terms must be included in the variable argument.")
  }

  lmer_posthoc_result <- withCallingHandlers(
    {
      #Filtering on valid OlinkID
      df <- df |>
        dplyr::filter(
          stringr::str_detect(
            string = .data[["OlinkID"]],
            pattern = "OID[0-9]{5}"
          )
        )

      if (is.null(olinkid_list) || length(olinkid_list) == 0L) {
        olinkid_list <- df |>
          dplyr::select(
            dplyr::all_of("OlinkID")
          ) |>
          dplyr::distinct() |>
          dplyr::pull()
      }

      #Allow for :/* notation in covariates
      variable <- gsub(pattern = "\\*", replacement = ":", x = variable)
      if (!is.null(covariates)) {
        covariates <- gsub(pattern = "\\*", replacement = ":", x = covariates)
      }

      add.main.effects <- NULL # nolint: object_name_linter
      if (any(grepl(pattern = ":", x = covariates))) {
        tmp <- strsplit(x = covariates, split = ":") |> unlist()
        add.main.effects <- c(add.main.effects, # nolint: object_name_linter
                              setdiff(x = tmp, y = covariates))
        covariates <- union(x = covariates, y = add.main.effects)
      }

      if (any(grepl(pattern = ":", x = variable))) {
        tmp <- strsplit(x = variable, split = ":") |> unlist()
        add.main.effects <- c(add.main.effects, setdiff(x = tmp, y = variable)) # nolint: object_name_linter
        variable <- union(x = variable,
                          y = unlist(strsplit(x = variable, split = ":")))
        variable <- variable[!grepl(pattern = ":", x = variable)]
      }

      # If variable is in both variable and covariate, keep it in variable or
      # will get removed from final table
      covariates <- setdiff(x = covariates, y = variable)
      add.main.effects <- setdiff(x = add.main.effects, y = variable) # nolint: object_name_linter

      variable_testers <- intersect(x = c(variable, covariates), y = names(df))
      # Remove rows where variables or covariate is NA (cant include in analysis
      # anyway)
      removed.sampleids <- NULL # nolint: object_name_linter
      for (i in variable_testers) {
        removed.sampleids <- c(removed.sampleids, # nolint: object_name_linter
                               df$SampleID[is.na(df[[i]])]) |>
          unique()
        df <- df[!is.na(df[[i]]), ]
      }

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

      if (missing(model_formula)) {
        if (!is.null(covariates)) {
          formula_string <- paste0(
            outcome, "~", paste(variable, collapse = "*"), "+",
            paste(covariates, sep = "", collapse = "+"), "+",
            paste(paste0("(1|", random, ")"), collapse = "+")
          )
        } else {
          formula_string <- paste0(
            outcome, "~", paste(variable, collapse = "*"), "+",
            paste(paste0("(1|", random, ")"), collapse = "+")
          )
        }
      } else if (!missing(model_formula)) {
        formula_string <- model_formula
      }

      if (!missing(effect_formula)) {
        e_form <- effect_formula # nolint: object_usage_linter
      } else if (missing(effect_formula)) {
        e_form <- paste0("pairwise~", paste(effect, collapse = "+")) # nolint: object_usage_linter
      }

      #Print verbose message
      if (verbose) {
        if (!is.null(add.main.effects) & length(add.main.effects) > 0L) {
          message(
            "Missing main effects added to the model formula: ",
            paste(add.main.effects, collapse = ", ")
          )
        }
        if (!is.null(removed.sampleids) & length(removed.sampleids) > 0L) {
          message(
            "Samples removed due to missing variable or covariate levels: ",
            paste(removed.sampleids, collapse = ", ")
          )
        }
        if (!is.null(converted.vars)) {
          message(
            paste0(
              "Variables and covariates converted from character to factors: ",
              paste(converted.vars, collapse = ", ")
            )
          )
        }
        if (!is.null(num.vars)) {
          message(
            paste0("Variables and covariates treated as numeric: ",
                   paste(num.vars, collapse = ", "))
          )
        }
        if (any(variable %in% num.vars)) {
          message(
            paste0(
              "Numeric variables post-hoc performed using",
              " Mean and Mean + 1SD: ",
              paste(num.vars[num.vars %in% variable], collapse = ", ")
            )
          )
        }
        message(
          paste("Means estimated for each assay from linear mixed effects",
                "model:", formula_string)
        )
      }

      # Check data format
      check_log <- run_check_npx(df = df, check_log = check_log)

      output_df <- df |>
        dplyr::filter(
          .data[["OlinkID"]] %in% .env[["olinkid_list"]]
        ) |>
        # Exclude assays that have all NA:s
        dplyr::filter(
          !(.data[["OlinkID"]] %in% check_log$assay_na)
        ) |>
        dplyr::group_by(
          dplyr::across(
            dplyr::all_of(
              c("Assay", "OlinkID", "UniProt", "Panel")
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
            c("Assay", "OlinkID", "UniProt", "Panel", "term")
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
      restart_if_spec_warn <- grepl(
        x = w,
        pattern = utils::glob2rx("*contains implicit NA, consider using*")
      )
      if (restart_if_spec_warn == TRUE) {
        invokeRestart("muffleWarning")
      }
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
    stop("effect must be a character string.")
  }

  the_model <- emmeans::emmeans(
    object = single_lmer(
      data = data,
      formula_string = formula_string
    ),
    # effect must be string to be converted to as.formula
    specs = stats::as.formula(effect),
    cov.reduce = function(x) {
      round(x = c(mean(x), mean(x) + stats::sd(x)), digits = 4L) # nolint: return_linter
    },
    lmer.df = "satterthwaite",
    infer = c(TRUE, TRUE),
    adjust = padjust_method
  )

  if (mean_return) {
    return(
      the_model$emmeans |>
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
    out_df <- the_model$contrasts |>
      dplyr::as_tibble() |>
      dplyr::rename(
        "Adjusted_pval" = "p.value",
        "conf.low" = "lower.CL",
        "conf.high" = "upper.CL"
      ) |>
      dplyr::mutate(
        Threshold = dplyr::if_else(
          .data[["Adjusted_pval"]] < 0.05,
          "Significant",
          "Non-significant"
        )
      ) |>
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
