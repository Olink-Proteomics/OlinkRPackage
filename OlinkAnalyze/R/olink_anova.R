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
#' @param check_log A named list returned by [`check_npx()`]. If `NULL`,
#' [`check_npx()`] will be run internally using `df`.
#' @param variable Single character value or character array. Variable(s) to
#' test. If length > 1, the included variable names will be used in crossed
#' analyses. Also takes ':' or '*' notation.
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
#'   # check data
#'   npx_df_check_log <- OlinkAnalyze::check_npx(
#'     df = npx_df
#'   )
#'
#'   # One-way ANOVA, no covariates.
#'   # Results in a model NPX~Time
#'   anova_results <- OlinkAnalyze::olink_anova(
#'     df = npx_df,
#'     check_log = npx_df_check_log,
#'     variable = "Time"
#'   )
#'
#'   # Two-way ANOVA, one main effect covariate.
#'   # Results in model NPX~Treatment*Time+Site.
#'   anova_results <- OlinkAnalyze::olink_anova(
#'     df = npx_df,
#'     check_log = npx_df_check_log,
#'     variable = c("Treatment:Time"),
#'     covariates = "Site"
#'   )
#'
#'   # One-way ANOVA, interaction effect covariate.
#'   # Results in model NPX~Treatment+Site:Time+Site+Time.
#'   anova_results <- OlinkAnalyze::olink_anova(
#'     df = npx_df,
#'     check_log = npx_df_check_log,
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
                        return.covariates = FALSE, # nolint object_name_linter
                        verbose = TRUE) {

  # Check if all required libraries for this function are installed
  rlang::check_installed(
    pkg = c("broom", "car"),
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
    if (!missing(variable) || !is.null(covariates)) {
      message("model_formula overriding variable and covariate arguments.")
    }

    # Parse formula so checks on the variable and outcome objects can continue
    # as usual
    model_formula <- gsub(pattern = " ", replacement = "", x = model_formula)
    splt_form <- strsplit(x = model_formula, split = c("\\+|~|\\*|:"))[[1L]]

    if ("-1" %in% splt_form) {
      splt_form <- splt_form[-which(splt_form == "-1")]
    }

    outcome <- splt_form[1L]
    variable <- splt_form[-1L]
    covariates <- NULL
  }

  if (missing(df) || missing(variable)) {
    stop("The df and variable arguments need to be specified.")
  }

  # Stop if internal controls (assays) have not been removed
  if ("AssayType" %in% names(df)) {
    if (any(df$AssayType != "assay")) {
      ctrl_assays <- df |>
        dplyr::filter(
          .data[["AssayType"]] != "assay"
        )

      stop(
        paste0(
          "Control assays have not been removed from the dataset.\n",
          "Assays with AssayType != \"assay\" should be excluded.\n",
          "The following ", length(unique(ctrl_assays$Assay)),
          " control assays were found:\n",
          paste(
            strwrap(
              toString(unique(ctrl_assays$Assay)),
              width = 80L
            ),
            collapse = "\n"
          )
        )
      )
    }
  } else {
    ctrl_assays <- df |>
      dplyr::filter(
        stringr::str_detect(
          .data[["Assay"]],
          stringr::regex("(?i)(?=.*\\bcontrol|ctrl\\b)(?!.*\\bCTRL\\b)")
        )
      )
    if (nrow(ctrl_assays) > 0L) {
      stop(
        paste0(
          "Control assays have not been removed from the dataset.\n",
          "Assays with \"control\" in their Assay field should be excluded.\n",
          "The following ", length(unique(ctrl_assays$Assay)),
          " control assays were found:\n",
          paste(
            strwrap(
              toString(unique(ctrl_assays$Assay)),
              width = 80L
            ),
            collapse = "\n"
          )
        )
      )
    }
  }

  anova_result <- withCallingHandlers(
    {
      #Filtering on valid OlinkID
      df <- df |>
        dplyr::filter(
          stringr::str_detect(
            string = .data[["OlinkID"]],
            pattern = "OID[0-9]{5}"
          )
        )

      #Allow for :/* notation in covariates
      variable <- gsub(pattern = "\\*", replacement = ":", x = variable)
      if (!is.null(covariates)) {
        covariates <- gsub(pattern = "\\*", replacement = ":", x = covariates)
      }

      add.main.effects <- NULL # nolint object_name_linter
      if (any(grepl(":", covariates))) {
        tmp <- unlist(strsplit(covariates, ":"))
        add.main.effects <- c(add.main.effects, setdiff(tmp, covariates)) # nolint object_name_linter
        covariates <- union(covariates, add.main.effects)
      }
      if (any(grepl(":", variable))) {
        tmp <- unlist(strsplit(variable, ":"))
        add.main.effects <- c(add.main.effects, setdiff(tmp, variable)) # nolint object_name_linter
        variable <- union(variable, unlist(strsplit(variable, ":")))
        variable <- variable[!grepl(":", variable)]
      }
      # If variable is in both variable and covariate, keep it in variable or
      # will get removed from final table
      covariates <- setdiff(x = covariates, y = variable)
      add.main.effects <- setdiff(x = add.main.effects, y = variable) # nolint object_name_linter

      # Variables to check
      variable_testers <- intersect(x = c(variable, covariates), y = names(df))

      # Remove rows where variables or covariate is NA (cant include in analysis
      # anyway)
      removed.sampleids <- NULL # nolint object_name_linter
      for (i in variable_testers) {
        removed.sampleids <- c(removed.sampleids, df$SampleID[is.na(df[[i]])]) |> # nolint object_name_linter
          unique()
        df <- df[!is.na(df[[i]]), ]
      }

      # Check data format
      check_log <- run_check_npx(df = df, check_log = check_log)

      ##Convert character vars to factor
      converted.vars <- NULL # nolint object_name_linter
      num.vars <- NULL # nolint object_name_linter
      for (i in variable_testers) {
        if (is.character(df[[i]])) {
          df[[i]] <- factor(df[[i]])
          converted.vars <- c(converted.vars, i) # nolint object_name_linter
        } else if (is.numeric(df[[i]])) {
          num.vars <- c(num.vars, i) # nolint object_name_linter
        }
      }

      #Not testing assays that have all NA:s in one level
      #Every sample needs to have a unique level of the factor

      nas_in_var <- character(0)

      if (!is.null(covariates)) {
        factors_in_df <- names(df)[sapply(df, is.factor)]
        single_fixed_effects <- c(variable,
                                  intersect(covariates,
                                            factors_in_df))
      } else {
        single_fixed_effects <- variable
      }

      for (effect in single_fixed_effects) {

        current_nas <- df |>
          dplyr::filter( # Exclude assays that have all NA:s
            !(.data[["OlinkID"]] %in% check_log$assay_na)
          ) |>
          dplyr::group_by(
            .data[["OlinkID"]], .data[[effect]]
          ) |>
          dplyr::summarise(
            n = dplyr::n(),
            n_na = sum(is.na(.data[["NPX"]]))
          ) |>
          dplyr::ungroup() |>
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
              " has only NA:s in atleast one level of ", effect,
              ". It will not be tested."
            ),
            call. = FALSE
          )
        }

        n_samples_w_more_than_1_level <- df |>
          dplyr::group_by(
            .data[["SampleID"]]
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

      if (missing(model_formula)) {
        if (!is.null(covariates)) {
          formula_string <- paste0(
            outcome, "~", paste(variable, collapse = "*"), "+",
            paste(covariates, sep = "", collapse = "+")
          )
        } else {
          formula_string <- paste0(
            outcome, "~", paste(variable, collapse = "*")
          )
        }
      } else if (!missing(model_formula)) {
        formula_string <- model_formula
      }

      #Get factors
      fact.vars <- sapply(variable_testers, function(x) is.factor(df[[x]])) # nolint object_name_linter
      fact.vars <- names(fact.vars)[fact.vars] # nolint object_name_linter


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
        message(
          paste("ANOVA model fit to each assay: "), formula_string
        )
      }

      if (!is.null(covariates) & any(grepl(":", covariates))) {
        covariate_filter_string <- covariates[stringr::str_detect(covariates, ":")] # nolint line_length_linter
        covariate_filter_string <- sub(
          pattern = "(.*)\\:(.*)$",
          replacement = "\\2:\\1",
          x = covariate_filter_string
        )
        covariate_filter_string <- c(covariates, covariate_filter_string)
      } else {
        covariate_filter_string <- covariates
      }

      p.val <- df |> # nolint object_name_linter
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
        dplyr::mutate(
          Adjusted_pval = stats::p.adjust(
            p = .data[["p.value"]],
            method = "fdr"
          ),
          Threshold  = dplyr::if_else(
            .data[["Adjusted_pval"]] < 0.05,
            "Significant",
            "Non-significant"
          )
        ) |>
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
        p.val <- p.val |> # nolint object_name_linter
          dplyr::filter(
            !(.data[["term"]] %in% .env[["covariate_filter_string"]])
          )
      }

      return(p.val)
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
#' @param check_log A named list returned by [`check_npx()`]. If `NULL`,
#' [`check_npx()`] will be run internally using `df`.
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
#' if (rlang::is_installed(pkg = c("broom", "car", "emmeans"))) {
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
#'   # check data
#'   npx_df_check_log <- OlinkAnalyze::check_npx(
#'     df = npx_df
#'   )
#'
#'   # Two-way ANOVA, one main effect (Site) covariate.
#'   # Results in model NPX~Treatment*Time+Site.
#'   anova_results <- OlinkAnalyze::olink_anova(
#'     df = npx_df,
#'     check_log = npx_df_check_log,
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
#'     check_log = npx_df_check_log,
#'     variable = c("Treatment:Time"),
#'     covariates = "Site",
#'     olinkid_list = significant_assays,
#'     effect = "Treatment:Time"
#'   )
#'
#'   # Posthoc, treated vs untreated at each timepoint, adjusted for Site effect
#'   anova_posthoc_results <- OlinkAnalyze::olink_anova_posthoc(
#'     df = npx_df,
#'     check_log = npx_df_check_log,
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
    pkg = c("broom", "car", "emmeans"),
    call = rlang::caller_env()
  )

  if (!missing(model_formula)) {
    if ("formula" %in% class(model_formula)) {
      model_formula <- deparse(model_formula) #Convert to string if is formula
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
    if (!missing(variable) || !is.null(covariates)) {
      message("model_formula overriding variable and covariate arguments.")
    }

    # Parse formula so checks on the  variable and outcome objects can continue
    # as usual
    model_formula <- gsub(pattern = " ", replacement = "", x = model_formula)
    splt_form <- strsplit(x = model_formula, split = c("\\+|~|\\*|:"))[[1L]]

    if ("-1" %in% splt_form) {
      splt_form <- splt_form[-which(splt_form == "-1")]
    }

    outcome <- splt_form[1L]
    variable <- splt_form[-1L]
    covariates <- NULL
  }

  if (!missing(effect_formula)) {
    if (length(effect_formula) == 1L) {
      #Parse formula so the check on the effect object can continue as usual
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

  if (missing(df) || missing(variable) || missing(effect)) {
    stop("The df and variable and effect arguments need to be specified.")
  }

  tmp_efect <- strsplit(x = effect, split = ":") |>
    unlist() |>
    unique()
  tmp_variable <- strsplit(x = variable, split = "[\\*:]") |>
    unlist() |>
    unique()
  if (!all(tmp_efect %in% tmp_variable)) {
    stop(
      paste0(
        "All effect terms must be included in the variable argument or model",
        " formula."
      )
    )
  }

  # Stop if internal controls (assays) have not been removed
  if ("AssayType" %in% names(df)) {
    if (any(df$AssayType != "assay")) {
      ctrl_assays <- df |>
        dplyr::filter(
          .data[["AssayType"]] != "assay"
        )

      stop(
        paste0(
          "Control assays have not been removed from the dataset.\n",
          "Assays with AssayType != \"assay\" should be excluded.\n",
          "The following ", length(unique(ctrl_assays$Assay)),
          " control assays were found:\n",
          paste(
            strwrap(
              toString(unique(ctrl_assays$Assay)),
              width = 80L
            ),
            collapse = "\n"
          )
        )
      )
    }
  } else {
    ctrl_assays <- df |>
      dplyr::filter(
        stringr::str_detect(
          .data[["Assay"]],
          stringr::regex("(?i)(?=.*\\bcontrol|ctrl\\b)(?!.*\\bCTRL\\b)")
        )
      )
    if (nrow(ctrl_assays) > 0L) {
      stop(
        paste0(
          "Control assays have not been removed from the dataset.\n",
          "Assays with \"control\" in their Assay field should be excluded.\n",
          "The following ", length(unique(ctrl_assays$Assay)),
          " control assays were found:\n",
          paste(
            strwrap(
              toString(unique(ctrl_assays$Assay)),
              width = 80L
            ),
            collapse = "\n"
          )
        )
      )
    }
  }

  anova_posthoc_result <- withCallingHandlers(
    {
      #Filtering on valid OlinkID
      df <- df |>
        dplyr::filter(
          stringr::str_detect(
            string = .data[["OlinkID"]],
            pattern = "OID[0-9]{5}"
          )
        )

      if (is.null(olinkid_list)) {
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

      add.main.effects <- NULL # nolint object_name_linter
      if (any(grepl(":", covariates))) {
        tmp <- unlist(strsplit(covariates, ":"))
        add.main.effects <- c(add.main.effects, setdiff(tmp, covariates)) # nolint object_name_linter
        covariates <- union(covariates, add.main.effects)
      }
      if (any(grepl(":", variable))) {
        tmp <- unlist(strsplit(variable, ":"))
        add.main.effects <- c(add.main.effects, setdiff(tmp, variable)) # nolint object_name_linter
        variable <- union(variable, unlist(strsplit(variable, ":")))
        variable <- variable[!grepl(":", variable)]
      }
      # If variable is in both variable and covariate, keep it in variable or
      # will get removed from final table
      covariates <- setdiff(x = covariates, y = variable)
      add.main.effects <- setdiff(x = add.main.effects, y = variable) # nolint object_name_linter

      # Variables to check
      variable_testers <- intersect(x = c(variable, covariates), y = names(df))

      # Remove rows where variables or covariate is NA (cant include in analysis
      # anyway)
      removed.sampleids <- NULL # nolint object_name_linter
      for (i in variable_testers) {
        removed.sampleids <- c(removed.sampleids, # nolint object_name_linter
                               df$SampleID[is.na(df[[i]])]) |>
          unique()
        df <- df[!is.na(df[[i]]), ]
      }

      # Check data format
      check_log <- run_check_npx(df = df, check_log = check_log)

      # Convert character vars to factor
      converted.vars <- NULL # nolint object_name_linter
      num.vars <- NULL # nolint object_name_linter
      for (i in variable_testers) {
        if (is.character(df[[i]])) {
          df[[i]] <- factor(df[[i]])
          converted.vars <- c(converted.vars, i) # nolint object_name_linter
        } else if (is.numeric(df[[i]])) {
          num.vars <- c(num.vars, i) # nolint object_name_linter
        }
      }

      if (missing(model_formula)) {
        if (!is.null(covariates)) {
          formula_string <- paste0(
            outcome, "~", paste(variable, collapse = "*"), "+",
            paste(covariates, sep = "", collapse = "+")
          )
        } else {
          formula_string <- paste0(
            outcome, "~", paste(variable, collapse = "*")
          )
        }
      } else if (!missing(model_formula)) {
        formula_string <- model_formula
      }

      # Print verbose message
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
          paste("Means estimated for each assay from ANOVA model:",
                formula_string)
        )
      }

      if (!missing(effect_formula)) {
        e_form <- stats::as.formula(effect_formula) # nolint object_usage_linter
      } else if (missing(effect_formula)) {
        e_form <- stats::as.formula(
          paste0("pairwise~", paste(effect, collapse = "+")) # nolint object_usage_linter
        )
      }

      anova_posthoc_results <- df |>
        dplyr::filter(
          .data[["OlinkID"]] %in% .env[["olinkid_list"]]
        ) |>
        # Exclude assays that have all NA:s
        dplyr::filter(
          !(.data[["OlinkID"]] %in% check_log$assay_na)
        ) |>
        dplyr::mutate(
          OlinkID = factor(
            x = .data[["OlinkID"]],
            levels = .env[["olinkid_list"]]
          )
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
          emmeans_result <- emmeans::emmeans(
            object = model,
            specs = e_form,
            cov.reduce = function(x) {
              round_num <- round(x = c(mean(x), mean(x) + stats::sd(x)),
                                 digits = 4L)
              return(round_num)
            },
            infer = c(TRUE, TRUE),
            adjust = post_hoc_padjust_method
          )
          result_type <- ifelse(mean_return == TRUE, "emmeans", "contrasts")
          data.frame(
            emmeans_result[[result_type]],
            stringsAsFactors = FALSE
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
          dplyr::mutate(
            Threshold = dplyr::if_else(.data[["Adjusted_pval"]] < 0.05,
                                       "Significant",
                                       "Non-significant")
          ) |>
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

    }, warning = function(w) {
      restart_if_spec_warn <- grepl(
        x = w,
        pattern = utils::glob2rx("*contains implicit NA, consider using*")
      )
      if (restart_if_spec_warn == TRUE) {
        invokeRestart("muffleWarning")
      }
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
                           fact.vars) {  # nolint object_name_linter
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
