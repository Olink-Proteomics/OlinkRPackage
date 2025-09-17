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
#' @param check_log A named list returned by [`check_npx()`]. If `NULL`,
#' [`check_npx()`] will be run internally using `df`.
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
#'   check_log <- OlinkAnalyze::check_npx(df = npx_df)
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
                                     return.covariates = FALSE, # nolint object_name_linter
                                     check_log = NULL,
                                     verbose = TRUE) {
  # Check if all required libraries for this function are installed
  rlang::check_installed(
    pkg = c("ordinal", "broom"),
    call = rlang::caller_env()
  )

  if (missing(df) || missing(variable)) {
    stop("The df and variable arguments need to be specified.")
  }

  ord_regr_results <- withCallingHandlers(
    {
      # Filtering on valid OlinkID
      df <- df |>
        dplyr::filter(
          stringr::str_detect(
            string = .data[["OlinkID"]],
            pattern = "OID[0-9]{5}"
          )
        )

      # Allow for :/* notation in covariates
      variable <- gsub(pattern = "\\*", replacement = ":", x = variable)
      if (!is.null(covariates)) {
        covariates <- gsub(pattern = "\\*", replacement = ":", x = covariates)
      }

      add.main.effects <- NULL # nolint object_name_linter

      if (any(grepl(pattern = ":", x = covariates))) {
        tmp <- strsplit(x = covariates, split = ":") |> unlist()
        add.main.effects <- c(add.main.effects, # nolint object_name_linter
                              setdiff(x = tmp, y = covariates))
        covariates <- union(x = covariates, y = add.main.effects)
      }
      if (any(grepl(pattern = ":", x = variable))) {
        tmp <- strsplit(x = variable, split = ":") |> unlist()
        add.main.effects <- c(add.main.effects, # nolint object_name_linter
                              setdiff(x = tmp, y = variable))
        variable <- union(x = variable,
                          y = unlist(strsplit(x = variable, split = ":")))
        variable <- variable[!grepl(pattern = ":", x = variable)]
      }

      # If variable is in both variable and covariate, keep it in variable or
      # will get removed from final table
      covariates <- setdiff(x = covariates, y = variable)
      add.main.effects <- setdiff(x = add.main.effects, y = variable) # nolint object_name_linter

      # Variables to check
      variable_testers <- intersect(x = c(variable, covariates),
                                    y = names(df))

      # Remove rows where variables or covariate is NA (cant include in analysis
      # anyway)
      removed.sampleids <- NULL # nolint object_name_linter
      for (i in variable_testers) {
        removed.sampleids <- c(removed.sampleids, # nolint object_name_linter
                               df$SampleID[is.na(df[[i]])]) |>
          unique()
        df <- df |>
          dplyr::filter(
            !is.na(.data[[i]])
          )
      }

      # check data format
      check_log <- run_check_npx(df = df, check_log = check_log)
      data_type <- check_log$col_names$quant

      # Convert outcome to factor
      df <- df |>
        dplyr::mutate(
          !!data_type := factor(x = .data[[data_type]], ordered = TRUE)
        )

      # Convert character vars to factor
      converted.vars <- NULL # nolint object_name_linter
      num.vars <- NULL # nolint object_name_linter
      for (i in variable_testers) {
        if (is.character(df[[i]])) {
          df <- df |>
            dplyr::mutate(
              !!i := factor(x = .data[[i]])
            )
          converted.vars <- c(converted.vars, i) # nolint object_name_linter
        } else if (is.numeric(df[[i]])) {
          num.vars <- c(num.vars, i) # nolint object_name_linter
        }
      }

      # Not testing assays that have all NA:s in one level
      # Every sample needs to have a unique level of the factor

      nas_in_var <- character(0)

      if (!is.null(covariates)) {
        factors_in_df <- names(df)[sapply(df, is.factor) == TRUE]
        single_fixed_effects <- c(variable,
                                  intersect(x = covariates, y = factors_in_df))
      } else {
        single_fixed_effects <- variable
      }

      for (effect in single_fixed_effects) {
        current_nas <- df |>
          dplyr::filter(
            !(.data[["OlinkID"]] %in% check_log$assay_na)
          ) |>
          dplyr::group_by(
            .data[["OlinkID"]], .data[[effect]]
          ) |>
          dplyr::summarise(
            n = dplyr::n(),
            n_na = sum(is.na(.data[[data_type]])),
            .groups = "drop"
          ) |>
          dplyr::filter(
            .data[["n"]] == .data[["n_na"]]
          ) |>
          dplyr::pull(
            .data[["OlinkID"]]
          ) |>
          unique()
        if (length(current_nas) > 0L) {
          nas_in_var <- c(nas_in_var, current_nas)
          warning(paste0("The assay(s) ", current_nas,
                         " has only NA:s in atleast one level of ", effect,
                         ". It will not be tested."),
                  call. = FALSE)
        }

        # Every sample needs to have a unique level of the factor
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

      if (!is.null(covariates)) {
        formula_string <- paste0(data_type, "~",
                                 paste(variable, collapse = "*"), "+",
                                 paste(covariates, sep = "", collapse = "+"))
      } else {
        formula_string <- paste0(data_type, "~",
                                 paste(variable, collapse = "*"))
      }

      # Get factors
      fact.vars <- sapply(variable_testers, function(x) is.factor(df[[x]])) # nolint object_name_linter
      fact.vars <- names(fact.vars)[fact.vars] # nolint object_name_linter

      #Print verbose message
      if (verbose) {
        if (!is.null(add.main.effects) && length(add.main.effects) > 0L) {
          message(paste0(
            "Missing main effects added to the model formula: ",
            paste(add.main.effects, collapse = ", ")
          ))
        }
        if (!is.null(removed.sampleids) && length(removed.sampleids) > 0L) {
          message(paste0(
            "Samples removed due to missing variable or covariate levels: ",
            paste(removed.sampleids, collapse = ", ")
          ))
        }
        if (!is.null(converted.vars)) {
          message(paste0(
            "Variables and covariates converted from character to factors: ",
            paste(converted.vars, collapse = ", ")
          ))
        }
        if (!is.null(num.vars)) {
          message(paste0(
            "Variables and covariates treated as numeric: ",
            paste(num.vars, collapse = ", ")
          ))
        }
        message(paste("Cumulative Link Model (CLM) fit to each assay:",
                      formula_string))
      }

      if (!is.null(covariates) & any(grepl(":", covariates))) {
        covariate_filter_str <- covariates[grepl(pattern = ":", x = covariates)]
        covariate_filter_str <- sub(pattern = "(.*)\\:(.*)$",
                                    replacement = "\\2:\\1",
                                    x = covariate_filter_str)
        covariate_filter_str <- c(covariates, covariate_filter_str)

      } else {
        covariate_filter_str <- covariates
      }

      p.val <- df |> # nolint object_name_linter
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
              type = 3L
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
        dplyr::mutate(
          Adjusted_pval = stats::p.adjust(p = .data[["p.value"]],
                                          method = "fdr"),
          Threshold = dplyr::if_else(.data[["Adjusted_pval"]] < 0.05,
                                     "Significant",
                                     "Non-significant")
        ) |>
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
        p.val <- p.val |> # nolint object_name_linter
          dplyr::filter(
            !(.data[["term"]] %in% .env[["covariate_filter_str"]])
          )
      }

      return(p.val)

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

  return(ord_regr_results)
}

#' @rdname olink_ordinal_regression
#' @export
olink_ordinalRegression <- olink_ordinal_regression  # nolint object_name_linter

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
#' @param check_log A named list returned by [`check_npx()`]. If `NULL`,
#' [`check_npx()`] will be run internally using `df`.
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
#'   check_log <- OlinkAnalyze::check_npx(df = npx_df)
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
#'     effect = "Time",
#'     check_log = check_log
#'   )
#' }
#' }
#'
olink_ordinal_regression_posthoc <- function(df, # nolint object_length_linter
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

  if (missing(df) || missing(variable) || missing(effect)) {
    stop("The df, variable and effect arguments need to be specified.")
  }

  tmp <- strsplit(effect, ":") |> unlist() |> unique()
  if (!all(tmp %in% unique(unlist(strsplit(variable, "[\\*:]"))))) {
    stop("All effect terms must be included in the variable argument.")
  }

  ord_regr_posthoc_result <- withCallingHandlers(
    {
      # Filtering on valid OlinkID
      df <- df |>
        dplyr::filter(
          stringr::str_detect(
            string = .data[["OlinkID"]],
            pattern = "OID[0-9]{5}"
          )
        )

      # if no list of OlinkID was provided, use all assays
      if (is.null(olinkid_list)) {
        olinkid_list <- df |>
          dplyr::pull(
            .data[["OlinkID"]]
          ) |>
          unique()
      }

      # check data format
      check_log <- run_check_npx(df = df, check_log = check_log)
      data_type <- check_log$col_names$quant

      # Allow for :/* notation in covariates
      variable <- gsub(pattern = "\\*", replacement = ":", x = variable)
      if (!is.null(covariates)) {
        covariates <- gsub(pattern = "\\*", replacement = ":", x = covariates)
      }

      add.main.effects <- NULL # nolint object_name_linter

      if (any(grepl(pattern = ":", x = covariates))) {
        tmp <- strsplit(x = covariates, split = ":") |> unlist()
        add.main.effects <- c(add.main.effects, # nolint object_name_linter
                              setdiff(x = tmp, y = covariates))
        covariates <- union(x = covariates, y = add.main.effects)
      }
      if (any(grepl(pattern = ":", x = variable))) {
        tmp <- strsplit(x = variable, split = ":") |> unlist()
        add.main.effects <- c(add.main.effects, # nolint object_name_linter
                              setdiff(x = tmp, y = variable))
        variable <- union(x = variable,
                          y = unlist(strsplit(x = variable, split = ":")))
        variable <- variable[!grepl(pattern = ":", x = variable)]
      }

      # If variable is in both variable and covariate, keep it in variable or
      # will get removed from final table
      covariates <- setdiff(x = covariates, y = variable)
      add.main.effects <- setdiff(x = add.main.effects, y = variable) # nolint object_name_linter

      # Variables to check
      variable_testers <- intersect(x = c(variable, covariates),
                                    y = names(df))

      # Remove rows where variables or covariate is NA (cant include in analysis
      # anyway)
      removed.sampleids <- NULL # nolint object_name_linter
      for (i in variable_testers) {
        removed.sampleids <- c(removed.sampleids, # nolint object_name_linter
                               df$SampleID[is.na(df[[i]])]) |>
          unique()
        df <- df |>
          dplyr::filter(
            !is.na(.data[[i]])
          )
      }

      # Convert outcome to factor
      df <- df |>
        dplyr::mutate(
          !!data_type := factor(x = .data[[data_type]], ordered = TRUE)
        )

      # Convert character vars to factor
      converted.vars <- NULL # nolint object_name_linter
      num.vars <- NULL # nolint object_name_linter
      for (i in variable_testers) {
        if (is.character(df[[i]])) {
          df <- df |>
            dplyr::mutate(
              !!i := factor(x = .data[[i]])
            )
          converted.vars <- c(converted.vars, i) # nolint object_name_linter
        } else if (is.numeric(df[[i]])) {
          num.vars <- c(num.vars, i) # nolint object_name_linter
        }
      }

      if (!is.null(covariates)) {
        formula_string <- paste0(data_type, "~",
                                 paste(variable, collapse = "*"), "+",
                                 paste(covariates, sep = "", collapse = "+"))
      } else {
        formula_string <- paste0(data_type, "~",
                                 paste(variable, collapse = "*"))
      }

      #Print verbose message
      if (verbose) {
        if (!is.null(add.main.effects) && length(add.main.effects) > 0L) {
          message(paste0(
            "Missing main effects added to the model formula: ",
            paste(add.main.effects, collapse = ", ")
          ))
        }
        if (!is.null(removed.sampleids) && length(removed.sampleids) > 0L) {
          message(paste0(
            "Samples removed due to missing variable or covariate levels: ",
            paste(removed.sampleids, collapse = ", ")
          ))
        }
        if (!is.null(converted.vars)) {
          message(paste0(
            "Variables and covariates converted from character to factors: ",
            paste(converted.vars, collapse = ", ")
          ))
        }
        if (!is.null(num.vars)) {
          message(paste0(
            "Variables and covariates treated as numeric: ",
            paste(num.vars, collapse = ", ")
          ))
        }

        if (any(variable %in% num.vars)) {
          message(paste0(
            "Numeric variables post-hoc performed using Mean and Mean + 1SD: ",
            paste(num.vars[num.vars %in% variable], collapse = ", ")
          ))
        }
        message(paste(
          "Estimated marginal means for each assay computed from the",
          "cumulative link model (CLM):", formula_string
        ))
      }

      if (!missing(effect_formula)) {
        e_form <- stats::as.formula(object = effect_formula) # nolint object_usage_linter
      } else {
        e_form <- stats::as.formula(
          object = paste0("pairwise~", paste(effect, collapse = "+")) # nolint object_usage_linter
        )
      }

      ord_regr_posthoc_results <- df |>
        dplyr::filter(
          .data[["OlinkID"]] %in% .env[["olinkid_list"]]
          & !(.data[["OlinkID"]] %in% check_log$assay_na)
        ) |>
        dplyr::mutate(
          OlinkID = factor(x = .data[["OlinkID"]], levels = olinkid_list)
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
          dplyr::all_of(
            c("Assay", "OlinkID", "UniProt", "Panel", "term", "contrast",
              "estimate", "Adjusted_pval", "Threshold")
          )
        )

      return(ord_regr_posthoc_results)

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

  return(ord_regr_posthoc_result)
}

#' @rdname olink_ordinal_regression_posthoc
#' @export
olink_ordinalRegression_posthoc <- olink_ordinal_regression_posthoc  # nolint object_name_linter
