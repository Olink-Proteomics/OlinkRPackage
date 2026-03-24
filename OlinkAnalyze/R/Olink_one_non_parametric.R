#' Function which performs a Kruskal-Wallis Test or Friedman Test per protein
#'
#' Performs an Kruskal-Wallis Test for each assay (by OlinkID) in every panel
#' using stats::kruskal.test.
#' Performs an Friedman Test for each assay (by OlinkID) in every panel
#' using rstatix::friedman_test. The function handles factor variable. \cr\cr
#' Samples that have no variable information or missing factor levels are
#' automatically removed from the analysis (specified in a message
#' if verbose = TRUE).
#' Character columns in the input dataframe are automatically converted to
#' factors (specified in a message if verbose = T).
#' Numerical variables are not converted to factors.
#' If a numerical variable is to be used as a factor, this conversion needs to
#' be done on the dataframe before the function call. \cr\cr
#' Inference is specified in a message if verbose = TRUE. \cr
#' The formula notation of the final model is specified in a message if
#' verbose = TRUE. \cr\cr
#' Adjusted p-values are calculated by stats::p.adjust according to the
#' Benjamini & Hochberg (1995) method (“fdr”).
#' The threshold is determined by logic evaluation of Adjusted_pval < 0.05.
#'
#'
#' @param df NPX data frame in long format with at least protein name (Assay),
#' OlinkID, UniProt, Panel and a factor with at least 3 levels.
#' @param check_log A named list returned by [`check_npx()`]. If `NULL`,
#' [`check_npx()`] will be run internally using `df`.
#' @param variable Single character value.
#' @param dependence Boolean. Default: FALSE. When the groups are independent,
#' the kruskal-Wallis will run, when the groups are dependent, the Friedman
#' test will run.
#' @param subject Group information for the repeated measurement. If
#' (dependence = TRUE), this parameter need to be specified.
#' @param verbose Boolean. Default: True. If information about removed samples,
#' factor conversion and final model formula is to be printed to the console.
#' @return A tibble containing the Kruskal-Wallis Test or Friedman Test results
#' for every protein.
#'
#' Columns include:
#' \itemize{
#'  \item{Assay:} "character" Protein symbol
#'  \item{OlinkID:} "character" Olink specific ID
#'  \item{UniProt:} "character" UniProt ID
#'  \item{Panel:} "character" Name of Olink Panel
#'  \item{term:} "character" term in model
#'  \item{df:} "numeric" degrees of freedom
#'  \item{method:} "character" which method was used
#'  \item{statistic:} "named numeric" the value of the test statistic with a
#'  name describing it
#'  \item{p.value:} "numeric" p-value for the test
#'  \item{Adjusted_pval:} "numeric" adjusted p-value for the test
#'  (Benjamini&Hochberg)
#'  \item{Threshold:} "character" if adjusted p-value is significant or
#'  not (< 0.05)
#' }
#'
#' @export
#' @examples \donttest{
#'
#' library(dplyr)
#'
#' # One-way Kruskal-Wallis Test
#' try({ # May fail if dependencies are not installed
#'   check_log <- check_npx(npx_data1)
#'
#'   kruskal_results <- olink_one_non_parametric(
#'     df = npx_data1,
#'     check_log = check_log,
#'     variable = "Site"
#'   )
#' })
#'
#' # Friedman Test
#' friedman_results <- olink_one_non_parametric(
#'   df = npx_data1,
#'   check_log = check_log,
#'   variable = "Time",
#'   subject = "Subject",
#'   dependence = TRUE
#' )
#' }
#'
#' @importFrom dplyr n filter group_by summarise ungroup pull n_distinct do
#' select arrange mutate
#' @importFrom stringr str_detect
#' @importFrom generics tidy
#' @importFrom stats kruskal.test IQR as.formula contr.sum lm median na.omit
#' p.adjust sd t.test var
#' @importFrom rstatix friedman_test convert_as_factor
#' @importFrom utils glob2rx read.table globalVariables

olink_one_non_parametric <- function(df,
                                     check_log = NULL,
                                     variable,
                                     dependence = FALSE,
                                     subject = NULL,
                                     verbose = TRUE) {
  if (missing(df) || missing(variable)) {
    cli::cli_abort("{.arg df} and {.arg variable} must be specified.")
  }

  # Check data format
  check_log <- run_check_npx(df = df, check_log = check_log)

  withCallingHandlers( # nolint return_linter
    {
      # Filtering on valid OlinkID
      df <- suppressMessages(
        clean_invalid_oid(df = df, check_log = check_log)
      )

      # Variables to check
      variable_testers <- intersect(c(variable), names(df))

      ## Remove rows where variables is NA (cant include in analysis anyway)
      removed.sampleids <- NULL # nolint object_name_linter

      for (i in variable_testers) {
        removed.sampleids <- unique( # nolint object_name_linter
          c(removed.sampleids, df[["SampleID"]][is.na(df[[i]])])
        )
        df <- df[!is.na(df[[i]]), ]
      }

      df <- df[!is.na(df[[variable_testers]]), ]

      # Temporary fix to avoid issues with rlang::ensym downstream
      data_type <- check_log$col_names$quant

      # Rename duplicate UniProts
      df <- suppressMessages(
        clean_nonunique_uniprot(df = df, check_log = check_log)
      )

      ## Convert character vars to factor
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

      # Not testing assays that have all NA:s in one level
      # Every sample needs to have a unique level of the factor

      nas_in_var <- character(0L)

      single_fixed_effects <- variable

      for (effect in single_fixed_effects) {
        current_nas <- df |>
          dplyr::filter(!(.data[["OlinkID"]] %in% check_log$assay_na)) |>
          dplyr::group_by(.data[["OlinkID"]], !!rlang::ensym(effect)) |>
          dplyr::summarise(
            n = dplyr::n(),
            n_na = sum(is.na(!!rlang::ensym(data_type))),
            .groups = "drop"
          ) |>
          dplyr::filter(n == .data[["n_na"]]) |>
          dplyr::distinct(.data[["OlinkID"]]) |>
          dplyr::pull(.data[["OlinkID"]])

        if (length(current_nas) > 0L) {
          nas_in_var <- c(nas_in_var, current_nas)
          cli::cli_warn(
            "The assay(s) {.val {current_nas}} have only NA values in at least
            one level of {.var {effect}} and will not be tested."
          )
        }

        number_of_samples_w_more_than_one_level <- df |> # nolint object_length_linter
          dplyr::group_by(.data[["SampleID"]]) |>
          dplyr::summarise(
            n_levels = dplyr::n_distinct(!!rlang::ensym(effect), na.rm = TRUE),
            .groups = "drop"
          ) |>
          dplyr::filter(.data[["n_levels"]] > 1L) |>
          nrow()

        if (number_of_samples_w_more_than_one_level > 0L) {
          cli::cli_abort(
            "There are {number_of_samples_w_more_than_one_level} samples that
            do not have a unique level for the effect {.var {effect}}. Only one
            level per sample is allowed."
          )
        }
      }

      formula_string <- paste0(data_type, "~", paste(variable, collapse = "*"))

      # Get factors
      fact.vars <- sapply(variable_testers, function(x) is.factor(df[[x]])) # nolint object_name_linter
      fact.vars <- names(fact.vars)[fact.vars] # nolint object_name_linter

      # Print verbose message
      if (verbose) {
        if (!is.null(removed.sampleids) & length(removed.sampleids) > 0L) { # nolint object_name_linter
          cli::cli_inform(
            "Samples removed due to missing variable:
            {.val {removed.sampleids}}"
          )
        }
        if (!is.null(converted.vars)) { # nolint object_name_linter
          cli::cli_inform(
            "Variables converted from character to factors:
            {.val {converted.vars}}"
          )
        }
        if (!is.null(num.vars)) { # nolint object_name_linter
          cli::cli_inform(
            "Variables treated as numeric: {.val {num.vars}}"
          )
        }
      }

      if (dependence) {
        if (is.null(subject)) {
          cli::cli_abort("The {.arg subject} variable must be specified.")
        }
        if (verbose) {
          cli::cli_inform(
            "Friedman model fit to each assay: {.code {formula_string}}"
          )
        }

        formula_string <- paste0(formula_string, "|", subject)

        # add repeat measurement groups
        df_nas_remove <- df |>
          dplyr::filter(!(.data[["OlinkID"]] %in% check_log$assay_na)) |>
          dplyr::filter(!(.data[["OlinkID"]] %in% nas_in_var))
        # remove subject without complete data
        ## number of the items in the subject

        n_subject <- df_nas_remove |>
          dplyr::select(!!!rlang::syms(variable)) |>
          stats::na.omit() |>
          unique() |>
          dplyr::pull() |>
          length()

        subject_remove <- df_nas_remove |>
          dplyr::filter(!is.na(.data[["NPX"]])) |>
          dplyr::group_by(.data[["Assay"]], !!!rlang::syms(subject)) |>
          dplyr::summarize(n = n(), .groups = "drop") |>
          dplyr::filter(n == n_subject) |>
          dplyr::mutate(Friedman_remove = "no") |>
          dplyr::select(-n)

        if (length(
          subject_remove |>
            dplyr::select(!!!rlang::syms(subject)) |>
            dplyr::pull() |>
            unique()
        ) < length(
          df_nas_remove |>
            dplyr::select(!!!rlang::syms(subject)) |>
            dplyr::pull() |>
            unique()
        )
        ) {
          cli::cli_inform("Subjects removed due to incomplete data")
        }

        p.val <- df_nas_remove |> # nolint object_name_linter
          dplyr::left_join(subject_remove, by = c("Assay", subject)) |>
          dplyr::filter(.data[["Friedman_remove"]] == "no") |>
          rstatix::convert_as_factor(!!!rlang::syms(variable)) |>
          dplyr::group_by(
            .data[["Assay"]],
            .data[["OlinkID"]],
            .data[["UniProt"]],
            .data[["Panel"]]
          ) |>
          dplyr::group_modify(~ {
            rstatix::friedman_test(
              as.formula(formula_string),
              data = .x,
              na.action = na.omit
            )
          }) |>
          dplyr::ungroup() |>
          dplyr::mutate(
            Adjusted_pval = p.adjust(.data[["p"]], method = "BH")
          ) |>
          dplyr::mutate(Threshold = ifelse(
            .data[["Adjusted_pval"]] < 0.05, "Significant", "Non-significant"
          )) |>
          dplyr::ungroup() |>
          dplyr::arrange(.data[["Adjusted_pval"]]) |>
          dplyr::select(!dplyr::all_of(c("n", ".y."))) |>
          dplyr::mutate(term = variable) |>
          dplyr::rename(p.value = .data[["p"]]) |>
          dplyr::select(dplyr::all_of(c(
            "Assay", "OlinkID", "UniProt", "Panel", "term", "df", "method",
            "statistic", "p.value", "Adjusted_pval", "Threshold"
          )))
      } else {
        { # nolint brace_linter
          if (verbose) {
            cli::cli_inform(
              "Kruskal model fit to each assay: {.code {formula_string}}"
            )
          }
          p.val <- df |> # nolint object_name_linter
            dplyr::filter(!(.data[["OlinkID"]] %in% check_log$assay_na)) |>
            dplyr::filter(!(.data[["OlinkID"]] %in% nas_in_var)) |>
            dplyr::group_by(
              .data[["Assay"]],
              .data[["OlinkID"]],
              .data[["UniProt"]],
              .data[["Panel"]]
            ) |>
            dplyr::group_modify(~ {
              broom::tidy(
                kruskal.test(
                  as.formula(formula_string),
                  data = .x
                )
              )
            }) |>
            dplyr::ungroup() |>
            dplyr::mutate(
              Adjusted_pval = p.adjust(.data[["p.value"]], method = "BH")
            ) |>
            dplyr::mutate(Threshold = ifelse(
              .data[["Adjusted_pval"]] < 0.05, "Significant", "Non-significant"
            )) |>
            dplyr::ungroup() |>
            dplyr::arrange(.data[["Adjusted_pval"]])
        } |> # nolint brace_linter
          dplyr::mutate(term = variable) |>
          dplyr::rename(df = .data[["parameter"]]) |>
          dplyr::select(dplyr::all_of(c(
            "Assay", "OlinkID", "UniProt", "Panel", "term", "df", "method",
            "statistic", "p.value", "Adjusted_pval", "Threshold"
          )))
      }
    },
    warning = function(w) {
      if (grepl(
        x = w,
        pattern = glob2rx("*contains implicit NA, consider using*")
      )) {
        invokeRestart("muffleWarning")
      }
    }
  )
}


#' Function which performs posthoc test per protein for the results from
#' Friedman or Kruskal-Wallis Test.
#'
#' Performs a posthoc test using rstatix::wilcox_test or FSA::dunnTest with
#' Benjamini-Hochberg p-value adjustment per assay (by OlinkID) for each panel
#' at confidence level 0.95.
#' See \code{olink_one_non_parametric} for details of input notation. \cr\cr
#' The function handles both factor and numerical variables.
#'
#' @param df NPX data frame in long format with at least protein name (Assay),
#' OlinkID, UniProt, Panel and a factor with at least 3 levels.
#' @param check_log A named list returned by [`check_npx()`]. If `NULL`,
#' [`check_npx()`] will be run internally using `df`.
#' @param olinkid_list Character vector of OlinkID's on which to perform post
#' hoc analysis. If not specified, all assays in df are used.
#' @param variable Single character value or character array.
#' @param test Single character value indicates running the post hoc test for
#' friedman or kruskal.
#' @param subject Group information for the repeated measurement.
#' If (dependence = TRUE), this parameter need to be specified.
#' @param verbose Boolean. Default: True. If information about removed samples,
#' factor conversion and final model formula is to be printed to the console.
#' @return Tibble of posthoc tests for specified effect, arranged by ascending
#' adjusted p-values.
#'
#' Columns include:
#' \itemize{
#'  \item{Assay:} "character" Protein symbol
#'  \item{OlinkID:} "character" Olink specific ID
#'  \item{UniProt:} "character" UniProt ID
#'  \item{Panel:} "character" Name of Olink Panel
#'  \item{term:} "character" term in model
#'  \item{contrast:} "character" the groups that were compared
#'  \item{estimate:} "numeric" the value of the test statistic with a name
#'  describing it
#'  \item{Adjusted_pval:} "numeric" adjusted p-value for the test
#'  \item{Threshold:} "character" if adjusted p-value is significant or
#'  not (< 0.05)
#' }
#'
#' @export
#' @examples \donttest{
#' library(dplyr)
#'
#' try({ # May fail if dependencies are not installed
#'   # Run check_npx
#'   check_log <- check_npx(npx_data1)
#'
#'   # One-way Kruskal-Wallis Test
#'   kruskal_results <- olink_one_non_parametric(
#'     df = npx_data1,
#'     check_log = check_log,
#'     variable = "Site"
#'   )
#' })
#'
#' # Friedman Test
#' friedman_results <- olink_one_non_parametric(
#'   df = npx_data1,
#'   check_log = check_log,
#'   variable = "Time",
#'   subject = "Subject",
#'   dependence = TRUE
#' )
#'
#' # Posthoc test for the results from Friedman Test
#' friedman_posthoc_results <- olink_one_non_parametric_posthoc(npx_data1,
#'   check_log = check_log,
#'   variable = "Time",
#'   test = "friedman",
#'   olinkid_list = {
#'     friedman_results %>%
#'       filter(Threshold == "Significant") %>%
#'       dplyr::select(OlinkID) %>%
#'       distinct() %>%
#'       pull()
#'   }
#' )
#' }
#'
#' @importFrom dplyr filter group_by ungroup pull do select arrange mutate
#' @importFrom stringr str_detect
#' @importFrom rstatix wilcox_test


olink_one_non_parametric_posthoc <- function(df, # nolint object_length_linter
                                             check_log = NULL,
                                             olinkid_list = NULL,
                                             variable,
                                             test = "kruskal",
                                             subject = "Subject",
                                             verbose = TRUE) {
  if (test != "friedman") {
    if (!requireNamespace("FSA", quietly = TRUE)) {
      cli::cli_abort(c(
        "Kruskal test requires the {.pkg FSA} package.",
        "i" = "Please install it before continuing:",
        " " = "{.code install.packages(\"FSA\")}"
      ))
    }
  }
  if (missing(df) || missing(variable)) {
    cli::cli_abort(
      "{.arg df} and {.arg variable} must be specified."
    )
  }
  if (!(test %in% c("friedman", "kruskal"))) {
    cli::cli_abort(
      "{.arg test} must be either {.val friedman} or {.val kruskal}."
    )
  }

  # Check data format
  check_log <- run_check_npx(df = df, check_log = check_log)

  withCallingHandlers( # nolint return_linter
    {
      # Filtering on valid OlinkID
      df <- suppressMessages(
        clean_invalid_oid(df = df, check_log = check_log)
      )

      if (is.null(olinkid_list)) {
        olinkid_list <- df |>
          dplyr::select(dplyr::all_of(c("OlinkID"))) |>
          dplyr::distinct() |>
          dplyr::pull()
      }

      # Temporary fix to avoid issues with rlang::ensym downstream
      data_type <- check_log$col_names$quant

      # Rename duplicate UniProts
      df <- suppressMessages(
        clean_nonunique_uniprot(df = df, check_log = check_log)
      )

      # Variables to check
      variable_testers <- intersect(c(variable), names(df))
      ## Remove rows where variables or covariate is NA (can't include in
      ## analysis anyway)
      removed.sampleids <- NULL # nolint object_name_linter

      for (i in variable_testers) {
        removed.sampleids <- unique( # nolint object_name_linter
          c(removed.sampleids, df$SampleID[is.na(df[[i]])])
        )
        df <- df[!is.na(df[[i]]), ]
      }

      ## Convert character vars to factor
      converted.vars <- NULL # nolint object_name_linter
      num.vars <- NULL # nolint object_name_linter

      if (is.character(df[[variable_testers]])) {
        df[[variable_testers]] <- factor(df[[variable_testers]])
        converted.vars <- c(converted.vars, variable_testers) # nolint object_name_linter
      } else if (is.numeric(df[[variable_testers]])) {
        num.vars <- c(num.vars, variable_testers) # nolint object_name_linter
      }

      formula_string <- paste0(data_type, "~", paste(variable, collapse = "*")) #nolint

      # Print verbose message
      if (verbose) {
        if (!is.null(removed.sampleids) & length(removed.sampleids) > 0) {
          cli::cli_inform(
            "Samples removed due to missing variable:
            {.val {removed.sampleids}}"
          )
        }
        if (!is.null(converted.vars)) {
          cli::cli_inform(
            "Variables converted from character to factors:
            {.val {converted.vars}}"
          )
        }
        if (!is.null(num.vars)) {
          cli::cli_inform(
            "Variables treated as numeric: {.val {num.vars}}"
          )
        }
      }

      if (test == "friedman") {
        cli::cli_inform(
          "Pairwise comparisons for Friedman test using paired Wilcoxon
          signed-rank test were performed."
        )

        # Not testing assays that have all NA:s in one level
        # Every sample needs to have a unique level of the factor

        nas_in_var <- character(0L)

        single_fixed_effects <- variable

        for (effect in single_fixed_effects) {
          current_nas <- df |>
            dplyr::filter(!(.data[["OlinkID"]] %in% check_log$assay_na)) |>
            dplyr::group_by(.data[["OlinkID"]], !!rlang::ensym(effect)) |>
            dplyr::summarise(
              n = dplyr::n(),
              n_na = sum(is.na(!!rlang::ensym(data_type))),
              .groups = "drop"
            ) |>
            dplyr::filter(n == .data[["n_na"]]) |>
            dplyr::distinct(.data[["OlinkID"]]) |>
            dplyr::pull(.data[["OlinkID"]])

          if (length(current_nas) > 0L) {
            nas_in_var <- c(nas_in_var, current_nas)
            cli::cli_warn(
              "The assay(s) {.val {current_nas}} have only NA values in at
              least one level of {.var {effect}} and will not be tested."
            )
          }

          number_of_samples_w_more_than_one_level <- df |> # nolint object_length_linter
            dplyr::group_by(.data[["SampleID"]]) |>
            dplyr::summarise(
              n_levels = dplyr::n_distinct(
                !!rlang::ensym(effect),
                na.rm = TRUE
              ),
              .groups = "drop"
            ) |>
            dplyr::filter(.data[["n_levels"]] > 1L) |>
            nrow()

          if (number_of_samples_w_more_than_one_level > 0L) {
            cli::cli_abort(
              "There are {number_of_samples_w_more_than_one_level} samples
              that do not have a unique level for the effect {.var {effect}}.
              Only one level per sample is allowed."
            )
          }
        }

        formula_string <- paste0(
          data_type, "~", paste(variable, collapse = "*")
        )

        # Get factors
        fact.vars <- sapply(variable_testers, function(x) is.factor(df[[x]])) # nolint object_name_linter
        fact.vars <- names(fact.vars)[fact.vars] # nolint object_name_linter

        # add repeat measurement groups
        df_nas_remove <- df |>
          dplyr::filter(!(.data[["OlinkID"]] %in% check_log$assay_na)) |>
          dplyr::filter(!(.data[["OlinkID"]] %in% nas_in_var))
        # remove subject without complete data
        ## number of the items in the subject

        n_subject <- df_nas_remove |>
          dplyr::select(!!!rlang::syms(variable)) |>
          stats::na.omit() |>
          unique() |>
          dplyr::pull() |>
          length()

        subject_remove <- df_nas_remove |>
          dplyr::filter(!is.na(.data[["NPX"]])) |>
          dplyr::group_by(.data[["Assay"]], !!!rlang::syms(subject)) |>
          dplyr::summarize(n = n(), .groups = "drop") |>
          dplyr::filter(n == n_subject) |>
          dplyr::mutate(Friedman_remove = "no") |>
          dplyr::select(-n)

        if (length(
          subject_remove |>
            dplyr::select(!!!rlang::syms(subject)) |>
            dplyr::pull() |>
            unique()
        ) < length(
          df_nas_remove |>
            dplyr::select(!!!rlang::syms(subject)) |>
            dplyr::pull() |>
            unique()
        )
        ) {
          cli::cli_inform("Subjects removed due to incomplete data")
        }

        p.hoc_val <- df_nas_remove |> # nolint object_name_linter
          dplyr::left_join(subject_remove, by = c("Assay", subject)) |>
          dplyr::filter(.data[["Friedman_remove"]] == "no") |>
          dplyr::filter(.data[["OlinkID"]] %in% olinkid_list) |>
          dplyr::filter(!(.data[["OlinkID"]] %in% check_log$assay_na)) |>
          dplyr::mutate(
            OlinkID = factor(.data[["OlinkID"]], levels = olinkid_list)
          ) |>
          dplyr::arrange(
            .data[["Assay"]],
            .data[["OlinkID"]],
            .data[["UniProt"]],
            .data[["Panel"]],
            .data[[variable]],
            .data[[subject]]
          ) |>
          dplyr::group_by(
            .data[["Assay"]],
            .data[["OlinkID"]],
            .data[["UniProt"]],
            .data[["Panel"]]
          ) |>
          dplyr::group_modify(~ {
            rstatix::wilcox_test(
              data = .x,
              formula = as.formula(formula_string),
              p.adjust.method = "BH",
              detailed = TRUE,
              conf.level = 0.95,
              paired = TRUE
            )
          }) |>
          dplyr::ungroup() |>
          dplyr::mutate("variable" = variable) |>
          dplyr::mutate(Threshold = dplyr::if_else(
            .data[["p.adj"]] < 0.05, "Significant", "Non-significant"
          )) |>
          dplyr::rename(term = variable) |>
          dplyr::mutate(
            contrast = paste(.data[["group1"]], .data[["group2"]], sep = " - ")
          ) |>
          dplyr::arrange(.data[["p.adj"]]) |>
          dplyr::rename(Adjusted_pval = .data[["p.adj"]]) |>
          dplyr::select(dplyr::all_of(c(
            "Assay", "OlinkID", "UniProt", "Panel", "term", "contrast",
            "estimate", "Adjusted_pval", "Threshold"
          )))
      } else {
        cli::cli_inform(
          "Pairwise comparisons for Kruskal-Wallis test using Dunn test
          were performed"
        )
        p.hoc_val <- df |> # nolint object_name_linter
          dplyr::filter(.data[["OlinkID"]] %in% olinkid_list) |>
          dplyr::filter(!(.data[["OlinkID"]] %in% check_log$assay_na)) |>
          dplyr::mutate(
            OlinkID = factor(.data[["OlinkID"]], levels = olinkid_list)
          ) |>
          dplyr::group_by(
            .data[["Assay"]],
            .data[["OlinkID"]],
            .data[["UniProt"]],
            .data[["Panel"]]
          ) |>
          dplyr::group_modify(~ {
            FSA::dunnTest(
              as.formula(formula_string),
              data = .x,
              method = "bh"
            )$res
          }) |>
          dplyr::ungroup() |>
          dplyr::mutate("variable" = variable) |>
          dplyr::mutate(Threshold = dplyr::if_else(
            .data[["P.adj"]] < 0.05, "Significant", "Non-significant"
          )) |>
          dplyr::rename(term = variable) |>
          dplyr::rename(contrast = .data[["Comparison"]]) |>
          dplyr::arrange(.data[["P.adj"]]) |>
          dplyr::rename(Adjusted_pval = .data[["P.adj"]]) |>
          dplyr::rename(estimate = .data[["Z"]]) |>
          dplyr::select(dplyr::all_of(c(
            "Assay", "OlinkID", "UniProt", "Panel", "term", "contrast",
            "estimate", "Adjusted_pval", "Threshold"
          )))
      }

      return(p.hoc_val) # nolint object_name_linter
    },
    warning = function(w) {
      if (grepl(
        x = w,
        pattern = glob2rx("*contains implicit NA, consider using*")
      )
      ) {
        invokeRestart("muffleWarning")
      }
    }
  )
}
