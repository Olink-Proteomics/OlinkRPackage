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
#' @param check_log Optional named list returned by [`check_npx()`]. If `NULL`,
#' an attached `check_log` is used when present; otherwise [`check_npx()`] will
#' be run internally using `df`.
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
#' @examples
#' \donttest{
#' if (rlang::is_installed(pkg = c("broom", "rstatix"))) {
#'
#'  # One-way Kruskal-Wallis Test
#'  kruskal_results <- OlinkAnalyze::olink_one_non_parametric(
#'     df = OlinkAnalyze::npx_data1,
#'     variable = "Site"
#'   )
#'
#'   # Friedman Test
#'   friedman_results <- OlinkAnalyze::olink_one_non_parametric(
#'     df = OlinkAnalyze::npx_data1,
#'     variable = "Time",
#'     subject = "Subject",
#'     dependence = TRUE
#'   )
#' }
#' }
#'
olink_one_non_parametric <- function(df,
                                     check_log = NULL,
                                     variable,
                                     dependence = FALSE,
                                     subject = NULL,
                                     verbose = TRUE) {
  # Check if all required libraries for this function are installed
  rlang::check_installed(
    pkg = c("broom", "rstatix"),
    call = rlang::caller_env()
  )

  check_stats_required(
    present = c(!missing(df), !missing(variable)),
    message = "{.arg df} and {.arg variable} must be specified."
  )

  # Check data format
  check_log <- get_check_npx(df = df, check_log = check_log)

  withCallingHandlers( # nolint: return_linter
    {
      df <- filter_stats_olink_ids(
        df = df,
        olink_id = check_log$col_names$olink_id,
        invalid_ids = check_log$oid_invalid
      )

      # Variables to check
      variable_testers <- intersect(c(variable), names(df))

      filtered_data <- filter_stats_missing(
        df = df,
        columns = variable_testers,
        sample_id = "SampleID"
      )
      df <- filtered_data$df
      removed.sampleids <- filtered_data$removed_sampleids # nolint: object_name_linter

      # Temporary fix to avoid issues with rlang::ensym downstream
      data_type <- check_log$col_names$quant

      # Rename duplicate UniProts
      df <- suppressMessages(
        clean_nonunique_uniprot(df = df, check_log = check_log)
      )

      converted_data <- convert_stats_predictors(df, variable_testers)
      df <- converted_data$df
      converted.vars <- converted_data$converted_vars # nolint: object_name_linter
      num.vars <- converted_data$numeric_vars # nolint: object_name_linter
      fact.vars <- converted_data$factor_vars # nolint: object_name_linter

      # Not testing assays that have all NA:s in one level
      # Every sample needs to have a unique level of the factor

      single_fixed_effects <- variable
      nas_in_var <- check_stats_effect_data(
        df = df,
        effects = single_fixed_effects,
        outcome = data_type,
        check_log = check_log
      )

      formula_string <- build_stats_formula(
        outcome = data_type,
        variable = variable
      )

      # Print verbose message
      if (verbose) {
        if (!is.null(removed.sampleids) & length(removed.sampleids) > 0L) { # nolint: object_name_linter
          cli::cli_inform(
            "Samples removed due to missing variable:
            {.val {removed.sampleids}}"
          )
        }
        if (!is.null(converted.vars)) { # nolint: object_name_linter
          cli::cli_inform(
            "Variables converted from character to factors:
            {.val {converted.vars}}"
          )
        }
        if (!is.null(num.vars)) { # nolint: object_name_linter
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
          dplyr::select(dplyr::all_of(variable)) |>
          stats::na.omit() |>
          unique() |>
          dplyr::pull() |>
          length()

        subject_remove <- df_nas_remove |>
          dplyr::filter(!is.na(.data[["NPX"]])) |>
          dplyr::group_by(.data[["Assay"]], .data[[subject]]) |>
          dplyr::summarize(n = dplyr::n(), .groups = "drop") |>
          dplyr::filter(.data[["n"]] == .env[["n_subject"]]) |>
          dplyr::mutate(Friedman_remove = "no") |>
          dplyr::select(-dplyr::all_of(c("n")))

        if (length(
          subject_remove |>
            dplyr::select(dplyr::all_of(subject)) |>
            dplyr::pull() |>
            unique()
        ) < length(
          df_nas_remove |>
            dplyr::select(dplyr::all_of(subject)) |>
            dplyr::pull() |>
            unique()
        )
        ) {
          cli::cli_inform("Subjects removed due to incomplete data")
        }

        p.val <- df_nas_remove |> # nolint: object_name_linter
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
              stats::as.formula(formula_string),
              data = .x,
              na.action = na.omit
            )
          }) |>
          dplyr::ungroup() |>
          add_stats_significance(p_column = "p", method = "BH") |>
          dplyr::ungroup() |>
          dplyr::arrange(.data[["Adjusted_pval"]]) |>
          dplyr::select(-dplyr::all_of(c("n", ".y."))) |>
          dplyr::mutate(term = .env[["variable"]]) |>
          dplyr::rename("p.value" = "p") |>
          dplyr::select(dplyr::all_of(c(
            "Assay", "OlinkID", "UniProt", "Panel", "term", "df", "method",
            "statistic", "p.value", "Adjusted_pval", "Threshold"
          )))
      } else {
        { # nolint: brace_linter
          if (verbose) {
            cli::cli_inform(
              "Kruskal model fit to each assay: {.code {formula_string}}"
            )
          }
          p.val <- df |> # nolint: object_name_linter
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
                  stats::as.formula(formula_string),
                  data = .x
                )
              )
            }) |>
            dplyr::ungroup() |>
            add_stats_significance(method = "BH") |>
            dplyr::ungroup() |>
            dplyr::arrange(.data[["Adjusted_pval"]])
        } |> # nolint: brace_linter
          dplyr::mutate(term = .env[["variable"]]) |>
          dplyr::rename("df" = "parameter") |>
          dplyr::select(dplyr::all_of(c(
            "Assay", "OlinkID", "UniProt", "Panel", "term", "df", "method",
            "statistic", "p.value", "Adjusted_pval", "Threshold"
          )))
      }
    },
    warning = function(w) {
      return(muffle_stats_warning(
        warning = w,
        patterns = utils::glob2rx("*contains implicit NA, consider using*")
      ))
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
#' @param check_log Optional named list returned by [`check_npx()`]. If `NULL`,
#' an attached `check_log` is used when present; otherwise [`check_npx()`] will
#' be run internally using `df`.
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
#'
#' @examples
#' \donttest{
#' if (rlang::is_installed(pkg = c("FSA", "broom", "rstatix"))) {
#'
#'   # One-way Kruskal-Wallis Test
#'   kruskal_results <- OlinkAnalyze::olink_one_non_parametric(
#'     df = OlinkAnalyze::npx_data1,
#'     variable = "Site"
#'   )
#'
#'   # Friedman Test
#'   friedman_results <- OlinkAnalyze::olink_one_non_parametric(
#'     df = OlinkAnalyze::npx_data1,
#'     variable = "Time",
#'     subject = "Subject",
#'     dependence = TRUE
#'   )
#'
#'   # Posthoc test for the results from Friedman Test
#'   friedman_posthoc_results <- OlinkAnalyze::olink_one_non_parametric_posthoc(
#'     df = OlinkAnalyze::npx_data1,
#'     variable = "Time",
#'     test = "friedman",
#'     olinkid_list = friedman_results |>
#'       dplyr::filter(.data[["Threshold"]] == "Significant") |>
#'       dplyr::select(
#'         dplyr::all_of("OlinkID")
#'       ) |>
#'       dplyr::distinct() |>
#'       dplyr::pull()
#'   )
#' }
#' }
#'
olink_one_non_parametric_posthoc <- function(df, # nolint: object_length_linter
                                             check_log = NULL,
                                             olinkid_list = NULL,
                                             variable,
                                             test = "kruskal",
                                             subject = "Subject",
                                             verbose = TRUE) {
  # Check if all required libraries for this function are installed
  rlang::check_installed(
    pkg = c("FSA", "rstatix"),
    call = rlang::caller_env()
  )

  check_stats_required(
    present = c(!missing(df), !missing(variable)),
    message = "{.arg df} and {.arg variable} must be specified."
  )
  if (!(test %in% c("friedman", "kruskal"))) {
    cli::cli_abort(
      "{.arg test} must be either {.val friedman} or {.val kruskal}."
    )
  }

  if (test == "kruskal") {
    warn_if_buggy_dunn_test()
  }

  # Check data format
  check_log <- get_check_npx(df = df, check_log = check_log)

  withCallingHandlers( # nolint: return_linter
    {
      df <- filter_stats_olink_ids(
        df = df,
        olink_id = check_log$col_names$olink_id,
        invalid_ids = check_log$oid_invalid
      )
      olinkid_list <- resolve_stats_olink_ids(df, olinkid_list)

      # Temporary fix to avoid issues with rlang::ensym downstream
      data_type <- check_log$col_names$quant

      # Rename duplicate UniProts
      df <- suppressMessages(
        clean_nonunique_uniprot(df = df, check_log = check_log)
      )

      # Variables to check
      variable_testers <- intersect(c(variable), names(df))
      filtered_data <- filter_stats_missing(
        df = df,
        columns = variable_testers,
        sample_id = "SampleID"
      )
      df <- filtered_data$df
      removed.sampleids <- filtered_data$removed_sampleids # nolint: object_name_linter

      converted_data <- convert_stats_predictors(df, variable_testers)
      df <- converted_data$df
      converted.vars <- converted_data$converted_vars # nolint: object_name_linter
      num.vars <- converted_data$numeric_vars # nolint: object_name_linter
      fact.vars <- converted_data$factor_vars # nolint: object_name_linter

      formula_string <- build_stats_formula( # nolint: object_usage_linter
        outcome = data_type,
        variable = variable
      )

      # Print verbose message
      if (verbose) {
        if (!is.null(removed.sampleids) && length(removed.sampleids) > 0) {
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

        single_fixed_effects <- variable
        nas_in_var <- check_stats_effect_data(
          df = df,
          effects = single_fixed_effects,
          outcome = data_type,
          check_log = check_log
        )

        # add repeat measurement groups
        df_nas_remove <- df |>
          dplyr::filter(!(.data[["OlinkID"]] %in% check_log$assay_na)) |>
          dplyr::filter(!(.data[["OlinkID"]] %in% nas_in_var))
        # remove subject without complete data
        ## number of the items in the subject

        n_subject <- df_nas_remove |>
          dplyr::select(dplyr::all_of(variable)) |>
          stats::na.omit() |>
          unique() |>
          dplyr::pull() |>
          length()

        subject_remove <- df_nas_remove |>
          dplyr::filter(!is.na(.data[["NPX"]])) |>
          dplyr::group_by(.data[["Assay"]], .data[[subject]]) |>
          dplyr::summarize(n = dplyr::n(), .groups = "drop") |>
          dplyr::filter(.data[["n"]] == .env[["n_subject"]]) |>
          dplyr::mutate(Friedman_remove = "no") |>
          dplyr::select(-dplyr::all_of("n"))

        if (length(
          subject_remove |>
            dplyr::select(dplyr::all_of(subject)) |>
            dplyr::pull() |>
            unique()
        ) < length(
          df_nas_remove |>
            dplyr::select(dplyr::all_of(subject)) |>
            dplyr::pull() |>
            unique()
        )
        ) {
          cli::cli_inform("Subjects removed due to incomplete data")
        }

        p.hoc_val <- df_nas_remove |> # nolint: object_name_linter
          dplyr::left_join(subject_remove, by = c("Assay", subject)) |>
          dplyr::filter(.data[["Friedman_remove"]] == "no") |>
          filter_stats_assays(
            olinkid_list = olinkid_list,
            assay_na = check_log$assay_na,
            preserve_order = TRUE
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
              formula = stats::as.formula(formula_string),
              p.adjust.method = "BH",
              detailed = TRUE,
              conf.level = 0.95,
              paired = TRUE
            )
          }) |>
          dplyr::ungroup() |>
          dplyr::mutate("variable" = variable) |>
          add_stats_threshold(p_column = "p.adj") |>
          dplyr::rename("term" = "variable") |>
          dplyr::mutate(
            contrast = paste(.data[["group1"]], .data[["group2"]], sep = " - ")
          ) |>
          dplyr::arrange(.data[["p.adj"]]) |>
          dplyr::rename("Adjusted_pval" = "p.adj") |>
          dplyr::select(dplyr::all_of(c(
            "Assay", "OlinkID", "UniProt", "Panel", "term", "contrast",
            "estimate", "Adjusted_pval", "Threshold"
          )))
      } else {
        cli::cli_inform(
          "Pairwise comparisons for Kruskal-Wallis test using Dunn test
          were performed"
        )
        p.hoc_val <- df |> # nolint: object_name_linter
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
            quiet_dunn(
              formula = stats::as.formula(formula_string),
              data = .x,
              method = "bh"
            )
          }) |>
          dplyr::ungroup() |>
          dplyr::mutate(variable = .env[["variable"]]) |>
          add_stats_threshold(p_column = "P.adj") |>
          dplyr::rename("term" = "variable") |>
          dplyr::rename("contrast" = "Comparison") |>
          dplyr::arrange(.data[["P.adj"]]) |>
          dplyr::rename("Adjusted_pval" = "P.adj") |>
          dplyr::rename("estimate" = "Z") |>
          dplyr::select(dplyr::all_of(c(
            "Assay", "OlinkID", "UniProt", "Panel", "term", "contrast",
            "estimate", "Adjusted_pval", "Threshold"
          )))
      }

      return(p.hoc_val) # nolint: object_name_linter
    },
    warning = function(w) {
      return(muffle_stats_warning(
        warning = w,
        patterns = utils::glob2rx("*contains implicit NA, consider using*")
      ))
    }
  )
}

quiet_dunn <- function(formula, data, method = "bh") {
  null_file <- if (.Platform$OS.type == "windows") "NUL" else "/dev/null"
  zz <- file(null_file, open = "wt")

  on.exit({
    try(sink(type = "message"), silent = TRUE)
    try(sink(), silent = TRUE)
    try(close(zz), silent = TRUE)
  }, add = FALSE)

  sink(zz)
  sink(zz, type = "message")

  out <- suppressMessages(
    FSA::dunnTest(x = formula, data = data, method = method)
  )

  return(out$res)
}

has_buggy_dunn_test <- function(is_installed = rlang::is_installed,
                                package_version = utils::packageVersion) {
  x <- is_installed("dunn.test") &&
    package_version("dunn.test") == "1.4.0"
  return(x)
}

warn_if_buggy_dunn_test <- function(has_buggy_version = has_buggy_dunn_test) {
  if (has_buggy_version()) {
    cli::cli_warn(
      c(
        "The installed version of {.pkg dunn.test} contains a known bug that
        provides incorrect results!",
        "i" = "Downgrade to v1.3.7 or upgrade to v1.4.1 using
        {.run remotes::install_version(\"dunn.test\", version = \"1.3.7\")} or
        {.run install.packages(\"dunn.test\")}, respectively."
      )
    )
  }

  return(invisible(NULL))
}
