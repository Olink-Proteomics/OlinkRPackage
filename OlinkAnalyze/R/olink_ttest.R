#' Function which performs a t-test per protein
#'
#' @description
#' Performs a Welch 2-sample t-test or paired t-test at confidence level 0.95
#' for every protein (by OlinkID) for a given grouping variable using
#' stats::t.test and corrects for multiple testing by the Benjamini-Hochberg
#' method (“fdr”) using stats::p.adjust. Adjusted p-values are logically
#' evaluated towards adjusted p-value<0.05. The resulting t-test table is
#' arranged by ascending p-values.
#'
#' @param df NPX data frame in long format with at least protein name (Assay),
#' OlinkID, UniProt and a factor with 2 levels.
#' @param variable Character value indicating which column should be used as the
#' grouping variable. Needs to have exactly 2 levels.
#' @param pair_id Character value indicating which column indicates the paired
#' sample identifier.
#' @param check_log A named list returned by [`check_npx()`]. If `NULL`,
#' [`check_npx()`] will be run internally using `df`.
#' @param ... Options to be passed to t.test. See \code{?t.test} for more
#' information.
#'
#' @export
#'
#' @return A "tibble" containing the t-test results for every protein. Columns
#' include:
#' \itemize{
#'  \item{Assay:} "character" Protein symbol
#'  \item{OlinkID:} "character" Olink specific ID
#'  \item{UniProt:} "character" UniProt ID
#'  \item{Panel:} "character" Name of Olink Panel
#'  \item{estimate:} "numeric" difference in mean NPX between groups
#'  \item{Group 1:} "numeric" Column is named first level of variable when
#'  converted to factor, contains mean NPX for that group
#'  \item{Group 2:} "numeric" Column is named second level of variable when
#'  converted to factor, contains mean NPX for that group
#'  \item{statistic:} "named numeric" value of the t-statistic
#'  \item{p.value:} "numeric" p-value for the test
#'  \item{parameter:} "named numeric" degrees of freedom for the t-statistic
#'  \item{conf.low:} "numeric" confidence interval for the mean (lower end)
#'  \item{conf.high:} "numeric" confidence interval for the mean (upper end)
#'  \item{method:} "character" which t-test method was used
#'  \item{alternative:} "character" describes the alternative hypothesis
#'  \item{Adjusted_pval:} "numeric" adjusted p-value for the test (Benjamini &
#'  Hochberg)
#'  \item{Threshold:} "character" if adjusted p-value is significant or not
#'  (< 0.05)
#' }
#'
#' @examples
#' \donttest{
#' if (rlang::is_installed(pkg = c("broom"))) {
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
#'   ttest_results <- OlinkAnalyze::olink_ttest(
#'     df = npx_df,
#'     variable = "Treatment",
#'     alternative = "two.sided",
#'     check_log = check_log
#'   )
#'
#'   # Paired t-test
#'   ttest_paired_results <- npx_df |>
#'     dplyr::filter(
#'       .data[["Time"]] %in% c("Baseline", "Week.6")
#'     ) |>
#'     OlinkAnalyze::olink_ttest(
#'       variable = "Time",
#'       pair_id = "Subject",
#'       check_log = check_log
#'     )
#' }
#'}
#'
olink_ttest <- function(df,
                        variable,
                        pair_id,
                        check_log = NULL,
                        ...) {

  if (missing(df) || missing(variable)) {
    stop("The df and variable arguments need to be specified.")
  }

  dot_lst <- rlang::list2(...) # nolint object_usage_linter

  # Filtering on valid OlinkID
  df <- df |>
    dplyr::filter(
      stringr::str_detect(
        string = .data[["OlinkID"]],
        pattern = "OID[0-9]{5}"
      )
    )

  # Removing SampleID:s with no level for variable
  removed.sampleids <- df |> # nolint object_name_linter
    dplyr::filter(
      is.na(.data[[variable]])
    ) |>
    dplyr::pull(
      .data[["SampleID"]]
    ) |>
    unique()
  if (length(removed.sampleids) > 0L) {
    message("Samples removed due to missing variable levels: ",
            paste(removed.sampleids, collapse = ", "))
  }
  df <- df |>
    dplyr::filter(
      !is.na(.data[[variable]])
    )

  if (!missing(pair_id)) {
    missing.pair <- df |> # nolint object_name_linter
      dplyr::filter(
        is.na(.data[[pair_id]])
      ) |>
      dplyr::pull(
        .data[["SampleID"]]
      ) |>
      unique()
    if (length(missing.pair) > 0L) {
      message("Samples removed due to missing pair ID: ",
              paste(missing.pair, collapse = ", "))
    }
    df <- df |>
      dplyr::filter(
        !is.na(.data[[pair_id]])
      )

    removed.sampleids <- unique(c(removed.sampleids, missing.pair)) # nolint object_name_linter
  }

  # Factor conversion
  if (is.character(dplyr::pull(df, .data[[variable]]))) {
    df <- df |>
      dplyr::mutate(
        !!variable := factor(x = .data[[variable]])
      )
    message(paste0("Variable converted from character to factor: ", variable))
  } else if (!is.factor(dplyr::pull(df, .data[[variable]]))) {
    stop(paste0("The grouping variable ", variable, "is neither factor nor ",
                "character. Only character and factor variable types allowed."))
  }

  var_levels <- df |>
    dplyr::pull(
      .data[[variable]]
    ) |>
    levels()
  number_of_levels <- length(var_levels)

  # Checking number of levels
  if (!(number_of_levels == 2L)) {
    stop(paste0("The number of levels in the factor needs to be 2. Your ",
                "factor has ", number_of_levels, " levels."))
  }

  # Every sample needs to have a unique level of the factor
  n_samples_w_more_than_1_level <- df |>
    dplyr::group_by(
      .data[["SampleID"]]
    ) |>
    dplyr::summarise(
      n_levels = dplyr::n_distinct(.data[[variable]], na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::filter(
      .data[["n_levels"]] > 1L
    ) |>
    nrow()
  if (n_samples_w_more_than_1_level > 0L) {
    stop(paste0("There are ", n_samples_w_more_than_1_level,
                " samples that do not have a unique level for your variable.",
                " Only one level per sample is allowed."))
  }

  # Check data format
  check_log <- run_check_npx(df = df, check_log = check_log)

  nas_in_level <- df |>
    dplyr::filter(
      !(.data[["OlinkID"]] %in% check_log$assay_na)
    ) |>
    dplyr::group_by(
      .data[["OlinkID"]], .data[[variable]]
    ) |>
    dplyr::summarise(
      n = dplyr::n(),
      n_na = sum(is.na(.data[["NPX"]])),
      .groups = "drop"
    ) |>
    dplyr::filter(
      .data[["n"]] == .data[["n_na"]]
    ) |>
    dplyr::pull(
      .data[["OlinkID"]]
    )
  if (length(nas_in_level) > 0L) {
    warning(paste0("The assays ", paste(nas_in_level, collapse = ", "),
                   " have too few datapoints in one level of the factor.",
                   " They will not be tested."),
            call. = FALSE)
  }

  # Filtering out non-tested assays
  df <- df |>
    dplyr::filter(
      !(.data[["OlinkID"]] %in% check_log$assay_na)
      & !(.data[["OlinkID"]] %in% .env[["nas_in_level"]])
    )

  if (nrow(df) == 0L) {
    stop("No assays passing initial check. T-test will not be performed.")
  }

  if (!missing(pair_id)) {

    if (!(pair_id %in% colnames(df))) {
      stop(paste0("Column ", pair_id, " not found."))
    }

    if (!check_is_tibble(x = df, error = FALSE)) {
      message("Converting data frame to tibble.")
      df <- dplyr::as_tibble(x = df)
    }

    # check that each "pair_id' has only 2 samples
    ct_pairs <- df |>
      dplyr::filter(
        !is.na(.data[[variable]])
      ) |>
      dplyr::group_by(
        .data[["OlinkID"]], .data[[pair_id]]
      ) |>
      dplyr::summarize(
        n = dplyr::n(),
        .groups = "drop"
      )
    if (!all(ct_pairs$n <= 2L)) {
      stop(
        paste0(
          "Each pair identifier must identify no more than 2 unique samples.",
          " Check pairs: ",
          paste(unique(ct_pairs[[pair_id]][ct_pairs$n > 2L]), collapse = ", ")
        )
      )
    }

    message(paste0("Paired t-test is performed on ",
                   var_levels[1L], " - ", var_levels[2L], "."))

    p.val <- df |> # nolint object_name_linter
      dplyr::select(
        dplyr::all_of(
          c("OlinkID", "UniProt", "Assay", "Panel", "NPX",
            variable, pair_id)
        )
      ) |>
      tidyr::pivot_wider(
        names_from = dplyr::all_of(variable),
        values_from = dplyr::all_of("NPX")
      ) |>
      dplyr::group_by(
        .data[["Assay"]], .data[["OlinkID"]],
        .data[["UniProt"]], .data[["Panel"]]
      ) |>
      dplyr::group_modify(
        ~ {
          # run paired t-test
          t_res <- rlang::exec(
            .fn = stats::t.test,
            .x[[var_levels[1L]]],
            y = .x[[var_levels[2L]]],
            paired = TRUE,
            !!!dot_lst
          )
          broom::tidy(x = t_res)
        }
      ) |>
      dplyr::ungroup() |>
      dplyr::mutate(
        Adjusted_pval = stats::p.adjust(p = .data[["p.value"]],
                                        method = "fdr"),
        Threshold = dplyr::if_else(.data[["Adjusted_pval"]] < 0.05,
                                   "Significant",
                                   "Non-significant")
      ) |>
      dplyr::arrange(
        .data[["p.value"]]
      )

  } else {

    message(paste0("T-test is performed on ",
                   var_levels[1L], " - ", var_levels[2L], "."))

    p.val <- df |> # nolint object_name_linter
      dplyr::select(
        dplyr::all_of(
          c("OlinkID", "UniProt", "Assay", "Panel", "NPX", variable)
        )
      ) |>
      dplyr::group_by(
        .data[["Assay"]], .data[["OlinkID"]],
        .data[["UniProt"]], .data[["Panel"]]
      ) |>
      dplyr::group_modify(~ {
        fml <- stats::reformulate(termlabels = variable, response = "NPX")
        # run the test with exec + dots, then tidy the result
        t_res <- rlang::exec(
          stats::t.test,
          fml,
          data = .x,
          !!!dot_lst
        )
        broom::tidy(x = t_res)
      }
      ) |>
      dplyr::ungroup() |>
      dplyr::mutate(
        Adjusted_pval = stats::p.adjust(p = .data[["p.value"]],
                                        method = "fdr"),
        Threshold = dplyr::if_else(.data[["Adjusted_pval"]] < 0.05,
                                   "Significant",
                                   "Non-significant")
      ) |>
      dplyr::rename(
        !!var_levels[1L] := "estimate1",
        !!var_levels[2L] := "estimate2"
      ) |>
      dplyr::arrange(
        .data[["p.value"]]
      )
  }

  return(p.val)
}
