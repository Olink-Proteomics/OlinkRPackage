#' Function which performs a Mann-Whitney U Test per protein
#'
#' Performs a Welch 2-sample Mann-Whitney U Test at confidence level 0.95 for
#' every protein (by OlinkID) for a given grouping variable using
#' stats::wilcox.test and corrects for multiple testing by the
#' Benjamini-Hochberg method (“fdr”) using stats::p.adjust. Adjusted p-values
#' are logically evaluated towards adjusted p-value<0.05. The resulting
#' Mann-Whitney U Test table is arranged by ascending p-values.
#'
#' @param df NPX or Quantified_value data frame in long format with at least
#' protein name (Assay), OlinkID, UniProt and a factor with 2 levels.
#' @param variable Character value indicating which column should be used as the
#' grouping variable. Needs to have exactly 2 levels.
#' @param pair_id Character value indicating which column indicates the paired
#' sample identifier.
#' @param check_log Optional named list returned by [`check_npx()`]. If `NULL`,
#' an attached `check_log` is used when present; otherwise [`check_npx()`] will
#' be run internally using `df`.
#' @param ... Options to be passed to wilcox.test. See \code{?wilcox_test} for
#' more information.
#'
#' @export
#'
#' @return A data frame containing the Mann-Whitney U Test results for every
#' protein. Columns include:
#' \itemize{
#'  \item{Assay:} "character" Protein symbol
#'  \item{OlinkID:} "character" Olink specific ID
#'  \item{UniProt:} "character" UniProt ID
#'  \item{Panel:} "character" Name of Olink Panel
#'  \item{estimate:} "numeric" median of NPX differences between groups
#'  \item{statistic:} "named numeric" the value of the test statistic with a
#'  name describing it
#'  \item{p.value:} "numeric" p-value for the test
#'  \item{conf.low:} "numeric" confidence interval for the median of differences
#'  (lower end)
#'  \item{conf.high:} "numeric" confidence interval for the median of
#'  differences (upper end)
#'  \item{method:} "character" which wilcoxon method was used
#'  \item{alternative:} "character" describes the alternative hypothesis
#'  \item{Adjusted_pval:} "numeric" adjusted p-value for the test
#'  (Benjamini&Hochberg)
#'  \item{Threshold:} "character" if adjusted p-value is significant or not
#'  (< 0.05)
#' }
#'
#' @examples
#' \donttest{
#' if (rlang::is_installed(pkg = c("broom"))) {
#'   npx_df <- npx_data1 |>
#'     dplyr::filter(
#'     !grepl(
#'       pattern = "control",
#'       x = .data[["SampleID"]],
#'       ignore.case = TRUE
#'     )
#'   )
#'
#'   # Mann-Whitney U Test
#'   wilcox_results <- OlinkAnalyze::olink_wilcox(
#'     df = npx_df,
#'     variable = "Treatment",
#'     alternative = "two.sided"
#'   )
#'
#'   # Paired Mann-Whitney U Test
#'   wilcox_paired_results <- npx_df |>
#'     dplyr::filter(
#'       .data[["Time"]] %in% c("Baseline", "Week.6")
#'     ) |>
#'     OlinkAnalyze::olink_wilcox(
#'       variable = "Time",
#'       pair_id = "Subject"
#'     )
#' }
#'}
#'
olink_wilcox <- function(df,
                         variable,
                         pair_id,
                         check_log = NULL,
                         ...) {

  # Check if all required libraries for this function are installed
  rlang::check_installed(
    pkg = c("broom"),
    call = rlang::caller_env()
  )

  check_stats_required(
    present = c(!missing(df), !missing(variable)),
    message = "The df and variable arguments need to be specified."
  )

  dot_lst <- rlang::list2(...) # nolint: object_usage_linter
  pair_id_missing <- missing(pair_id)
  checked <- prepare_two_group_stats(
    df = df,
    variable = variable,
    pair_id = if (pair_id_missing) NULL else pair_id,
    check_log = check_log,
    test_name = "Mann-Whitney U Test"
  )
  df <- checked$df
  var_levels <- checked$variable_levels # nolint: object_usage_linter
  check_log <- checked$check_log

  if (!pair_id_missing) {
    cli::cli_inform(
      paste0(
        "Paired Mann-Whitney U Test is performed on ",
        "{var_levels[1L]} - {var_levels[2L]}."
      )
    )

    p.val <- df |> # nolint: object_name_linter
      dplyr::arrange(
        .data[[pair_id]], .data[["SampleID"]], .data[["OlinkID"]]
      ) |>
      dplyr::select(
        dplyr::all_of(
          c("OlinkID", "UniProt", "Assay", "Panel", "NPX",
            variable, pair_id)
        )
      ) |>
      tidyr::pivot_wider(
        id_cols = dplyr::all_of(
          c("OlinkID", "UniProt", "Assay", "Panel",
            pair_id)
        ),
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
          wilcox_res <- rlang::exec(
            .fn = stats::wilcox.test,
            .x[[var_levels[1L]]],
            y = .x[[var_levels[2L]]],
            paired = TRUE,
            conf.int = TRUE,
            !!!dot_lst
          )
          broom::tidy(x = wilcox_res)
        }
      ) |>
      dplyr::ungroup() |>
      add_stats_significance() |>
      dplyr::arrange(
        .data[["p.value"]]
      )

  } else {

    cli::cli_inform(
      "Mann-Whitney U Test is performed on {var_levels[1L]} - {var_levels[2L]}."
    )

    p.val <- df |> # nolint: object_name_linter
      dplyr::arrange(
        .data[["SampleID"]], .data[["OlinkID"]]
      ) |>
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
        wilcox_res <- rlang::exec(
          stats::wilcox.test,
          fml,
          data = .x,
          conf.int = TRUE,
          !!!dot_lst
        )
        broom::tidy(x = wilcox_res)
      }
      ) |>
      dplyr::ungroup() |>
      add_stats_significance() |>
      dplyr::arrange(
        .data[["p.value"]]
      )

  }

  return(p.val)

}
