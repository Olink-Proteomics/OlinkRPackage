#' Creates bargraph of top/selected enrichment terms from GSEA or ORA results
#' from `olink_pathway_enrichment`
#'
#' @description
#' Pathways are ordered by increasing p-value (unadjusted)
#'
#' @param enrich_results data frame of enrichment results from
#' `olink_pathway_enrichment`
#' @param method method used in `olink_pathway_enrichment` ("GSEA" (default)
#' or "ORA")
#' @param keyword (optional) keyword to filter enrichment results on. If not
#' specified, displays top terms.
#' @param number_of_terms number of terms to display (default is 20).
#'
#' @return A bargraph as a ggplot object.
#'
#' @export
#'
#' @examples
#' \donttest{
#' if (rlang::is_installed(pkg = c("msigdbr", "clusterProfiler"))) {
#'
#'   # Run olink_ttest or other stats test (see documentation )
#'   npx_df <- npx_data1 |>
#'     dplyr::filter(
#'       !grepl(
#'         pattern = "control",
#'         x = .data[["SampleID"]],
#'         ignore.case = TRUE
#'       )
#'     )
#'
#'   check_log <- check_npx(df = npx_df)
#'
#'   ttest_results <- OlinkAnalyze::olink_ttest(
#'     df = npx_df,
#'     variable = "Treatment",
#'     alternative = "two.sided",
#'     check_log = check_log
#'   )
#'
#'   # Run olink_pathway_enrichment (see documentation)
#'
#'   # GSEA
#'   gsea_results <- OlinkAnalyze::olink_pathway_enrichment(
#'     df = npx_df,
#'     test_results = ttest_results,
#'     check_log = check_log
#'   )
#'
#'   # ORA
#'   ora_results <- OlinkAnalyze::olink_pathway_enrichment(
#'     df = npx_df,
#'     test_results = ttest_results,
#'     check_log = check_log,
#'     method = "ORA"
#'   )
#'
#'   # Plot
#'
#'   OlinkAnalyze::olink_pathway_visualization(
#'     enrich_results = gsea_results
#'   )
#'
#'   OlinkAnalyze::olink_pathway_visualization(
#'     enrich_results = gsea_results,
#'     keyword = "immune"
#'   )
#'
#'   OlinkAnalyze::olink_pathway_visualization(
#'     enrich_results = ora_results,
#'     method = "ORA",
#'     number_of_terms = 15L
#'   )
#' }
#'}
#'
olink_pathway_visualization <- function(enrich_results,
                                        method = "GSEA",
                                        keyword = NULL,
                                        number_of_terms = 20L) {

  # check inputs ----

  ## check enrichment method ----

  expected_methods <- c("GSEA", "ORA")

  if (!(method %in% expected_methods)) {
    cli::cli_abort(
      c(
        "x" = "{.val {method}} is not a valid method for pathway enrichment!",
        "i" = "Expected one of {.val {expected_methods}}."
      ),
      call = rlang::caller_env(),
      wrap = TRUE
    )
  }

  ## check keywords ----

  if (!is.null(keyword)) {
    enrich_results <- enrich_results |>
      dplyr::filter(
        grepl(pattern = .env[["keyword"]],
              x = .data[["Description"]],
              ignore.case = TRUE),
      )

    if (nrow(enrich_results) == 0L) {
      cli::cli_abort(
        c(
          "x" = "Filtering {.arg enrich_results} for {.arg enrich_results} =
          {.val {keyword}} did not return any results.",
          "i" = "Ensure that the keyword is spelled correctly and is present in
          the enrichment results."
        ),
        call = rlang::caller_env(),
        wrap = TRUE
      )
    }
  }

  ## check number of terms ----

  check_is_scalar_numeric(x = number_of_terms, error = TRUE)

  # plot ----

  enrich_results <- enrich_results |>
    dplyr::arrange(
      .data[["pvalue"]]
    ) |>
    dplyr::slice_head(
      n = number_of_terms
    ) |>
    dplyr::arrange(
      dplyr::desc(x = .data[["pvalue"]])
    ) |>
    dplyr::mutate(
      Description = stringr::str_trunc(
        string = .data[["Description"]],
        width = 50L,
        side = "center"
      ),
      Description = factor(
        x = .data[["Description"]],
        levels = unique(.data[["Description"]])
      )
    )

  if (method == "ORA") {
    p <- ggplot2::ggplot(
      data = enrich_results,
      mapping = ggplot2::aes(
        x = .data[["Description"]],
        y = .data[["Count"]]
      )
    )
  } else if (method == "GSEA") {
    p <- ggplot2::ggplot(
      data = enrich_results,
      mapping = ggplot2::aes(
        x = .data[["Description"]],
        y = .data[["NES"]]
      )
    )
  }

  p <- p +
    ggplot2::geom_bar(
      stat = "identity",
      mapping = ggplot2::aes(
        fill = .data[["p.adjust"]]
      )
    ) +
    OlinkAnalyze::olink_fill_gradient(
      coloroption = c("teal", "red")
    ) +
    ggplot2::coord_flip() +
    ggplot2::xlab(
      label = "Description"
    )

  if (method == "ORA") {
    p <- p +
      ggplot2::geom_text(
        mapping = ggplot2::aes(
          label = paste(gsub(x = .data[["GeneRatio"]],
                             pattern = "/.*",
                             replacement = ""),
                        gsub(x = .data[["BgRatio"]],
                             pattern = "/.*",
                             replacement = ""),
                        sep = "/")),
        hjust = -0.1,
        color = "black",
        size = 3.5
      )
  }

  return(p)
}
