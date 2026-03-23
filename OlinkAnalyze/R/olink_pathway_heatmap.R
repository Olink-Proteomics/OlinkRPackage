#' Creates a heatmap of proteins related to pathways using enrichment results
#' from `olink_pathway_enrichment`.
#'
#' @inherit olink_pathway_visualization params
#' @inherit olink_pathway_enrichment params
#'
#' @return A heatmap as a ggplot object.
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
#'   OlinkAnalyze::olink_pathway_heatmap(
#'     enrich_results = gsea_results,
#'     test_results = ttest_results
#'   )
#'
#'   OlinkAnalyze::olink_pathway_heatmap(
#'     enrich_results = ora_results,
#'     test_results = ttest_results,
#'     method = "ORA",
#'     keyword = "cell"
#'   )
#' }
#' }
#'
olink_pathway_heatmap <- function(enrich_results,
                                  test_results,
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

  ## check test results and enrich results ----

  if (missing(test_results) || missing(enrich_results)) {
    cli::cli_abort("Arguments {.arg enrich_results} and {.arg test_results}
                   are required!")
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

  # prepare data ----

  sub_enrich <- enrich_results |>
    dplyr::arrange(
      .data[["pvalue"]]
    ) |>
    dplyr::slice_head(
      n = number_of_terms
    ) |>
    dplyr::arrange(
      dplyr::desc(x = .data[["pvalue"]])
    )

  if (method == "ORA") {
    results_list <- strsplit(x = sub_enrich[["geneID"]], split = "/")
  } else if (method == "GSEA") {
    results_list <- strsplit(x = sub_enrich[["core_enrichment"]], split = "/")
  }
  names(results_list) <- sub_enrich[["Description"]]

  long_list <- do.call(rbind, lapply(results_list,
                                     data.frame,
                                     stringsAsFactors = FALSE))
  long_list[["Pathway"]] <- row.names(long_list)
  long_list[["Pathway"]] <- gsub(pattern = "\\..*",
                                 replacement = "",
                                 x = long_list[["Pathway"]])
  names(long_list)[1L] <- "Assay"

  long_list1 <- long_list |>
    as.data.frame() |>
    dplyr::as_tibble() |>
    dplyr::inner_join(
      test_results,
      by = "Assay",
      relationship = "many-to-many"
    ) |>
    dplyr::arrange(
      .data[["estimate"]]
    ) |>
    dplyr::mutate(
      Assay = factor(x = .data[["Assay"]],
                     levels = unique(.data[["Assay"]])),
      Pathway = stringr::str_trunc(string = .data[["Pathway"]],
                                   width = 50L,
                                   side = "center")
    )

  # plot ----

  p <- ggplot2::ggplot(
    data = long_list1,
    mapping = ggplot2::aes(
      x = .data[["Assay"]],
      y = .data[["Pathway"]]
    )
  ) +
    ggplot2::geom_tile(
      mapping = ggplot2::aes(
        fill = .data[["estimate"]]
      )
    ) +
    OlinkAnalyze::olink_fill_gradient(
      coloroption = c("teal", "red"),
      name = "estimate"
    ) +
    OlinkAnalyze::set_plot_theme() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(
        angle = 60L,
        hjust = 1
      )
    ) +
    ggplot2::labs(
      x = "Protein Symbol",
      y = "Pathway"
    )

  return(p)
}
