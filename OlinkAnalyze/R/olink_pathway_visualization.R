#' Creates bargraph of top/selected enrichment terms from GSEA or ORA results
#' from olink_pathway_enrichment()
#'
#' Pathways are ordered by increasing p-value (unadjusted)
#'
#'@param enrich_results data frame of enrichment results from
#'olink_pathway_enrichment()
#'@param method method used in olink_pathway_enrichment ("GSEA" (default)
#'or "ORA")
#'@param keyword (optional) keyword to filter enrichment results on, if not
#'specified, displays top terms
#'@param number_of_terms number of terms to display, default is 20
#'@return A bargraph as a ggplot object
#'@examples
#'\donttest{
#'library(dplyr)
#'# Run olink_ttest or other stats test (see documentation )
#'npx_df <- npx_data1 |> filter(!grepl('control',SampleID, ignore.case = TRUE))
#'check_log <- check_npx(npx_df)
#'ttest_results <- olink_ttest(df = npx_df,
#'                             check_log = check_log,
#'                             variable = 'Treatment',
#'                              alternative = 'two.sided')
#'
#' try({ # This expression might fail if dependencies are not installed
#'# Run olink_pathway_enrichment (see documentation)
#' gsea_results <- olink_pathway_enrichment(data = npx_df,
#'                                          check_log = check_log,
#'                                          test_results = ttest_results)
#' ora_results <- olink_pathway_enrichment(data = npx_df,
#'                                         check_log = check_log,
#'                                         test_results = ttest_results,
#'                                         method = "ORA")
#'olink_pathway_visualization(enrich_results = gsea_results)
#'olink_pathway_visualization(enrich_results = gsea_results,
#'                            keyword = "immune")
#'olink_pathway_visualization(enrich_results = ora_results,
#'                            method = "ORA",
#'                            number_of_terms = 15)
#'})
#'
#'}
#'
#'@export

olink_pathway_visualization <- function(enrich_results,
                                        method = "GSEA",
                                        keyword = NULL,
                                        number_of_terms = 20) {

  if (!(method %in% c("ORA", "GSEA"))) {
    cli::cli_abort("Method must be \"GSEA\" or \"ORA\".")
  }

  if (!is.null(keyword)) {
    enrich_results <- enrich_results |>
      dplyr::filter(grepl(pattern = toupper(keyword),
                          x = .data[["Description"]]))

    if (nrow(enrich_results) == 0) {
      cli::cli_abort(paste0("Keyword not found. ",
                            "Please choose a different keyword or ",
                            "use a set number of terms."))
    }
  }

  enrich_results <- enrich_results |>
    dplyr::arrange(.data[["pvalue"]]) |>
    dplyr::slice_head(n = number_of_terms) |>
    dplyr::arrange(dplyr::desc(x = .data[["pvalue"]])) |>
    dplyr::mutate(Description = stringr::str_trunc(string =
                                                     .data[["Description"]],
                                                   width = 50,
                                                   side = "center")) |>
    dplyr::mutate(Description = factor(.data[["Description"]],
                                       levels = unique(.data[["Description"]]))
    )

  if (method == "ORA") {
    p <- ggplot2::ggplot(data = enrich_results,
                         ggplot2::aes(x = .data[["Description"]],
                                      y = .data[["Count"]]))
  } else if (method == "GSEA") {
    p <- ggplot2::ggplot(data = enrich_results,
                         ggplot2::aes(x = .data[["Description"]],
                                      y = .data[["NES"]]))
  }

  p <- p +
    ggplot2::geom_bar(stat = "identity",
                      ggplot2::aes(fill = .data[["p.adjust"]])) +
    OlinkAnalyze::olink_fill_gradient(coloroption = c("teal", "red")) +
    ggplot2::coord_flip() +
    ggplot2::xlab("Description")

  if (method == "ORA") {
    p <- p +
      ggplot2::geom_text(ggplot2::aes(label = paste(gsub(x =
                                                           .data[["GeneRatio"]],
                                                         pattern = "/.*",
                                                         replacement = ""),
                                                    gsub(x = .data[["BgRatio"]],
                                                         pattern = "/.*",
                                                         replacement = ""),
                                                    sep = "/")),
                         hjust = -0.1, color = "black", size = 3.5)
  }

  return(p)
}
