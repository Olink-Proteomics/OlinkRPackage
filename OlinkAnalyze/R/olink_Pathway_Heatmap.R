#' Creates a heatmap of selected pathways and proteins
#'
#' Creates a heatmap of proteins related to pathways using enrichment results from olink_pathway_enrichment.
#'
#'@param enrich_results data frame of enrichment results from olink_pathway_enrichment()
#'@param test_results filtered results from statistical test with Assay, OlinkID, and estimate columns
#'@param method method used in olink_pathway_enrichment ("GSEA" (default) or "ORA")
#'@param keyword (optional) keyword to filter enrichment results on, if not specified, displays top terms
#'@param number_of_terms number of terms to display, default is 20
#'@return A heatmap as a ggplot object
#'@examples
#'\donttest{
#'library(dplyr)
#' npx_df <- npx_data1 %>% filter(!grepl('control',SampleID, ignore.case = TRUE))
#' ttest_results <- olink_ttest(df=npx_df,
#'                              variable = 'Treatment',
#'                              alternative = 'two.sided')
#'
#' try({ # This expression might fail if dependencies are not installed
#' gsea_results <- olink_pathway_enrichment(data = npx_data1, test_results = ttest_results)
#' ora_results <- olink_pathway_enrichment(data = npx_data1,
#'                                         test_results = ttest_results, method = "ORA")
#' olink_pathway_heatmap(enrich_results = gsea_results, test_results = ttest_results)
#' olink_pathway_heatmap(enrich_results = ora_results, test_results = ttest_results,
#'                       method = "ORA", keyword = "cell")
#' })
#'
#'
#'}
#'
#' @seealso \itemize{
#' \item{\code{\link[OlinkAnalyze:olink_pathway_enrichment]{olink_pathway_enrichment}} for generating enrichment results}
#' \item{\code{\link[OlinkAnalyze:olink_pathway_visualization]{olink_pathway_visualization}} for generating a bar graph of results}
#' }
#'@importFrom dplyr filter distinct inner_join arrange select distinct pull
#'@importFrom ggplot2 ggplot geom_tile geom_text aes xlab ylab theme
#'@importFrom stringr str_trunc
#'@importFrom magrittr %>%
#'@export

olink_pathway_heatmap<- function(enrich_results, test_results, method = "GSEA", keyword = NULL, number_of_terms = 20){
  if(is.null(keyword)){
    sub_enrich <- enrich_results %>% dplyr::arrange(dplyr::desc(estimate)) %>% head(number_of_terms)
  } else{
    sub_enrich <- enrich_results %>%
      dplyr::filter(grepl(pattern = toupper(keyword), Description)) %>%
      head(number_of_terms)
    if (nrow(sub_enrich) == 0) {
      stop("Keyword not found. Please choose a different keyword or use a set number of terms.")
    }
  }
  if (method == "ORA"){
    results_list<-strsplit(sub_enrich$geneID, "/")
  } else if (method == "GSEA") {
    results_list<- strsplit(sub_enrich$core_enrichment, "/")
  } else {
    stop("Method must be \"GSEA\" or \"ORA\".")
  }
    results_list<-strsplit(sub_enrich$geneID, "/")
  }else{
    results_list<- strsplit(sub_enrich$core_enrichment, "/")
  }
  names(results_list) <- sub_enrich$Description
  long_list<-do.call(rbind, lapply(results_list, data.frame, stringsAsFactors=FALSE))
  long_list$Pathway <- row.names(long_list)
  long_list$Pathway <- gsub("\\..*",replacement = "",x= long_list$Pathway)
  names(long_list)[1] <- "Assay"
  long_list <- as.data.frame(long_list)

  long_list1<- long_list %>%
    dplyr::inner_join(test_results, by = "Assay") %>%
    dplyr::arrange(estimate)

  orderprot <- unique(long_list1$Assay)

  p<-ggplot2::ggplot(long_list1, ggplot2::aes(factor(Assay, levels = orderprot), stringr::str_trunc(Pathway, 50, "center"))) +
    ggplot2::geom_tile(aes(fill = estimate)) +
    OlinkAnalyze::olink_fill_gradient(coloroption = c('teal', 'red'), name = "estimate") +
    OlinkAnalyze::set_plot_theme() +
    ggplot2::theme(panel.grid.major = element_blank(),
          axis.text.x=ggplot2::element_text(angle = 60, hjust = 1)) +
    ggplot2::xlab("Protein Symbol") + ggplot2::ylab("Pathway")

  return(p)
}
