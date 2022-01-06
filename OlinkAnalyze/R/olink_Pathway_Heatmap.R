#' Creates a heatmap of selected pathways and proteins
#'
#' Creates a heatmap of proteins related to pathways using enrichment results from olink_pathway_enrichment. 
#'
#'@param enrich_results data frame of enrichment results from olink_pathway_enrichment()
#'@param test_results filtered results from statistical test with Assay, OlinkID, and estimate columns
#'@param method method used in olink_pathway_enrichment ("GSEA" (default) or "ORA")
#'@param keyword (optional) keyword to filter enrichment results on, if not specified, displays top terms
#'@param number_of_terms number of terms to display, default is 20
#'@return heatmap as a ggplot object
#'@examples
#'\donttest{
#'library(dplyr)
#'npx_df <- npx_data1 %>% filter(!grepl('control',SampleID, ignore.case = TRUE))
#'ttest_results <- olink_ttest(df=npx_df,
#'                              variable = 'Treatment',
#'                              alternative = 'two.sided')
#'gsea_results <- olink_pathway_enrichment(data = npx_data1, test_results = ttest_results)
#'ora_results <- olink_pathway_enrichment(data = npx_data1,
#'                                       test_results = ttest_results, method = "ORA")
#'ttest_sub <- ttest_results %>% filter(Panel == "CARDIOMETABOLIC")
#'olink_pathway_heatmap(enrich_results = gsea_results, test_results = ttest_sub)
#'olink_pathway_heatmap(enrich_results = ora_results, test_results = ttest_sub,
#'                      method = "ORA", keyword = "heart")
#'}
#'
#' @seealso \itemize{
#' \item{\code{\link[OlinkAnalyze:olink_pathway_enrichment]{olink_pathway_enrichment}} for generating enrichment results}
#' \item{\code{\link[OlinkAnalyze:olink_pathway_visualization]{olink_pathway_visualization}} for generating a bar graph of results}
#' }
#'@importFrom dplyr filter left_join distinct inner_join arrange select distinct pull
#'@importFrom ggplot2 ggplot geom_tile geom_text aes xlab ylab theme
#'@importFrom stringr str_trunc
#'@importFrom clusterProfiler bitr
#'@importFrom magrittr %>%
#'@export

olink_pathway_heatmap<- function(enrich_results, test_results, method = "GSEA", keyword = NULL, number_of_terms = 20){
  if(is.null(keyword)){
    sub_enrich <- enrich_results %>% head(number_of_terms)
  } else{
    sub_enrich <- enrich_results %>%
      filter(grepl(pattern = toupper(keyword), Description)) %>%
      head(number_of_terms)
  }
  if (method == "ORA"){
    results_list<-strsplit(sub_enrich$geneID, "/")
  }else{
    results_list<- strsplit(sub_enrich$core_enrichment, "/")
  }
  names(results_list) <- sub_enrich$Description
  long_list<-do.call(rbind, lapply(results_list, data.frame, stringsAsFactors=FALSE))
  long_list$Pathway <- row.names(long_list)
  long_list$Pathway <- gsub("\\..*",replacement = "",x= long_list$Pathway, )
  names(long_list)[1] <- "ENTREZID"
  uniprot <- bitr(long_list$ENTREZID, fromType = "ENTREZID", toType = "SYMBOL", OrgDb = "org.Hs.eg.db")

  long_list1<- long_list %>%
    left_join(uniprot, by = "ENTREZID" ) %>%
    distinct() %>%
    inner_join(test_results, by = c("SYMBOL" = "Assay")) %>%
    arrange(estimate)

  orderprot <- long_list1 %>% dplyr::select(SYMBOL) %>% distinct() %>% pull()


  p<-ggplot(long_list1, aes(factor(SYMBOL, levels = orderprot), str_trunc(Pathway, 50, "center"))) +
    geom_tile(aes(fill = estimate)) + olink_fill_gradient(coloroption = c('teal', 'red'), name = "estimate") + set_plot_theme() +
    theme(panel.grid.major = element_blank(),
          axis.text.x=element_text(angle = 60, hjust = 1)) +
    xlab("Protein Symbol") + ylab("Pathway")
  return(p)
}
