#' Creates bargraph of top/selected enrichment terms from GSEA or ORA results from olink_pathway_enrichment()
#' 
#' Pathways are ordered by increasing p-value (unadjusted)
#'
#'@param enrich_results data frame of enrichment results from olink_pathway_enrichment()
#'@param method method used in olink_pathway_enrichment ("GSEA" (default) or "ORA")
#'@param keyword (optional) keyword to filter enrichment results on, if not specified, displays top terms
#'@param number_of_terms number of terms to display, default is 20
#'@return A bargraph as a ggplot object
#'@examples
#'\donttest{
#'library(dplyr)
#'npx_df <- npx_data1 %>% filter(!grepl('control',SampleID, ignore.case = TRUE))
#'ttest_results <- olink_ttest(df=npx_df,
#'                              variable = 'Treatment',
#'                              alternative = 'two.sided')
#'
#' try({ # This expression might fail if dependencies are not installed
#'gsea_results <- olink_pathway_enrichment(data = npx_data1, test_results = ttest_results)
#'ora_results <- olink_pathway_enrichment(data = npx_data1,
#'                                       test_results = ttest_results, method = "ORA")
#'olink_pathway_visualization(enrich_results = gsea_results)
#'olink_pathway_visualization(enrich_results = gsea_results, keyword = "immune")
#'olink_pathway_visualization(enrich_results = ora_results, method = "ORA", number_of_terms = 15)
#'})
#'
#'}
#'
#' @seealso \itemize{
#' \item{\code{\link[OlinkAnalyze:olink_pathway_enrichment]{olink_pathway_enrichment}} for generating enrichment results}
#' \item{\code{\link[OlinkAnalyze:olink_pathway_heatmap]{olink_pathway_heatmap}} for generating a heat map of results}
#' }
#'
#'@importFrom dplyr filter
#'@importFrom ggplot2 ggplot geom_bar geom_text aes coord_flip xlab
#'@importFrom stringr str_trunc
#'@importFrom forcats fct_inorder
#'@importFrom magrittr %>%
#'@export
olink_pathway_visualization<- function(enrich_results, method = "GSEA", keyword = NULL, number_of_terms = 20){
  if (method == "ORA"){
    if(is.null(keyword)){
      enrich_results %>% 
        dplyr::arrange(dplyr::desc(pvalue)) %>%
        head(number_of_terms) %>%
        dplyr::mutate(Description = stringr::str_trunc(Description, 50, "center")) %>% 
        dplyr::mutate(Description = forcats::fct_inorder(Description)) %>% 
        ggplot2::ggplot(., ggplot2::aes(x = Description, y=Count)) +
        ggplot2::geom_bar(stat = "identity", ggplot2::aes(fill = p.adjust))+
        OlinkAnalyze::olink_fill_gradient(coloroption = c('teal', 'red')) +
        ggplot2::coord_flip() +
        ggplot2::xlab("Description")+
        ggplot2::geom_text(ggplot2::aes(label=paste(gsub(x = GeneRatio,pattern = "/.*", replacement = ""),
                                  gsub(x = BgRatio,pattern = "/.*", replacement = ""), sep = "/")),
                  hjust=-0.1, color="black", size=3.5)
    }else{
      if (nrow(enrich_results %>%
               dplyr::filter(grepl(pattern = toupper(keyword), Description))) == 0) {
        stop("Keyword not found. Please choose a different keyword or use a set number of terms.")
      }
      enrich_results %>%
        dplyr::filter(grepl(pattern = toupper(keyword), Description)) %>% 
        dplyr::arrange(dplyr::desc(pvalue)) %>%
        head(number_of_terms) %>%
        dplyr::mutate(Description = stringr::str_trunc(Description, 50, "center")) %>% 
        dplyr::mutate(Description = forcats::fct_inorder(Description)) %>% 
        ggplot2::ggplot(., ggplot2::aes(x = Description, y=Count))+
        ggplot2::geom_bar(stat = "identity", ggplot2::aes(fill = p.adjust))+
        OlinkAnalyze::olink_fill_gradient(coloroption = c('teal', 'red')) +
        ggplot2::coord_flip() +
        ggplot2::xlab("Description")+
        ggplot2::geom_text(ggplot2::aes(label=paste(gsub(x = GeneRatio,pattern = "/.*", replacement = ""),
                                  gsub(x = BgRatio,pattern = "/.*", replacement = ""), sep = "/")),
                  hjust=-0.1, color="black", size=3.5)
    }
  } else if (method == "GSEA") {
    if(is.null(keyword)){
     enrich_results %>%
        head(number_of_terms) %>% 
        dplyr::arrange(dplyr::desc(pvalue)) %>%
        dplyr::mutate(Description = stringr::str_trunc(Description, 50, "center")) %>% 
        dplyr::mutate(Description = forcats::fct_inorder(Description)) %>% 
        ggplot2::ggplot(., ggplot2::aes(x = Description, y=NES)) +
        ggplot2::geom_bar(stat = "identity", ggplot2::aes(fill = p.adjust))+
        OlinkAnalyze::olink_fill_gradient(coloroption = c('teal', 'red')) +
        ggplot2::coord_flip() +
        ggplot2::xlab("Description")
    }else{
      if (nrow(enrich_results %>%
               dplyr::filter(grepl(pattern = toupper(keyword), Description))) == 0) {
        stop("Keyword not found. Please choose a different keyword or use a set number of terms.")
      }
      enrich_results %>%
        dplyr::filter(grepl(pattern = toupper(keyword), Description)) %>% 
        dplyr::arrange(dplyr::desc(pvalue)) %>%
        head(number_of_terms) %>%
        dplyr::mutate(Description = stringr::str_trunc(Description, 50, "center")) %>% 
        dplyr::mutate(Description = forcats::fct_inorder(Description)) %>% 
        ggplot2::ggplot(., ggplot2::aes(x = Description, y=NES)) +
        ggplot2::geom_bar(stat = "identity", ggplot2::aes(fill = p.adjust))+
        OlinkAnalyze::olink_fill_gradient(coloroption = c('teal', 'red')) +
        ggplot2::coord_flip() +
        ggplot2::xlab("Description")
    }
  } else {
    stop("Method must be \"GSEA\" or \"ORA\".")
  }
}
