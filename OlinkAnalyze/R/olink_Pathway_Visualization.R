#' Creates bargraph of top/selected enrichment terms from GSEA or ORA results from olink_Pathway_Enrichment()
#'
#'@param enrich_results data frame of enrichment results from olink_Pathway_Enrichment()
#'@param method method used in olink_Pathway_Enrichment ("GSEA" (default) or "ORA")
#'@param keyword (optional) keyword to filter enrichment results on, if not specified, displays top terms
#'@param number_of_terms number of terms to display, default is 20
#'@return bargraph as a ggplot object
#'@examples
#'\donttest{
#'library(dplyr)
#'npx_df <- npx_data1 %>% filter(!grepl('control',SampleID, ignore.case = TRUE))
#'ttest_results <- olink_ttest(df=npx_df,
#'                              variable = 'Treatment',
#'                              alternative = 'two.sided')
#'gsea_results <- olink_Pathway_Enrichment(data = npx_data1, test_results = ttest_results)
#'ora_results <- olink_Pathway_Enrichment(data = npx_data1,
#'                                       test_results = ttest_results, method = "ORA")
#'olink_Pathway_Visualization(enrich_results = gsea_results)
#'olink_Pathway_Visualization(enrich_results = gsea_results, keyword = "immune")
#'olink_Pathway_Visualization(enrich_results = ora_results, method = "ORA", number_of_terms = 15)
#'
#'}
#'
#' @seealso \itemize{
#' \item{\code{\link[OlinkAnalyze:olink_Pathway_Enrichment]{olink_Pathway_Enrichment}} for generating enrichment results}
#' \item{\code{\link[OlinkAnalyze:olink_Pathway_Heatmap]{olink_Pathway_Heatmap}} for generating a heat map of results}
#' }
#'
#'@importFrom dplyr filter
#'@importFrom ggplot2 ggplot geom_bar geom_text aes coord_flip xlab
#'@importFrom stringr str_trunc
#'@importFrom magrittr %>%
#'@export
olink_Pathway_Visualization<- function(enrich_results, method = "GSEA", keyword = NULL, number_of_terms = 20){
  if (method == "ORA"){
    if(is.null(keyword)){
      enrich_results %>%  head(number_of_terms) %>%
        ggplot(., aes(x = str_trunc(Description, 50, "center"), y=Count)) +
        geom_bar(stat = "identity", aes(fill = p.adjust))+
        olink_fill_gradient(coloroption = c('teal', 'red')) + coord_flip() + xlab("Description")+
        geom_text(aes(label=paste(gsub(x = GeneRatio,pattern = "/.*", replacement = ""),
                                  gsub(x = BgRatio,pattern = "/.*", replacement = ""), sep = "/")),
                  hjust=-0.1, color="black", size=3.5)
    }else{
      enrich_results %>%  filter(grepl(pattern = toupper(keyword), Description)) %>%  head(number_of_terms) %>%
        ggplot(., aes(x = str_trunc(Description, 50, "center"), y=Count)) +
        geom_bar(stat = "identity", aes(fill = p.adjust))+
        olink_fill_gradient(coloroption = c('teal', 'red')) + coord_flip() + xlab("Description")+
        geom_text(aes(label=paste(gsub(x = GeneRatio,pattern = "/.*", replacement = ""),
                                  gsub(x = BgRatio,pattern = "/.*", replacement = ""), sep = "/")),
                  hjust=-0.1, color="black", size=3.5)
    }
  }else{
    if(is.null(keyword)){
     enrich_results %>%  head(number_of_terms) %>%
        ggplot(., aes(x = str_trunc(Description, 50, "center"), y=NES)) +
        geom_bar(stat = "identity", aes(fill = p.adjust))+
        olink_fill_gradient(coloroption = c('teal', 'red')) + coord_flip() + xlab("Description")
    }else{
     enrich_results %>%  filter(grepl(pattern = toupper(keyword), Description)) %>%  head(number_of_terms) %>%
        ggplot(., aes(x = str_trunc(Description, 50, "center"), y=NES)) +
        geom_bar(stat = "identity", aes(fill = p.adjust))+
        olink_fill_gradient(coloroption = c('teal', 'red')) + coord_flip() + xlab("Description")
    }
  }
}
