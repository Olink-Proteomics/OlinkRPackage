#' Performs pathway enrichment using over-representation analysis (ORA) or gene set enrichment analysis (GSEA) based on statistical test results and full data using clusterProfiler's gsea and enrich functions for various databases.
#'
#' @param data NPX data frame in long format with at least protein name (Assay), OlinkID, UniProt,SampleID, QC_Warning, NPX, and LOD
#' @param test_results a dataframe of statistical test results including Adjusted_pval and estimate columns. Currently only works for tests with estimates for all assays.
#' @param method Either "GSEA" (default) or "ORA"
#' @param ontology Supports "MSigDb" (default), "KEGG", "GO", and "Reactome" as arguements. MSigDb contains C2 and C5 genesets. C2 and C5 encompass KEGG, GO, and Reactome.
#' @param organism Either "human" (default) or "mouse"
#' @return A data frame of enriched pathway/ontological terms including pvalue, p.adjust, and qvalues, and list of Entrez IDs in that pathway and the data
#' @examples
#' \donttest{
#' library(dplyr)
#' npx_df <- npx_data1 %>% filter(!grepl("control", SampleID, ignore.case = TRUE))
#' ttest_results <- olink_ttest(
#'   df = npx_df,
#'   variable = "Treatment",
#'   alternative = "two.sided"
#' )
#' gsea_results <- olink_pathway_enrichment(data = npx_data1, test_results = ttest_results)
#' ora_results <- olink_pathway_enrichment(
#'   data = npx_data1,
#'   test_results = ttest_results, method = "ORA"
#' )
#' }
#' @seealso \itemize{
#' \item{\code{\link[OlinkAnalyze:olink_pathway_heatmap]{olink_pathway_heatmap}} for generating a heat map of results}
#' \item{\code{\link[OlinkAnalyze:olink_pathway_visualization]{olink_pathway_visualization}} for generating a bar graph of results}
#' }
#'
#' @importFrom dplyr filter mutate select group_by summarise arrange ungroup distinct pull full_join
#' @importFrom clusterProfiler  GSEA enricher
#' @importFrom msigdbr msigdbr
#' @importFrom stringr str_length
#' @importFrom magrittr %>%
#' @export

olink_pathway_enrichment <- function(data, test_results, method = "GSEA", ontology = "MSigDb", organism = "human") {
  data2 <- data_prep(data = data)
  test_results2 <- test_prep(data = data2, test_results = test_results, organism = organism)
  
  if (method == "ORA") {
    results <- ora_pathwayenrichment(
      test_results = test_results2, ontology = ontology,
      organism = organism)
    message("Over-representation Analysis performed")
  } else {
    geneList <- results_to_genelist(test_results = test_results2)
    results <- gsea_pathwayenrichment(geneList = geneList, ontology = ontology, organism = organism)
    message("Gene set enrichment analysis used by default.")
  }
  return(results)
}


data_prep <- function(data) {
  # Filter highest detectibility for repeated IDs
  olink_ids <- data %>%
    dplyr::filter(!str_detect(SampleID, "CONTROL*.")) %>%
    dplyr::filter(QC_Warning == "Pass") %>%
    dplyr::mutate(Detected = as.numeric(NPX > LOD)) %>%
    dplyr::select(OlinkID, UniProt, Assay, Panel, Detected) %>%
    dplyr::group_by(OlinkID, Assay) %>%
    dplyr::summarise(Sum = (sum(Detected) / n())) %>%
    dplyr::arrange(desc(Sum)) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(Assay, .keep_all = TRUE) %>%
    dplyr::pull(OlinkID)
  data <- data %>% dplyr::filter(OlinkID %in% olink_ids)
  message("Data filtered for highest detectibility in duplicate assay names.")
  return(data)
}
test_prep <- function(data, test_results, organism = "human") {
  test_results <- test_results %>%
    dplyr::filter(OlinkID %in% unique(data$OlinkID))
  message("Test results filtered for highest detectibility in duplicate assay names.")
  return(test_results)
}
results_to_genelist <- function(test_results) {
  estimate <- test_results$estimate
  # names(estimate) <- test_results$ENTREZID
  names(estimate)<- test_results$Assay
  geneList <- sort(estimate, decreasing = TRUE)
  message("Test results converted to gene list")
  return(geneList)
}
gsea_pathwayenrichment <- function(geneList, ontology, organism) {
  if (organism == "human") {
    msig_df <- msigdbr::msigdbr(species = "Homo sapiens", category = "C2") %>%
      rbind(msigdbr::msigdbr(species = "Homo sapiens", category = "C5"))
  } else if (organism == "mouse") {
    msig_df <- msigdbr::msigdbr(species = "Mus musculus", category = "C2") %>%
      rbind(msigdbr::msigdbr(species = "Mus musculus", category = "C5"))
    
  } else {
    stop(print("organism should be \"human\" or \"mouse\""))
  }
  if (ontology == "Reactome"){
    message("Extracting Reactome Database from MSigDB...")
    msig_df<-  msig_df %>% dplyr::filter(gs_subcat == "CP:REACTOME")
   } else if (ontology == "KEGG"){
     message("Extracting KEGG Database from MSigDB...")
     msig_df<-  msig_df %>% dplyr::filter(gs_subcat == "CP:KEGG")
  } else if (ontology == "GO"){
    message("Extracting GO Database from MSigDB...")
    msig_df<-  msig_df %>% dplyr::filter(gs_subcat %in% c("GO:BP", "GO:CC", "GO:MF"))
  } else{
    message("Using MSigDB...")
  }
  msig_df <- msig_df  %>% dplyr::select(gs_name, gene_symbol)
  if(length(setdiff(names(geneList), msig_df$gene_symbol) != 0)){
     message(paste0(length(setdiff(names(geneList), msig_df$gene_symbol)),
                        " assays are not found in the database. Please check the Assay names for the following assays:\n ", 
                        toString(setdiff(names(geneList), msig_df$gene_symbol))))
  }
  GSEA <- clusterProfiler::GSEA(geneList = geneList, TERM2GENE = msig_df, pvalueCutoff = 1)
  return(GSEA@result)
}
ora_pathwayenrichment <- function(test_results, organism, ontology = ontology, pvalue_cutoff = 0.05, estimate_cutoff = 0) {
  sig_genes <- test_results %>%
    filter(Adjusted_pval < pvalue_cutoff) %>%
    filter(abs(estimate) > estimate_cutoff) %>% 
    distinct(Assay) %>% 
    pull(Assay)
  universe <- test_results %>%
    distinct(Assay) %>%
    pull(Assay)
  if (organism == "human") {
    msig_df <- msigdbr::msigdbr(species = "Homo sapiens", category = "C2") %>%
      rbind(msigdbr::msigdbr(species = "Homo sapiens", category = "C5"))
  } else if (organism == "mouse") {
    msig_df <- msigdbr::msigdbr(species = "Mus musculus", category = "C2") %>%
      rbind(msigdbr::msigdbr(species = "Mus musculus", category = "C5"))
    
  } else {
    stop(print("organism should be \"human\" or \"mouse\""))
  }
  if (ontology == "Reactome"){
    message("Extracting Reactome Database from MSigDB...")
    msig_df<-  msig_df %>% dplyr::filter(gs_subcat == "CP:REACTOME")
  } else if (ontology == "KEGG"){
    message("Extracting KEGG Database from MSigDB...")
    msig_df<-  msig_df %>% dplyr::filter(gs_subcat == "CP:KEGG")
  } else if (ontology == "GO"){
    message("Extracting GO Database from MSigDB...")
    msig_df<-  msig_df %>% dplyr::filter(gs_subcat %in% c("GO:BP", "GO:CC", "GO:MF"))
  } else{
    message("Using MSigDB...")
  }
  msig_df <- msig_df  %>% dplyr::select(gs_name, gene_symbol)
  if(length(setdiff(names(universe), msig_df$gene_symbol) != 0)){
    message(paste0(length(setdiff(names(geneList), msig_df$gene_symbol)),
                   " assays are not found in the database. Please check the Assay names for the following assays:\n ", 
                   toString(setdiff(names(universe), msig_df$gene_symbol))))
  }
  ORA <- enricher(gene = sig_genes, universe = universe, TERM2GENE = msig_df, pvalueCutoff = 1)
  return(ORA@result)
}
