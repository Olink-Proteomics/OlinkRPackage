#' Performs pathway enrichment using over-representation analysis (ORA) or gene set enrichment analysis (GSEA) 
#' 
#' This enrichment analysis based on statistical test results and full data using clusterProfiler's gsea and enrich functions for MSigDB. 
#' 
#' MSigDB is subset if ontology is KEGG, GO, or Reactome. test_results must contain estimates for all assays. 
#' Posthoc results can be used but should be filtered for one contrast to improve interpretability.
#' clusterProfiler is originally developed by Guangchuang Yu at the School of Basic Medical Sciences at Southern Medical University.
#' 
#' T Wu, E Hu, S Xu, M Chen, P Guo, Z Dai, T Feng, L Zhou, W Tang, L Zhan, X Fu, S Liu, X Bo, and G Yu. 
#' clusterProfiler 4.0: A universal enrichment tool for interpreting omics data. The Innovation. 2021, 2(3):100141. 
#' doi: 10.1016/j.xinn.2021.100141
#' 
#' @param data NPX data frame in long format with at least protein name (Assay), OlinkID, UniProt,SampleID, QC_Warning, NPX, and LOD
#' @param test_results a dataframe of statistical test results including Adjusted_pval and estimate columns. 
#' @param method Either "GSEA" (default) or "ORA"
#' @param ontology Supports "MSigDb" (default), "KEGG", "GO", and "Reactome" as arguments. MSigDb contains C2 and C5 genesets. C2 and C5 encompass KEGG, GO, and Reactome.
#' @param organism Either "human" (default) or "mouse"
#' @return A data frame of enrichment results.
#' Columns for ORA include:
#' \itemize{
#'  \item{ID:} "character" Pathway ID from MSigDB
#'  \item{Description:} "character" Description of Pathway from MSigDB  
#'  \item{GeneRatio:} "character" ratio of input proteins that are annotated in a term
#'  \item{BgRatio:} "character" ratio of all genes that are annotated in this term
#'  \item{pvalue:} "numeric" p-value of enrichment
#'  \item{p.adjust:} "numeric" Adjusted p-value (Benjamini-Hochberg)
#'  \item{qvalue:} "numeric" false discovery rate, the estimated probability that the normalized enrichment score represents
#'  a false positive finding
#'  \item{geneID:} "character" list of input proteins (Gene Symbols) annotated in a term delimited by "/"
#'  \item{Count:} "integer" Number of input proteins that are annotated in a term
#' }
#' 
#' Columns for GSEA:
#' \itemize{
#'  \item{ID:} "character" Pathway ID from MSigDB
#'  \item{Description:} "character" Description of Pathway from MSigDB  
#'  \item{setSize:} "integer" ratio of input proteins that are annotated in a term
#'  \item{enrichmentScore:} "numeric" Enrichment score, degree to which a gene set is over-represented at the top or
#'  bottom of the ranked list of genes
#'  \item{NES:} "numeric" Normalized Enrichment Score, normalized to account for differences in gene set size and in
#'  correlations between gene sets and expression data sets. NES can be used to compare analysis results
#'  across gene sets. 
#'  \item{pvalue:} "numeric" p-value of enrichment
#'  \item{p.adjust:} "numeric" Adjusted p-value (Benjamini-Hochberg)
#'  \item{qvalue:} "numeric" false discovery rate, the estimated probability that the normalized enrichment score represents
#'  a false positive finding
#'  \item{rank:} "numeric" the position in the ranked list where the maximum enrichment score occurred
#'  \item{leading_edge:} "character" contains tags, list, and signal. Tags gives an indication of the percentage of genes
#'  contributing to the enrichment score. List gives an indication of where in the list the enrichment
#'  score is obtained. Signal represents the enrichment signal strength and combines the tag and list.
#'  \item{core_enrichment:} "character" list of input proteins (Gene Symbols) annotated in a term delimited by "/"
#' }
#' 
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
  # Data Checks
  
  if(length(unique(data$OlinkID)) != length(unique(test_results$OlinkID))) {
    warning("The number of Olink IDs in the data does not equal the number of Olink IDs in the test results.")
  }
  
  if("contrast" %in% colnames(test_results) && length(unique(test_results$contrast)) > 1){
    stop("More than one contrast is specified in test results. Filter test_results for desired contrast.")
  }
  if(!("estimate" %in% colnames(test_results))){
    stop("Estimate column is not present in test results. Please check arguments.")
  }
  
  if(!(method %in% c("GSEA", "ORA"))){
    stop("Method must be \"GSEA\" or \"ORA\".")
  }
  
  if(!(ontology %in% c("MSigDb", "Reactome", "KEGG", "GO"))){
    stop("Ontology must be one of MSigDb, Reactome, KEGG, or GO.")
  }
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
