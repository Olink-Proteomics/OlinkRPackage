#' Performs pathway enrichment using over-representation analysis (ORA) or gene set enrichment analysis (GSEA) based on statistical test results and full data using clusterProfiler's gsea and enrich functions for various databases.
#'
#' @param data NPX data frame in long format with at least protein name (Assay), OlinkID, UniProt,SampleID, QC_Warning, NPX, and LOD
#' @param test_results a dataframe of statistical test results including Adjusted_pval and estimate columns. Currently only works for tests with estimates for all assays.
#' @param method Either "GSEA" (default) or "ORA"
#' @param ontology Supports "MSigDb" (default), "KEGG", "GO", and "Reactome" as arguements. MSigDb contains C2 and C5 genesets. C2 and C5 encompass KEGG, GO, and Reactome.
#' @param organism Either "human" (default) or "mouse"
#' @param ... Additional options to be passed to enricher or GSEA from Cluster profiler
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
#' gsea_results <- olink_Pathway_Enrichment(data = npx_data1, test_results = ttest_results)
#' ora_results <- olink_Pathway_Enrichment(
#'   data = npx_data1,
#'   test_results = ttest_results, method = "ORA"
#' )
#' }
#' @seealso \itemize{
#' \item{\code{\link[OlinkAnalyze:olink_Pathway_Heatmap]{olink_Pathway_Heatmap}} for generating a heat map of results}
#' \item{\code{\link[OlinkAnalyze:olink_Pathway_Visualization]{olink_Pathway_Visualization}} for generating a bar graph of results}
#' }
#'
#' @importFrom dplyr filter mutate select group_by summarise arrange ungroup distinct pull full_join
#' @importFrom clusterProfiler bitr gseKEGG gseGO GSEA enrichKEGG enrichGO enricher
#' @importFrom msigdbr msigdbr
#' @importFrom ReactomePA gsePathway enrichPathway
#' @importFrom stringr str_length
#' @importFrom org.Hs.eg.db org.Hs.eg.db
#' @importFrom org.Mm.eg.db org.Mm.eg.db
#' @importFrom magrittr %>%
#' @export

olink_Pathway_Enrichment <- function(data, test_results, method = "GSEA", ontology = "MSigDb", organism = "human") {
  data2 <- data_prep(data = data)
  test_results2 <- test_prep(data = data2, test_results = test_results, organism = organism)
  geneList <- results_to_genelist(test_results = test_results2)
  if (method == "ORA") {
    results <- ora_pathwayenrichment(
      test_results = test_results2, ontology = ontology,
      organism = organism, ...
    )
    message("Over-representation Analysis performed")
  } else {
    results <- gsea_pathwayenrichment(geneList = geneList, ontology = ontology, organism = organism)
    message("Gene set enrichment analysis used by default.")
  }
  return(results)
}


data_prep <- function(data) {
  # Filter out invalid Uniprot IDs
  invalid_Uniprots <- data %>%
    dplyr::filter(str_length(UniProt) != 6) %>%
    dplyr::select(Assay) %>%
    dplyr::distinct() %>% 
    dplyr::pull()
  if (length(invalid_Uniprots) != 0){
    message(paste0("There are ", length(invalid_Uniprots), " UniProt IDs that are an unexpected length and have been filtered out. \nThe expected length is 6 characters."))
  }
  data <- data %>% filter(str_length(UniProt) == 6)
  # Filter highest detectibility for repeated IDs
  olink_ids <- data %>%
    dplyr::filter(!str_detect(SampleID, "CONTROL*.")) %>%
    dplyr::filter(QC_Warning == "Pass") %>%
    dplyr::mutate(Detected = as.numeric(NPX > LOD)) %>%
    dplyr::select(OlinkID, UniProt, Assay, Panel, Detected) %>%
    dplyr::group_by(OlinkID, UniProt) %>%
    dplyr::summarise(Sum = (sum(Detected) / n())) %>%
    dplyr::arrange(desc(Sum)) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(UniProt, .keep_all = TRUE) %>%
    dplyr::pull(OlinkID)
  data <- data %>% dplyr::filter(OlinkID %in% olink_ids)
  message("Data filtered for highest detectibility and valid UniProt IDs.")
  return(data)
}
test_prep <- function(data, test_results, organism = "human") {
  test_results <- test_results %>%
    dplyr::filter(OlinkID %in% data$OlinkID)
  message("Test results filtered for highest detectibility and converted to ENTREZID")
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
    human_df <- msigdbr::msigdbr(species = "Homo sapiens", category = "C2") %>%
      rbind(msigdbr::msigdbr(species = "Homo sapiens", category = "C5"))
    if()
    msig_df <- human_df %>% dplyr::select(gs_name, gene_symbol)
  } else if (organism == "mouse") {
    mouse_df <- msigdbr::msigdbr(species = "Mus musculus", category = "C2") %>%
      rbind(msigdbr::msigdbr(species = "Mus musculus", category = "C5"))
    msig_df <- mouse_df %>% dplyr::select(gs_name, gene_symbol)
  } else {
    stop(print("organism should be \"human\" or \"mouse\""))
  }
  if (ontology == "Reactome"){
    GSEA <- ReactomePA::gsePathway(geneList, organism = organism, pvalueCutoff = 1)
  } else if (ontology == "KEGG"){
    GSEA <- clusterProfiler::gseKEGG(geneList, organism = KEGG_org,  keyType = "ncbi-geneid", pvalueCutoff = 1)
  } else if (ontology == "GO"){
    GSEA <- clusterProfiler::gseGO(geneList, OrgDb = GO_org, pvalueCutoff = 1)
  } else if (ontology == "MSigDb"){
    ifelse(length(setdiff(names$geneList, msig_df$gene_symbol) != 0),
           message(paste0(length(setdiff(names(geneList), msig_df$gene_symbol)), " assays are not found in the database. Please check the Assay names for the following assays, "))
    GSEA <- clusterProfiler::GSEA(geneList = geneList, TERM2GENE = msig_df, pvalueCutoff = 1)
  }else {
    stop(print("Ontology must equal GO, KEGG, MSigDb or Reactome."))
  }
  return(GSEA@result)
}
ora_pathwayenrichment <- function(test_results, organism, ontology = ontology, pvalue_cutoff = 0.05, estimate_cutoff = 0) {
  sig_genes <- test_results %>%
    filter(Adjusted_pval < pvalue_cutoff) %>%
    filter(estimate > estimate_cutoff) %>%
    distinct(ENTREZID) %>%
    pull(ENTREZID)
  universe <- test_results %>%
    distinct(ENTREZID) %>%
    pull(ENTREZID)
  if (organism == "human") {
    human_df <- msigdbr(species = "Homo sapiens", category = "C2") %>%
      rbind(msigdbr(species = "Homo sapiens", category = "C5"))
    msig_df <- human_df %>% dplyr::select(gs_name, entrez_gene)
  } else if (organism == "mouse") {
    mouse_df <- msigdbr(species = "Mus musculus", category = "C2") %>%
      rbind(msigdbr(species = "Mus musculus", category = "C5"))
    msig_df <- mouse_df %>% dplyr::select(gs_name, entrez_gene)
  } else {
    stop(print("organism should be \"human\" or \"mouse\""))
  }
  if (ontology == "Reactome"){
    ORA <- enrichPathway(gene = sig_genes, universe = universe, organism = organism, pvalueCutoff = 1)
  } else if (ontology == "KEGG"){
    ORA <- enrichKEGG(gene = sig_genes, universe = universe, keyType = 'ncbi-geneid', organism = KEGG_org, pvalueCutoff = 1)
  } else if (ontology == "GO"){
    ORA <- enrichGO(gene = sig_genes, universe = universe, ont = "BP", OrgDb = GO_org, pvalueCutoff = 1)
  } else if (ontology == "MSigDb"){
    ORA <- enricher(gene = sig_genes, universe = universe, TERM2GENE = msig_df, pvalueCutoff = 1)
  } else{
    stop(print("ontology must equal GO, KEGG, MSigDb or Reactome."))
  }
  return(ORA@result)
}
