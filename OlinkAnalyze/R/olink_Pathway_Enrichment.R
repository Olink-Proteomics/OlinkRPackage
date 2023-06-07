#' Performs pathway enrichment using over-representation analysis (ORA) or gene set enrichment analysis (GSEA)
#'
#' This function performs enrichment analysis based on statistical test results and full data using clusterProfiler's gsea and enrich functions for MSigDB.
#'
#' @details
#' MSigDB is subset if the  ontology argument is KEGG, GO, or Reactome. test_results must contain estimates for all assays.
#' Posthoc results can be used but should be filtered for one contrast to improve interpretability.
#' Alternative statistical results can be used as input as long as they include the columns
#'  "OlinkID", "Assay", and "estimate". A column named "Adjusted_pal" is also needed for ORA. Any statistical results that contains one estimate per protein will work as long as the estimates are comparable to each other.
#'
#' clusterProfiler is originally developed by Guangchuang Yu at the School of Basic Medical Sciences at Southern Medical University.
#'
#' T Wu, E Hu, S Xu, M Chen, P Guo, Z Dai, T Feng, L Zhou, W Tang, L Zhan, X Fu, S Liu, X Bo, and G Yu.
#' clusterProfiler 4.0: A universal enrichment tool for interpreting omics data. The Innovation. 2021, 2(3):100141.
#' doi: 10.1016/j.xinn.2021.100141
#'
#' \strong{NB:} We strongly recommend to set a seed prior to running this function to ensure reproducibility of the results.
#'
#' \strong{A few notes on Pathway Enrichment with Olink Data}
#'
#' It is important to note that sometimes the proteins that are assayed in Olink Panels
#'  are related to specific biological areas and therefore do not represent an unbiased overview of the proteome as a whole.
#'  Pathways can only interpreted based on the background/context they came from. For this reason, an estimate for all assays measured must
#'  be provided. Furthermore, certain pathways cannot come up based on Olink's  coverage in this area.  Additionally,
#'   if only the Inflammation panel was run, then the available pathways would be given based on a background
#'   of proteins related to inflammation. Both ORA and GSEA can provide mechanistic and disease related insight and are best to use when
#'   trying to uncover pathways/annotations of interest. It is recommended to only use pathway enrichment for hypothesis generating data, which
#'   is more well suited for data on the Explore platform or on multiple Target 96 panels. For smaller lists of proteins it may be more informative to use biological annotation in directed research,
#'   to discover which significant assay are related to keywords of interest.
#'
#'
#' @param data NPX data frame in long format with at least protein name (Assay), OlinkID, UniProt,SampleID, QC_Warning, NPX, and LOD
#' @param test_results a dataframe of statistical test results including Adjusted_pval and estimate columns.
#' @param method Either "GSEA" (default) or "ORA"
#' @param ontology Supports "MSigDb" (default), "KEGG", "GO", and "Reactome" as arguments. MSigDb contains C2 and C5 genesets. C2 and C5 encompass KEGG, GO, and Reactome.
#' @param organism Either "human" (default) or "mouse"
#' @param pvalue_cutoff (numeric) maximum Adjusted p-value cutoff for ORA filtering of foreground set (default = 0.05). This argument is not used for GSEA.
#' @param estimate_cutoff (numeric) minimum estimate cutoff for ORA filtering of foreground set (default = 0) This argument is not used for GSEA.
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
#' try(
#'   { # This expression might fail if dependencies are not installed
#'     gsea_results <- olink_pathway_enrichment(data = npx_data1, test_results = ttest_results)
#'     ora_results <- olink_pathway_enrichment(
#'       data = npx_data1,
#'       test_results = ttest_results, method = "ORA"
#'     )
#'   },
#'   silent = TRUE
#' )
#' }
#' @seealso \itemize{
#' \item{\code{\link[OlinkAnalyze:olink_pathway_heatmap]{olink_pathway_heatmap}} for generating a heat map of results}
#' \item{\code{\link[OlinkAnalyze:olink_pathway_visualization]{olink_pathway_visualization}} for generating a bar graph of results}
#' }
#'
#' @importFrom dplyr filter mutate select group_by summarise arrange desc ungroup distinct pull
#' @importFrom stringr str_detect
#' @importFrom magrittr %>%
#' @export

olink_pathway_enrichment <- function(data, test_results, method = "GSEA", ontology = "MSigDb", organism = "human", pvalue_cutoff = 0.05, estimate_cutoff = 0) {
  # Is Package installed
  if (!requireNamespace("clusterProfiler", quietly = TRUE)) {
    stop(" Pathway enrichment requires clusterProfiler package.
         Please install clusterProfiler before continuing.

         if (!require(\"BiocManager\", quietly = TRUE))
            install.packages(\"BiocManager\")
         BiocManager::install(\"clusterProfiler\")")
  }

  if (!requireNamespace("msigdbr", quietly = TRUE)) {
    stop(" Pathway enrichment requires msigdbr package.
         Please install msigdbr before continuing.

         install.packages(\"msigdbr\")")
  }

  # Data Checks
  if (length(unique(data$OlinkID)) != length(unique(test_results$OlinkID))) {
    warning("The number of Olink IDs in the data does not equal the number of Olink IDs in the test results.")
  }

  if ("contrast" %in% colnames(test_results) && length(unique(test_results$contrast)) > 1) {
    stop("More than one contrast is specified in test results. Filter test_results for desired contrast.")
  }

  if (!("estimate" %in% colnames(test_results))) {
    stop("Estimate column is not present in test results. Please check arguments.")
  }

  if (!(method %in% c("GSEA", "ORA"))) {
    stop("Method must be \"GSEA\" or \"ORA\".")
  }

  if (!(ontology %in% c("MSigDb", "Reactome", "KEGG", "GO"))) {
    stop("Ontology must be one of MSigDb, Reactome, KEGG, or GO.")
  }

  if (!(organism %in% c("human", "mouse"))) {
    stop(print("organism should be \"human\" or \"mouse\""))
  }

  data2 <- data_prep(data = data)
  test_results2 <- test_prep(data = data2, test_results = test_results, organism = organism)
  msig_df <- select_db(ontology = ontology, organism = organism)

  if (method == "ORA") {
    results <- ora_pathwayenrichment(
      test_results = test_results2,
      msig_df = msig_df,
      pvalue_cutoff = pvalue_cutoff,
      estimate_cutoff = estimate_cutoff
    )
    message("Over-representation Analysis performed")
  } else {
    geneList <- results_to_genelist(test_results = test_results2)
    results <- gsea_pathwayenrichment(
      geneList = geneList,
      msig_df = msig_df
    )
    message("Gene set enrichment analysis used by default.")
  }

  return(results)
}

data_prep <- function(data) {
  # Remove NA data and filter for valid Olink IDs
  npx_check <- npxCheck(data)
  data <- data %>%
    dplyr::filter(!(OlinkID %in% npx_check$all_nas)) %>%
    dplyr::filter(stringr::str_detect(
      OlinkID,
      "OID[0-9]{5}"
    ))
  # Filter highest detectibility for repeated IDs
  olink_ids <- data %>%
    dplyr::filter(!stringr::str_detect(string = SampleID, pattern = "CONTROL*.")) %>%
    dplyr::filter(toupper(QC_Warning) == "PASS") %>%
    dplyr::mutate(Detected = as.numeric(NPX > LOD)) %>%
    dplyr::select(OlinkID, Assay, Detected) %>%
    dplyr::group_by(OlinkID, Assay) %>%
    dplyr::summarise(Sum = (sum(Detected) / n())) %>%
    dplyr::arrange(dplyr::desc(Sum)) %>%
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
  names(estimate) <- test_results$Assay
  geneList <- sort(estimate, decreasing = TRUE)

  message("Test results converted to gene list")

  return(geneList)
}

select_db <- function(ontology = ontology, organism = organism) {
  if (organism == "human") {
    msig_df <- msigdbr::msigdbr(species = "Homo sapiens", category = "C2") %>%
      rbind(msigdbr::msigdbr(species = "Homo sapiens", category = "C5"))
  } else if (organism == "mouse") {
    msig_df <- msigdbr::msigdbr(species = "Mus musculus", category = "C2") %>%
      rbind(msigdbr::msigdbr(species = "Mus musculus", category = "C5"))
  }

  if (ontology == "Reactome") {
    message("Extracting Reactome Database from MSigDB...")
    msig_df <- msig_df %>%
      dplyr::filter(gs_subcat == "CP:REACTOME")
  } else if (ontology == "KEGG") {
    message("Extracting KEGG Database from MSigDB...")
    msig_df <- msig_df %>%
      dplyr::filter(gs_subcat == "CP:KEGG")
  } else if (ontology == "GO") {
    message("Extracting GO Database from MSigDB...")
    msig_df <- msig_df %>%
      dplyr::filter(gs_subcat %in% c("GO:BP", "GO:CC", "GO:MF"))
  } else {
    message("Using MSigDB...")
  }

  msig_df <- msig_df %>%
    dplyr::select(gs_name, gene_symbol)

  return(msig_df)
}

gsea_pathwayenrichment <- function(geneList, msig_df) {
  if (length(setdiff(names(geneList), msig_df$gene_symbol) != 0)) {
    message(paste0(
      length(setdiff(names(geneList), msig_df$gene_symbol)),
      " assays are not found in the database. Please check the Assay names for the following assays:\n ",
      toString(setdiff(names(geneList), msig_df$gene_symbol))
    ))
  }

  GSEA <- clusterProfiler::GSEA(geneList = geneList, TERM2GENE = msig_df, pvalueCutoff = 1)

  return(GSEA@result)
}

ora_pathwayenrichment <- function(test_results, msig_df, pvalue_cutoff = pvalue_cutoff, estimate_cutoff = estimate_cutoff) {
  sig_genes <- test_results %>%
    dplyr::filter(Adjusted_pval < pvalue_cutoff) %>%
    dplyr::filter(abs(estimate) > estimate_cutoff) %>%
    dplyr::distinct(Assay) %>%
    dplyr::pull(Assay)

  universe <- test_results %>%
    dplyr::distinct(Assay) %>%
    dplyr::pull(Assay)

  if (length(setdiff(universe, msig_df$gene_symbol) != 0)) {
    message(paste0(
      length(setdiff(universe, msig_df$gene_symbol)),
      " assays are not found in the database. Please check the Assay names for the following assays:\n ",
      toString(setdiff(universe, msig_df$gene_symbol))
    ))
  }

  ORA <- clusterProfiler::enricher(gene = sig_genes, universe = universe, TERM2GENE = msig_df, pvalueCutoff = 1)

  return(ORA@result)
}
