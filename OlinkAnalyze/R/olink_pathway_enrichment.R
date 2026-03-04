#' Performs pathway enrichment using over-representation analysis (ORA) or
#' gene set enrichment analysis (GSEA)
#'
#' This function performs enrichment analysis based on statistical test results
#' and full data using clusterProfiler's gsea and enrich functions for MSigDB.
#'
#' @details
#' MSigDB is subset if the  ontology argument is KEGG, GO, or Reactome.
#' test_results must contain estimates for all assays.
#' Posthoc results can be used but should be filtered for one contrast to
#' improve interpretability.
#' Alternative statistical results can be used as input as long as they include
#' the columns "OlinkID", "Assay", and "estimate". A column named
#' "Adjusted_pal" is also needed for ORA. Any statistical results that contains
#' one estimate per protein will work as long as the estimates are comparable
#' to each other.
#'
#' clusterProfiler is originally developed by Guangchuang Yu at the School of
#' Basic Medical Sciences at Southern Medical University.
#'
#' T Wu, E Hu, S Xu, M Chen, P Guo, Z Dai, T Feng, L Zhou, W Tang, L Zhan,
#' X Fu, S Liu, X Bo, and G Yu.
#' clusterProfiler 4.0: A universal enrichment tool for interpreting omics data.
#' The Innovation. 2021, 2(3):100141.
#' doi: 10.1016/j.xinn.2021.100141
#'
#' \strong{NB:} We strongly recommend to set a seed prior to running this
#' function to ensure reproducibility of the results.
#'
#' \strong{A few notes on Pathway Enrichment with Olink Data}
#'
#' It is important to note that sometimes the proteins that are assayed in
#' Olink Panels are related to specific biological areas and therefore do not
#' represent an unbiased overview of the proteome as a whole.
#' Pathways can only interpreted based on the background/context they came from.
#' For this reason, an estimate for all assays measured must
#'  be provided. Furthermore, certain pathways cannot come up based on Olink's
#'  coverage in this area.  Additionally, if only the Inflammation panel was
#'  run, then the available pathways would be given based on a background
#'   of proteins related to inflammation. Both ORA and GSEA can provide
#'   mechanistic and disease related insight and are best to use when
#'   trying to uncover pathways/annotations of interest. It is recommended to
#'   only use pathway enrichment for hypothesis generating data, which is more
#'   well suited for data on the Explore platform or on multiple Target 96
#'   panels. For smaller lists of proteins it may be more informative to use
#'   biological annotation in directed research, to discover which significant
#'   assays are related to keywords of interest.
#'
#'
#' @param data NPX data frame in long format with at least protein name (Assay),
#' OlinkID, UniProt,SampleID, QC_Warning, NPX, and LOD
#' @param check_log A named list returned by [`check_npx()`]. If `NULL`,
#' [`check_npx()`] will be run internally using `data`.
#' @param test_results a dataframe of statistical test results including
#' Adjusted_pval and estimate columns.
#' @param method Either "GSEA" (default) or "ORA"
#' @param ontology Supports "MSigDb" (default), "MSigDb_com",
#' "KEGG", "GO", and "Reactome" as arguments.
#' MSigDb contains C2 and C5 genesets. C2 and C5 encompass KEGG, GO,
#' and Reactome. MSigDb_com consists of C2 and C5 genesets without KEGG
#' genesets as KEGG is not permitted for commercial use.
#' @param organism Either "human" (default) or "mouse"
#' @param pvalue_cutoff (numeric) maximum Adjusted p-value cutoff for ORA
#' filtering of foreground set (default = 0.05). This argument is not used for
#' GSEA.
#' @param estimate_cutoff (numeric) minimum estimate cutoff for ORA filtering
#' of foreground set (default = 0) This argument is not used for GSEA.
#' @return A data frame of enrichment results.
#' Columns for ORA include:
#' \itemize{
#'  \item{ID:} "character" Pathway ID from MSigDB
#'  \item{Description:} "character" Description of Pathway from MSigDB
#'  \item{GeneRatio:} "character" ratio of input proteins that are annotated in
#'  a term
#'  \item{BgRatio:} "character" ratio of all genes that are annotated in this
#'  term
#'  \item{pvalue:} "numeric" p-value of enrichment
#'  \item{p.adjust:} "numeric" Adjusted p-value (Benjamini-Hochberg)
#'  \item{qvalue:} "numeric" false discovery rate, the estimated probability
#'  that the normalized enrichment score represents
#'  a false positive finding
#'  \item{geneID:} "character" list of input proteins (Gene Symbols) annotated
#'  in a term delimited by "/"
#'  \item{Count:} "integer" Number of input proteins that are annotated in a
#'  term
#' }
#'
#' Columns for GSEA:
#' \itemize{
#'  \item{ID:} "character" Pathway ID from MSigDB
#'  \item{Description:} "character" Description of Pathway from MSigDB
#'  \item{setSize:} "integer" ratio of input proteins that are annotated in a
#'  term
#'  \item{enrichmentScore:} "numeric" Enrichment score, degree to which a gene
#'  set is over-represented at the top or bottom of the ranked list of genes
#'  \item{NES:} "numeric" Normalized Enrichment Score, normalized to account
#'  for differences in gene set size and in correlations between gene sets and
#'  expression data sets. NES can be used to compare analysis results across
#'  gene sets.
#'  \item{pvalue:} "numeric" p-value of enrichment
#'  \item{p.adjust:} "numeric" Adjusted p-value (Benjamini-Hochberg)
#'  \item{qvalue:} "numeric" false discovery rate, the estimated probability
#'  that the normalized enrichment score represents a false positive finding
#'  \item{rank:} "numeric" the position in the ranked list where the
#'  maximum enrichment score occurred
#'  \item{leading_edge:} "character" contains tags, list, and signal. Tags
#'  gives an indication of the percentage of genes contributing to the
#'  enrichment score. List gives an indication of where in the list
#'  the enrichment score is obtained. Signal represents the enrichment signal
#'  strength and combines the tag and list.
#'  \item{core_enrichment:} "character" list of input proteins (Gene Symbols)
#'  annotated in a term delimited by "/"
#' }
#'
#' @examples
#' \donttest{
#' library(dplyr)
#' npx_df <- npx_data1 |> filter(!grepl("control", SampleID,
#'   ignore.case = TRUE))
#'  try({ 
#' ttest_results <- olink_ttest(
#'   df = npx_df,
#'   variable = "Treatment",
#'   alternative = "two.sided"
#' )
#' # This expression might fail if dependencies are not installed
#' gsea_results <- olink_pathway_enrichment(data = npx_data1,
#'   test_results = ttest_results)
#' ora_results <- olink_pathway_enrichment(
#'   data = npx_data1,
#'   test_results = ttest_results, method = "ORA"
#' )
#' }, silent = TRUE)
#' }
#'
#' @export

olink_pathway_enrichment <- function(data,
                                     check_log = NULL,
                                     test_results,
                                     method = "GSEA",
                                     ontology = "MSigDb",
                                     organism = "human",
                                     pvalue_cutoff = 0.05,
                                     estimate_cutoff = 0) {
  # Is Package installed
  rlang::check_installed(pkg = c("clusterProfiler", "msigdbr"),
                         call = rlang::caller_env())

  if (utils::packageVersion("msigdbr") < package_version("24.1.0")) {
    cli::cli_abort(c(paste("Pathway enrichment requires version >=24.1.0",
                           "of the msigdbr package.",
                           "Please install a supported version of msigdbr",
                           "before continuing."),
                     " " = "install.packages(\"msigdbr\")"))
  }

  if (missing(data) || missing(test_results)) {
    cli::cli_abort("The data and test_results arguments need to be specified.")
  }

  check_log <- check_pe_inputs(data = data,
                               check_log = check_log,
                               test_results = test_results,
                               method = method,
                               ontology = ontology,
                               organism = organism)


  df <- data_prep(data = data,
                  check_log = check_log,
                  test_results = test_results)


  test_results <- test_prep(df = df,
                            test_results = test_results)


  msig_df <- select_ont(ontology = ontology,
                        organism = organism)

  if (method == "ORA") {
    results <- ora_pathwayenrichment(test_results = test_results,
                                     msig_df = msig_df,
                                     pvalue_cutoff = pvalue_cutoff,
                                     estimate_cutoff = estimate_cutoff)
    cli::cli_inform("Over-representation Analysis performed")
  } else {
    gene_list <- results_to_genelist(test_results = test_results)
    results <- gsea_pathwayenrichment(gene_list = gene_list,
                                      msig_df = msig_df)
    cli::cli_inform("Gene set enrichment analysis used by default.")
  }

  return(results)
}


check_pe_inputs <- function(data,
                            check_log = check_log,
                            test_results,
                            method,
                            ontology,
                            organism) {

  check_log <- run_check_npx(df = data, check_log = check_log)

  # Check required columns in test results

  if (!all(c("OlinkID",
             "estimate",
             "Assay") %in% colnames(test_results))) {
    cli::cli_abort(message = paste("test_results must include the",
                                   "following columns: \n",
                                   "OlinkID,",
                                   "Assay,",
                                   "estimate", sep = "\n"))
  }

  if (length(c(setdiff(unique(data[["OlinkID"]]),
                       unique(test_results[["OlinkID"]])),
               setdiff(unique(test_results[["OlinkID"]]),
                       unique(data[["OlinkID"]])))) != 0) {
    cli::cli_warn(paste("The Olink IDs in the data do not all match",
                        "the Olink IDs in the test_results."))
  }

  if ("contrast" %in% colnames(test_results) &&
        length(unique(test_results[["contrast"]])) > 1) {
    cli::cli_abort(paste("More than one contrast is specified in test results.",
                         "Filter test_results for desired contrast."))
  }

  if (!(method %in% c("GSEA", "ORA"))) {
    cli::cli_abort("Method must be \"GSEA\" or \"ORA\".")
  }

  if (!(ontology %in% c("MSigDb",
                        "Reactome",
                        "KEGG",
                        "GO",
                        "MSigDb_com"))) {
    cli::cli_abort(paste("Ontology must be one of MSigDb, MSigDb_com,",
                         "Reactome, KEGG, or GO."))
  }

  if (!(organism %in% c("human", "mouse"))) {
    cli::cli_abort(print("organism should be \"human\" or \"mouse\""))
  }

  return(check_log)
}

data_prep <- function(data,
                      test_results,
                      check_log) {
  cli::cli_inform("Removing invalid OlinkIDs, control assays, and NA assays.")
  data <- clean_npx(df = data,
                    check_log = check_log,
                    remove_assay_na = TRUE,
                    remove_invalid_oid = TRUE,
                    remove_dup_sample_id = FALSE,
                    remove_control_assay = TRUE,
                    remove_control_sample = FALSE,
                    remove_qc_warning = FALSE,
                    remove_assay_warning = FALSE,
                    convert_df_cols = TRUE,
                    convert_nonunique_uniprot = FALSE,
                    verbose = FALSE)


  if (length(c(setdiff(unique(data[["OlinkID"]]),
                       unique(test_results[["OlinkID"]])),
               setdiff(unique(test_results[["OlinkID"]]),
                       unique(data[["OlinkID"]])))) != 0) {
    cli::cli_inform(paste0("Filtering data for overlapping OlinkIDs in data ",
                           "and test_results."))
    data <- data |>
      dplyr::filter(.data[["OlinkID"]] %in% test_results[["OlinkID"]])
  }

  duplicated_assays <- data |>
    dplyr::select(dplyr::any_of(c("SampleID", "Assay"))) |>
    duplicated()


  if (any(duplicated_assays)) {
    cli::cli_abort(paste("Duplicated assays detected:",
                         paste(data[["Assay"]][duplicated_assays],
                               collapse = ","),
                         "\n",
                         "Filter assay from test_result or data",
                         sep = "\n"))
  }
  return(data)
}

test_prep <- function(df,
                      test_results) {
  if (length(c(setdiff(unique(df[["OlinkID"]]),
                       unique(test_results[["OlinkID"]])),
               setdiff(unique(test_results[["OlinkID"]]),
                       unique(df[["OlinkID"]])))) != 0) {
    test_results <- test_results |>
      dplyr::filter(test_results[["OlinkID"]] %in% df[["OlinkID"]])
  }

  return(test_results)

}


select_ont <- function(ontology,
                       organism) {
  if (organism == "human") {
    msig_df <- msigdbr::msigdbr(species = "Homo sapiens", collection = "C2") |>
      dplyr::bind_rows(msigdbr::msigdbr(species = "Homo sapiens",
                                        collection = "C5"))
  } else if (organism == "mouse") {
    msig_df <- msigdbr::msigdbr(species = "Mus musculus", collection = "C2") |>
      dplyr::bind_rows(msigdbr::msigdbr(species = "Mus musculus",
                                        collection = "C5"))
  }

  if (ontology == "Reactome") {
    cli::cli_inform("Extracting Reactome Database from MSigDB...")
    msig_df <- msig_df |>
      dplyr::filter(.data[["gs_subcollection"]] == "CP:REACTOME")

  } else if (ontology == "KEGG") {
    cli::cli_alert_warning("KEGG is not approved for commercial use.")
    cli::cli_inform("Extracting KEGG Database from MSigDB...")
    msig_df <- msig_df |>
      dplyr::filter(.data[["gs_subcollection"]] == "CP:KEGG_MEDICUS")

  } else if (ontology == "GO") {
    cli::cli_inform("Extracting GO Database from MSigDB...")
    msig_df <- msig_df |>
      dplyr::filter(.data[["gs_subcollection"]] %in% c("GO:BP",
                                                       "GO:CC",
                                                       "GO:MF"))

  } else if (ontology == "MSigDb_com") {
    msig_df <- msig_df |>
      dplyr::filter(stringr::str_detect(.data[["gs_subcollection"]],
                                        "KEGG",
                                        negate = TRUE))

    cli::cli_inform("Using MSigDB without KEGG subcollections...")

  } else {
    cli::cli_inform("Using MSigDB...")
  }

  msig_df <- msig_df |>
    dplyr::select(dplyr::any_of(c("gs_name", "gene_symbol")))

  return(msig_df)
}

results_to_genelist <- function(test_results) {
  estimate <- test_results[["estimate"]]
  names(estimate) <- test_results[["Assay"]]
  gene_list <- sort(estimate, decreasing = TRUE)

  cli::cli_inform("Test results converted to gene list")

  return(gene_list)
}

gsea_pathwayenrichment <- function(gene_list, msig_df) {
  if (length(setdiff(names(gene_list), msig_df[["gene_symbol"]]) != 0)) {
    cli::cli_inform(paste0(length(setdiff(names(gene_list),
                                          msig_df[["gene_symbol"]])),
                           " assays are not found in the database. ",
                           "Please check the Assay names for the following",
                           " assays:\n ",
                           toString(setdiff(names(gene_list),
                                            msig_df[["gene_symbol"]]))))
  }

  gsea <- clusterProfiler::GSEA(geneList = gene_list,
                                TERM2GENE = msig_df,
                                pvalueCutoff = 1)

  if (is.null(gsea)) {
    cli::cli_warn(
      "No remaining pathways within the range 10-500 proteins!"
    )
    return(NULL)
  } else {
    return(gsea@result)
  }
}

ora_pathwayenrichment <- function(test_results,
                                  msig_df,
                                  pvalue_cutoff = pvalue_cutoff,
                                  estimate_cutoff = estimate_cutoff) {
  sig_genes <- test_results |>
    dplyr::filter(.data[["Adjusted_pval"]] < pvalue_cutoff) |>
    dplyr::filter(abs(.data[["estimate"]]) > estimate_cutoff) |>
    dplyr::distinct(.data[["Assay"]]) |>
    dplyr::pull(.data[["Assay"]])

  universe <- test_results |>
    dplyr::distinct(.data[["Assay"]]) |>
    dplyr::pull(.data[["Assay"]])

  if (length(setdiff(universe, msig_df[["gene_symbol"]]) != 0)) {
    cli::cli_inform(paste0(length(setdiff(universe, msig_df[["gene_symbol"]])),
                           " assays are not found in the database. ",
                           "Please check the Assay names for the following",
                           " assays:\n ",
                           toString(setdiff(universe,
                                            msig_df[["gene_symbol"]]))))
  }

  ora <- clusterProfiler::enricher(gene = sig_genes,
                                   universe = universe,
                                   TERM2GENE = msig_df,
                                   pvalueCutoff = 1)

  if (is.null(ora)) {
    cli::cli_warn(
      "No remaining pathways within the range 10-500 proteins!"
    )
    return(NULL)
  } else {
    return(ora@result)
  }
}
