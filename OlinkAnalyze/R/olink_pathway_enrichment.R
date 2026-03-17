#' Performs pathway enrichment using over-representation analysis (ORA) or
#' gene set enrichment analysis (GSEA)
#'
#' @author
#'   Kathleen Nevola;
#'   Klev Diamanti
#'
#' @description
#' This function performs enrichment analysis based on statistical test results
#' and full data using `clusterProfiler`'s functions `gsea` and `enrich` for
#' MSigDB.
#'
#' @details
#' MSigDB is subset if the ontology argument is "KEGG", "GO", or "Reactome". The
#' argument `test_results` must contain estimates for all assays, otherwise an
#' error will be thrown. Results from a post-hoc statistical test can be used as
#' argument for `test_results`, but the user needs to select and filter one
#' contrast to improve interpretability of the results. Alternative statistical
#' results can be used as input as long as they include the columns "OlinkID",
#' "Assay", and "estimate". A column named "Adjusted_pval" is also required for
#' ORA. Any statistical result that contains exactly one estimate per protein
#' will work as long as the estimates are comparable to each other.
#'
#' The R library `clusterProfiler` is originally developed by Guangchuang Yu at
#' the School of Basic Medical Sciences at Southern Medical University.
#'
#' \strong{NB:} We strongly recommend to set a seed prior to running this
#' function to ensure reproducibility of the results.
#'
#' \strong{An important note regarding Pathway Enrichment with Olink Data}
#'
#' It is important to note that sometimes the proteins that are assayed in Olink
#' Panels are related to specific biological areas and therefore do not
#' represent an unbiased overview of the proteome as a whole, which is an
#' assumption for pathway enrichment. Pathways can only interpreted based on the
#' background/context they came from. For this reason, an estimate for all
#' assays measured must be provided. Furthermore, certain pathways cannot come
#' up based on Olink's coverage in this area. Additionally, if only the
#' Inflammation panel was run, then the available pathways would be given based
#' on a background of proteins related to inflammation. Both ORA and GSEA can
#' provide mechanistic and disease related insight and are best to use when
#' trying to uncover pathways/annotations of interest. It is recommended to only
#' use pathway enrichment for hypothesis generating data, which is better suited
#' for data originating from Olink's NGS platforms
#' `r ansi_collapse_quot(get_olink_platforms(broad_platform = "NGS"))` or on
#' multiple Target 96 panels from Olink's qPCR platform. For smaller lists of
#' proteins it may be more informative to use biological annotation in directed
#' research, to discover which significant assays are related to keywords of
#' interest.
#'
#' @references
#' Wu, T. et al. (2021). *clusterProfiler 4.0: A universal enrichment tool for*
#' *interpreting omics data*. The Innovation, 2(3):100141.
#' doi: 10.1016/j.xinn.2021.100141.
#'
#' @param df NPX data frame in long format with at least protein name ("Assay"),
#' "OlinkID", "UniProt", "SampleID", QC warning ("QC_Warning" or "SampleQC"),
#' quantification column ("NPX", "Ct" or "Quantified_value"), and one or more
#' columns representing limit of detection ("LOD", "PlateLOD" or "MaxLOD").
#' @param test_results a data frame of statistical test results including the
#' columns "Adjusted_pval" and "estimate".
#' @param check_log A named list returned by [`check_npx()`]. If `NULL`,
#' [`check_npx()`] will be run internally using `df`.
#' @param method One of "GSEA" (default) or "ORA".
#' @param ontology One of "MSigDb" (default), "MSigDb_com", "KEGG", "GO", and
#' "Reactome". "MSigDb" contains "C2" and "C5" gene sets which encompass "KEGG",
#' "GO", and "Reactome". "MSigDb_com" consists of "C2" and "C5" gene sets
#' without "KEGG", as the latter not permitted for commercial use.
#' @param organism One of "human" (default) or "mouse".
#' @param pvalue_cutoff (numeric) maximum adjusted p-value cutoff for ORA
#' filtering of foreground set (default = 0.05). This argument is not used for
#' GSEA.
#' @param estimate_cutoff (numeric) minimum estimate cutoff for ORA filtering
#' of foreground set (default = 0). This argument is not used for GSEA.
#'
#' @return A data frame of enrichment results.
#'
#' Columns for ORA include:
#' \itemize{
#'  \item{ID:} Pathway ID from MSigDB.
#'  \item{Description:} Description of Pathway from MSigDB.
#'  \item{GeneRatio:} Ratio of input proteins that are annotated in a term.
#'  \item{BgRatio:} Ratio of all genes that are annotated in this term.
#'  \item{pvalue:} P-value of enrichment.
#'  \item{p.adjust:} Benjamini-Hochberg adjusted p-value.
#'  \item{qvalue:} False discovery rate (FDR), the estimated probability that
#'  the normalized enrichment score represents a false positive finding.
#'  \item{geneID:} List of input proteins (Gene Symbols) annotated in a term,
#'  delimited by "/".
#'  \item{Count:} Number of input proteins that are annotated in a term.
#' }
#'
#' Columns for GSEA:
#' \itemize{
#'  \item{ID:} Pathway ID from MSigDB.
#'  \item{Description:} Description of Pathway from MSigDB.
#'  \item{setSize:} Ratio of input proteins that are annotated in a term.
#'  \item{enrichmentScore:} Enrichment score (ES), degree to which a gene set is
#'  over-represented at the top or bottom of the ranked list of genes.
#'  \item{NES:} Normalized Enrichment Score (NES), normalized to account for
#'  differences in gene set size and in correlations between gene sets and
#'  expression data sets. NES can be used to compare analysis results across
#'  gene sets.
#'  \item{pvalue:} P-value of enrichment.
#'  \item{p.adjust:} Benjamini-Hochberg adjusted p-value.
#'  \item{qvalue:} False discovery rate (FDR), the estimated probability that
#'  the normalized enrichment score represents a false positive finding.
#'  \item{rank:} The position in the ranked list where the maximum enrichment
#'  score occurred.
#'  \item{leading_edge:} Contains tags, list, and signal. Tags provide an
#'  indication of the percentage of genes contributing to the ES. List gives an
#'  indication of where in the list the ES is obtained. Signal represents the
#'  enrichment signal strength and combines the tag and list.
#'  \item{core_enrichment:} List of input proteins (Gene Symbols) annotated in a
#'  term, delimited by "/".
#' }
#'
#' @export
#'
#' @examples
#' \donttest{
#' if (rlang::is_installed(pkg = c("msigdbr", "clusterProfiler"))) {
#'   npx_df <- npx_data1 |>
#'     dplyr::filter(
#'       !grepl(
#'         pattern = "control",
#'         x = .data[["SampleID"]],
#'         ignore.case = TRUE
#'       )
#'     )
#'
#'   check_log <- check_npx(df = npx_df)
#'
#'   ttest_results <- OlinkAnalyze::olink_ttest(
#'     df = npx_df,
#'     variable = "Treatment",
#'     alternative = "two.sided",
#'     check_log = check_log
#'   )
#'
#'   # GSEA
#'   gsea_results <- OlinkAnalyze::olink_pathway_enrichment(
#'     df = npx_df,
#'     test_results = ttest_results,
#'     check_log = check_log
#'   )
#'
#'   # ORA
#'   ora_results <- olink_pathway_enrichment(
#'     df = npx_df,
#'     test_results = ttest_results,
#'     check_log = check_log,
#'     method = "ORA"
#'   )
#' }
#' }
#'
olink_pathway_enrichment <- function(df,
                                     test_results,
                                     check_log = NULL,
                                     method = "GSEA",
                                     ontology = "MSigDb",
                                     organism = "human",
                                     pvalue_cutoff = 0.05,
                                     estimate_cutoff = 0) {
  # check input ----

  # Is Package installed
  rlang::check_installed(pkg = c("msigdbr"),
                         version = "24.1.0",
                         call = rlang::caller_env())

  # custom check for "clusterProfiler" as it is a bioconductor package and not
  # on CRAN, which is not supported by rlang::check_installed()
  check_library_installed(x = "clusterProfiler",
                          error = TRUE)

  if (missing(df) || missing(test_results)) {
    cli::cli_abort("Arguments {.arg df} and {.arg test_results} are required!")
  }

  check_log <- check_pe_inputs(df = df,
                               check_log = check_log,
                               test_results = test_results,
                               method = method,
                               ontology = ontology,
                               organism = organism)

  # prepare data ----

  df <- data_prep(df = df,
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

check_pe_inputs <- function(df,
                            check_log,
                            test_results,
                            method,
                            ontology,
                            organism) {

  check_log <- run_check_npx(df = df, check_log = check_log)

  # check required columns in test_results ----

  check_columns(df = test_results,
                col_list = list("OlinkID", "estimate", "Assay"))

  # check that the assays in test_results match those in df ----

  non_overlap_assays <- helper_non_overlap_assays(
    df = df,
    test_results = test_results,
    check_log = check_log,
    which = "both"
  )

  if (length(non_overlap_assays) != 0L) {
    cli::cli_warn(
      c(
        "The sets of assays in {.arg df} and {.arg test_results} do not
        match!",
        "i" = "The sets are expected to be identical for pathway enrichment."
      ),
      call = rlang::caller_env(),
      wrap = TRUE
    )
  }

  # check for contrasts ----

  # if contrasts is present in the test_results, check that there is only one
  # contrast. If there are multiple contrasts, throw an error and ask the user
  # to filter for the desired contrast. This is because the results can be
  # difficult, or impossible, to interpret if there are multiple contrasts.
  if ("contrast" %in% colnames(test_results) &&
        length(unique(test_results[["contrast"]])) > 1L) {
    cli::cli_abort(
      c(
        "x" = "{.val {length(unique(test_results[['contrast']]))}} contrast{?s}
        present in {.arg test_results}!",
        "i" = "Filter {.arg test_results} for desired contrast prior to running
        pathway enrichment."
      ),
      call = rlang::caller_env(),
      wrap = TRUE
    )
  }

  # check enrichment method ----

  expected_methods <- c("GSEA", "ORA")

  if (!(method %in% expected_methods)) {
    cli::cli_abort(
      c(
        "x" = "{.val {method}} is not a valid method for pathway enrichment!",
        "i" = "Expected one of {.val {expected_methods}}."
      ),
      call = rlang::caller_env(),
      wrap = TRUE
    )
  }

  # check ontology  ----

  expected_ontologies <- c("MSigDb", "MSigDb_com", "Reactome", "KEGG", "GO")

  if (!(ontology %in% expected_ontologies)) {
    cli::cli_abort(
      c(
        "x" = "{.val {ontology}} is not a valid ontology for pathway
        enrichment!",
        "i" = "Expected one of {.val {expected_ontologies}}."
      ),
      call = rlang::caller_env(),
      wrap = TRUE
    )
  }

  # check organism  ----

  expected_organisms <- c("human", "mouse")

  if (!(organism %in% expected_organisms)) {
    cli::cli_abort(
      c(
        "x" = "{.val {organism}} is not a valid organism for pathway
        enrichment!",
        "i" = "Expected one of {.val {expected_organisms}}."
      ),
      call = rlang::caller_env(),
      wrap = TRUE
    )
  }

  return(check_log)
}

helper_non_overlap_assays <- function(df,
                                      test_results,
                                      check_log,
                                      which = "both") {
  no_overlap_in_df <- setdiff(
    x = unique(df[[check_log$col_names$olink_id]]),
    y = unique(test_results[["OlinkID"]])
  )

  no_overlap_in_res <- setdiff(
    x = unique(test_results[["OlinkID"]]),
    y = unique(df[[check_log$col_names$olink_id]])
  )

  if (which == "both") {
    no_overlap_assays <- c(no_overlap_in_df,
                           no_overlap_in_res)
  } else if (which == "df") {
    no_overlap_assays <- no_overlap_in_df
  } else if (which == "res") {
    no_overlap_assays <- no_overlap_in_res
  }

  return(no_overlap_assays)
}

data_prep <- function(df,
                      test_results,
                      check_log) {
  # clean up data from invalid entries ----

  nrow_df_original <- nrow(df)

  df <- clean_npx(df = df,
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
                  verbose = FALSE) |>
    suppressMessages() |>
    suppressWarnings()

  if (nrow(df) != nrow_df_original) {
    cli::cli_inform(
      "{cli::qty(nrow_df_original - nrow(df))} Removed
      {.val {nrow_df_original - nrow(df)}} entr{?y/ies} from {.arg df}
      containing invalid assay identifiers, control assays, and/or 'NA'
      assays. Run function {.fun clean_npx} to get details on removed entries."
    )
  }

  # remove non-overlapping assays between df and test_results ----

  no_overlap_assays <- helper_non_overlap_assays(
    df = df,
    test_results = test_results,
    check_log = check_log,
    which = "df"
  )

  if (length(no_overlap_assays) != 0L) {
    cli::cli_inform(
      "{.val {length(no_overlap_assays)}} assay{?s} in {.arg df} {?is/are} not
      represented in {.arg test_results} and will be removed from {.arg df}:
      {.val {no_overlap_assays}}"
    )

    df <- df |>
      dplyr::filter(
        !(.data[[check_log$col_names$olink_id]] %in%
            .env[["no_overlap_assays"]])
      )
  }

  # check for duplicated assays in df ----

  # this is used because all pathway annotations work with assay names, and not
  # olink assay identifiers.

  duplicated_assays <- df |>
    dplyr::select(
      dplyr::all_of(
        c(check_log$col_names$sample_id,
          check_log$col_names$assay)
      )
    )

  if (any(duplicated(duplicated_assays))) {
    duplicated_assays <- duplicated_assays |>
      dplyr::filter(
        duplicated(.env[["duplicated_assays"]])
      ) |>
      dplyr::pull(
        check_log$col_names$assay
      ) |>
      unique()

    cli::cli_abort(
      c(
        "x" = "{cli::qty(duplicated_assays)} Detected
        {.val {length(duplicated_assays)}} duplicated assay{?s} in {.arg df}:
        {.val {duplicated_assays}}!",
        "i" = "{cli::qty(duplicated_assays)} Filter {?it/them} out from
        {.arg df} and {.arg test_results}."
      ),
      call = rlang::caller_env(),
      wrap = TRUE
    )
  }

  return(df)
}

test_prep <- function(df,
                      test_results,
                      check_log) {

  # remove non-overlapping assays between test_results and df ----

  no_overlap_assays <- helper_non_overlap_assays(
    df = df,
    test_results = test_results,
    check_log = check_log,
    which = "res"
  )

  if (length(no_overlap_assays) != 0L) {
    cli::cli_inform(
      "{.val {length(no_overlap_assays)}} assay{?s} in {.arg test_results}
      {?is/are} not represented in {.arg df} and will be removed from
      {.arg test_results}: {.val {no_overlap_assays}}"
    )

    test_results <- test_results |>
      dplyr::filter(
        !(.data[["OlinkID"]] %in% .env[["no_overlap_assays"]])
      )
  }

  return(test_results)

}

select_ont <- function(ontology,
                       organism,
                       only_relevant = TRUE) {
  # Is Package installed
  rlang::check_installed(pkg = c("msigdbr"),
                         version = "24.1.0",
                         call = rlang::caller_env())

  # select MSigDB collection based on organism ----

  if (organism == "human") {
    msig_df <- msigdbr::msigdbr(
      species = "Homo sapiens",
      collection = "C2"
    ) |>
      dplyr::bind_rows(
        msigdbr::msigdbr(
          species = "Homo sapiens",
          collection = "C5"
        )
      )
  } else if (organism == "mouse") {
    msig_df <- msigdbr::msigdbr(
      species = "Mus musculus",
      collection = "C2"
    ) |>
      dplyr::bind_rows(
        msigdbr::msigdbr(
          species = "Mus musculus",
          collection = "C5"
        )
      )
  }

  # select annotation based on ontology ----

  ontology_msg <- ""

  if (ontology == "Reactome") {
    msig_df <- msig_df |>
      dplyr::filter(
        .data[["gs_subcollection"]] == "CP:REACTOME"
      )
    ontology_msg <- "Extracting Reactome Database from MSigDB..."
  } else if (ontology == "KEGG") {
    msig_df <- msig_df |>
      dplyr::filter(
        .data[["gs_subcollection"]] == "CP:KEGG_MEDICUS"
      )
    ontology_msg <- "Extracting KEGG Database from MSigDB..."
  } else if (ontology == "GO") {
    msig_df <- msig_df |>
      dplyr::filter(
        .data[["gs_subcollection"]] %in% c("GO:BP", "GO:CC", "GO:MF")
      )
    ontology_msg <- "Extracting GO Database from MSigDB..."
  } else if (ontology == "MSigDb_com") {
    msig_df <- msig_df |>
      dplyr::filter(
        stringr::str_detect(
          string = .data[["gs_subcollection"]],
          pattern = "KEGG",
          negate = TRUE
        )
      )
    ontology_msg <- "Using MSigDB without KEGG subcollections..."
  } else {
    ontology_msg <- "Using MSigDB..."
  }

  cli::cli_inform(ontology_msg)
  if (ontology == "KEGG") {
    cli::cli_alert_warning("KEGG is not approved for commercial use!")
  }

  # final cleanup ----

  if (only_relevant == TRUE) {
    msig_df <- msig_df |>
      dplyr::select(
        dplyr::any_of(
          c("gs_name", "gene_symbol")
        )
      )
  }

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
