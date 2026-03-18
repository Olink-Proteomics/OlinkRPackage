# Test check_pe_inputs ----

test_that(
  "check_pe_inputs - warning - non-overlapping OID df and test_results",
  {
    # Load reference results - skipped if files are absent
    reference_results <- get_example_data(filename = "reference_results.rds")

    expect_warning(
      object = check_pe_inputs(
        df = npx_data1,
        check_log = check_log,
        test_results = reference_results$t_test |>
          dplyr::filter(
            !(.data[["OlinkID"]] %in% head(x = npx_data1[["OlinkID"]], n = 1L))
          ),
        method = "GSEA",
        ontology = "MSigDb",
        organism = "human"
      ),
      regexp = "The sets of assays in `df` and `test_results` do not match!"
    )
  }
)

test_that(
  "check_pe_inputs - error - too many contrasts",
  {
    # Load reference results - skipped if files are absent
    reference_results <- get_example_data(filename = "reference_results.rds")

    expect_error(
      object = check_pe_inputs(
        df = npx_data1 |>
          dplyr::filter(
            .data[["OlinkID"]] %in% reference_results$anova_site_posthoc$OlinkID
          ),
        check_log = check_log,
        test_results = reference_results$anova_site_posthoc |>
          dplyr::filter(
            .data[["OlinkID"]] %in% npx_data1[["OlinkID"]]
          ),
        method = "GSEA",
        ontology = "MSigDb",
        organism = "human"
      ),
      regexp = "10 contrasts present in `test_results`!"
    )
  }
)

test_that(
  "check_pe_inputs - error - invalid method",
  {
    # Load reference results - skipped if files are absent
    reference_results <- get_example_data(filename = "reference_results.rds")

    expect_error(
      object = check_pe_inputs(
        df = npx_data1,
        check_log = check_log,
        test_results = reference_results$t_test,
        method = "IRA",
        ontology = "MSigDb",
        organism = "human"
      ),
      regex = "\"IRA\" is not a valid method for pathway enrichment!"
    )
  }
)

test_that(
  "check_pe_inputs - error - invalid ontology",
  {
    # Load reference results - skipped if files are absent
    reference_results <- get_example_data(filename = "reference_results.rds")

    expect_error(
      object = check_pe_inputs(
        df = npx_data1,
        check_log = check_log,
        test_results = reference_results$t_test,
        method = "GSEA",
        ontology = "WikiPathways",
        organism = "human"
      ),
      regex = "\"WikiPathways\" is not a valid ontology for pathway enrichment!"
    )
  }
)

test_that(
  "check_pe_inputs - error - invalid organism",
  {
    # Load reference results - skipped if files are absent
    reference_results <- get_example_data(filename = "reference_results.rds")

    expect_error(
      object =  olink_pathway_enrichment(
        df = npx_data1,
        check_log = check_log,
        test_results = reference_results$t_test,
        method = "GSEA",
        ontology = "MSigDb",
        organism = "rat"
      ),
      regexp = "\"rat\" is not a valid organism for pathway enrichment!"
    )
  }
)

# Test helper_non_overlap_assays ----

test_that(
  "helper_non_overlap_assays - works",
  {
    # Load reference results - skipped if files are absent
    reference_results <- get_example_data(filename = "reference_results.rds")

    all_assays <- npx_data1[["OlinkID"]] |> unique() |> sort()

    # all assays overlap - both ----

    expect_equal(
      object = helper_non_overlap_assays(
        df = npx_data1,
        test_results = reference_results$t_test,
        check_log = check_log,
        which = "both"
      ) |>
        sort(),
      expected = character(0L)
    )

    # all assays overlap - df ----

    expect_equal(
      object = helper_non_overlap_assays(
        df = npx_data1,
        test_results = reference_results$t_test,
        check_log = check_log,
        which = "df"
      ) |>
        sort(),
      expected = character(0L)
    )

    # all assays overlap - res ----

    expect_equal(
      object = helper_non_overlap_assays(
        df = npx_data1,
        test_results = reference_results$t_test,
        check_log = check_log,
        which = "res"
      ) |>
        sort(),
      expected = character(0L)
    )

    # assays only in df - both ----

    expect_equal(
      object = helper_non_overlap_assays(
        df = npx_data1,
        test_results = reference_results$t_test |>
          dplyr::filter(
            !(.data[["OlinkID"]] %in% head(x = all_assays, n = 2L))
          ),
        check_log = check_log,
        which = "both"
      ) |>
        sort(),
      expected = head(x = all_assays, n = 2L)
    )

    # assays only in df - df ----

    expect_equal(
      object = helper_non_overlap_assays(
        df = npx_data1,
        test_results = reference_results$t_test |>
          dplyr::filter(
            !(.data[["OlinkID"]] %in% head(x = all_assays, n = 2L))
          ),
        check_log = check_log,
        which = "df"
      ) |>
        sort(),
      expected = head(x = all_assays, n = 2L)
    )

    # assays only in df - res ----

    expect_equal(
      object = helper_non_overlap_assays(
        df = npx_data1,
        test_results = reference_results$t_test |>
          dplyr::filter(
            !(.data[["OlinkID"]] %in% head(x = all_assays, n = 2L))
          ),
        check_log = check_log,
        which = "res"
      ) |>
        sort(),
      expected = character(0L)
    )

    # assays only in test_results - both ----

    expect_equal(
      object = helper_non_overlap_assays(
        df = npx_data1 |>
          dplyr::filter(
            !(.data[["OlinkID"]] %in% head(x = all_assays, n = 2L))
          ),
        test_results = reference_results$t_test,
        check_log = check_log,
        which = "both"
      ) |>
        sort(),
      expected = head(x = all_assays, n = 2L)
    )

    # assays only in test_results - df ----

    expect_equal(
      object = helper_non_overlap_assays(
        df = npx_data1 |>
          dplyr::filter(
            !(.data[["OlinkID"]] %in% head(x = all_assays, n = 2L))
          ),
        test_results = reference_results$t_test,
        check_log = check_log,
        which = "df"
      ) |>
        sort(),
      expected = character(0L)
    )

    # assays only in test_results - both ----

    expect_equal(
      object = helper_non_overlap_assays(
        df = npx_data1 |>
          dplyr::filter(
            !(.data[["OlinkID"]] %in% head(x = all_assays, n = 2L))
          ),
        test_results = reference_results$t_test,
        check_log = check_log,
        which = "res"
      ) |>
        sort(),
      expected = head(x = all_assays, n = 2L)
    )
  }
)

# Test data_prep ----

test_that(
  "data_prep - works - remove invalid entries",
  {
    # Load reference results - skipped if files are absent
    reference_results <- get_example_data(filename = "reference_results.rds")

    npx_data_invalid <- npx_data1 |>
      dplyr::filter(
        !grepl(
          pattern = "control",
          x = .data[["SampleID"]],
          ignore.case = TRUE
        )
      ) |>
      dplyr::mutate(
        AssayType = dplyr::if_else(
          .data[["OlinkID"]] == "OID01216",
          "ext_ctrl",
          "assay"
        ),
        NPX = dplyr::if_else(
          .data[["OlinkID"]] == "OID01217",
          NA_real_,
          .data[["NPX"]]
        ),
        OlinkID = dplyr::if_else(
          .data[["OlinkID"]] == "OID01218",
          "OID01218A",
          .data[["OlinkID"]]
        )
      )

    expect_message(
      object = data_prep_out <- data_prep(
        df = npx_data_invalid,
        test_results = reference_results$t_test,
        check_log = check_npx(df = npx_data_invalid) |>
          suppressWarnings() |>
          suppressMessages()
      ),
      regexp = paste("Removed 468 entries from `df` containing invalid assay",
                     "identifiers, control assays, and/or 'NA' assays. Run",
                     "function `clean_npx()` to get details on removed",
                     "entries."),
      fixed = TRUE
    )

    expect_identical(
      object = dim(data_prep_out),
      expected = c(28236L, 18L)
    )
  }
)

test_that(
  "data_prep - works - remove non-overlapping assays",
  {
    # Load reference results - skipped if files are absent
    reference_results <- get_example_data(filename = "reference_results.rds")

    exclude_assays <- npx_data1[["OlinkID"]] |> unique() |> head(n = 5L)

    expect_message(
      object = data_prep_out <- data_prep(
        df = npx_data1 |>
          dplyr::filter(
            !grepl(pattern = "control",
                   x = .data[["SampleID"]],
                   ignore.case = TRUE)
          ),
        test_results = reference_results$t_test |>
          dplyr::filter(
            !(.data[["OlinkID"]] %in% .env[["exclude_assays"]])
          ),
        check_log = check_log
      ),
      regexp = paste("5 assays in `df` are not represented in `test_results`",
                     "and will be removed from `df`: \"OID01216\",",
                     "\"OID01217\", \"OID01218\", \"OID01219\", and",
                     "\"OID01220\""),
      fixed = TRUE
    )

    expect_identical(
      object = dim(data_prep_out),
      expected = c(27924L, 17L)
    )
  }
)

test_that(
  "data_prep - error - duplicates assays for same sample",
  {
    # Load reference results - skipped if files are absent
    reference_results <- get_example_data(filename = "reference_results.rds")

    expect_error(
      object = data_prep(
        df = npx_data1,
        test_results = reference_results$t_test,
        check_log = check_log
      ),
      regexp = "Detected 184 duplicated assays in `df`: \"CHL1\", \"NRP1\"",
    )
  }
)

test_that(
  "data_prep - error - duplicates assays for same sample v2",
  {
    # Load reference results - skipped if files are absent
    reference_results <- get_example_data(filename = "reference_results.rds")

    duplicate_assay_data <- npx_data1 |>
      dplyr::filter(
        !grepl(
          pattern = "control",
          x = .data[["SampleID"]],
          ignore.case = TRUE
        )
      ) |>
      dplyr::filter(
        .data[["Assay"]] == "MET"
      ) |>
      dplyr::mutate(
        OlinkID = "OID01254",
        LOD = .data[["LOD"]] + 1L
      )

    duplicated_assay_data <- npx_data1 |>
      dplyr::filter(
        !grepl(
          pattern = "control",
          x = .data[["SampleID"]],
          ignore.case = TRUE
        )
      ) |>
      dplyr::bind_rows(
        duplicate_assay_data
      )

    expect_error(
      object = data_prep(
        df = duplicated_assay_data,
        test_results = reference_results$t_test,
        check_log = check_npx(df = duplicated_assay_data) |>
          suppressWarnings() |>
          suppressMessages()
      ),
      regexp = "Detected 1 duplicated assay in `df`: \"MET\"!"
    )
  }
)

# Test test_prep ----

test_that(
  "test_prep - works - remove non-overlapping assays",
  {
    # Load reference results - skipped if files are absent
    reference_results <- get_example_data(filename = "reference_results.rds")

    exclude_assays <- npx_data1[["OlinkID"]] |> unique() |> head(n = 5L)

    expect_message(
      object = test_prep_out <- test_prep(
        df = npx_data1 |>
          dplyr::filter(
            !grepl(pattern = "control",
                   x = .data[["SampleID"]],
                   ignore.case = TRUE)
          ) |>
          dplyr::filter(
            !(.data[["OlinkID"]] %in% .env[["exclude_assays"]])
          ),
        test_results = reference_results$t_test,
        check_log = check_log
      ),
      regexp = paste("5 assays in `test_results` are not represented in `df`",
                     "and will be removed from `test_results`: \"OID01220\",",
                     "\"OID01217\", \"OID01216\", \"OID01219\", and",
                     "\"OID01218\""),
      fixed = TRUE
    )

    expect_identical(
      object = dim(test_prep_out),
      expected = c(179L, 16L)
    )
  }
)

# Test select_ont ----

test_that(
  "test_prep works",
  {
    skip_on_cran()
    skip_if_not_installed("msigdbr", minimum_version = "24.1.0")

    # human & MSigDb ----

    expect_message(
      object = ont_hs_msigdb <- select_ont(
        ontology = "MSigDb",
        organism = "human",
        only_relevant = FALSE
      ),
      regexp = "Using MSigDB..."
    )

    expect_identical(
      object = ont_hs_msigdb[["gs_collection"]] |> unique() |> sort(),
      expected = c("C2", "C5")
    )

    expect_identical(
      object = ont_hs_msigdb[["gs_subcollection"]] |> unique() |> sort(),
      expected = c("CGP", "CP", "CP:BIOCARTA", "CP:KEGG_LEGACY",
                   "CP:KEGG_MEDICUS", "CP:PID", "CP:REACTOME",
                   "CP:WIKIPATHWAYS", "GO:BP", "GO:CC", "GO:MF", "HPO")
    )

    expect_identical(
      object = ont_hs_msigdb[["db_target_species"]] |> unique() |> sort(),
      expected = "HS"
    )

    # human & MSigDb_com ----

    expect_message(
      object = ont_hs_msigdbcom <- select_ont(
        ontology = "MSigDb_com",
        organism = "human",
        only_relevant = FALSE
      ),
      regexp = "Using MSigDB without KEGG subcollections..."
    )

    expect_identical(
      object = ont_hs_msigdbcom[["gs_collection"]] |> unique() |> sort(),
      expected = c("C2", "C5")
    )

    expect_identical(
      object = ont_hs_msigdbcom[["gs_subcollection"]] |> unique() |> sort(),
      expected = c("CGP", "CP", "CP:BIOCARTA", "CP:PID", "CP:REACTOME",
                   "CP:WIKIPATHWAYS", "GO:BP", "GO:CC", "GO:MF", "HPO")
    )

    expect_identical(
      object = ont_hs_msigdbcom[["db_target_species"]] |> unique() |> sort(),
      expected = "HS"
    )

    # human & KEGG ----

    expect_message(
      object = expect_message(
        object = ont_hs_kegg <- select_ont(
          ontology = "KEGG",
          organism = "human",
          only_relevant = FALSE
        ),
        regexp = "Extracting KEGG Database from MSigDB..."
      ),
      regexp = "KEGG is not approved for commercial use!"
    )

    expect_identical(
      object = ont_hs_kegg[["gs_collection"]] |> unique() |> sort(),
      expected = "C2"
    )

    expect_identical(
      object = ont_hs_kegg[["gs_subcollection"]] |> unique() |> sort(),
      expected = "CP:KEGG_MEDICUS"
    )

    expect_identical(
      object = ont_hs_kegg[["db_target_species"]] |> unique() |> sort(),
      expected = "HS"
    )

    # human & GO ----

    expect_message(
      object = ont_hs_go <- select_ont(
        ontology = "GO",
        organism = "human",
        only_relevant = FALSE
      ),
      regexp = "Extracting GO Database from MSigDB..."
    )

    expect_identical(
      object = ont_hs_go[["gs_collection"]] |> unique() |> sort(),
      expected = "C5"
    )

    expect_identical(
      object = ont_hs_go[["gs_subcollection"]] |> unique() |> sort(),
      expected = c("GO:BP", "GO:CC", "GO:MF")
    )

    expect_identical(
      object = ont_hs_go[["db_target_species"]] |> unique() |> sort(),
      expected = "HS"
    )

    # human & Reactome ----

    expect_message(
      object = ont_hs_reactome <- select_ont(
        ontology = "Reactome",
        organism = "human",
        only_relevant = FALSE
      ),
      regexp = "Extracting Reactome Database from MSigDB..."
    )

    expect_identical(
      object = ont_hs_reactome[["gs_collection"]] |> unique() |> sort(),
      expected = "C2"
    )

    expect_identical(
      object = ont_hs_reactome[["gs_subcollection"]] |> unique() |> sort(),
      expected = "CP:REACTOME"
    )

    expect_identical(
      object = ont_hs_reactome[["db_target_species"]] |> unique() |> sort(),
      expected = "HS"
    )

    # mouse warmup ----

    # we run this because "msigdbr::msigdbr" prints a startup message when run
    # with the current setting for mouse. We want to ensure that this message is
    # printed when we call select_ont with mouse and not when we call
    # msigdbr::msigdbr directly, so we run it here to "use up" the startup
    # message for mouse before we run the tests for select_ont.

    msigdbr::msigdbr(
      species = "Mus musculus",
      collection = "C2"
    ) |>
      suppressMessages() |>
      suppressWarnings() |>
      suppressPackageStartupMessages()

    # mouse & MSigDb ----

    expect_message(
      object = ont_mm_msigdb <- select_ont(
        ontology = "MSigDb",
        organism = "mouse",
        only_relevant = FALSE
      ),
      regexp = "Using MSigDB..."
    )

    expect_identical(
      object = ont_mm_msigdb[["gs_collection"]] |> unique() |> sort(),
      expected = c("C2", "C5")
    )

    expect_identical(
      object = ont_mm_msigdb[["gs_subcollection"]] |> unique() |> sort(),
      expected = c("CGP", "CP", "CP:BIOCARTA", "CP:KEGG_LEGACY",
                   "CP:KEGG_MEDICUS", "CP:PID", "CP:REACTOME",
                   "CP:WIKIPATHWAYS", "GO:BP", "GO:CC", "GO:MF", "HPO")
    )

    expect_identical(
      object = ont_mm_msigdb[["db_target_species"]] |> unique() |> sort(),
      expected = "HS"
    )

    # mouse & MSigDb_com ----

    expect_message(
      object = ont_mm_msigdbcom <- select_ont(
        ontology = "MSigDb_com",
        organism = "mouse",
        only_relevant = FALSE
      ),
      regexp = "Using MSigDB without KEGG subcollections..."
    )

    expect_identical(
      object = ont_mm_msigdbcom[["gs_collection"]] |> unique() |> sort(),
      expected = c("C2", "C5")
    )

    expect_identical(
      object = ont_mm_msigdbcom[["gs_subcollection"]] |> unique() |> sort(),
      expected = c("CGP", "CP", "CP:BIOCARTA", "CP:PID", "CP:REACTOME",
                   "CP:WIKIPATHWAYS", "GO:BP", "GO:CC", "GO:MF", "HPO")
    )

    expect_identical(
      object = ont_mm_msigdbcom[["db_target_species"]] |> unique() |> sort(),
      expected = "HS"
    )

    # mouse & KEGG ----

    expect_message(
      object = expect_message(
        object = ont_mm_kegg <- select_ont(
          ontology = "KEGG",
          organism = "mouse",
          only_relevant = FALSE
        ),
        regexp = "Extracting KEGG Database from MSigDB..."
      ),
      regexp = "KEGG is not approved for commercial use!"
    )

    expect_identical(
      object = ont_mm_kegg[["gs_collection"]] |> unique() |> sort(),
      expected = "C2"
    )

    expect_identical(
      object = ont_mm_kegg[["gs_subcollection"]] |> unique() |> sort(),
      expected = "CP:KEGG_MEDICUS"
    )

    expect_identical(
      object = ont_mm_kegg[["db_target_species"]] |> unique() |> sort(),
      expected = "HS"
    )

    # mouse & GO ----

    expect_message(
      object = ont_mm_go <- select_ont(
        ontology = "GO",
        organism = "mouse",
        only_relevant = FALSE
      ),
      regexp = "Extracting GO Database from MSigDB..."
    )

    expect_identical(
      object = ont_mm_go[["gs_collection"]] |> unique() |> sort(),
      expected = "C5"
    )

    expect_identical(
      object = ont_mm_go[["gs_subcollection"]] |> unique() |> sort(),
      expected = c("GO:BP", "GO:CC", "GO:MF")
    )

    expect_identical(
      object = ont_mm_go[["db_target_species"]] |> unique() |> sort(),
      expected = "HS"
    )

    # mouse & Reactome ----

    expect_message(
      object = ont_mm_reactome <- select_ont(
        ontology = "Reactome",
        organism = "mouse",
        only_relevant = FALSE
      ),
      regexp = "Extracting Reactome Database from MSigDB..."
    )

    expect_identical(
      object = ont_mm_reactome[["gs_collection"]] |> unique() |> sort(),
      expected = "C2"
    )

    expect_identical(
      object = ont_mm_reactome[["gs_subcollection"]] |> unique() |> sort(),
      expected = "CP:REACTOME"
    )

    expect_identical(
      object = ont_mm_reactome[["db_target_species"]] |> unique() |> sort(),
      expected = "HS"
    )
  }
)

# Test results_to_genelist ----

test_that(
  "results_to_genelist - works",
  {
    # Load reference results - skipped if files are absent
    reference_results <- get_example_data(filename = "reference_results.rds")

    # try with ttest_results ----

    expect_identical(
      object = results_to_genelist(
        test_results = reference_results$t_test
      ),
      expected = setNames(
        object = reference_results$t_test |>
          dplyr::arrange(
            dplyr::desc(.data[["estimate"]])
          ) |>
          dplyr::pull(.data[["estimate"]]),
        nm = reference_results$t_test |>
          dplyr::arrange(
            dplyr::desc(.data[["estimate"]])
          ) |>
          dplyr::pull(.data[["Assay"]])
      )
    )

    # try with a custom randomly generated test_results ----

    custom_test_results <- dplyr::tibble(
      Assay = paste0("OID", sprintf("%05d", 1:100)),
      estimate = rnorm(100)
    )

    expect_identical(
      object = results_to_genelist(test_results = custom_test_results),
      expected = setNames(
        object = custom_test_results |>
          dplyr::arrange(
            dplyr::desc(.data[["estimate"]])
          ) |>
          dplyr::pull(.data[["estimate"]]),
        nm = custom_test_results |>
          dplyr::arrange(
            dplyr::desc(.data[["estimate"]])
          ) |>
          dplyr::pull(.data[["Assay"]])
      )
    )

  }
)
