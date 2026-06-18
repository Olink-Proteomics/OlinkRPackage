check_log <- check_npx(df = npx_data1) |>
  suppressWarnings() |>
  suppressMessages()

# Test olink_pathway_enrichment ----

test_that(
  "olink_pathway_enrichment - error - missing args",
  {
    skip_on_cran()
    suppressMessages(skip_if_not_installed("clusterProfiler"))
    skip_if_not_installed("msigdbr", minimum_version = "24.1.0")

    expect_error(
      object = olink_pathway_enrichment(),
      regexp = "Arguments `df` and `test_results` are required!"
    )

    expect_error(
      object = olink_pathway_enrichment(
        df = npx_data1
      ),
      regexp = "Arguments `df` and `test_results` are required!"
    )

    expect_error(
      object = olink_pathway_enrichment(
        test_results = ttest_results
      ),
      regexp = "Arguments `df` and `test_results` are required!"
    )
  }
)

## GSEA ----

test_that(
  "olink_pathway_enrichment - works - t-test GSEA",
  {
    # Load reference results - skipped if files are absent
    reference_results <- get_example_data(filename = "reference_results.rds")

    skip_on_cran()
    suppressMessages(skip_if_not_installed("clusterProfiler"))
    skip_if_not_installed("msigdbr", minimum_version = "24.1.0")

    skip_if_not(file.exists(test_path("data", "msidbr_v26.1.0_hs.parquet")))
    skip_if_not(file.exists(test_path("data", "msidbr_v26.1.0_mm.parquet")))

    expect_message(
      object = expect_message(
        object = expect_message(
          object = expect_message(
            object = tt_gsea <- olink_pathway_enrichment(
              df = npx_data1 |>
                dplyr::filter(
                  !grepl(pattern = "control",
                         x = .data[["SampleID"]],
                         ignore.case = TRUE)
                ),
              check_log = check_log,
              test_results = reference_results$t_test,
              method = "TEST#GSEA",
              ontology = "TEST#MSigDb",
              organism = "TEST#human"
            ),
            regexp = paste("Test mode activated: using fixed version of MSigDB",
                           "for human")
          ),
          regexp = "Using MSigDB..."
        ),
        regexp = "45 assays are not found in the database"
      ),
      regexp = "Gene set enrichment analysis used by default"
    )

    expect_identical(
      object = dim(tt_gsea),
      expected = c(6992L, 12L)
    )

  }
)

test_that(
  "olink_pathway_enrichment - works - t-test GSEA, NA",
  {
    # Load reference results - skipped if files are absent
    npx_data_format22 <- get_example_data("npx_data_format-Oct-2022.rds")

    skip_on_cran()
    suppressMessages(skip_if_not_installed("clusterProfiler"))
    skip_if_not_installed("msigdbr", minimum_version = "24.1.0")

    skip_if_not(file.exists(test_path("data", "msidbr_v26.1.0_hs.parquet")))
    skip_if_not(file.exists(test_path("data", "msidbr_v26.1.0_mm.parquet")))

    # prepare data

    ttest_na <- olink_ttest(
      df = npx_data_format22,
      check_log = check_npx(npx_data_format22) |>
        suppressWarnings() |>
        suppressMessages(),
      variable = "treatment1"
    ) |>
      suppressMessages() |>
      suppressWarnings()

    npx_data_format22 <- npx_data_format22 |>
      dplyr::mutate(
        OlinkID = dplyr::if_else(.data[["OlinkID"]] == "OID30646",
                                 "OID12345",
                                 .data[["OlinkID"]])
      )

    # run pe

    expect_message(
      object = expect_message(
        object = expect_message(
          object = expect_message(
            object = expect_message(
              object = expect_message(
                object = expect_message(
                  object = expect_warning(
                    object = tt_gsea_na <- olink_pathway_enrichment(
                      df = npx_data_format22,
                      test_results = ttest_na,
                      check_log = check_npx(npx_data_format22) |>
                        suppressWarnings() |>
                        suppressMessages(),
                      method = "TEST#GSEA",
                      ontology = "TEST#MSigDb",
                      organism = "TEST#human"
                    ),
                    regexp = paste("The sets of assays in `df` and",
                                   "`test_results` do not match!")
                  ),
                  regexp = paste("1530 entries removed by `clean_npx()` from",
                                 "the input dataset `df`. Run `clean_npx()` on",
                                 "your dataset with `verbose = TRUE` to",
                                 "inspect which rows were removed."),
                  fixed = TRUE
                ),
                regexp = paste("1 assay in `df` is not represented in",
                               "`test_results` and will be removed from",
                               "`df`: \"OID12345\"")
              ),
              regexp = paste("1 assay in `test_results` is not represented in",
                             "`df` and will be removed from `test_results`:",
                             "\"OID30646\"")
            ),
            regexp = paste("Test mode activated: using fixed version of MSigDB",
                           "for human")
          ),
          regexp = "Using MSigDB..."
        ),
        regexp = paste("6 assays are not found in the database. Please check",
                       "the names for the following assays in `test_results`",
                       "and `df`: \"BAP18\", \"Incubation control 1\",",
                       "\"SPACA5_SPACA5B\", \"Amplification control 4\",",
                       "\"AMY1A_AMY1B_AMY1C\", and",
                       "\"Amplification control 2\"."),
        fixed = TRUE
      ),
      regexp = "Gene set enrichment analysis used by default"
    )

    expect_identical(
      object = dim(tt_gsea_na),
      expected = c(4626L, 12L)
    )

  }
)

test_that(
  "olink_pathway_enrichment - works - Reactome GSEA",
  {
    # Load reference results - skipped if files are absent
    reference_results <- get_example_data(filename = "reference_results.rds")

    skip_on_cran()
    suppressMessages(skip_if_not_installed("clusterProfiler"))
    skip_if_not_installed("msigdbr", minimum_version = "24.1.0")

    skip_if_not(file.exists(test_path("data", "msidbr_v26.1.0_hs.parquet")))
    skip_if_not(file.exists(test_path("data", "msidbr_v26.1.0_mm.parquet")))

    expect_message(
      object = expect_message(
        object = expect_message(
          object = expect_message(
            object = tt_gsea_reactome <- olink_pathway_enrichment(
              df = npx_data1 |>
                dplyr::filter(
                  !grepl(pattern = "control",
                         x = .data[["SampleID"]],
                         ignore.case = TRUE)
                ),
              check_log = check_log,
              test_results = reference_results$t_test,
              ontology = "TEST#Reactome",
              method = "TEST#GSEA",
              organism = "TEST#human"
            ),
            regexp = paste("Test mode activated: using fixed version of MSigDB",
                           "for human")
          ),
          regexp = "Extracting Reactome Database from MSigDB..."
        ),
        regexp = paste("65 assays are not found in the database. Please check",
                       "the names for the following assays in `test_results`",
                       "and `df`")
      ),
      regexp = "Gene set enrichment analysis used by default"
    )

    expect_equal(
      object = dim(tt_gsea_reactome),
      expected = c(320L, 12L)
    )
  }
)

test_that(
  "olink_pathway_enrichment - works - MSigDB_com GSEA",
  {
    # Load reference results - skipped if files are absent
    reference_results <- get_example_data(filename = "reference_results.rds")

    skip_on_cran()
    suppressMessages(skip_if_not_installed("clusterProfiler"))
    skip_if_not_installed("msigdbr", minimum_version = "24.1.0")

    skip_if_not(file.exists(test_path("data", "msidbr_v26.1.0_hs.parquet")))
    skip_if_not(file.exists(test_path("data", "msidbr_v26.1.0_mm.parquet")))

    expect_message(
      object = expect_message(
        object = expect_message(
          object = expect_message(
            object = tt_gsea_msigdb_com <- olink_pathway_enrichment(
              df = npx_data1 |>
                dplyr::filter(
                  !grepl(pattern = "control",
                         x = .data[["SampleID"]],
                         ignore.case = TRUE)
                ),
              check_log = check_log,
              test_results = reference_results$t_test,
              ontology = "TEST#MSigDb_com",
              method = "TEST#GSEA",
              organism = "TEST#human"
            ),
            regexp = paste("Test mode activated: using fixed version of MSigDB",
                           "for human")
          ),
          regexp = "Using MSigDB without KEGG subcollections..."
        ),
        regexp = paste("45 assays are not found in the database. Please check",
                       "the names for the following assays in `test_results`",
                       "and `df`")
      ),
      regexp = "Gene set enrichment analysis used by default"
    )

    expect_equal(
      object = dim(tt_gsea_msigdb_com),
      expected = c(6862L, 12L)
    )
  }
)

test_that(
  "olink_pathway_enrichment - works - KEGG GSEA",
  {
    # Load reference results - skipped if files are absent
    reference_results <- get_example_data(filename = "reference_results.rds")

    skip_on_cran()
    suppressMessages(skip_if_not_installed("clusterProfiler"))
    skip_if_not_installed("msigdbr", minimum_version = "24.1.0")

    skip_if_not(file.exists(test_path("data", "msidbr_v26.1.0_hs.parquet")))
    skip_if_not(file.exists(test_path("data", "msidbr_v26.1.0_mm.parquet")))

    expect_message(
      object = expect_message(
        object = expect_message(
          object = expect_message(
            object = expect_message(
              object = tt_gsea_kegg <- olink_pathway_enrichment(
                df = npx_data1 |>
                  dplyr::filter(
                    !grepl(pattern = "control",
                           x = .data[["SampleID"]],
                           ignore.case = TRUE)
                  ),
                check_log = check_log,
                test_results = reference_results$t_test,
                ontology = "TEST#KEGG",
                method = "TEST#GSEA",
                organism = "TEST#human"
              ),
              regexp = paste("Test mode activated: using fixed version of",
                             "MSigDB for human")
            ),
            regexp = "Extracting KEGG Database from MSigDB..."
          ),
          regexp = "KEGG is not approved for commercial use!"
        ),
        regexp = paste("138 assays are not found in the database. Please check",
                       "the names for the following assays in `test_results`",
                       "and `df`")
      ),
      regexp = "Gene set enrichment analysis used by default"
    )

    expect_equal(
      object = dim(tt_gsea_kegg),
      expected = c(53L, 12L)
    )
  }
)

test_that(
  "olink_pathway_enrichment - works - GO GSEA",
  {
    # Load reference results - skipped if files are absent
    reference_results <- get_example_data(filename = "reference_results.rds")

    skip_on_cran()
    suppressMessages(skip_if_not_installed("clusterProfiler"))
    skip_if_not_installed("msigdbr", minimum_version = "24.1.0")

    skip_if_not(file.exists(test_path("data", "msidbr_v26.1.0_hs.parquet")))
    skip_if_not(file.exists(test_path("data", "msidbr_v26.1.0_mm.parquet")))

    expect_message(
      object = expect_message(
        object = expect_message(
          object = expect_message(
            object = tt_gsea_go <- olink_pathway_enrichment(
              df = npx_data1 |>
                dplyr::filter(
                  !grepl(pattern = "control",
                         x = .data[["SampleID"]],
                         ignore.case = TRUE)
                ),
              check_log = check_log,
              test_results = reference_results$t_test,
              ontology = "TEST#GO",
              method = "TEST#GSEA",
              organism = "TEST#human"
            ),
            regexp = paste("Test mode activated: using fixed version of MSigDB",
                           "for human")
          ),
          regexp = "Extracting GO Database from MSigDB..."
        ),
        regexp = paste("45 assays are not found in the database. Please check",
                       "the names for the following assays in `test_results`",
                       "and `df`")
      ),
      regexp = "Gene set enrichment analysis used by default"
    )

    expect_equal(
      object = dim(tt_gsea_go),
      expected = c(2946L, 12L)
    )
  }
)

## ORA ----

test_that(
  "olink_pathway_enrichment - works - t-test ORA",
  {
    # Load reference results - skipped if files are absent
    reference_results <- get_example_data(filename = "reference_results.rds")

    skip_on_cran()
    suppressMessages(skip_if_not_installed("clusterProfiler"))
    skip_if_not_installed("msigdbr", minimum_version = "24.1.0")

    skip_if_not(file.exists(test_path("data", "msidbr_v26.1.0_hs.parquet")))
    skip_if_not(file.exists(test_path("data", "msidbr_v26.1.0_mm.parquet")))

    expect_message(
      object = expect_message(
        object = expect_message(
          object = expect_message(
            object = tt_ora <- olink_pathway_enrichment(
              df = npx_data1 |>
                dplyr::filter(
                  !grepl(pattern = "control",
                         x = .data[["SampleID"]],
                         ignore.case = TRUE)
                ),
              check_log = check_log,
              test_results = reference_results$t_test,
              method = "TEST#ORA",
              ontology = "TEST#MSigDb",
              organism = "TEST#human"
            ),
            regexp = paste("Test mode activated: using fixed version of MSigDB",
                           "for human")
          ),
          regexp = "Using MSigDB..."
        ),
        regexp = paste("45 assays are not found in the database. Please check",
                       "the names for the following assays in `test_results`",
                       "and `df`")
      ),
      regexp = "Over-representation analysis performed"
    )

    expect_equal(
      object = dim(tt_ora),
      expected = c(573L, 12L)
    )
  }
)

test_that(
  "olink_pathway_enrichment - works - Reactome ORA",
  {
    # Load reference results - skipped if files are absent
    reference_results <- get_example_data(filename = "reference_results.rds")

    skip_on_cran()
    suppressMessages(skip_if_not_installed("clusterProfiler"))
    skip_if_not_installed("msigdbr", minimum_version = "24.1.0")

    skip_if_not(file.exists(test_path("data", "msidbr_v26.1.0_hs.parquet")))
    skip_if_not(file.exists(test_path("data", "msidbr_v26.1.0_mm.parquet")))

    expect_message(
      object = expect_message(
        object = expect_message(
          object = expect_message(
            object = tt_ora_reactome <- olink_pathway_enrichment(
              df = npx_data1 |>
                dplyr::filter(
                  !grepl(pattern = "control",
                         x = .data[["SampleID"]],
                         ignore.case = TRUE)
                ),
              check_log = check_log,
              test_results = reference_results$t_test,
              method = "TEST#ORA",
              ontology = "TEST#Reactome",
              organism = "TEST#human"
            ),
            regexp = paste("Test mode activated: using fixed version of MSigDB",
                           "for human")
          ),
          regexp = "Extracting Reactome Database from MSigDB..."
        ),
        regexp = paste("65 assays are not found in the database. Please check",
                       "the names for the following assays in `test_results`",
                       "and `df`")
      ),
      regexp = "Over-representation analysis performed"
    )

    expect_equal(
      object = dim(tt_ora_reactome),
      expected = c(20L, 12L)
    )
  }
)

test_that(
  "olink_pathway_enrichment - works - KEGG ORA",
  {
    # Load reference results - skipped if files are absent
    reference_results <- get_example_data(filename = "reference_results.rds")

    skip_on_cran()
    suppressMessages(skip_if_not_installed("clusterProfiler"))
    skip_if_not_installed("msigdbr", minimum_version = "24.1.0")

    skip_if_not(file.exists(test_path("data", "msidbr_v26.1.0_hs.parquet")))
    skip_if_not(file.exists(test_path("data", "msidbr_v26.1.0_mm.parquet")))

    expect_warning(
      object = expect_message(
        object = expect_message(
          object = expect_message(
            object = expect_message(
              object = expect_message(
                object = tt_ora_kegg <- olink_pathway_enrichment(
                  df = npx_data1 |>
                    dplyr::filter(
                      !grepl(pattern = "control",
                             x = .data[["SampleID"]],
                             ignore.case = TRUE)
                    ),
                  check_log = check_log,
                  test_results = reference_results$t_test,
                  method = "TEST#ORA",
                  ontology = "TEST#KEGG",
                  organism = "TEST#human"
                ),
                regexp = paste("Test mode activated: using fixed version of",
                               "MSigDB for human")
              ),
              regexp = "Extracting KEGG Database from MSigDB..."
            ),
            regexp = "KEGG is not approved for commercial use!"
          ),
          regexp = paste("138 assays are not found in the database. Please",
                         "check the names for the following assays in",
                         "`test_results` and `df`")
        ),
        regexp = "Over-representation analysis performed"
      ),
      regexp = "No remaining pathways within the range 10-500 proteins!"
    )

    expect_true(
      object = is.null(tt_ora_kegg)
    )
  }
)

test_that(
  "olink_pathway_enrichment - works - GO ORA",
  {
    # Load reference results - skipped if files are absent
    reference_results <- get_example_data(filename = "reference_results.rds")

    skip_on_cran()
    suppressMessages(skip_if_not_installed("clusterProfiler"))
    skip_if_not_installed("msigdbr", minimum_version = "24.1.0")

    skip_if_not(file.exists(test_path("data", "msidbr_v26.1.0_hs.parquet")))
    skip_if_not(file.exists(test_path("data", "msidbr_v26.1.0_mm.parquet")))

    expect_message(
      object = expect_message(
        object = expect_message(
          object = expect_message(
            object = tt_ora_go <- olink_pathway_enrichment(
              df = npx_data1 |>
                dplyr::filter(
                  !grepl(pattern = "control",
                         x = .data[["SampleID"]],
                         ignore.case = TRUE)
                ),
              check_log = check_log,
              test_results = reference_results$t_test,
              method = "TEST#ORA",
              ontology = "TEST#GO",
              organism = "TEST#human"
            ),
            regexp = paste("Test mode activated: using fixed version of MSigDB",
                           "for human")
          ),
          regexp = "Extracting GO Database from MSigDB..."
        ),
        regexp = paste("45 assays are not found in the database. Please check",
                       "the names for the following assays in `test_results`",
                       "and `df`")
      ),
      regexp = "Over-representation analysis performed"
    )

    expect_equal(
      object = dim(tt_ora_go),
      expected = c(356L, 12L)
    )
  }
)

# Test check_test_mode ----

test_that(
  "check_test_mode - works",
  {
    # test_mode is FALSE ----

    expect_identical(
      object = check_test_mode(
        method = "GSEA",
        ontology = "MSigDb",
        organism = "human"
      ),
      expected = list(
        method = "GSEA",
        ontology = "MSigDb",
        organism = "human",
        test_mode = FALSE
      )
    )

    expect_identical(
      object = check_test_mode(
        method = "ORA",
        ontology = "Reactome",
        organism = "mouse"
      ),
      expected = list(
        method = "ORA",
        ontology = "Reactome",
        organism = "mouse",
        test_mode = FALSE
      )
    )

    expect_identical(
      object = check_test_mode(
        method = "A",
        ontology = "B",
        organism = "C"
      ),
      expected = list(
        method = "A",
        ontology = "B",
        organism = "C",
        test_mode = FALSE
      )
    )

    # test_mode is TRUE ----

    expect_identical(
      object = check_test_mode(
        method = "TEST#GSEA",
        ontology = "TEST#MSigDb",
        organism = "TEST#human"
      ),
      expected = list(
        method = "GSEA",
        ontology = "MSigDb",
        organism = "human",
        test_mode = TRUE
      )
    )

    expect_identical(
      object = check_test_mode(
        method = "TEST#ORA",
        ontology = "TEST#Reactome",
        organism = "TEST#mouse"
      ),
      expected = list(
        method = "ORA",
        ontology = "Reactome",
        organism = "mouse",
        test_mode = TRUE
      )
    )

    expect_identical(
      object = check_test_mode(
        method = "TEST#A",
        ontology = "TEST#B",
        organism = "TEST#C"
      ),
      expected = list(
        method = "A",
        ontology = "B",
        organism = "C",
        test_mode = TRUE
      )
    )
  }
)

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
      regexp = paste("468 entries removed by `clean_npx()` from the",
                     "input dataset `df`. Run `clean_npx()` on your",
                     "dataset with `verbose = TRUE` to inspect which",
                     "rows were removed."),
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
  "select_ont - works",
  {
    skip_on_cran()
    skip_if_not_installed("msigdbr", minimum_version = "24.1.0")
    skip_if_not(file.exists(test_path("data", "msidbr_v26.1.0_hs.parquet")))
    skip_if_not(file.exists(test_path("data", "msidbr_v26.1.0_mm.parquet")))

    # human & MSigDb in test mode ----

    expect_message(
      object = expect_message(
        object = ont_hs_msigdb_test <- select_ont(
          ontology = "MSigDb",
          organism = "human",
          test_mode = TRUE,
          only_relevant = FALSE
        ),
        regexp = "Using MSigDB..."
      ),
      regexp = paste("Test mode activated: using fixed version of MSigDB",
                     "for human")
    )

    expect_identical(
      object = ont_hs_msigdb_test[["gs_collection"]] |> unique() |> sort(),
      expected = c("C2", "C5")
    )

    expect_identical(
      object = ont_hs_msigdb_test[["gs_subcollection"]] |> unique() |> sort(),
      expected = c("CGP", "CP", "CP:BIOCARTA", "CP:KEGG_LEGACY",
                   "CP:KEGG_MEDICUS", "CP:PID", "CP:REACTOME",
                   "CP:WIKIPATHWAYS", "GO:BP", "GO:CC", "GO:MF", "HPO")
    )

    expect_identical(
      object = ont_hs_msigdb_test[["db_target_species"]] |> unique() |> sort(),
      expected = "HS"
    )

    # human & MSigDb ----

    expect_message(
      object = ont_hs_msigdb <- select_ont(
        ontology = "MSigDb",
        organism = "human",
        test_mode = FALSE,
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
        test_mode = FALSE,
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
          test_mode = FALSE,
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
        test_mode = FALSE,
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
        test_mode = FALSE,
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

    # mouse & MSigDB test mode ----

    expect_message(
      object = expect_message(
        object = ont_mm_msigdb_test <- select_ont(
          ontology = "MSigDb",
          organism = "mouse",
          test_mode = TRUE,
          only_relevant = FALSE
        ),
        regexp = "Using MSigDB..."
      ),
      regexp = paste("Test mode activated: using fixed version of MSigDB",
                     "for mouse")
    )

    expect_identical(
      object = ont_mm_msigdb_test[["gs_collection"]] |> unique() |> sort(),
      expected = c("C2", "C5")
    )

    expect_identical(
      object = ont_mm_msigdb_test[["gs_subcollection"]] |> unique() |> sort(),
      expected = c("CGP", "CP", "CP:BIOCARTA", "CP:KEGG_LEGACY",
                   "CP:KEGG_MEDICUS", "CP:PID", "CP:REACTOME",
                   "CP:WIKIPATHWAYS", "GO:BP", "GO:CC", "GO:MF", "HPO")
    )

    expect_identical(
      object = ont_mm_msigdb_test[["db_target_species"]] |> unique() |> sort(),
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
        test_mode = FALSE,
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
        test_mode = FALSE,
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
          test_mode = FALSE,
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
        test_mode = FALSE,
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
        test_mode = FALSE,
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
