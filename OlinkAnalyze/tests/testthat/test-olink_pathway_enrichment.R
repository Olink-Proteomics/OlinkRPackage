skip_on_cran()
skip_if_not_installed("clusterProfiler")
skip_if_not_installed("msigdbr", minimum_version = "24.1.0")
skip_if_not_installed("broom")
skip_if_not_installed("car")

# clean up npa_data1
npx_df <- npx_data1 |>
  dplyr::filter(
    !stringr::str_detect(.data[["SampleID"]], "CONTROL")
  )

check_log <- check_npx(npx_df)

npx_data_format22 <- get_example_data("npx_data_format-Oct-2022.rds")

# example data

check_log_221010 <- check_npx(npx_data_format22) |>
  suppressWarnings() |>
  suppressMessages()

ttest_na <- olink_ttest(
  df = npx_data_format22,
  check_log = check_log_221010,
  variable = "treatment1"
) |>
  suppressMessages() |>
  suppressWarnings()

npx_data_format22 <- npx_data_format22 |>
  dplyr::mutate(OlinkID = ifelse(OlinkID == "OID30646",
                                 "OID12345",
                                 OlinkID))
check_log_221010 <- check_npx(npx_data_format22) |>
  suppressWarnings() |>
  suppressMessages()

# statistical tests
ttest_results <- olink_ttest(
  df = npx_df,
  check_log = check_log,
  variable = "Treatment"
) |>
  suppressMessages() |>
  suppressWarnings()

anova_posthoc_results <- olink_anova_posthoc(
  df = npx_df,
  check_log = check_log,
  variable = "Site",
  effect = "Site"
) |>
  suppressMessages() |>
  suppressWarnings()

# set seed
set.seed(123)

test_that("Error with duplicated assays", {
  duplicate_assay_data <- npx_data1 |>
    dplyr::filter(
      .data[["Assay"]] == "MET"
    ) |>
    dplyr::mutate(
      OlinkID = "OID01254"
    ) |>
    dplyr::mutate(
      LOD = .data[["LOD"]] + 1L
    )


  # error with duplicate assays
  duplicated_assay_data <- npx_data1 |>
    dplyr::bind_rows(duplicate_assay_data)
  expect_error(object = data_prep(df = duplicated_assay_data,
                                  test_results = ttest_results,
                                  check_log = check_log),
               regexp = "Duplicated assays detected:")
})

test_that("T-test GSEA works", {
  skip_on_cran()
  skip_if_not_installed("clusterProfiler")
  skip_if_not_installed("msigdbr", minimum_version = "24.1.0")

  expect_no_warning(
    object = expect_no_error(
      object = tt_gsea <- olink_pathway_enrichment(
        df = npx_df,
        check_log = check_log,
        test_results = ttest_results
      ) |>
        suppressMessages() |>
        suppressWarnings()
    )
  )

  expect_equal(
    object = nrow(tt_gsea),
    expected = 573L
  )

  expect_message(
    object = expect_warning(
      object = olink_pathway_enrichment(
        df = npx_data_format22,
        test_results = ttest_na,
        check_log = check_log_221010
      ),
      regexp = "The Olink IDs in the df do not all match the Olink IDs"
    ),
    regexp = "Filtering df for overlapping OlinkIDs in df"
  )

})

test_that("Reactome GSEA works", {
  skip_on_cran()
  skip_if_not_installed("clusterProfiler")
  skip_if_not_installed("msigdbr", minimum_version = "24.1.0")

  expect_no_warning(
    expect_no_error(
      object = tt_gsea_reactome <- olink_pathway_enrichment(
        df = npx_df,
        check_log = check_log,
        test_results = ttest_results,
        ontology = "Reactome"
      ) |>
        suppressMessages()
    )
  )

  expect_equal(
    object = nrow(tt_gsea_reactome),
    expected = 20L
  )
})

test_that("MSigDB_com GSEA works", {
  skip_on_cran()
  skip_if_not_installed("clusterProfiler")
  skip_if_not_installed("msigdbr", minimum_version = "24.1.0")

  expect_no_warning(
    object = expect_no_error(
      object = tt_gsea_msigdb_com <- olink_pathway_enrichment(
        df = npx_df,
        check_log = check_log,
        test_results = ttest_results,
        ontology = "MSigDb_com"
      ) |>
        suppressMessages()
    )
  )

  expect_equal(
    object = nrow(tt_gsea_msigdb_com),
    expected = 568L
  )
})

test_that("KEGG GSEA works", {
  skip_on_cran()
  skip_if_not_installed("clusterProfiler")
  skip_if_not_installed("msigdbr", minimum_version = "24.1.0")

  expect_no_error(
    object = tt_gsea_kegg <- olink_pathway_enrichment(df = npx_df,
      check_log = check_log,
      test_results = ttest_results,
      ontology = "KEGG"
    ) |>
      suppressMessages() |>
      suppressWarnings()
  )

  expect_equal(
    object = nrow(tt_gsea_kegg),
    expected = 0L
  )
})

test_that("GO GSEA works", {
  skip_on_cran()
  skip_if_not_installed("clusterProfiler")
  skip_if_not_installed("msigdbr", minimum_version = "24.1.0")

  expect_no_warning(
    object = expect_no_error(
      object = tt_gsea_go <- olink_pathway_enrichment(
        df = npx_df,
        check_log = check_log,
        test_results = ttest_results,
        ontology = "GO"
      ) |>
        suppressMessages()
    )
  )

  expect_equal(
    object = nrow(tt_gsea_go),
    expected = 356L
  )
})

test_that("T-test ORA works", {
  skip_on_cran()
  skip_if_not_installed("clusterProfiler")
  skip_if_not_installed("msigdbr", minimum_version = "24.1.0")

  expect_no_warning(
    object = expect_no_error(
      object = tt_ora <- olink_pathway_enrichment(
        df = npx_df,
        check_log = check_log,
        test_results = ttest_results,
        method = "ORA",
        ontology = "MSigDb"
      ) |>
        suppressMessages()
    )
  )

  expect_equal(
    object = nrow(tt_ora),
    expected = 345L
  )
})

test_that("Reactome ORA works", {
  skip_on_cran()
  skip_if_not_installed("clusterProfiler")
  skip_if_not_installed("msigdbr", minimum_version = "24.1.0")

  expect_no_warning(
    object = expect_no_error(
      object = tt_ora_reactome <- olink_pathway_enrichment(
        df = npx_df,
        check_log = check_log,
        test_results = ttest_results,
        method = "ORA",
        ontology = "Reactome"
      ) |>
        suppressMessages()
    )
  )

  expect_equal(
    object = nrow(tt_ora_reactome),
    expected = 15L
  )
})

test_that("KEGG ORA works", {
  skip_on_cran()
  skip_if_not_installed("clusterProfiler")
  skip_if_not_installed("msigdbr", minimum_version = "24.1.0")

  expect_warning(
    object = tt_ora_kegg <- olink_pathway_enrichment(
      df = npx_df,
      check_log = check_log,
      test_results = ttest_results,
      method = "ORA",
      ontology = "KEGG"
    ) |>
      suppressMessages(),
    regexp = "No remaining pathways within the range 10-500 proteins!"
  )

  expect_true(
    object = is.null(tt_ora_kegg)
  )
})

test_that("GO ORA works", {
  skip_on_cran()
  skip_if_not_installed("clusterProfiler")
  skip_if_not_installed("msigdbr", minimum_version = "24.1.0")

  expect_no_warning(
    object = expect_no_error(
      object = tt_ora_go <- olink_pathway_enrichment(
        df = npx_df,
        check_log = check_log,
        test_results = ttest_results,
        method = "ORA",
        ontology = "GO"
      ) |>
        suppressMessages()
    )
  )

  expect_equal(
    object = nrow(tt_ora_go),
    expected = 212L
  )
})

test_that("Errors occur", {
  skip_on_cran()
  skip_if_not_installed("clusterProfiler")
  skip_if_not_installed("msigdbr", minimum_version = "24.1.0")

  expect_message(
    object = expect_message(object = data_prep(df = npx_data_format22,
                                               test_results = ttest_na,
                                               check_log = check_log_221010),
                            regex = "Removing invalid OlinkIDs"),
    regex = "Filtering df for overlapping OlinkIDs in df"
  )
})

test_that(
  "olink_pathway_enrichment - error - missing args",
  {
    skip_on_cran()
    skip_if_not_installed("clusterProfiler")
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

test_that("ORA warns assays not found in database", {
  skip_on_cran()
  skip_if_not_installed("clusterProfiler")
  skip_if_not_installed("msigdbr", minimum_version = "24.1.0")

  messages <- capture_messages(
    code = olink_pathway_enrichment(
      df = npx_df,
      check_log = check_log,
      test_results = ttest_results,
      method = "ORA",
      ontology = "MSigDb"
    )
  )
  expect_true(
    object = grepl(
      pattern = "assays are not found in the database.",
      x = paste(messages, collapse = "")
    )
  )
})

test_that("gsea warns assays not found in database", {
  skip_on_cran()
  skip_if_not_installed("clusterProfiler")
  skip_if_not_installed("msigdbr", minimum_version = "24.1.0")

  messages <- capture_messages(
    code = olink_pathway_enrichment(
      df = npx_df,
      check_log = check_log,
      test_results = ttest_results
    )
  )
  expect_true(
    object = grepl(
      pattern = "assays are not found in the database.",
      x = paste(messages, collapse = "")
    )
  )
})

test_that("mouse works", {
  skip_on_cran()
  skip_if_not_installed("clusterProfiler")
  skip_if_not_installed("msigdbr", minimum_version = "24.1.0")

  expect_equal(
    object = select_ont("Reactome", "mouse"),
    expected = {
      msigdbr::msigdbr(species = "Mus musculus", collection = "C2") |>
        dplyr::bind_rows(msigdbr::msigdbr(species = "Mus musculus",
                                          collection = "C5")) |>
        dplyr::filter(.data[["gs_subcollection"]] == "CP:REACTOME")  |>
        dplyr::select(dplyr::any_of(c("gs_name", "gene_symbol")))
    }
  )
})

test_that("test_prep works", {
  skip_on_cran()
  skip_if_not_installed("clusterProfiler")
  skip_if_not_installed("msigdbr", minimum_version = "24.1.0")

  expect_equal(object = test_prep(df = npx_data_format22,
                                  test_results = ttest_na) |>
                 dplyr::select(any_of("OlinkID")) |>
                 dplyr::distinct() |>
                 dplyr::pull() |>
                 sort(),
               expected = intersect(npx_data_format22$OlinkID,
                                    ttest_na$OlinkID) |>
                 sort())
})

# Test check_pe_inputs ----

test_that(
  "check_pe_inputs - warning - non-overlapping OID df and test_results",
  {
    expect_warning(
      object = check_pe_inputs(
        df = npx_data1,
        check_log = check_log,
        test_results = ttest_results |>
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
    expect_error(
      object = check_pe_inputs(
        df = npx_data1,
        check_log = check_log,
        test_results = anova_posthoc_results,
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
    expect_error(
      object = check_pe_inputs(
        df = npx_data1,
        check_log = check_log,
        test_results = ttest_results,
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
    expect_error(
      object = check_pe_inputs(
        df = npx_data1,
        check_log = check_log,
        test_results = ttest_results,
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
    expect_error(
      object =  olink_pathway_enrichment(
        df = npx_data1,
        check_log = check_log,
        test_results = ttest_results,
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
    all_assays <- npx_data1[["OlinkID"]] |> unique() |> sort()

    # all assays overlap - both ----

    expect_equal(
      object = helper_non_overlap_assays(
        df = npx_data1,
        test_results = ttest_results,
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
        test_results = ttest_results,
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
        test_results = ttest_results,
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
        test_results = ttest_results |>
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
        test_results = ttest_results |>
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
        test_results = ttest_results |>
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
        test_results = ttest_results,
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
        test_results = ttest_results,
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
        test_results = ttest_results,
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

    npx_data_invalid_check <- check_npx(df = npx_data_invalid) |>
      suppressWarnings() |>
      suppressMessages()

    expect_message(
      object = data_prep_out <- data_prep(
        df = npx_data_invalid,
        test_results = ttest_results,
        check_log = npx_data_invalid_check
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
    exclude_assays <- npx_data1[["OlinkID"]] |> unique() |> head(n = 5L)

    expect_message(
      object = data_prep_out <- data_prep(
        df = npx_data1 |>
          dplyr::filter(
            !grepl(pattern = "control",
                   x = .data[["SampleID"]],
                   ignore.case = TRUE)
          ),
        test_results = ttest_results |>
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
    expect_error(
      object = data_prep(
        df = npx_data1,
        test_results = ttest_results,
        check_log = check_log
      ),
      regexp = "Detected 184 duplicated assays in `df`: \"CHL1\", \"NRP1\"",
    )
  }
)

# Test test_prep ----

test_that(
  "test_prep - works - remove non-overlapping assays",
  {
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
        test_results = ttest_results,
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
