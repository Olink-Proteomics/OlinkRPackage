# Test olink_normalization ----

# this tests also all functions called norm_internal_* except from
# "norm_internal_rename_cols". Namely:
# - norm_internal_assay_median
# - norm_internal_reference_median
# - norm_internal_bridge
# - norm_internal_subset
# - norm_internal_adjust
# - norm_internal_adjust_ref
# - norm_internal_adjust_not_ref
#

test_that(
  "olink_normalization - works - bridge normalization",
  {
    skip_if_not(file.exists("../data/ref_results_norm.rds"))

    # load normalized datasets generated with the original olink_normalization
    # function from OlinkAnalyze 3.8.2
    get_ref_norm_res <- function() {
      ref_norm_res_file <- test_path("..", "data", "ref_results_norm.rds")
      readRDS(file = ref_norm_res_file)
    }
    ref_norm_res <- get_ref_norm_res()

    ### bridge normalization - no norm column ----

    expect_warning(
      object = expect_message(
        object = bridge_no_norm <- olink_normalization(
          df1 = ref_norm_res$lst_df$df1_no_norm,
          df2 = ref_norm_res$lst_df$df2_no_norm,
          overlapping_samples_df1 = ref_norm_res$lst_sample$bridge_samples,
          df1_project_nr = "df1_no_norm",
          df2_project_nr = "df2_no_norm",
          reference_project = "df1_no_norm"
        ) |>
          dplyr::filter(
            .data[["SampleID"]] %in% ref_norm_res$lst_sample$sample_subset
          ),
        regexp = "Bridge normalization will be performed!"
      ),
      regexp = paste("Datasets \"df1_no_norm\" and \"df2_no_norm\" do not",
                     "contain a column named \"Normalization\"")
    )

    expect_equal(
      object = bridge_no_norm,
      expected = ref_norm_res$lst_norm$bridge_norm$no_norm,
      tolerance = 1e-4
    )

    ### bridge normalization - with norm column ----

    expect_message(
      object = bridge_norm <- olink_normalization(
        df1 = ref_norm_res$lst_df$df1_norm,
        df2 = ref_norm_res$lst_df$df2_norm,
        overlapping_samples_df1 = ref_norm_res$lst_sample$bridge_samples,
        df1_project_nr = "df1_norm",
        df2_project_nr = "df2_norm",
        reference_project = "df1_norm"
      ) |>
        dplyr::filter(
          .data[["SampleID"]] %in% ref_norm_res$lst_sample$sample_subset
        ),
      regexp = "Bridge normalization will be performed!"
    )

    expect_equal(
      object = bridge_norm,
      expected = ref_norm_res$lst_norm$bridge_norm$norm,
      tolerance = 1e-4
    )

    ### bridge normalization - no lod column ----

    expect_message(
      object = bridge_no_lod <- olink_normalization(
        df1 = ref_norm_res$lst_df$df1_no_lod,
        df2 = ref_norm_res$lst_df$df2_no_lod,
        overlapping_samples_df1 = ref_norm_res$lst_sample$bridge_samples,
        df1_project_nr = "df1_no_lod",
        df2_project_nr = "df2_no_lod",
        reference_project = "df1_no_lod"
      ) |>
        dplyr::filter(
          .data[["SampleID"]] %in% ref_norm_res$lst_sample$sample_subset
        ),
      regexp = "Bridge normalization will be performed!"
    )

    expect_equal(
      object = bridge_no_lod,
      expected = ref_norm_res$lst_norm$bridge_norm$no_lod,
      tolerance = 1e-4
    )

    ### bridge normalization - multiple lod columns ----

    expect_message(
      object = expect_message(
        object = bridge_multiple_lod <- olink_normalization(
          df1 = ref_norm_res$lst_df$df1_multiple_lod,
          df2 = ref_norm_res$lst_df$df2_multiple_lod,
          overlapping_samples_df1 = ref_norm_res$lst_sample$bridge_samples,
          df1_project_nr = "df1_multiple_lod",
          df2_project_nr = "df2_multiple_lod",
          reference_project = "df1_multiple_lod"
        ) |>
          dplyr::filter(
            .data[["SampleID"]] %in% ref_norm_res$lst_sample$sample_subset
          ),
        regexp = "Bridge normalization will be performed!"
      ),
      regexp = "Datasets \"df1_multiple_lod\" and \"df2_multiple_lod\" contain"
    )

    expect_equal(
      object = bridge_multiple_lod,
      expected = ref_norm_res$lst_norm$bridge_norm$multiple_lod,
      tolerance = 1e-4
    )
  }
)

test_that(
  "olink_normalization - works - intensity normalization",
  {
    skip_if_not(file.exists("../data/ref_results_norm.rds"))

    # load normalized datasets generated with the original olink_normalization
    # function from OlinkAnalyze 3.8.2
    get_ref_norm_res <- function() {
      ref_norm_res_file <- test_path("..", "data", "ref_results_norm.rds")
      readRDS(file = ref_norm_res_file)
    }
    ref_norm_res <- get_ref_norm_res()

    ### intensity normalization - no norm column ----

    expect_warning(
      object = expect_message(
        object = intensity_no_norm <- olink_normalization(
          df1 = ref_norm_res$lst_df$df1_no_norm,
          df2 = ref_norm_res$lst_df$df2_no_norm,
          overlapping_samples_df1 = ref_norm_res$lst_sample$df1_all,
          overlapping_samples_df2 = ref_norm_res$lst_sample$df2_all,
          df1_project_nr = "df1_no_norm",
          df2_project_nr = "df2_no_norm",
          reference_project = "df1_no_norm"
        ) |>
          dplyr::filter(
            .data[["SampleID"]] %in% ref_norm_res$lst_sample$sample_subset
          ),
        regexp = "Subset normalization will be performed!"
      ),
      regexp = paste("Datasets \"df1_no_norm\" and \"df2_no_norm\" do not",
                     "contain a column named \"Normalization\"")
    )

    expect_equal(
      object = intensity_no_norm,
      expected = ref_norm_res$lst_norm$intensity_norm$no_norm,
      tolerance = 1e-4
    )

    ### intensity normalization - with norm column ----

    expect_message(
      object = intensity_norm <- olink_normalization(
        df1 = ref_norm_res$lst_df$df1_norm,
        df2 = ref_norm_res$lst_df$df2_norm,
        overlapping_samples_df1 = ref_norm_res$lst_sample$df1_all,
        overlapping_samples_df2 = ref_norm_res$lst_sample$df2_all,
        df1_project_nr = "df1_norm",
        df2_project_nr = "df2_norm",
        reference_project = "df1_norm"
      ) |>
        dplyr::filter(
          .data[["SampleID"]] %in% ref_norm_res$lst_sample$sample_subset
        ),
      regexp = "Subset normalization will be performed!"
    )

    expect_equal(
      object = intensity_norm,
      expected = ref_norm_res$lst_norm$intensity_norm$norm,
      tolerance = 1e-4
    )

    ### intensity normalization - no lod column ----

    expect_message(
      object = intensity_no_lod <- olink_normalization(
        df1 = ref_norm_res$lst_df$df1_no_lod,
        df2 = ref_norm_res$lst_df$df2_no_lod,
        overlapping_samples_df1 = ref_norm_res$lst_sample$df1_all,
        overlapping_samples_df2 = ref_norm_res$lst_sample$df2_all,
        df1_project_nr = "df1_no_lod",
        df2_project_nr = "df2_no_lod",
        reference_project = "df1_no_lod"
      ) |>
        dplyr::filter(
          .data[["SampleID"]] %in% ref_norm_res$lst_sample$sample_subset
        ),
      regexp = "Subset normalization will be performed!"
    )

    expect_equal(
      object = intensity_no_lod,
      expected = ref_norm_res$lst_norm$intensity_norm$no_lod,
      tolerance = 1e-4
    )

    ### intensity normalization - multiple lod columns ----

    expect_message(
      object = expect_message(
        object = intensity_multiple_lod <- olink_normalization(
          df1 = ref_norm_res$lst_df$df1_multiple_lod,
          df2 = ref_norm_res$lst_df$df2_multiple_lod,
          overlapping_samples_df1 = ref_norm_res$lst_sample$df1_all,
          overlapping_samples_df2 = ref_norm_res$lst_sample$df2_all,
          df1_project_nr = "df1_multiple_lod",
          df2_project_nr = "df2_multiple_lod",
          reference_project = "df1_multiple_lod"
        ) |>
          dplyr::filter(
            .data[["SampleID"]] %in% ref_norm_res$lst_sample$sample_subset
          ),
        regexp = "Subset normalization will be performed!"
      ),
      regexp = "Datasets \"df1_multiple_lod\" and \"df2_multiple_lod\" contain"
    )

    expect_equal(
      object = intensity_multiple_lod,
      expected = ref_norm_res$lst_norm$intensity_norm$multiple_lod,
      tolerance = 1e-4
    )
  }
)

test_that(
  "olink_normalization - works - subset normalization",
  {
    skip_if_not(file.exists("../data/ref_results_norm.rds"))

    # load normalized datasets generated with the original olink_normalization
    # function from OlinkAnalyze 3.8.2
    get_ref_norm_res <- function() {
      ref_norm_res_file <- test_path("..", "data", "ref_results_norm.rds")
      readRDS(file = ref_norm_res_file)
    }
    ref_norm_res <- get_ref_norm_res()

    ### subset normalization - no norm column ----

    expect_warning(
      object = expect_message(
        object = subset_no_norm <- olink_normalization(
          df1 = ref_norm_res$lst_df$df1_no_norm,
          df2 = ref_norm_res$lst_df$df2_no_norm,
          overlapping_samples_df1 = ref_norm_res$lst_sample$df1_subset,
          overlapping_samples_df2 = ref_norm_res$lst_sample$df2_subset,
          df1_project_nr = "df1_no_norm",
          df2_project_nr = "df2_no_norm",
          reference_project = "df1_no_norm"
        ) |>
          dplyr::filter(
            .data[["SampleID"]] %in% ref_norm_res$lst_sample$sample_subset
          ),
        regexp = "Subset normalization will be performed!"
      ),
      regexp = paste("Datasets \"df1_no_norm\" and \"df2_no_norm\" do not",
                     "contain a column named \"Normalization\"")
    )

    expect_equal(
      object = subset_no_norm,
      expected = ref_norm_res$lst_norm$subset_norm$no_norm,
      tolerance = 1e-4
    )

    ### subset normalization - with norm column ----

    expect_message(
      object = subset_norm <- olink_normalization(
        df1 = ref_norm_res$lst_df$df1_norm,
        df2 = ref_norm_res$lst_df$df2_norm,
        overlapping_samples_df1 = ref_norm_res$lst_sample$df1_subset,
        overlapping_samples_df2 = ref_norm_res$lst_sample$df2_subset,
        df1_project_nr = "df1_norm",
        df2_project_nr = "df2_norm",
        reference_project = "df1_norm"
      ) |>
        dplyr::filter(
          .data[["SampleID"]] %in% ref_norm_res$lst_sample$sample_subset
        ),
      regexp = "Subset normalization will be performed!"
    )

    expect_equal(
      object = subset_norm,
      expected = ref_norm_res$lst_norm$subset_norm$norm,
      tolerance = 1e-4
    )

    ### subset normalization - no lod column ----

    expect_message(
      object = subset_no_lod <- olink_normalization(
        df1 = ref_norm_res$lst_df$df1_no_lod,
        df2 = ref_norm_res$lst_df$df2_no_lod,
        overlapping_samples_df1 = ref_norm_res$lst_sample$df1_subset,
        overlapping_samples_df2 = ref_norm_res$lst_sample$df2_subset,
        df1_project_nr = "df1_no_lod",
        df2_project_nr = "df2_no_lod",
        reference_project = "df1_no_lod"
      ) |>
        dplyr::filter(
          .data[["SampleID"]] %in% ref_norm_res$lst_sample$sample_subset
        ),
      regexp = "Subset normalization will be performed!"
    )

    expect_equal(
      object = subset_no_lod,
      expected = ref_norm_res$lst_norm$subset_norm$no_lod,
      tolerance = 1e-4
    )

    ### subset normalization - multiple lod columns ----

    expect_message(
      object = expect_message(
        object = subset_multiple_lod <- olink_normalization(
          df1 = ref_norm_res$lst_df$df1_multiple_lod,
          df2 = ref_norm_res$lst_df$df2_multiple_lod,
          overlapping_samples_df1 = ref_norm_res$lst_sample$df1_subset,
          overlapping_samples_df2 = ref_norm_res$lst_sample$df2_subset,
          df1_project_nr = "df1_multiple_lod",
          df2_project_nr = "df2_multiple_lod",
          reference_project = "df1_multiple_lod"
        ) |>
          dplyr::filter(
            .data[["SampleID"]] %in% ref_norm_res$lst_sample$sample_subset
          ),
        regexp = "Subset normalization will be performed!"
      ),
      regexp = "Datasets \"df1_multiple_lod\" and \"df2_multiple_lod\" contain"
    )

    expect_equal(
      object = subset_multiple_lod,
      expected = ref_norm_res$lst_norm$subset_norm$multiple_lod,
      tolerance = 1e-4
    )
  }
)

test_that(
  "olink_normalization - works - reference median normalization",
  {
    skip_if_not(file.exists("../data/ref_results_norm.rds"))

    # load normalized datasets generated with the original olink_normalization
    # function from OlinkAnalyze 3.8.2
    get_ref_norm_res <- function() {
      ref_norm_res_file <- test_path("..", "data", "ref_results_norm.rds")
      readRDS(file = ref_norm_res_file)
    }
    ref_norm_res <- get_ref_norm_res()

    ### reference median normalization - no norm column ----

    expect_warning(
      object = expect_message(
        object = ref_med_no_norm <- olink_normalization(
          df1 = ref_norm_res$lst_df$df1_no_norm,
          overlapping_samples_df1 = ref_norm_res$lst_sample$df1_subset,
          df1_project_nr = "df1_no_norm",
          reference_medians = ref_norm_res$lst_df$ref_med
        ) |>
          dplyr::filter(
            .data[["SampleID"]] %in% ref_norm_res$lst_sample$sample_subset
          ),
        regexp = "Reference median normalization will be performed!"
      ),
      regexp = paste("Dataset \"df1_no_norm\" does not contain a column named",
                     "\"Normalization\"")
    )

    expect_equal(
      object = ref_med_no_norm,
      expected = ref_norm_res$lst_norm$ref_med_norm$no_norm,
      tolerance = 1e-4
    )

    ### reference median normalization - with norm column ----

    expect_message(
      object = ref_med_norm <- olink_normalization(
        df1 = ref_norm_res$lst_df$df1_norm,
        overlapping_samples_df1 = ref_norm_res$lst_sample$df1_subset,
        df1_project_nr = "df1_norm",
        reference_medians = ref_norm_res$lst_df$ref_med
      ) |>
        dplyr::filter(
          .data[["SampleID"]] %in% ref_norm_res$lst_sample$sample_subset
        ),
      regexp = "Reference median normalization will be performed!"
    )

    expect_equal(
      object = ref_med_norm,
      expected = ref_norm_res$lst_norm$ref_med_norm$norm,
      tolerance = 1e-4
    )

    ### reference median normalization - no lod column ----

    expect_message(
      object = ref_med_no_lod <- olink_normalization(
        df1 = ref_norm_res$lst_df$df1_no_lod,
        overlapping_samples_df1 = ref_norm_res$lst_sample$df1_subset,
        df1_project_nr = "df1_no_lod",
        reference_medians = ref_norm_res$lst_df$ref_med
      ) |>
        dplyr::filter(
          .data[["SampleID"]] %in% ref_norm_res$lst_sample$sample_subset
        ),
      regexp = "Reference median normalization will be performed!"
    )

    expect_equal(
      object = ref_med_no_lod,
      expected = ref_norm_res$lst_norm$ref_med_norm$no_lod,
      tolerance = 1e-4
    )

    ### reference median normalization - multiple lod columns ----

    expect_message(
      object = expect_message(
        object = ref_med_multiple_lod <- olink_normalization(
          df1 = ref_norm_res$lst_df$df1_multiple_lod,
          overlapping_samples_df1 = ref_norm_res$lst_sample$df1_subset,
          df1_project_nr = "df1_multiple_lod",
          reference_medians = ref_norm_res$lst_df$ref_med
        ) |>
          dplyr::filter(
            .data[["SampleID"]] %in% ref_norm_res$lst_sample$sample_subset
          ),
        regexp = "Reference median normalization will be performed!"
      ),
      regexp = "Dataset \"df1_multiple_lod\" contains multiple columns matching"
    )

    expect_equal(
      object = ref_med_multiple_lod,
      expected = ref_norm_res$lst_norm$ref_med_norm$multiple_lod,
      tolerance = 1e-4
    )
  }
)

test_that(
  "olink_normalization - works - 3k-HT normalization",
  {
    skip_if_not(file.exists("../data/example_3k_data.rds"))
    skip_if_not(file.exists("../data/example_HT_data.rds"))

    data_3k <- get_example_data(filename = "example_3k_data.rds")
    data_ht <- get_example_data(filename = "example_HT_data.rds")


    expect_message(expect_warning(
      object = ht_3k_norm <- olink_normalization(
        df1 = data_ht,
        df2 = data_3k,
        overlapping_samples_df1 = intersect(
          x = unique(data_ht$SampleID),
          y = unique(data_3k$SampleID)
        ) |>
          (\(.) .[!grepl("CONTROL", .)])(),
        df1_project_nr = "df_ht",
        df2_project_nr = "df_3k",
        reference_project = "df_ht"
      ),
      regexp = "2 assays are not shared across products."),
      regexp = "Cross-product normalization will be performed!"
    )

    expect_identical(
      object = dim(ht_3k_norm),
      expected = c(39936L, 22L)
    )

    expect_identical(
      object = names(ht_3k_norm),
      expected = c("SampleID", "OlinkID", "SampleType", "WellID", "PlateID",
                   "UniProt", "Assay", "AssayType", "Panel", "Block", "NPX",
                   "PCNormalizedNPX", "Count", "Normalization", "AssayQC",
                   "SampleQC", "DataAnalysisRefID", "Project", "OlinkID_E3072",
                   "MedianCenteredNPX", "QSNormalizedNPX",
                   "BridgingRecommendation")
    )
  }
)

# Test norm_internal_rename_cols ----

test_that(
  "norm_internal_rename_cols - works",
  {
    skip_if_not_installed("arrow")

    ## example 1: panel_version ----

    lst_cnames_v1 <- olink_norm_input_check_df_cols(
      lst_df = list(
        "p1" = npx_data1 |>
          dplyr::mutate(
            Normalization = "Intensity"
          ),
        "p2" = npx_data2 |>
          dplyr::rename(
            "Panel_Lot_Nr" = "Panel_Version"
          ) |>
          dplyr::mutate(
            Normalization = "Intensity"
          )
      )
    )

    update_cnames_v1 <- norm_internal_rename_cols(
      ref_cols = lst_cnames_v1$p2,
      not_ref_cols = lst_cnames_v1$p1,
      not_ref_df = npx_data1 |>
        dplyr::mutate(
          Normalization = "Intensity"
        ) |>
        dplyr::slice_head(n = 10L)
    ) |>
      names()

    expect_identical(
      object = update_cnames_v1,
      expected = c(names(npx_data1), "Normalization") |>
        stringr::str_replace("Panel_Version", "Panel_Lot_Nr")
    )

    ## example 2: panel_version & qc_warn ----

    lst_cnames_v2 <- olink_norm_input_check_df_cols(
      lst_df = list(
        "p1" = npx_data1 |>
          dplyr::rename(
            "SampleQC" = "QC_Warning"
          ) |>
          dplyr::mutate(
            Normalization = "Intensity"
          ),
        "p2" = npx_data2 |>
          dplyr::rename(
            "Panel_Lot_Nr" = "Panel_Version"
          ) |>
          dplyr::mutate(
            Normalization = "Intensity"
          )
      )
    )

    update_cnames_v2 <- norm_internal_rename_cols(
      ref_cols = lst_cnames_v2$p1,
      not_ref_cols = lst_cnames_v2$p2,
      not_ref_df = npx_data2 |>
        dplyr::rename(
          "Panel_Lot_Nr" = "Panel_Version"
        ) |>
        dplyr::mutate(
          Normalization = "Intensity"
        )
    ) |>
      names()

    expect_identical(
      object = update_cnames_v2,
      expected = c(names(npx_data2), "Normalization") |>
        stringr::str_replace("QC_Warning", "SampleQC")
    )

    ## example 2: panel_version, qc_warn, assay_qc - arrow ----

    lst_cnames_v3 <- olink_norm_input_check_df_cols(
      lst_df = list(
        "p1" = npx_data1 |>
          dplyr::rename(
            "SampleQC" = "QC_Warning"
          ) |>
          dplyr::mutate(
            Normalization = "Intensity",
            AssayQC = "Pass"
          ) |>
          arrow::as_arrow_table(),
        "p2" = npx_data2 |>
          dplyr::rename(
            "Panel_Lot_Nr" = "Panel_Version"
          ) |>
          dplyr::mutate(
            Normalization = "Intensity",
            Assay_Warning = "Pass"
          ) |>
          arrow::as_arrow_table()
      )
    )

    update_cnames_v3 <- norm_internal_rename_cols(
      ref_cols = lst_cnames_v3$p1,
      not_ref_cols = lst_cnames_v3$p2,
      not_ref_df = npx_data2 |>
        dplyr::rename(
          "Panel_Lot_Nr" = "Panel_Version"
        ) |>
        dplyr::mutate(
          Normalization = "Intensity",
          Assay_Warning = "Pass"
        ) |>
        arrow::as_arrow_table()
    ) |>
      names()

    expect_identical(
      object = update_cnames_v3,
      expected = c(names(npx_data2), c("Normalization", "AssayQC")) |>
        stringr::str_replace("QC_Warning", "SampleQC")
    )

  }
)

test_that(
  "olink_norm_rename_cols_to_ref - error",
  {
    skip_if_not_installed("arrow")

    ## error v1: reference has too many matches ----

    expect_error(
      object = norm_internal_rename_cols(
        ref_cols = list(
          sample_id = "SampleID",
          olink_id = "OlinkID",
          uniprot = "UniProt",
          assay = "Assay",
          panel = "Panel",
          panel_version = c("Panel_Lot_Nr", "Panel_Version",
                            "DataAnalysisRefID"),
          plate_id = "PlateID",
          qc_warn = "QC_Warning",
          quant = "NPX",
          lod = "LOD",
          normalization = "Normalization"
        ),
        not_ref_cols = list(
          sample_id = "SampleID",
          olink_id = "OlinkID",
          uniprot = "UniProt",
          assay = "Assay",
          panel = "Panel",
          panel_version = "Panel_Version",
          plate_id = "PlateID",
          qc_warn = "QC_Warning",
          quant = "NPX",
          lod = "LOD",
          normalization = "Normalization"
        ),
        not_ref_df = npx_data1 |>
          dplyr::mutate(
            Normalization = "Intensity"
          ) |>
          dplyr::slice_head(n = 10L)
      ),
      regexp = paste("Cannot rename column \"Panel_Version\", with columns",
                     "\"Panel_Lot_Nr\"")
    )

    ## error v2: non-reference has too many matches ----

    expect_error(
      object = norm_internal_rename_cols(
        ref_cols = list(
          sample_id = "SampleID",
          olink_id = "OlinkID",
          uniprot = "UniProt",
          assay = "Assay",
          panel = "Panel",
          panel_version = "Panel_Lot_Nr",
          plate_id = "PlateID",
          qc_warn = "QC_Warning",
          quant = "NPX",
          lod = "LOD",
          normalization = "Normalization"
        ),
        not_ref_cols = list(
          sample_id = "SampleID",
          olink_id = "OlinkID",
          uniprot = "UniProt",
          assay = "Assay",
          panel = "Panel",
          panel_version = c("Panel_Lot_Nr", "Panel_Version",
                            "DataAnalysisRefID"),
          plate_id = "PlateID",
          qc_warn = "QC_Warning",
          quant = "NPX",
          lod = "LOD",
          normalization = "Normalization"
        ),
        not_ref_df = npx_data1 |>
          dplyr::mutate(
            Normalization = "Intensity"
          ) |>
          dplyr::slice_head(n = 10L)
      ),
      regexp = paste("Cannot rename columns \"Panel_Lot_Nr\",",
                     "\"Panel_Version\", and \"DataAnalysisRefID\"")
    )

  }
)

test_that(
  "Cross product normalization works - correlation assays present",
  {
    data_3k <- get_example_data(filename = "example_3k_data.rds")
    data_ht <- get_example_data(filename = "example_HT_data.rds")

    # bridge samples
    bridge_samples <- intersect(
      x = unique(data_ht$SampleID),
      y = unique(data_3k$SampleID)
    ) |>
      (\(x) x[!grepl("CONTROL", x)])() |>
      sort() |>
      head(50L)

    #correlation assay IDs
    oid_ht <- "OID43204"
    oid_3k <- eHT_e3072_mapping$OlinkID_E3072[
      eHT_e3072_mapping$OlinkID_HT == oid_ht]

    # HT correlation is present
    expect_contains(
      object = olink_normalization(
        df1 = data_ht |>
          dplyr::filter(!(OlinkID %in% c("OID12345", "OID54321"))),
        df2 = data_3k |>
          dplyr::filter(!(OlinkID %in% c("OID12345", "OID54321"))),
        overlapping_samples_df1 = bridge_samples,
        df1_project_nr = "proj_ht",
        df2_project_nr = "proj_3k",
        reference_project = "proj_ht"
      ) |>
        dplyr::distinct(OlinkID) |>
        dplyr::pull(),
      expected = oid_ht
    )

    # All 3k correlations are present
    expect_contains(
      object = olink_normalization(
        df1 = data_ht |>
          dplyr::filter(!(OlinkID %in% c("OID12345", "OID54321"))),
        df2 = data_3k |>
          dplyr::filter(!(OlinkID %in% c("OID12345", "OID54321"))),
        overlapping_samples_df1 = bridge_samples,
        df1_project_nr = "proj_ht",
        df2_project_nr = "proj_3k",
        reference_project = "proj_ht"
      ) |>
        dplyr::distinct(OlinkID_E3072) |>
        dplyr::pull(),
      expected = oid_3k
    )
  }
)
