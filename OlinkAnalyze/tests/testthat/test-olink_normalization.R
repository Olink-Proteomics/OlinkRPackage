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
    skip_if_not(file.exists(test_path("data", "ref_results_norm.rds")))

    ref_norm_res <- get_example_data("ref_results_norm.rds")

    ### bridge normalization - no norm column ----

    expect_message(
      object = expect_warning(
        object = expect_message(
          object = bridge_no_norm <- olink_normalization(
            df1 = ref_norm_res$lst_df$df1_no_norm,
            df2 = ref_norm_res$lst_df$df2_no_norm,
            overlapping_samples_df1 = ref_norm_res$lst_sample$bridge_samples,
            df1_project_nr = "df1_no_norm",
            df2_project_nr = "df2_no_norm",
            reference_project = "df1_no_norm",
            format = FALSE
          ) |>
            dplyr::filter(
              .data[["SampleID"]] %in% ref_norm_res$lst_sample$sample_subset
            ),
          regexp = "Bridge normalization will be performed!"
        ),
        regexp = paste("Datasets \"df1_no_norm\" and \"df2_no_norm\" do not",
                       "contain a column named \"Normalization\"")
      ),
      regexp = "Output includes two sets of bridging samples"
    )

    expect_equal(
      object = bridge_no_norm,
      expected = ref_norm_res$lst_norm$bridge_norm$no_norm,
      tolerance = 1e-4
    )

    ### bridge normalization - with norm column ----

    expect_message(
      object = expect_message(
        object = bridge_norm <- olink_normalization(
          df1 = ref_norm_res$lst_df$df1_norm,
          df2 = ref_norm_res$lst_df$df2_norm,
          overlapping_samples_df1 = ref_norm_res$lst_sample$bridge_samples,
          df1_project_nr = "df1_norm",
          df2_project_nr = "df2_norm",
          reference_project = "df1_norm",
          format = FALSE
        ) |>
          dplyr::filter(
            .data[["SampleID"]] %in% ref_norm_res$lst_sample$sample_subset
          ),
        regexp = "Bridge normalization will be performed!"
      ),
      regexp = "Output includes two sets of bridging samples"
    )

    expect_equal(
      object = bridge_norm,
      expected = ref_norm_res$lst_norm$bridge_norm$norm,
      tolerance = 1e-4
    )

    ### bridge normalization - no lod column ----

    expect_message(
      object = expect_message(
        object = bridge_no_lod <- olink_normalization(
          df1 = ref_norm_res$lst_df$df1_no_lod,
          df2 = ref_norm_res$lst_df$df2_no_lod,
          overlapping_samples_df1 = ref_norm_res$lst_sample$bridge_samples,
          df1_project_nr = "df1_no_lod",
          df2_project_nr = "df2_no_lod",
          reference_project = "df1_no_lod",
          format = FALSE
        ) |>
          dplyr::filter(
            .data[["SampleID"]] %in% ref_norm_res$lst_sample$sample_subset
          ),
        regexp = "Bridge normalization will be performed!"
      ),
      regexp = "Output includes two sets of bridging samples"
    )

    expect_equal(
      object = bridge_no_lod,
      expected = ref_norm_res$lst_norm$bridge_norm$no_lod,
      tolerance = 1e-4
    )

    ### bridge normalization - multiple lod columns ----

    expect_message(
      object = expect_message(
        object = expect_message(
          object = bridge_multiple_lod <- olink_normalization(
            df1 = ref_norm_res$lst_df$df1_multiple_lod,
            df2 = ref_norm_res$lst_df$df2_multiple_lod,
            overlapping_samples_df1 = ref_norm_res$lst_sample$bridge_samples,
            df1_project_nr = "df1_multiple_lod",
            df2_project_nr = "df2_multiple_lod",
            reference_project = "df1_multiple_lod",
            format = FALSE
          ) |>
            dplyr::filter(
              .data[["SampleID"]] %in% ref_norm_res$lst_sample$sample_subset
            ),
          regexp = "Bridge normalization will be performed!"
        ),
        regexp = paste("Datasets \"df1_multiple_lod\" and \"df2_multiple_lod\"",
                       "contain")
      ),
      regexp = "Output includes two sets of bridging samples"
    )

    expect_equal(
      object = bridge_multiple_lod,
      expected = ref_norm_res$lst_norm$bridge_norm$multiple_lod,
      tolerance = 1e-4
    )
  }
)

test_that(
  "olink_normalization - works - bridge normalization - format",
  {
    skip_if_not(file.exists(test_path("data", "ref_results_norm.rds")))

    ref_norm_res <- get_example_data("ref_results_norm.rds")

    sample_subset <- ref_norm_res$lst_sample$sample_subset

    ### bridge - w/ norm col - vanilla ----

    df_norm_ref_v1 <- ref_norm_res$lst_df$df1_norm
    df_norm_not_ref_v1 <- ref_norm_res$lst_df$df2_norm

    bridge_samples_v1 <- ref_norm_res$lst_sample$bridge_samples

    ref_bridge_norm_v1 <- ref_norm_res$lst_norm$bridge_norm$norm |>
      dplyr::arrange(
        .data[["SampleID"]], .data[["OlinkID"]]
      )

    expect_message(
      object = expect_message(
        object = bridge_norm_v1 <- olink_normalization(
          df1 = df_norm_ref_v1,
          df2 = df_norm_not_ref_v1,
          overlapping_samples_df1 = bridge_samples_v1,
          df1_project_nr = "df1_norm",
          df2_project_nr = "df2_norm",
          reference_project = "df1_norm",
          format = TRUE
        ) |>
          dplyr::filter(
            .data[["SampleID"]] %in% .env[["sample_subset"]]
          ) |>
          dplyr::arrange(
            .data[["SampleID"]], .data[["OlinkID"]]
          ),
        regexp = "Bridge normalization will be performed!"
      ),
      regexp = "Output includes two sets of bridging samples"
    )

    expect_equal(
      object = bridge_norm_v1,
      expected = ref_bridge_norm_v1,
      tolerance = 1e-4
    )

    ### bridge - w/ norm col - non-overlapping assays ----

    oid_only_in_ref_v2 <- c("OID01216", "OID01217")
    oid_only_in_nonref_v2 <- c("OID01218", "OID01219")

    df_norm_ref_v2 <- ref_norm_res$lst_df$df1_norm |>
      dplyr::filter(
        !(.data[["OlinkID"]] %in% .env[["oid_only_in_nonref_v2"]])
      )
    df_norm_not_ref_v2 <- ref_norm_res$lst_df$df2_norm |>
      dplyr::filter(
        !(.data[["OlinkID"]] %in% .env[["oid_only_in_ref_v2"]])
      )

    bridge_samples_v2 <- ref_norm_res$lst_sample$bridge_samples

    # preapre expected result by modifying reference result
    ref_bridge_norm_v2 <- ref_norm_res$lst_norm$bridge_norm$norm |>
      # remove non-overlapping assays
      dplyr::filter(
        (!(.data[["OlinkID"]] %in% .env[["oid_only_in_nonref_v2"]])
         & .data[["Project"]] == "df1_norm")
        | (!(.data[["OlinkID"]] %in% .env[["oid_only_in_ref_v2"]])
           & .data[["Project"]] == "df2_norm")
      ) |>
      # remove adjustement for non-overlapping assays
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(c("LOD", "NPX")),
          ~ dplyr::if_else(
            .data[["OlinkID"]] %in% c(.env[["oid_only_in_nonref_v2"]],
                                      .env[["oid_only_in_ref_v2"]]),
            .x - .data[["Adj_factor"]],
            .x
          )
        )
      ) |>
      # set adjustment factor to 0 for non-overlapping assays
      dplyr::mutate(
        Adj_factor = dplyr::if_else(
          .data[["OlinkID"]] %in% c(.env[["oid_only_in_nonref_v2"]],
                                    .env[["oid_only_in_ref_v2"]]),
          0,
          .data[["Adj_factor"]]
        )
      ) |>
      # order data frame for results comparison
      dplyr::arrange(
        .data[["SampleID"]], .data[["OlinkID"]]
      )

    expect_warning(
      object = expect_message(
        object = expect_message(
          object = expect_message(
            object = bridge_norm_v2 <- olink_normalization(
              df1 = df_norm_ref_v2,
              df2 = df_norm_not_ref_v2,
              overlapping_samples_df1 = bridge_samples_v2,
              df1_project_nr = "df1_norm",
              df2_project_nr = "df2_norm",
              reference_project = "df1_norm",
              format = TRUE
            ) |>
              dplyr::filter(
                .data[["SampleID"]] %in% .env[["sample_subset"]]
              ) |>
              dplyr::arrange(
                .data[["SampleID"]], .data[["OlinkID"]]
              ),
            regexp = "Bridge normalization will be performed!"
          ),
          regexp = "Output includes two sets of bridging samples"
        ),
        regexp = paste("4 non-overlapping assays are included in the",
                       "normalized dataset without adjustment")
      ),
      regexp = paste("Assays \"OID01218\", \"OID01219\", \"OID01216\", and",
                     "\"OID01217\" not shared across input dataset")
    )

    expect_equal(
      object = bridge_norm_v2,
      expected = ref_bridge_norm_v2,
      tolerance = 1e-4
    )

    ### bridge - w/ norm column - external controls ----

    df_norm_ref_v3 <- ref_norm_res$lst_df$df1_norm |>
      dplyr::mutate(
        SampleID = dplyr::case_match(
          .data[["SampleID"]],
          "A6" ~ "NEG_CTRL_1",
          "A38" ~ "PLATE_CTRL_1",
          .default = .data[["SampleID"]]
        )
      )
    df_norm_not_ref_v3 <- ref_norm_res$lst_df$df2_norm |>
      dplyr::mutate(
        SampleID = dplyr::case_match(
          .data[["SampleID"]],
          "C66" ~ "NEG_CTRL_2",
          "D1" ~ "PLATE_CTRL_2",
          .default = .data[["SampleID"]]
        )
      )

    bridge_samples_v3 <- ref_norm_res$lst_sample$bridge_samples

    ref_bridge_norm_v3 <- ref_norm_res$lst_norm$bridge_norm$norm |>
      dplyr::mutate(
        SampleID = dplyr::case_match(
          .data[["SampleID"]],
          "A6" ~ "NEG_CTRL_1",
          "A38" ~ "PLATE_CTRL_1",
          "C66" ~ "NEG_CTRL_2",
          "D1" ~ "PLATE_CTRL_2",
          .default = .data[["SampleID"]]
        )
      ) |>
      dplyr::filter(
        !grepl("CTRL", .data[["SampleID"]])
      ) |>
      # order data frame for results comparison
      dplyr::arrange(
        .data[["SampleID"]], .data[["OlinkID"]]
      )

    expect_message(
      object = expect_message(
        object = expect_message(
          object = expect_message(
            object = expect_message(
              object = bridge_norm_v3 <- olink_normalization(
                df1 = df_norm_ref_v3,
                df2 = df_norm_not_ref_v3,
                overlapping_samples_df1 = bridge_samples_v3,
                df1_project_nr = "df1_norm",
                df2_project_nr = "df2_norm",
                reference_project = "df1_norm",
                format = TRUE
              ) |>
                dplyr::filter(
                  .data[["SampleID"]] %in% .env[["sample_subset"]]
                ) |>
                dplyr::arrange(
                  .data[["SampleID"]], .data[["OlinkID"]]
                ),
              regexp = "Bridge normalization will be performed!"
            ),
            regexp = "Output includes two sets of bridging samples"
          ),
          regexp = paste("2 Negative Controls were removed from dataset:",
                         "\"NEG_CTRL_1\" and \"NEG_CTRL_2\"")
        ),
        regexp = paste("2 Plate Controls were removed from dataset:",
                       "\"PLATE_CTRL_1\" and \"PLATE_CTRL_2\"")
      ),
      regexp = paste("Negative Control and Plate Control samples were",
                     "identified and removed based on common patterns in",
                     "sample identifiers")
    )

    expect_equal(
      object = bridge_norm_v3,
      expected = ref_bridge_norm_v3,
      tolerance = 1e-4
    )

    ### bridge - w/ norm col - external controls & non overlapping assays ----

    oid_only_in_ref_v4 <- c("OID01216", "OID01217")
    oid_only_in_nonref_v4 <- c("OID01218", "OID01219")

    df_norm_ref_v4 <- ref_norm_res$lst_df$df1_norm |>
      dplyr::filter(
        !(.data[["OlinkID"]] %in% .env[["oid_only_in_nonref_v4"]])
      ) |>
      dplyr::mutate(
        SampleID = dplyr::case_match(
          .data[["SampleID"]],
          "A6" ~ "NEG_CTRL_1",
          "A38" ~ "PLATE_CTRL_1",
          .default = .data[["SampleID"]]
        )
      )
    df_norm_not_ref_v4 <- ref_norm_res$lst_df$df2_norm |>
      dplyr::filter(
        !(.data[["OlinkID"]] %in% .env[["oid_only_in_ref_v4"]])
      ) |>
      dplyr::mutate(
        SampleID = dplyr::case_match(
          .data[["SampleID"]],
          "C66" ~ "NEG_CTRL_2",
          "D1" ~ "PLATE_CTRL_2",
          .default = .data[["SampleID"]]
        )
      )

    bridge_samples_v4 <- ref_norm_res$lst_sample$bridge_samples

    # preapre expected result by modifying reference result
    ref_bridge_norm_v4 <- ref_norm_res$lst_norm$bridge_norm$norm |>
      # remove non-overlapping assays
      dplyr::filter(
        (!(.data[["OlinkID"]] %in% .env[["oid_only_in_nonref_v4"]])
         & .data[["Project"]] == "df1_norm")
        | (!(.data[["OlinkID"]] %in% .env[["oid_only_in_ref_v4"]])
           & .data[["Project"]] == "df2_norm")
      ) |>
      # remove adjustement for non-overlapping assays
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(c("LOD", "NPX")),
          ~ dplyr::if_else(
            .data[["OlinkID"]] %in% c(.env[["oid_only_in_nonref_v4"]],
                                      .env[["oid_only_in_ref_v4"]]),
            .x - .data[["Adj_factor"]],
            .x
          )
        )
      ) |>
      # set adjustment factor to 0 for non-overlapping assays
      dplyr::mutate(
        Adj_factor = dplyr::if_else(
          .data[["OlinkID"]] %in% c(.env[["oid_only_in_nonref_v4"]],
                                    .env[["oid_only_in_ref_v4"]]),
          0,
          .data[["Adj_factor"]]
        )
      ) |>
      # create controls
      dplyr::mutate(
        SampleID = dplyr::case_match(
          .data[["SampleID"]],
          "A6" ~ "NEG_CTRL_1",
          "A38" ~ "PLATE_CTRL_1",
          "C66" ~ "NEG_CTRL_2",
          "D1" ~ "PLATE_CTRL_2",
          .default = .data[["SampleID"]]
        )
      ) |>
      # remove controls
      dplyr::filter(
        !grepl("CTRL", .data[["SampleID"]])
      ) |>
      # order data frame for results comparison
      dplyr::arrange(
        .data[["SampleID"]], .data[["OlinkID"]]
      )

    expect_warning(
      object = expect_message(
        object = expect_message(
          object = expect_message(
            object = expect_message(
              object = expect_message(
                object = expect_message(
                  object = bridge_norm_v4 <- olink_normalization(
                    df1 = df_norm_ref_v4,
                    df2 = df_norm_not_ref_v4,
                    overlapping_samples_df1 = bridge_samples_v4,
                    df1_project_nr = "df1_norm",
                    df2_project_nr = "df2_norm",
                    reference_project = "df1_norm",
                    format = TRUE
                  ) |>
                    dplyr::filter(
                      .data[["SampleID"]] %in% .env[["sample_subset"]]
                    ) |>
                    dplyr::arrange(
                      .data[["SampleID"]], .data[["OlinkID"]]
                    ),
                  regexp = "Bridge normalization will be performed!"
                ),
                regexp = "Output includes two sets of bridging samples"
              ),
              regexp = paste("4 non-overlapping assays are included in the",
                             "normalized dataset without adjustment")
            ),
            regexp = paste("2 Negative Controls were removed from dataset:",
                           "\"NEG_CTRL_1\" and \"NEG_CTRL_2\"")
          ),
          regexp = paste("2 Plate Controls were removed from dataset:",
                         "\"PLATE_CTRL_1\" and \"PLATE_CTRL_2\"")
        ),
        regexp = paste("Negative Control and Plate Control samples were",
                       "identified and removed based on common patterns in",
                       "sample identifiers")
      ),
      regexp = paste("Assays \"OID01218\", \"OID01219\", \"OID01216\", and",
                     "\"OID01217\" not shared across input dataset")
    )

    expect_equal(
      object = bridge_norm_v4,
      expected = ref_bridge_norm_v4,
      tolerance = 1e-4
    )

  }
)

test_that(
  "olink_normalization - works - intensity normalization",
  {
    skip_if_not(file.exists(test_path("data", "ref_results_norm.rds")))

    ref_norm_res <- get_example_data("ref_results_norm.rds")

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
          reference_project = "df1_no_norm",
          format = FALSE
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
        reference_project = "df1_norm",
        format = FALSE
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
        reference_project = "df1_no_lod",
        format = FALSE
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
          reference_project = "df1_multiple_lod",
          format = FALSE
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
    skip_if_not(file.exists(test_path("data", "ref_results_norm.rds")))

    ref_norm_res <- get_example_data("ref_results_norm.rds")

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
          reference_project = "df1_no_norm",
          format = FALSE
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
        reference_project = "df1_norm",
        format = FALSE
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
        reference_project = "df1_no_lod",
        format = FALSE
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
          reference_project = "df1_multiple_lod",
          format = FALSE
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
  "olink_normalization - works - subset normalization - format",
  {
    skip_if_not(file.exists(test_path("data", "ref_results_norm.rds")))

    ref_norm_res <- get_example_data("ref_results_norm.rds")

    sample_subset <- ref_norm_res$lst_sample$sample_subset

    ### subset - w/ norm col - vanilla ----

    df_norm_ref_v1 <- ref_norm_res$lst_df$df1_norm
    df_norm_not_ref_v1 <- ref_norm_res$lst_df$df2_norm

    subset_samples_ref_v1 <- ref_norm_res$lst_sample$df1_subset
    subset_samples_not_ref_v1 <- ref_norm_res$lst_sample$df2_subset

    subset_int_norm_v1 <- ref_norm_res$lst_norm$subset_norm$norm |>
      dplyr::arrange(
        .data[["SampleID"]], .data[["OlinkID"]]
      )

    expect_message(
      object = subset_norm_v1 <- olink_normalization(
        df1 = df_norm_ref_v1,
        df2 = df_norm_not_ref_v1,
        overlapping_samples_df1 = subset_samples_ref_v1,
        overlapping_samples_df2 = subset_samples_not_ref_v1,
        df1_project_nr = "df1_norm",
        df2_project_nr = "df2_norm",
        reference_project = "df1_norm",
        format = TRUE
      ) |>
        dplyr::filter(
          .data[["SampleID"]] %in% .env[["sample_subset"]]
        ) |>
        dplyr::arrange(
          .data[["SampleID"]], .data[["OlinkID"]]
        ),
      regexp = "Subset normalization will be performed!"
    )

    expect_equal(
      object = subset_norm_v1,
      expected = subset_int_norm_v1,
      tolerance = 1e-4
    )

    ### subset - w/ norm col - non-overlapping assays ----

    oid_only_in_ref_v2 <- c("OID01216", "OID01217")
    oid_only_in_nonref_v2 <- c("OID01218", "OID01219")

    df_norm_ref_v2 <- ref_norm_res$lst_df$df1_norm |>
      dplyr::filter(
        !(.data[["OlinkID"]] %in% .env[["oid_only_in_nonref_v2"]])
      )
    df_norm_not_ref_v2 <- ref_norm_res$lst_df$df2_norm |>
      dplyr::filter(
        !(.data[["OlinkID"]] %in% .env[["oid_only_in_ref_v2"]])
      )

    subset_samples_ref_v2 <- ref_norm_res$lst_sample$df1_subset
    subset_samples_not_ref_v2 <- ref_norm_res$lst_sample$df2_subset

    # preapre expected result by modifying reference result
    ref_subset_norm_v2 <- ref_norm_res$lst_norm$subset_norm$norm |>
      # remove non-overlapping assays
      dplyr::filter(
        (!(.data[["OlinkID"]] %in% .env[["oid_only_in_nonref_v2"]])
         & .data[["Project"]] == "df1_norm")
        | (!(.data[["OlinkID"]] %in% .env[["oid_only_in_ref_v2"]])
           & .data[["Project"]] == "df2_norm")
      ) |>
      # remove adjustement for non-overlapping assays
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(c("LOD", "NPX")),
          ~ dplyr::if_else(
            .data[["OlinkID"]] %in% c(.env[["oid_only_in_nonref_v2"]],
                                      .env[["oid_only_in_ref_v2"]]),
            .x - .data[["Adj_factor"]],
            .x
          )
        )
      ) |>
      # set adjustment factor to 0 for non-overlapping assays
      dplyr::mutate(
        Adj_factor = dplyr::if_else(
          .data[["OlinkID"]] %in% c(.env[["oid_only_in_nonref_v2"]],
                                    .env[["oid_only_in_ref_v2"]]),
          0,
          .data[["Adj_factor"]]
        )
      ) |>
      # order data frame for results comparison
      dplyr::arrange(
        .data[["SampleID"]], .data[["OlinkID"]]
      )

    expect_warning(
      object = expect_message(
        object = expect_message(
          object = subset_norm_v2 <- olink_normalization(
            df1 = df_norm_ref_v2,
            df2 = df_norm_not_ref_v2,
            overlapping_samples_df1 = subset_samples_ref_v2,
            overlapping_samples_df2 = subset_samples_not_ref_v2,
            df1_project_nr = "df1_norm",
            df2_project_nr = "df2_norm",
            reference_project = "df1_norm",
            format = TRUE
          ) |>
            dplyr::filter(
              .data[["SampleID"]] %in% .env[["sample_subset"]]
            ) |>
            dplyr::arrange(
              .data[["SampleID"]], .data[["OlinkID"]]
            ),
          regexp = "Subset normalization will be performed!"
        ),
        regexp = paste("4 non-overlapping assays are included in the",
                       "normalized dataset without adjustment")
      ),
      regexp = paste("Assays \"OID01218\", \"OID01219\", \"OID01216\", and",
                     "\"OID01217\" not shared across input dataset")
    )

    expect_equal(
      object = subset_norm_v2,
      expected = ref_subset_norm_v2,
      tolerance = 1e-4
    )

    ### subset - w/ norm column - external controls ----

    df_norm_ref_v3 <- ref_norm_res$lst_df$df1_norm |>
      dplyr::mutate(
        SampleID = dplyr::case_match(
          .data[["SampleID"]],
          "A6" ~ "NEG_CTRL_1",
          "A38" ~ "PLATE_CTRL_1",
          .default = .data[["SampleID"]]
        )
      )
    df_norm_not_ref_v3 <- ref_norm_res$lst_df$df2_norm |>
      dplyr::mutate(
        SampleID = dplyr::case_match(
          .data[["SampleID"]],
          "C66" ~ "NEG_CTRL_2",
          "D1" ~ "PLATE_CTRL_2",
          .default = .data[["SampleID"]]
        )
      )

    subset_samples_ref_v3 <- ref_norm_res$lst_sample$df1_subset
    subset_samples_not_ref_v3 <- ref_norm_res$lst_sample$df2_subset

    ref_subset_norm_v3 <- ref_norm_res$lst_norm$subset_norm$norm |>
      dplyr::mutate(
        SampleID = dplyr::case_match(
          .data[["SampleID"]],
          "A6" ~ "NEG_CTRL_1",
          "A38" ~ "PLATE_CTRL_1",
          "C66" ~ "NEG_CTRL_2",
          "D1" ~ "PLATE_CTRL_2",
          .default = .data[["SampleID"]]
        )
      ) |>
      dplyr::filter(
        !grepl("CTRL", .data[["SampleID"]])
      ) |>
      # order data frame for results comparison
      dplyr::arrange(
        .data[["SampleID"]], .data[["OlinkID"]]
      )

    expect_message(
      object = expect_message(
        object = expect_message(
          object = expect_message(
            object = subset_norm_v3 <- olink_normalization(
              df1 = df_norm_ref_v3,
              df2 = df_norm_not_ref_v3,
              overlapping_samples_df1 = subset_samples_ref_v3,
              overlapping_samples_df2 = subset_samples_not_ref_v3,
              df1_project_nr = "df1_norm",
              df2_project_nr = "df2_norm",
              reference_project = "df1_norm",
              format = TRUE
            ) |>
              dplyr::filter(
                .data[["SampleID"]] %in% .env[["sample_subset"]]
              ) |>
              dplyr::arrange(
                .data[["SampleID"]], .data[["OlinkID"]]
              ),
            regexp = "Subset normalization will be performed!"
          ),
          regexp = paste("2 Negative Controls were removed from dataset:",
                         "\"NEG_CTRL_1\" and \"NEG_CTRL_2\"")
        ),
        regexp = paste("2 Plate Controls were removed from dataset:",
                       "\"PLATE_CTRL_1\" and \"PLATE_CTRL_2\"")
      ),
      regexp = paste("Negative Control and Plate Control samples were",
                     "identified and removed based on common patterns in",
                     "sample identifiers")
    )

    expect_equal(
      object = subset_norm_v3,
      expected = ref_subset_norm_v3,
      tolerance = 1e-4
    )

    ### subset - w/ norm col - external controls & non overlapping assays ----

    oid_only_in_ref_v4 <- c("OID01216", "OID01217")
    oid_only_in_nonref_v4 <- c("OID01218", "OID01219")

    df_norm_ref_v4 <- ref_norm_res$lst_df$df1_norm |>
      dplyr::filter(
        !(.data[["OlinkID"]] %in% .env[["oid_only_in_nonref_v4"]])
      ) |>
      dplyr::mutate(
        SampleID = dplyr::case_match(
          .data[["SampleID"]],
          "A6" ~ "NEG_CTRL_1",
          "A38" ~ "PLATE_CTRL_1",
          .default = .data[["SampleID"]]
        )
      )
    df_norm_not_ref_v4 <- ref_norm_res$lst_df$df2_norm |>
      dplyr::filter(
        !(.data[["OlinkID"]] %in% .env[["oid_only_in_ref_v4"]])
      ) |>
      dplyr::mutate(
        SampleID = dplyr::case_match(
          .data[["SampleID"]],
          "C66" ~ "NEG_CTRL_2",
          "D1" ~ "PLATE_CTRL_2",
          .default = .data[["SampleID"]]
        )
      )

    subset_samples_ref_v4 <- ref_norm_res$lst_sample$df1_subset
    subset_samples_not_ref_v4 <- ref_norm_res$lst_sample$df2_subset

    # preapre expected result by modifying reference result
    ref_subset_norm_v4 <- ref_norm_res$lst_norm$subset_norm$norm |>
      # remove non-overlapping assays
      dplyr::filter(
        (!(.data[["OlinkID"]] %in% .env[["oid_only_in_nonref_v4"]])
         & .data[["Project"]] == "df1_norm")
        | (!(.data[["OlinkID"]] %in% .env[["oid_only_in_ref_v4"]])
           & .data[["Project"]] == "df2_norm")
      ) |>
      # remove adjustement for non-overlapping assays
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(c("LOD", "NPX")),
          ~ dplyr::if_else(
            .data[["OlinkID"]] %in% c(.env[["oid_only_in_nonref_v4"]],
                                      .env[["oid_only_in_ref_v4"]]),
            .x - .data[["Adj_factor"]],
            .x
          )
        )
      ) |>
      # set adjustment factor to 0 for non-overlapping assays
      dplyr::mutate(
        Adj_factor = dplyr::if_else(
          .data[["OlinkID"]] %in% c(.env[["oid_only_in_nonref_v4"]],
                                    .env[["oid_only_in_ref_v4"]]),
          0,
          .data[["Adj_factor"]]
        )
      ) |>
      # create controls
      dplyr::mutate(
        SampleID = dplyr::case_match(
          .data[["SampleID"]],
          "A6" ~ "NEG_CTRL_1",
          "A38" ~ "PLATE_CTRL_1",
          "C66" ~ "NEG_CTRL_2",
          "D1" ~ "PLATE_CTRL_2",
          .default = .data[["SampleID"]]
        )
      ) |>
      # remove controls
      dplyr::filter(
        !grepl("CTRL", .data[["SampleID"]])
      ) |>
      # order data frame for results comparison
      dplyr::arrange(
        .data[["SampleID"]], .data[["OlinkID"]]
      )

    expect_warning(
      object = expect_message(
        object = expect_message(
          object = expect_message(
            object = expect_message(
              object = expect_message(
                object = subset_norm_v4 <- olink_normalization(
                  df1 = df_norm_ref_v4,
                  df2 = df_norm_not_ref_v4,
                  overlapping_samples_df1 = subset_samples_ref_v4,
                  overlapping_samples_df2 = subset_samples_not_ref_v4,
                  df1_project_nr = "df1_norm",
                  df2_project_nr = "df2_norm",
                  reference_project = "df1_norm",
                  format = TRUE
                ) |>
                  dplyr::filter(
                    .data[["SampleID"]] %in% .env[["sample_subset"]]
                  ) |>
                  dplyr::arrange(
                    .data[["SampleID"]], .data[["OlinkID"]]
                  ),
                regexp = "Subset normalization will be performed!"
              ),
              regexp = paste("4 non-overlapping assays are included in the",
                             "normalized dataset without adjustment")
            ),
            regexp = paste("2 Negative Controls were removed from dataset:",
                           "\"NEG_CTRL_1\" and \"NEG_CTRL_2\"")
          ),
          regexp = paste("2 Plate Controls were removed from dataset:",
                         "\"PLATE_CTRL_1\" and \"PLATE_CTRL_2\"")
        ),
        regexp = paste("Negative Control and Plate Control samples were",
                       "identified and removed based on common patterns in",
                       "sample identifiers")
      ),
      regexp = paste("Assays \"OID01218\", \"OID01219\", \"OID01216\", and",
                     "\"OID01217\" not shared across input dataset")
    )

    expect_equal(
      object = subset_norm_v4,
      expected = ref_subset_norm_v4,
      tolerance = 1e-4
    )

  }
)

test_that(
  "olink_normalization - works - reference median normalization",
  {
    skip_if_not(file.exists(test_path("data", "ref_results_norm.rds")))

    ref_norm_res <- get_example_data("ref_results_norm.rds")

    ### reference median normalization - no norm column ----

    expect_warning(
      object = expect_message(
        object = ref_med_no_norm <- olink_normalization(
          df1 = ref_norm_res$lst_df$df1_no_norm,
          overlapping_samples_df1 = ref_norm_res$lst_sample$df1_subset,
          df1_project_nr = "df1_no_norm",
          reference_medians = ref_norm_res$lst_df$ref_med,
          format = FALSE
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
        reference_medians = ref_norm_res$lst_df$ref_med,
        format = FALSE
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
        reference_medians = ref_norm_res$lst_df$ref_med,
        format = FALSE
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
          reference_medians = ref_norm_res$lst_df$ref_med,
          format = FALSE
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
  "olink_normalization - works - reference median normalization - format",
  {
    skip_if_not(file.exists(test_path("data", "ref_results_norm.rds")))

    ref_norm_res <- get_example_data("ref_results_norm.rds")

    sample_subset <- ref_norm_res$lst_sample$sample_subset

    ### ref med - w/ norm col - vanilla ----

    df_norm_ref_v1 <- ref_norm_res$lst_df$df1_norm
    df_ref_med_v1 <- ref_norm_res$lst_df$ref_med

    subset_samples_v1 <- ref_norm_res$lst_sample$df1_subset

    ref_refmed_norm_v1 <- ref_norm_res$lst_norm$ref_med_norm$norm |>
      dplyr::arrange(
        .data[["SampleID"]], .data[["OlinkID"]]
      )

    expect_message(
      object = refmed_norm_v1 <- olink_normalization(
        df1 = df_norm_ref_v1,
        overlapping_samples_df1 = subset_samples_v1,
        df1_project_nr = "df1_norm",
        reference_medians = df_ref_med_v1,
        format = TRUE
      ) |>
        dplyr::filter(
          .data[["SampleID"]] %in% .env[["sample_subset"]]
        ) |>
        dplyr::arrange(
          .data[["SampleID"]], .data[["OlinkID"]]
        ),
      regexp = "Reference median normalization will be performed!"
    )

    expect_equal(
      object = refmed_norm_v1,
      expected = ref_refmed_norm_v1,
      tolerance = 1e-4
    )

    ### ref med - w/ norm col - non-overlapping assays ----

    oid_only_in_ref_v2 <- c("OID01216", "OID01217")
    oid_only_in_nonref_v2 <- c("OID01218", "OID01219")

    df_norm_ref_v2 <- ref_norm_res$lst_df$df1_norm |>
      dplyr::filter(
        !(.data[["OlinkID"]] %in% .env[["oid_only_in_nonref_v2"]])
      )
    df_ref_med_v2 <- ref_norm_res$lst_df$ref_med |>
      dplyr::filter(
        !(.data[["OlinkID"]] %in% .env[["oid_only_in_ref_v2"]])
      )

    subset_samples_v2 <- ref_norm_res$lst_sample$df1_subset

    # preapre expected result by modifying reference result
    ref_refmed_norm_v2 <- ref_norm_res$lst_norm$ref_med_norm$norm |>
      # remove non-overlapping assays
      dplyr::filter(
        (!(.data[["OlinkID"]] %in% .env[["oid_only_in_nonref_v2"]])
         & .data[["Project"]] == "df1_norm")
        | (!(.data[["OlinkID"]] %in% .env[["oid_only_in_ref_v2"]])
           & .data[["Project"]] == "df2_norm")
      ) |>
      # remove adjustement for non-overlapping assays
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(c("LOD", "NPX")),
          ~ dplyr::if_else(
            .data[["OlinkID"]] %in% c(.env[["oid_only_in_nonref_v2"]],
                                      .env[["oid_only_in_ref_v2"]]),
            .x - .data[["Adj_factor"]],
            .x
          )
        )
      ) |>
      # set adjustment factor to 0 for non-overlapping assays
      dplyr::mutate(
        Adj_factor = dplyr::if_else(
          .data[["OlinkID"]] %in% c(.env[["oid_only_in_nonref_v2"]],
                                    .env[["oid_only_in_ref_v2"]]),
          0,
          .data[["Adj_factor"]]
        )
      ) |>
      # order data frame for results comparison
      dplyr::arrange(
        .data[["SampleID"]], .data[["OlinkID"]]
      )

    expect_warning(
      object = expect_message(
        object = expect_message(
          object = refmed_norm_v2 <- olink_normalization(
            df1 = df_norm_ref_v2,
            overlapping_samples_df1 = subset_samples_v2,
            df1_project_nr = "df1_norm",
            reference_medians = df_ref_med_v2,
            format = TRUE
          ) |>
            dplyr::filter(
              .data[["SampleID"]] %in% .env[["sample_subset"]]
            ) |>
            dplyr::arrange(
              .data[["SampleID"]], .data[["OlinkID"]]
            ),
          regexp = "Reference median normalization will be performed!"
        ),
        regexp = paste("2 non-overlapping assays found in the dataset but not",
                       "in the reference medians")
      ),
      regexp = paste("Assays \"OID01218\", \"OID01219\", \"OID01216\", and",
                     "\"OID01217\" not shared across input dataset")
    )

    expect_equal(
      object = refmed_norm_v2,
      expected = ref_refmed_norm_v2,
      tolerance = 1e-4
    )

    ### ref med - w/ norm column - external controls ----

    df_norm_ref_v3 <- ref_norm_res$lst_df$df1_norm |>
      dplyr::mutate(
        SampleID = dplyr::case_match(
          .data[["SampleID"]],
          "A6" ~ "NEG_CTRL_1",
          "A38" ~ "PLATE_CTRL_1",
          .default = .data[["SampleID"]]
        )
      )
    df_ref_med_v3 <- ref_norm_res$lst_df$ref_med

    subset_samples_v3 <- ref_norm_res$lst_sample$df1_subset

    ref_refmed_norm_v3 <- ref_norm_res$lst_norm$ref_med_norm$norm |>
      dplyr::mutate(
        SampleID = dplyr::case_match(
          .data[["SampleID"]],
          "A6" ~ "NEG_CTRL_1",
          "A38" ~ "PLATE_CTRL_1",
          .default = .data[["SampleID"]]
        )
      ) |>
      dplyr::filter(
        !grepl("CTRL", .data[["SampleID"]])
      ) |>
      # order data frame for results comparison
      dplyr::arrange(
        .data[["SampleID"]], .data[["OlinkID"]]
      )

    expect_message(
      object = expect_message(
        object = expect_message(
          object = expect_message(
            object = refmed_norm_v3 <- olink_normalization(
              df1 = df_norm_ref_v3,
              overlapping_samples_df1 = subset_samples_v3,
              df1_project_nr = "df1_norm",
              reference_medians = df_ref_med_v3,
              format = TRUE
            ) |>
              dplyr::filter(
                .data[["SampleID"]] %in% .env[["sample_subset"]]
              ) |>
              dplyr::arrange(
                .data[["SampleID"]], .data[["OlinkID"]]
              ),
            regexp = "Reference median normalization will be performed!"
          ),
          regexp = paste("1 Negative Control was removed from dataset:",
                         "\"NEG_CTRL_1\"")
        ),
        regexp = paste("1 Plate Control was removed from dataset:",
                       "\"PLATE_CTRL_1\"")
      ),
      regexp = paste("Negative Control and Plate Control samples were",
                     "identified and removed based on common patterns in",
                     "sample identifiers")
    )

    expect_equal(
      object = refmed_norm_v3,
      expected = ref_refmed_norm_v3,
      tolerance = 1e-4
    )

    ### ref med - w/ norm col - external controls & non overlapping assays ----

    oid_only_in_ref_v4 <- c("OID01216", "OID01217")
    oid_only_in_nonref_v4 <- c("OID01218", "OID01219")

    df_norm_ref_v4 <- ref_norm_res$lst_df$df1_norm |>
      dplyr::filter(
        !(.data[["OlinkID"]] %in% .env[["oid_only_in_nonref_v4"]])
      ) |>
      dplyr::mutate(
        SampleID = dplyr::case_match(
          .data[["SampleID"]],
          "A6" ~ "NEG_CTRL_1",
          "A38" ~ "PLATE_CTRL_1",
          .default = .data[["SampleID"]]
        )
      )
    df_ref_med_v4 <- ref_norm_res$lst_df$ref_med |>
      dplyr::filter(
        !(.data[["OlinkID"]] %in% .env[["oid_only_in_ref_v4"]])
      )

    subset_samples_v4 <- ref_norm_res$lst_sample$df1_subset

    # preapre expected result by modifying reference result
    ref_refmed_norm_v4 <- ref_norm_res$lst_norm$ref_med_norm$norm |>
      # remove non-overlapping assays
      dplyr::filter(
        (!(.data[["OlinkID"]] %in% .env[["oid_only_in_nonref_v4"]])
         & .data[["Project"]] == "df1_norm")
        | (!(.data[["OlinkID"]] %in% .env[["oid_only_in_ref_v4"]])
           & .data[["Project"]] == "df2_norm")
      ) |>
      # remove adjustement for non-overlapping assays
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(c("LOD", "NPX")),
          ~ dplyr::if_else(
            .data[["OlinkID"]] %in% c(.env[["oid_only_in_nonref_v4"]],
                                      .env[["oid_only_in_ref_v4"]]),
            .x - .data[["Adj_factor"]],
            .x
          )
        )
      ) |>
      # set adjustment factor to 0 for non-overlapping assays
      dplyr::mutate(
        Adj_factor = dplyr::if_else(
          .data[["OlinkID"]] %in% c(.env[["oid_only_in_nonref_v4"]],
                                    .env[["oid_only_in_ref_v4"]]),
          0,
          .data[["Adj_factor"]]
        )
      ) |>
      # create controls
      dplyr::mutate(
        SampleID = dplyr::case_match(
          .data[["SampleID"]],
          "A6" ~ "NEG_CTRL_1",
          "A38" ~ "PLATE_CTRL_1",
          .default = .data[["SampleID"]]
        )
      ) |>
      # remove controls
      dplyr::filter(
        !grepl("CTRL", .data[["SampleID"]])
      ) |>
      # order data frame for results comparison
      dplyr::arrange(
        .data[["SampleID"]], .data[["OlinkID"]]
      )

    expect_warning(
      object = expect_message(
        object = expect_message(
          object = expect_message(
            object = expect_message(
              object = expect_message(
                object = refmed_norm_v4 <- olink_normalization(
                  df1 = df_norm_ref_v4,
                  overlapping_samples_df1 = subset_samples_v4,
                  df1_project_nr = "df1_norm",
                  reference_medians = df_ref_med_v4,
                  format = TRUE
                ) |>
                  dplyr::filter(
                    .data[["SampleID"]] %in% .env[["sample_subset"]]
                  ) |>
                  dplyr::arrange(
                    .data[["SampleID"]], .data[["OlinkID"]]
                  ),
                regexp = "Reference median normalization will be performed!"
              ),
              regexp = paste("2 non-overlapping assays found in the dataset",
                             "but not in the reference medians")
            ),
            regexp = paste("1 Negative Control was removed from dataset:",
                           "\"NEG_CTRL_1\"")
          ),
          regexp = paste("1 Plate Control was removed from dataset:",
                         "\"PLATE_CTRL_1\"")
        ),
        regexp = paste("Negative Control and Plate Control samples were",
                       "identified and removed based on common patterns in",
                       "sample identifiers")
      ),
      regexp = paste("Assays \"OID01218\", \"OID01219\", \"OID01216\", and",
                     "\"OID01217\" not shared across input dataset")
    )

    expect_equal(
      object = refmed_norm_v4,
      expected = ref_refmed_norm_v4,
      tolerance = 1e-4
    )

  }
)

test_that(
  "olink_normalization - works - 3k-HT normalization",
  {

    skip_if_not(file.exists(test_path("data", "example_3k_data.rds")))
    skip_if_not(file.exists(test_path("data", "example_HT_data.rds")))

    data_3k <- get_example_data(filename = "example_3k_data.rds")
    data_ht <- get_example_data(filename = "example_HT_data.rds")

    ## no format ----

    expect_message(
      object = expect_message(
        expect_warning(
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
            reference_project = "df_ht",
            format = FALSE
          ),
          regexp = "2 assays are not shared across products."
        ),
        regexp = "Cross-product normalization will be performed!"
      ),
      regexp = "Output includes two sets of bridging samples"
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

    ## with format ----

    expect_message(
      object = expect_message(
        object = expect_message(
          object = expect_message(
            object = expect_message(
              object = expect_message(
                expect_warning(
                  object = ht_3k_norm_format <- olink_normalization(
                    df1 = data_ht,
                    df2 = data_3k,
                    overlapping_samples_df1 = intersect(
                      x = unique(data_ht$SampleID),
                      y = unique(data_3k$SampleID)
                    ) |>
                      (\(.) .[!grepl("CONTROL", .)])(),
                    df1_project_nr = "df_ht",
                    df2_project_nr = "df_3k",
                    reference_project = "df_ht",
                    format = TRUE
                  ),
                  regexp = "2 assays are not shared across products."
                ),
                regexp = "Cross-product normalization will be performed!"
              ),
              regexp = "Output includes two sets of bridging samples"
            ),
            regexp = "10 Plate Controls were removed from dataset"
          ),
          regexp = "6 Negative Controls were removed from dataset"
        ),
        regexp = paste("2 not bridgeable assays are included in the",
                       "bridged dataset without adjustment")
      ),
      regexp = paste("2 non-overlapping assays are included in the",
                     "normalized dataset without adjustment")
    )

    expect_identical(
      object = dim(ht_3k_norm_format),
      expected = c(37590L, 19L)
    )

    expect_identical(
      object = names(ht_3k_norm_format),
      expected = c("SampleID", "OlinkID", "SampleType", "WellID", "PlateID",
                   "UniProt", "Assay", "AssayType", "Panel", "Block", "NPX",
                   "PCNormalizedNPX", "Count", "Normalization", "AssayQC",
                   "SampleQC", "DataAnalysisRefID", "Project",
                   "BridgingRecommendation")
    )

  }
)

test_that(
  "olink_normalization - works - 3k-Reveal",
  {
    skip_if_not(file.exists(test_path("data", "example_3k_data.rds")))
    skip_if_not(file.exists(test_path("data", "example_Reveal_data.rds")))

    data_3k <- get_example_data(filename = "example_3k_data.rds")
    data_reveal <- get_example_data(filename = "example_Reveal_data.rds")

    ### no format ----

    expect_message(
      object = expect_warning(
        object = expect_warning(
          object = expect_message(
            object = rev_3k_norm <- olink_normalization(
              df1 = data_reveal,
              df2 = data_3k,
              overlapping_samples_df1 = intersect(
                x = unique(data_reveal$SampleID),
                y = unique(data_3k$SampleID)
              ) |>
                (\(x) x[!grepl("CONTROL", x)])() |>
                sort() |>
                head(32L),
              df1_project_nr = "Reveal",
              df2_project_nr = "3k",
              reference_project = "Reveal",
              format = FALSE
            ),
            regexp = "Cross-product normalization will be performed!"
          ),
          regexp = "85 assays are not shared across products"
        ),
        regexp = "There are 20 assays with fewer than 32 bridge samples for QS"
      ),
      regexp = "Output includes two sets of bridging samples"
    )

    expect_identical(
      object = dim(rev_3k_norm),
      expected = c(8064L, 22L)
    )

    expect_identical(
      object = names(rev_3k_norm),
      expected = c("SampleID", "OlinkID", "SampleType", "WellID", "PlateID",
                   "UniProt", "Assay", "AssayType", "Panel", "Block", "NPX",
                   "PCNormalizedNPX", "Count", "Normalization", "AssayQC",
                   "SampleQC", "DataAnalysisRefID", "Project", "OlinkID_E3072",
                   "MedianCenteredNPX", "QSNormalizedNPX",
                   "BridgingRecommendation")
    )

    ### with formatting ----

    expect_message(
      object = expect_message(
        object = expect_message(
          object = expect_message(
            object = expect_message(
              object = expect_warning(
                object = expect_warning(
                  object = expect_message(
                    object = rev_3k_norm_format <- olink_normalization(
                      df1 = data_reveal,
                      df2 = data_3k,
                      overlapping_samples_df1 = intersect(
                        x = unique(data_reveal$SampleID),
                        y = unique(data_3k$SampleID)
                      ) |>
                        (\(x) x[!grepl("CONTROL", x)])() |>
                        sort() |>
                        head(32L),
                      df1_project_nr = "Reveal",
                      df2_project_nr = "3k",
                      reference_project = "Reveal",
                      format = TRUE
                    ),
                    regexp = "Cross-product normalization will be performed!"
                  ),
                  regexp = "85 assays are not shared across products"
                ),
                regexp = paste("There are 20 assays with fewer than 32 bridge",
                               "samples for QS normalization")
              ),
              regexp = "Output includes two sets of bridging samples"
            ),
            regexp = paste("10 Plate Controls were removed from dataset")
          ),
          regexp = paste("6 Negative Controls were removed from dataset")
        ),
        regexp = paste("26 not bridgeable assays are included in the",
                       "bridged dataset without adjustment")
      ),
      regexp = paste("85 non-overlapping assays are included in the",
                     "normalized dataset without adjustment")
    )

    expect_identical(
      object = dim(rev_3k_norm_format),
      expected = c(22816L, 19L)
    )

    expect_identical(
      object = names(rev_3k_norm_format),
      expected = c("SampleID", "OlinkID", "SampleType", "WellID", "PlateID",
                   "UniProt", "Assay", "AssayType", "Panel", "Block", "NPX",
                   "PCNormalizedNPX", "Count", "Normalization", "AssayQC",
                   "SampleQC", "DataAnalysisRefID", "Project",
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
  "norm_internal_rename_cols - works - no error when AssayQC is missing",
  {
    skip_if_not_installed("arrow")

    # AssayQC in reference only ----

    lst_cnames_v1 <- olink_norm_input_check_df_cols(
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
            Normalization = "Intensity"
          ) |>
          arrow::as_arrow_table()
      )
    )

    update_cnames_v1 <- norm_internal_rename_cols(
      ref_cols = lst_cnames_v1$p1,
      not_ref_cols = lst_cnames_v1$p2,
      not_ref_df = npx_data2 |>
        dplyr::rename(
          "Panel_Lot_Nr" = "Panel_Version"
        ) |>
        dplyr::mutate(
          Normalization = "Intensity"
        ) |>
        arrow::as_arrow_table()
    ) |>
      names()

    # ensure that
    expect_identical(
      object = update_cnames_v1,
      expected = c(names(npx_data2), c("Normalization")) |>
        stringr::str_replace("QC_Warning", "SampleQC")
    )

    # AssayQC in non-reference only ----

    lst_cnames_v2 <- olink_norm_input_check_df_cols(
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
            Normalization = "Intensity"
          ) |>
          arrow::as_arrow_table()
      )
    )

    update_cnames_v2 <- norm_internal_rename_cols(
      ref_cols = lst_cnames_v2$p2,
      not_ref_cols = lst_cnames_v2$p1,
      not_ref_df = npx_data1 |>
        dplyr::rename(
          "SampleQC" = "QC_Warning"
        ) |>
        dplyr::mutate(
          Normalization = "Intensity",
          AssayQC = "Pass"
        ) |>
        arrow::as_arrow_table()
    ) |>
      names()

    # ensure that
    expect_identical(
      object = update_cnames_v2,
      expected = c(names(npx_data2), c("Normalization", "AssayQC")) |>
        stringr::str_replace("Panel_Version", "Panel_Lot_Nr")
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

# Cross-product specific tests ----

test_that(
  "Cross product normalization - works - correlation assays present",
  {
    skip_if_not(file.exists(test_path("data", "example_3k_data.rds")))
    skip_if_not(file.exists(test_path("data", "example_HT_data.rds")))

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

    # correlation assay IDs
    oid_ht <- "OID43204"
    oid_3k <- eHT_e3072_mapping |>
      dplyr::filter(
        .data[["OlinkID_HT"]] == oid_ht
      ) |>
      dplyr::pull(
        .data[["OlinkID_E3072"]]
      )

    # HT correlation is present
    expect_message(
      object = expect_message(
        object = expect_contains(
          object = olink_normalization(
            df1 = data_ht |>
              dplyr::filter(
                !(.data[["OlinkID"]] %in% c("OID12345", "OID54321"))
              ),
            df2 = data_3k |>
              dplyr::filter(
                !(.data[["OlinkID"]] %in% c("OID12345", "OID54321"))
              ),
            overlapping_samples_df1 = bridge_samples,
            df1_project_nr = "proj_ht",
            df2_project_nr = "proj_3k",
            reference_project = "proj_ht"
          ) |>
            dplyr::distinct(OlinkID) |>
            dplyr::pull(),
          expected = oid_ht
        ),
        regexp = "Cross-product normalization will be performed!"
      ),
      regexp = "Output includes two sets of bridging samples"
    )

    # All 3k correlations are present
    expect_message(
      object = expect_message(
        object = expect_contains(
          object = olink_normalization(
            df1 = data_ht |>
              dplyr::filter(
                !(.data[["OlinkID"]] %in% c("OID12345", "OID54321"))
              ),
            df2 = data_3k |>
              dplyr::filter(
                !(.data[["OlinkID"]] %in% c("OID12345", "OID54321"))
              ),
            overlapping_samples_df1 = bridge_samples,
            df1_project_nr = "proj_ht",
            df2_project_nr = "proj_3k",
            reference_project = "proj_ht"
          ) |>
            dplyr::distinct(OlinkID_E3072) |>
            dplyr::pull(),
          expected = oid_3k
        ),
        regexp = "Cross-product normalization will be performed!"
      ),
      regexp = "Output includes two sets of bridging samples"
    )
  }
)

test_that(
  "norm_internal_cross_product missing counts - error",
  {

    skip_if_not(file.exists(test_path("data", "example_3k_data.rds")))
    skip_if_not(file.exists(test_path("data", "example_HT_data.rds")))

    data_3k <- get_example_data(filename = "example_3k_data.rds")
    data_ht <- get_example_data(filename = "example_HT_data.rds")

    ## error v1: reference/HT and new/3K DF missing count column ----

    expect_error(
      object = olink_normalization(
        df1 = data_ht |> dplyr::select(-dplyr::all_of("Count")),
        df2 = data_3k |> dplyr::select(-dplyr::all_of("Count")),
        overlapping_samples_df1 = intersect(data_ht$SampleID, data_3k$SampleID),
        df1_project_nr = "proj_ht",
        df2_project_nr = "proj_3k",
        reference_project = "proj_ht"
      ),
      regexp = "Column \"Count\" not found in datasets \"proj_ht\" and"
    )

    ## error v2: reference/HT DF missing count column ----

    expect_error(
      object = olink_normalization(
        df1 = data_ht |> dplyr::select(-dplyr::all_of("Count")),
        df2 = data_3k,
        overlapping_samples_df1 = intersect(data_ht$SampleID, data_3k$SampleID),
        df1_project_nr = "proj_ht",
        df2_project_nr = "proj_3k",
        reference_project = "proj_ht"
      ),
      regexp = "Column \"Count\" not found in dataset \"proj_ht\"!"
    )

    ## error v3: new/3K DF missing count column ----

    expect_error(
      object = olink_normalization(
        df1 = data_ht,
        df2 = data_3k |> dplyr::select(-dplyr::all_of("Count")),
        overlapping_samples_df1 = intersect(data_ht$SampleID, data_3k$SampleID),
        df1_project_nr = "proj_ht",
        df2_project_nr = "proj_3k",
        reference_project = "proj_ht"
      ),
      regexp = "Column \"Count\" not found in dataset \"proj_3k\"!"
    )

  }
)

test_that(
  "olink_normalization - works - old3kformat-HT normalization",
  {

    skip_if_not(file.exists(test_path("data", "example_3k_data.rds")))
    skip_if_not(file.exists(test_path("data", "example_HT_data.rds")))

    data_3k <- get_example_data(filename = "example_3k_data.rds")
    data_ht <- get_example_data(filename = "example_HT_data.rds")

    data_3k_old <- data_3k |>
      dplyr::rename(
        "Sample_Type" = "SampleType",
        "QC_Warning" = "SampleQC",
        "Assay_Warning" = "AssayQC"
      )

    expect_message(
      object = expect_warning(
        object = expect_message(
          object = ht_3k_norm <- olink_normalization(
            df1 = data_ht,
            df2 = data_3k_old,
            overlapping_samples_df1 = intersect(
              x = unique(data_ht$SampleID),
              y = unique(data_3k_old$SampleID)
            ) |>
              (\(.) .[!grepl("CONTROL", .)])(),
            df1_project_nr = "df_ht",
            df2_project_nr = "df_3k",
            reference_project = "df_ht"
          ),
          regexp = "Cross-product normalization will be performed!"
        ),
        regexp = "2 assays are not shared across products."
      ),
      regexp = "Output includes two sets of bridging samples"
    )

    expect_identical(
      object = dim(ht_3k_norm),
      expected = c(39936L, 23L)
    )

    expect_identical(
      object = names(ht_3k_norm),
      expected = c("SampleID", "OlinkID", "SampleType", "WellID", "PlateID",
                   "UniProt", "Assay", "AssayType", "Panel", "Block", "NPX",
                   "PCNormalizedNPX", "Count", "Normalization", "AssayQC",
                   "SampleQC", "DataAnalysisRefID", "Project", "OlinkID_E3072",
                   "Sample_Type", "MedianCenteredNPX", "QSNormalizedNPX",
                   "BridgingRecommendation")
    )

  }
)
