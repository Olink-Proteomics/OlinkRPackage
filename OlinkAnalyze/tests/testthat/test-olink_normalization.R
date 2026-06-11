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
    ref_norm_res <- get_example_data("ref_results_norm.rds")

    ### bridge normalization - no norm column ----

    expect_warning(
      object = expect_message(
        object = expect_warning(
          object = expect_message(
            object = bridge_no_norm <- olink_normalization(
              df1 = ref_norm_res$lst_df$df1_no_norm,
              df2 = ref_norm_res$lst_df$df2_no_norm,
              overlapping_samples_df1 = ref_norm_res$lst_sample$bridge_samples,
              df1_project_nr = "df1_no_norm",
              df2_project_nr = "df2_no_norm",
              reference_project = "df1_no_norm",
              format = FALSE,
              df1_check_log = check_npx(df = ref_norm_res$lst_df$df1_no_norm) |>
                suppressMessages() |>
                suppressWarnings(),
              df2_check_log = check_npx(df = ref_norm_res$lst_df$df2_no_norm) |>
                suppressMessages() |>
                suppressWarnings()
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
      ),
      regexp = paste("Duplicate SampleIDs detected: \"A13\", \"A29\", \"A30\",",
                     "\"A36\", \"A45\", \"A46\", \"A52\", \"A63\", \"A71\",",
                     "\"A73\", \"B3\", \"B37\", \"B4\", \"B45\", \"B63\",",
                     "\"B75\", \"CONTROL_SAMPLE_AS 1\", and",
                     "\"CONTROL_SAMPLE_AS 2\"")
    )

    expect_equal(
      object = strip_check_log(df = bridge_no_norm),
      expected = ref_norm_res$lst_norm$bridge_norm$no_norm,
      tolerance = 1e-4
    )

    ### bridge normalization - with norm column ----

    expect_warning(
      object = expect_message(
        object = expect_message(
          object = bridge_norm <- olink_normalization(
            df1 = ref_norm_res$lst_df$df1_norm,
            df2 = ref_norm_res$lst_df$df2_norm,
            overlapping_samples_df1 = ref_norm_res$lst_sample$bridge_samples,
            df1_project_nr = "df1_norm",
            df2_project_nr = "df2_norm",
            reference_project = "df1_norm",
            format = FALSE,
            df1_check_log = check_npx(df = ref_norm_res$lst_df$df1_norm) |>
              suppressMessages() |>
              suppressWarnings(),
            df2_check_log = check_npx(df = ref_norm_res$lst_df$df2_norm) |>
              suppressMessages() |>
              suppressWarnings()
          ) |>
            dplyr::filter(
              .data[["SampleID"]] %in% ref_norm_res$lst_sample$sample_subset
            ),
          regexp = "Bridge normalization will be performed!"
        ),
        regexp = "Output includes two sets of bridging samples"
      ),
      regexp = paste("Duplicate SampleIDs detected: \"A13\", \"A29\", \"A30\",",
                     "\"A36\", \"A45\", \"A46\", \"A52\", \"A63\", \"A71\",",
                     "\"A73\", \"B3\", \"B37\", \"B4\", \"B45\", \"B63\",",
                     "\"B75\", \"CONTROL_SAMPLE_AS 1\", and",
                     "\"CONTROL_SAMPLE_AS 2\"")
    )

    expect_equal(
      object = strip_check_log(df = bridge_norm),
      expected = ref_norm_res$lst_norm$bridge_norm$norm,
      tolerance = 1e-4
    )

    ### bridge normalization - no lod column ----

    expect_warning(
      object = expect_message(
        object = expect_message(
          object = bridge_no_lod <- olink_normalization(
            df1 = ref_norm_res$lst_df$df1_no_lod,
            df2 = ref_norm_res$lst_df$df2_no_lod,
            overlapping_samples_df1 = ref_norm_res$lst_sample$bridge_samples,
            df1_project_nr = "df1_no_lod",
            df2_project_nr = "df2_no_lod",
            reference_project = "df1_no_lod",
            format = FALSE,
            df1_check_log = check_npx(df = ref_norm_res$lst_df$df1_no_lod) |>
              suppressMessages() |>
              suppressWarnings(),
            df2_check_log = check_npx(df = ref_norm_res$lst_df$df2_no_lod) |>
              suppressMessages() |>
              suppressWarnings()
          ) |>
            dplyr::filter(
              .data[["SampleID"]] %in% ref_norm_res$lst_sample$sample_subset
            ),
          regexp = "Bridge normalization will be performed!"
        ),
        regexp = "Output includes two sets of bridging samples"
      ),
      regexp = paste("Duplicate SampleIDs detected: \"A13\", \"A29\", \"A30\",",
                     "\"A36\", \"A45\", \"A46\", \"A52\", \"A63\", \"A71\",",
                     "\"A73\", \"B3\", \"B37\", \"B4\", \"B45\", \"B63\",",
                     "\"B75\", \"CONTROL_SAMPLE_AS 1\", and",
                     "\"CONTROL_SAMPLE_AS 2\"")
    )

    expect_equal(
      object = strip_check_log(df = bridge_no_lod),
      expected = ref_norm_res$lst_norm$bridge_norm$no_lod,
      tolerance = 1e-4
    )

    ### bridge normalization - multiple lod columns ----

    expect_warning(
      object = expect_message(
        object = expect_message(
          object = expect_message(
            object = bridge_multiple_lod <- olink_normalization(
              df1 = ref_norm_res$lst_df$df1_multiple_lod,
              df2 = ref_norm_res$lst_df$df2_multiple_lod,
              overlapping_samples_df1 = ref_norm_res$lst_sample$bridge_samples,
              df1_project_nr = "df1_multiple_lod",
              df2_project_nr = "df2_multiple_lod",
              reference_project = "df1_multiple_lod",
              format = FALSE,
              df1_check_log = check_npx(
                df = ref_norm_res$lst_df$df1_multiple_lod
              ) |>
                suppressMessages() |>
                suppressWarnings(),
              df2_check_log = check_npx(
                df = ref_norm_res$lst_df$df2_multiple_lod
              ) |>
                suppressMessages() |>
                suppressWarnings()
            ) |>
              dplyr::filter(
                .data[["SampleID"]] %in% ref_norm_res$lst_sample$sample_subset
              ),
            regexp = "Bridge normalization will be performed!"
          ),
          regexp = paste("Datasets \"df1_multiple_lod\" and",
                         "\"df2_multiple_lod\" contain multiple columns",
                         "matching")
        ),
        regexp = "Output includes two sets of bridging samples"
      ),
      regexp = paste("Duplicate SampleIDs detected: \"A13\", \"A29\", \"A30\",",
                     "\"A36\", \"A45\", \"A46\", \"A52\", \"A63\", \"A71\",",
                     "\"A73\", \"B3\", \"B37\", \"B4\", \"B45\", \"B63\",",
                     "\"B75\", \"CONTROL_SAMPLE_AS 1\", and",
                     "\"CONTROL_SAMPLE_AS 2\"")
    )

    expect_equal(
      object = strip_check_log(df = bridge_multiple_lod),
      expected = ref_norm_res$lst_norm$bridge_norm$multiple_lod,
      tolerance = 1e-4
    )

    ### olink_class ----

    expect_no_error(
      object = expect_no_message(
        object = expect_warning(
          object = df1_no_norm_obj <- attach_check_log(
            df = ref_norm_res$lst_df$df1_no_norm,
            out_df = "tibble"
          ),
          regexp = paste("Duplicate SampleIDs detected: \"CONTROL_SAMPLE_AS",
                         "1\" and \"CONTROL_SAMPLE_AS 2\"")
        )
      )
    )

    expect_no_error(
      object = expect_no_message(
        object = expect_warning(
          object = df2_no_norm_obj <- attach_check_log(
            df = ref_norm_res$lst_df$df2_no_norm,
            out_df = "tibble"
          ),
          regexp = paste("Duplicate SampleIDs detected: \"CONTROL_SAMPLE_AS",
                         "1\" and \"CONTROL_SAMPLE_AS 2\"")
        )
      )
    )

    expect_warning(
      object = expect_message(
        object = expect_warning(
          object = expect_message(
            object = bridge_no_norm_obj <- olink_normalization(
              df1 = df1_no_norm_obj,
              df2 = df2_no_norm_obj,
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
      ),
      regexp = paste("Duplicate SampleIDs detected: \"A13\", \"A29\", \"A30\",",
                     "\"A36\", \"A45\", \"A46\", \"A52\", \"A63\", \"A71\",",
                     "\"A73\", \"B3\", \"B37\", \"B4\", \"B45\", \"B63\",",
                     "\"B75\", \"CONTROL_SAMPLE_AS 1\", and",
                     "\"CONTROL_SAMPLE_AS 2\"")
    )

    expect_s3_class(object = bridge_no_norm_obj, class = "olink_class")
    expect_s3_class(object = bridge_no_norm_obj, class = "tbl_df")

    expect_equal(
      object = strip_check_log(df = bridge_no_norm_obj),
      expected = ref_norm_res$lst_norm$bridge_norm$no_norm,
      tolerance = 1e-4
    )

    ### olink arrow ----

    expect_no_error(
      object = expect_no_message(
        object = expect_warning(
          object = df1_no_norm_arrow <- attach_check_log(
            df = ref_norm_res$lst_df$df1_no_norm,
            out_df = "arrow"
          ),
          regexp = paste("Duplicate SampleIDs detected: \"CONTROL_SAMPLE_AS",
                         "1\" and \"CONTROL_SAMPLE_AS 2\"")
        )
      )
    )

    expect_no_error(
      object = expect_no_message(
        object = expect_warning(
          object = df2_no_norm_arrow <- attach_check_log(
            df = ref_norm_res$lst_df$df2_no_norm,
            out_df = "arrow"
          ),
          regexp = paste("Duplicate SampleIDs detected: \"CONTROL_SAMPLE_AS",
                         "1\" and \"CONTROL_SAMPLE_AS 2\"")
        )
      )
    )

    expect_warning(
      object = expect_message(
        object = expect_warning(
          object = expect_message(
            object = bridge_no_norm_arrow <- olink_normalization(
              df1 = df1_no_norm_obj,
              df2 = df2_no_norm_obj,
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
      ),
      regexp = paste("Duplicate SampleIDs detected: \"A13\", \"A29\", \"A30\",",
                     "\"A36\", \"A45\", \"A46\", \"A52\", \"A63\", \"A71\",",
                     "\"A73\", \"B3\", \"B37\", \"B4\", \"B45\", \"B63\",",
                     "\"B75\", \"CONTROL_SAMPLE_AS 1\", and",
                     "\"CONTROL_SAMPLE_AS 2\"")
    )

    expect_s3_class(object = bridge_no_norm_arrow, class = "olink_class")
    expect_s3_class(object = bridge_no_norm_arrow, class = "tbl_df")

    expect_equal(
      object = strip_check_log(df = bridge_no_norm_arrow),
      expected = ref_norm_res$lst_norm$bridge_norm$no_norm,
      tolerance = 1e-4
    )
  }
)

test_that(
  "olink_normalization - works - bridge normalization - format",
  {
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

    expect_warning(
      object = expect_message(
        object = expect_message(
          object = bridge_norm_v1 <- olink_normalization(
            df1 = df_norm_ref_v1,
            df2 = df_norm_not_ref_v1,
            overlapping_samples_df1 = bridge_samples_v1,
            df1_project_nr = "df1_norm",
            df2_project_nr = "df2_norm",
            reference_project = "df1_norm",
            format = TRUE,
            df1_check_log = check_npx(df = df_norm_ref_v1) |>
              suppressMessages() |>
              suppressWarnings(),
            df2_check_log = check_npx(df = df_norm_not_ref_v1) |>
              suppressMessages() |>
              suppressWarnings()
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
      regexp = paste("Duplicate SampleIDs detected: \"A13\", \"A29\", \"A30\",",
                     "\"A36\", \"A45\", \"A46\", \"A52\", \"A63\", \"A71\",",
                     "\"A73\", \"B3\", \"B37\", \"B4\", \"B45\", \"B63\",",
                     "\"B75\", \"CONTROL_SAMPLE_AS 1\", and",
                     "\"CONTROL_SAMPLE_AS 2\"")
    )

    expect_equal(
      object = strip_check_log(df = bridge_norm_v1),
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

    # prepare expected result by modifying reference result
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
      object = expect_warning(
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
                format = TRUE,
                df1_check_log = check_npx(df = df_norm_ref_v2) |>
                  suppressMessages() |>
                  suppressWarnings(),
                df2_check_log = check_npx(df = df_norm_not_ref_v2) |>
                  suppressMessages() |>
                  suppressWarnings()
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
      ),
      regexp = paste("Duplicate SampleIDs detected: \"A13\", \"A29\", \"A30\",",
                     "\"A36\", \"A45\", \"A46\", \"A52\", \"A63\", \"A71\",",
                     "\"A73\", \"B3\", \"B37\", \"B4\", \"B45\", \"B63\",",
                     "\"B75\", \"CONTROL_SAMPLE_AS 1\", and",
                     "\"CONTROL_SAMPLE_AS 2\"")
    )

    expect_equal(
      object = strip_check_log(df = bridge_norm_v2),
      expected = ref_bridge_norm_v2,
      tolerance = 1e-4
    )

    ### bridge - w/ norm column - external controls ----

    df_norm_ref_v3 <- ref_norm_res$lst_df$df1_norm |>
      dplyr::mutate(
        SampleID = dplyr::recode_values(
          .data[["SampleID"]],
          "A6" ~ "NEG_CTRL_1",
          "A38" ~ "PLATE_CTRL_1",
          default = .data[["SampleID"]]
        )
      )
    df_norm_not_ref_v3 <- ref_norm_res$lst_df$df2_norm |>
      dplyr::mutate(
        SampleID = dplyr::recode_values(
          .data[["SampleID"]],
          "C66" ~ "NEG_CTRL_2",
          "D1" ~ "PLATE_CTRL_2",
          default = .data[["SampleID"]]
        )
      )

    bridge_samples_v3 <- ref_norm_res$lst_sample$bridge_samples

    ref_bridge_norm_v3 <- ref_norm_res$lst_norm$bridge_norm$norm |>
      dplyr::mutate(
        SampleID = dplyr::recode_values(
          .data[["SampleID"]],
          "A6" ~ "NEG_CTRL_1",
          "A38" ~ "PLATE_CTRL_1",
          "C66" ~ "NEG_CTRL_2",
          "D1" ~ "PLATE_CTRL_2",
          default = .data[["SampleID"]]
        )
      ) |>
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
                object = bridge_norm_v3 <- olink_normalization(
                  df1 = df_norm_ref_v3,
                  df2 = df_norm_not_ref_v3,
                  overlapping_samples_df1 = bridge_samples_v3,
                  df1_project_nr = "df1_norm",
                  df2_project_nr = "df2_norm",
                  reference_project = "df1_norm",
                  format = TRUE,
                  df1_check_log = check_npx(df = df_norm_ref_v3) |>
                    suppressMessages() |>
                    suppressWarnings(),
                  df2_check_log = check_npx(df = df_norm_not_ref_v3) |>
                    suppressMessages() |>
                    suppressWarnings()
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
      ),
      regexp = paste("Duplicate SampleIDs detected: \"A13\", \"A29\", \"A30\",",
                     "\"A36\", \"A45\", \"A46\", \"A52\", \"A63\", \"A71\",",
                     "\"A73\", \"B3\", \"B37\", \"B4\", \"B45\", \"B63\",",
                     "\"B75\", \"CONTROL_SAMPLE_AS 1\", and",
                     "\"CONTROL_SAMPLE_AS 2\"")
    )

    expect_equal(
      object = strip_check_log(df = bridge_norm_v3),
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
        SampleID = dplyr::recode_values(
          .data[["SampleID"]],
          "A6" ~ "NEG_CTRL_1",
          "A38" ~ "PLATE_CTRL_1",
          default = .data[["SampleID"]]
        )
      )
    df_norm_not_ref_v4 <- ref_norm_res$lst_df$df2_norm |>
      dplyr::filter(
        !(.data[["OlinkID"]] %in% .env[["oid_only_in_ref_v4"]])
      ) |>
      dplyr::mutate(
        SampleID = dplyr::recode_values(
          .data[["SampleID"]],
          "C66" ~ "NEG_CTRL_2",
          "D1" ~ "PLATE_CTRL_2",
          default = .data[["SampleID"]]
        )
      )

    bridge_samples_v4 <- ref_norm_res$lst_sample$bridge_samples

    # prepare expected result by modifying reference result
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
        SampleID = dplyr::recode_values(
          .data[["SampleID"]],
          "A6" ~ "NEG_CTRL_1",
          "A38" ~ "PLATE_CTRL_1",
          "C66" ~ "NEG_CTRL_2",
          "D1" ~ "PLATE_CTRL_2",
          default = .data[["SampleID"]]
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
      object = expect_warning(
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
                      format = TRUE,
                      df1_check_log = check_npx(df = df_norm_ref_v4) |>
                        suppressMessages() |>
                        suppressWarnings(),
                      df2_check_log = check_npx(df = df_norm_not_ref_v4) |>
                        suppressMessages() |>
                        suppressWarnings()
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
      ),
      regexp = paste("Duplicate SampleIDs detected: \"A13\", \"A29\", \"A30\",",
                     "\"A36\", \"A45\", \"A46\", \"A52\", \"A63\", \"A71\",",
                     "\"A73\", \"B3\", \"B37\", \"B4\", \"B45\", \"B63\",",
                     "\"B75\", \"CONTROL_SAMPLE_AS 1\", and",
                     "\"CONTROL_SAMPLE_AS 2\"")
    )

    expect_equal(
      object = strip_check_log(df = bridge_norm_v4),
      expected = ref_bridge_norm_v4,
      tolerance = 1e-4
    )

    ### olink_class ----

    expect_no_error(
      object = expect_no_message(
        object = expect_warning(
          object = df_norm_ref_v1_obj <- attach_check_log(
            df = df_norm_ref_v1,
            out_df = "tibble"
          ),
          regexp = paste("Duplicate SampleIDs detected: \"CONTROL_SAMPLE_AS",
                         "1\" and \"CONTROL_SAMPLE_AS 2\"")
        )
      )
    )

    expect_no_error(
      object = expect_no_message(
        object = expect_warning(
          object = df_norm_not_ref_v1_obj <- attach_check_log(
            df = df_norm_not_ref_v1,
            out_df = "tibble"
          ),
          regexp = paste("Duplicate SampleIDs detected: \"CONTROL_SAMPLE_AS",
                         "1\" and \"CONTROL_SAMPLE_AS 2\"")
        )
      )
    )

    expect_warning(
      object = expect_message(
        object = expect_message(
          object = bridge_no_norm_obj <- olink_normalization(
            df1 = df_norm_ref_v1_obj,
            df2 = df_norm_not_ref_v1_obj,
            overlapping_samples_df1 = ref_norm_res$lst_sample$bridge_samples,
            df1_project_nr = "df1_norm",
            df2_project_nr = "df2_norm",
            reference_project = "df1_norm",
            format = FALSE
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
      regexp = paste("Duplicate SampleIDs detected: \"A13\", \"A29\", \"A30\",",
                     "\"A36\", \"A45\", \"A46\", \"A52\", \"A63\", \"A71\",",
                     "\"A73\", \"B3\", \"B37\", \"B4\", \"B45\", \"B63\",",
                     "\"B75\", \"CONTROL_SAMPLE_AS 1\", and",
                     "\"CONTROL_SAMPLE_AS 2\"")
    )

    expect_s3_class(object = bridge_no_norm_obj, class = "olink_class")
    expect_s3_class(object = bridge_no_norm_obj, class = "tbl_df")

    expect_equal(
      object = strip_check_log(df = bridge_no_norm_obj),
      expected = ref_bridge_norm_v1,
      tolerance = 1e-4
    )

    ### olink arrow ----

    expect_no_error(
      object = expect_no_message(
        object = expect_warning(
          object = df_norm_ref_v1_arrow <- attach_check_log(
            df = df_norm_ref_v1,
            out_df = "arrow"
          ),
          regexp = paste("Duplicate SampleIDs detected: \"CONTROL_SAMPLE_AS",
                         "1\" and \"CONTROL_SAMPLE_AS 2\"")
        )
      )
    )

    expect_no_error(
      object = expect_no_message(
        object = expect_warning(
          object = df_norm_not_ref_v1_arrow <- attach_check_log(
            df = df_norm_not_ref_v1,
            out_df = "arrow"
          ),
          regexp = paste("Duplicate SampleIDs detected: \"CONTROL_SAMPLE_AS",
                         "1\" and \"CONTROL_SAMPLE_AS 2\"")
        )
      )
    )

    expect_warning(
      object = expect_message(
        object = expect_message(
          object = bridge_no_norm_arrow <- olink_normalization(
            df1 = df_norm_ref_v1_arrow,
            df2 = df_norm_not_ref_v1_arrow,
            overlapping_samples_df1 = ref_norm_res$lst_sample$bridge_samples,
            df1_project_nr = "df1_norm",
            df2_project_nr = "df2_norm",
            reference_project = "df1_norm",
            format = FALSE
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
      regexp = paste("Duplicate SampleIDs detected: \"A13\", \"A29\", \"A30\",",
                     "\"A36\", \"A45\", \"A46\", \"A52\", \"A63\", \"A71\",",
                     "\"A73\", \"B3\", \"B37\", \"B4\", \"B45\", \"B63\",",
                     "\"B75\", \"CONTROL_SAMPLE_AS 1\", and",
                     "\"CONTROL_SAMPLE_AS 2\"")
    )

    expect_s3_class(object = bridge_no_norm_arrow, class = "olink_class")
    expect_s3_class(object = bridge_no_norm_arrow, class = "tbl_df")

    expect_equal(
      object = strip_check_log(df = bridge_no_norm_arrow),
      expected = ref_bridge_norm_v1,
      tolerance = 1e-4
    )

  }
)

test_that(
  "olink_normalization - works - intensity normalization",
  {
    ref_norm_res <- get_example_data("ref_results_norm.rds")

    ### intensity normalization - no norm column ----

    expect_warning(
      object = expect_warning(
        object = expect_message(
          object = intensity_no_norm <- olink_normalization(
            df1 = ref_norm_res$lst_df$df1_no_norm,
            df2 = ref_norm_res$lst_df$df2_no_norm,
            overlapping_samples_df1 = ref_norm_res$lst_sample$df1_all,
            overlapping_samples_df2 = ref_norm_res$lst_sample$df2_all,
            df1_project_nr = "df1_no_norm",
            df2_project_nr = "df2_no_norm",
            reference_project = "df1_no_norm",
            format = FALSE,
            df1_check_log = check_npx(df = ref_norm_res$lst_df$df1_no_norm) |>
              suppressMessages() |>
              suppressWarnings(),
            df2_check_log = check_npx(df = ref_norm_res$lst_df$df2_no_norm) |>
              suppressMessages() |>
              suppressWarnings()
          ) |>
            dplyr::filter(
              .data[["SampleID"]] %in% ref_norm_res$lst_sample$sample_subset
            ),
          regexp = "Subset normalization will be performed!"
        ),
        regexp = paste("Datasets \"df1_no_norm\" and \"df2_no_norm\" do not",
                       "contain a column named \"Normalization\"")
      ),
      regexp = paste("Duplicate SampleIDs detected: \"A13\", \"A29\", \"A30\",",
                     "\"A36\", \"A45\", \"A46\", \"A52\", \"A63\", \"A71\",",
                     "\"A73\", \"B3\", \"B37\", \"B4\", \"B45\", \"B63\",",
                     "\"B75\", \"CONTROL_SAMPLE_AS 1\", and",
                     "\"CONTROL_SAMPLE_AS 2\"")
    )

    expect_equal(
      object = strip_check_log(df = intensity_no_norm),
      expected = ref_norm_res$lst_norm$intensity_norm$no_norm,
      tolerance = 1e-4
    )

    ### intensity normalization - with norm column ----

    expect_warning(
      object = expect_message(
        object = intensity_norm <- olink_normalization(
          df1 = ref_norm_res$lst_df$df1_norm,
          df2 = ref_norm_res$lst_df$df2_norm,
          overlapping_samples_df1 = ref_norm_res$lst_sample$df1_all,
          overlapping_samples_df2 = ref_norm_res$lst_sample$df2_all,
          df1_project_nr = "df1_norm",
          df2_project_nr = "df2_norm",
          reference_project = "df1_norm",
          format = FALSE,
          df1_check_log = check_npx(df = ref_norm_res$lst_df$df1_norm) |>
            suppressMessages() |>
            suppressWarnings(),
          df2_check_log = check_npx(df = ref_norm_res$lst_df$df2_norm) |>
            suppressMessages() |>
            suppressWarnings()
        ) |>
          dplyr::filter(
            .data[["SampleID"]] %in% ref_norm_res$lst_sample$sample_subset
          ),
        regexp = "Subset normalization will be performed!"
      ),
      regexp = paste("Duplicate SampleIDs detected: \"A13\", \"A29\", \"A30\",",
                     "\"A36\", \"A45\", \"A46\", \"A52\", \"A63\", \"A71\",",
                     "\"A73\", \"B3\", \"B37\", \"B4\", \"B45\", \"B63\",",
                     "\"B75\", \"CONTROL_SAMPLE_AS 1\", and",
                     "\"CONTROL_SAMPLE_AS 2\"")
    )

    expect_equal(
      object = strip_check_log(df = intensity_norm),
      expected = ref_norm_res$lst_norm$intensity_norm$norm,
      tolerance = 1e-4
    )

    ### intensity normalization - no lod column ----

    expect_warning(
      object = expect_message(
        object = intensity_no_lod <- olink_normalization(
          df1 = ref_norm_res$lst_df$df1_no_lod,
          df2 = ref_norm_res$lst_df$df2_no_lod,
          overlapping_samples_df1 = ref_norm_res$lst_sample$df1_all,
          overlapping_samples_df2 = ref_norm_res$lst_sample$df2_all,
          df1_project_nr = "df1_no_lod",
          df2_project_nr = "df2_no_lod",
          reference_project = "df1_no_lod",
          format = FALSE,
          df1_check_log = check_npx(df = ref_norm_res$lst_df$df1_no_lod) |>
            suppressMessages() |>
            suppressWarnings(),
          df2_check_log = check_npx(df = ref_norm_res$lst_df$df2_no_lod) |>
            suppressMessages() |>
            suppressWarnings()
        ) |>
          dplyr::filter(
            .data[["SampleID"]] %in% ref_norm_res$lst_sample$sample_subset
          ),
        regexp = "Subset normalization will be performed!"
      ),
      regexp = paste("Duplicate SampleIDs detected: \"A13\", \"A29\", \"A30\",",
                     "\"A36\", \"A45\", \"A46\", \"A52\", \"A63\", \"A71\",",
                     "\"A73\", \"B3\", \"B37\", \"B4\", \"B45\", \"B63\",",
                     "\"B75\", \"CONTROL_SAMPLE_AS 1\", and",
                     "\"CONTROL_SAMPLE_AS 2\"")
    )

    expect_equal(
      object = strip_check_log(df = intensity_no_lod),
      expected = ref_norm_res$lst_norm$intensity_norm$no_lod,
      tolerance = 1e-4
    )

    ### intensity normalization - multiple lod columns ----

    expect_warning(
      object = expect_message(
        object = expect_message(
          object = intensity_multiple_lod <- olink_normalization(
            df1 = ref_norm_res$lst_df$df1_multiple_lod,
            df2 = ref_norm_res$lst_df$df2_multiple_lod,
            overlapping_samples_df1 = ref_norm_res$lst_sample$df1_all,
            overlapping_samples_df2 = ref_norm_res$lst_sample$df2_all,
            df1_project_nr = "df1_multiple_lod",
            df2_project_nr = "df2_multiple_lod",
            reference_project = "df1_multiple_lod",
            format = FALSE,
            df1_check_log = check_npx(
              df = ref_norm_res$lst_df$df1_multiple_lod
            ) |>
              suppressMessages() |>
              suppressWarnings(),
            df2_check_log = check_npx(
              df = ref_norm_res$lst_df$df2_multiple_lod
            ) |>
              suppressMessages() |>
              suppressWarnings()
          ) |>
            dplyr::filter(
              .data[["SampleID"]] %in% ref_norm_res$lst_sample$sample_subset
            ),
          regexp = "Subset normalization will be performed!"
        ),
        regexp = "Datasets \"df1_multiple_lod\" and \"df2_multiple_lod\" contain"
      ),
      regexp = paste("Duplicate SampleIDs detected: \"A13\", \"A29\", \"A30\",",
                     "\"A36\", \"A45\", \"A46\", \"A52\", \"A63\", \"A71\",",
                     "\"A73\", \"B3\", \"B37\", \"B4\", \"B45\", \"B63\",",
                     "\"B75\", \"CONTROL_SAMPLE_AS 1\", and",
                     "\"CONTROL_SAMPLE_AS 2\"")
    )

    expect_equal(
      object = strip_check_log(df = intensity_multiple_lod),
      expected = ref_norm_res$lst_norm$intensity_norm$multiple_lod,
      tolerance = 1e-4
    )

    ### olink_class ----

    expect_no_error(
      object = expect_no_message(
        object = expect_warning(
          object = df1_no_norm_obj <- attach_check_log(
            df = ref_norm_res$lst_df$df1_no_norm,
            out_df = "tibble"
          ),
          regexp = paste("Duplicate SampleIDs detected: \"CONTROL_SAMPLE_AS",
                         "1\" and \"CONTROL_SAMPLE_AS 2\"")
        )
      )
    )

    expect_no_error(
      object = expect_no_message(
        object = expect_warning(
          object = df2_no_norm_obj <- attach_check_log(
            df = ref_norm_res$lst_df$df2_no_norm,
            out_df = "tibble"
          ),
          regexp = paste("Duplicate SampleIDs detected: \"CONTROL_SAMPLE_AS",
                         "1\" and \"CONTROL_SAMPLE_AS 2\"")
        )
      )
    )

    expect_warning(
      object = expect_warning(
        object = expect_message(
          object = intensity_no_norm_obj <- olink_normalization(
            df1 = df1_no_norm_obj,
            df2 = df2_no_norm_obj,
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
      ),
      regexp = paste("Duplicate SampleIDs detected: \"A13\", \"A29\", \"A30\",",
                     "\"A36\", \"A45\", \"A46\", \"A52\", \"A63\", \"A71\",",
                     "\"A73\", \"B3\", \"B37\", \"B4\", \"B45\", \"B63\",",
                     "\"B75\", \"CONTROL_SAMPLE_AS 1\", and",
                     "\"CONTROL_SAMPLE_AS 2\"")
    )

    expect_s3_class(object = intensity_no_norm_obj, class = "olink_class")
    expect_s3_class(object = intensity_no_norm_obj, class = "tbl_df")

    expect_equal(
      object = strip_check_log(df = intensity_no_norm_obj),
      expected = ref_norm_res$lst_norm$intensity_norm$no_norm,
      tolerance = 1e-4
    )

    ### olink arrow ----

    expect_no_error(
      object = expect_no_message(
        object = expect_warning(
          object = df1_no_norm_arrow <- attach_check_log(
            df = ref_norm_res$lst_df$df1_no_norm,
            out_df = "arrow"
          ),
          regexp = paste("Duplicate SampleIDs detected: \"CONTROL_SAMPLE_AS",
                         "1\" and \"CONTROL_SAMPLE_AS 2\"")
        )
      )
    )

    expect_no_error(
      object = expect_no_message(
        object = expect_warning(
          object = df2_no_norm_arrow <- attach_check_log(
            df = ref_norm_res$lst_df$df2_no_norm,
            out_df = "arrow"
          ),
          regexp = paste("Duplicate SampleIDs detected: \"CONTROL_SAMPLE_AS",
                         "1\" and \"CONTROL_SAMPLE_AS 2\"")
        )
      )
    )

    expect_warning(
      object = expect_warning(
        object = expect_message(
          object = intensity_no_norm_arrow <- olink_normalization(
            df1 = df1_no_norm_arrow,
            df2 = df2_no_norm_arrow,
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
      ),
      regexp = paste("Duplicate SampleIDs detected: \"A13\", \"A29\", \"A30\",",
                     "\"A36\", \"A45\", \"A46\", \"A52\", \"A63\", \"A71\",",
                     "\"A73\", \"B3\", \"B37\", \"B4\", \"B45\", \"B63\",",
                     "\"B75\", \"CONTROL_SAMPLE_AS 1\", and",
                     "\"CONTROL_SAMPLE_AS 2\"")
    )

    expect_s3_class(object = intensity_no_norm_arrow, class = "olink_class")
    expect_s3_class(object = intensity_no_norm_arrow, class = "tbl_df")

    expect_equal(
      object = strip_check_log(df = intensity_no_norm_arrow),
      expected = ref_norm_res$lst_norm$intensity_norm$no_norm,
      tolerance = 1e-4
    )

  }
)

test_that(
  "olink_normalization - works - subset normalization",
  {
    ref_norm_res <- get_example_data("ref_results_norm.rds")

    ### subset normalization - no norm column ----

    expect_warning(
      object = expect_warning(
        object = expect_message(
          object = subset_no_norm <- olink_normalization(
            df1 = ref_norm_res$lst_df$df1_no_norm,
            df2 = ref_norm_res$lst_df$df2_no_norm,
            overlapping_samples_df1 = ref_norm_res$lst_sample$df1_subset,
            overlapping_samples_df2 = ref_norm_res$lst_sample$df2_subset,
            df1_project_nr = "df1_no_norm",
            df2_project_nr = "df2_no_norm",
            reference_project = "df1_no_norm",
            format = FALSE,
            df1_check_log = check_npx(df = ref_norm_res$lst_df$df1_no_norm) |>
              suppressMessages() |>
              suppressWarnings(),
            df2_check_log = check_npx(df = ref_norm_res$lst_df$df2_no_norm) |>
              suppressMessages() |>
              suppressWarnings()
          ) |>
            dplyr::filter(
              .data[["SampleID"]] %in% ref_norm_res$lst_sample$sample_subset
            ),
          regexp = "Subset normalization will be performed!"
        ),
        regexp = paste("Datasets \"df1_no_norm\" and \"df2_no_norm\" do not",
                       "contain a column named \"Normalization\"")
      ),
      regexp = paste("Duplicate SampleIDs detected: \"A13\", \"A29\", \"A30\",",
                     "\"A36\", \"A45\", \"A46\", \"A52\", \"A63\", \"A71\",",
                     "\"A73\", \"B3\", \"B37\", \"B4\", \"B45\", \"B63\",",
                     "\"B75\", \"CONTROL_SAMPLE_AS 1\", and",
                     "\"CONTROL_SAMPLE_AS 2\"")
    )

    expect_equal(
      object = strip_check_log(df = subset_no_norm),
      expected = ref_norm_res$lst_norm$subset_norm$no_norm,
      tolerance = 1e-4
    )

    ### subset normalization - with norm column ----

    expect_warning(
      object = expect_message(
        object = subset_norm <- olink_normalization(
          df1 = ref_norm_res$lst_df$df1_norm,
          df2 = ref_norm_res$lst_df$df2_norm,
          overlapping_samples_df1 = ref_norm_res$lst_sample$df1_subset,
          overlapping_samples_df2 = ref_norm_res$lst_sample$df2_subset,
          df1_project_nr = "df1_norm",
          df2_project_nr = "df2_norm",
          reference_project = "df1_norm",
          format = FALSE,
          df1_check_log = check_npx(df = ref_norm_res$lst_df$df1_norm) |>
            suppressMessages() |>
            suppressWarnings(),
          df2_check_log = check_npx(df = ref_norm_res$lst_df$df2_norm) |>
            suppressMessages() |>
            suppressWarnings()
        ) |>
          dplyr::filter(
            .data[["SampleID"]] %in% ref_norm_res$lst_sample$sample_subset
          ),
        regexp = "Subset normalization will be performed!"
      ),
      regexp = paste("Duplicate SampleIDs detected: \"A13\", \"A29\", \"A30\",",
                     "\"A36\", \"A45\", \"A46\", \"A52\", \"A63\", \"A71\",",
                     "\"A73\", \"B3\", \"B37\", \"B4\", \"B45\", \"B63\",",
                     "\"B75\", \"CONTROL_SAMPLE_AS 1\", and",
                     "\"CONTROL_SAMPLE_AS 2\"")
    )

    expect_equal(
      object = strip_check_log(df = subset_norm),
      expected = ref_norm_res$lst_norm$subset_norm$norm,
      tolerance = 1e-4
    )

    ### subset normalization - no lod column ----

    expect_warning(
      object = expect_message(
        object = subset_no_lod <- olink_normalization(
          df1 = ref_norm_res$lst_df$df1_no_lod,
          df2 = ref_norm_res$lst_df$df2_no_lod,
          overlapping_samples_df1 = ref_norm_res$lst_sample$df1_subset,
          overlapping_samples_df2 = ref_norm_res$lst_sample$df2_subset,
          df1_project_nr = "df1_no_lod",
          df2_project_nr = "df2_no_lod",
          reference_project = "df1_no_lod",
          format = FALSE,
          df1_check_log = check_npx(df = ref_norm_res$lst_df$df1_no_lod) |>
            suppressMessages() |>
            suppressWarnings(),
          df2_check_log = check_npx(df = ref_norm_res$lst_df$df2_no_lod) |>
            suppressMessages() |>
            suppressWarnings()
        ) |>
          dplyr::filter(
            .data[["SampleID"]] %in% ref_norm_res$lst_sample$sample_subset
          ),
        regexp = "Subset normalization will be performed!"
      ),
      regexp = paste("Duplicate SampleIDs detected: \"A13\", \"A29\", \"A30\",",
                     "\"A36\", \"A45\", \"A46\", \"A52\", \"A63\", \"A71\",",
                     "\"A73\", \"B3\", \"B37\", \"B4\", \"B45\", \"B63\",",
                     "\"B75\", \"CONTROL_SAMPLE_AS 1\", and",
                     "\"CONTROL_SAMPLE_AS 2\"")
    )

    expect_equal(
      object = strip_check_log(df = subset_no_lod),
      expected = ref_norm_res$lst_norm$subset_norm$no_lod,
      tolerance = 1e-4
    )

    ### subset normalization - multiple lod columns ----

    expect_warning(
      object = expect_message(
        object = expect_message(
          object = subset_multiple_lod <- olink_normalization(
            df1 = ref_norm_res$lst_df$df1_multiple_lod,
            df2 = ref_norm_res$lst_df$df2_multiple_lod,
            overlapping_samples_df1 = ref_norm_res$lst_sample$df1_subset,
            overlapping_samples_df2 = ref_norm_res$lst_sample$df2_subset,
            df1_project_nr = "df1_multiple_lod",
            df2_project_nr = "df2_multiple_lod",
            reference_project = "df1_multiple_lod",
            format = FALSE,
            df1_check_log = check_npx(
              df = ref_norm_res$lst_df$df1_multiple_lod
            ) |>
              suppressMessages() |>
              suppressWarnings(),
            df2_check_log = check_npx(
              df = ref_norm_res$lst_df$df2_multiple_lod
            ) |>
              suppressMessages() |>
              suppressWarnings()
          ) |>
            dplyr::filter(
              .data[["SampleID"]] %in% ref_norm_res$lst_sample$sample_subset
            ),
          regexp = "Subset normalization will be performed!"
        ),
        regexp = "Datasets \"df1_multiple_lod\" and \"df2_multiple_lod\" contain"
      ),
      regexp = paste("Duplicate SampleIDs detected: \"A13\", \"A29\", \"A30\",",
                     "\"A36\", \"A45\", \"A46\", \"A52\", \"A63\", \"A71\",",
                     "\"A73\", \"B3\", \"B37\", \"B4\", \"B45\", \"B63\",",
                     "\"B75\", \"CONTROL_SAMPLE_AS 1\", and",
                     "\"CONTROL_SAMPLE_AS 2\"")
    )

    expect_equal(
      object = strip_check_log(df = subset_multiple_lod),
      expected = ref_norm_res$lst_norm$subset_norm$multiple_lod,
      tolerance = 1e-4
    )

    ### olink_class ----

    expect_no_error(
      object = expect_no_message(
        object = expect_warning(
          object = df1_no_norm_obj <- attach_check_log(
            df = ref_norm_res$lst_df$df1_no_norm,
            out_df = "tibble"
          ),
          regexp = paste("Duplicate SampleIDs detected: \"CONTROL_SAMPLE_AS",
                         "1\" and \"CONTROL_SAMPLE_AS 2\"")
        )
      )
    )

    expect_no_error(
      object = expect_no_message(
        object = expect_warning(
          object = df2_no_norm_obj <- attach_check_log(
            df = ref_norm_res$lst_df$df2_no_norm,
            out_df = "tibble"
          ),
          regexp = paste("Duplicate SampleIDs detected: \"CONTROL_SAMPLE_AS",
                         "1\" and \"CONTROL_SAMPLE_AS 2\"")
        )
      )
    )

    expect_warning(
      object = expect_warning(
        object = expect_message(
          object = subset_no_norm_obj <- olink_normalization(
            df1 = df1_no_norm_obj,
            df2 = df2_no_norm_obj,
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
      ),
      regexp = paste("Duplicate SampleIDs detected: \"A13\", \"A29\", \"A30\",",
                     "\"A36\", \"A45\", \"A46\", \"A52\", \"A63\", \"A71\",",
                     "\"A73\", \"B3\", \"B37\", \"B4\", \"B45\", \"B63\",",
                     "\"B75\", \"CONTROL_SAMPLE_AS 1\", and",
                     "\"CONTROL_SAMPLE_AS 2\"")
    )

    expect_s3_class(object = subset_no_norm_obj, class = "olink_class")
    expect_s3_class(object = subset_no_norm_obj, class = "tbl_df")

    expect_equal(
      object = strip_check_log(df = subset_no_norm_obj),
      expected = ref_norm_res$lst_norm$subset_norm$no_norm,
      tolerance = 1e-4
    )

    ### olink arrow ----

    expect_no_error(
      object = expect_no_message(
        object = expect_warning(
          object = df1_no_norm_arrow <- attach_check_log(
            df = ref_norm_res$lst_df$df1_no_norm,
            out_df = "arrow"
          ),
          regexp = paste("Duplicate SampleIDs detected: \"CONTROL_SAMPLE_AS",
                         "1\" and \"CONTROL_SAMPLE_AS 2\"")
        )
      )
    )

    expect_no_error(
      object = expect_no_message(
        object = expect_warning(
          object = df2_no_norm_arrow <- attach_check_log(
            df = ref_norm_res$lst_df$df2_no_norm,
            out_df = "arrow"
          ),
          regexp = paste("Duplicate SampleIDs detected: \"CONTROL_SAMPLE_AS",
                         "1\" and \"CONTROL_SAMPLE_AS 2\"")
        )
      )
    )

    expect_warning(
      object = expect_warning(
        object = expect_message(
          object = subset_no_norm_arrow <- olink_normalization(
            df1 = df1_no_norm_arrow,
            df2 = df2_no_norm_arrow,
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
      ),
      regexp = paste("Duplicate SampleIDs detected: \"A13\", \"A29\", \"A30\",",
                     "\"A36\", \"A45\", \"A46\", \"A52\", \"A63\", \"A71\",",
                     "\"A73\", \"B3\", \"B37\", \"B4\", \"B45\", \"B63\",",
                     "\"B75\", \"CONTROL_SAMPLE_AS 1\", and",
                     "\"CONTROL_SAMPLE_AS 2\"")
    )

    expect_s3_class(object = subset_no_norm_arrow, class = "olink_class")
    expect_s3_class(object = subset_no_norm_arrow, class = "tbl_df")

    expect_equal(
      object = strip_check_log(df = subset_no_norm_arrow),
      expected = ref_norm_res$lst_norm$subset_norm$no_norm,
      tolerance = 1e-4
    )
  }
)

test_that(
  "olink_normalization - works - subset normalization - format",
  {
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

    expect_warning(
      object = expect_message(
        object = subset_norm_v1 <- olink_normalization(
          df1 = df_norm_ref_v1,
          df2 = df_norm_not_ref_v1,
          overlapping_samples_df1 = subset_samples_ref_v1,
          overlapping_samples_df2 = subset_samples_not_ref_v1,
          df1_project_nr = "df1_norm",
          df2_project_nr = "df2_norm",
          reference_project = "df1_norm",
          format = TRUE,
          df1_check_log = check_npx(df = df_norm_ref_v1) |>
            suppressMessages() |>
            suppressWarnings(),
          df2_check_log = check_npx(df = df_norm_not_ref_v1) |>
            suppressMessages() |>
            suppressWarnings()
        ) |>
          dplyr::filter(
            .data[["SampleID"]] %in% .env[["sample_subset"]]
          ) |>
          dplyr::arrange(
            .data[["SampleID"]], .data[["OlinkID"]]
          ),
        regexp = "Subset normalization will be performed!"
      ),
      regexp = paste("Duplicate SampleIDs detected: \"A13\", \"A29\", \"A30\",",
                     "\"A36\", \"A45\", \"A46\", \"A52\", \"A63\", \"A71\",",
                     "\"A73\", \"B3\", \"B37\", \"B4\", \"B45\", \"B63\",",
                     "\"B75\", \"CONTROL_SAMPLE_AS 1\", and",
                     "\"CONTROL_SAMPLE_AS 2\"")
    )

    expect_equal(
      object = strip_check_log(df = subset_norm_v1),
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

    # prepare expected result by modifying reference result
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
      object = expect_warning(
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
              format = TRUE,
              df1_check_log = check_npx(df = df_norm_ref_v2) |>
                suppressMessages() |>
                suppressWarnings(),
              df2_check_log = check_npx(df = df_norm_not_ref_v2) |>
                suppressMessages() |>
                suppressWarnings()
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
      ),
      regexp = paste("Duplicate SampleIDs detected: \"A13\", \"A29\", \"A30\",",
                     "\"A36\", \"A45\", \"A46\", \"A52\", \"A63\", \"A71\",",
                     "\"A73\", \"B3\", \"B37\", \"B4\", \"B45\", \"B63\",",
                     "\"B75\", \"CONTROL_SAMPLE_AS 1\", and",
                     "\"CONTROL_SAMPLE_AS 2\"")
    )

    expect_equal(
      object = strip_check_log(df = subset_norm_v2),
      expected = ref_subset_norm_v2,
      tolerance = 1e-4
    )

    ### subset - w/ norm column - external controls ----

    df_norm_ref_v3 <- ref_norm_res$lst_df$df1_norm |>
      dplyr::mutate(
        SampleID = dplyr::recode_values(
          .data[["SampleID"]],
          "A6" ~ "NEG_CTRL_1",
          "A38" ~ "PLATE_CTRL_1",
          default = .data[["SampleID"]]
        )
      )
    df_norm_not_ref_v3 <- ref_norm_res$lst_df$df2_norm |>
      dplyr::mutate(
        SampleID = dplyr::recode_values(
          .data[["SampleID"]],
          "C66" ~ "NEG_CTRL_2",
          "D1" ~ "PLATE_CTRL_2",
          default = .data[["SampleID"]]
        )
      )

    subset_samples_ref_v3 <- ref_norm_res$lst_sample$df1_subset
    subset_samples_not_ref_v3 <- ref_norm_res$lst_sample$df2_subset

    ref_subset_norm_v3 <- ref_norm_res$lst_norm$subset_norm$norm |>
      dplyr::mutate(
        SampleID = dplyr::recode_values(
          .data[["SampleID"]],
          "A6" ~ "NEG_CTRL_1",
          "A38" ~ "PLATE_CTRL_1",
          "C66" ~ "NEG_CTRL_2",
          "D1" ~ "PLATE_CTRL_2",
          default = .data[["SampleID"]]
        )
      ) |>
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
              object = subset_norm_v3 <- olink_normalization(
                df1 = df_norm_ref_v3,
                df2 = df_norm_not_ref_v3,
                overlapping_samples_df1 = subset_samples_ref_v3,
                overlapping_samples_df2 = subset_samples_not_ref_v3,
                df1_project_nr = "df1_norm",
                df2_project_nr = "df2_norm",
                reference_project = "df1_norm",
                format = TRUE,
                df1_check_log = check_npx(df = df_norm_ref_v3) |>
                  suppressMessages() |>
                  suppressWarnings(),
                df2_check_log = check_npx(df = df_norm_not_ref_v3) |>
                  suppressMessages() |>
                  suppressWarnings()
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
      ),
      regexp = paste("Duplicate SampleIDs detected: \"A13\", \"A29\", \"A30\",",
                     "\"A36\", \"A45\", \"A46\", \"A52\", \"A63\", \"A71\",",
                     "\"A73\", \"B3\", \"B37\", \"B4\", \"B45\", \"B63\",",
                     "\"B75\", \"CONTROL_SAMPLE_AS 1\", and",
                     "\"CONTROL_SAMPLE_AS 2\"")
    )

    expect_equal(
      object = strip_check_log(df = subset_norm_v3),
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
        SampleID = dplyr::recode_values(
          .data[["SampleID"]],
          "A6" ~ "NEG_CTRL_1",
          "A38" ~ "PLATE_CTRL_1",
          default = .data[["SampleID"]]
        )
      )
    df_norm_not_ref_v4 <- ref_norm_res$lst_df$df2_norm |>
      dplyr::filter(
        !(.data[["OlinkID"]] %in% .env[["oid_only_in_ref_v4"]])
      ) |>
      dplyr::mutate(
        SampleID = dplyr::recode_values(
          .data[["SampleID"]],
          "C66" ~ "NEG_CTRL_2",
          "D1" ~ "PLATE_CTRL_2",
          default = .data[["SampleID"]]
        )
      )

    subset_samples_ref_v4 <- ref_norm_res$lst_sample$df1_subset
    subset_samples_not_ref_v4 <- ref_norm_res$lst_sample$df2_subset

    # prepare expected result by modifying reference result
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
        SampleID = dplyr::recode_values(
          .data[["SampleID"]],
          "A6" ~ "NEG_CTRL_1",
          "A38" ~ "PLATE_CTRL_1",
          "C66" ~ "NEG_CTRL_2",
          "D1" ~ "PLATE_CTRL_2",
          default = .data[["SampleID"]]
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
      object = expect_warning(
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
                    format = TRUE,
                    df1_check_log = check_npx(df = df_norm_ref_v4) |>
                      suppressMessages() |>
                      suppressWarnings(),
                    df2_check_log = check_npx(df = df_norm_not_ref_v4) |>
                      suppressMessages() |>
                      suppressWarnings()
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
      ),
      regexp = paste("Duplicate SampleIDs detected: \"A13\", \"A29\", \"A30\",",
                     "\"A36\", \"A45\", \"A46\", \"A52\", \"A63\", \"A71\",",
                     "\"A73\", \"B3\", \"B37\", \"B4\", \"B45\", \"B63\",",
                     "\"B75\", \"CONTROL_SAMPLE_AS 1\", and",
                     "\"CONTROL_SAMPLE_AS 2\"")
    )

    expect_equal(
      object = strip_check_log(df = subset_norm_v4),
      expected = ref_subset_norm_v4,
      tolerance = 1e-4
    )

    ### olink_class ----

    expect_no_error(
      object = expect_no_message(
        object = expect_warning(
          object = df_norm_ref_v1_obj <- attach_check_log(
            df = df_norm_ref_v1,
            out_df = "tibble"
          ),
          regexp = paste("Duplicate SampleIDs detected: \"CONTROL_SAMPLE_AS",
                         "1\" and \"CONTROL_SAMPLE_AS 2\"")
        )
      )
    )

    expect_no_error(
      object = expect_no_message(
        object = expect_warning(
          object = df_norm_not_ref_v1_obj <- attach_check_log(
            df = df_norm_not_ref_v1,
            out_df = "tibble"
          ),
          regexp = paste("Duplicate SampleIDs detected: \"CONTROL_SAMPLE_AS",
                         "1\" and \"CONTROL_SAMPLE_AS 2\"")
        )
      )
    )

    expect_warning(
      object = expect_message(
        object = subset_norm_v1_obj <- olink_normalization(
          df1 = df_norm_ref_v1_obj,
          df2 = df_norm_not_ref_v1_obj,
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
      ),
      regexp = paste("Duplicate SampleIDs detected: \"A13\", \"A29\", \"A30\",",
                     "\"A36\", \"A45\", \"A46\", \"A52\", \"A63\", \"A71\",",
                     "\"A73\", \"B3\", \"B37\", \"B4\", \"B45\", \"B63\",",
                     "\"B75\", \"CONTROL_SAMPLE_AS 1\", and",
                     "\"CONTROL_SAMPLE_AS 2\"")
    )

    expect_s3_class(object = subset_norm_v1_obj, class = "olink_class")
    expect_s3_class(object = subset_norm_v1_obj, class = "tbl_df")

    expect_equal(
      object = strip_check_log(df = subset_norm_v1_obj),
      expected = subset_int_norm_v1,
      tolerance = 1e-4
    )

    ### olink arrow ----

    expect_no_error(
      object = expect_no_message(
        object = expect_warning(
          object = df_norm_ref_v1_arrow <- attach_check_log(
            df = df_norm_ref_v1,
            out_df = "arrow"
          ),
          regexp = paste("Duplicate SampleIDs detected: \"CONTROL_SAMPLE_AS",
                         "1\" and \"CONTROL_SAMPLE_AS 2\"")
        )
      )
    )

    expect_no_error(
      object = expect_no_message(
        object = expect_warning(
          object = df_norm_not_ref_v1_arrow <- attach_check_log(
            df = df_norm_not_ref_v1,
            out_df = "arrow"
          ),
          regexp = paste("Duplicate SampleIDs detected: \"CONTROL_SAMPLE_AS",
                         "1\" and \"CONTROL_SAMPLE_AS 2\"")
        )
      )
    )

    expect_warning(
      object = expect_message(
        object = subset_norm_v1_arrow <- olink_normalization(
          df1 = df_norm_ref_v1_arrow,
          df2 = df_norm_not_ref_v1_arrow,
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
      ),
      regexp = paste("Duplicate SampleIDs detected: \"A13\", \"A29\", \"A30\",",
                     "\"A36\", \"A45\", \"A46\", \"A52\", \"A63\", \"A71\",",
                     "\"A73\", \"B3\", \"B37\", \"B4\", \"B45\", \"B63\",",
                     "\"B75\", \"CONTROL_SAMPLE_AS 1\", and",
                     "\"CONTROL_SAMPLE_AS 2\"")
    )

    expect_s3_class(object = subset_norm_v1_arrow, class = "olink_class")
    expect_s3_class(object = subset_norm_v1_arrow, class = "tbl_df")

    expect_equal(
      object = strip_check_log(df = subset_norm_v1_arrow),
      expected = subset_int_norm_v1,
      tolerance = 1e-4
    )
  }
)

test_that(
  "olink_normalization - works - reference median normalization",
  {
    ref_norm_res <- get_example_data("ref_results_norm.rds")

    ### reference median normalization - no norm column ----

    expect_warning(
      object = expect_warning(
        object = expect_message(
          object = ref_med_no_norm <- olink_normalization(
            df1 = ref_norm_res$lst_df$df1_no_norm,
            overlapping_samples_df1 = ref_norm_res$lst_sample$df1_subset,
            df1_project_nr = "df1_no_norm",
            reference_medians = ref_norm_res$lst_df$ref_med,
            format = FALSE,
            df1_check_log = check_npx(df = ref_norm_res$lst_df$df1_no_norm) |>
              suppressMessages() |>
              suppressWarnings()
          ) |>
            dplyr::filter(
              .data[["SampleID"]] %in% ref_norm_res$lst_sample$sample_subset
            ),
          regexp = "Reference median normalization will be performed!"
        ),
        regexp = paste("Dataset \"df1_no_norm\" does not contain a column",
                       "named \"Normalization\"")
      ),
      regexp = paste("Duplicate SampleIDs detected: \"CONTROL_SAMPLE_AS 1\"",
                     "and \"CONTROL_SAMPLE_AS 2\"")
    )

    expect_equal(
      object = strip_check_log(df = ref_med_no_norm),
      expected = ref_norm_res$lst_norm$ref_med_norm$no_norm,
      tolerance = 1e-4
    )

    ### reference median normalization - with norm column ----

    expect_warning(
      object = expect_message(
        object = ref_med_norm <- olink_normalization(
          df1 = ref_norm_res$lst_df$df1_norm,
          overlapping_samples_df1 = ref_norm_res$lst_sample$df1_subset,
          df1_project_nr = "df1_norm",
          reference_medians = ref_norm_res$lst_df$ref_med,
          format = FALSE,
          df1_check_log = check_npx(df = ref_norm_res$lst_df$df1_norm) |>
            suppressMessages() |>
            suppressWarnings()
        ) |>
          dplyr::filter(
            .data[["SampleID"]] %in% ref_norm_res$lst_sample$sample_subset
          ),
        regexp = "Reference median normalization will be performed!"
      ),
      regexp = paste("Duplicate SampleIDs detected: \"CONTROL_SAMPLE_AS 1\"",
                     "and \"CONTROL_SAMPLE_AS 2\"")
    )

    expect_equal(
      object = strip_check_log(df = ref_med_norm),
      expected = ref_norm_res$lst_norm$ref_med_norm$norm,
      tolerance = 1e-4
    )

    ### reference median normalization - no lod column ----

    expect_warning(
      object = expect_message(
        object = ref_med_no_lod <- olink_normalization(
          df1 = ref_norm_res$lst_df$df1_no_lod,
          overlapping_samples_df1 = ref_norm_res$lst_sample$df1_subset,
          df1_project_nr = "df1_no_lod",
          reference_medians = ref_norm_res$lst_df$ref_med,
          format = FALSE,
          df1_check_log = check_npx(df = ref_norm_res$lst_df$df1_no_lod) |>
            suppressMessages() |>
            suppressWarnings()
        ) |>
          dplyr::filter(
            .data[["SampleID"]] %in% ref_norm_res$lst_sample$sample_subset
          ),
        regexp = "Reference median normalization will be performed!"
      ),
      regexp = paste("Duplicate SampleIDs detected: \"CONTROL_SAMPLE_AS 1\"",
                     "and \"CONTROL_SAMPLE_AS 2\"")
    )

    expect_equal(
      object = strip_check_log(df = ref_med_no_lod),
      expected = ref_norm_res$lst_norm$ref_med_norm$no_lod,
      tolerance = 1e-4
    )

    ### reference median normalization - multiple lod columns ----

    expect_warning(
      object = expect_message(
        object = expect_message(
          object = ref_med_multiple_lod <- olink_normalization(
            df1 = ref_norm_res$lst_df$df1_multiple_lod,
            overlapping_samples_df1 = ref_norm_res$lst_sample$df1_subset,
            df1_project_nr = "df1_multiple_lod",
            reference_medians = ref_norm_res$lst_df$ref_med,
            format = FALSE,
            df1_check_log = check_npx(
              df = ref_norm_res$lst_df$df1_multiple_lod
            ) |>
              suppressMessages() |>
              suppressWarnings()
          ) |>
            dplyr::filter(
              .data[["SampleID"]] %in% ref_norm_res$lst_sample$sample_subset
            ),
          regexp = "Reference median normalization will be performed!"
        ),
        regexp = "Dataset \"df1_multiple_lod\" contains multiple columns"
      ),
      regexp = paste("Duplicate SampleIDs detected: \"CONTROL_SAMPLE_AS 1\"",
                     "and \"CONTROL_SAMPLE_AS 2\"")
    )

    expect_equal(
      object = strip_check_log(df = ref_med_multiple_lod),
      expected = ref_norm_res$lst_norm$ref_med_norm$multiple_lod,
      tolerance = 1e-4
    )

    ### olink_class ----

    expect_no_error(
      object = expect_no_message(
        object = expect_warning(
          object = df1_no_norm_obj <- attach_check_log(
            df = ref_norm_res$lst_df$df1_no_norm,
            out_df = "tibble"
          ),
          regexp = paste("Duplicate SampleIDs detected: \"CONTROL_SAMPLE_AS",
                         "1\" and \"CONTROL_SAMPLE_AS 2\"")
        )
      )
    )

    expect_warning(
      object = expect_warning(
        object = expect_message(
          object = ref_med_no_norm_obj <- olink_normalization(
            df1 = df1_no_norm_obj,
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
        regexp = paste("Dataset \"df1_no_norm\" does not contain a column",
                       "named \"Normalization\"")
      ),
      regexp = paste("Duplicate SampleIDs detected: \"CONTROL_SAMPLE_AS 1\"",
                     "and \"CONTROL_SAMPLE_AS 2\"")
    )

    expect_s3_class(object = ref_med_no_norm_obj, class = "olink_class")
    expect_s3_class(object = ref_med_no_norm_obj, class = "tbl_df")

    expect_equal(
      object = strip_check_log(df = ref_med_no_norm_obj),
      expected = ref_norm_res$lst_norm$ref_med_norm$no_norm,
      tolerance = 1e-4
    )

    ### olink arrow ----

    expect_no_error(
      object = expect_no_message(
        object = expect_warning(
          object = df1_no_norm_arrow <- attach_check_log(
            df = ref_norm_res$lst_df$df1_no_norm,
            out_df = "arrow"
          ),
          regexp = paste("Duplicate SampleIDs detected: \"CONTROL_SAMPLE_AS",
                         "1\" and \"CONTROL_SAMPLE_AS 2\"")
        )
      )
    )

    expect_warning(
      object = expect_warning(
        object = expect_message(
          object = ref_med_no_norm_arrow <- olink_normalization(
            df1 = df1_no_norm_arrow,
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
        regexp = paste("Dataset \"df1_no_norm\" does not contain a column",
                       "named \"Normalization\"")
      ),
      regexp = paste("Duplicate SampleIDs detected: \"CONTROL_SAMPLE_AS 1\"",
                     "and \"CONTROL_SAMPLE_AS 2\"")
    )

    expect_s3_class(object = ref_med_no_norm_arrow, class = "olink_class")
    expect_s3_class(object = ref_med_no_norm_arrow, class = "tbl_df")

    expect_equal(
      object = strip_check_log(df = ref_med_no_norm_arrow),
      expected = ref_norm_res$lst_norm$ref_med_norm$no_norm,
      tolerance = 1e-4
    )
  }
)

test_that(
  "olink_normalization - works - reference median normalization - format",
  {
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

    expect_warning(
      object = expect_message(
        object = refmed_norm_v1 <- olink_normalization(
          df1 = df_norm_ref_v1,
          overlapping_samples_df1 = subset_samples_v1,
          df1_project_nr = "df1_norm",
          reference_medians = df_ref_med_v1,
          format = TRUE,
          df1_check_log = check_npx(df = df_norm_ref_v1) |>
            suppressMessages() |>
            suppressWarnings()
        ) |>
          dplyr::filter(
            .data[["SampleID"]] %in% .env[["sample_subset"]]
          ) |>
          dplyr::arrange(
            .data[["SampleID"]], .data[["OlinkID"]]
          ),
        regexp = "Reference median normalization will be performed!"
      ),
      regexp = paste("Duplicate SampleIDs detected: \"CONTROL_SAMPLE_AS 1\"",
                     "and \"CONTROL_SAMPLE_AS 2\"")
    )

    expect_equal(
      object = strip_check_log(df = refmed_norm_v1),
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

    # prepare expected result by modifying reference result
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
      object = expect_warning(
        object = expect_message(
          object = expect_message(
            object = refmed_norm_v2 <- olink_normalization(
              df1 = df_norm_ref_v2,
              overlapping_samples_df1 = subset_samples_v2,
              df1_project_nr = "df1_norm",
              reference_medians = df_ref_med_v2,
              format = TRUE,
              df1_check_log = check_npx(df = df_norm_ref_v2) |>
                suppressMessages() |>
                suppressWarnings()
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
      ),
      regexp = paste("Duplicate SampleIDs detected: \"CONTROL_SAMPLE_AS 1\"",
                     "and \"CONTROL_SAMPLE_AS 2\"")
    )

    expect_equal(
      object = strip_check_log(df = refmed_norm_v2),
      expected = ref_refmed_norm_v2,
      tolerance = 1e-4
    )

    ### ref med - w/ norm column - external controls ----

    df_norm_ref_v3 <- ref_norm_res$lst_df$df1_norm |>
      dplyr::mutate(
        SampleID = dplyr::recode_values(
          .data[["SampleID"]],
          "A6" ~ "NEG_CTRL_1",
          "A38" ~ "PLATE_CTRL_1",
          default = .data[["SampleID"]]
        )
      )
    df_ref_med_v3 <- ref_norm_res$lst_df$ref_med

    subset_samples_v3 <- ref_norm_res$lst_sample$df1_subset

    ref_refmed_norm_v3 <- ref_norm_res$lst_norm$ref_med_norm$norm |>
      dplyr::mutate(
        SampleID = dplyr::recode_values(
          .data[["SampleID"]],
          "A6" ~ "NEG_CTRL_1",
          "A38" ~ "PLATE_CTRL_1",
          default = .data[["SampleID"]]
        )
      ) |>
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
              object = refmed_norm_v3 <- olink_normalization(
                df1 = df_norm_ref_v3,
                overlapping_samples_df1 = subset_samples_v3,
                df1_project_nr = "df1_norm",
                reference_medians = df_ref_med_v3,
                format = TRUE,
                df1_check_log = check_npx(df = df_norm_ref_v3) |>
                  suppressMessages() |>
                  suppressWarnings()
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
      ),
      regexp = paste("Duplicate SampleIDs detected: \"CONTROL_SAMPLE_AS 1\"",
                     "and \"CONTROL_SAMPLE_AS 2\"")
    )

    expect_equal(
      object = strip_check_log(df = refmed_norm_v3),
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
        SampleID = dplyr::recode_values(
          .data[["SampleID"]],
          "A6" ~ "NEG_CTRL_1",
          "A38" ~ "PLATE_CTRL_1",
          default = .data[["SampleID"]]
        )
      )
    df_ref_med_v4 <- ref_norm_res$lst_df$ref_med |>
      dplyr::filter(
        !(.data[["OlinkID"]] %in% .env[["oid_only_in_ref_v4"]])
      )

    subset_samples_v4 <- ref_norm_res$lst_sample$df1_subset

    # prepare expected result by modifying reference result
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
        SampleID = dplyr::recode_values(
          .data[["SampleID"]],
          "A6" ~ "NEG_CTRL_1",
          "A38" ~ "PLATE_CTRL_1",
          default = .data[["SampleID"]]
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
      object = expect_warning(
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
                    format = TRUE,
                    df1_check_log = check_npx(df = df_norm_ref_v4) |>
                      suppressMessages() |>
                      suppressWarnings()
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
      ),
      regexp = paste("Duplicate SampleIDs detected: \"CONTROL_SAMPLE_AS 1\"",
                     "and \"CONTROL_SAMPLE_AS 2\"")
    )

    expect_equal(
      object = strip_check_log(df = refmed_norm_v4),
      expected = ref_refmed_norm_v4,
      tolerance = 1e-4
    )

    ### olink_class ----

    expect_no_error(
      object = expect_no_message(
        object = expect_warning(
          object = df_norm_ref_v1_obj <- attach_check_log(
            df = df_norm_ref_v1,
            out_df = "tibble"
          ),
          regexp = paste("Duplicate SampleIDs detected: \"CONTROL_SAMPLE_AS",
                         "1\" and \"CONTROL_SAMPLE_AS 2\"")
        )
      )
    )

    expect_warning(
      object = expect_message(
        object = refmed_norm_v1_obj <- olink_normalization(
          df1 = df_norm_ref_v1_obj,
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
      ),
      regexp = paste("Duplicate SampleIDs detected: \"CONTROL_SAMPLE_AS 1\"",
                     "and \"CONTROL_SAMPLE_AS 2\"")
    )

    expect_s3_class(object = refmed_norm_v1_obj, class = "olink_class")
    expect_s3_class(object = refmed_norm_v1_obj, class = "tbl_df")

    expect_equal(
      object = strip_check_log(df = refmed_norm_v1_obj),
      expected = ref_refmed_norm_v1,
      tolerance = 1e-4
    )

    ### olink arrow ----

    expect_no_error(
      object = expect_no_message(
        object = expect_warning(
          object = df_norm_ref_v1_arrow <- attach_check_log(
            df = df_norm_ref_v1,
            out_df = "arrow"
          ),
          regexp = paste("Duplicate SampleIDs detected: \"CONTROL_SAMPLE_AS",
                         "1\" and \"CONTROL_SAMPLE_AS 2\"")
        )
      )
    )

    expect_warning(
      object = expect_message(
        object = refmed_norm_v1_arrow <- olink_normalization(
          df1 = df_norm_ref_v1_arrow,
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
      ),
      regexp = paste("Duplicate SampleIDs detected: \"CONTROL_SAMPLE_AS 1\"",
                     "and \"CONTROL_SAMPLE_AS 2\"")
    )

    expect_s3_class(object = refmed_norm_v1_arrow, class = "olink_class")
    expect_s3_class(object = refmed_norm_v1_arrow, class = "tbl_df")

    expect_equal(
      object = strip_check_log(df = refmed_norm_v1_arrow),
      expected = ref_refmed_norm_v1,
      tolerance = 1e-4
    )
  }
)

test_that(
  "olink_normalization - works - 3k-HT normalization",
  {
    data_3k <- get_example_data(filename = "example_3k_data.rds")
    data_ht <- get_example_data(filename = "example_HT_data.rds")

    data_ref <- get_example_data(
      filename = "ref_results_norm_cross-product.rds"
    )

    # tibble ----

    ## no format ----

    withr::with_seed(
      seed = 123,
      code = {
        expect_message(
          object = expect_warning(
            object = expect_message(
              object = expect_message(
                object = expect_warning(
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
                    format = FALSE,
                    df1_check_log = check_npx(df = data_ht) |>
                      suppressMessages() |>
                      suppressWarnings(),
                    df2_check_log = check_npx(df = data_3k) |>
                      suppressMessages() |>
                      suppressWarnings()
                  ),
                  regexp = "2 assays are not shared across products."
                ),
                regexp = "Cross-product normalization will be performed!"
              ),
              regexp = "Output includes two sets of bridging samples"
            ),
            regexp = paste("Duplicate SampleIDs detected:",
                           "\"CONTROL_SAMPLE_1\", \"CONTROL_SAMPLE_2\",",
                           "\"CONTROL_SAMPLE_3\"")
          ),
          regexp = paste("More than one column names in `df` was associated",
                         "with certain key")
        )
      }
    )

    expect_equal(
      object = ht_3k_norm |>
        dplyr::arrange(
          .data[["SampleID"]],
          .data[["OlinkID"]]
        ) |>
        strip_check_log(),
      expected = data_ref$Reference$NoFormat$HT_3K |>
        dplyr::arrange(
          .data[["SampleID"]],
          .data[["OlinkID"]]
        ),
      tolerance = 1e-4
    )

    ## with format ----

    withr::with_seed(
      seed = 123,
      code = {
        expect_message(
          object = expect_warning(
            object = expect_message(
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
                            format = TRUE,
                            df1_check_log = check_npx(df = data_ht) |>
                              suppressMessages() |>
                              suppressWarnings(),
                            df2_check_log = check_npx(df = data_3k) |>
                              suppressMessages() |>
                              suppressWarnings()
                          ),
                          regexp = "2 assays are not shared across products."
                        ),
                        regexp = "Cross-product normalization will be performed"
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
            ),
            regexp = paste("Duplicate SampleIDs detected:",
                           "\"CONTROL_SAMPLE_1\", \"CONTROL_SAMPLE_2\",",
                           "\"CONTROL_SAMPLE_3\"")
          ),
          regexp = paste("More than one column names in `df` was associated",
                         "with certain key")
        )
      }
    )

    expect_equal(
      object = ht_3k_norm_format |>
        dplyr::arrange(
          .data[["SampleID"]],
          .data[["OlinkID"]]
        ) |>
        strip_check_log(),
      expected = data_ref$Reference$Format$HT_3K |>
        dplyr::arrange(
          .data[["SampleID"]],
          .data[["OlinkID"]]
        ),
      tolerance = 1e-4
    )

    # olink_class ----

    expect_no_error(
      object = expect_no_warning(
        object = expect_message(
          object = data_ht_obj <- attach_check_log(
            df = data_ht,
            out_df = "tibble"
          ),
          regexp = paste("More than one column names in `df` was associated",
                         "with certain key")
        )
      )
    )

    expect_no_error(
      object = expect_no_warning(
        object = expect_message(
          object = data_3k_obj <- attach_check_log(
            df = data_3k,
            out_df = "tibble"
          ),
          regexp = paste("More than one column names in `df` was associated",
                         "with certain key")
        )
      )
    )

    ## no format ----

    withr::with_seed(
      seed = 123,
      code = {
        expect_message(
          object = expect_warning(
            object = expect_message(
              object = expect_message(
                object = expect_warning(
                  object = ht_3k_norm_obj <- olink_normalization(
                    df1 = data_ht_obj,
                    df2 = data_3k_obj,
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
            ),
            regexp = paste("Duplicate SampleIDs detected:",
                           "\"CONTROL_SAMPLE_1\", \"CONTROL_SAMPLE_2\",",
                           "\"CONTROL_SAMPLE_3\"")
          ),
          regexp = paste("More than one column names in `df` was associated",
                         "with certain key")
        )
      }
    )

    expect_s3_class(object = ht_3k_norm_obj, class = "olink_class")
    expect_s3_class(object = ht_3k_norm_obj, class = "tbl_df")

    expect_equal(
      object = ht_3k_norm_obj |>
        dplyr::arrange(
          .data[["SampleID"]],
          .data[["OlinkID"]]
        ) |>
        strip_check_log(),
      expected = data_ref$Reference$NoFormat$HT_3K |>
        dplyr::arrange(
          .data[["SampleID"]],
          .data[["OlinkID"]]
        ),
      tolerance = 1e-4
    )

    ## with format ----

    withr::with_seed(
      seed = 123,
      code = {
        expect_message(
          object = expect_warning(
            object = expect_message(
              object = expect_message(
                object = expect_message(
                  object = expect_message(
                    object = expect_message(
                      object = expect_message(
                        expect_warning(
                          object = ht_3k_norm_format_obj <- olink_normalization(
                            df1 = data_ht_obj,
                            df2 = data_3k_obj,
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
                        regexp = "Cross-product normalization will be performed"
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
            ),
            regexp = paste("Duplicate SampleIDs detected:",
                           "\"CONTROL_SAMPLE_1\", \"CONTROL_SAMPLE_2\",",
                           "\"CONTROL_SAMPLE_3\"")
          ),
          regexp = paste("More than one column names in `df` was associated",
                         "with certain key")
        )
      }
    )

    expect_s3_class(object = ht_3k_norm_format_obj, class = "olink_class")
    expect_s3_class(object = ht_3k_norm_format_obj, class = "tbl_df")

    expect_equal(
      object = ht_3k_norm_format_obj |>
        dplyr::arrange(
          .data[["SampleID"]],
          .data[["OlinkID"]]
        ) |>
        strip_check_log(),
      expected = data_ref$Reference$Format$HT_3K |>
        dplyr::arrange(
          .data[["SampleID"]],
          .data[["OlinkID"]]
        ),
      tolerance = 1e-4
    )

    # olink arrow ----

    expect_no_error(
      object = expect_no_warning(
        object = expect_message(
          object = data_ht_arrow <- attach_check_log(
            df = data_ht,
            out_df = "arrow"
          ),
          regexp = paste("More than one column names in `df` was associated",
                         "with certain key")
        )
      )
    )

    expect_no_error(
      object = expect_no_warning(
        object = expect_message(
          object = data_3k_arrow <- attach_check_log(
            df = data_3k,
            out_df = "arrow"
          ),
          regexp = paste("More than one column names in `df` was associated",
                         "with certain key")
        )
      )
    )

    ## no format ----

    withr::with_seed(
      seed = 123,
      code = {
        expect_message(
          object = expect_warning(
            object = expect_message(
              object = expect_message(
                object = expect_warning(
                  object = ht_3k_norm_arrow <- olink_normalization(
                    df1 = data_ht_arrow,
                    df2 = data_3k_arrow,
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
            ),
            regexp = paste("Duplicate SampleIDs detected:",
                           "\"CONTROL_SAMPLE_1\", \"CONTROL_SAMPLE_2\",",
                           "\"CONTROL_SAMPLE_3\"")
          ),
          regexp = paste("More than one column names in `df` was associated",
                         "with certain key")
        )
      }
    )

    expect_s3_class(object = ht_3k_norm_arrow, class = "olink_class")
    expect_s3_class(object = ht_3k_norm_arrow, class = "tbl_df")

    expect_equal(
      object = ht_3k_norm_arrow |>
        dplyr::arrange(
          .data[["SampleID"]],
          .data[["OlinkID"]],
          .data[["OlinkID_E3072"]]
        ) |>
        strip_check_log(),
      expected = data_ref$Reference$NoFormat$HT_3K |>
        dplyr::arrange(
          .data[["SampleID"]],
          .data[["OlinkID"]],
          .data[["OlinkID_E3072"]]
        ),
      tolerance = 1e-4
    )

    ## with format ----

    withr::with_seed(
      seed = 123,
      code = {
        expect_message(
          object = expect_warning(
            object = expect_message(
              object = expect_message(
                object = expect_message(
                  object = expect_message(
                    object = expect_message(
                      object = expect_message(
                        expect_warning(
                          object = ht_3k_norm_format_arrow <-
                            olink_normalization(
                              df1 = data_ht_arrow,
                              df2 = data_3k_arrow,
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
                        regexp = "Cross-product normalization will be performed"
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
            ),
            regexp = paste("Duplicate SampleIDs detected:",
                           "\"CONTROL_SAMPLE_1\", \"CONTROL_SAMPLE_2\",",
                           "\"CONTROL_SAMPLE_3\"")
          ),
          regexp = paste("More than one column names in `df` was associated",
                         "with certain key")
        )
      }
    )

    expect_s3_class(object = ht_3k_norm_format_arrow, class = "olink_class")
    expect_s3_class(object = ht_3k_norm_format_arrow, class = "tbl_df")

    expect_equal(
      object = ht_3k_norm_format_arrow |>
        dplyr::arrange(
          .data[["SampleID"]],
          .data[["OlinkID"]]
        ) |>
        strip_check_log(),
      expected = data_ref$Reference$Format$HT_3K |>
        dplyr::arrange(
          .data[["SampleID"]],
          .data[["OlinkID"]]
        ),
      tolerance = 1e-4
    )
  }
)

test_that(
  "olink_normalization - works - 3k-Reveal",
  {
    data_3k <- get_example_data(filename = "example_3k_data.rds")
    data_reveal <- get_example_data(filename = "example_Reveal_data.rds")

    data_ref <- get_example_data(
      filename = "ref_results_norm_cross-product.rds"
    )

    # tibble ----

    ### no format ----

    withr::with_seed(
      seed = 123,
      code = {
        expect_message(
          object = expect_warning(
            object = expect_message(
              object = expect_warning(
                object = expect_message(
                  object = rev_3k_norm <- olink_normalization(
                    df1 = data_reveal,
                    df2 = data_3k,
                    overlapping_samples_df1 = intersect(
                      x = unique(data_reveal$SampleID),
                      y = unique(data_3k$SampleID)
                    ) |>
                      (\(x) x[!grepl("CONTROL", x)])(),
                    df1_project_nr = "df_reveal",
                    df2_project_nr = "df_3k",
                    reference_project = "df_reveal",
                    format = FALSE,
                    df1_check_log = check_npx(df = data_reveal) |>
                      suppressMessages() |>
                      suppressWarnings(),
                    df2_check_log = check_npx(df = data_3k) |>
                      suppressMessages() |>
                      suppressWarnings()
                  ),
                  regexp = "Cross-product normalization will be performed!"
                ),
                regexp = "85 assays are not shared across products"
              ),
              regexp = "Output includes two sets of bridging samples"
            ),
            regexp = paste("Duplicate SampleIDs detected:",
                           "\"CONTROL_SAMPLE_1\", \"CONTROL_SAMPLE_2\",",
                           "\"CONTROL_SAMPLE_3\", \"CONTROL_SAMPLE_4\"")
          ),
          regexp = paste("More than one column names in `df` was associated",
                         "with certain key")
        )
      }
    )

    expect_equal(
      object = rev_3k_norm |>
        dplyr::arrange(
          .data[["SampleID"]],
          .data[["OlinkID"]]
        ) |>
        strip_check_log(),
      expected = data_ref$Reference$NoFormat$Reveal_3K |>
        dplyr::arrange(
          .data[["SampleID"]],
          .data[["OlinkID"]]
        ),
      tolerance = 1e-4
    )

    ### with formatting ----

    withr::with_seed(
      seed = 123,
      code = {
        expect_message(
          object = expect_warning(
            object = expect_message(
              object = expect_message(
                object = expect_message(
                  object = expect_message(
                    object = expect_message(
                      object = expect_warning(
                        object = expect_message(
                          object = rev_3k_norm_format <- olink_normalization(
                            df1 = data_reveal,
                            df2 = data_3k,
                            overlapping_samples_df1 = intersect(
                              x = unique(data_reveal$SampleID),
                              y = unique(data_3k$SampleID)
                            ) |>
                              (\(x) x[!grepl("CONTROL", x)])(),
                            df1_project_nr = "df_reveal",
                            df2_project_nr = "df_3k",
                            reference_project = "df_reveal",
                            format = TRUE,
                            df1_check_log = check_npx(df = data_reveal) |>
                              suppressMessages() |>
                              suppressWarnings(),
                            df2_check_log = check_npx(df = data_3k) |>
                              suppressMessages() |>
                              suppressWarnings()
                          ),
                          regexp = "Cross-product normalization will be"
                        ),
                        regexp = "85 assays are not shared across products"
                      ),
                      regexp = "Output includes two sets of bridging samples"
                    ),
                    regexp = paste("10 Plate Controls were removed from datase")
                  ),
                  regexp = paste("6 Negative Controls were removed from datase")
                ),
                regexp = paste("24 not bridgeable assays are included in the",
                               "bridged dataset without adjustment")
              ),
              regexp = paste("85 non-overlapping assays are included in the",
                             "normalized dataset without adjustment")
            ),
            regexp = paste("Duplicate SampleIDs detected:",
                           "\"CONTROL_SAMPLE_1\", \"CONTROL_SAMPLE_2\",",
                           "\"CONTROL_SAMPLE_3\", \"CONTROL_SAMPLE_4\"")
          ),
          regexp = paste("More than one column names in `df` was associated",
                         "with certain key")
        )
      }
    )

    expect_equal(
      object = rev_3k_norm_format |>
        dplyr::arrange(
          .data[["SampleID"]],
          .data[["OlinkID"]]
        ) |>
        strip_check_log(),
      expected = data_ref$Reference$Format$Reveal_3K |>
        dplyr::arrange(
          .data[["SampleID"]],
          .data[["OlinkID"]]
        ),
      tolerance = 1e-4
    )

    # olink_class ----

    expect_no_error(
      object = expect_no_warning(
        object = expect_message(
          object = data_reveal_obj <- attach_check_log(
            df = data_reveal,
            out_df = "tibble"
          ),
          regexp = paste("More than one column names in `df` was associated",
                         "with certain key")
        )
      )
    )

    expect_no_error(
      object = expect_no_warning(
        object = expect_message(
          object = data_3k_obj <- attach_check_log(
            df = data_3k,
            out_df = "tibble"
          ),
          regexp = paste("More than one column names in `df` was associated",
                         "with certain key")
        )
      )
    )

    ### no format ----

    withr::with_seed(
      seed = 123,
      code = {
        expect_message(
          object = expect_warning(
            object = expect_message(
              object = expect_warning(
                object = expect_message(
                  object = rev_3k_norm_obj <- olink_normalization(
                    df1 = data_reveal_obj,
                    df2 = data_3k_obj,
                    overlapping_samples_df1 = intersect(
                      x = unique(data_reveal$SampleID),
                      y = unique(data_3k$SampleID)
                    ) |>
                      (\(x) x[!grepl("CONTROL", x)])(),
                    df1_project_nr = "df_reveal",
                    df2_project_nr = "df_3k",
                    reference_project = "df_reveal",
                    format = FALSE
                  ),
                  regexp = "Cross-product normalization will be performed!"
                ),
                regexp = "85 assays are not shared across products"
              ),
              regexp = "Output includes two sets of bridging samples"
            ),
            regexp = paste("Duplicate SampleIDs detected:",
                           "\"CONTROL_SAMPLE_1\", \"CONTROL_SAMPLE_2\",",
                           "\"CONTROL_SAMPLE_3\", \"CONTROL_SAMPLE_4\"")
          ),
          regexp = paste("More than one column names in `df` was associated",
                         "with certain key")
        )
      }
    )

    expect_s3_class(object = rev_3k_norm_obj, class = "olink_class")
    expect_s3_class(object = rev_3k_norm_obj, class = "tbl_df")

    expect_equal(
      object = rev_3k_norm_obj |>
        dplyr::arrange(
          .data[["SampleID"]],
          .data[["OlinkID"]]
        ) |>
        strip_check_log(),
      expected = data_ref$Reference$NoFormat$Reveal_3K |>
        dplyr::arrange(
          .data[["SampleID"]],
          .data[["OlinkID"]]
        ),
      tolerance = 1e-4
    )

    ### with formatting ----

    withr::with_seed(
      seed = 123,
      code = {
        expect_message(
          object = expect_warning(
            object = expect_message(
              object = expect_message(
                object = expect_message(
                  object = expect_message(
                    object = expect_message(
                      object = expect_warning(
                        object = expect_message(
                          object = rev_3k_norm_format_obj <-
                            olink_normalization(
                              df1 = data_reveal_obj,
                              df2 = data_3k_obj,
                              overlapping_samples_df1 = intersect(
                                x = unique(data_reveal$SampleID),
                                y = unique(data_3k$SampleID)
                              ) |>
                                (\(x) x[!grepl("CONTROL", x)])(),
                              df1_project_nr = "df_reveal",
                              df2_project_nr = "df_3k",
                              reference_project = "df_reveal",
                              format = TRUE
                            ),
                          regexp = "Cross-product normalization will be"
                        ),
                        regexp = "85 assays are not shared across products"
                      ),
                      regexp = "Output includes two sets of bridging samples"
                    ),
                    regexp = paste("10 Plate Controls were removed from datase")
                  ),
                  regexp = paste("6 Negative Controls were removed from datase")
                ),
                regexp = paste("24 not bridgeable assays are included in the",
                               "bridged dataset without adjustment")
              ),
              regexp = paste("85 non-overlapping assays are included in the",
                             "normalized dataset without adjustment")
            ),
            regexp = paste("Duplicate SampleIDs detected:",
                           "\"CONTROL_SAMPLE_1\", \"CONTROL_SAMPLE_2\",",
                           "\"CONTROL_SAMPLE_3\", \"CONTROL_SAMPLE_4\"")
          ),
          regexp = paste("More than one column names in `df` was associated",
                         "with certain key")
        )
      }
    )

    expect_s3_class(object = rev_3k_norm_format_obj, class = "olink_class")
    expect_s3_class(object = rev_3k_norm_format_obj, class = "tbl_df")

    expect_equal(
      object = rev_3k_norm_format_obj |>
        dplyr::arrange(
          .data[["SampleID"]],
          .data[["OlinkID"]]
        ) |>
        strip_check_log(),
      expected = data_ref$Reference$Format$Reveal_3K |>
        dplyr::arrange(
          .data[["SampleID"]],
          .data[["OlinkID"]]
        ),
      tolerance = 1e-4
    )

    # olink arrow ----

    expect_no_error(
      object = expect_no_warning(
        object = expect_message(
          object = data_reveal_arrow <- attach_check_log(
            df = data_reveal,
            out_df = "arrow"
          ),
          regexp = paste("More than one column names in `df` was associated",
                         "with certain key")
        )
      )
    )

    expect_no_error(
      object = expect_no_warning(
        object = expect_message(
          object = data_3k_arrow <- attach_check_log(
            df = data_3k,
            out_df = "arrow"
          ),
          regexp = paste("More than one column names in `df` was associated",
                         "with certain key")
        )
      )
    )

    ### no format ----

    withr::with_seed(
      seed = 123,
      code = {
        expect_message(
          object = expect_warning(
            object = expect_message(
              object = expect_warning(
                object = expect_message(
                  object = rev_3k_norm_arrow <- olink_normalization(
                    df1 = data_reveal_arrow,
                    df2 = data_3k_arrow,
                    overlapping_samples_df1 = intersect(
                      x = unique(data_reveal$SampleID),
                      y = unique(data_3k$SampleID)
                    ) |>
                      (\(x) x[!grepl("CONTROL", x)])(),
                    df1_project_nr = "df_reveal",
                    df2_project_nr = "df_3k",
                    reference_project = "df_reveal",
                    format = FALSE
                  ),
                  regexp = "Cross-product normalization will be performed!"
                ),
                regexp = "85 assays are not shared across products"
              ),
              regexp = "Output includes two sets of bridging samples"
            ),
            regexp = paste("Duplicate SampleIDs detected:",
                           "\"CONTROL_SAMPLE_1\", \"CONTROL_SAMPLE_2\",",
                           "\"CONTROL_SAMPLE_3\", \"CONTROL_SAMPLE_4\"")
          ),
          regexp = paste("More than one column names in `df` was associated",
                         "with certain key")
        )
      }
    )

    expect_s3_class(object = rev_3k_norm_arrow, class = "olink_class")
    expect_s3_class(object = rev_3k_norm_arrow, class = "tbl_df")

    expect_equal(
      object = rev_3k_norm_arrow |>
        dplyr::arrange(
          .data[["SampleID"]],
          .data[["OlinkID"]]
        ) |>
        strip_check_log(),
      expected = data_ref$Reference$NoFormat$Reveal_3K |>
        dplyr::arrange(
          .data[["SampleID"]],
          .data[["OlinkID"]]
        ),
      tolerance = 1e-4
    )

    ### with formatting ----

    withr::with_seed(
      seed = 123,
      code = {
        expect_message(
          object = expect_warning(
            object = expect_message(
              object = expect_message(
                object = expect_message(
                  object = expect_message(
                    object = expect_message(
                      object = expect_warning(
                        object = expect_message(
                          object = rev_3k_norm_format_arrow <-
                            olink_normalization(
                              df1 = data_reveal_arrow,
                              df2 = data_3k_arrow,
                              overlapping_samples_df1 = intersect(
                                x = unique(data_reveal$SampleID),
                                y = unique(data_3k$SampleID)
                              ) |>
                                (\(x) x[!grepl("CONTROL", x)])(),
                              df1_project_nr = "df_reveal",
                              df2_project_nr = "df_3k",
                              reference_project = "df_reveal",
                              format = TRUE
                            ),
                          regexp = "Cross-product normalization will be"
                        ),
                        regexp = "85 assays are not shared across products"
                      ),
                      regexp = "Output includes two sets of bridging samples"
                    ),
                    regexp = paste("10 Plate Controls were removed from datase")
                  ),
                  regexp = paste("6 Negative Controls were removed from datase")
                ),
                regexp = paste("24 not bridgeable assays are included in the",
                               "bridged dataset without adjustment")
              ),
              regexp = paste("85 non-overlapping assays are included in the",
                             "normalized dataset without adjustment")
            ),
            regexp = paste("Duplicate SampleIDs detected:",
                           "\"CONTROL_SAMPLE_1\", \"CONTROL_SAMPLE_2\",",
                           "\"CONTROL_SAMPLE_3\", \"CONTROL_SAMPLE_4\"")
          ),
          regexp = paste("More than one column names in `df` was associated",
                         "with certain key")
        )
      }
    )

    expect_s3_class(object = rev_3k_norm_format_arrow, class = "olink_class")
    expect_s3_class(object = rev_3k_norm_format_arrow, class = "tbl_df")

    expect_equal(
      object = rev_3k_norm_format_arrow |>
        dplyr::arrange(
          .data[["SampleID"]],
          .data[["OlinkID"]]
        ) |>
        strip_check_log(),
      expected = data_ref$Reference$Format$Reveal_3K |>
        dplyr::arrange(
          .data[["SampleID"]],
          .data[["OlinkID"]]
        ),
      tolerance = 1e-4
    )
  }
)

test_that(
  "olink_normalization - works - HT-Reveal",
  {
    data_ht <- get_example_data(filename = "example_HT_data.rds")
    data_reveal <- get_example_data(filename = "example_Reveal_data.rds")

    data_ref <- get_example_data(
      filename = "ref_results_norm_cross-product.rds"
    )

    # tibble ----

    ### no format ----

    withr::with_seed(
      seed = 123,
      code = {
        expect_message(
          object = expect_warning(
            object = expect_warning(
              object = expect_message(
                object = expect_warning(
                  object = expect_message(
                    object = ht_rev_norm <- olink_normalization(
                      df1 = data_reveal,
                      df2 = data_ht,
                      overlapping_samples_df1 = intersect(
                        x = unique(data_reveal$SampleID),
                        y = unique(data_ht$SampleID)
                      ) |>
                        (\(x) x[!grepl("CONTROL", x)])(),
                      df1_project_nr = "df_reveal",
                      df2_project_nr = "df_ht",
                      reference_project = "df_ht",
                      format = FALSE,
                      df1_check_log = check_npx(df = data_reveal) |>
                        suppressMessages() |>
                        suppressWarnings(),
                      df2_check_log = check_npx(df = data_ht) |>
                        suppressMessages() |>
                        suppressWarnings()
                    ),
                    regexp = "Cross-product normalization will be performed!"
                  ),
                  regexp = "80 assays are not shared across products"
                ),
                regexp = "Output includes two sets of bridging samples"
              ),
              regexp = paste("Duplicate SampleIDs detected:",
                             "\"CONTROL_SAMPLE_1\", \"CONTROL_SAMPLE_2\",",
                             "\"CONTROL_SAMPLE_3\", \"CONTROL_SAMPLE_4\"")
            ),
            regexp = paste("Detected multiple UniProt identifiers for assay:",
                           "\"OID54321\"")
          ),
          regexp = paste("More than one column names in `df` was associated",
                         "with certain key")
        )
      }
    )

    expect_equal(
      object = ht_rev_norm |>
        dplyr::arrange(
          .data[["SampleID"]],
          .data[["OlinkID"]]
        ) |>
        strip_check_log(),
      expected = data_ref$Reference$NoFormat$HT_Reveal |>
        dplyr::arrange(
          .data[["SampleID"]],
          .data[["OlinkID"]]
        ),
      tolerance = 1e-4
    )

    ### with formatting ----

    withr::with_seed(
      seed = 123,
      code = {
        expect_message(
          object = expect_warning(
            object = expect_warning(
              object = expect_message(
                object = expect_message(
                  object = expect_message(
                    object = expect_message(
                      object = expect_message(
                        object = expect_warning(
                          object = expect_message(
                            object = ht_rev_norm_format <- olink_normalization(
                              df1 = data_reveal,
                              df2 = data_ht,
                              overlapping_samples_df1 = intersect(
                                x = unique(data_reveal$SampleID),
                                y = unique(data_ht$SampleID)
                              ) |>
                                (\(x) x[!grepl("CONTROL", x)])(),
                              df1_project_nr = "df_reveal",
                              df2_project_nr = "df_ht",
                              reference_project = "df_ht",
                              format = TRUE,
                              df1_check_log = check_npx(df = data_reveal) |>
                                suppressMessages() |>
                                suppressWarnings(),
                              df2_check_log = check_npx(df = data_ht) |>
                                suppressMessages() |>
                                suppressWarnings()
                            ),
                            regexp = "Cross-product normalization will be perfo"
                          ),
                          regexp = "80 assays are not shared across products"
                        ),
                        regexp = "Output includes two sets of bridging samples"
                      ),
                      regexp = paste("10 Plate Controls were removed from data")
                    ),
                    regexp = paste("4 Negative Controls were removed from data")
                  ),
                  regexp = paste("24 not bridgeable assays are included in the",
                                 "bridged dataset without adjustment")
                ),
                regexp = paste("80 non-overlapping assays are included in the",
                               "normalized dataset without adjustment")
              ),
              regexp = paste("Duplicate SampleIDs detected:",
                             "\"CONTROL_SAMPLE_1\", \"CONTROL_SAMPLE_2\",",
                             "\"CONTROL_SAMPLE_3\", \"CONTROL_SAMPLE_4\"")
            ),
            regexp = paste("Detected multiple UniProt identifiers for assay:",
                           "\"OID54321_OID54321\"")
          ),
          regexp = paste("More than one column names in `df` was associated",
                         "with certain key")
        )
      }
    )

    expect_equal(
      object = ht_rev_norm_format |>
        dplyr::arrange(
          .data[["SampleID"]],
          .data[["OlinkID"]]
        ) |>
        strip_check_log(),
      expected = data_ref$Reference$Format$HT_Reveal |>
        dplyr::arrange(
          .data[["SampleID"]],
          .data[["OlinkID"]]
        ),
      tolerance = 1e-4
    )

    # olink_class ----

    expect_no_error(
      object = expect_no_warning(
        object = expect_message(
          object = data_reveal_obj <- attach_check_log(
            df = data_reveal,
            out_df = "tibble"
          ),
          regexp = paste("More than one column names in `df` was associated",
                         "with certain key")
        )
      )
    )

    expect_no_error(
      object = expect_no_warning(
        object = expect_message(
          object = data_ht_obj <- attach_check_log(
            df = data_ht,
            out_df = "tibble"
          ),
          regexp = paste("More than one column names in `df` was associated",
                         "with certain key")
        )
      )
    )

    ### no format ----

    withr::with_seed(
      seed = 123,
      code = {
        expect_message(
          object = expect_warning(
            object = expect_warning(
              object = expect_message(
                object = expect_warning(
                  object = expect_message(
                    object = ht_rev_norm_obj <- olink_normalization(
                      df1 = data_reveal_obj,
                      df2 = data_ht_obj,
                      overlapping_samples_df1 = intersect(
                        x = unique(data_reveal$SampleID),
                        y = unique(data_ht$SampleID)
                      ) |>
                        (\(x) x[!grepl("CONTROL", x)])(),
                      df1_project_nr = "df_reveal",
                      df2_project_nr = "df_ht",
                      reference_project = "df_ht",
                      format = FALSE
                    ),
                    regexp = "Cross-product normalization will be performed!"
                  ),
                  regexp = "80 assays are not shared across products"
                ),
                regexp = "Output includes two sets of bridging samples"
              ),
              regexp = paste("Duplicate SampleIDs detected:",
                             "\"CONTROL_SAMPLE_1\", \"CONTROL_SAMPLE_2\",",
                             "\"CONTROL_SAMPLE_3\", \"CONTROL_SAMPLE_4\"")
            ),
            regexp = paste("Detected multiple UniProt identifiers for assay:",
                           "\"OID54321\"")
          ),
          regexp = paste("More than one column names in `df` was associated",
                         "with certain key")
        )
      }
    )

    expect_s3_class(object = ht_rev_norm_obj, class = "olink_class")
    expect_s3_class(object = ht_rev_norm_obj, class = "tbl_df")

    expect_equal(
      object = ht_rev_norm_obj |>
        dplyr::arrange(
          .data[["SampleID"]],
          .data[["OlinkID"]]
        ) |>
        strip_check_log(),
      expected = data_ref$Reference$NoFormat$HT_Reveal |>
        dplyr::arrange(
          .data[["SampleID"]],
          .data[["OlinkID"]]
        ),
      tolerance = 1e-4
    )

    ### with formatting ----

    withr::with_seed(
      seed = 123,
      code = {
        expect_message(
          object = expect_warning(
            object = expect_warning(
              object = expect_message(
                object = expect_message(
                  object = expect_message(
                    object = expect_message(
                      object = expect_message(
                        object = expect_warning(
                          object = expect_message(
                            object = ht_rev_norm_format_obj <-
                              olink_normalization(
                                df1 = data_reveal_obj,
                                df2 = data_ht_obj,
                                overlapping_samples_df1 = intersect(
                                  x = unique(data_reveal$SampleID),
                                  y = unique(data_ht$SampleID)
                                ) |>
                                  (\(x) x[!grepl("CONTROL", x)])(),
                                df1_project_nr = "df_reveal",
                                df2_project_nr = "df_ht",
                                reference_project = "df_ht",
                                format = TRUE
                              ),
                            regexp = "Cross-product normalization will be perfo"
                          ),
                          regexp = "80 assays are not shared across products"
                        ),
                        regexp = "Output includes two sets of bridging samples"
                      ),
                      regexp = paste("10 Plate Controls were removed from data")
                    ),
                    regexp = paste("4 Negative Controls were removed from data")
                  ),
                  regexp = paste("24 not bridgeable assays are included in the",
                                 "bridged dataset without adjustment")
                ),
                regexp = paste("80 non-overlapping assays are included in the",
                               "normalized dataset without adjustment")
              ),
              regexp = paste("Duplicate SampleIDs detected:",
                             "\"CONTROL_SAMPLE_1\", \"CONTROL_SAMPLE_2\",",
                             "\"CONTROL_SAMPLE_3\", \"CONTROL_SAMPLE_4\"")
            ),
            regexp = paste("Detected multiple UniProt identifiers for assay:",
                           "\"OID54321_OID54321\"")
          ),
          regexp = paste("More than one column names in `df` was associated",
                         "with certain key")
        )
      }
    )

    expect_s3_class(object = ht_rev_norm_format_obj, class = "olink_class")
    expect_s3_class(object = ht_rev_norm_format_obj, class = "tbl_df")

    expect_equal(
      object = ht_rev_norm_format_obj |>
        dplyr::arrange(
          .data[["SampleID"]],
          .data[["OlinkID"]]
        ) |>
        strip_check_log(),
      expected = data_ref$Reference$Format$HT_Reveal |>
        dplyr::arrange(
          .data[["SampleID"]],
          .data[["OlinkID"]]
        ),
      tolerance = 1e-4
    )

    # olink arrow ----

    expect_no_error(
      object = expect_no_warning(
        object = expect_message(
          object = data_reveal_arrow <- attach_check_log(
            df = data_reveal,
            out_df = "arrow"
          ),
          regexp = paste("More than one column names in `df` was associated",
                         "with certain key")
        )
      )
    )

    expect_no_error(
      object = expect_no_warning(
        object = expect_message(
          object = data_ht_arrow <- attach_check_log(
            df = data_ht,
            out_df = "arrow"
          ),
          regexp = paste("More than one column names in `df` was associated",
                         "with certain key")
        )
      )
    )

    ### no format ----

    withr::with_seed(
      seed = 123,
      code = {
        expect_message(
          object = expect_warning(
            object = expect_warning(
              object = expect_message(
                object = expect_warning(
                  object = expect_message(
                    object = ht_rev_norm_arrow <- olink_normalization(
                      df1 = data_reveal_arrow,
                      df2 = data_ht_arrow,
                      overlapping_samples_df1 = intersect(
                        x = unique(data_reveal$SampleID),
                        y = unique(data_ht$SampleID)
                      ) |>
                        (\(x) x[!grepl("CONTROL", x)])(),
                      df1_project_nr = "df_reveal",
                      df2_project_nr = "df_ht",
                      reference_project = "df_ht",
                      format = FALSE
                    ),
                    regexp = "Cross-product normalization will be performed!"
                  ),
                  regexp = "80 assays are not shared across products"
                ),
                regexp = "Output includes two sets of bridging samples"
              ),
              regexp = paste("Duplicate SampleIDs detected:",
                             "\"CONTROL_SAMPLE_1\", \"CONTROL_SAMPLE_2\",",
                             "\"CONTROL_SAMPLE_3\", \"CONTROL_SAMPLE_4\"")
            ),
            regexp = paste("Detected multiple UniProt identifiers for assay:",
                           "\"OID54321\"")
          ),
          regexp = paste("More than one column names in `df` was associated",
                         "with certain key")
        )
      }
    )

    expect_s3_class(object = ht_rev_norm_arrow, class = "olink_class")
    expect_s3_class(object = ht_rev_norm_arrow, class = "tbl_df")

    expect_equal(
      object = ht_rev_norm_arrow |>
        dplyr::arrange(
          .data[["SampleID"]],
          .data[["OlinkID"]]
        ) |>
        strip_check_log(),
      expected = data_ref$Reference$NoFormat$HT_Reveal |>
        dplyr::arrange(
          .data[["SampleID"]],
          .data[["OlinkID"]]
        ),
      tolerance = 1e-4
    )

    ### with formatting ----

    withr::with_seed(
      seed = 123,
      code = {
        expect_message(
          object = expect_warning(
            object = expect_warning(
              object = expect_message(
                object = expect_message(
                  object = expect_message(
                    object = expect_message(
                      object = expect_message(
                        object = expect_warning(
                          object = expect_message(
                            object = ht_rev_norm_format_arrow <-
                              olink_normalization(
                                df1 = data_reveal_arrow,
                                df2 = data_ht_arrow,
                                overlapping_samples_df1 = intersect(
                                  x = unique(data_reveal$SampleID),
                                  y = unique(data_ht$SampleID)
                                ) |>
                                  (\(x) x[!grepl("CONTROL", x)])(),
                                df1_project_nr = "df_reveal",
                                df2_project_nr = "df_ht",
                                reference_project = "df_ht",
                                format = TRUE
                              ),
                            regexp = "Cross-product normalization will be perfo"
                          ),
                          regexp = "80 assays are not shared across products"
                        ),
                        regexp = "Output includes two sets of bridging samples"
                      ),
                      regexp = paste("10 Plate Controls were removed from data")
                    ),
                    regexp = paste("4 Negative Controls were removed from data")
                  ),
                  regexp = paste("24 not bridgeable assays are included in the",
                                 "bridged dataset without adjustment")
                ),
                regexp = paste("80 non-overlapping assays are included in the",
                               "normalized dataset without adjustment")
              ),
              regexp = paste("Duplicate SampleIDs detected:",
                             "\"CONTROL_SAMPLE_1\", \"CONTROL_SAMPLE_2\",",
                             "\"CONTROL_SAMPLE_3\", \"CONTROL_SAMPLE_4\"")
            ),
            regexp = paste("Detected multiple UniProt identifiers for assay:",
                           "\"OID54321_OID54321\"")
          ),
          regexp = paste("More than one column names in `df` was associated",
                         "with certain key")
        )
      }
    )

    expect_s3_class(object = ht_rev_norm_format_arrow, class = "olink_class")
    expect_s3_class(object = ht_rev_norm_format_arrow, class = "tbl_df")

    expect_equal(
      object = ht_rev_norm_format_arrow |>
        dplyr::arrange(
          .data[["SampleID"]],
          .data[["OlinkID"]]
        ) |>
        strip_check_log(),
      expected = data_ref$Reference$Format$HT_Reveal |>
        dplyr::arrange(
          .data[["SampleID"]],
          .data[["OlinkID"]]
        ),
      tolerance = 1e-4
    )
  }
)

test_that(
  "olink_normalization - works - Reveal-HT",
  {
    data_ht <- get_example_data(filename = "example_HT_data.rds")
    data_reveal <- get_example_data(filename = "example_Reveal_data.rds")

    data_ref <- get_example_data(
      filename = "ref_results_norm_cross-product.rds"
    )

    # tibble ----

    ### no format ----

    withr::with_seed(
      seed = 123,
      code = {
        expect_message(
          object = expect_warning(
            object = expect_warning(
              object = expect_message(
                object = expect_warning(
                  object = expect_message(
                    object = rev_ht_norm <- olink_normalization(
                      df1 = data_reveal,
                      df2 = data_ht,
                      overlapping_samples_df1 = intersect(
                        x = unique(data_reveal$SampleID),
                        y = unique(data_ht$SampleID)
                      ) |>
                        (\(x) x[!grepl("CONTROL", x)])(),
                      df1_project_nr = "df_reveal",
                      df2_project_nr = "df_ht",
                      reference_project = "df_reveal",
                      format = FALSE,
                      df1_check_log = check_npx(df = data_reveal) |>
                        suppressMessages() |>
                        suppressWarnings(),
                      df2_check_log = check_npx(df = data_ht) |>
                        suppressMessages() |>
                        suppressWarnings()
                    ),
                    regexp = "Cross-product normalization will be performed!"
                  ),
                  regexp = "80 assays are not shared across products"
                ),
                regexp = "Output includes two sets of bridging samples"
              ),
              regexp = paste("Duplicate SampleIDs detected:",
                             "\"CONTROL_SAMPLE_1\", \"CONTROL_SAMPLE_2\",",
                             "\"CONTROL_SAMPLE_3\", \"CONTROL_SAMPLE_4\"")
            ),
            regexp = paste("Detected multiple UniProt identifiers for assay:",
                           "\"OID54321\"")
          ),
          regexp = paste("More than one column names in `df` was associated",
                         "with certain key")
        )
      }
    )

    expect_equal(
      object = rev_ht_norm |>
        dplyr::arrange(
          .data[["SampleID"]],
          .data[["OlinkID"]]
        ) |>
        strip_check_log(),
      expected = data_ref$Reference$NoFormat$Reveal_HT |>
        dplyr::arrange(
          .data[["SampleID"]],
          .data[["OlinkID"]]
        ),
      tolerance = 1e-4
    )

    ### with formatting ----

    withr::with_seed(
      seed = 123,
      code = {
        expect_message(
          object = expect_warning(
            object = expect_warning(
              object = expect_message(
                object = expect_message(
                  object = expect_message(
                    object = expect_message(
                      object = expect_message(
                        object = expect_warning(
                          object = expect_message(
                            object = rev_ht_norm_format <- olink_normalization(
                              df1 = data_reveal,
                              df2 = data_ht,
                              overlapping_samples_df1 = intersect(
                                x = unique(data_reveal$SampleID),
                                y = unique(data_ht$SampleID)
                              ) |>
                                (\(x) x[!grepl("CONTROL", x)])(),
                              df1_project_nr = "df_reveal",
                              df2_project_nr = "df_ht",
                              reference_project = "df_reveal",
                              format = TRUE,
                              df1_check_log = check_npx(df = data_reveal) |>
                                suppressMessages() |>
                                suppressWarnings(),
                              df2_check_log = check_npx(df = data_ht) |>
                                suppressMessages() |>
                                suppressWarnings()
                            ),
                            regexp = "Cross-product normalization will be perfo"
                          ),
                          regexp = "80 assays are not shared across products"
                        ),
                        regexp = "Output includes two sets of bridging samples"
                      ),
                      regexp = paste("10 Plate Controls were removed from data")
                    ),
                    regexp = paste("4 Negative Controls were removed from data")
                  ),
                  regexp = paste("24 not bridgeable assays are included in the",
                                 "bridged dataset without adjustment")
                ),
                regexp = paste("80 non-overlapping assays are included in the",
                               "normalized dataset without adjustment")
              ),
              regexp = paste("Duplicate SampleIDs detected:",
                             "\"CONTROL_SAMPLE_1\", \"CONTROL_SAMPLE_2\",",
                             "\"CONTROL_SAMPLE_3\", \"CONTROL_SAMPLE_4\"")
            ),
            regexp = paste("Detected multiple UniProt identifiers for assay:",
                           "\"OID54321_OID54321\"")
          ),
          regexp = paste("More than one column names in `df` was associated",
                         "with certain key")
        )
      }
    )

    expect_equal(
      object = rev_ht_norm_format |>
        dplyr::arrange(
          .data[["SampleID"]],
          .data[["OlinkID"]]
        ) |>
        strip_check_log(),
      expected = data_ref$Reference$Format$Reveal_HT |>
        dplyr::arrange(
          .data[["SampleID"]],
          .data[["OlinkID"]]
        ),
      tolerance = 1e-4
    )

    # olink_class ----

    expect_no_error(
      object = expect_no_warning(
        object = expect_message(
          object = data_reveal_obj <- attach_check_log(
            df = data_reveal,
            out_df = "tibble"
          ),
          regexp = paste("More than one column names in `df` was associated",
                         "with certain key")
        )
      )
    )

    expect_no_error(
      object = expect_no_warning(
        object = expect_message(
          object = data_ht_obj <- attach_check_log(
            df = data_ht,
            out_df = "tibble"
          ),
          regexp = paste("More than one column names in `df` was associated",
                         "with certain key")
        )
      )
    )

    ### no format ----

    withr::with_seed(
      seed = 123,
      code = {
        expect_message(
          object = expect_warning(
            object = expect_warning(
              object = expect_message(
                object = expect_warning(
                  object = expect_message(
                    object = rev_ht_norm_obj <- olink_normalization(
                      df1 = data_reveal_obj,
                      df2 = data_ht_obj,
                      overlapping_samples_df1 = intersect(
                        x = unique(data_reveal$SampleID),
                        y = unique(data_ht$SampleID)
                      ) |>
                        (\(x) x[!grepl("CONTROL", x)])(),
                      df1_project_nr = "df_reveal",
                      df2_project_nr = "df_ht",
                      reference_project = "df_reveal",
                      format = FALSE
                    ),
                    regexp = "Cross-product normalization will be performed!"
                  ),
                  regexp = "80 assays are not shared across products"
                ),
                regexp = "Output includes two sets of bridging samples"
              ),
              regexp = paste("Duplicate SampleIDs detected:",
                             "\"CONTROL_SAMPLE_1\", \"CONTROL_SAMPLE_2\",",
                             "\"CONTROL_SAMPLE_3\", \"CONTROL_SAMPLE_4\"")
            ),
            regexp = paste("Detected multiple UniProt identifiers for assay:",
                           "\"OID54321\"")
          ),
          regexp = paste("More than one column names in `df` was associated",
                         "with certain key")
        )
      }
    )

    expect_s3_class(object = rev_ht_norm_obj, class = "olink_class")
    expect_s3_class(object = rev_ht_norm_obj, class = "tbl_df")

    expect_equal(
      object = rev_ht_norm_obj |>
        dplyr::arrange(
          .data[["SampleID"]],
          .data[["OlinkID"]]
        ) |>
        strip_check_log(),
      expected = data_ref$Reference$NoFormat$Reveal_HT |>
        dplyr::arrange(
          .data[["SampleID"]],
          .data[["OlinkID"]]
        ),
      tolerance = 1e-4
    )

    ### with formatting ----

    withr::with_seed(
      seed = 123,
      code = {
        expect_message(
          object = expect_warning(
            object = expect_warning(
              object = expect_message(
                object = expect_message(
                  object = expect_message(
                    object = expect_message(
                      object = expect_message(
                        object = expect_warning(
                          object = expect_message(
                            object = rev_ht_norm_format_obj <-
                              olink_normalization(
                              df1 = data_reveal_obj,
                              df2 = data_ht_obj,
                              overlapping_samples_df1 = intersect(
                                x = unique(data_reveal$SampleID),
                                y = unique(data_ht$SampleID)
                              ) |>
                                (\(x) x[!grepl("CONTROL", x)])(),
                              df1_project_nr = "df_reveal",
                              df2_project_nr = "df_ht",
                              reference_project = "df_reveal",
                              format = TRUE
                            ),
                            regexp = "Cross-product normalization will be perfo"
                          ),
                          regexp = "80 assays are not shared across products"
                        ),
                        regexp = "Output includes two sets of bridging samples"
                      ),
                      regexp = paste("10 Plate Controls were removed from data")
                    ),
                    regexp = paste("4 Negative Controls were removed from data")
                  ),
                  regexp = paste("24 not bridgeable assays are included in the",
                                 "bridged dataset without adjustment")
                ),
                regexp = paste("80 non-overlapping assays are included in the",
                               "normalized dataset without adjustment")
              ),
              regexp = paste("Duplicate SampleIDs detected:",
                             "\"CONTROL_SAMPLE_1\", \"CONTROL_SAMPLE_2\",",
                             "\"CONTROL_SAMPLE_3\", \"CONTROL_SAMPLE_4\"")
            ),
            regexp = paste("Detected multiple UniProt identifiers for assay:",
                           "\"OID54321_OID54321\"")
          ),
          regexp = paste("More than one column names in `df` was associated",
                         "with certain key")
        )
      }
    )

    expect_s3_class(object = rev_ht_norm_format_obj, class = "olink_class")
    expect_s3_class(object = rev_ht_norm_format_obj, class = "tbl_df")

    expect_equal(
      object = rev_ht_norm_format_obj |>
        dplyr::arrange(
          .data[["SampleID"]],
          .data[["OlinkID"]]
        ) |>
        strip_check_log(),
      expected = data_ref$Reference$Format$Reveal_HT |>
        dplyr::arrange(
          .data[["SampleID"]],
          .data[["OlinkID"]]
        ),
      tolerance = 1e-4
    )

    # olink arrow ----

    expect_no_error(
      object = expect_no_warning(
        object = expect_message(
          object = data_reveal_arrow <- attach_check_log(
            df = data_reveal,
            out_df = "arrow"
          ),
          regexp = paste("More than one column names in `df` was associated",
                         "with certain key")
        )
      )
    )

    expect_no_error(
      object = expect_no_warning(
        object = expect_message(
          object = data_ht_arrow <- attach_check_log(
            df = data_ht,
            out_df = "arrow"
          ),
          regexp = paste("More than one column names in `df` was associated",
                         "with certain key")
        )
      )
    )

    ### no format ----

    withr::with_seed(
      seed = 123,
      code = {
        expect_message(
          object = expect_warning(
            object = expect_warning(
              object = expect_message(
                object = expect_warning(
                  object = expect_message(
                    object = rev_ht_norm_arrow <- olink_normalization(
                      df1 = data_reveal_arrow,
                      df2 = data_ht_arrow,
                      overlapping_samples_df1 = intersect(
                        x = unique(data_reveal$SampleID),
                        y = unique(data_ht$SampleID)
                      ) |>
                        (\(x) x[!grepl("CONTROL", x)])(),
                      df1_project_nr = "df_reveal",
                      df2_project_nr = "df_ht",
                      reference_project = "df_reveal",
                      format = FALSE
                    ),
                    regexp = "Cross-product normalization will be performed!"
                  ),
                  regexp = "80 assays are not shared across products"
                ),
                regexp = "Output includes two sets of bridging samples"
              ),
              regexp = paste("Duplicate SampleIDs detected:",
                             "\"CONTROL_SAMPLE_1\", \"CONTROL_SAMPLE_2\",",
                             "\"CONTROL_SAMPLE_3\", \"CONTROL_SAMPLE_4\"")
            ),
            regexp = paste("Detected multiple UniProt identifiers for assay:",
                           "\"OID54321\"")
          ),
          regexp = paste("More than one column names in `df` was associated",
                         "with certain key")
        )
      }
    )

    expect_s3_class(object = rev_ht_norm_arrow, class = "olink_class")
    expect_s3_class(object = rev_ht_norm_arrow, class = "tbl_df")

    expect_equal(
      object = rev_ht_norm_arrow |>
        dplyr::arrange(
          .data[["SampleID"]],
          .data[["OlinkID"]]
        ) |>
        strip_check_log(),
      expected = data_ref$Reference$NoFormat$Reveal_HT |>
        dplyr::arrange(
          .data[["SampleID"]],
          .data[["OlinkID"]]
        ),
      tolerance = 1e-4
    )

    ### with formatting ----

    withr::with_seed(
      seed = 123,
      code = {
        expect_message(
          object = expect_warning(
            object = expect_warning(
              object = expect_message(
                object = expect_message(
                  object = expect_message(
                    object = expect_message(
                      object = expect_message(
                        object = expect_warning(
                          object = expect_message(
                            object = rev_ht_norm_format_arrow <-
                              olink_normalization(
                                df1 = data_reveal_arrow,
                                df2 = data_ht_arrow,
                                overlapping_samples_df1 = intersect(
                                  x = unique(data_reveal$SampleID),
                                  y = unique(data_ht$SampleID)
                                ) |>
                                  (\(x) x[!grepl("CONTROL", x)])(),
                                df1_project_nr = "df_reveal",
                                df2_project_nr = "df_ht",
                                reference_project = "df_reveal",
                                format = TRUE
                              ),
                            regexp = "Cross-product normalization will be perfo"
                          ),
                          regexp = "80 assays are not shared across products"
                        ),
                        regexp = "Output includes two sets of bridging samples"
                      ),
                      regexp = paste("10 Plate Controls were removed from data")
                    ),
                    regexp = paste("4 Negative Controls were removed from data")
                  ),
                  regexp = paste("24 not bridgeable assays are included in the",
                                 "bridged dataset without adjustment")
                ),
                regexp = paste("80 non-overlapping assays are included in the",
                               "normalized dataset without adjustment")
              ),
              regexp = paste("Duplicate SampleIDs detected:",
                             "\"CONTROL_SAMPLE_1\", \"CONTROL_SAMPLE_2\",",
                             "\"CONTROL_SAMPLE_3\", \"CONTROL_SAMPLE_4\"")
            ),
            regexp = paste("Detected multiple UniProt identifiers for assay:",
                           "\"OID54321_OID54321\"")
          ),
          regexp = paste("More than one column names in `df` was associated",
                         "with certain key")
        )
      }
    )

    expect_s3_class(object = rev_ht_norm_format_arrow, class = "olink_class")
    expect_s3_class(object = rev_ht_norm_format_arrow, class = "tbl_df")

    expect_equal(
      object = rev_ht_norm_format_arrow |>
        dplyr::arrange(
          .data[["SampleID"]],
          .data[["OlinkID"]]
        ) |>
        strip_check_log(),
      expected = data_ref$Reference$Format$Reveal_HT |>
        dplyr::arrange(
          .data[["SampleID"]],
          .data[["OlinkID"]]
        ),
      tolerance = 1e-4
    )
  }
)

# Test norm_internal_rename_cols ----

test_that(
  "norm_internal_rename_cols - works",
  {
    skip_if_not_installed("arrow")

    ## example 1: panel_version ----

    ref_df <- npx_data2 |>
      dplyr::rename(
        "Panel_Lot_Nr" = "Panel_Version"
      ) |>
      dplyr::mutate(
        Normalization = "Intensity"
      )
    not_ref_df <- npx_data1 |>
      dplyr::mutate(
        Normalization = "Intensity"
      )

    ref_df_check <- check_npx(df = ref_df) |>
      suppressMessages() |>
      suppressWarnings()

    not_ref_df_check <- check_npx(df = not_ref_df) |>
      suppressMessages() |>
      suppressWarnings()

    update_cnames_v1 <- norm_internal_preferred_names(
      ref_cols = ref_df_check$col_names,
      not_ref_cols = not_ref_df_check$col_names
    )

    expect_identical(
      object = update_cnames_v1,
      expected = c("panel_version" = "Panel_Lot_Nr")
    )

    ## example 2: panel_version & qc_warn ----

    ref_df_v2 <- npx_data1 |>
      dplyr::rename(
        "SampleQC" = "QC_Warning"
      ) |>
      dplyr::mutate(
        Normalization = "Intensity"
      )
    not_ref_df_v2 <- npx_data2 |>
      dplyr::rename(
        "Panel_Lot_Nr" = "Panel_Version"
      ) |>
      dplyr::mutate(
        Normalization = "Intensity"
      )

    ref_df_v2_check <- check_npx(df = ref_df_v2) |>
      suppressMessages() |>
      suppressWarnings()

    not_ref_df_v2_check <- check_npx(df = not_ref_df_v2) |>
      suppressMessages() |>
      suppressWarnings()

    update_cnames_v2 <- norm_internal_preferred_names(
      ref_cols = ref_df_v2_check$col_names,
      not_ref_cols = not_ref_df_v2_check$col_names
    )

    expect_identical(
      object = update_cnames_v2,
      expected = c("panel_version" = "Panel_Version", "qc_warning" = "SampleQC")
    )

    ## example 2: panel_version, qc_warn, assay_qc - arrow ----

    ref_df_v3 <- npx_data1 |>
      dplyr::rename(
        "SampleQC" = "QC_Warning"
      ) |>
      dplyr::mutate(
        Normalization = "Intensity",
        AssayQC = "Pass"
      ) |>
      arrow::as_arrow_table()

    not_ref_df_v3 <-  npx_data2 |>
      dplyr::rename(
        "Panel_Lot_Nr" = "Panel_Version"
      ) |>
      dplyr::mutate(
        Normalization = "Intensity",
        Assay_Warning = "Pass"
      ) |>
      arrow::as_arrow_table()

    ref_df_v3_check <- check_npx(df = ref_df_v3) |>
      suppressMessages() |>
      suppressWarnings()

    not_ref_df_v3_check <- check_npx(df = not_ref_df_v3) |>
      suppressMessages() |>
      suppressWarnings()

    update_cnames_v3 <- norm_internal_preferred_names(
      ref_cols = ref_df_v3_check$col_names,
      not_ref_cols = not_ref_df_v3_check$col_names
    )

    expect_identical(
      object = update_cnames_v3,
      expected = c("panel_version" = "Panel_Version",
                   "qc_warning" = "SampleQC",
                   "assay_warn" = "AssayQC")
    )

  }
)

test_that(
  "norm_internal_rename_cols - works - no error when AssayQC is missing",
  {
    skip_if_not_installed("arrow")

    # AssayQC in reference only ----

    ref_df <- npx_data1 |>
      dplyr::rename(
        "SampleQC" = "QC_Warning"
      ) |>
      dplyr::mutate(
        Normalization = "Intensity",
        AssayQC = "Pass"
      ) |>
      arrow::as_arrow_table()

    not_ref_df <- npx_data2 |>
      dplyr::rename(
        "Panel_Lot_Nr" = "Panel_Version"
      ) |>
      dplyr::mutate(
        Normalization = "Intensity"
      ) |>
      arrow::as_arrow_table()

    ref_df_check <- check_npx(df = ref_df) |>
      suppressMessages() |>
      suppressWarnings()

    not_ref_df_check <- check_npx(df = not_ref_df) |>
      suppressMessages() |>
      suppressWarnings()

    update_cnames_v1 <- norm_internal_preferred_names(
      ref_cols = ref_df_check$col_names,
      not_ref_cols = not_ref_df_check$col_names
    )

    # ensure that
    expect_identical(
      object = update_cnames_v1,
      expected = c("panel_version" = "Panel_Version", "qc_warning" = "SampleQC")
    )

    # AssayQC in non-reference only ----

    ref_df_v2 <- npx_data2 |>
      dplyr::rename(
        "Panel_Lot_Nr" = "Panel_Version"
      ) |>
      dplyr::mutate(
        Normalization = "Intensity"
      ) |>
      arrow::as_arrow_table()

    not_ref_df_v2 <- npx_data1 |>
      dplyr::rename(
        "SampleQC" = "QC_Warning"
      ) |>
      dplyr::mutate(
        Normalization = "Intensity",
        AssayQC = "Pass"
      ) |>
      arrow::as_arrow_table()

    ref_df_v2_check <- check_npx(df = ref_df_v2) |>
      suppressMessages() |>
      suppressWarnings()

    not_ref_df_v2_check <- check_npx(df = not_ref_df_v2) |>
      suppressMessages() |>
      suppressWarnings()

    update_cnames_v2 <- norm_internal_preferred_names(
      ref_cols = ref_df_v2_check$col_names,
      not_ref_cols = not_ref_df_v2_check$col_names
    )

    # ensure that
    expect_identical(
      object = update_cnames_v2,
      expected = c("panel_version" = "Panel_Lot_Nr",
                   "qc_warning" = "QC_Warning")
    )

  }
)

test_that(
  "norm_internal_rename_cols - works - all input combos",
  {
    ref_lst <- list(
      "x1" = c("A"),
      "x2" = c("A"),
      "x3" = c("A"),
      "x4" = c("A"),
      "x5" = c("A"),
      "x6" = c("A"),
      "x7" = c("B"),
      # "x8" = NULL, missing
      "x9" = c("A", "B"),
      "x10" = c("B", "C"),
      "x11" = c("A", "B")
      # "x12" = NULL missing
    )

    not_ref_lst <- list(
      "x1" = c("A"),
      "x2" = c("B"),
      # "x3" = NULL, missing
      "x4" = c("A", "B"),
      "x5" = c("B", "C"),
      "x6" = c("A"),
      "x7" = c("A"),
      "x8" = c("A"),
      "x9" = c("A"),
      "x10" = c("A"),
      # "x11" = NULL, missing
      "x12" = c("A", "B")
    )

    expect_no_condition(
      object = update_cnames <- norm_internal_preferred_names(
        ref_cols = ref_lst,
        not_ref_cols = not_ref_lst
      )
    )

    # ensure that
    expect_identical(
      object = update_cnames,
      expected = c("x2" = "A", "x7" = "B")
    )
  }
)

# Cross-product specific tests ----

test_that(
  "Cross product normalization - works - correlation assays present",
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

    # correlation assay IDs
    oid_ht <- "OID43204"
    oid_3k <- eHT_e3072_mapping |>
      dplyr::filter(
        .data[["OlinkID_HT"]] == oid_ht
      ) |>
      dplyr::pull(
        .data[["OlinkID_E3072"]]
      )

    df_ht_mod <- data_ht |>
      dplyr::filter(
        !(.data[["OlinkID"]] %in% c("OID12345", "OID54321"))
      )

    df_3k_mod <- data_3k |>
      dplyr::filter(
        !(.data[["OlinkID"]] %in% c("OID12345", "OID54321"))
      )

    # HT correlation is present
    expect_warning(
      object = expect_message(
        object = expect_message(
          object = expect_message(
            object = expect_contains(
              object = olink_normalization(
                df1 = df_ht_mod,
                df2 = df_3k_mod,
                overlapping_samples_df1 = bridge_samples,
                df1_project_nr = "proj_ht",
                df2_project_nr = "proj_3k",
                reference_project = "proj_ht",
                format = FALSE,
                df1_check_log = check_npx(df = df_ht_mod) |>
                  suppressMessages() |>
                  suppressWarnings(),
                df2_check_log = check_npx(df = df_3k_mod) |>
                  suppressMessages() |>
                  suppressWarnings()
              ) |>
                dplyr::distinct(OlinkID) |>
                dplyr::pull(),
              expected = oid_ht
            ),
            regexp = "Cross-product normalization will be performed!"
          ),
          regexp = "Output includes two sets of bridging samples"
        ),
        regexp = paste("More than one column names in `df` was associated with",
                       "certain key")
      ),
      regexp = paste("Duplicate SampleIDs detected: \"CONTROL_SAMPLE_1\",",
                     "\"CONTROL_SAMPLE_2\", \"CONTROL_SAMPLE_3\",",
                     "\"CONTROL_SAMPLE_4\"")
    )

    # All 3k correlations are present
    expect_warning(
      object = expect_message(
        object = expect_message(
          object = expect_message(
            object = expect_contains(
              object = olink_normalization(
                df1 = df_ht_mod,
                df2 = df_3k_mod,
                overlapping_samples_df1 = bridge_samples,
                df1_project_nr = "proj_ht",
                df2_project_nr = "proj_3k",
                reference_project = "proj_ht",
                format = FALSE,
                df1_check_log = check_npx(df = df_ht_mod) |>
                  suppressMessages() |>
                  suppressWarnings(),
                df2_check_log = check_npx(df = df_3k_mod) |>
                  suppressMessages() |>
                  suppressWarnings()
              ) |>
                dplyr::distinct(OlinkID_E3072) |>
                dplyr::pull(),
              expected = oid_3k
            ),
            regexp = "Cross-product normalization will be performed!"
          ),
          regexp = "Output includes two sets of bridging samples"
        ),
        regexp = paste("More than one column names in `df` was associated with",
                       "certain key")
      ),
      regexp = paste("Duplicate SampleIDs detected: \"CONTROL_SAMPLE_1\",",
                     "\"CONTROL_SAMPLE_2\", \"CONTROL_SAMPLE_3\",",
                     "\"CONTROL_SAMPLE_4\"")
    )
  }
)

test_that(
  "norm_internal_cross_product missing counts - error",
  {
    data_3k <- get_example_data(filename = "example_3k_data.rds")
    data_ht <- get_example_data(filename = "example_HT_data.rds")

    ## error v1: reference/HT and new/3K DF missing count column ----

    data_ht_mod_v1 <- data_ht |> dplyr::select(-dplyr::all_of("Count"))
    data_3k_mod_v1 <- data_3k |> dplyr::select(-dplyr::all_of("Count"))

    expect_error(
      object = olink_normalization(
        df1 = data_ht_mod_v1,
        df2 = data_3k_mod_v1,
        overlapping_samples_df1 = intersect(data_ht$SampleID, data_3k$SampleID),
        df1_project_nr = "proj_ht",
        df2_project_nr = "proj_3k",
        reference_project = "proj_ht",
        format = FALSE,
        df1_check_log = check_npx(df = data_ht_mod_v1) |>
          suppressMessages() |>
          suppressWarnings(),
        df2_check_log = check_npx(df = data_3k_mod_v1) |>
          suppressMessages() |>
          suppressWarnings()
      ),
      regexp = "Column \"Count\" not found in datasets \"proj_ht\" and"
    )

    ## error v2: reference/HT DF missing count column ----

    data_ht_mod_v2 <- data_ht |> dplyr::select(-dplyr::all_of("Count"))
    data_3k_mod_v2 <- data_3k

    expect_error(
      object = olink_normalization(
        df1 = data_ht_mod_v2,
        df2 = data_3k_mod_v2,
        overlapping_samples_df1 = intersect(data_ht$SampleID, data_3k$SampleID),
        df1_project_nr = "proj_ht",
        df2_project_nr = "proj_3k",
        reference_project = "proj_ht",
        format = FALSE,
        df1_check_log = check_npx(df = data_ht_mod_v2) |>
          suppressMessages() |>
          suppressWarnings(),
        df2_check_log = check_npx(df = data_3k_mod_v2) |>
          suppressMessages() |>
          suppressWarnings()
      ),
      regexp = "Column \"Count\" not found in dataset \"proj_ht\"!"
    )

    ## error v3: new/3K DF missing count column ----

    data_ht_mod_v3 <- data_ht
    data_3k_mod_v3 <- data_3k |> dplyr::select(-dplyr::all_of("Count"))

    expect_error(
      object = olink_normalization(
        df1 = data_ht_mod_v3,
        df2 = data_3k_mod_v3,
        overlapping_samples_df1 = intersect(data_ht$SampleID, data_3k$SampleID),
        df1_project_nr = "proj_ht",
        df2_project_nr = "proj_3k",
        reference_project = "proj_ht",
        format = FALSE,
        df1_check_log = check_npx(df = data_ht_mod_v3) |>
          suppressMessages() |>
          suppressWarnings(),
        df2_check_log = check_npx(df = data_3k_mod_v3) |>
          suppressMessages() |>
          suppressWarnings()
      ),
      regexp = "Column \"Count\" not found in dataset \"proj_3k\"!"
    )

  }
)

test_that(
  "olink_normalization - works - old3kformat-HT normalization",
  {
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
                reference_project = "df_ht",
                format = FALSE,
                df1_check_log = check_npx(df = data_ht) |>
                  suppressMessages() |>
                  suppressWarnings(),
                df2_check_log = check_npx(df = data_3k_old) |>
                  suppressMessages() |>
                  suppressWarnings()
              ),
              regexp = "Cross-product normalization will be performed!"
            ),
            regexp = "2 assays are not shared across products."
          ),
          regexp = "Output includes two sets of bridging samples"
        ),
        regexp = paste("Duplicate SampleIDs detected:",
                       "\"CONTROL_SAMPLE_1\", \"CONTROL_SAMPLE_2\",",
                       "\"CONTROL_SAMPLE_3\"")
      ),
      regexp = paste("More than one column names in `df` was associated",
                     "with certain key")
    )

    expect_identical(
      object = dim(ht_3k_norm),
      expected = c(39936L, 25L)
    )

    expect_identical(
      object = names(ht_3k_norm),
      expected = c("SampleID", "OlinkID", "SampleType", "WellID", "PlateID",
                   "UniProt", "Assay", "AssayType", "Panel", "Block", "NPX",
                   "PCNormalizedNPX", "Count", "Normalization", "AssayQC",
                   "SampleQC", "DataAnalysisRefID", "Project", "OlinkID_E3072",
                   "Sample_Type", "Assay_Warning", "QC_Warning",
                   "MedianCenteredNPX", "QSNormalizedNPX",
                   "BridgingRecommendation")
    )

  }
)
