# Test olink_normalization_format ----

# Verification of correctness of results from olink_normalization_format
# are done in combination with olink_normalization. Here we test only that the
# relevant messages are shown.

test_that(
  "olink_normalization_format - works - within-product bridge norm",
  {
    skip_if_not(file.exists(test_path("data", "ref_results_norm.rds")))

    ref_norm_res <- get_example_data("ref_results_norm.rds")

    # all assays present in both datasets ----

    df1_bridge_v1 <- ref_norm_res$lst_df$df1_norm
    df2_bridge_v1 <- ref_norm_res$lst_df$df2_norm

    df_norm_bridge_v1 <- ref_norm_res$lst_norm$bridge_norm$norm

    # run input check for normalization
    check_lst_bridge_v1 <- olink_norm_input_check(
      df1 = df1_bridge_v1,
      df2 = df2_bridge_v1,
      overlapping_samples_df1 = ref_norm_res$lst_sample$bridge_samples,
      overlapping_samples_df2 = NULL,
      df1_project_nr = "df1_norm",
      df2_project_nr = "df2_norm",
      reference_project = "df1_norm",
      reference_medians = NULL
    ) |>
      suppressMessages() |>
      suppressWarnings()

    # check that function returns formatted data
    expect_no_condition(
      object = olink_normalization_format(
        df_norm = df_norm_bridge_v1,
        lst_check = check_lst_bridge_v1
      )
    )

    # assays not present in all datasets ----

    oid_only_in_ref_v2 <- c("OID01216", "OID01217")
    oid_only_in_nonref_v2 <- c("OID01218", "OID01219")

    df1_bridge_v2 <- ref_norm_res$lst_df$df1_norm |>
      dplyr::filter(
        !(.data[["OlinkID"]] %in% .env[["oid_only_in_nonref_v2"]])
      )
    df2_bridge_v2 <- ref_norm_res$lst_df$df2_norm |>
      dplyr::filter(
        !(.data[["OlinkID"]] %in% .env[["oid_only_in_ref_v2"]])
      )

    df_norm_bridge_v2 <- ref_norm_res$lst_norm$bridge_norm$norm

    # run input check for normalization
    check_lst_bridge_v2 <- olink_norm_input_check(
      df1 = df1_bridge_v2,
      df2 = df2_bridge_v2,
      overlapping_samples_df1 = ref_norm_res$lst_sample$bridge_samples,
      overlapping_samples_df2 = NULL,
      df1_project_nr = "df1_norm",
      df2_project_nr = "df2_norm",
      reference_project = "df1_norm",
      reference_medians = NULL
    ) |>
      suppressMessages() |>
      suppressWarnings()

    # check that function returns formatted data
    expect_message(
      object = olink_normalization_format(
        df_norm = df_norm_bridge_v2,
        lst_check = check_lst_bridge_v2
      ),
      regexp = paste("4 non-overlapping assays are included in the",
                     "normalized dataset without adjustment")
    )

    # external controls in datasets ----

    df1_bridge_v3 <- ref_norm_res$lst_df$df1_norm |>
      dplyr::mutate(
        SampleID = dplyr::case_match(
          .data[["SampleID"]],
          "A6" ~ "NEG_CTRL",
          "A38" ~ "PLATE_CTRL",
          .default = .data[["SampleID"]]
        )
      )
    df2_bridge_v3 <- ref_norm_res$lst_df$df2_norm |>
      dplyr::mutate(
        SampleID = dplyr::case_match(
          .data[["SampleID"]],
          "C66" ~ "NEG_CTRL",
          "D1" ~ "PLATE_CTRL",
          .default = .data[["SampleID"]]
        )
      )

    df_norm_bridge_v3 <- ref_norm_res$lst_norm$bridge_norm$norm |>
      dplyr::mutate(
        SampleID = dplyr::case_match(
          .data[["SampleID"]],
          "A6" ~ "NEG_CTRL",
          "A38" ~ "PLATE_CTRL",
          "C66" ~ "NEG_CTRL",
          "D1" ~ "PLATE_CTRL",
          .default = .data[["SampleID"]]
        )
      )

    # run input check for normalization
    check_lst_bridge_v3 <- olink_norm_input_check(
      df1 = df1_bridge_v3,
      df2 = df2_bridge_v3,
      overlapping_samples_df1 = ref_norm_res$lst_sample$bridge_samples,
      overlapping_samples_df2 = NULL,
      df1_project_nr = "df1_norm",
      df2_project_nr = "df2_norm",
      reference_project = "df1_norm",
      reference_medians = NULL
    ) |>
      suppressMessages() |>
      suppressWarnings()

    # check that function returns formatted data
    expect_message(
      expect_message(
        object = expect_message(
          object = olink_normalization_format(
            df_norm = df_norm_bridge_v3,
            lst_check = check_lst_bridge_v3
          ),
          regexp = "1 Negative Control was removed from dataset: \"NEG_CTRL\""
        ),
        regexp = "1 Plate Control was removed from dataset: \"PLATE_CTRL\""
      ),
      regexp = "Please verify that no other samples were removed unintentional"
    )

    # assays not present in all datasets and external controls ----

    oid_only_in_ref_v4 <- c("OID01216", "OID01217")
    oid_only_in_nonref_v4 <- c("OID01218", "OID01219")

    df1_bridge_v4 <- ref_norm_res$lst_df$df1_norm |>
      dplyr::filter(
        !(.data[["OlinkID"]] %in% .env[["oid_only_in_nonref_v4"]])
      ) |>
      dplyr::mutate(
        SampleID = dplyr::case_match(
          .data[["SampleID"]],
          "A6" ~ "NEG_CTRL",
          "A38" ~ "PLATE_CTRL",
          .default = .data[["SampleID"]]
        )
      )
    df2_bridge_v4 <- ref_norm_res$lst_df$df2_norm |>
      dplyr::filter(
        !(.data[["OlinkID"]] %in% .env[["oid_only_in_ref_v4"]])
      ) |>
      dplyr::mutate(
        SampleID = dplyr::case_match(
          .data[["SampleID"]],
          "C66" ~ "NEG_CTRL",
          "D1" ~ "PLATE_CTRL",
          .default = .data[["SampleID"]]
        )
      )

    df_norm_bridge_v4 <- ref_norm_res$lst_norm$bridge_norm$norm |>
      dplyr::mutate(
        SampleID = dplyr::case_match(
          .data[["SampleID"]],
          "A6" ~ "NEG_CTRL",
          "A38" ~ "PLATE_CTRL",
          "C66" ~ "NEG_CTRL",
          "D1" ~ "PLATE_CTRL",
          .default = .data[["SampleID"]]
        )
      )

    # run input check for normalization
    check_lst_bridge_v4 <- olink_norm_input_check(
      df1 = df1_bridge_v4,
      df2 = df2_bridge_v4,
      overlapping_samples_df1 = ref_norm_res$lst_sample$bridge_samples,
      overlapping_samples_df2 = NULL,
      df1_project_nr = "df1_norm",
      df2_project_nr = "df2_norm",
      reference_project = "df1_norm",
      reference_medians = NULL
    ) |>
      suppressMessages() |>
      suppressWarnings()

    # check that function returns formatted data
    expect_message(
      object = expect_message(
        object = expect_message(
          object = expect_message(
            object = olink_normalization_format(
              df_norm = df_norm_bridge_v4,
              lst_check = check_lst_bridge_v4
            ),
            regexp = paste("4 non-overlapping assays are included in the",
                           "normalized dataset without adjustment")
          ),
          regexp = "1 Negative Control was removed from dataset: \"NEG_CTRL\""
        ),
        regexp = "1 Plate Control was removed from dataset: \"PLATE_CTRL\""
      ),
      regexp = "Please verify that no other samples were removed unintentional"
    )
  }
)

test_that(
  "olink_normalization_format - works - within-product subset norm",
  {
    skip_if_not(file.exists(test_path("data", "ref_results_norm.rds")))

    ref_norm_res <- get_example_data("ref_results_norm.rds")

    # all assays present in both datasets ----

    df1_subset_v1 <- ref_norm_res$lst_df$df1_norm
    df2_subset_v1 <- ref_norm_res$lst_df$df2_norm

    df_norm_subset_v1 <- ref_norm_res$lst_norm$subset_norm$norm

    # run input check for normalization
    check_lst_subset_v1 <- olink_norm_input_check(
      df1 = df1_subset_v1,
      df2 = df2_subset_v1,
      overlapping_samples_df1 = ref_norm_res$lst_sample$df1_subset,
      overlapping_samples_df2 = ref_norm_res$lst_sample$df2_subset,
      df1_project_nr = "df1_norm",
      df2_project_nr = "df2_norm",
      reference_project = "df1_norm",
      reference_medians = NULL
    ) |>
      suppressMessages() |>
      suppressWarnings()

    # check that function returns formatted data
    expect_no_condition(
      object = olink_normalization_format(
        df_norm = df_norm_subset_v1,
        lst_check = check_lst_subset_v1
      )
    )

    # assays not present in all datasets ----

    oid_only_in_ref_v2 <- c("OID01216", "OID01217")
    oid_only_in_nonref_v2 <- c("OID01218", "OID01219")

    df1_subset_v2 <- ref_norm_res$lst_df$df1_norm |>
      dplyr::filter(
        !(.data[["OlinkID"]] %in% .env[["oid_only_in_nonref_v2"]])
      )
    df2_subset_v2 <- ref_norm_res$lst_df$df2_norm |>
      dplyr::filter(
        !(.data[["OlinkID"]] %in% .env[["oid_only_in_ref_v2"]])
      )

    df_norm_subset_v2 <- ref_norm_res$lst_norm$subset_norm$norm

    # run input check for normalization
    check_lst_subset_v2 <- olink_norm_input_check(
      df1 = df1_subset_v2,
      df2 = df2_subset_v2,
      overlapping_samples_df1 = ref_norm_res$lst_sample$df1_subset,
      overlapping_samples_df2 = ref_norm_res$lst_sample$df2_subset,
      df1_project_nr = "df1_norm",
      df2_project_nr = "df2_norm",
      reference_project = "df1_norm",
      reference_medians = NULL
    ) |>
      suppressMessages() |>
      suppressWarnings()

    # check that function returns formatted data
    expect_message(
      object = olink_normalization_format(
        df_norm = df_norm_subset_v2,
        lst_check = check_lst_subset_v2
      ),
      regexp = paste("4 non-overlapping assays are included in the",
                     "normalized dataset without adjustment")
    )

    # external controls in datasets ----

    df1_subset_v3 <- ref_norm_res$lst_df$df1_norm |>
      dplyr::mutate(
        SampleID = dplyr::case_match(
          .data[["SampleID"]],
          "A6" ~ "NEG_CTRL",
          "A38" ~ "PLATE_CTRL",
          .default = .data[["SampleID"]]
        )
      )
    df2_subset_v3 <- ref_norm_res$lst_df$df2_norm |>
      dplyr::mutate(
        SampleID = dplyr::case_match(
          .data[["SampleID"]],
          "C66" ~ "NEG_CTRL",
          "D1" ~ "PLATE_CTRL",
          .default = .data[["SampleID"]]
        )
      )

    df_norm_subset_v3 <- ref_norm_res$lst_norm$subset_norm$norm |>
      dplyr::mutate(
        SampleID = dplyr::case_match(
          .data[["SampleID"]],
          "A6" ~ "NEG_CTRL",
          "A38" ~ "PLATE_CTRL",
          "C66" ~ "NEG_CTRL",
          "D1" ~ "PLATE_CTRL",
          .default = .data[["SampleID"]]
        )
      )

    # run input check for normalization
    check_lst_subset_v3 <- olink_norm_input_check(
      df1 = df1_subset_v3,
      df2 = df2_subset_v3,
      overlapping_samples_df1 = ref_norm_res$lst_sample$df1_subset,
      overlapping_samples_df2 = ref_norm_res$lst_sample$df2_subset,
      df1_project_nr = "df1_norm",
      df2_project_nr = "df2_norm",
      reference_project = "df1_norm",
      reference_medians = NULL
    ) |>
      suppressMessages() |>
      suppressWarnings()

    # check that function returns formatted data
    expect_message(
      expect_message(
        object = expect_message(
          object = olink_normalization_format(
            df_norm = df_norm_subset_v3,
            lst_check = check_lst_subset_v3
          ),
          regexp = "1 Negative Control was removed from dataset: \"NEG_CTRL\""
        ),
        regexp = "1 Plate Control was removed from dataset: \"PLATE_CTRL\""
      ),
      regexp = "Please verify that no other samples were removed unintentional"
    )

    # assays not present in all datasets and external controls ----

    oid_only_in_ref_v4 <- c("OID01216", "OID01217")
    oid_only_in_nonref_v4 <- c("OID01218", "OID01219")

    df1_subset_v4 <- ref_norm_res$lst_df$df1_norm |>
      dplyr::filter(
        !(.data[["OlinkID"]] %in% .env[["oid_only_in_nonref_v4"]])
      ) |>
      dplyr::mutate(
        SampleID = dplyr::case_match(
          .data[["SampleID"]],
          "A6" ~ "NEG_CTRL",
          "A38" ~ "PLATE_CTRL",
          .default = .data[["SampleID"]]
        )
      )
    df2_subset_v4 <- ref_norm_res$lst_df$df2_norm |>
      dplyr::filter(
        !(.data[["OlinkID"]] %in% .env[["oid_only_in_ref_v4"]])
      ) |>
      dplyr::mutate(
        SampleID = dplyr::case_match(
          .data[["SampleID"]],
          "C66" ~ "NEG_CTRL",
          "D1" ~ "PLATE_CTRL",
          .default = .data[["SampleID"]]
        )
      )

    df_norm_subset_v4 <- ref_norm_res$lst_norm$subset_norm$norm |>
      dplyr::mutate(
        SampleID = dplyr::case_match(
          .data[["SampleID"]],
          "A6" ~ "NEG_CTRL",
          "A38" ~ "PLATE_CTRL",
          "C66" ~ "NEG_CTRL",
          "D1" ~ "PLATE_CTRL",
          .default = .data[["SampleID"]]
        )
      )

    # run input check for normalization
    check_lst_subset_v4 <- olink_norm_input_check(
      df1 = df1_subset_v4,
      df2 = df2_subset_v4,
      overlapping_samples_df1 = ref_norm_res$lst_sample$df1_subset,
      overlapping_samples_df2 = ref_norm_res$lst_sample$df2_subset,
      df1_project_nr = "df1_norm",
      df2_project_nr = "df2_norm",
      reference_project = "df1_norm",
      reference_medians = NULL
    ) |>
      suppressMessages() |>
      suppressWarnings()

    # check that function returns formatted data
    expect_message(
      object = expect_message(
        object = expect_message(
          object = expect_message(
            object = olink_normalization_format(
              df_norm = df_norm_subset_v4,
              lst_check = check_lst_subset_v4
            ),
            regexp = paste("4 non-overlapping assays are included in the",
                           "normalized dataset without adjustment")
          ),
          regexp = "1 Negative Control was removed from dataset: \"NEG_CTRL\""
        ),
        regexp = "1 Plate Control was removed from dataset: \"PLATE_CTRL\""
      ),
      regexp = "Please verify that no other samples were removed unintentional"
    )
  }
)

test_that(
  "olink_normalization_format - works - within-product ref med norm",
  {
    skip_if_not(file.exists(test_path("data", "ref_results_norm.rds")))

    ref_norm_res <- get_example_data("ref_results_norm.rds")

    # all assays present in both datasets ----

    df1_refmed_v1 <- ref_norm_res$lst_df$df1_norm
    refmed_v1 <- ref_norm_res$lst_df$ref_med

    df_norm_refmed_v1 <- ref_norm_res$lst_norm$ref_med_norm$norm

    # run input check for normalization
    check_lst_refmed_v1 <- olink_norm_input_check(
      df1 = df1_refmed_v1,
      df2 = NULL,
      overlapping_samples_df1 = ref_norm_res$lst_sample$df1_subset,
      overlapping_samples_df2 = NULL,
      df1_project_nr = "df1_norm",
      df2_project_nr = NULL,
      reference_project = NULL,
      reference_medians = refmed_v1
    ) |>
      suppressMessages() |>
      suppressWarnings()

    # check that function returns formatted data
    expect_no_condition(
      object = olink_normalization_format(
        df_norm = df_norm_refmed_v1,
        lst_check = check_lst_refmed_v1
      )
    )

    # assays not present in all datasets ----

    oid_only_in_ref_v2 <- c("OID01216", "OID01217")
    oid_only_in_nonref_v2 <- c("OID01218", "OID01219")

    df1_refmed_v2 <- ref_norm_res$lst_df$df1_norm |>
      dplyr::filter(
        !(.data[["OlinkID"]] %in% .env[["oid_only_in_nonref_v2"]])
      )
    refmed_v2 <- ref_norm_res$lst_df$ref_med |>
      dplyr::filter(
        !(.data[["OlinkID"]] %in% .env[["oid_only_in_ref_v2"]])
      )

    df_norm_refmed_v2 <- ref_norm_res$lst_norm$ref_med_norm$norm

    # run input check for normalization
    check_lst_refmed_v2 <- olink_norm_input_check(
      df1 = df1_refmed_v2,
      df2 = NULL,
      overlapping_samples_df1 = ref_norm_res$lst_sample$df1_subset,
      overlapping_samples_df2 = NULL,
      df1_project_nr = "df1_norm",
      df2_project_nr = NULL,
      reference_project = NULL,
      reference_medians = refmed_v2
    ) |>
      suppressMessages() |>
      suppressWarnings()

    # check that function returns formatted data
    expect_message(
      object = olink_normalization_format(
        df_norm = df_norm_refmed_v2,
        lst_check = check_lst_refmed_v2
      ),
      regexp = paste("2 non-overlapping assays found in the dataset but not",
                     "in the reference medians")
    )

    # external controls in datasets ----

    df1_refmed_v3 <- ref_norm_res$lst_df$df1_norm |>
      dplyr::mutate(
        SampleID = dplyr::case_match(
          .data[["SampleID"]],
          "A6" ~ "NEG_CTRL",
          "A38" ~ "PLATE_CTRL",
          .default = .data[["SampleID"]]
        )
      )
    refmed_v3 <- ref_norm_res$lst_df$ref_med

    df_norm_refmed_v3 <- ref_norm_res$lst_norm$ref_med_norm$norm |>
      dplyr::mutate(
        SampleID = dplyr::case_match(
          .data[["SampleID"]],
          "A6" ~ "NEG_CTRL",
          "A38" ~ "PLATE_CTRL",
          .default = .data[["SampleID"]]
        )
      )

    # run input check for normalization
    check_lst_refmed_v3 <- olink_norm_input_check(
      df1 = df1_refmed_v3,
      df2 = NULL,
      overlapping_samples_df1 = ref_norm_res$lst_sample$df1_subset,
      overlapping_samples_df2 = NULL,
      df1_project_nr = "df1_norm",
      df2_project_nr = NULL,
      reference_project = NULL,
      reference_medians = refmed_v3
    ) |>
      suppressMessages() |>
      suppressWarnings()

    # check that function returns formatted data
    expect_message(
      expect_message(
        object = expect_message(
          object = olink_normalization_format(
            df_norm = df_norm_refmed_v3,
            lst_check = check_lst_refmed_v3
          ),
          regexp = "1 Negative Control was removed from dataset: \"NEG_CTRL\""
        ),
        regexp = "1 Plate Control was removed from dataset: \"PLATE_CTRL\""
      ),
      regexp = "Please verify that no other samples were removed unintentional"
    )

    # assays not present in all datasets and external controls ----

    oid_only_in_ref_v4 <- c("OID01216", "OID01217")
    oid_only_in_nonref_v4 <- c("OID01218", "OID01219")

    df1_refmed_v4 <- ref_norm_res$lst_df$df1_norm |>
      dplyr::filter(
        !(.data[["OlinkID"]] %in% .env[["oid_only_in_nonref_v4"]])
      ) |>
      dplyr::mutate(
        SampleID = dplyr::case_match(
          .data[["SampleID"]],
          "A6" ~ "NEG_CTRL",
          "A38" ~ "PLATE_CTRL",
          .default = .data[["SampleID"]]
        )
      )
    refmed_v4 <- ref_norm_res$lst_df$ref_med |>
      dplyr::filter(
        !(.data[["OlinkID"]] %in% .env[["oid_only_in_ref_v4"]])
      )

    df_norm_refmed_v4 <- ref_norm_res$lst_norm$ref_med_norm$norm |>
      dplyr::mutate(
        SampleID = dplyr::case_match(
          .data[["SampleID"]],
          "A6" ~ "NEG_CTRL",
          "A38" ~ "PLATE_CTRL",
          .default = .data[["SampleID"]]
        )
      )

    # run input check for normalization
    check_lst_refmed_v4 <- olink_norm_input_check(
      df1 = df1_refmed_v4,
      df2 = NULL,
      overlapping_samples_df1 = ref_norm_res$lst_sample$df1_subset,
      overlapping_samples_df2 = NULL,
      df1_project_nr = "df1_norm",
      df2_project_nr = NULL,
      reference_project = NULL,
      reference_medians = refmed_v4
    ) |>
      suppressMessages() |>
      suppressWarnings()

    # check that function returns formatted data
    expect_message(
      object = expect_message(
        object = expect_message(
          object = expect_message(
            object = olink_normalization_format(
              df_norm = df_norm_refmed_v4,
              lst_check = check_lst_refmed_v4
            ),
            regexp = paste("2 non-overlapping assays found in the dataset but",
                           "not in the reference medians")
          ),
          regexp = "1 Negative Control was removed from dataset: \"NEG_CTRL\""
        ),
        regexp = "1 Plate Control was removed from dataset: \"PLATE_CTRL\""
      ),
      regexp = "Please verify that no other samples were removed unintentional"
    )
  }
)

test_that(
  "olink_normalization_format - works - cross-product normalization",
  {
    skip_if_not(file.exists(test_path("data", "example_3k_data.rds")))
    skip_if_not(file.exists(test_path("data", "example_HT_data.rds")))

    data_3k <- get_example_data(filename = "example_3k_data.rds")
    data_ht <- get_example_data(filename = "example_HT_data.rds")

    # help fun ----

    get_non_shares_oid <- function(df1, df_suffix_p1, df2, df_suffix_p2) {

      oid_p1 <- paste("OlinkID", df_suffix_p1, sep = "_")
      oid_p2 <- paste("OlinkID", df_suffix_p2, sep = "_")

      non_overlap_oid <- eHT_e3072_mapping |>
        dplyr::select(
          dplyr::all_of(
            c(oid_p1, oid_p2)
          )
        ) |>
        dplyr::filter(
          .data[[oid_p1]] %in% df1$OlinkID
          | .data[[oid_p2]] %in% df2$OlinkID
        ) |>
        dplyr::mutate(
          in_p1 = .data[[oid_p1]] %in% df1$OlinkID,
          in_p2 = .data[[oid_p2]] %in% df2$OlinkID
        ) |>
        dplyr::filter(
          .data[["in_p1"]] == TRUE
          & .data[["in_p2"]] == FALSE
        ) |>
        dplyr::pull(
          .data[[oid_p1]]
        )
      return(non_overlap_oid)
    }

    # cross-product normalization - vanilla ----

    data_3k_v1 <- data_3k
    data_ht_v1 <- data_ht

    # not using get_non_shares_oid here because the originally non-overlapping
    # assays are synthetic, and not present in eHT_e3072_mapping.
    oid_only_in_ref_v1 <- setdiff(data_ht_v1$OlinkID,
                                  eHT_e3072_mapping$OlinkID_HT)
    oid_only_in_notref_v1 <- setdiff(data_3k_v1$OlinkID,
                                     eHT_e3072_mapping$OlinkID_E3072)

    bridge_samples_crossproduct_v1 <- intersect(
      x = unique(data_3k_v1$SampleID),
      y = unique(data_ht_v1$SampleID)
    ) |>
      (\(x) x[!grepl("CONTROL", x)])()

    lst_check_cross_product_v1 <- olink_norm_input_check(
      df1 = data_ht_v1,
      df2 = data_3k_v1,
      overlapping_samples_df1 = bridge_samples_crossproduct_v1,
      overlapping_samples_df2 = NULL,
      df1_project_nr = "HT",
      df2_project_nr = "3K",
      reference_project = "HT",
      reference_medians = NULL
    ) |>
      suppressMessages() |>
      suppressWarnings()

    data_ht_3k_norm_v1 <- olink_normalization(
      df1 = data_ht_v1,
      df2 = data_3k_v1,
      overlapping_samples_df1 = bridge_samples_crossproduct_v1,
      df1_project_nr = "HT",
      df2_project_nr = "3K",
      reference_project = "HT",
      format = FALSE
    ) |>
      suppressMessages() |>
      suppressWarnings()

    # check that function returns expected messages
    expect_message(
      object = expect_message(
        object = expect_message(
          object = expect_message(
            object = olink_normalization_format(
              df_norm = data_ht_3k_norm_v1,
              lst_check = lst_check_cross_product_v1
            ),
            regexp = paste("2 not bridgeable assays are included in the",
                           "bridged dataset without adjustment")
          ),
          regexp = paste("2 non-overlapping assays are included in the",
                         "normalized dataset without adjustment")
        ),
        regexp = paste("6 Negative Controls were removed from dataset")
      ),
      regexp = paste("10 Plate Controls were removed from dataset")
    )

    # cross-product normalization - remove additional assays from both ----

    drop_assays <- eHT_e3072_mapping |>
      dplyr::filter(
        .data[["OlinkID_HT"]] %in% data_ht$OlinkID
        & .data[["OlinkID_E3072"]] %in% data_3k$OlinkID
      ) |>
      dplyr::arrange(.data[["Assay"]]) |>
      dplyr::select(dplyr::all_of(c("OlinkID_HT", "OlinkID_E3072")))

    drop_ht_assays <- drop_assays$OlinkID_HT[1L:4L]
    drop_3k_assays <- drop_assays$OlinkID_E3072[5L:7L]

    data_3k_v2 <- data_3k |>
      dplyr::filter(
        !(.data[["OlinkID"]] %in% .env[["drop_3k_assays"]])
        & !(.data[["OlinkID"]] %in% .env[["oid_only_in_notref_v1"]])
      )
    data_ht_v2 <- data_ht |>
      dplyr::filter(
        !(.data[["OlinkID"]] %in% .env[["drop_ht_assays"]])
        & !(.data[["OlinkID"]] %in% .env[["oid_only_in_ref_v1"]])
      )

    oid_only_in_ref_v2 <- get_non_shares_oid(
      df1 = data_ht_v2,
      df_suffix_p1 = "HT",
      df2 = data_3k_v2,
      df_suffix_p2 = "E3072"
    )
    oid_only_in_notref_v2 <- get_non_shares_oid(
      df1 = data_3k_v2,
      df_suffix_p1 = "E3072",
      df2 = data_ht_v2,
      df_suffix_p2 = "HT"
    )

    bridge_samples_crossproduct_v2 <- intersect(
      x = unique(data_3k_v2$SampleID),
      y = unique(data_ht_v2$SampleID)
    ) |>
      (\(x) x[!grepl("CONTROL", x)])()

    lst_check_cross_product_v2 <- olink_norm_input_check(
      df1 = data_ht_v2,
      df2 = data_3k_v2,
      overlapping_samples_df1 = bridge_samples_crossproduct_v2,
      overlapping_samples_df2 = NULL,
      df1_project_nr = "HT",
      df2_project_nr = "3K",
      reference_project = "HT",
      reference_medians = NULL
    ) |>
      suppressMessages() |>
      suppressWarnings()

    data_ht_3k_norm_v2 <- olink_normalization(
      df1 = data_ht_v2,
      df2 = data_3k_v2,
      overlapping_samples_df1 = bridge_samples_crossproduct_v2,
      df1_project_nr = "HT",
      df2_project_nr = "3K",
      reference_project = "HT",
      format = FALSE
    ) |>
      suppressMessages() |>
      suppressWarnings()

    expect_message(
      object = expect_message(
        object = expect_message(
          object = expect_message(
            object = olink_normalization_format(
              df_norm = data_ht_3k_norm_v2,
              lst_check = lst_check_cross_product_v2
            ),
            regexp = paste("2 not bridgeable assays are included in the",
                           "bridged dataset without adjustment")
          ),
          regexp = paste("7 non-overlapping assays are included in the",
                         "normalized dataset without adjustment")
        ),
        regexp = paste("6 Negative Controls were removed from dataset")
      ),
      regexp = paste("10 Plate Controls were removed from dataset")
    )

    # cross-product normalization - remove assays from reference ----

    data_3k_v3 <- data_3k |>
      dplyr::filter(
        !(.data[["OlinkID"]] %in% .env[["oid_only_in_notref_v1"]])
      )
    data_ht_v3 <- data_ht |>
      dplyr::filter(
        !(.data[["OlinkID"]] %in% .env[["drop_ht_assays"]])
        & !(.data[["OlinkID"]] %in% .env[["oid_only_in_ref_v1"]])
      )

    oid_only_in_ref_v3 <- get_non_shares_oid(
      df1 = data_ht_v3,
      df_suffix_p1 = "HT",
      df2 = data_3k_v3,
      df_suffix_p2 = "E3072"
    )
    oid_only_in_notref_v3 <- get_non_shares_oid(
      df1 = data_3k_v3,
      df_suffix_p1 = "E3072",
      df2 = data_ht_v3,
      df_suffix_p2 = "HT"
    )

    bridge_samples_crossproduct_v3 <- intersect(
      x = unique(data_3k_v3$SampleID),
      y = unique(data_ht_v3$SampleID)
    ) |>
      (\(x) x[!grepl("CONTROL", x)])()

    lst_check_cross_product_v3 <- olink_norm_input_check(
      df1 = data_ht_v3,
      df2 = data_3k_v3,
      overlapping_samples_df1 = bridge_samples_crossproduct_v3,
      overlapping_samples_df2 = NULL,
      df1_project_nr = "HT",
      df2_project_nr = "3K",
      reference_project = "HT",
      reference_medians = NULL
    ) |>
      suppressMessages() |>
      suppressWarnings()

    data_ht_3k_norm_v3 <- olink_normalization(
      df1 = data_ht_v3,
      df2 = data_3k_v3,
      overlapping_samples_df1 = bridge_samples_crossproduct_v3,
      df1_project_nr = "HT",
      df2_project_nr = "3K",
      reference_project = "HT",
      format = FALSE
    ) |>
      suppressMessages() |>
      suppressWarnings()

    expect_message(
      object = expect_message(
        object = expect_message(
          object = expect_message(
            object = olink_normalization_format(
              df_norm = data_ht_3k_norm_v3,
              lst_check = lst_check_cross_product_v3
            ),
            regexp = paste("2 not bridgeable assays are included in the",
                           "bridged dataset without adjustment")
          ),
          regexp = paste("4 non-overlapping assays are included in the",
                         "normalized dataset without adjustment")
        ),
        regexp = paste("6 Negative Controls were removed from dataset")
      ),
      regexp = paste("10 Plate Controls were removed from dataset")
    )

    # cross-product normalization - remove assays from non reference ----

    data_3k_v4 <- data_3k |>
      dplyr::filter(
        !(.data[["OlinkID"]] %in% .env[["drop_3k_assays"]])
        & !(.data[["OlinkID"]] %in% .env[["oid_only_in_notref_v1"]])
      )
    data_ht_v4 <- data_ht |>
      dplyr::filter(
        !(.data[["OlinkID"]] %in% .env[["oid_only_in_ref_v1"]])
      )

    oid_only_in_ref_v4 <- get_non_shares_oid(
      df1 = data_ht_v4,
      df_suffix_p1 = "HT",
      df2 = data_3k_v4,
      df_suffix_p2 = "E3072"
    )
    oid_only_in_notref_v4 <- get_non_shares_oid(
      df1 = data_3k_v4,
      df_suffix_p1 = "E3072",
      df2 = data_ht_v4,
      df_suffix_p2 = "HT"
    )

    bridge_samples_crossproduct_v4 <- intersect(
      x = unique(data_3k_v4$SampleID),
      y = unique(data_ht_v4$SampleID)
    ) |>
      (\(x) x[!grepl("CONTROL", x)])()

    lst_check_cross_product_v4 <- olink_norm_input_check(
      df1 = data_ht_v4,
      df2 = data_3k_v4,
      overlapping_samples_df1 = bridge_samples_crossproduct_v4,
      overlapping_samples_df2 = NULL,
      df1_project_nr = "HT",
      df2_project_nr = "3K",
      reference_project = "HT",
      reference_medians = NULL
    ) |>
      suppressMessages() |>
      suppressWarnings()

    data_ht_3k_norm_v4 <- olink_normalization(
      df1 = data_ht_v4,
      df2 = data_3k_v4,
      overlapping_samples_df1 = bridge_samples_crossproduct_v4,
      df1_project_nr = "HT",
      df2_project_nr = "3K",
      reference_project = "HT",
      format = FALSE
    ) |>
      suppressMessages() |>
      suppressWarnings()

    expect_message(
      object = expect_message(
        object = expect_message(
          object = expect_message(
            object = olink_normalization_format(
              df_norm = data_ht_3k_norm_v4,
              lst_check = lst_check_cross_product_v4
            ),
            regexp = paste("2 not bridgeable assays are included in the",
                           "bridged dataset without adjustment")
          ),
          regexp = paste("3 non-overlapping assays are included in the",
                         "normalized dataset without adjustment")
        ),
        regexp = paste("6 Negative Controls were removed from dataset")
      ),
      regexp = paste("10 Plate Controls were removed from dataset")
    )
  }
)

# Test olink_format_rm_ext_ctrl ----

test_that(
  "olink_format_rm_ext_ctrl - works - no external controls",
  {
    skip_if_not(file.exists(test_path("data", "ref_results_norm.rds")))

    ref_norm_res <- get_example_data("ref_results_norm.rds")

    ### bridge normalization - no norm column ----

    bridge_nonorm_noctrl_check <- olink_norm_input_check(
      df1 = ref_norm_res$lst_df$df1_no_norm,
      df2 = ref_norm_res$lst_df$df2_no_norm,
      overlapping_samples_df1 = ref_norm_res$lst_sample$bridge_samples,
      overlapping_samples_df2 = NULL,
      df1_project_nr = "df1_no_norm",
      df2_project_nr = "df2_no_norm",
      reference_project = "df1_no_norm",
      reference_medians = NULL
    ) |>
      suppressMessages() |>
      suppressWarnings()

    expect_no_condition(
      object = olink_format_rm_ext_ctrl(
        df = ref_norm_res$lst_norm$bridge_norm$no_norm,
        lst_check = bridge_nonorm_noctrl_check
      )
    )

    ### bridge normalization - with norm column ----

    bridge_norm_noctrl_check <- olink_norm_input_check(
      df1 = ref_norm_res$lst_df$df1_norm,
      df2 = ref_norm_res$lst_df$df2_norm,
      overlapping_samples_df1 = ref_norm_res$lst_sample$bridge_samples,
      overlapping_samples_df2 = NULL,
      df1_project_nr = "df1_norm",
      df2_project_nr = "df2_norm",
      reference_project = "df1_norm",
      reference_medians = NULL
    ) |>
      suppressMessages() |>
      suppressWarnings()

    expect_no_condition(
      object = olink_format_rm_ext_ctrl(
        df = ref_norm_res$lst_norm$bridge_norm$norm,
        lst_check = bridge_norm_noctrl_check
      )
    )

    ### bridge normalization - no lod column ----

    bridge_nolod_noctrl_check <- olink_norm_input_check(
      df1 = ref_norm_res$lst_df$df1_no_lod,
      df2 = ref_norm_res$lst_df$df2_no_lod,
      overlapping_samples_df1 = ref_norm_res$lst_sample$bridge_samples,
      overlapping_samples_df2 = NULL,
      df1_project_nr = "df1_nolod",
      df2_project_nr = "df2_nolod",
      reference_project = "df1_nolod",
      reference_medians = NULL
    ) |>
      suppressMessages() |>
      suppressWarnings()

    expect_no_condition(
      object = olink_format_rm_ext_ctrl(
        df = ref_norm_res$lst_norm$bridge_norm$no_lod,
        lst_check = bridge_nolod_noctrl_check
      )
    )

    ### bridge normalization - multiple lod columns ----

    bridge_multilod_noctrl_check <- olink_norm_input_check(
      df1 = ref_norm_res$lst_df$df1_multiple_lod,
      df2 = ref_norm_res$lst_df$df2_multiple_lod,
      overlapping_samples_df1 = ref_norm_res$lst_sample$bridge_samples,
      overlapping_samples_df2 = NULL,
      df1_project_nr = "df1_multiple_lod",
      df2_project_nr = "df2_multiple_lod",
      reference_project = "df1_multiple_lod",
      reference_medians = NULL
    ) |>
      suppressMessages() |>
      suppressWarnings()

    expect_no_condition(
      object = olink_format_rm_ext_ctrl(
        df = ref_norm_res$lst_norm$bridge_norm$multiple_lod,
        lst_check = bridge_multilod_noctrl_check
      )
    )
  }
)

test_that(
  "olink_format_rm_ext_ctrl - works - external controls - no sample_type",
  {
    skip_if_not(file.exists(test_path("data", "ref_results_norm.rds")))

    ref_norm_res <- get_example_data("ref_results_norm.rds")

    bridge_norm_noctrl_check <- olink_norm_input_check(
      df1 = ref_norm_res$lst_df$df1_norm,
      df2 = ref_norm_res$lst_df$df2_norm,
      overlapping_samples_df1 = ref_norm_res$lst_sample$bridge_samples,
      overlapping_samples_df2 = NULL,
      df1_project_nr = "df1_norm",
      df2_project_nr = "df2_norm",
      reference_project = "df1_norm",
      reference_medians = NULL
    ) |>
      suppressMessages() |>
      suppressWarnings()

    # NC samples ----

    # Add NCs to remove
    bridge_norm_wctrl_nc <- ref_norm_res$lst_norm$bridge_norm$no_norm |>
      dplyr::mutate(
        SampleID = dplyr::case_match(
          .data[["SampleID"]],
          "A6" ~ "NEG_CTRL",
          "A38" ~ "NEGATIVE_CONTROL",
          "A43" ~ "Neg_Ctrl",
          "D1" ~ "NEG_CTRL_2",
          "D52" ~ "NEGATIVE_CONTROL_2",
          "D75" ~ "Neg_Ctrl_2",
          .default = .data[["SampleID"]]
        )
      )

    expect_message(
      object = expect_message(
        object = olink_format_rm_ext_ctrl(
          df = bridge_norm_wctrl_nc,
          lst_check = bridge_norm_noctrl_check
        ),
        regexp = paste("6 Negative Controls were removed from dataset:",
                       "\"NEG_CTRL\", \"NEGATIVE_CONTROL\", \"Neg_Ctrl\",",
                       "\"NEG_CTRL_2\", \"NEGATIVE_CONTROL_2\", and",
                       "\"Neg_Ctrl_2\"")
      ),
      regexp = "Please verify that no other samples were removed unintentional"
    )

    # PC samples ----

    # Add PCs to remove
    bridge_norm_wctrl_pc <- ref_norm_res$lst_norm$bridge_norm$no_norm |>
      dplyr::mutate(
        SampleID = dplyr::case_match(
          .data[["SampleID"]],
          "A6" ~ "PLATE",
          "A38" ~ "IPC",
          "A43" ~ "Plate",
          "D1" ~ "plate",
          "D52" ~ "plate_control",
          "D75" ~ "Plate_Control",
          .default = .data[["SampleID"]]
        )
      )

    expect_message(
      object = expect_message(
        object = olink_format_rm_ext_ctrl(
          df = bridge_norm_wctrl_pc,
          lst_check = bridge_norm_noctrl_check
        ),
        regexp = paste("6 Plate Controls were removed from dataset: \"PLATE\",",
                       "\"IPC\", \"Plate\", \"plate\", \"plate_control\", and",
                       "\"Plate_Control\"")
      ),
      regexp = "Please verify that no other samples were removed unintentional"
    )

    # NC and PC samples ----

    bridge_norm_wctrl_ncpc <- ref_norm_res$lst_norm$bridge_norm$no_norm |>
      dplyr::mutate(
        SampleID = dplyr::case_match(
          .data[["SampleID"]],
          "A6" ~ "NEG_CTRL",
          "A38" ~ "IPC",
          "A43" ~ "Neg_Ctrl",
          "D1" ~ "plate",
          "D52" ~ "NEGATIVE_CONTROL_2",
          "D75" ~ "Plate_Control",
          .default = .data[["SampleID"]]
        )
      )

    expect_message(
      object = expect_message(
        object = expect_message(
          object = olink_format_rm_ext_ctrl(
            df = bridge_norm_wctrl_ncpc,
            lst_check = bridge_norm_noctrl_check
          ),
          regexp = paste("3 Negative Controls were removed from dataset:",
                         "\"NEG_CTRL\", \"Neg_Ctrl\", and",
                         "\"NEGATIVE_CONTROL_2\"")
        ),
        regexp = paste("3 Plate Controls were removed from dataset: \"IPC\",",
                       "\"plate\", and \"Plate_Control\"")
      ),
      regexp = "Please verify that no other samples were removed unintentional"
    )
  }
)

test_that(
  "olink_format_rm_ext_ctrl - works - external controls - with sample_type",
  {
    skip_if_not(file.exists(test_path("data", "example_3k_data.rds")))
    skip_if_not(file.exists(test_path("data", "example_HT_data.rds")))

    data_3k <- get_example_data(filename = "example_3k_data.rds")
    data_ht <- get_example_data(filename = "example_HT_data.rds")

    bridge_samples_3k_ht <- intersect(
      x = unique(data_3k$SampleID),
      y = unique(data_ht$SampleID)
    ) |>
      (\(x) x[!grepl("CONTROL", x)])()

    # both datasets have SampleType ----

    bridge_norm_noctrl_check_v1 <- olink_norm_input_check(
      df1 = data_ht,
      df2 = data_3k,
      overlapping_samples_df1 = bridge_samples_3k_ht,
      overlapping_samples_df2 = NULL,
      df1_project_nr = "df1_norm",
      df2_project_nr = "df2_norm",
      reference_project = "df1_norm",
      reference_medians = NULL
    ) |>
      suppressMessages() |>
      suppressWarnings()

    df_3k_ht_format_v1 <- data_3k |>
      dplyr::bind_rows(data_ht)

    expect_message(
      object = expect_message(
        object = olink_format_rm_ext_ctrl(
          df = df_3k_ht_format_v1,
          lst_check = bridge_norm_noctrl_check_v1
        ),
        regexp = paste("6 Negative Controls were removed from dataset:")
      ),
      regexp = paste("10 Plate Controls were removed from dataset:")
    )

    # one dataset has SampleType and the other has Sample_Type ----

    bridge_norm_noctrl_check_v2 <- olink_norm_input_check(
      df1 = data_ht,
      df2 = data_3k |>
        dplyr::rename(
          "Sample_Type" = "SampleType"
        ),
      overlapping_samples_df1 = bridge_samples_3k_ht,
      overlapping_samples_df2 = NULL,
      df1_project_nr = "df1_norm",
      df2_project_nr = "df2_norm",
      reference_project = "df1_norm",
      reference_medians = NULL
    ) |>
      suppressMessages() |>
      suppressWarnings()

    df_3k_ht_format_v2 <- data_3k |>
      dplyr::rename(
        "Sample_Type" = "SampleType"
      ) |>
      dplyr::bind_rows(data_ht)

    expect_message(
      object = expect_message(
        object = olink_format_rm_ext_ctrl(
          df = df_3k_ht_format_v2,
          lst_check = bridge_norm_noctrl_check_v2
        ),
        regexp = paste("6 Negative Controls were removed from dataset:")
      ),
      regexp = paste("10 Plate Controls were removed from dataset:")
    )

    # one dataset has SampleType and the other has no sample type col ----

    bridge_norm_noctrl_check_v3 <- olink_norm_input_check(
      df1 = data_ht,
      df2 = data_3k |>
        dplyr::select(
          -dplyr::all_of("SampleType")
        ),
      overlapping_samples_df1 = bridge_samples_3k_ht,
      overlapping_samples_df2 = NULL,
      df1_project_nr = "df1_norm",
      df2_project_nr = "df2_norm",
      reference_project = "df1_norm",
      reference_medians = NULL
    ) |>
      suppressMessages() |>
      suppressWarnings()

    df_3k_ht_format_v3 <- data_3k |>
      dplyr::select(
        -dplyr::all_of("SampleType")
      ) |>
      dplyr::bind_rows(data_ht)

    expect_message(
      object = expect_message(
        object = expect_message(
          object = olink_format_rm_ext_ctrl(
            df = df_3k_ht_format_v3,
            lst_check = bridge_norm_noctrl_check_v3
          ),
          regexp = paste("6 Negative Controls were removed from dataset:")
        ),
        regexp = paste("10 Plate Controls were removed from dataset:")
      ),
      regexp = "Please verify that no other samples were removed unintentional"
    )
  }
)

# Test olink_format_oid_no_overlap ----

test_that(
  "olink_format_oid_no_overlap - works - reference median normalization",
  {
    skip_if_not(file.exists(test_path("data", "ref_results_norm.rds")))

    ref_norm_res <- get_example_data("ref_results_norm.rds")

    oid_only_in_ref <- c("OID01216", "OID01217")

    # df1 contains assays not in reference medians
    df1_ref_med <- ref_norm_res$lst_df$df1_norm

    # remove the assays from the reference medians for testing
    df_ref_med <- ref_norm_res$lst_df$ref_med |>
      dplyr::filter(
        !(.data[["OlinkID"]] %in% .env[["oid_only_in_ref"]])
      )

    # run input check for normalization
    ref_med_check_lst <- olink_norm_input_check(
      df1 = df1_ref_med,
      df2 = NULL,
      overlapping_samples_df1 = ref_norm_res$lst_sample$df1_subset,
      overlapping_samples_df2 = NULL,
      df1_project_nr = "df1_norm",
      reference_medians = df_ref_med
    ) |>
      suppressMessages() |>
      suppressWarnings()

    # check that function returns missing OlinkIDs with adjustment factor 0
    expect_message(
      object = df_ref_med_miss_oid <- olink_format_oid_no_overlap(
        lst_check = ref_med_check_lst
      ),
      regexp = paste("2 non-overlapping assays found in the dataset but not in",
                     "the reference medians. Assays are included in the",
                     "normalized dataset without adjustment.")
    )

    expect_equal(
      object = df_ref_med_miss_oid |>
        dplyr::arrange(
          .data[["SampleID"]], .data[["OlinkID"]]
        ),
      expected = df1_ref_med |>
        dplyr::filter(
          .data[["OlinkID"]] %in% .env[["oid_only_in_ref"]]
        ) |>
        dplyr::mutate(
          Project = "df1_norm",
          Adj_factor = 0
        ) |>
        dplyr::arrange(
          .data[["SampleID"]], .data[["OlinkID"]]
        )
    )
  }
)

test_that(
  "olink_format_oid_no_overlap - works - bridge/subset normalization",
  {
    skip_if_not(file.exists(test_path("data", "ref_results_norm.rds")))

    ref_norm_res <- get_example_data("ref_results_norm.rds")

    # assays present in reference dataset only ----

    oid_only_in_ref_v1 <- c("OID01216", "OID01217")

    # df1 contains assays not in df2
    df1_subset_v1 <- ref_norm_res$lst_df$df1_norm

    df2_subset_v1 <- ref_norm_res$lst_df$df2_norm |>
      dplyr::filter(
        !(.data[["OlinkID"]] %in% .env[["oid_only_in_ref_v1"]])
      )

    # run input check for normalization
    subset_check_lst_v1 <- olink_norm_input_check(
      df1 = df1_subset_v1,
      df2 = df2_subset_v1,
      overlapping_samples_df1 = ref_norm_res$lst_sample$df1_subset,
      overlapping_samples_df2 = ref_norm_res$lst_sample$df2_subset,
      df1_project_nr = "df1_norm",
      df2_project_nr = "df2_norm",
      reference_project = "df1_norm",
      reference_medians = NULL
    ) |>
      suppressMessages() |>
      suppressWarnings()

    # check that function returns missing OlinkIDs with adjustment factor 0
    expect_message(
      object = df_subset_miss_oid_v1 <- olink_format_oid_no_overlap(
        lst_check = subset_check_lst_v1
      ),
      regexp = paste("2 non-overlapping assays are included in the normalized",
                     "dataset without adjustment. Assays found in only one",
                     "project will have decreased statistical power due to the",
                     "lower number of samples.")
    )

    expect_equal(
      object = df_subset_miss_oid_v1 |>
        dplyr::arrange(
          .data[["SampleID"]], .data[["OlinkID"]]
        ),
      expected = df1_subset_v1 |>
        dplyr::filter(
          .data[["OlinkID"]] %in% .env[["oid_only_in_ref_v1"]]
        ) |>
        dplyr::mutate(
          Project = "df1_norm",
          Adj_factor = 0
        ) |>
        dplyr::arrange(
          .data[["SampleID"]], .data[["OlinkID"]]
        )
    )

    # assays present in the non-reference dataset only ----

    oid_only_in_ref_v2 <- c("OID00561", "OID00562", "OID01213", "OID05124")

    df1_subset_v2 <- ref_norm_res$lst_df$df1_norm |>
      dplyr::filter(
        !(.data[["OlinkID"]] %in% .env[["oid_only_in_ref_v2"]])
      )

    df2_subset_v2 <- ref_norm_res$lst_df$df2_norm

    # run input check for normalization
    subset_check_lst_v2 <- olink_norm_input_check(
      df1 = df1_subset_v2,
      df2 = df2_subset_v2,
      overlapping_samples_df1 = ref_norm_res$lst_sample$df1_subset,
      overlapping_samples_df2 = ref_norm_res$lst_sample$df2_subset,
      df1_project_nr = "df1_norm",
      df2_project_nr = "df2_norm",
      reference_project = "df1_norm",
      reference_medians = NULL
    ) |>
      suppressMessages() |>
      suppressWarnings()

    # check that function returns missing OlinkIDs with adjustment factor 0
    expect_message(
      object = df_subset_miss_oid_v2 <- olink_format_oid_no_overlap(
        lst_check = subset_check_lst_v2
      ),
      regexp = paste("4 non-overlapping assays are included in the normalized",
                     "dataset without adjustment. Assays found in only one",
                     "project will have decreased statistical power due to the",
                     "lower number of samples.")
    )

    expect_equal(
      object = df_subset_miss_oid_v2 |>
        dplyr::arrange(
          .data[["SampleID"]], .data[["OlinkID"]]
        ),
      expected = df2_subset_v2 |>
        dplyr::filter(
          .data[["OlinkID"]] %in% .env[["oid_only_in_ref_v2"]]
        ) |>
        dplyr::mutate(
          Project = "df2_norm",
          Adj_factor = 0
        ) |>
        dplyr::arrange(
          .data[["SampleID"]], .data[["OlinkID"]]
        )
    )

    # different assays present in either dataset ----

    # run input check for normalization
    subset_check_lst_v3 <- olink_norm_input_check(
      df1 = df1_subset_v2,
      df2 = df2_subset_v1,
      overlapping_samples_df1 = ref_norm_res$lst_sample$df1_subset,
      overlapping_samples_df2 = ref_norm_res$lst_sample$df2_subset,
      df1_project_nr = "df1_norm",
      df2_project_nr = "df2_norm",
      reference_project = "df1_norm",
      reference_medians = NULL
    ) |>
      suppressMessages() |>
      suppressWarnings()

    # check that function returns missing OlinkIDs with adjustment factor 0
    expect_message(
      object = df_subset_miss_oid_v3 <- olink_format_oid_no_overlap(
        lst_check = subset_check_lst_v3
      ),
      regexp = paste("6 non-overlapping assays are included in the normalized",
                     "dataset without adjustment. Assays found in only one",
                     "project will have decreased statistical power due to the",
                     "lower number of samples.")
    )

    expect_equal(
      object = df_subset_miss_oid_v3 |>
        dplyr::arrange(
          .data[["SampleID"]], .data[["OlinkID"]]
        ),
      expected = df1_subset_v1 |>
        dplyr::filter(
          .data[["OlinkID"]] %in% .env[["oid_only_in_ref_v1"]]
        ) |>
        dplyr::mutate(
          Project = "df1_norm"
        ) |>
        dplyr::bind_rows(
          df2_subset_v2 |>
            dplyr::filter(
              .data[["OlinkID"]] %in% .env[["oid_only_in_ref_v2"]]
            ) |>
            dplyr::mutate(
              Project = "df2_norm"
            )
        ) |>
        dplyr::mutate(
          Adj_factor = 0
        ) |>
        dplyr::arrange(
          .data[["SampleID"]], .data[["OlinkID"]]
        )
    )
  }
)

test_that(
  "olink_format_oid_no_overlap - works - cross-product normalization",
  {
    skip_if_not(file.exists(test_path("data", "example_3k_data.rds")))
    skip_if_not(file.exists(test_path("data", "example_HT_data.rds")))

    data_3k <- get_example_data(filename = "example_3k_data.rds")
    data_ht <- get_example_data(filename = "example_HT_data.rds")

    # help fun ----

    get_non_shares_oid <- function(df1, df_suffix_p1, df2, df_suffix_p2) {

      oid_p1 <- paste("OlinkID", df_suffix_p1, sep = "_")
      oid_p2 <- paste("OlinkID", df_suffix_p2, sep = "_")

      non_overlap_oid <- eHT_e3072_mapping |>
        dplyr::select(
          dplyr::all_of(
            c(oid_p1, oid_p2)
          )
        ) |>
        dplyr::filter(
          .data[[oid_p1]] %in% df1$OlinkID
          | .data[[oid_p2]] %in% df2$OlinkID
        ) |>
        dplyr::mutate(
          in_p1 = .data[[oid_p1]] %in% df1$OlinkID,
          in_p2 = .data[[oid_p2]] %in% df2$OlinkID
        ) |>
        dplyr::filter(
          .data[["in_p1"]] == TRUE
          & .data[["in_p2"]] == FALSE
        ) |>
        dplyr::pull(
          .data[[oid_p1]]
        )
      return(non_overlap_oid)
    }

    # cross-product normalization - vanilla ----

    data_3k_v1 <- data_3k
    data_ht_v1 <- data_ht

    # not using get_non_shares_oid here because the originally non-overlapping
    # assays are synthetic, and not present in eHT_e3072_mapping.
    oid_only_in_ref_v1 <- setdiff(data_ht_v1$OlinkID,
                                  eHT_e3072_mapping$OlinkID_HT)
    oid_only_in_notref_v1 <- setdiff(data_3k_v1$OlinkID,
                                     eHT_e3072_mapping$OlinkID_E3072)

    bridge_samples_crossproduct_v1 <- intersect(
      x = unique(data_3k_v1$SampleID),
      y = unique(data_ht_v1$SampleID)
    ) |>
      (\(x) x[!grepl("CONTROL", x)])()

    lst_check_cross_product_v1 <- olink_norm_input_check(
      df1 = data_ht_v1,
      df2 = data_3k_v1,
      overlapping_samples_df1 = bridge_samples_crossproduct_v1,
      overlapping_samples_df2 = NULL,
      df1_project_nr = "HT",
      df2_project_nr = "3K",
      reference_project = "HT",
      reference_medians = NULL
    ) |>
      suppressMessages() |>
      suppressWarnings()

    # check that function returns missing OlinkIDs with BridgingRecommendation
    expect_message(
      object = df_cross_product_miss_oid_v1 <- olink_format_oid_no_overlap(
        lst_check = lst_check_cross_product_v1
      ),
      regexp = paste("2 non-overlapping assays are included in the normalized",
                     "dataset without adjustment. Assays found in only one",
                     "project will have decreased statistical power due to the",
                     "lower number of samples.")
    )

    expect_equal(
      object = df_cross_product_miss_oid_v1 |>
        dplyr::arrange(
          .data[["SampleID"]], .data[["OlinkID"]]
        ),
      expected = data_ht_v1 |>
        dplyr::filter(
          .data[["OlinkID"]] %in% .env[["oid_only_in_ref_v1"]]
        ) |>
        dplyr::mutate(
          Project = "HT"
        ) |>
        dplyr::bind_rows(
          data_3k_v1 |>
            dplyr::filter(
              .data[["OlinkID"]] %in% .env[["oid_only_in_notref_v1"]]
            ) |>
            dplyr::mutate(
              Project = "3K"
            )
        ) |>
        dplyr::mutate(
          BridgingRecommendation = "NotOverlapping"
        ) |>
        dplyr::arrange(
          .data[["SampleID"]], .data[["OlinkID"]]
        )
    )

    # cross-product normalization - remove additional assays from both ----

    drop_assays <- eHT_e3072_mapping |>
      dplyr::filter(
        .data[["OlinkID_HT"]] %in% data_ht$OlinkID
        & .data[["OlinkID_E3072"]] %in% data_3k$OlinkID
      ) |>
      dplyr::arrange(.data[["Assay"]]) |>
      dplyr::select(dplyr::all_of(c("OlinkID_HT", "OlinkID_E3072")))

    drop_ht_assays <- drop_assays$OlinkID_HT[1L:4L]
    drop_3k_assays <- drop_assays$OlinkID_E3072[5L:7L]

    data_3k_v2 <- data_3k |>
      dplyr::filter(
        !(.data[["OlinkID"]] %in% .env[["drop_3k_assays"]])
        & !(.data[["OlinkID"]] %in% .env[["oid_only_in_notref_v1"]])
      )
    data_ht_v2 <- data_ht |>
      dplyr::filter(
        !(.data[["OlinkID"]] %in% .env[["drop_ht_assays"]])
        & !(.data[["OlinkID"]] %in% .env[["oid_only_in_ref_v1"]])
      )

    oid_only_in_ref_v2 <- get_non_shares_oid(
      df1 = data_ht_v2,
      df_suffix_p1 = "HT",
      df2 = data_3k_v2,
      df_suffix_p2 = "E3072"
    )
    oid_only_in_notref_v2 <- get_non_shares_oid(
      df1 = data_3k_v2,
      df_suffix_p1 = "E3072",
      df2 = data_ht_v2,
      df_suffix_p2 = "HT"
    )

    bridge_samples_crossproduct_v2 <- intersect(
      x = unique(data_3k_v2$SampleID),
      y = unique(data_ht_v2$SampleID)
    ) |>
      (\(x) x[!grepl("CONTROL", x)])()

    lst_check_cross_product_v2 <- olink_norm_input_check(
      df1 = data_ht_v2,
      df2 = data_3k_v2,
      overlapping_samples_df1 = bridge_samples_crossproduct_v2,
      overlapping_samples_df2 = NULL,
      df1_project_nr = "HT",
      df2_project_nr = "3K",
      reference_project = "HT",
      reference_medians = NULL
    ) |>
      suppressMessages() |>
      suppressWarnings()

    # check that function returns missing OlinkIDs with BridgingRecommendation
    expect_message(
      object = df_cross_product_miss_oid_v2 <- olink_format_oid_no_overlap(
        lst_check = lst_check_cross_product_v2
      ),
      regexp = paste("7 non-overlapping assays are included in the normalized",
                     "dataset without adjustment. Assays found in only one",
                     "project will have decreased statistical power due to the",
                     "lower number of samples.")
    )

    expect_equal(
      object = df_cross_product_miss_oid_v2 |>
        dplyr::arrange(
          .data[["SampleID"]], .data[["OlinkID"]]
        ),
      expected = data_ht_v2 |>
        dplyr::filter(
          .data[["OlinkID"]] %in% .env[["oid_only_in_ref_v2"]]
        ) |>
        dplyr::mutate(
          Project = "HT"
        ) |>
        dplyr::bind_rows(
          data_3k_v2 |>
            dplyr::filter(
              .data[["OlinkID"]] %in% .env[["oid_only_in_notref_v2"]]
            ) |>
            dplyr::mutate(
              Project = "3K"
            )
        ) |>
        dplyr::mutate(
          BridgingRecommendation = "NotOverlapping"
        ) |>
        dplyr::arrange(
          .data[["SampleID"]], .data[["OlinkID"]]
        )
    )

    # cross-product normalization - remove assays from reference ----

    data_3k_v3 <- data_3k |>
      dplyr::filter(
        !(.data[["OlinkID"]] %in% .env[["oid_only_in_notref_v1"]])
      )
    data_ht_v3 <- data_ht |>
      dplyr::filter(
        !(.data[["OlinkID"]] %in% .env[["drop_ht_assays"]])
        & !(.data[["OlinkID"]] %in% .env[["oid_only_in_ref_v1"]])
      )

    oid_only_in_ref_v3 <- get_non_shares_oid(
      df1 = data_ht_v3,
      df_suffix_p1 = "HT",
      df2 = data_3k_v3,
      df_suffix_p2 = "E3072"
    )
    oid_only_in_notref_v3 <- get_non_shares_oid(
      df1 = data_3k_v3,
      df_suffix_p1 = "E3072",
      df2 = data_ht_v3,
      df_suffix_p2 = "HT"
    )

    bridge_samples_crossproduct_v3 <- intersect(
      x = unique(data_3k_v3$SampleID),
      y = unique(data_ht_v3$SampleID)
    ) |>
      (\(x) x[!grepl("CONTROL", x)])()

    lst_check_cross_product_v3 <- olink_norm_input_check(
      df1 = data_ht_v3,
      df2 = data_3k_v3,
      overlapping_samples_df1 = bridge_samples_crossproduct_v3,
      overlapping_samples_df2 = NULL,
      df1_project_nr = "HT",
      df2_project_nr = "3K",
      reference_project = "HT",
      reference_medians = NULL
    ) |>
      suppressMessages() |>
      suppressWarnings()

    # check that function returns missing OlinkIDs with BridgingRecommendation
    expect_message(
      object = df_cross_product_miss_oid_v3 <- olink_format_oid_no_overlap(
        lst_check = lst_check_cross_product_v3
      ),
      regexp = paste("4 non-overlapping assays are included in the normalized",
                     "dataset without adjustment. Assays found in only one",
                     "project will have decreased statistical power due to the",
                     "lower number of samples.")
    )

    expect_equal(
      object = df_cross_product_miss_oid_v3 |>
        dplyr::arrange(
          .data[["SampleID"]], .data[["OlinkID"]]
        ),
      expected = data_ht_v3 |>
        dplyr::filter(
          .data[["OlinkID"]] %in% .env[["oid_only_in_ref_v3"]]
        ) |>
        dplyr::mutate(
          Project = "HT"
        ) |>
        dplyr::bind_rows(
          data_3k_v3 |>
            dplyr::filter(
              .data[["OlinkID"]] %in% .env[["oid_only_in_notref_v3"]]
            ) |>
            dplyr::mutate(
              Project = "3K"
            )
        ) |>
        dplyr::mutate(
          BridgingRecommendation = "NotOverlapping"
        ) |>
        dplyr::arrange(
          .data[["SampleID"]], .data[["OlinkID"]]
        )
    )

    # cross-product normalization - remove assays from non reference ----

    data_3k_v4 <- data_3k |>
      dplyr::filter(
        !(.data[["OlinkID"]] %in% .env[["drop_3k_assays"]])
        & !(.data[["OlinkID"]] %in% .env[["oid_only_in_notref_v1"]])
      )
    data_ht_v4 <- data_ht |>
      dplyr::filter(
        !(.data[["OlinkID"]] %in% .env[["oid_only_in_ref_v1"]])
      )

    oid_only_in_ref_v4 <- get_non_shares_oid(
      df1 = data_ht_v4,
      df_suffix_p1 = "HT",
      df2 = data_3k_v4,
      df_suffix_p2 = "E3072"
    )
    oid_only_in_notref_v4 <- get_non_shares_oid(
      df1 = data_3k_v4,
      df_suffix_p1 = "E3072",
      df2 = data_ht_v4,
      df_suffix_p2 = "HT"
    )

    bridge_samples_crossproduct_v4 <- intersect(
      x = unique(data_3k_v4$SampleID),
      y = unique(data_ht_v4$SampleID)
    ) |>
      (\(x) x[!grepl("CONTROL", x)])()

    lst_check_cross_product_v4 <- olink_norm_input_check(
      df1 = data_ht_v4,
      df2 = data_3k_v4,
      overlapping_samples_df1 = bridge_samples_crossproduct_v4,
      overlapping_samples_df2 = NULL,
      df1_project_nr = "HT",
      df2_project_nr = "3K",
      reference_project = "HT",
      reference_medians = NULL
    ) |>
      suppressMessages() |>
      suppressWarnings()

    # check that function returns missing OlinkIDs with BridgingRecommendation
    expect_message(
      object = df_cross_product_miss_oid_v4 <- olink_format_oid_no_overlap(
        lst_check = lst_check_cross_product_v4
      ),
      regexp = paste("3 non-overlapping assays are included in the normalized",
                     "dataset without adjustment. Assays found in only one",
                     "project will have decreased statistical power due to the",
                     "lower number of samples.")
    )

    expect_equal(
      object = df_cross_product_miss_oid_v4 |>
        dplyr::arrange(
          .data[["SampleID"]], .data[["OlinkID"]]
        ),
      expected = data_ht_v4 |>
        dplyr::filter(
          .data[["OlinkID"]] %in% .env[["oid_only_in_ref_v4"]]
        ) |>
        dplyr::mutate(
          Project = "HT"
        ) |>
        dplyr::bind_rows(
          data_3k_v4 |>
            dplyr::filter(
              .data[["OlinkID"]] %in% .env[["oid_only_in_notref_v4"]]
            ) |>
            dplyr::mutate(
              Project = "3K"
            )
        ) |>
        dplyr::mutate(
          BridgingRecommendation = "NotOverlapping"
        ) |>
        dplyr::arrange(
          .data[["SampleID"]], .data[["OlinkID"]]
        )
    )
  }
)
