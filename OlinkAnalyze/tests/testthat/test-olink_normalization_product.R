# Test olink_normalization_bridgeable ----

test_that(
  "olink_normalization_is_bridgeable - works",
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

    # Warning that some assays are not overlapping and will be removed from
    # normalization.
    expect_warning(
      object = expect_message(
        object = olink_norm_input_check(
          df1 = data_3k,
          df2 = data_ht,
          overlapping_samples_df1 = bridge_samples_3k_ht,
          overlapping_samples_df2 = NULL,
          df1_project_nr = "P1",
          df2_project_nr = "P2",
          reference_project = "P2",
          reference_medians = NULL
        ),
        regexp = "Cross-product normalization will be performed!"
      ),
      regexp = "2 assays are not shared across products."
    )

    expect_message(
      object = data_explore_check <- olink_norm_input_check(
        df1 = data_3k |>
          dplyr::filter(
            !(.data[["OlinkID"]] %in% c("OID12345", "OID54321"))
          ),
        df2 = data_ht |>
          dplyr::filter(
            !(.data[["OlinkID"]] %in% c("OID12345", "OID54321"))
          ),
        overlapping_samples_df1 = bridge_samples_3k_ht,
        overlapping_samples_df2 = NULL,
        df1_project_nr = "P1",
        df2_project_nr = "P2",
        reference_project = "P2",
        reference_medians = NULL
      ),
      regexp = "Cross-product normalization will be performed!"
    )

    lst_df <- list(
      data_explore_check$ref_df,
      data_explore_check$not_ref_df
    )
    names(lst_df) <- c(data_explore_check$ref_name,
                       data_explore_check$not_ref_name)

    ref_cols <- data_explore_check$ref_cols
    not_ref_cols <- data_explore_check$not_ref_cols

    is_bridgeable_result <- olink_normalization_bridgeable(
      lst_df = lst_df,
      ref_cols = ref_cols,
      not_ref_cols = not_ref_cols,
      seed = 1
    )

    expect_equal(
      object = nrow(is_bridgeable_result),
      expected = 104L
    ) ## check nr of rows (added correlation assays)

    expect_equal(
      object = is_bridgeable_result |>
        dplyr::filter(
          .data[["BridgingRecommendation"]] == "MedianCentering"
        ) |>
        dplyr::distinct() |>
        nrow(),
      expected = 40L # 3 of 4 correlation assays added here
    )

    expect_equal(
      object = is_bridgeable_result |>
        dplyr::filter(
          .data[["BridgingRecommendation"]] == "QuantileSmoothing"
        ) |>
        dplyr::distinct() |>
        nrow(),
      expected = 63L # 1 correlation assay added here
    )

    expect_equal(
      object = is_bridgeable_result |>
        dplyr::filter(
          .data[["BridgingRecommendation"]] == "NotBridgeable"
        ) |>
        dplyr::distinct() |>
        nrow(),
      expected = 1L
    )

    expect_equal(
      object = is_bridgeable_result |>
        dplyr::filter(
          .data[["OlinkID"]] == "OID41012_OID20054"
        ) |>
        dplyr::distinct() |>
        dplyr::pull(
          .data[["BridgingRecommendation"]]
        ),
      expected = "NotBridgeable"
    )
  }
)

# Test olink_normalization_qs ----

test_that(
  "olink_normalization_qs - works - compare to reference",
  {
    skip_if_not(file.exists(test_path("data", "example_3k_data.rds")))
    skip_if_not(file.exists(test_path("data", "example_HT_data.rds")))
    skip_if_not(file.exists(test_path("data", "example_Reveal_data.rds")))

    exclude_assays <- c("OID12345", "OID54321")
    data_3k <- get_example_data(filename = "example_3k_data.rds") |>
      dplyr::filter(
        !(.data[["OlinkID"]] %in% .env[["exclude_assays"]])
      )
    data_ht <- get_example_data(filename = "example_HT_data.rds") |>
      dplyr::filter(
        !(.data[["OlinkID"]] %in% .env[["exclude_assays"]])
      )
    data_reveal <- get_example_data(filename = "example_Reveal_data.rds") |>
      dplyr::filter(
        !(.data[["OlinkID"]] %in% .env[["exclude_assays"]])
      )

    # load reference data ----

    skip_if_not(file.exists(
      testthat::test_path("data", "qq_normalization_reference_result.rds")
    ))
    skip_if_not(file.exists(
      testthat::test_path("data",
                          "qq_normalization_reference_result_reveal.rds")
    ))

    ref_qs_norm <- get_example_data(
      filename = "qq_normalization_reference_result.rds"
    )
    ref_qs_norm_reveal <- get_example_data(
      filename = "qq_normalization_reference_result_reveal.rds"
    )

    # run example data in the function ----

    # bridge samples
    bridge_samples <- intersect(
      x = unique(data_ht$SampleID),
      y = unique(data_3k$SampleID)
    ) |>
      (\(x) x[!grepl("CONTROL", x)])()

    bridge_samples_reveal <- intersect(
      x = unique(data_reveal$SampleID),
      y = unique(data_3k$SampleID)
    ) |>
      (\(x) x[!grepl("CONTROL", x)])()

    # run the internal function that check input from olink_normalization
    expect_message(
      object = norm_input_check <- olink_norm_input_check(
        df1 = data_ht,
        df2 = data_3k,
        overlapping_samples_df1 = bridge_samples,
        overlapping_samples_df2 = NULL,
        df1_project_nr = "P1",
        df2_project_nr = "P2",
        reference_project = "P1",
        reference_medians = NULL
      ),
      regexp = "Cross-product normalization will be performed!"
    )

    expect_warning(
      object = expect_message(
        object = norm_input_check_reveal <- olink_norm_input_check(
          df1 = data_reveal,
          df2 = data_3k,
          overlapping_samples_df1 = bridge_samples_reveal,
          overlapping_samples_df2 = NULL,
          df1_project_nr = "P1",
          df2_project_nr = "P2",
          reference_project = "P1",
          reference_medians = NULL
        ),
        regexp = "Cross-product normalization will be performed!"
      ),
      regexp = "83 assays are not shared"
    )

    lst_df <- list(
      norm_input_check$ref_df,
      norm_input_check$not_ref_df
    ) |>
      lapply(function(l_df) {
        l_df_res <- l_df |>
          dplyr::filter(
            .data[[norm_input_check$ref_cols$sample_id]] %in%
              .env[["bridge_samples"]]
          )
        return(l_df_res)
      })
    names(lst_df) <- c(norm_input_check$ref_name,
                       norm_input_check$not_ref_name)
    lst_product <- norm_input_check$lst_product

    lst_df_reveal <- list(
      norm_input_check_reveal$ref_df,
      norm_input_check_reveal$not_ref_df
    ) |>
      lapply(function(l_df) {
        l_df_res <- l_df |>
          dplyr::filter(
            .data[[norm_input_check$ref_cols$sample_id]] %in%
              .env[["bridge_samples"]]
          )
        return(l_df_res)
      })
    names(lst_df_reveal) <- c(norm_input_check$ref_name,
                              norm_input_check$not_ref_name)
    lst_product_reveal <- norm_input_check_reveal$lst_product

    # run the function
    expect_no_message(
      object = expect_no_warning(
        object = expect_no_error(
          object = qs_norm <- olink_normalization_qs(
            lst_df = lst_df,
            ref_cols = norm_input_check$ref_cols,
            not_ref_cols = norm_input_check$not_ref_cols,
            bridge_samples = bridge_samples,
            ref_product = norm_input_check$ref_product
          )
        )
      )
    )

    expect_no_message(
      object = expect_no_warning(
        object = expect_no_error(
          object = qs_norm_reveal <- olink_normalization_qs(
            lst_df = lst_df_reveal,
            ref_cols = norm_input_check_reveal$ref_cols,
            not_ref_cols = norm_input_check$not_ref_cols,
            bridge_samples = bridge_samples_reveal,
            ref_product = norm_input_check$ref_product
          )
        )
      )
    )

    expect_error(
      object = olink_normalization_qs(
        lst_df = lst_df,
        ref_cols = norm_input_check$ref_cols,
        not_ref_cols = norm_input_check$not_ref_cols,
        bridge_samples = bridge_samples,
        ref_product = "other"
      ),
      regexp = "Reference product must be HT or Reveal"
    )

    # check if reference is reproduced ----

    expect_equal(
      object = qs_norm |>
        dplyr::filter(
          .data[["Project"]] == norm_input_check$not_ref_name
        ) |>
        dplyr::select(
          dplyr::all_of(
            colnames(ref_qs_norm)
          )
        ) |>
        dplyr::arrange(
          .data[[norm_input_check$ref_cols$sample_id]],
          .data[[norm_input_check$ref_cols$olink_id]]
        ),
      expected = ref_qs_norm |>
        dplyr::arrange(
          .data[[norm_input_check$ref_cols$sample_id]],
          .data[[norm_input_check$ref_cols$olink_id]]
        ),
      tolerance = 1e-4
    )

    expect_equal(
      object = qs_norm_reveal |>
        dplyr::filter(
          .data[["Project"]] == norm_input_check_reveal$not_ref_name
        ) |>
        dplyr::select(
          dplyr::all_of(
            colnames(ref_qs_norm_reveal)
          )
        ) |>
        dplyr::arrange(
          .data[[norm_input_check_reveal$ref_cols$sample_id]],
          .data[[norm_input_check_reveal$ref_cols$olink_id]]
        ),
      expected = ref_qs_norm_reveal |>
        dplyr::arrange(
          .data[[norm_input_check_reveal$ref_cols$sample_id]],
          .data[[norm_input_check_reveal$ref_cols$olink_id]]
        ),
      tolerance = 1e-4
    )

  }
)

test_that(
  "olink_normalization_qs - works - expected output, all bridge samples",
  {
    skip_if_not(file.exists(test_path("data", "example_3k_data.rds")))
    skip_if_not(file.exists(test_path("data", "example_HT_data.rds")))

    excluded_assays <- c("OID12345", "OID54321")
    data_3k <- get_example_data(filename = "example_3k_data.rds") |>
      dplyr::filter(
        !(.data[["OlinkID"]] %in% .env[["excluded_assays"]])
      )
    data_ht <- get_example_data(filename = "example_HT_data.rds") |>
      dplyr::filter(
        !(.data[["OlinkID"]] %in% .env[["excluded_assays"]])
      )

    # bridge samples
    bridge_samples <- intersect(
      x = unique(data_ht$SampleID),
      y = unique(data_3k$SampleID)
    ) |>
      (\(x) x[!grepl("CONTROL", x)])()

    # run the internal function that check input from olink_normalization
    expect_message(
      object = norm_input_check <- olink_norm_input_check(
        df1 = data_ht,
        df2 = data_3k,
        overlapping_samples_df1 = bridge_samples,
        overlapping_samples_df2 = NULL,
        df1_project_nr = "P1",
        df2_project_nr = "P2",
        reference_project = "P1",
        reference_medians = NULL
      ),
      regexp = "Cross-product normalization will be performed!"
    )

    lst_df <- list(
      norm_input_check$ref_df,
      norm_input_check$not_ref_df
    )
    lst_product <- norm_input_check$lst_product
    names(lst_df) <- c(norm_input_check$ref_name,
                       norm_input_check$not_ref_name)

    # run the function
    expect_no_message(
      object = expect_no_warning(
        object = expect_no_error(
          object = qs_norm <- olink_normalization_qs(
            lst_df = lst_df,
            ref_cols = norm_input_check$ref_cols,
            not_ref_cols = norm_input_check$not_ref_cols,
            bridge_samples = bridge_samples,
            ref_product = norm_input_check$ref_product
          )
        )
      )
    )

    # random checks
    expect_equal(
      object = qs_norm |>
        dplyr::filter(
          .data[[norm_input_check$ref_cols$sample_id]] == "Sample_A"
          & .data[["Project"]] == norm_input_check$not_ref_name
          & .data[[norm_input_check$ref_cols$olink_id]] %in%
            c("OID40770_OID20117", "OID40835_OID31162", "OID40981_OID30796",
              "OID40986_OID20052", "OID41012_OID20054", "OID41032_OID20118")
        ) |>
        dplyr::arrange(
          .data[[norm_input_check$ref_cols$sample_id]],
          .data[[norm_input_check$ref_cols$olink_id]]
        ) |>
        dplyr::pull(
          .data[["QSNormalizedNPX"]]
        ),
      expected = c(0.9551492,  2.4156396, -2.2252346,
                   -1.4116657,  0.5896144,  3.5641947),
      tolerance = 1e-4
    )

    expect_equal(
      object = qs_norm |>
        dplyr::filter(
          .data[[norm_input_check$ref_cols$sample_id]] == "Sample_J"
          & .data[["Project"]] == norm_input_check$not_ref_name
          & .data[[norm_input_check$ref_cols$olink_id]] == "OID42135_OID21255"
        ) |>
        dplyr::pull(
          .data[["QSNormalizedNPX"]]
        ),
      expected = 8.852096,
      tolerance = 1e-4
    )

    expect_equal(
      object = qs_norm |>
        dplyr::filter(
          .data[[norm_input_check$ref_cols$sample_id]] == "Sample_AZ"
          & .data[["Project"]] == norm_input_check$not_ref_name
          & .data[[norm_input_check$ref_cols$olink_id]] == "OID41486_OID31160"
        ) |>
        dplyr::pull(
          .data[["QSNormalizedNPX"]]
        ),
      expected = 1.867354,
      tolerance = 1e-4
    )

    expect_identical(
      object = qs_norm |>
        dplyr::filter(
          .data[["Project"]] == norm_input_check$not_ref_name
        ) |>
        nrow(),
      expected = 18304L # no control samples
    )

    expect_identical(
      object = qs_norm |>
        dplyr::filter(
          .data[["Project"]] == norm_input_check$ref_name
        ) |>
        nrow(),
      expected = 17888L # no control samples
    )
  }
)

test_that(
  "olink_normalization_qs - works - expected output, 50 bridge samples",
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

    # run the internal function that check input from olink_normalization
    expect_warning(
      object = expect_message(
        object = norm_input_check <- olink_norm_input_check(
          df1 = data_ht,
          df2 = data_3k,
          overlapping_samples_df1 = bridge_samples,
          overlapping_samples_df2 = NULL,
          df1_project_nr = "P1",
          df2_project_nr = "P2",
          reference_project = "P1",
          reference_medians = NULL
        ),
        regexp = "Cross-product normalization will be performed!"
      ),
      regexp = "2 assays are not shared across products."
    )

    lst_df <- list(
      norm_input_check$ref_df,
      norm_input_check$not_ref_df
    )
    names(lst_df) <- c(norm_input_check$ref_name,
                       norm_input_check$not_ref_name)

    # run the function
    expect_no_message(
      object = expect_no_warning(
        object = expect_no_error(
          object = qs_norm <- olink_normalization_qs(
            lst_df = lst_df,
            ref_cols = norm_input_check$ref_cols,
            not_ref_cols = norm_input_check$not_ref_cols,
            bridge_samples = bridge_samples,
            ref_product = norm_input_check$ref_product
          )
        )
      )
    )

    # random checks
    expect_equal(
      object = qs_norm |>
        dplyr::filter(
          .data[[norm_input_check$ref_cols$sample_id]] == "Sample_A"
          & .data[["Project"]] == norm_input_check$not_ref_name
          & .data[[norm_input_check$ref_cols$olink_id]] %in%
            c("OID40770_OID20117", "OID40835_OID31162", "OID40981_OID30796",
              "OID40986_OID20052", "OID41012_OID20054", "OID41032_OID20118")
        ) |>
        dplyr::arrange(
          .data[[norm_input_check$ref_cols$sample_id]],
          .data[[norm_input_check$ref_cols$olink_id]]
        ) |>
        dplyr::pull(
          .data[["QSNormalizedNPX"]]
        ),
      expected = c(1.0147421,  2.2074429, -1.9974353,
                   -1.5961883,  0.6344671,  3.5684450),
      tolerance = 1e-4
    )

    expect_equal(
      object = qs_norm |>
        dplyr::filter(
          .data[[norm_input_check$ref_cols$sample_id]] == "Sample_CT_3k"
          & .data[["Project"]] == norm_input_check$not_ref_name
          & .data[[norm_input_check$ref_cols$olink_id]] == "OID42135_OID21255"
        ) |>
        dplyr::pull(
          .data[["QSNormalizedNPX"]]
        ),
      expected = 3.185605,
      tolerance = 1e-4
    )

    expect_equal(
      object = qs_norm |>
        dplyr::filter(
          .data[[norm_input_check$ref_cols$sample_id]] == "Sample_EW_3k"
          & .data[["Project"]] == norm_input_check$not_ref_name
          & .data[[norm_input_check$ref_cols$olink_id]] == "OID41486_OID31160"
        ) |>
        dplyr::pull(
          .data[["QSNormalizedNPX"]]
        ),
      expected = 6.028731,
      tolerance = 1e-4
    )

    expect_identical(
      object = qs_norm |>
        dplyr::filter(
          .data[["Project"]] == norm_input_check$not_ref_name
        ) |>
        nrow(),
      expected = 18304L # no control samples
    )

    expect_identical(
      object = qs_norm |>
        dplyr::filter(
          .data[["Project"]] == norm_input_check$ref_name
        ) |>
        nrow(),
      expected = 17888L # no control samples
    )
  }
)

test_that(
  "olink_normalization_qs - works - fewer than 40 bridge samples",
  {

    skip_if_not(file.exists(test_path("data", "example_3k_data.rds")))
    skip_if_not(file.exists(test_path("data", "example_HT_data.rds")))

    data_3k <- get_example_data(filename = "example_3k_data.rds")
    data_ht <- get_example_data(filename = "example_HT_data.rds")

    # 38 bridge samples for all assays ----

    bridge_samples <- intersect(
      x = unique(data_ht$SampleID),
      y = unique(data_3k$SampleID)
    ) |>
      (\(x) x[!grepl("CONTROL", x)])() |>
      sort()

    # run the internal function that check input from olink_normalization
    expect_warning(
      object = expect_message(
        object = norm_input_check <- olink_norm_input_check(
          df1 = data_ht,
          df2 = data_3k,
          overlapping_samples_df1 = head(x = bridge_samples, 38L),
          overlapping_samples_df2 = NULL,
          df1_project_nr = "P1",
          df2_project_nr = "P2",
          reference_project = "P1",
          reference_medians = NULL
        ),
        regexp = "Cross-product normalization will be performed!"
      ),
      regexp = "2 assays are not shared across products."
    )

    lst_df <- list(
      norm_input_check$ref_df,
      norm_input_check$not_ref_df
    )
    names(lst_df) <- c(norm_input_check$ref_name,
                       norm_input_check$not_ref_name)

    # run the function
    expect_warning(
      object = olink_normalization_qs(
        lst_df = lst_df,
        ref_cols = norm_input_check$ref_cols,
        not_ref_cols = norm_input_check$not_ref_cols,
        bridge_samples = head(x = bridge_samples, 38L),
        ref_product = norm_input_check$ref_product
      ),
      regexp = "There are 104 assays with fewer than 40 bridge samples for QS"
    )

    expect_warning(
      object = olink_normalization_qs(
        lst_df = lst_df,
        ref_cols = norm_input_check$ref_cols,
        not_ref_cols = norm_input_check$not_ref_cols,
        bridge_samples = head(x = bridge_samples, 40L),
        ref_product = norm_input_check$ref_product
      ),
      regexp = "There are 31 assays with fewer than 40 bridge samples for QS"
    )
  }
)

# Test olink_normalization_product_format ----

test_that(
  "olink_normalization_product_format - works - 3k-HT",
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

    # formatted data
    expect_message(
      object = expect_warning(
        object = df_3k_ht_format <- olink_normalization(
          df1 = data_3k,
          df2 = data_ht,
          overlapping_samples_df1 = bridge_samples_3k_ht,
          df1_project_nr = "P1",
          df2_project_nr = "P2",
          reference_project = "P2",
          format = TRUE # format data
        ),
        regexp = "2 assays are not shared across products."
      ),
      regexp = "Cross-product normalization will be performed!"
    )

    # unformatted data
    expect_message(
      object = expect_warning(
        object = df_3k_ht_noformat <- olink_normalization(
          df1 = data_3k,
          df2 = data_ht,
          overlapping_samples_df1 = bridge_samples_3k_ht,
          df1_project_nr = "P1",
          df2_project_nr = "P2",
          reference_project = "P2"
        ),
        regexp = "2 assays are not shared across products."
      ),
      regexp = "Cross-product normalization will be performed!"
    )

    expect_no_condition(
      object = df_3k_ht_format_fun <- olink_normalization_product_format(
        df_norm = df_3k_ht_noformat,
        df1 = data_ht,
        df1_project_nr = "P2",
        df2 = data_3k,
        df2_project_nr = "P1",
        reference_project = "P2",
        ref_product = "HT"
      )
    )

    ## check that function works both ways
    expect_equal(
      object = dim(df_3k_ht_format),
      expected = dim(df_3k_ht_format_fun)
    )

    ## check that correct columns are removed
    expect_false(
      object = any(c("MedianCenteredNPX", "QSNormalizedNPX", "OlinkID_E3072")
                   %in% colnames(df_3k_ht_format))
    )

    ## check that NotBridgeable assays get their own OlinkIDs
    expect_equal(
      object = df_3k_ht_format |>
        dplyr::filter(
          .data[["BridgingRecommendation"]] == "NotBridgeable"
        ) |>
        dplyr::pull(
          .data[["OlinkID"]]
        ) |>
        unique() |>
        sort(),
      expected = c("OID20054", "OID41012")
    )

    ## check that NPX is being replaced correctly
    npx_bridging_recs <- df_3k_ht_noformat |>
      dplyr:::mutate(
        SampleID = paste0(.data[["SampleID"]], "_", .data[["Project"]]),
        OlinkID =  paste0(.data[["OlinkID"]], "_", .data[["OlinkID_E3072"]])
      ) |>
      dplyr::select(
        dplyr::all_of(
          c("SampleID", "OlinkID", "Block",
            "BridgingRecommendationOriginal" = "BridgingRecommendation",
            "MedianCenteredNPX", "QSNormalizedNPX")
        )
      )

    npx_assignment_check <- df_3k_ht_format |>
      dplyr::filter(
        !(.data[["BridgingRecommendation"]] %in% c("NotBridgeable",
                                                   "NotOverlapping"))
      ) |>
      dplyr::left_join(
        npx_bridging_recs,
        by = c("SampleID", "OlinkID", "Block"),
        relationship = "one-to-one"
      ) |>
      dplyr::filter(
        .data[["Project"]] == "P1"
        & !((.data[["BridgingRecommendationOriginal"]] == "MedianCentering"
             & .data[["NPX"]] == .data[["MedianCenteredNPX"]])
            | (.data[["BridgingRecommendationOriginal"]] == "QuantileSmoothing"
               & .data[["NPX"]] == .data[["QSNormalizedNPX"]]))
      )

    expect_equal(
      object = nrow(npx_assignment_check),
      expected = 0L
    )

    ## check that the numbers of assay assignments are correct
    expect_equal(
      object = df_3k_ht_format |>
        dplyr::distinct(
          .data[["OlinkID"]], .data[["BridgingRecommendation"]]
        ) |>
        dplyr::filter(
          .data[["BridgingRecommendation"]] == "NotOverlapping"
        ) |>
        nrow(),
      expected = 2L
    )

    expect_equal(
      object = df_3k_ht_format |>
        dplyr::distinct(
          .data[["OlinkID"]], .data[["BridgingRecommendation"]]
        ) |>
        dplyr::filter(
          .data[["BridgingRecommendation"]] == "NotBridgeable"
        ) |>
        nrow(),
      expected = 2L
    )

    expect_equal(
      object = df_3k_ht_format |>
        dplyr::distinct(
          .data[["OlinkID"]], .data[["BridgingRecommendation"]]
        ) |>
        dplyr::filter(
          .data[["BridgingRecommendation"]] == "MedianCentering"
        ) |>
        nrow(),
      expected = 40L
    )

    expect_equal(
      object = df_3k_ht_format |>
        dplyr::distinct(
          .data[["OlinkID"]], .data[["BridgingRecommendation"]]
        ) |>
        dplyr::filter(
          .data[["BridgingRecommendation"]] == "QuantileSmoothing"
        ) |>
        nrow(),
      expected = 63L
    )
  }
)

test_that(
  "olink_normalization_product_format - works - 3k-Reveal",
  {
    skip_if_not(file.exists(test_path("data", "example_3k_data.rds")))
    skip_if_not(file.exists(test_path("data", "example_Reveal_data.rds")))

    data_3k <- get_example_data(filename = "example_3k_data.rds")
    data_reveal <- get_example_data(filename = "example_Reveal_data.rds")

    # 3k-HT ----

    bridge_samples_3k_reveal <- intersect(
      x = unique(data_3k$SampleID),
      y = unique(data_reveal$SampleID)
    ) |>
      (\(x) x[!grepl("CONTROL", x)])() |>
      sort() |>
      head(35L)

    # formatted data
    expect_warning(
      object = expect_warning(
        object = expect_message(
          object = df_3k_reveal_format <- olink_normalization(
            df1 = data_3k,
            df2 = data_reveal,
            overlapping_samples_df1 = bridge_samples_3k_reveal,
            df1_project_nr = "P1",
            df2_project_nr = "P2",
            reference_project = "P2",
            format = TRUE # format data
          ),
          regexp = "Cross-product normalization will be performed!"
        ),
        regexp = "85 assays are not shared across products."
      ),
      regexp = "There are 6 assays with fewer than 32 bridge samples for QS"
    )

    # unformatted data
    expect_warning(
      object = expect_warning(
        object = expect_message(
          object = df_3k_reveal_noformat <- olink_normalization(
            df1 = data_3k,
            df2 = data_reveal,
            overlapping_samples_df1 = bridge_samples_3k_reveal,
            df1_project_nr = "P1",
            df2_project_nr = "P2",
            reference_project = "P2"
          ),
          regexp = "Cross-product normalization will be performed!"
        ),
        regexp = "85 assays are not shared across products."
      ),
      regexp = "There are 6 assays with fewer than 32 bridge samples for QS"
    )

    expect_no_condition(
      object = df_3k_reveal_format_fun <- olink_normalization_product_format(
        df_norm = df_3k_reveal_noformat,
        df1 = data_reveal,
        df1_project_nr = "P2",
        df2 = data_3k,
        df2_project_nr = "P1",
        reference_project = "P2",
        ref_product = "Reveal"
      )
    )

    ## check that function works both ways
    expect_equal(
      object = dim(df_3k_reveal_format),
      expected = dim(df_3k_reveal_format_fun)
    )

    ## check that correct columns are removed
    expect_false(
      object = any(c("MedianCenteredNPX", "QSNormalizedNPX", "OlinkID_E3072")
                   %in% colnames(df_3k_reveal_format))
    )

    ## check that NotBridgeable assays get their own OlinkIDs
    expect_equal(
      object = df_3k_reveal_format |>
        dplyr::filter(
          .data[["BridgingRecommendation"]] == "NotBridgeable"
        ) |>
        dplyr::pull(
          .data[["OlinkID"]]
        ) |>
        unique() |>
        sort(),
      expected = c("OID20052", "OID20117", "OID20798", "OID20843", "OID20856",
                   "OID21178", "OID21197", "OID21198", "OID21234", "OID30086",
                   "OID30421", "OID30498", "OID31161", "OID50023", "OID50033",
                   "OID50044", "OID50059", "OID50088", "OID50092", "OID50093",
                   "OID50100", "OID50101", "OID50102", "OID50105", "OID50106",
                   "OID50107")
    )

    ## check that NPX is being replaced correctly
    npx_bridging_recs <- df_3k_reveal_noformat |>
      dplyr:::mutate(
        SampleID = paste0(.data[["SampleID"]], "_", .data[["Project"]]),
        OlinkID =  paste0(.data[["OlinkID"]], "_", .data[["OlinkID_E3072"]])
      ) |>
      dplyr::select(
        dplyr::all_of(
          c("SampleID", "OlinkID", "Block",
            "BridgingRecommendationOriginal" = "BridgingRecommendation",
            "MedianCenteredNPX", "QSNormalizedNPX")
        )
      )

    npx_assignment_check <- df_3k_reveal_format |>
      dplyr::filter(
        !(.data[["BridgingRecommendation"]] %in% c("NotBridgeable",
                                                   "NotOverlapping"))
      ) |>
      dplyr::left_join(
        npx_bridging_recs,
        by = c("SampleID", "OlinkID", "Block"),
        relationship = "one-to-one"
      ) |>
      dplyr::filter(
        .data[["Project"]] == "P1"
        & !((.data[["BridgingRecommendationOriginal"]] == "MedianCentering"
             & .data[["NPX"]] == .data[["MedianCenteredNPX"]])
            | (.data[["BridgingRecommendationOriginal"]] == "QuantileSmoothing"
               & .data[["NPX"]] == .data[["QSNormalizedNPX"]]))
      )

    expect_equal(
      object = nrow(npx_assignment_check),
      expected = 0L
    )

    ## check that the numbers of assay assignments are correct
    expect_equal(
      object = df_3k_reveal_format |>
        dplyr::distinct(
          .data[["OlinkID"]], .data[["BridgingRecommendation"]]
        ) |>
        dplyr::filter(
          .data[["BridgingRecommendation"]] == "NotOverlapping"
        ) |>
        nrow(),
      expected = 84L
    )

    expect_equal(
      object = df_3k_reveal_format |>
        dplyr::distinct(
          .data[["OlinkID"]], .data[["BridgingRecommendation"]]
        ) |>
        dplyr::filter(
          .data[["BridgingRecommendation"]] == "NotBridgeable"
        ) |>
        nrow(),
      expected = 26L
    )

    expect_equal(
      object = df_3k_reveal_format |>
        dplyr::distinct(
          .data[["OlinkID"]], .data[["BridgingRecommendation"]]
        ) |>
        dplyr::filter(
          .data[["BridgingRecommendation"]] == "MedianCentering"
        ) |>
        nrow(),
      expected = 2L
    )

    expect_equal(
      object = df_3k_reveal_format |>
        dplyr::distinct(
          .data[["OlinkID"]], .data[["BridgingRecommendation"]]
        ) |>
        dplyr::filter(
          .data[["BridgingRecommendation"]] == "QuantileSmoothing"
        ) |>
        nrow(),
      expected = 6L
    )
  }
)

test_that(
  "Non-overlapping assays 3k and Reveal",
  {
    skip_if_not(file.exists(test_path("data", "example_3k_data.rds")))
    skip_if_not(file.exists(test_path("data", "example_Reveal_data.rds")))

    data_3k <- get_example_data(filename = "example_3k_data.rds")
    data_reveal <- get_example_data(filename = "example_Reveal_data.rds")

    lst_df <- list("e3k" = data_3k,
                   "Reveal" = data_reveal)
    lst_cols <- olink_norm_input_check_df_cols(lst_df = lst_df)
    lst_product <- olink_norm_product_id(
      lst_df = lst_df,
      lst_cols = lst_cols
    )
    ref_ids <- olink_norm_reference_id(
      lst_product = lst_product,
      reference_project = "Reveal"
    )
    lst_norm_cp <- olink_norm_input_cross_product(
      lst_df = lst_df,
      lst_cols = lst_cols,
      reference_project = "Reveal",
      product_ids = lst_product,
      ref_ids = ref_ids
    )

    expect_warning(
      object = overlapping_assays <- olink_norm_input_assay_overlap(
        lst_df = lst_norm_cp$lst_df,
        reference_medians = NULL,
        lst_cols = lst_cols,
        norm_mode = lst_norm_cp$norm_mode
      ),
      regexp = "85 assays are not shared across products"
    )

    expect_equal(
      object = length(
        unique(
          overlapping_assays$lst_df$e3k$OlinkID
        )
      ),
      expected = 21L)

    expect_equal(
      object = length(
        unique(
          overlapping_assays$lst_df$Reveal$OlinkID
        )
      ),
      expected = 21L)

  }
)
