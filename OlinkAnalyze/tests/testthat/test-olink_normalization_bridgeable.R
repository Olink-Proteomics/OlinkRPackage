test_that(
  "olink_normalization_is_bridgeable - works - 20 bridge samples",
  {
    expect_message(
      object = data_explore_check <- olink_norm_input_check(
        df1 = data_3k,
        df2 = data_ht,
        overlapping_samples_df1 = intersect(
          x = unique(data_3k$SampleID),
          y = unique(data_ht$SampleID)
        ) |>
          (\(x) x[!grepl("CONTROL", x)])() |>
          head(20L),
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

    is_bridgeable_result <- olink_normalization_bridgeable(
      lst_df = lst_df,
      ref_cols = ref_cols,
      seed = 1
    )

    expect_equal(
      object = nrow(is_bridgeable_result),
      expected = 100L
    ) ## check nr of rows

    expect_equal(
      object = is_bridgeable_result |>
        dplyr::filter(
          .data[["BridgingRecommendation"]] == "MedianCentering"
        ) |>
        dplyr::distinct() |>
        nrow(),
      expected = 37L
    )

    expect_equal(
      object = is_bridgeable_result |>
        dplyr::filter(
          .data[["BridgingRecommendation"]] == "QuantileSmoothing"
        ) |>
        dplyr::distinct() |>
        nrow(),
      expected = 62L
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
