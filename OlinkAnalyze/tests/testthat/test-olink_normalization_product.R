# Test olink_normalization_bridgeable ----

test_that(
  "olink_normalization_is_bridgeable - works",
  {
    skip_if_not(file.exists(normalizePath("../data/example_3k_data.rds")))
    data_3k <- get_example_data(filename = "example_3k_data.rds")
    data_ht <- get_example_data(filename = "example_HT_data.rds")

    expect_warning(
      object = olink_norm_input_check(
        df1 = data_3k,
        df2 = data_ht,
        overlapping_samples_df1 = intersect(
          x = unique(data_3k$SampleID),
          y = unique(data_ht$SampleID)
        ) |>
          (\(x) x[!grepl("CONTROL", x)])(),
        overlapping_samples_df2 = NULL,
        df1_project_nr = "P1",
        df2_project_nr = "P2",
        reference_project = "P2",
        reference_medians = NULL
      ),
      regexp = "2 assays are not shared across products." # Warning that some assays are not overlapping and will be removed from normalization.
    )


    expect_message(
      object = data_explore_check <- olink_norm_input_check(
        df1 = data_3k |>
          dplyr::filter(!(OlinkID %in% c("OID12345", "OID54321"))),
        df2 = data_ht|>
          dplyr::filter(!(OlinkID %in% c("OID12345", "OID54321"))),
        overlapping_samples_df1 = intersect(
          x = unique(data_3k$SampleID),
          y = unique(data_ht$SampleID)
        ) |>
          (\(x) x[!grepl("CONTROL", x)])(),
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
    skip_if_not(file.exists("../data/example_3k_data.rds"))
    skip_if_not(file.exists("../data/example_HT_data.rds"))

    data_3k <- get_example_data(filename = "example_3k_data.rds") |>
      dplyr::filter(!(OlinkID %in% c("OID12345", "OID54321")))
    data_ht <- get_example_data(filename = "example_HT_data.rds") |>
      dplyr::filter(!(OlinkID %in% c("OID12345", "OID54321")))

    # load reference data ----

    ref_qs_norm_file <- test_path("..",
                                  "data",
                                  "qq_normalization_reference_result.rds")
    ref_qs_norm <- readRDS(file = ref_qs_norm_file)

    # run example data in the function ----

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
    ) |>
      lapply(function(l_df) {
        l_df |>
          dplyr::filter(
            .data[[norm_input_check$ref_cols$sample_id]] %in%
              .env[["bridge_samples"]]
          )
      })
    names(lst_df) <- c(norm_input_check$ref_name,
                       norm_input_check$not_ref_name)

    # run the function
    expect_no_message(
      object = expect_no_warning(
        object = expect_no_error(
          object = qs_norm <- olink_normalization_qs(
            lst_df = lst_df,
            ref_cols = norm_input_check$ref_cols,
            bridge_samples = bridge_samples
          )
        )
      )
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

  }
)

test_that(
  "olink_normalization_qs - works - expected output, all bridge samples",
  {
    skip_if_not(file.exists("../data/example_3k_data.rds"))
    skip_if_not(file.exists("../data/example_HT_data.rds"))

    data_3k <- get_example_data(filename = "example_3k_data.rds") |>
      dplyr::filter(!(OlinkID %in% c("OID12345", "OID54321")))
    data_ht <- get_example_data(filename = "example_HT_data.rds") |>
      dplyr::filter(!(OlinkID %in% c("OID12345", "OID54321")))

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
    names(lst_df) <- c(norm_input_check$ref_name,
                       norm_input_check$not_ref_name)

    # run the function
    expect_no_message(
      object = expect_no_warning(
        object = expect_no_error(
          object = qs_norm <- olink_normalization_qs(
            lst_df = lst_df,
            ref_cols = norm_input_check$ref_cols,
            bridge_samples = bridge_samples
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
    skip_if_not(file.exists("../data/example_3k_data.rds"))
    skip_if_not(file.exists("../data/example_HT_data.rds"))

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
    expect_message(expect_warning(
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
      regexp = "2 assays are not shared across products."),
      regexp = "Cross-product normalization will be performed!"
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
            bridge_samples = bridge_samples
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
