test_that(
  "olink_normalization_qs - works - compare to reference",
  {
    # load reference data ----

    ref_qs_norm_file <- test_path("..",
                                  "data",
                                  "qq_normalization_reference_result.rds")
    ref_qs_norm <- readRDS(file = ref_qs_norm_file)

    data_3k_file <- test_path("..",
                                  "data",
                                  "data_3k.rds")
    data_3k <- readRDS(file = data_3k_file)

    data_ht_file <- test_path("..",
                              "data",
                              "data_ht.rds")
    data_ht <- readRDS(file = data_ht_file)

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
      expected = c(1.3373010,  0.9455337, 0.9250129,
                   0.0757910, -0.3936278, 4.1004279),
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
      expected = 1.815376,
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
      expected = 2.872351,
      tolerance = 1e-4
    )

    expect_identical(
      object = qs_norm |>
        dplyr::filter(
          .data[["Project"]] == norm_input_check$not_ref_name
        ) |>
        nrow(),
      expected = 17600L # no control samples
    )

    expect_identical(
      object = qs_norm |>
        dplyr::filter(
          .data[["Project"]] == norm_input_check$ref_name
        ) |>
        nrow(),
      expected = 17200L # no control samples
    )
  }
)


test_that(
  "olink_normalization_qs - works - expected output, 50 bridge samples",
  {
    # bridge samples
    bridge_samples <- intersect(
      x = unique(data_ht$SampleID),
      y = unique(data_3k$SampleID)
    ) |>
      (\(x) x[!grepl("CONTROL", x)])() |>
      sort() |>
      head(50L)

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
      expected = c(1.31915956,  0.76520904, 0.95169186,
                   -0.01082492, -0.40067950, 4.05650397),
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
      expected = -1.066912,
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
      expected = 2.899737,
      tolerance = 1e-4
    )

    expect_identical(
      object = qs_norm |>
        dplyr::filter(
          .data[["Project"]] == norm_input_check$not_ref_name
        ) |>
        nrow(),
      expected = 17600L # no control samples
    )

    expect_identical(
      object = qs_norm |>
        dplyr::filter(
          .data[["Project"]] == norm_input_check$ref_name
        ) |>
        nrow(),
      expected = 17200L # no control samples
    )
  }
)
