test_that(
  "olink_normalization_qs - works - compare to reference",
  {
    # load reference data ----

    ref_qs_norm_file <- test_path("..",
                                  "data",
                                  "qq_normalization_reference_result.rds")
    ref_qs_norm <- readRDS(file = ref_qs_norm_file)

    # run example data in the function ----

    # bridge samples
    overlap_samples <- intersect(
      x = unique(data_ht$SampleID),
      y = unique(data_3k$SampleID)
    ) |>
      (\(x) x[!grepl("CONTROL", x)])()

    # run the internal function that check input from olink_normalization
    expect_message(
      object = norm_input_check <- olink_norm_input_check(
        df1 = data_ht,
        df2 = data_3k,
        overlapping_samples_df1 = overlap_samples,
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
              .env[["overlap_samples"]]
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
            bridge_samples = overlap_samples
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

overlap_samples <- intersect(data_ht |>
                               dplyr::filter(SampleType == "SAMPLE") |>
                               dplyr::pull(SampleID) |>
                               unique(),
                             data_3k |>
                               dplyr::filter(SampleType == "SAMPLE") |>
                               dplyr::pull(SampleID) |>
                               unique())

bridge_samples <- list("DF_ht" = overlap_samples, "DF_3k" = overlap_samples)
rm(overlap_samples)
results <- olink_normalization_qs(exploreht_df = data_ht,
                                  explore3072_df = data_3k,
                                  bridge_samples = bridge_samples)
results$QSNormalizedNPX <- round(results$QSNormalizedNPX, 2)

test_that("quantile smoothing normalization is accurate", {
  expect_identical(head(results),
                   tibble(SampleID = rep("Sample_A", 6),
                          Project = rep("reference", 6),
                          OlinkID_concat = c("OID40770_OID20117",
                                             "OID40835_OID31162",
                                             "OID40981_OID30796",
                                             "OID40986_OID20052",
                                             "OID41012_OID20054",
                                             "OID41032_OID20118"),
                          QSNormalizedNPX = c(3.17, -1.61, -1.01, 2.57,
                                              0.1, -0.09)))
  expect_identical(results |>
                     dplyr::filter(SampleID == "Sample_CT",
                                   Project == "new",
                                   OlinkID_concat == "OID42135_OID21255") |>
                     unique() |>
                     dplyr::pull(QSNormalizedNPX), -0.35)

  expect_identical(results |>
                     dplyr::filter(SampleID == "Sample_W",
                                   Project == "new",
                                   OlinkID_concat == "OID41486_OID31160") |>
                     unique() |>
                     dplyr::pull(QSNormalizedNPX), 4.71)

  expect_equal(results |>
                 dplyr::filter(Project == "new") |>
                 unique() |>
                 nrow(), 18000)

  expect_equal(results |>
                 dplyr::filter(Project == "reference") |>
                 unique() |>
                 nrow(), 17800)
}
)
