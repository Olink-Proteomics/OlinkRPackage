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
