results <- bridgeable(data_Explore384 = data_3k, data_HT = data_ht)

test_that("bridgeable function works", {
  expect_equal(nrow(results), 100) ## check nr of rows
  expect_equal(results |>
                 dplyr::filter(BridgingRecommendation == "Median Centered") |>
                 unique() |>
                 nrow(), 62)
  expect_equal(results |>
                 dplyr::filter(BridgingRecommendation ==
                                 "Quantile Smoothing") |>
                 unique() |>
                 nrow(), 37)

  expect_equal(results |>
                 dplyr::filter(BridgingRecommendation == "Not Bridgeable") |>
                 unique() |>
                 nrow(), 1)
  expect_equal(results |>
                 dplyr::filter(OlinkID_concat == "OID41012_OID20054") |>
                 unique() |>
                 dplyr::pull(BridgingRecommendation), "Not Bridgeable")
}
)
