test_that("olink_normalization_is_bridgeable function works", {

  results <- olink_normalization_is_bridgeable(data_Explore384 = data_3k, data_HT = data_ht)

  expect_equal(object = nrow(results),
               expected = 100L) ## check nr of rows
  expect_equal(object = results |>
                 dplyr::filter(.data[["BridgingRecommendation"]] == "Median Centered") |>
                 dplyr::distinct() |>
                 nrow(),
               expected = 60L)
  expect_equal(object = results |>
                 dplyr::filter(.data[["BridgingRecommendation"]] ==
                                 "Quantile Smoothing") |>
                 dplyr::distinct() |>
                 nrow(),
               expected = 39L)

  expect_equal(object = results |>
                 dplyr::filter(.data[["BridgingRecommendation"]] == "Not Bridgeable") |>
                 dplyr::distinct() |>
                 nrow(),
               expected = 1L)
  expect_equal(object = results |>
                 dplyr::filter(.data[["OlinkID_concat"]] == "OID41012_OID20054") |>
                 dplyr::distinct() |>
                 dplyr::pull(.data[["BridgingRecommendation"]]),
               expected = "Not Bridgeable")
}
)
