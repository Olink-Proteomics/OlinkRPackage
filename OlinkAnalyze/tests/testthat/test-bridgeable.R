#Load test data
test_file <- '../data/bridgeable_data.rda'
load(test_file)
rm(e3k_eHT_mapping) # Use mapping file called inside bridgeable()

# Run bridgeable()
results <- bridgeable(data_Explore384 = data_3k, data_HT = data_ht)

#results |> group_by(BridgingRecommendation) |> tally()
# A tibble: 3 × 2
# BridgingRecommendation     n
# <chr>                  <int>
#   1 Median Centered           59
# 2 Not Bridgeable             1
# 3 Quantile Smoothing        40

test_that("bridgeable function works", {
  expect_equal(nrow(results), 100) ## check nr of rows
  expect_equal(results |>
                 dplyr::filter(BridgingRecommendation == "Median Centered") |>
                 unique() |>
                 nrow(),59)
  
  expect_equal(results |>
                 dplyr::filter(BridgingRecommendation == "Quantile Smoothing") |>
                 unique() |>
                 nrow(),40)
  
  expect_equal(results |>
                 dplyr::filter(BridgingRecommendation == "Not Bridgeable") |>
                 unique() |>
                 nrow(),1)
  
})
