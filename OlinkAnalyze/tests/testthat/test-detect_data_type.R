#' Test for detect_data_type function
#'

# input is not an arrow object
test_that("detect_data_type detects that input is not an arrow object", {
  df <- dplyr::tibble(X = c(1, 2, 3),
                      Y = c(4, 5, 6))
  expect_error(detect_data_type(df), "is not an R6 ArrowObject!")
})

# NPX data
test_that("detect_data_type correctly detects NPX column", {
  df <- dplyr::tibble(NPX = c(1, 2, 3),
                      Y = c(4, 5, 6)) |>
    arrow::as_arrow_table()
  expect_equal(detect_data_type(df), "NPX")
})

# Quant data
test_that("detect_data_type correctly detects Quantified_value column", {
  df <- dplyr::tibble(Quantified_value = c(1, 2, 3),
                      Y = c(4, 5, 6)) |>
    arrow::as_arrow_table()
  expect_equal(detect_data_type(df), "Quantified_value")
})

# Neither in dataset
test_that(
  "detect_data_type handles absence of NPX or Quantified_value column",
  {
    df <- dplyr::tibble(X = c(1, 2, 3),
                        Y = c(4, 5, 6)) |>
      arrow::as_arrow_table()
    expect_error(detect_data_type(df),
                 "Neither NPX nor Quantified_value column present in the data")
  })
