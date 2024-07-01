#' Tests for check_all_na_assays function
#'

test_that("assay has all-NA values and is captured", {
  df <- tibble::tibble(
    SampleID = c("A", "B", "A", "B"),
    OlinkID = c("OID12345",
                "OID12345",
                "OID23456",
                "OID23456"),
    NPX = c(NA_real_, NA_real_, 1.2, 1.3)
  )
  col_names <-  list(quant = "NPX",
                     olink_id = "OlinkID")
  result <- suppressWarnings(check_all_na_assays(df = df,
                                                 col_names = col_names))

  expect_warning(check_all_na_assays(df = df,
                                     col_names = col_names),
                 "OID12345 has NPX = NA for all samples.")
  expect_equal(result,
               "OID12345")

})

test_that("assay has some but not all NAs.", {
  df <- tibble::tibble(
    SampleID = c("A", "B", "A", "B"),
    OlinkID = c("OID12345",
                "OID12345",
                "OID23456",
                "OID23456"),
    NPX = c(1.1, 1.2, 1.3, NA_real_)
  )
  col_names <-  list(quant = "NPX",
                     olink_id = "OlinkID")
  result <- suppressWarnings(check_all_na_assays(df = df,
                                                 col_names = col_names))

  expect_equal(result,
               character(0))
})


test_that("no NA value exists.", {
  df <- arrow::arrow_table(
    SampleID = c("A", "B", "A", "B"),
    OlinkID = c("OID12345",
                "OID12345",
                "OID23456",
                "OID23456"),
    NPX = rnorm(4)
  )
  col_names <- list(quant = "NPX",
                    olink_id = "OlinkID")
  result <- suppressWarnings(check_all_na_assays(df = df,
                                                 col_names = col_names))

  expect_equal(result,
               character(0))
})
