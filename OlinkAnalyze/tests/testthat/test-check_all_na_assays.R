#' Tests for check_all_na_assays function
#'

test_that("all NA values are detected", {
  df <- arrow::arrow_table(
    SampleID = c("A", "B", "A", "B"),
    OlinkID = c("OID12345", "OID12345", "OID23456", "OID23456"),
    NPX = c(NA_real_, NA_real_, 1.2, 1.3)
  )
  expect_warning(check_all_na_assays(df),
                 "OID12345 has NPX = NA for all samples.")
  expect_equal(suppressWarnings(check_all_na_assays(df)), "OID12345")

})

test_that("no NA values are detected", {
  df <- arrow::arrow_table(
    SampleID = c("A", "B", "A", "B"),
    OlinkID = c("OID12345", "OID12345", "OID23456", "OID23456"),
    NPX = c(1.1, 1.2, 1.3, 1.4)
  )
  expect_equal(check_all_na_assays(df), character(0))
})

test_that("some NA values are detected correctly", {
  df <- arrow::arrow_table(
    SampleID = c("A", "B", "A", "B"),
    OlinkID = c("OID12345", "OID12345", "OID23456", "OID23456"),
    NPX = c(1.1, rep(NA_real_, 3))
  )
  expect_equal(suppressWarnings(check_all_na_assays(df)), "OID23456")
})
