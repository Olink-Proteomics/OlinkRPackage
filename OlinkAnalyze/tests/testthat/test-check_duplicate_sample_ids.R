#' Test for check_duplicate_sample_ids function
#'

test_that("function detects that input is not an arrow object", {
  df <- dplyr::tibble(SampleID = c("A", "B", "A", "C"),
                      OlinkID = rep("OID12345", 4),
                      NPX = rnorm(4))
  expect_error(check_duplicate_sample_ids(df), "is not an R6 ArrowObject!")
})

test_that("function detects duplicate sample ID", {
  df <- arrow::arrow_table(SampleID = c("A", "B", "A", "C"),
                           OlinkID = rep("OID12345", 4),
                           NPX = rnorm(4))
  expect_warning(check_duplicate_sample_ids(df),
                 "Duplicate sample ID detected: A")
})

test_that("function correctly does not detect duplicates", {
  df <- arrow::arrow_table(
    SampleID = c("A", "B", "C", "D"),
    OlinkID = rep("OID12345", 4),
    NPX = rnorm(4))
  expect_equal(check_duplicate_sample_ids(df), invisible(NULL))
})
