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

test_that("function detects duplicate sample IDs", {
  df <- arrow::arrow_table(SampleID = c("A", "A", "B", "B", "C"),
                           OlinkID = c(rep("OID12345", 2),
                                       rep("OID12346", 3)),
                           NPX = rnorm(5))
  expect_warning(check_duplicate_sample_ids(df),
                 "Duplicate sample IDs detected: A and B")
})

test_that("function correctly does not detect duplicates", {
  df <- arrow::arrow_table(
    SampleID = c("A", "B", "A", "B"),
    OlinkID = c(rep("OID12345", 2),
                rep("OID12346", 2)),
    NPX = rnorm(4))
  expect_equal(check_duplicate_sample_ids(df), invisible(NULL))
})
