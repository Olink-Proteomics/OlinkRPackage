#' Test for check_duplicate_sample_ids function
#'


test_that("function detects duplicate sample ID", {
  df <- arrow::arrow_table(SampleID = c("A", "B", "A", "C"),
                           OlinkID = rep("OID12345", 4),
                           NPX = rnorm(4))
  col_names <-  list(quant = "NPX",
                     olink_id = "OlinkID",
                     sample_id = "SampleID")

  expect_warning(check_duplicate_sample_ids(df, col_names),
                 "Duplicate sample ID detected: A")
})

test_that("function detects duplicate sample IDs", {
  df <- arrow::arrow_table(SampleID = c("A", "A", "B", "B", "C"),
                           OlinkID = c(rep("OID12345", 2),
                                       rep("OID12346", 3)),
                           NPX = rnorm(5))
  col_names <-  list(quant = "NPX",
                     olink_id = "OlinkID",
                     sample_id = "SampleID")

  expect_warning(check_duplicate_sample_ids(df, col_names),
                 "Duplicate sample IDs detected: A and B")
})

test_that("no duplicates exist, no warning", {
  df <- tibble::tibble(
    SampleID = c("A", "B", "A", "B"),
    OlinkID = c(rep("OID12345", 2),
                rep("OID12346", 2)),
    NPX = rnorm(4)
  )
  col_names <-  list(quant = "NPX",
                     olink_id = "OlinkID",
                     sample_id = "SampleID")

  expect_equal(check_duplicate_sample_ids(df, col_names),
               invisible(NULL))
})
