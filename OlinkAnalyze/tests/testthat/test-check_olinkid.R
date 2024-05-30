#' Test for check_olinkid function
#'

# input is neither a tibble nor an arrow data frame
test_that("df is not tibble or arrow data frame", {
  df <- data.frame(
    SampleID = c("A", "B", "C", "D"),
    OlinkID = rep("OID12345", 4),
    SampleType = rep("SAMPLE", 4),
    NPX = rnorm(4),
    PlateID = rep("plate1", 4),
    QC_Warning = rep("Pass", 4),
    LOD = rnorm(4)
  )
  expect_error(check_olinkid(df),
               "`df` is not a tibble or an arrow data frame!")
})

# Test that check_olinkid returns invalid olink IDs correctly.
test_that("check_olinkid returns invalid Olink IDs", {
  df <- dplyr::tibble(
    SampleID = c("A", "B", "C", "D", "E"),
    OlinkID = c("OID12345",
                "OID123456",
                "OID1234",
                "12345",
                "NA"), ## does not work for NA_real
    SampleType = rep("SAMPLE", 5),
    NPX = rnorm(5),
    PlateID = rep("plate1", 5),
    QC_Warning = rep("Pass", 5),
    LOD = rnorm(5)
  )
  col_names <- check_npx_col_names(df)
  result <- suppressWarnings(check_olinkid(df, col_names))

  expect_warning(check_olinkid(df, col_names),
                 "Unrecognized Olink IDs detected:
                 OID123456, OID1234, 12345, and NA")
  expect_equal(result, c("OID123456",
                         "OID1234",
                         "12345",
                         "NA"))
})

# All Olink IDs are valid
test_that("check_olinkid returns invalid Olink IDs", {
  df <- arrow::arrow_table(
    SampleID = c("A", "B", "C", "D"),
    OlinkID = rep("OID12345", 4),
    SampleType = rep("SAMPLE", 4),
    NPX = rnorm(4),
    PlateID = rep("plate1", 4),
    QC_Warning = rep("Pass", 4),
    LOD = rnorm(4)
  )
  col_names <- check_npx_col_names(df)
  result <- check_olinkid(df, col_names)

  expect_equal(result, character(0L))
})
