#' Test for check_olinkid function
#'

# input is not an arrow object
test_that("check_olinkid detects that input is not an arrow object", {
  df <- dplyr::tibble(X = c(1, 2, 3),
                      Y = c(4, 5, 6))
  expect_error(detect_data_type(df), "is not an R6 ArrowObject!")
})

test_that("check_olinkid handles missing OlinkID column", {
  # Create a data frame without the OlinkID column
  df <- dplyr::tibble(X = 1:5,
                      Y = letters[1:5]) |>
    arrow::as_arrow_table()

  # Test that the function produces an error when 'OlinkID' is missing
  expect_error(check_olinkid(df), "OlinkID column not present in the data.")
})

test_that("check_olinkid returns correct non_conforming_OID", {
  # Create a data frame with 'OlinkID' column
  df <- arrow::arrow_table(OlinkID =
                        c("OID12345",
                          "OID54321",
                          "OID1234",
                          "12345",
                          "OID123456"
                          ))

  # Test that the function returns the correct non_conforming_OID
  expect_warning(check_olinkid(df),
                 "Unrecognized Olink IDs detected: OID1234, 12345, and OID123456")
  result <- suppressWarnings(check_olinkid(df))
  expect_equal(result, c("OID1234",
                         "12345",
                         "OID123456"))
})
