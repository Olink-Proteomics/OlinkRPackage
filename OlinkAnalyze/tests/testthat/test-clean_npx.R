library(testthat)

# Test clean_assay_na -----------------------------------------------------

test_that(
  "clean_assay_na - message - no assays with only NA values",
  {
    df <- dplyr::tibble(
      SampleID = c("A", "B", "C", "D"),
      OlinkID = rep("OID12345", 4L),
      SampleType = rep("SAMPLE", 4L),
      NPX = rnorm(4L),
      PlateID = rep("plate1", 4L),
      QC_Warning = rep("Pass", 4L),
      LOD = rnorm(4L)
    )

    log <- suppressWarnings(check_npx(df))

    expect_message(
      object = clean_assay_na(df,
                              check_npx_log = log),
      regexp = "No assays with only NA values found. Returning original data frame."
    )
  }
)


test_that(
  "clean_assay_na - works - assays with only NA values",
  {
    df <- dplyr::tibble(
      SampleID = c("A", "B", "C", "D"),
      OlinkID = c(rep("OID11111", 2L), rep("OID22222", 2L)),
      SampleType = rep("SAMPLE", 4L),
      NPX = c(rep(1.0, 2L), rep(NA, 2L)),
      PlateID = rep("plate1", 4L),
      QC_Warning = rep("Pass", 4L),
      LOD = rep(1.0, 4L)
    )

    expected_result <- dplyr::tibble(
      SampleID = c("A", "B"),
      OlinkID = rep("OID11111", 2L),
      SampleType = rep("SAMPLE", 2L),
      NPX = rep(1.0, 2L),
      PlateID = rep("plate1", 2L),
      QC_Warning = rep("Pass", 2L),
      LOD = rep(1.0, 2L)
    )

    log <- suppressWarnings(check_npx(df))

    expect_equal(
      object = clean_assay_na(df,
                              check_npx_log = log),
      expected = expected_result
    )

  }
)



# clean_invalid_olid ------------------------------------------------------

test_that(
  "clean_invalid_oid - message - no invalid OlinkId",
  {
    df <- dplyr::tibble(
      SampleID = c("A", "B", "C", "D"),
      OlinkID = rep("OID12345", 4L),
      SampleType = rep("SAMPLE", 4L),
      NPX = rnorm(4L),
      PlateID = rep("plate1", 4L),
      QC_Warning = rep("Pass", 4L),
      LOD = rnorm(4L)
    )

    log <- suppressWarnings(check_npx(df))

    expect_message(
      object = clean_invalid_oid(df,
                                 check_npx_log = log),
      regexp = "No invalid OlinkIDs found. Returning original data frame."
    )
  }
)


test_that(
  "clean_invalid_oid - works - remove invalid OlinkId",
  {
    df <- dplyr::tibble(
      SampleID = c("A", "B", "C", "D"),
      OlinkID = c(rep("OID12345", 2L), rep("OID123456", 2L)),
      SampleType = rep("SAMPLE", 4L),
      NPX = c(rep(1.0, 2L), rep(2.0, 2L)),
      PlateID = rep("plate1", 4L),
      QC_Warning = rep("Pass", 4L),
      LOD = rep(1.0, 4L)
    )

    expected_result <- dplyr::tibble(
      SampleID = c("A", "B"),
      OlinkID = rep("OID12345", 2L),
      SampleType = rep("SAMPLE", 2L),
      NPX = rep(1.0, 2L),
      PlateID = rep("plate1", 2L),
      QC_Warning = rep("Pass", 2L),
      LOD = rep(1.0, 2L)
    )

    log <- suppressWarnings(check_npx(df))

    expect_equal(
      object = clean_invalid_oid(df,
                                 check_npx_log = log),
      expected = expected_result
    )

  }
)


# clean_duplicate_sample_id -----------------------------------------------

test_that(
  "clean_duplicate_sample_id - message - no duplicate sample id",
  {
    df <- dplyr::tibble(
      SampleID = c("A", "B", "C", "D"),
      OlinkID = rep("OID12345", 4L),
      SampleType = rep("SAMPLE", 4L),
      NPX = rnorm(4L),
      PlateID = rep("plate1", 4L),
      QC_Warning = rep("Pass", 4L),
      LOD = rnorm(4L)
    )

    log <- suppressWarnings(check_npx(df))

    expect_message(
      object = clean_duplicate_sample_id(df,
                                         check_npx_log = log),
      regexp = "No duplicate SampleIDs found. Returning original data frame."
    )
  }
)


test_that(
  "clean_duplicate_sample_id - works - remove duplicate sample id",
  {
    df <- dplyr::tibble(
      SampleID = c("A", "B", "C", "C"),
      OlinkID = c(rep("OID12345", 2L), rep("OID12345", 2L)),
      SampleType = rep("SAMPLE", 4L),
      NPX = c(rep(1.0, 2L), rep(2.0, 2L)),
      PlateID = rep("plate1", 4L),
      QC_Warning = rep("Pass", 4L),
      LOD = rep(1.0, 4L)
    )

    expected_result <- dplyr::tibble(
      SampleID = c("A", "B"),
      OlinkID = c(rep("OID12345", 2L)),
      SampleType = rep("SAMPLE", 2L),
      NPX = c(rep(1.0, 2L)),
      PlateID = rep("plate1", 2L),
      QC_Warning = rep("Pass", 2L),
      LOD = rep(1.0, 2L)
    )

    log <- suppressWarnings(check_npx(df))

    expect_equal(
      object = clean_duplicate_sample_id(df,
                                         check_npx_log = log),
      expected = expected_result
    )

  }
)


# clean_sample_type -------------------------------------------------------

test_that(
  "clean_sample_type - message - sample type is not available",
  {
    df <- dplyr::tibble(
      SampleID = c("A", "B", "C", "D"),
      OlinkID = rep("OID12345", 4L),
      NPX = rep(1, 4L),
      PlateID = rep("plate1", 4L),
      QC_Warning = rep("Pass", 4L),
      LOD = rep(1, 4L)
    )

    log <- suppressWarnings(check_npx(df))

    expect_equal(
      object = clean_sample_type(df,
                                 check_npx_log = log),
      expected = df
    )
  }
)

test_that(
  "clean_sample_type - works - remove control sample based on sample type",
  {
    df <- dplyr::tibble(
      SampleID = c("A", "B", "C", "D"),
      OlinkID = rep("OID12345", 4L),
      SampleType = c("SAMPLE",
                     "SAMPLE_CONTROL",
                     "PLATE_CONTROL",
                     "NEGATIVE_CONTROL"),
      NPX = rep(1, 4L),
      PlateID = rep("plate1", 4L),
      QC_Warning = rep("Pass", 4L),
      LOD = rep(1, 4L)
    )

    expected_result <- dplyr::tibble(
      SampleID = c("A"),
      OlinkID = rep("OID12345", 1L),
      SampleType = c("SAMPLE"),
      NPX = rep(1, 1L),
      PlateID = rep("plate1", 1L),
      QC_Warning = rep("Pass", 1L),
      LOD = rep(1, 1L)

    )

    log <- suppressWarnings(check_npx(df))

    expect_equal(
      object = clean_sample_type(df,
                                 check_npx_log = log),
      expected = expected_result
    )
  }
)


# clean_assay_type --------------------------------------------------------

test_that(
  "clean_assay_type - message - assay type is not available",
  {
    df <- dplyr::tibble(
      SampleID = c("A", "B", "C", "D"),
      OlinkID = rep("OID12345", 4L),
      NPX = rep(1, 4L),
      PlateID = rep("plate1", 4L),
      QC_Warning = rep("Pass", 4L),
      LOD = rep(1, 4L)
    )

    log <- suppressWarnings(check_npx(df))

    expect_equal(
      object = clean_assay_type(df,
                                check_npx_log = log),
      expected = df
    )
  }
)


test_that(
  "clean_assay_type - works - remove control assays",
  {
    df <- dplyr::tibble(
      SampleID = c("A", "B", "C", "D"),
      OlinkID = rep("OID12345", 4L),
      NPX = rep(1, 4L),
      PlateID = rep("plate1", 4L),
      QC_Warning = rep("Pass", 4L),
      LOD = rep(1, 4L),
      AssayType = c("assay",
                    "ext_ctrl",
                    "inc_ctrl",
                    "amp_ctrl")
    )

    expected_result <- dplyr::tibble(
      SampleID = c("A"),
      OlinkID = rep("OID12345", 1L),
      NPX = rep(1, 1L),
      PlateID = rep("plate1", 1L),
      QC_Warning = rep("Pass", 1L),
      LOD = rep(1, 1L),
      AssayType = c("assay")
    )

    log <- suppressWarnings(check_npx(df))

    expect_equal(
      object = clean_assay_type(df,
                                check_npx_log = log),
      expected = expected_result
    )
  }
)


# clean_sample_qc ---------------------------------------------------------

test_that(
  "clean_sample_qc - works - sample QC failed",
  {
    df <- dplyr::tibble(
      SampleID = c("A", "B", "C", "D"),
      OlinkID = rep("OID12345", 4L),
      NPX = rep(1, 4L),
      PlateID = rep("plate1", 4L),
      QC_Warning = c(rep("Pass", 3L), "FAIL"),
      LOD = rep(1, 4L)
    )

    expected_result <- dplyr::tibble(
      SampleID = c("A", "B", "C"),
      OlinkID = rep("OID12345", 3L),
      NPX = rep(1, 3L),
      PlateID = rep("plate1", 3L),
      QC_Warning = c(rep("Pass", 3L)),
      LOD = rep(1, 3L)
    )

    log <- suppressWarnings(check_npx(df))

    expect_equal(
      object = clean_sample_qc(df,
                               check_npx_log = log),
      expected = expected_result
    )
  }
)


# clean_control_sample_id -------------------------------------------------

test_that(
  "clean_control_sample_id - works - remove control sample",
  {
    df <- dplyr::tibble(
      SampleID = c("A", "B", "C", "control_D"),
      OlinkID = rep("OID12345", 4L),
      NPX = rep(1, 4L),
      PlateID = rep("plate1", 4L),
      QC_Warning = rep("Pass", 4L),
      LOD = rep(1, 4L)
    )

    expected_result <- dplyr::tibble(
      SampleID = c("A", "B", "C"),
      OlinkID = rep("OID12345", 3L),
      NPX = rep(1, 3L),
      PlateID = rep("plate1", 3L),
      QC_Warning = c(rep("Pass", 3L)),
      LOD = rep(1, 3L)
    )

    log <- suppressWarnings(check_npx(df))

    expect_equal(
      object = clean_control_sample_id(df,
                                       check_npx_log = log),
      expected = expected_result
    )
  }
)


# clean_npx ---------------------------------------------------------------

test_that(
  "clean_npx - works - remove samples/assays identified by check_npx",
  {

    df <- dplyr::tibble(
      SampleID = c("A", "B", "C", "D", "E", "F", "F", "control_F"),
      OlinkID = c("OID123456",
                  rep("OID12345", 3L),
                  rep("OID11111", 4L)),
      NPX = c(1, NA, NA, NA, 5, 6, 7, 8),
      PlateID = rep("plate1", 8L),
      SampleType = c(
        rep("SAMPLE", 5L),
        "SAMPLE_CONTROL",
        "PLATE_CONTROL",
        "NEGATIVE_CONTROL"
      ),
      AssayType = c(
        rep("assay", 3L),
        rep("ext_ctrl", 5L)
      ),
      QC_Warning = c(
        rep("FAIL", 3L),
        rep("PASS", 5L)
      )
    )

    expected_result <- dplyr::tibble(
      SampleID = character(),
      OlinkID = character(),
      NPX = numeric(),
      PlateID = character(),
      SampleType = character(),
      AssayType = character(),
      QC_Warning = character()
    )

    log <- suppressWarnings(check_npx(df))

    expect_equal(
      object = clean_npx(df, check_npx_log = log),
      expected = expected_result
    )


  }
)


