library(testthat)


# Create Test Data Table --------------------------------------------------
df <- dplyr::tibble(
  SampleID = c(
    "ValidSample",     # valid
    "InvalidOID",      # invalid OlinkID (too short)
    "AllNA",           # all NPX values NA for assay
    "DuplicateSample", # duplicate SampleID
    "ControlType",     # control SampleType
    "ControlID",       # control SampleID (e.g., contains 'control')
    "FailQC",          # QC_Warning is FAIL
    "ControlAssay",    # internal control assay
    "AssayWarn",       # flagged by AssayQC warning
    "DuplicateSample"  # duplicate SampleID
  ),
  OlinkID = c(
    "OID12345",  # valid (5 digits)
    "OID1234",   # invalid (only 4 digits)
    "OID23456",  # valid, but will be all NA
    "OID34567",  # valid
    "OID45678",  # valid
    "OID56789",  # valid
    "OID67890",  # valid
    "OID78901",  # valid
    "OID89012",  # valid
    "OID34567"   # valid
  ),
  NPX = c(
    10,   # valid
    11,   # still gets removed due to invalid OlinkID
    NA,   # all NA for assay
    12,   # duplicate SampleID ("DuplicateSample")
    13,   # control sample type
    14,   # control SampleID
    15,   # fails QC
    16,   # internal control assay
    17,   # flagged by assay warning
    12    # duplicate SampleID ("DuplicateSample")
  ),
  SampleType = c(
    "SAMPLE",
    "SAMPLE",
    "SAMPLE",
    "SAMPLE",
    "PLATE_CONTROL",  # control sample
    "SAMPLE",
    "SAMPLE",
    "SAMPLE",
    "SAMPLE",
    "SAMPLE"
  ),
  AssayType = c(
    "assay",
    "assay",
    "assay",
    "assay",
    "assay",
    "assay",
    "assay",
    "ext_ctrl",   #control assay
    "assay",
    "assay"
  ),
  SampleQC = c(
    "PASS",
    "PASS",
    "PASS",
    "PASS",
    "PASS",
    "PASS",
    "FAIL",    #fails QC
    "PASS",
    "PASS",
    "PASS"
  ),
  AssayQC = c(
    "PASS",
    "PASS",
    "PASS",
    "PASS",
    "PASS",
    "PASS",
    "PASS",
    "PASS",
    "WARN", # assay qc warning flag
    "PASS"
  ),
  PlateID = rep("plate1", 10L),
  UniProt = rep("uniprotid1", 10L),
  Assay = rep("assay_a", 10L),
  Panel = rep("panel_a", 10L),
  PanelVersion = rep("panel_version_a", 10L),
  LOD = rnorm(10L),
  ExtNPX = rnorm(10L),
  Count = rnorm(10L),
  Normalization = rep("Intensity", 10L)
)



# Test clean_assay_na -----------------------------------------------------
test_that(
  "clean_assay_na - works - assays with only NA values",
  {
    expected_result <- df |>
      dplyr::filter(!SampleID == "AllNA")

    log <- suppressWarnings(check_npx(df))

    expect_equal(
      object = clean_assay_na(df,
                              check_npx_log = log),
      expected = expected_result
    )

  }
)



# Test clean_invalid_oid --------------------------------------------------
test_that(
  "clean_invalid_oid - works - remove invalid OlinkId",
  {
    expected_result <- df |>
      dplyr::filter(!SampleID == "InvalidOID")

    log <- suppressWarnings(check_npx(df))

    expect_equal(
      object = clean_invalid_oid(df,
                                 check_npx_log = log),
      expected = expected_result
    )

  }
)



# Test clean_duplicate_sample_id ------------------------------------------
test_that(
  "clean_duplicate_sample_id - works - remove duplicate sample id",
  {
    expected_result <- df |>
      dplyr::filter(!SampleID == "DuplicateSample")

    log <- suppressWarnings(check_npx(df))

    expect_equal(
      object = clean_duplicate_sample_id(df,
                                         check_npx_log = log),
      expected = expected_result
    )

  }
)



# Test clean_sample_type --------------------------------------------------
test_that(
  "clean_sample_type - works - remove control sample based on sample type",
  {
    expected_result <- df |>
      dplyr::filter(!SampleID == "ControlType")

    log <- suppressWarnings(check_npx(df))

    expect_equal(
      object = clean_sample_type(df,
                                 check_npx_log = log,
                                 keep_control_sample = FALSE),
      expected = expected_result
    )
  }
)



# Test clean_assay_type ---------------------------------------------------
test_that(
  "clean_assay_type - works - remove control assays",
  {
    expected_result <- df |>
      dplyr::filter(!SampleID == "ControlAssay")

    log <- suppressWarnings(check_npx(df))

    expect_equal(
      object = clean_assay_type(df,
                                check_npx_log = log,
                                keep_control_assay = FALSE),
      expected = expected_result
    )
  }
)



# Test clean_qc_warning ---------------------------------------------------
test_that(
  "clean_qc_warning - works - sample QC failed",
  {

    expected_result <- df |>
      dplyr::filter(!SampleID == "FailQC")

    log <- suppressWarnings(check_npx(df))

    expect_equal(
      object = clean_qc_warning(df,
                                check_npx_log = log),
      expected = expected_result
    )
  }
)



# Test clean_assay_warning ------------------------------------------------
test_that(
  "clean_assay_warning - works - assays flagged with assay warning",
  {

    expected_result <- df |>
      dplyr::filter(!SampleID == "AssayWarn")

    log <- suppressWarnings(check_npx(df))

    expect_equal(
      object = clean_assay_warning(df,
                                   check_npx_log = log),
      expected = expected_result
    )
  }
)



# Test clean_control_sample_id --------------------------------------------
test_that(
  "clean_control_sample_id - works - remove control sample",
  {
    expected_result <- df |>
      dplyr::filter(!SampleID == "ControlID")

    log <- suppressWarnings(check_npx(df))

    expect_equal(
      object = clean_control_sample_id(df,
                                       check_npx_log = log,
                                       control_sample_ids = c("ControlID")),
      expected = expected_result
    )
  }
)



# Test clean_npx ----------------------------------------------------------
test_that(
  "clean_npx - works - remove samples/assays identified by check_npx",
  {
    expected_result <- df |>
      dplyr::filter(SampleID == "ValidSample")

    log <- suppressWarnings(check_npx(df))

    expect_equal(
      object = clean_npx(df,
                         check_log = log,
                         keep_controls = NULL,
                         control_sample_ids = c("ControlID"),
                         verbose = TRUE),
      expected = expected_result
    )


  }
)



# Test clean_col_class ----------------------------------------------------
test_that(
  "clean_col_class - works - correct column class",
  {
    test_df <- OlinkAnalyze::npx_data1 |>
      dplyr::mutate(NPX = as.character(NPX),
                    LOD = as.character(LOD))

    log <- suppressWarnings(check_npx(test_df))

    expect_equal(
      object = clean_col_class(test_df,
                               check_npx_log = log),
      expected = OlinkAnalyze::npx_data1
    )
  }
)
