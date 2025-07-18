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
  NPX = replace(x = rnorm(n = 10L), list = 3L, values = NA_real_),
  PlateID = rep(x = "plate1", times = 10L),
  UniProt = rep(x = "uniprotid1", times = 10L),
  Assay = rep(x = "assay_a", times = 10L),
  Panel = rep(x = "panel_a", times = 10L),
  PanelVersion = rep(x = "panel_version_a", times = 10L),
  LOD = rnorm(n = 10L),
  ExtNPX = rnorm(n = 10L),
  Count = rnorm(n = 10L),
  Normalization = rep(x = "Intensity", times = 10L)
)

# Test clean_assay_na -----------------------------------------------------

test_that(
  "clean_assay_na - works - 1 assay with only NA values",
  {
    expected_result <- df |>
      dplyr::filter(
        .data[["SampleID"]] != "AllNA"
      )

    log <- check_npx(df = df) |>
      suppressWarnings() |>
      suppressMessages()

    ## verbose FALSE ----

    expect_message(
      object = expect_equal(
        object = clean_assay_na(df = df,
                                check_npx_log = log,
                                verbose = FALSE),
        expected = expected_result
      ),
      regexp = "Excluding 1 assay with only \"NA\" values: \"OID23456\""
    )

    ## verbose TRUE ----

    expect_message(
      object = expect_message(
        object = expect_equal(
          object = clean_assay_na(df = df,
                                  check_npx_log = log,
                                  verbose = TRUE),
          expected = expected_result
        ),
        regexp = "Excluding 1 assay with only \"NA\" values: \"OID23456\""
      ),
      regexp = "Removed assays with only \"NA\" values"
    )
  }
)

test_that(
  "clean_assay_na - works - no assay with NA values",
  {
    expected_result <- df |>
      dplyr::filter(
        .data[["SampleID"]] != "AllNA"
      )

    log <- check_npx(df = expected_result) |>
      suppressWarnings() |>
      suppressMessages()

    ## verbose FALSE ----

    expect_equal(
      object = clean_assay_na(df = expected_result,
                              check_npx_log = log,
                              verbose = FALSE),
      expected = expected_result
    )

    ## verbose TRUE ----

    expect_message(
      object = expect_equal(
        object = clean_assay_na(df = expected_result,
                                check_npx_log = log,
                                verbose = TRUE),
        expected = expected_result
      ),
      regexp = "No assays with only \"NA\" values."
    )
  }
)

# Test clean_invalid_oid --------------------------------------------------

test_that(
  "clean_invalid_oid - works - 1 invalid OlinkID",
  {
    expected_result <- df |>
      dplyr::filter(
        .data[["SampleID"]] != "InvalidOID"
      )

    log <- check_npx(df) |>
      suppressWarnings() |>
      suppressMessages()

    ## verbose = FALSE ----

    expect_message(
      object = expect_equal(
        object = clean_invalid_oid(df,
                                   check_npx_log = log,
                                   verbose = FALSE),
        expected = expected_result
      ),
      regexp = "Excluding 1 assay with invalid identifier: \"OID1234\"."
    )

    ## verbose = TRUE ----

    expect_message(
      object = expect_message(
        object = expect_equal(
          object = clean_invalid_oid(df,
                                     check_npx_log = log,
                                     verbose = TRUE),
          expected = expected_result
        ),
        regexp = "Excluding 1 assay with invalid identifier: \"OID1234\"."
      ),
      regexp = "Removed assays with invalid identifiers"
    )
  }
)

test_that(
  "clean_invalid_oid - works - no invalid OlinkID",
  {
    expected_result <- df |>
      dplyr::filter(
        .data[["SampleID"]] != "InvalidOID"
      )

    log <- check_npx(df = expected_result) |>
      suppressWarnings() |>
      suppressMessages()

    ## verbose = FALSE ----

    expect_equal(
      object = clean_invalid_oid(df = expected_result,
                                 check_npx_log = log,
                                 verbose = FALSE),
      expected = expected_result
    )

    ## verbose = TRUE ----

    expect_message(
      object = expect_equal(
        object = clean_invalid_oid(df = expected_result,
                                   check_npx_log = log,
                                   verbose = TRUE),
        expected = expected_result
      ),
      regexp = "No invalid assay identifiers."
    )
  }
)

# Test clean_duplicate_sample_id ------------------------------------------

test_that(
  "clean_duplicate_sample_id - works - 1 duplicate SampleID",
  {
    expected_result <- df |>
      dplyr::filter(
        .data[["SampleID"]] != "DuplicateSample"
      )

    log <- check_npx(df) |>
      suppressWarnings() |>
      suppressMessages()

    ## verbose = FALSE ----

    expect_message(
      object = expect_equal(
        object = clean_duplicate_sample_id(df,
                                           check_npx_log = log,
                                           verbose = FALSE),
        expected = expected_result
      ),
      regexp = paste("Excluding 1 sample with duplicate identifier:",
                     "\"DuplicateSample\"")
    )

    ## verbose = TRUE ----

    expect_message(
      object = expect_message(
        object = expect_equal(
          object = clean_duplicate_sample_id(df,
                                             check_npx_log = log,
                                             verbose = TRUE),
          expected = expected_result
        ),
        regexp = paste("Excluding 1 sample with duplicate identifier:",
                       "\"DuplicateSample\"")
      ),
      regexp = "Removed samples with duplicate identifiers."
    )
  }
)

test_that(
  "clean_duplicate_sample_id - works - no duplicate SampleID",
  {
    expected_result <- df |>
      dplyr::filter(
        .data[["SampleID"]] != "DuplicateSample"
      )

    log <- check_npx(df = expected_result) |>
      suppressWarnings() |>
      suppressMessages()

    ## verbose = FALSE ----

    expect_equal(
      object = clean_duplicate_sample_id(df = expected_result,
                                         check_npx_log = log,
                                         verbose = FALSE),
      expected = expected_result
    )

    ## verbose = TRUE ----

    expect_message(
      object = expect_equal(
        object = clean_duplicate_sample_id(df = expected_result,
                                           check_npx_log = log,
                                           verbose = TRUE),
        expected = expected_result
      ),
      regexp = "No duplicate sample identifiers."
    )
  }
)

# Test clean_sample_type --------------------------------------------------

test_that(
  "clean_sample_type - works - remove control sample based on sample type",
  {
    expected_result <- df |>
      dplyr::filter(
        .data[["SampleID"]] != "ControlType"
      )

    log <- check_npx(df) |>
      suppressWarnings() |>
      suppressMessages()

    ## vebose = FALSE ----

    expect_message(
      object = expect_equal(
        object = clean_sample_type(df,
                                   check_npx_log = log,
                                   keep_control_sample = FALSE,
                                   verbose = FALSE),
        expected = expected_result
      ),
      regexp = "Excluding 1 control sample: \"ControlType\"."
    )

    ## vebose = TRUE ----

    expect_message(
      object = expect_message(
        object = expect_equal(
          object = clean_sample_type(df,
                                     check_npx_log = log,
                                     keep_control_sample = FALSE,
                                     verbose = TRUE),
          expected = expected_result
        ),
        regexp = "Excluding 1 control sample: \"ControlType\"."
      ),
      regexp = paste("Removed control samples marked as \"SAMPLE_CONTROL\",",
                     "\"PLATE_CONTROL\", and \"NEGATIVE_CONTROL\".")
    )
  }
)

test_that(
  "clean_sample_type - works - do not remove control samples",
  {
    log <- check_npx(df) |>
      suppressWarnings() |>
      suppressMessages()

    ## verbose = FALSE ----

    expect_equal(
      object = clean_sample_type(df = df,
                                 check_npx_log = log,
                                 keep_control_sample = TRUE,
                                 verbose = FALSE),
      expected = df
    )

    ## verbose = TRUE ----

    expect_message(
      object = expect_equal(
        object = clean_sample_type(df = df,
                                   check_npx_log = log,
                                   keep_control_sample = TRUE,
                                   verbose = TRUE),
        expected = df
      ),
      regexp = "Skipping exclusion of control samples as per user input"
    )
  }
)

test_that(
  "clean_sample_type - works - sample_type is not available",
  {
    test_df <- df |>
      dplyr::select(
        -dplyr::all_of("SampleType")
      )

    log <- check_npx(test_df) |>
      suppressWarnings() |>
      suppressMessages()

    expect_message(
      object = expect_equal(
        object = clean_sample_type(df = test_df,
                                   check_npx_log = log,
                                   keep_control_sample = FALSE),
        expected = test_df
      ),
      regexp = paste("No column marking control samples in dataset.")
    )
  }
)

# Test clean_assay_type ---------------------------------------------------

test_that(
  "clean_assay_type - works - remove control assays",
  {
    expected_result <- df |>
      dplyr::filter(
        .data[["SampleID"]] != "ControlAssay"
      )

    log <- check_npx(df) |>
      suppressWarnings() |>
      suppressMessages()

    ## verbose = FALSE ----

    expect_message(
      object = expect_equal(
        object = clean_assay_type(df,
                                  check_npx_log = log,
                                  keep_control_assay = FALSE,
                                  verbose = FALSE),
        expected = expected_result
      ),
      regexp = "Excluding 1 control assay: \"OID78901\"."
    )

    ## verbose = TRUE ----

    expect_message(
      object = expect_message(
        object = expect_equal(
          object = clean_assay_type(df,
                                    check_npx_log = log,
                                    keep_control_assay = FALSE,
                                    verbose = TRUE),
          expected = expected_result
        ),
        regexp = "Excluding 1 control assay: \"OID78901\"."
      ),
      regexp = paste("Removed control assays marked as \"ext_ctrl\",",
                     "\"inc_ctrl\", and \"amp_ctrl\".")
    )
  }
)

test_that(
  "clean_assay_type - works - do not remove control assays",
  {
    log <- check_npx(df) |>
      suppressWarnings() |>
      suppressMessages()

    ## verbose = FALSE ----

    expect_equal(
      object = clean_assay_type(df,
                                check_npx_log = log,
                                keep_control_assay = TRUE,
                                verbose = FALSE),
      expected = df
    )

    ## verbose = TRUE ----

    expect_message(
      object = expect_equal(
        object = clean_assay_type(df,
                                  check_npx_log = log,
                                  keep_control_assay = TRUE,
                                  verbose = TRUE),
        expected = df
      ),
      regexp = "Skipping exclusion of control assays as per user input"
    )
  }
)

test_that(
  "clean_assay_type - assay_type is not available",
  {
    test_df <- df |>
      dplyr::select(
        -dplyr::all_of("AssayType")
      )

    log <- check_npx(test_df) |>
      suppressWarnings() |>
      suppressMessages()

    expect_message(
      object = expect_equal(
        object = clean_assay_type(test_df,
                                  check_npx_log = log,
                                  keep_control_assay = FALSE),
        expected = test_df
      ),
      regexp = "No column marking control assays in dataset."
    )
  }
)

# Test clean_qc_warning ---------------------------------------------------

test_that(
  "clean_qc_warning - works - sample QC failed",
  {
    expected_result <- df |>
      dplyr::filter(
        .data[["SampleID"]] != "FailQC"
      )

    log <- check_npx(df) |>
      suppressWarnings() |>
      suppressMessages()

    expect_message(
      object = expect_equal(
        object = clean_qc_warning(df,
                                  check_npx_log = log),
        expected = expected_result
      ),
      regexp = "Samples flaged SampleQC = \"'FAIL'\" Removed."
    )
  }
)

test_that(
  "clean_qc_warning - The check_npx() function requires the qc_warning
  column. In this test, we verify the edge case where qc_warning column
  is removed after running check_npx().",

  {
    log <- check_npx(df) |>
      suppressWarnings() |>
      suppressMessages()

    # SampleQC column was removed after running check_npx()
    test_df <- df |>
      dplyr::select(!SampleQC)

    expect_message(
      object = expect_equal(
        object = clean_qc_warning(test_df,
                                  check_npx_log = log),
        expected = test_df
      ),
      regexp = "SampleQC is not found in data table."
    )
  }
)


# Test clean_assay_warning ------------------------------------------------

test_that(
  "clean_assay_warning - works - assays flagged with assay warning",
  {
    expected_result <- df |>
      dplyr::filter(
        .data[["SampleID"]] != "AssayWarn"
      )

    log <- check_npx(df) |>
      suppressWarnings() |>
      suppressMessages()

    expect_message(
      object = expect_equal(
        object = clean_assay_warning(df,
                                     check_npx_log = log),
        expected = expected_result
      ),
      regexp = "Removing assays where AssayQC contains a warning flag"
    )
  }
)

test_that(
  "clean_assay_warning  - `assay_warn` is not exist.",
  {

    test_df <- df |>
      dplyr::select(!AssayQC)

    log <- check_npx(test_df) |>
      suppressWarnings() |>
      suppressMessages()

    expect_message(
      object = expect_equal(
        object = clean_assay_warning(test_df,
                                     check_npx_log = log),
        expected = test_df
      ),
      regexp = paste("No column name found for `assay_warn` in",
                     "`check_npx_log\\$col_names`\\.")
    )
  }
)

# Test clean_control_sample_id --------------------------------------------

test_that(
  "clean_control_sample_id - works - remove control sample",
  {
    expected_result <- df |>
      dplyr::filter(
        .data[["SampleID"]] != "ControlID"
      )

    log <- check_npx(df) |>
      suppressWarnings() |>
      suppressMessages()

    expect_message(
      object = expect_equal(
        object = clean_control_sample_id(df,
                                         check_npx_log = log,
                                         control_sample_ids = c("ControlID")),
        expected = expected_result
      ),
      regexp = "Control sample: \"ControlID\" removed."
    )
  }
)

test_that(
  "clean_control_sample_id - No control sample IDs provided.",
  {
    log <- check_npx(df) |>
      suppressWarnings() |>
      suppressMessages()

    expect_message(
      object = expect_equal(
        object = clean_control_sample_id(df,
                                         check_npx_log = log,
                                         control_sample_ids = NULL),
        expected = df
      ),
      regexp = "No control sample IDs provided."
    )
  }
)

test_that(
  "clean_control_sample_id - The check_npx() function requires the SampleID
  column. In this test, we verify the edge case where SampleID is removed after
  running check_npx().",
  {

    log <- check_npx(df) |>
      suppressWarnings() |>
      suppressMessages()

    # SampleID column was removed after running check_npx()
    test_df <- df |>
      dplyr::select(!SampleID)

    expect_message(object = expect_equal(
      object = clean_control_sample_id(test_df,
                                       check_npx_log = log,
                                       control_sample_ids = c("ControlID")),
      expected = test_df
    ),
    regexp = "SampleID is missing from the data table.")
  }
)

# Test clean_col_class ----------------------------------------------------

test_that(
  "clean_col_class - works - correct column class",
  {
    test_df <- OlinkAnalyze::npx_data1 |>
      dplyr::mutate(
        NPX = as.character(.data[["NPX"]]),
        LOD = as.character(.data[["LOD"]])
      )

    log <- check_npx(test_df) |>
      suppressWarnings() |>
      suppressMessages()

    expect_message(
      object = expect_equal(
        object = clean_col_class(df = test_df,
                                 check_npx_log = log),
        expected = OlinkAnalyze::npx_data1
      ),
      regexp = "Corrected column classes for: NPX and LOD"
    )
  }
)

test_that(
  "clean_col_class - return data unchanged",
  {
    test_df <- OlinkAnalyze::npx_data1

    log <- check_npx(test_df) |>
      suppressWarnings() |>
      suppressMessages()

    expect_message(
      object = expect_equal(
        object = clean_col_class(df = test_df,
                                 check_npx_log = log),
        expected = OlinkAnalyze::npx_data1
      ),
      regexp = paste("`lod`, `quant`, `ext_npx`, and `count` passed the",
                     "column class check.")
    )
  }
)

test_that("clean_col_class correctly coerces columns via coerce_col()", {
  # Simulate a data frame with incorrect types
  test_df <- tibble::tibble(
    col_chr = 123,        # should be character
    col_num = "456",       # should be numeric
    col_logic = TRUE      # remain the same
  )

  # Simulate the check_npx_log$col_class
  check_log <- list(
    col_class = tibble::tibble(
      col_name = c("col_chr", "col_num", "col_logic"),
      expected_col_class = c("character", "numeric", "logical")
    )
  )

  # Apply the function
  cleaned_df <- clean_col_class(test_df, check_log) |>
    suppressWarnings() |>
    suppressMessages()

  # Check types
  expect_type(cleaned_df$col_chr, "character")
  expect_type(cleaned_df$col_num, "double")
  expect_type(cleaned_df$col_logic, "logical")

  # Check values
  expect_equal(cleaned_df$col_chr, "123")
  expect_equal(cleaned_df$col_num, 456)
  expect_equal(cleaned_df$col_logic, TRUE)
})

# Test clean_npx ----------------------------------------------------------

test_that(
  "clean_npx - works - remove samples/assays identified by check_npx",
  {
    expected_result <- df |>
      dplyr::filter(
        .data[["SampleID"]] == "ValidSample"
      )

    log <- check_npx(df) |>
      suppressWarnings() |>
      suppressMessages()

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

test_that("clean_npx emits clean messages without ANSI styling", {

  # Set CLI color option locally for this test
  withr::local_options(cli.num_colors = 1)

  # Expected result
  expected_result <- df |>
    dplyr::filter(
      .data[["SampleID"]] == "ValidSample"
    )

  # Minimal fake check_log with required structure
  log <- check_npx(df) |>
    suppressWarnings() |>
    suppressMessages()


  msgs <- capture_messages({
    clean_npx(df, check_log = log, verbose = TRUE)
  })

  # Drop empty or whitespace-only lines
  msgs_clean <- msgs |>
    stringr::str_replace_all("\n", "") |>
    stringr::str_replace_all("^\\s*[\\u2500-]+\\s*|\\s*[\\u2500-]+\\s*$", "") |>
    stringr::str_trim()
  msgs_clean <- msgs_clean[msgs_clean != ""]


  # Validate processing message
  expect_true(grepl("Starting `clean_npx\\(\\)` pipeline",
                    msgs_clean[1L]))
  expect_true(grepl("Cleaning assays with invalid OlinkIDs",
                    msgs_clean[2L]))
  expect_true(grepl("Excluding 1 assay with invalid identifier: \"OID1234\"",
                    msgs_clean[3L]))
  expect_true(grepl("Removed assays with invalid identifiers",
                    msgs_clean[4L]))
  expect_true(grepl("Cleaning assays with all NA values",
                    msgs_clean[5L]))
  expect_true(grepl("Excluding 1 assay with only \"NA\" values: \"OID23456\"",
                    msgs_clean[6L]))
  expect_true(grepl("Removed assays with only \"NA\" values",
                    msgs_clean[7L]))
  expect_true(grepl("Cleaning duplicate SampleIDs",
                    msgs_clean[8L]))
  expect_true(grepl(paste("Excluding 1 sample with duplicate identifier:",
                          "\"DuplicateSample\""),
                    msgs_clean[9L]))
  expect_true(grepl("Removed samples with duplicate identifiers",
                    msgs_clean[10L]))
  expect_true(grepl("Cleaning control samples based on sample type",
                    msgs_clean[11L]))
  expect_true(grepl("Excluding 1 control sample: \"ControlType\"",
                    msgs_clean[12L]))
  expect_true(grepl(paste("Removed control samples marked as",
                          "\"SAMPLE_CONTROL\", \"PLATE_CONTROL\",",
                          "and \"NEGATIVE_CONTROL\""),
                    msgs_clean[13L]))
  expect_true(grepl("Cleaning control samples based on Sample ID",
                    msgs_clean[14L]))
  expect_true(grepl("No control sample IDs provided",
                    msgs_clean[15L]))
  expect_true(grepl("Cleaning Samples with QC Status 'FAIL'",
                    msgs_clean[16L]))
  expect_true(grepl("Samples flaged SampleQC = .* Removed",
                    msgs_clean[17L]))
  expect_true(grepl("Cleaning internal control assays",
                    msgs_clean[18L]))
  expect_true(grepl("Excluding 1 control assay: \"OID78901\"",
                    msgs_clean[19L]))
  expect_true(grepl(paste("Removed control assays marked as \"ext_ctrl\",",
                          "\"inc_ctrl\", and \"amp_ctrl\""),
                    msgs_clean[20L]))
  expect_true(grepl("Cleaning assays flagged by assay warning",
                    msgs_clean[21L]))
  expect_true(grepl("Removing assays where AssayQC contains a warning flag",
                    msgs_clean[22L]))
  expect_true(grepl("Correcting flagged column class",
                    msgs_clean[23L]))
  expect_true(grepl(paste("Corrected column classes for: .* Returning cleaned",
                          "data table"),
                    msgs_clean[24L]))
  expect_true(grepl("Completed `clean_npx\\(\\)`",
                    msgs_clean[25L]))

})
