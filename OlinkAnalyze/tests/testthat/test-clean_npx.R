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

df_arrow <- arrow::as_arrow_table(x = df)

log <- check_npx(df = df) |>
  suppressWarnings() |>
  suppressMessages()

# Test clean_assay_na -----------------------------------------------------

test_that(
  "clean_assay_na - works - 1 assay with only NA values",
  {
    expected_result <- df |>
      dplyr::filter(
        .data[["SampleID"]] != "AllNA"
      )

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
  "clean_assay_na - works - arrow - 1 assay with only NA values",
  {
    expected_result <- df_arrow |>
      dplyr::filter(
        .data[["SampleID"]] != "AllNA"
      ) |>
      dplyr::collect()

    ## verbose FALSE ----

    expect_message(
      object = expect_equal(
        object = clean_assay_na(df = df_arrow,
                                check_npx_log = log,
                                verbose = FALSE) |>
          dplyr::collect(),
        expected = expected_result
      ),
      regexp = "Excluding 1 assay with only \"NA\" values: \"OID23456\""
    )

    ## verbose TRUE ----

    expect_message(
      object = expect_message(
        object = expect_equal(
          object = clean_assay_na(df = df_arrow,
                                  check_npx_log = log,
                                  verbose = TRUE) |>
            dplyr::collect(),
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

    log_exp <- check_npx(df = expected_result) |>
      suppressWarnings() |>
      suppressMessages()

    ## verbose FALSE ----

    expect_equal(
      object = clean_assay_na(df = expected_result,
                              check_npx_log = log_exp,
                              verbose = FALSE),
      expected = expected_result
    )

    ## verbose TRUE ----

    expect_message(
      object = expect_equal(
        object = clean_assay_na(df = expected_result,
                                check_npx_log = log_exp,
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

    ## verbose = FALSE ----

    expect_message(
      object = expect_equal(
        object = clean_invalid_oid(df = df,
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
          object = clean_invalid_oid(df = df,
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
  "clean_invalid_oid - works - arrow - 1 invalid OlinkID",
  {
    expected_result <- df_arrow |>
      dplyr::filter(
        .data[["SampleID"]] != "InvalidOID"
      ) |>
      dplyr::collect()

    ## verbose = FALSE ----

    expect_message(
      object = expect_equal(
        object = clean_invalid_oid(df = df_arrow,
                                   check_npx_log = log,
                                   verbose = FALSE) |>
          dplyr::collect(),
        expected = expected_result
      ),
      regexp = "Excluding 1 assay with invalid identifier: \"OID1234\"."
    )

    ## verbose = TRUE ----

    expect_message(
      object = expect_message(
        object = expect_equal(
          object = clean_invalid_oid(df = df_arrow,
                                     check_npx_log = log,
                                     verbose = TRUE) |>
            dplyr::collect(),
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

    log_exp <- check_npx(df = expected_result) |>
      suppressWarnings() |>
      suppressMessages()

    ## verbose = FALSE ----

    expect_equal(
      object = clean_invalid_oid(df = expected_result,
                                 check_npx_log = log_exp,
                                 verbose = FALSE),
      expected = expected_result
    )

    ## verbose = TRUE ----

    expect_message(
      object = expect_equal(
        object = clean_invalid_oid(df = expected_result,
                                   check_npx_log = log_exp,
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

    ## verbose = FALSE ----

    expect_message(
      object = expect_equal(
        object = clean_duplicate_sample_id(df = df,
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
          object = clean_duplicate_sample_id(df = df,
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
  "clean_duplicate_sample_id - works - arrow - 1 duplicate SampleID",
  {
    expected_result <- df_arrow |>
      dplyr::filter(
        .data[["SampleID"]] != "DuplicateSample"
      ) |>
      dplyr::collect()

    ## verbose = FALSE ----

    expect_message(
      object = expect_equal(
        object = clean_duplicate_sample_id(df = df_arrow,
                                           check_npx_log = log,
                                           verbose = FALSE) |>
          dplyr::collect(),
        expected = expected_result
      ),
      regexp = paste("Excluding 1 sample with duplicate identifier:",
                     "\"DuplicateSample\"")
    )

    ## verbose = TRUE ----

    expect_message(
      object = expect_message(
        object = expect_equal(
          object = clean_duplicate_sample_id(df = df_arrow,
                                             check_npx_log = log,
                                             verbose = TRUE) |>
            dplyr::collect(),
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

    log_exp <- check_npx(df = expected_result) |>
      suppressWarnings() |>
      suppressMessages()

    ## verbose = FALSE ----

    expect_equal(
      object = clean_duplicate_sample_id(df = expected_result,
                                         check_npx_log = log_exp,
                                         verbose = FALSE),
      expected = expected_result
    )

    ## verbose = TRUE ----

    expect_message(
      object = expect_equal(
        object = clean_duplicate_sample_id(df = expected_result,
                                           check_npx_log = log_exp,
                                           verbose = TRUE),
        expected = expected_result
      ),
      regexp = "No duplicate sample identifiers."
    )
  }
)

# Test clean_sample_type --------------------------------------------------

test_that(
  "clean_sample_type - works - remove control sample on sample type",
  {
    expected_result <- df |>
      dplyr::filter(
        .data[["SampleID"]] != "ControlType"
      )

    ## vebose = FALSE ----

    expect_message(
      object = expect_equal(
        object = clean_sample_type(df = df,
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
          object = clean_sample_type(df = df,
                                     check_npx_log = log,
                                     keep_control_sample = FALSE,
                                     verbose = TRUE),
          expected = expected_result
        ),
        regexp = "Excluding 1 control sample: \"ControlType\"."
      ),
      regexp = olink_sample_types[
        !(names(olink_sample_types) %in% c("sample"))
      ] |>
        unlist() |>
        unname() |>
        ansi_collapse_quot() |>
        (\(x) paste0("Removed control samples marked as ", x, "."))()
    )
  }
)

test_that(
  "clean_sample_type - works - arrow - remove control sample on sample type",
  {
    expected_result <- df_arrow |>
      dplyr::filter(
        .data[["SampleID"]] != "ControlType"
      ) |>
      dplyr::collect()

    ## vebose = FALSE ----

    expect_message(
      object = expect_equal(
        object = clean_sample_type(df = df_arrow,
                                   check_npx_log = log,
                                   keep_control_sample = FALSE,
                                   verbose = FALSE) |>
          dplyr::collect(),
        expected = expected_result
      ),
      regexp = "Excluding 1 control sample: \"ControlType\"."
    )

    ## vebose = TRUE ----

    expect_message(
      object = expect_message(
        object = expect_equal(
          object = clean_sample_type(df = df_arrow,
                                     check_npx_log = log,
                                     keep_control_sample = FALSE,
                                     verbose = TRUE) |>
            dplyr::collect(),
          expected = expected_result
        ),
        regexp = "Excluding 1 control sample: \"ControlType\"."
      ),
      regexp = olink_sample_types[
        !(names(olink_sample_types) %in% c("sample"))
      ] |>
        unlist() |>
        unname() |>
        ansi_collapse_quot() |>
        (\(x) paste0("Removed control samples marked as ", x, "."))()
    )
  }
)

test_that(
  "clean_sample_type - works - do not remove control samples",
  {
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

    log_test <- check_npx(df = test_df) |>
      suppressWarnings() |>
      suppressMessages()

    expect_message(
      object = expect_equal(
        object = clean_sample_type(df = test_df,
                                   check_npx_log = log_test,
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

    ## verbose = FALSE ----

    expect_message(
      object = expect_equal(
        object = clean_assay_type(df = df,
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
          object = clean_assay_type(df = df,
                                    check_npx_log = log,
                                    keep_control_assay = FALSE,
                                    verbose = TRUE),
          expected = expected_result
        ),
        regexp = "Excluding 1 control assay: \"OID78901\"."
      ),
      regexp = olink_assay_types[
        !(names(olink_assay_types) %in% c("assay"))
      ] |>
        unlist() |>
        unname() |>
        ansi_collapse_quot() |>
        (\(x) paste0("Removed control assays marked as ", x, "."))()
    )
  }
)

test_that(
  "clean_assay_type - works - arrow - remove control assays",
  {
    expected_result <- df_arrow |>
      dplyr::filter(
        .data[["SampleID"]] != "ControlAssay"
      ) |>
      dplyr::collect()

    ## verbose = FALSE ----

    expect_message(
      object = expect_equal(
        object = clean_assay_type(df = df,
                                  check_npx_log = log,
                                  keep_control_assay = FALSE,
                                  verbose = FALSE) |>
          dplyr::collect(),
        expected = expected_result
      ),
      regexp = "Excluding 1 control assay: \"OID78901\"."
    )

    ## verbose = TRUE ----

    expect_message(
      object = expect_message(
        object = expect_equal(
          object = clean_assay_type(df = df,
                                    check_npx_log = log,
                                    keep_control_assay = FALSE,
                                    verbose = TRUE) |>
            dplyr::collect(),
          expected = expected_result
        ),
        regexp = "Excluding 1 control assay: \"OID78901\"."
      ),
      regexp = olink_assay_types[
        !(names(olink_assay_types) %in% c("assay"))
      ] |>
        unlist() |>
        unname() |>
        ansi_collapse_quot() |>
        (\(x) paste0("Removed control assays marked as ", x, "."))()
    )
  }
)

test_that(
  "clean_assay_type - works - do not remove control assays",
  {
    ## verbose = FALSE ----

    expect_equal(
      object = clean_assay_type(df = df,
                                check_npx_log = log,
                                keep_control_assay = TRUE,
                                verbose = FALSE),
      expected = df
    )

    ## verbose = TRUE ----

    expect_message(
      object = expect_equal(
        object = clean_assay_type(df = df,
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

    log_test <- check_npx(test_df) |>
      suppressWarnings() |>
      suppressMessages()

    expect_message(
      object = expect_equal(
        object = clean_assay_type(df = test_df,
                                  check_npx_log = log_test,
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

    ## verbose = FALSE ----

    expect_message(
      object = expect_equal(
        object = clean_qc_warning(df = df,
                                  check_npx_log = log,
                                  verbose = FALSE),
        expected = expected_result
      ),
      regexp = paste("Excluding 1 datapoint from 1 sample flagged with",
                     "SampleQC = \"FAIL\": \"FailQC\".")
    )

    ## verbose = TRUE ----

    expect_message(
      object = expect_equal(
        object = clean_qc_warning(df = df,
                                  check_npx_log = log,
                                  verbose = TRUE),
        expected = expected_result
      ),
      regexp = paste("Excluding 1 datapoint from 1 sample flagged with",
                     "SampleQC = \"FAIL\": \"FailQC\".")
    )
  }
)

test_that(
  "clean_qc_warning - works - arrow - sample QC failed",
  {
    expected_result <- df_arrow |>
      dplyr::filter(
        .data[["SampleID"]] != "FailQC"
      ) |>
      dplyr::collect()

    ## verbose = FALSE ----

    expect_message(
      object = expect_equal(
        object = clean_qc_warning(df = df_arrow,
                                  check_npx_log = log,
                                  verbose = FALSE) |>
          dplyr::collect(),
        expected = expected_result
      ),
      regexp = paste("Excluding 1 datapoint from 1 sample flagged with",
                     "SampleQC = \"FAIL\": \"FailQC\".")
    )

    ## verbose = TRUE ----

    expect_message(
      object = expect_equal(
        object = clean_qc_warning(df = df_arrow,
                                  check_npx_log = log,
                                  verbose = TRUE) |>
          dplyr::collect(),
        expected = expected_result
      ),
      regexp = paste("Excluding 1 datapoint from 1 sample flagged with",
                     "SampleQC = \"FAIL\": \"FailQC\".")
    )
  }
)

test_that(
  "clean_qc_warning - works - sample QC failed with pluralization",
  {
    expected_result <- df |>
      dplyr::filter(
        .data[["SampleID"]] != "FailQC"
      )

    ## 2 datapoints, 1 sample ----

    expect_message(
      object = expect_equal(
        object = clean_qc_warning(
          df = df |>
            dplyr::bind_rows(
              dplyr::tibble(
                "SampleID" = "FailQC",
                "OlinkID" = "OID12345",
                "SampleType" = "SAMPLE",
                "AssayType" = "assay",
                "SampleQC" = "FAIL",
                "AssayQC" = "PASS",
                "NPX" = -1L,
                "PlateID" = "plate1",
                "UniProt" = "uniprotid1",
                "Assay" = "assay_a",
                "Panel" = "panel_a",
                "PanelVersion" = "panel_version_a",
                "LOD" = -1L,
                "ExtNPX" = -1L,
                "Count" = -1L,
                "Normalization" = "Intensity"
              )
            ),
          check_npx_log = log,
          verbose = FALSE
        ),
        expected = expected_result
      ),
      regexp = paste("Excluding 2 datapoints from 1 sample flagged with",
                     "SampleQC = \"FAIL\": \"FailQC\".")
    )

    ## 2 datapoints, 2 samples ----

    expect_message(
      object = expect_equal(
        object = clean_qc_warning(
          df = df |>
            dplyr::bind_rows(
              dplyr::tibble(
                "SampleID" = "InvalidOID",
                "OlinkID" = "OID12345",
                "SampleType" = "SAMPLE",
                "AssayType" = "assay",
                "SampleQC" = "FAIL",
                "AssayQC" = "PASS",
                "NPX" = -1L,
                "PlateID" = "plate1",
                "UniProt" = "uniprotid1",
                "Assay" = "assay_a",
                "Panel" = "panel_a",
                "PanelVersion" = "panel_version_a",
                "LOD" = -1L,
                "ExtNPX" = -1L,
                "Count" = -1L,
                "Normalization" = "Intensity"
              )
            ),
          check_npx_log = log,
          verbose = FALSE
        ),
        expected = expected_result
      ),
      regexp = paste("Excluding 2 datapoints from 2 samples flagged with",
                     "SampleQC = \"FAIL\": \"FailQC\".")
    )
  }
)

test_that(
  "clean_qc_warning - works - no failed samples",
  {
    expected_result <- df |>
      dplyr::filter(
        .data[["SampleID"]] != "FailQC"
      )

    ## verbose = FALSE ----

    expect_equal(
      object = clean_qc_warning(df = expected_result,
                                check_npx_log = log,
                                verbose = FALSE),
      expected = expected_result
    )

    ## verbose = TRUE ----

    expect_message(
      object = expect_equal(
        object = clean_qc_warning(df = expected_result,
                                  check_npx_log = log,
                                  verbose = TRUE),
        expected = expected_result
      ),
      regexp = paste("No samples flagged with SampleQC = \"FAIL\".")
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

    ## verbose = FALSE ----

    expect_message(
      object = expect_equal(
        object = clean_assay_warning(df = df,
                                     check_npx_log = log,
                                     verbose = FALSE),
        expected = expected_result
      ),
      regexp = paste("Excluding 1 datapoint from 1 assay flagged with AssayQC",
                     "= \"WARN\" or \"Warning\": \"OID89012\".")
    )

    ## verbose = TRUE ----

    expect_message(
      object = expect_equal(
        object = clean_assay_warning(df = df,
                                     check_npx_log = log,
                                     verbose = TRUE),
        expected = expected_result
      ),
      regexp = paste("Excluding 1 datapoint from 1 assay flagged with AssayQC",
                     "= \"WARN\" or \"Warning\": \"OID89012\".")
    )
  }
)

test_that(
  "clean_assay_warning - works - arrow - assays flagged with assay warning",
  {
    expected_result <- df_arrow |>
      dplyr::filter(
        .data[["SampleID"]] != "AssayWarn"
      ) |>
      dplyr::collect()

    ## verbose = FALSE ----

    expect_message(
      object = expect_equal(
        object = clean_assay_warning(df = df_arrow,
                                     check_npx_log = log,
                                     verbose = FALSE) |>
          dplyr::collect(),
        expected = expected_result
      ),
      regexp = paste("Excluding 1 datapoint from 1 assay flagged with AssayQC",
                     "= \"WARN\" or \"Warning\": \"OID89012\".")
    )

    ## verbose = TRUE ----

    expect_message(
      object = expect_equal(
        object = clean_assay_warning(df = df_arrow,
                                     check_npx_log = log,
                                     verbose = TRUE) |>
          dplyr::collect(),
        expected = expected_result
      ),
      regexp = paste("Excluding 1 datapoint from 1 assay flagged with AssayQC",
                     "= \"WARN\" or \"Warning\": \"OID89012\".")
    )
  }
)

test_that(
  "clean_assay_warning - works - assays flagged - pluralization",
  {
    expected_result <- df |>
      dplyr::filter(
        .data[["SampleID"]] != "AssayWarn"
      )

    ## 2 datapoints, 1 assay ----

    expect_message(
      object = expect_equal(
        object = clean_assay_warning(
          df = df |>
            dplyr::bind_rows(
              dplyr::tibble(
                "SampleID" = "ControlAssay",
                "OlinkID" = "OID89012",
                "SampleType" = "SAMPLE",
                "AssayType" = "assay",
                "SampleQC" = "PASS",
                "AssayQC" = "WARN",
                "NPX" = -1L,
                "PlateID" = "plate1",
                "UniProt" = "uniprotid1",
                "Assay" = "assay_a",
                "Panel" = "panel_a",
                "PanelVersion" = "panel_version_a",
                "LOD" = -1L,
                "ExtNPX" = -1L,
                "Count" = -1L,
                "Normalization" = "Intensity"
              )
            ),
          check_npx_log = log,
          verbose = FALSE
        ),
        expected = expected_result
      ),
      regexp = paste("Excluding 2 datapoints from 1 assay flagged with AssayQC",
                     "= \"WARN\" or \"Warning\": \"OID89012\".")
    )

    ## 2 datapoints, 2 assays ----

    expect_message(
      object = expect_equal(
        object = clean_assay_warning(
          df = df |>
            dplyr::bind_rows(
              dplyr::tibble(
                "SampleID" = "AssayWarn",
                "OlinkID" = "OID78901",
                "SampleType" = "SAMPLE",
                "AssayType" = "assay",
                "SampleQC" = "PASS",
                "AssayQC" = "WARN",
                "NPX" = -1L,
                "PlateID" = "plate1",
                "UniProt" = "uniprotid1",
                "Assay" = "assay_a",
                "Panel" = "panel_a",
                "PanelVersion" = "panel_version_a",
                "LOD" = -1L,
                "ExtNPX" = -1L,
                "Count" = -1L,
                "Normalization" = "Intensity"
              )
            ),
          check_npx_log = log,
          verbose = TRUE
        ),
        expected = expected_result
      ),
      regexp = paste("Excluding 2 datapoints from 2 assays flagged with",
                     "AssayQC = \"WARN\" or \"Warning\": \"OID89012\" and",
                     "\"OID78901\".")
    )
  }
)

test_that(
  "clean_assay_warning - `assay_warn` column does not exist",
  {
    test_df <- df |>
      dplyr::select(
        -dplyr::all_of("AssayQC")
      )

    log_test <- check_npx(df = test_df) |>
      suppressWarnings() |>
      suppressMessages()

    expect_message(
      object = expect_equal(
        object = clean_assay_warning(df = test_df,
                                     check_npx_log = log_test),
        expected = test_df
      ),
      regexp = "No column marking assay warnings in dataset."
    )
  }
)

test_that(
  "clean_qc_warning - works - no assays with warnings",
  {
    expected_result <- df |>
      dplyr::filter(
        .data[["SampleID"]] != "AssayWarn"
      )

    ## verbose = FALSE ----

    expect_equal(
      object = clean_assay_warning(df = expected_result,
                                   check_npx_log = log,
                                   verbose = FALSE),
      expected = expected_result
    )

    ## verbose = TRUE ----

    expect_message(
      object = expect_equal(
        object = clean_assay_warning(df = expected_result,
                                     check_npx_log = log,
                                     verbose = TRUE),
        expected = expected_result
      ),
      regexp = "No assays flagged with AssayQC = \"WARN\" or \"Warning\"."
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

    ## verbose = FALSE ----

    expect_message(
      object = expect_equal(
        object = clean_control_sample_id(df = df,
                                         check_npx_log = log,
                                         control_sample_ids = c("ControlID"),
                                         verbose = FALSE),
        expected = expected_result
      ),
      regexp = "Excluding sample: \"ControlID\"."
    )

    ## verbose = FALSE ----

    expect_message(
      object = expect_equal(
        object = clean_control_sample_id(df = df,
                                         check_npx_log = log,
                                         control_sample_ids = c("ControlID"),
                                         verbose = TRUE),
        expected = expected_result
      ),
      regexp = "Excluding sample: \"ControlID\"."
    )
  }
)

test_that(
  "clean_control_sample_id - works - arrow - remove control sample",
  {
    expected_result <- df_arrow |>
      dplyr::filter(
        .data[["SampleID"]] != "ControlID"
      ) |>
      dplyr::collect()

    ## verbose = FALSE ----

    expect_message(
      object = expect_equal(
        object = clean_control_sample_id(df = df_arrow,
                                         check_npx_log = log,
                                         control_sample_ids = c("ControlID"),
                                         verbose = FALSE) |>
          dplyr::collect(),
        expected = expected_result
      ),
      regexp = "Excluding sample: \"ControlID\"."
    )

    ## verbose = FALSE ----

    expect_message(
      object = expect_equal(
        object = clean_control_sample_id(df = df_arrow,
                                         check_npx_log = log,
                                         control_sample_ids = c("ControlID"),
                                         verbose = TRUE) |>
          dplyr::collect(),
        expected = expected_result
      ),
      regexp = "Excluding sample: \"ControlID\"."
    )
  }
)

test_that(
  "clean_control_sample_id - no vector of sample identifiers provided",
  {
    ## verbose = FALSE ----

    expect_equal(
      object = clean_control_sample_id(df = df,
                                       check_npx_log = log,
                                       control_sample_ids = NULL,
                                       verbose = FALSE),
      expected = df
    )

    ## verbose = TRUE ----

    expect_message(
      object = expect_equal(
        object = clean_control_sample_id(df = df,
                                         check_npx_log = log,
                                         control_sample_ids = NULL,
                                         verbose = TRUE),
        expected = df
      ),
      regexp = paste("Skipping exclusion of control samples based on",
                     "`control_sample_ids`.")
    )

  }
)

test_that(
  "clean_control_sample_id - sample identifiers provided do not ovelap",
  {
    expected_result <- df |>
      dplyr::filter(
        .data[["SampleID"]] != "ControlID"
      )

    ## none overlaps ----

    expect_message(
      object = expect_equal(
        object = clean_control_sample_id(df = df,
                                         check_npx_log = log,
                                         control_sample_ids = c("ControlID_22"),
                                         verbose = FALSE),
        expected = df
      ),
      regexp = paste("None of the sample identifiers in `control_sample_ids`",
                     "was present in the dataset `df`.")
    )

    ## partial overlap ----

    expect_message(
      object = expect_equal(
        object = clean_control_sample_id(df = df,
                                         check_npx_log = log,
                                         control_sample_ids = c("ControlID",
                                                                "ControlID_22"),
                                         verbose = FALSE),
        expected = expected_result
      ),
      regexp = paste("Excluding sample: \"ControlID\". Sample not in dataset:",
                     "\"ControlID_22\".")
    )
  }
)

# Test clean_col_class ----------------------------------------------------

test_that(
  "clean_col_class - works - correct column class",
  {
    # one column ----

    test_df <- OlinkAnalyze::npx_data1 |>
      dplyr::mutate(
        NPX = as.character(.data[["NPX"]])
      )

    log_test <- check_npx(df = test_df) |>
      suppressWarnings() |>
      suppressMessages()

    expect_message(
      object = expect_equal(
        object = clean_col_class(df = test_df,
                                 check_npx_log = log_test,
                                 verbose = FALSE),
        expected = OlinkAnalyze::npx_data1
      ),
      regexp = "Converted class of column:"
    )

    # multiple columns ----

    test_df <- OlinkAnalyze::npx_data1 |>
      dplyr::mutate(
        NPX = as.character(.data[["NPX"]]),
        LOD = as.character(.data[["LOD"]])
      )

    log_test <- check_npx(df = test_df) |>
      suppressWarnings() |>
      suppressMessages()

    log_test$col_class <- log_test$col_class |>
      dplyr::bind_rows(
        dplyr::tibble(
          "col_name" = "PlateID",
          "col_class" = "numeric",
          "col_key" = "plate_id",
          "expected_col_class" = "character"
        )
      )

    expect_message(
      object = expect_equal(
        object = clean_col_class(df = test_df,
                                 check_npx_log = log_test,
                                 verbose = FALSE),
        expected = OlinkAnalyze::npx_data1
      ),
      regexp = "Converted classes of columns:"
    )
  }
)

test_that(
  "clean_col_class - return data unchanged",
  {
    test_df <- OlinkAnalyze::npx_data1

    log_test <- check_npx(df = test_df) |>
      suppressWarnings() |>
      suppressMessages()

    ## verbose = FALSE ----

    expect_equal(
      object = clean_col_class(df = test_df,
                               check_npx_log = log_test,
                               verbose = FALSE),
      expected = OlinkAnalyze::npx_data1
    )

    ## verbose = TRUE ----

    expect_message(
      object = expect_equal(
        object = clean_col_class(df = test_df,
                                 check_npx_log = log_test,
                                 verbose = TRUE),
        expected = OlinkAnalyze::npx_data1
      ),
      regexp = "Columns are in the correct format."
    )
  }
)

# Test clean_npx ----------------------------------------------------------

test_that(
  "clean_npx - works - remove samples/assays identified by check_npx",
  {
    expected_result <- df |>
      dplyr::filter(
        .data[["SampleID"]] == "ValidSample"
      )

    expect_equal(
      object = clean_npx(df = df,
                         check_log = log,
                         keep_controls = NULL,
                         control_sample_ids = c("ControlID"),
                         verbose = TRUE) |>
        suppressMessages(),
      expected = expected_result
    )
  }
)

test_that(
  "clean_npx emits clean messages without ANSI styling",
  {

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


    msgs <- capture_messages(
      {
        clean_npx(
          df = df,
          check_log = log,
          verbose = TRUE
        )
      }
    )

    # Drop empty or whitespace-only lines
    msgs_clean <- msgs |>
      stringr::str_replace_all(
        pattern = "\n",
        replacement = ""
      ) |>
      stringr::str_replace_all(
        pattern = "^\\s*[\\u2500-]+\\s*|\\s*[\\u2500-]+\\s*$",
        replacement = ""
      ) |>
      stringr::str_trim(
        side = "both"
      ) |>
      (\(x) x[x != ""])()


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
    expect_true(grepl(paste("Removed control samples marked as"),
                      msgs_clean[13L]))
    expect_true(grepl("Cleaning control samples based on Sample ID",
                      msgs_clean[14L]))
    expect_true(grepl(paste("Skipping exclusion of control samples based on",
                            "`control_sample_ids`."),
                      msgs_clean[15L]))
    expect_true(grepl("Cleaning Samples with QC Status 'FAIL'",
                      msgs_clean[16L]))
    expect_true(grepl(paste("Excluding 1 datapoint from 1 sample flagged with",
                            "SampleQC = \"FAIL\": \"FailQC\"."),
                      msgs_clean[17L]))
    expect_true(grepl("Cleaning internal control assays",
                      msgs_clean[18L]))
    expect_true(grepl("Excluding 1 control assay: \"OID78901\"",
                      msgs_clean[19L]))
    expect_true(grepl(paste("Removed control assays marked as"),
                      msgs_clean[20L]))
    expect_true(grepl("Cleaning assays flagged by assay warning",
                      msgs_clean[21L]))
    expect_true(grepl(paste("Excluding 1 datapoint from 1 assay flagged with",
                            "AssayQC = \"WARN\" or \"Warning\": \"OID89012\""),
                      msgs_clean[22L]))
    expect_true(grepl("Correcting flagged column class",
                      msgs_clean[23L]))
    expect_true(grepl("Columns are in the correct format.",
                      msgs_clean[24L]))
    expect_true(grepl("Completed `clean_npx\\(\\)`",
                      msgs_clean[25L]))

  }
)
