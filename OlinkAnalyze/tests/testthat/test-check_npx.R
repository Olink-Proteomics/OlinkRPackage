# Test check_npx ----

test_that(
  "check_npx - error - df is not tibble or arrow data frame",
  {
    df <- data.frame(
      SampleID = c("A", "B", "C", "D"),
      OlinkID = rep("OID12345", 4L),
      SampleType = rep("SAMPLE", 4L),
      NPX = rnorm(4L),
      PlateID = rep("plate1", 4L),
      QC_Warning = rep("Pass", 4L),
      LOD = rnorm(4L)
    )

    expect_error(
      object = check_npx(df),
      regexp = "`df` is not a tibble or an ArrowObject dataset!"
    )
  }
)

test_that(
  "check_npx - works - minimum set of columns, results as expected",
  {
    df <- dplyr::tibble(
      SampleID = LETTERS[1L:4L],
      OlinkID = rep("OID12345", 4L),
      UniProt = LETTERS[1L:4L],
      Assay = LETTERS[1L:4L],
      Panel = LETTERS[1L:4L],
      Panel_Lot_Nr = LETTERS[1L:4L],
      SampleType = rep("SAMPLE", 4L),
      NPX = rnorm(4L),
      PlateID = rep("plate1", 4L),
      QC_Warning = rep("Pass", 4L),
      LOD = rnorm(4L)
    )

    expected_result <- list(
      col_names = list(sample_id = "SampleID",
                       sample_type = "SampleType",
                       olink_id = "OlinkID",
                       uniprot = "UniProt",
                       assay = "Assay",
                       panel = "Panel",
                       plate_id = "PlateID",
                       panel_version = "Panel_Lot_Nr",
                       lod = "LOD",
                       quant = "NPX",
                       qc_warning = "QC_Warning"),
      oid_invalid = character(0L),
      assay_na = character(0L),
      sample_id_dups = character(0L)
    )

    expect_equal(
      object = check_npx(df = df,
                         preferred_names = NULL),
      expected = expected_result
    )
  }
)

test_that(
  "check_npx - warning - invalid OlinkID and duplicate SampleID",
  {
    df <- dplyr::tibble(
      SampleID = c("A", "A", "C", "D"),
      OlinkID = rep("OID123456", 4L),
      SampleType = rep("SAMPLE", 4L),
      NPX = rnorm(4L),
      PlateID = rep("plate1", 4L),
      QC_Warning = rep("Pass", 4L),
      LOD = rnorm(4L)
    )

    expected_result <- list(
      col_names = list(sample_id = "SampleID",
                       sample_type = "SampleType",
                       olink_id = "OlinkID",
                       plate_id = "PlateID",
                       qc_warning = "QC_Warning",
                       lod = "LOD",
                       quant = "NPX"),
      oid_invalid = c("OID123456"),
      assay_na = character(0L),
      sample_id_dups = c("A")
    )

    expect_warning(
      object = expect_warning(
        object = expect_equal(
          object = check_npx(df = df,
                             preferred_names = NULL),
          expected = expected_result
        ),
        regexp = "Unrecognized OlinkID detected"
      ),
      regexp = "Duplicate SampleID detected"
    )
  }
)

# Test check_npx_col_names ----

test_that(
  "check_npx_col_names - works - all columns are detected",
  {
    df <- arrow::arrow_table(
      SampleID = c("A", "B", "C", "D"),
      OlinkID = rep("OID12345", 4L),
      SampleType = rep("SAMPLE", 4L),
      NPX = rnorm(4L),
      PlateID = rep("plate1", 4L),
      QC_Warning = rep("Pass", 4L),
      LOD = rnorm(4L)
    )

    expected_result <- list(
      sample_id = "SampleID",
      sample_type = "SampleType",
      olink_id = "OlinkID",
      plate_id = "PlateID",
      qc_warning = "QC_Warning",
      lod = "LOD",
      quant = "NPX"
    )

    expect_equal(
      object = check_npx_col_names(df = df,
                                   preferred_names = NULL),
      expected = expected_result
    )
  }
)

test_that(
  "check_npx_col_names - works - nullable columns are ok",
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

    # missing SampleType ----

    expect_equal(
      object = df |>
        dplyr::select(
          -dplyr::all_of("SampleType")
        ) |>
        check_npx_col_names(preferred_names = NULL),
      expected = list(
        sample_id = "SampleID",
        olink_id = "OlinkID",
        plate_id = "PlateID",
        qc_warning = "QC_Warning",
        lod = "LOD",
        quant = "NPX"
      )
    )

    # missing LOD ----

    expect_equal(
      object = df |>
        dplyr::select(
          -dplyr::all_of("LOD")
        ) |>
        dplyr::compute() |>
        check_npx_col_names(preferred_names = NULL),
      expected = list(
        sample_id = "SampleID",
        sample_type = "SampleType",
        olink_id = "OlinkID",
        plate_id = "PlateID",
        qc_warning = "QC_Warning",
        quant = "NPX"
      )
    )

    # missing LOD and SampleType ----

    expect_equal(
      object = df |>
        dplyr::select(
          -dplyr::all_of(c("SampleType", "LOD"))
        ) |>
        check_npx_col_names(preferred_names = NULL),
      expected = list(
        sample_id = "SampleID",
        olink_id = "OlinkID",
        plate_id = "PlateID",
        qc_warning = "QC_Warning",
        quant = "NPX"
      )
    )
  }
)

test_that(
  "check_npx_col_names - works - preferred_names",
  {
    df <- arrow::arrow_table(
      SampleID = c("A", "B", "C", "D"),
      OlinkID = rep("OID12345", 4L),
      SampleType = rep("SAMPLE", 4L),
      NPX = rnorm(4L),
      PlateID = rep("plate1", 4L),
      QC_Warning = rep("Pass", 4L),
      LOD = rnorm(4L)
    )

    # one column name ----

    expect_equal(
      object = df |>
        dplyr::rename(
          "IamSampleName" = "SampleID"
        ) |>
        dplyr::compute() |>
        check_npx_col_names(
          preferred_names = c("sample_id" = "IamSampleName")
        ),
      expected = list(
        sample_type = "SampleType",
        olink_id = "OlinkID",
        plate_id = "PlateID",
        qc_warning = "QC_Warning",
        lod = "LOD",
        quant = "NPX",
        sample_id = "IamSampleName"
      )
    )

    # multiple column names ----

    expect_equal(
      object = df |>
        dplyr::rename(
          "IamSampleName" = "SampleID",
          "IamSampleType" = "SampleType",
          "IamPlateIdentifier" = "PlateID",
          "IamOlinkIdentifier" = "OlinkID"
        ) |>
        dplyr::compute() |>
        check_npx_col_names(
          preferred_names = c("sample_id" = "IamSampleName",
                              "sample_type" = "IamSampleType",
                              "plate_id" = "IamPlateIdentifier",
                              "olink_id" = "IamOlinkIdentifier")
        ),
      expected = list(
        qc_warning = "QC_Warning",
        lod = "LOD",
        quant = "NPX",
        sample_id = "IamSampleName",
        sample_type = "IamSampleType",
        plate_id = "IamPlateIdentifier",
        olink_id = "IamOlinkIdentifier"
      )
    )

    # break ties (multiple matches) ----

    expect_equal(
      object = df |>
        dplyr::rename(
          "IamSampleName" = "SampleID",
          "IamSampleType" = "SampleType"
        ) |>
        dplyr::collect() |>
        dplyr::mutate(
          PlateLOD = rnorm(4L),
          MaxLOD = rnorm(4L),
          Quantified_value = rnorm(4L)
        ) |>
        check_npx_col_names(
          preferred_names = c("sample_id" = "IamSampleName",
                              "sample_type" = "IamSampleType",
                              "lod" = "PlateLOD",
                              "quant" = "Quantified_value")
        ),
      expected = list(
        olink_id = "OlinkID",
        plate_id = "PlateID",
        qc_warning = "QC_Warning",
        sample_id = "IamSampleName",
        sample_type = "IamSampleType",
        lod = "PlateLOD",
        quant = "Quantified_value"
      )
    )
  }
)

test_that(
  "check_npx_col_names - error - preferred_names val not in data frame colname",
  {
    df <- arrow::arrow_table(
      SampleID = c("A", "B", "C", "D"),
      OlinkID = rep("OID12345", 4L),
      SampleType = rep("SAMPLE", 4L),
      NPX = rnorm(4L),
      PlateID = rep("plate1", 4L),
      QC_Warning = rep("Pass", 4L),
      LOD = rnorm(4L)
    )

    # one non existing column column name ----

    expect_error(
      object = df |>
        check_npx_col_names(
          preferred_names = c("sample_id" = "IamSampleName")
        ),
      regexp = "Some of the values of `preferred_names` are not detected in"
    )

    # multiple non existing column column names ----

    expect_error(
      object = df |>
        check_npx_col_names(
          preferred_names = c("sample_id" = "IamSampleName",
                              "lod" = "PlateLOD",
                              "sample_type" = "IamSampleType")
        ),
      regexp = "Some of the values of `preferred_names` are not detected in"
    )
  }
)

test_that(
  "check_npx_col_names - error - ties (multiple matches)",
  {
    df <- arrow::arrow_table(
      SampleID = c("A", "B", "C", "D"),
      OlinkID = rep("OID12345", 4L),
      SampleType = rep("SAMPLE", 4L),
      NPX = rnorm(4L),
      PlateID = rep("plate1", 4L),
      QC_Warning = rep("Pass", 4L),
      LOD = rnorm(4L)
    )

    # one column with multiple matches ----

    expect_error(
      object = df |>
        dplyr::collect() |>
        dplyr::mutate(
          PlateLOD = rnorm(4L)
        ) |>
        check_npx_col_names(preferred_names = NULL),
      regexp = "There are multiple column names associated with the following k"
    )

    # mutiple columns with multiple matches ----

    expect_error(
      object = df |>
        dplyr::collect() |>
        dplyr::mutate(
          PlateLOD = rnorm(4L),
          Quantified_value = rnorm(4L)
        ) |>
        check_npx_col_names(preferred_names = NULL),
      regexp = "There are multiple column names associated with the following k"
    )
  }
)

test_that(
  "check_npx_col_names - error - no match for non-nullable columns",
  {
    df <- arrow::arrow_table(
      SampleID = c("A", "B", "C", "D"),
      OlinkID = rep("OID12345", 4L),
      SampleType = rep("SAMPLE", 4L),
      NPX = rnorm(4L),
      PlateID = rep("plate1", 4L),
      QC_Warning = rep("Pass", 4L),
      LOD = rnorm(4L)
    )

    # one column with no matches ----

    expect_error(
      object = df |>
        dplyr::rename(
          "IamSampleName" = "SampleID"
        ) |>
        dplyr::compute() |>
        check_npx_col_names(preferred_names = NULL),
      regexp = "There are no column names associated with the following key"
    )

    # mutiple columns with no matches ----

    expect_error(
      object = df |>
        dplyr::rename(
          "IamSampleName" = "SampleID",
          "IamSampleType" = "SampleType"
        ) |>
        dplyr::compute() |>
        check_npx_col_names(preferred_names = NULL),
      regexp = "There are no column names associated with the following key"
    )
  }
)

# Test check_npx_update_col_names ----

# working cases are covered by tests on check_npx_col_names
# here we will check only the errors

test_that(
  "check_npx_update_col_names - error - no match for non-nullable columns",
  {
    # one name not in column_name_dict ----

    expect_error(
      object = check_npx_update_col_names(
        preferred_names = c("sample_id_wrong" = "SampleID")
      ),
      regexp = "Unexpected name in"
    )

    # multiple names not in column_name_dict ----

    expect_error(
      object = check_npx_update_col_names(
        preferred_names = c("sample_id_wrong" = "SampleID",
                            "wrong_sample_type" = "SampleType",
                            "lod2" = "LOD")
      ),
      regexp = "Unexpected names in"
    )
  }
)

# Test check_npx_olinkid ----

test_that(
  "check_npx_olinkid - warning - returns invalid Olink IDs",
  {
    df <- dplyr::tibble(
      SampleID = c("A", "B", "C", "D", "E"),
      OlinkID = c("OID12345",
                  "OID123456",
                  "OID1234",
                  "12345",
                  "NA"),
      SampleType = rep("SAMPLE", 5L),
      NPX = rnorm(5L),
      PlateID = rep("plate1", 5L),
      QC_Warning = rep("Pass", 5L),
      LOD = rnorm(5L)
    )

    expect_no_condition(
      object = col_names <- check_npx_col_names(df = df,
                                                preferred_names = NULL)
    )

    expect_warning(
      object = expect_equal(
        object = check_npx_olinkid(df = df,
                                   col_names = col_names),
        expected = c("OID123456",
                     "OID1234",
                     "12345",
                     "NA")
      ),
      regexp = "Unrecognized OlinkIDs detected: \"OID123456\", \"OID1234\","
    )
  }
)

test_that(
  "check_npx_olinkid - works - all OlinkID are valid",
  {
    df <- arrow::arrow_table(
      SampleID = c("A", "B", "C", "D"),
      OlinkID = rep("OID12345", 4L),
      SampleType = rep("SAMPLE", 4L),
      NPX = rnorm(4L),
      PlateID = rep("plate1", 4L),
      QC_Warning = rep("Pass", 4L),
      LOD = rnorm(4L)
    )

    expect_no_condition(
      object = col_names <- check_npx_col_names(df = df,
                                                preferred_names = NULL)
    )

    expect_equal(
      object = check_npx_olinkid(df = df,
                                 col_names = col_names),
      expected = character(0L)
    )
  }
)

# check_npx_all_na_assays ----

test_that(
  "check_npx_all_na_assays - warning - all-NA assay captured",
  {
    df <- dplyr::tibble(
      SampleID = c("A", "B", "A", "B"),
      OlinkID = c("OID12345",
                  "OID12345",
                  "OID23456",
                  "OID23456"),
      NPX = c(NA_real_,
              NA_real_,
              1.2,
              1.3)
    )

    col_names <-  list(quant = "NPX",
                       olink_id = "OlinkID")

    expect_warning(
      object = expect_equal(
        object = check_npx_all_na_assays(
          df = df,
          col_names = col_names
        ),
        expected = "OID12345"
      ),
      regexp = "\"OID12345\" has \"NPX\" = NA for all samples."
    )
  }
)

test_that(
  "check_npx_all_na_assays - works - no assay has all NAs",
  {
    df <- dplyr::tibble(
      SampleID = c("A", "B", "A", "B"),
      OlinkID = c("OID12345",
                  "OID12345",
                  "OID23456",
                  "OID23456"),
      NPX = c(1.1,
              1.2,
              1.3,
              NA_real_)
    )

    col_names <-  list(quant = "NPX",
                       olink_id = "OlinkID")

    expect_equal(
      object = check_npx_all_na_assays(df = df,
                                       col_names = col_names),
      expected = character(0L)
    )
  }
)

test_that(
  "check_npx_all_na_assays - warning - arrow - all-NA assay captured",
  {
    df <- arrow::arrow_table(
      SampleID = c("A", "B", "A", "B"),
      OlinkID = c("OID12345",
                  "OID12345",
                  "OID23456",
                  "OID23456"),
      NPX = c(NA_real_,
              NA_real_,
              1.2,
              1.3)
    )

    col_names <-  list(quant = "NPX",
                       olink_id = "OlinkID")

    expect_warning(
      object = expect_equal(
        object = check_npx_all_na_assays(
          df = df,
          col_names = col_names
        ),
        expected = "OID12345"
      ),
      regexp = "\"OID12345\" has \"NPX\" = NA for all samples."
    )
  }
)

test_that(
  "check_npx_all_na_assays - works - arrow - no assay has all NAs",
  {
    df <- arrow::arrow_table(
      SampleID = c("A", "B", "A", "B"),
      OlinkID = c("OID12345",
                  "OID12345",
                  "OID23456",
                  "OID23456"),
      NPX = rnorm(4L)
    )

    col_names <- list(quant = "NPX",
                      olink_id = "OlinkID")

    expect_equal(
      object = check_npx_all_na_assays(df = df,
                                       col_names = col_names),
      expected = character(0L)
    )
  }
)

# check_npx_duplicate_sample_ids ----

test_that(
  "check_npx_duplicate_sample_ids - warning - duplicate SampleID",
  {
    df <- arrow::arrow_table(SampleID = c("A", "B", "A", "C"),
                             OlinkID = rep("OID12345", 4L),
                             NPX = rnorm(4L))

    col_names <-  list(quant = "NPX",
                       olink_id = "OlinkID",
                       sample_id = "SampleID")

    expect_warning(
      object = expect_equal(
        object = check_npx_duplicate_sample_ids(df = df,
                                                col_names = col_names),
        expected = "A"
      ),
      regexp = "Duplicate SampleID detected: \"A\""
    )
  }
)

test_that(
  "check_npx_duplicate_sample_ids - warning - mutiple duplicate sample IDs",
  {
    df <- arrow::arrow_table(SampleID = c("A", "A", "B", "B", "C"),
                             OlinkID = c(rep("OID12345", 2L),
                                         rep("OID12346", 3L)),
                             NPX = rnorm(5L))

    col_names <-  list(quant = "NPX",
                       olink_id = "OlinkID",
                       sample_id = "SampleID")

    expect_warning(
      object = expect_equal(
        object = check_npx_duplicate_sample_ids(df = df,
                                                col_names = col_names),
        expected = c("A", "B")
      ),
      regexp = "Duplicate SampleIDs detected: \"A\" and \"B\""
    )
  }
)

test_that(
  "check_npx_duplicate_sample_ids - works - no duplicates",
  {
    df <- dplyr::tibble(
      SampleID = c("A", "B", "A", "B"),
      OlinkID = c(rep("OID12345", 2L),
                  rep("OID12346", 2L)),
      NPX = rnorm(4L)
    )

    col_names <-  list(quant = "NPX",
                       olink_id = "OlinkID",
                       sample_id = "SampleID")

    expect_equal(
      object = check_npx_duplicate_sample_ids(df = df,
                                              col_names = col_names),
      expected = character(0L)
    )
  }
)
