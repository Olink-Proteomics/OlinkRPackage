# check_npx_col_names ----

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

    result <- list(
      sample_id = "SampleID",
      sample_type = "SampleType",
      olink_id = "OlinkID",
      plate_id = "PlateID",
      qc_warning = "QC_Warning",
      lod = "LOD",
      quant = "NPX"
    )

    expect_equal(
      object = check_npx_col_names(df),
      expected = result
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
        check_npx_col_names(),
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
        check_npx_col_names(),
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
        check_npx_col_names(),
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
      regexp = "Some of the values of \"preferred_names\" are not detected in"
    )

    # multiple non existing column column names ----

    expect_error(
      object = df |>
        check_npx_col_names(
          preferred_names = c("sample_id" = "IamSampleName",
                              "lod" = "PlateLOD",
                              "sample_type" = "IamSampleType")
        ),
      regexp = "Some of the values of \"preferred_names\" are not detected in"
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
        check_npx_col_names(),
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
        check_npx_col_names(),
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
        check_npx_col_names(),
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
        check_npx_col_names(),
      regexp = "There are no column names associated with the following key"
    )
  }
)

# check_npx_update_col_names ----

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
