# check_npx ----
#' Tests for check_npx function

test_that("check_npx - error - df is not tibble or arrow data frame", {
  df <- data.frame(
    SampleID = c("A", "B", "C", "D"),
    OlinkID = rep("OID12345", 4L),
    SampleType = rep("SAMPLE", 4L),
    NPX = rnorm(4L),
    PlateID = rep("plate1", 4L),
    QC_Warning = rep("Pass", 4L),
    LOD = rnorm(4L)
  )
  expect_error(check_npx(df),
               "`df` is not a tibble or an arrow data frame!")
})

test_that(" check_npx - no warning -  results as expected", {
  df <- tibble::tibble(
    SampleID = c("A", "B", "C", "D"),
    OlinkID = rep("OID12345", 4L),
    SampleType = rep("SAMPLE", 4L),
    NPX = rnorm(4L),
    PlateID = rep("plate1", 4L),
    QC_Warning = rep("Pass", 4L),
    LOD = rnorm(4L)
  )
  expected <- list(
    col_names = list(sample_id = "SampleID",
                     sample_type = "SampleType",
                     olink_id = "OlinkID",
                     plate_id = "PlateID",
                     qc_warning = "QC_Warning",
                     lod = "LOD",
                     quant = "NPX"),
    invalid_oids = character(0L),
    all_na_assays = character(0L),
    duplicate_sample_ids = character(0L)
  )
  result <- check_npx(df = df,
                      preferred_names = NULL)
  expect_equal(result, expected)
})

test_that(
  "check_npx - warning - detects invalid Olink ID and duplicate Sample ID", {
    df <- tibble::tibble(
      SampleID = c("A", "A", "C", "D"),
      OlinkID = rep("OID123456", 4L),
      SampleType = rep("SAMPLE", 4L),
      NPX = rnorm(4L),
      PlateID = rep("plate1", 4L),
      QC_Warning = rep("Pass", 4L),
      LOD = rnorm(4L)
    )
    expected <- list(
      col_names = list(sample_id = "SampleID",
                       sample_type = "SampleType",
                       olink_id = "OlinkID",
                       plate_id = "PlateID",
                       qc_warning = "QC_Warning",
                       lod = "LOD",
                       quant = "NPX"),
      invalid_oids = "OID123456",
      all_na_assays = character(0L),
      duplicate_sample_ids = "A"
    )
    result <- suppressWarnings(check_npx(df = df, # warning suppressed
                                         preferred_names = NULL))
    expect_equal(result, expected)

  }
)


# check_npx_col_names ----
#' Test for check_npx_col_names function

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

# check_npx_olinkid ----
#' Test for check_npx_olinkid function

test_that("check_npx_olinkid - warning - returns invalid Olink IDs", {
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
  col_names <- check_npx_col_names(df)
  result <- suppressWarnings(check_npx_olinkid(df, col_names))

  expect_warning(check_npx_olinkid(df, col_names),
                 paste("Unrecognized Olink IDs detected:",
                       "OID123456, OID1234, 12345, and NA",
                       sep = " "))
  expect_equal(result, c("OID123456",
                         "OID1234",
                         "12345",
                         "NA"))
})

test_that("check_npx_olinkid - no warning - all OIDs are valid", {
  df <- arrow::arrow_table(
    SampleID = c("A", "B", "C", "D"),
    OlinkID = rep("OID12345", 4L),
    SampleType = rep("SAMPLE", 4L),
    NPX = rnorm(4L),
    PlateID = rep("plate1", 4L),
    QC_Warning = rep("Pass", 4L),
    LOD = rnorm(4L)
  )
  col_names <- check_npx_col_names(df)
  result <- check_npx_olinkid(df, col_names)

  expect_equal(result, character(0L))
})

# check_npx_all_na_assays ----

test_that("check_npx_all_na_assays - warning - all-NA assay captured", {
  df <- tibble::tibble(
    SampleID = c("A", "B", "A", "B"),
    OlinkID = c("OID12345",
                "OID12345",
                "OID23456",
                "OID23456"),
    NPX = c(NA_real_, NA_real_, 1.2, 1.3)
  )
  col_names <-  list(quant = "NPX",
                     olink_id = "OlinkID")
  result <- suppressWarnings(check_npx_all_na_assays(df = df,
                                                     col_names = col_names))

  expect_warning(check_npx_all_na_assays(df = df,
                                         col_names = col_names),
                 "OID12345 has NPX = NA for all samples.")
  expect_equal(result,
               "OID12345")

})

test_that("check_npx_all_na_assays - warning - no assay has all NAs.", {
  df <- tibble::tibble(
    SampleID = c("A", "B", "A", "B"),
    OlinkID = c("OID12345",
                "OID12345",
                "OID23456",
                "OID23456"),
    NPX = c(1.1, 1.2, 1.3, NA_real_)
  )
  col_names <-  list(quant = "NPX",
                     olink_id = "OlinkID")
  result <- check_npx_all_na_assays(df = df,
                                    col_names = col_names)
  expect_equal(result,
               character(0L))
})


test_that("check_npx_all_na_assays - warning - no NA value exists.", {
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
  result <- check_npx_all_na_assays(df = df,
                                    col_names = col_names)
  expect_equal(result,
               character(0L))
})

# check_npx_duplicate_sample_ids ----
#' Test for check_npx_duplicate_sample_ids function

test_that(
  "check_npx_duplicate_sample_ids - warning - detects duplicate sample ID", {
    df <- arrow::arrow_table(SampleID = c("A", "B", "A", "C"),
                             OlinkID = rep("OID12345", 4L),
                             NPX = rnorm(4L))
    col_names <-  list(quant = "NPX",
                       olink_id = "OlinkID",
                       sample_id = "SampleID")
    result <- suppressWarnings(check_npx_duplicate_sample_ids(df, col_names))
    expect_warning(check_npx_duplicate_sample_ids(df, col_names),
                   "Duplicate sample ID detected: A")
    expect_equal(result, "A")
  }
)

test_that(
  "check_npx_duplicate_sample_ids - warning - mutiple duplicate sample IDs", {
    df <- arrow::arrow_table(SampleID = c("A", "A", "B", "B", "C"),
                             OlinkID = c(rep("OID12345", 2L),
                                         rep("OID12346", 3L)),
                             NPX = rnorm(5L))
    col_names <-  list(quant = "NPX",
                       olink_id = "OlinkID",
                       sample_id = "SampleID")

    result <- suppressWarnings(check_npx_duplicate_sample_ids(df, col_names))
    expect_warning(check_npx_duplicate_sample_ids(df, col_names),
                   "Duplicate sample IDs detected: A and B")
    expect_equal(result, c("A", "B"))
  }
)

test_that("check_npx_duplicate_sample_ids - no warning - no duplicates", {
  df <- tibble::tibble(
    SampleID = c("A", "B", "A", "B"),
    OlinkID = c(rep("OID12345", 2L),
                rep("OID12346", 2L)),
    NPX = rnorm(4L)
  )
  col_names <-  list(quant = "NPX",
                     olink_id = "OlinkID",
                     sample_id = "SampleID")

  expect_equal(check_npx_duplicate_sample_ids(df, col_names),
               character(0L))
})
