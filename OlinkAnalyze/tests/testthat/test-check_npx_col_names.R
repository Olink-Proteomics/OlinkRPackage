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
    df <- arrow::arrow_table(
      SampleID = c("A", "B", "C", "D"),
      OlinkID = rep("OID12345", 4L),
      SampleType = rep("SAMPLE", 4L),
      NPX = rnorm(4L),
      PlateID = rep("plate1", 4L),
      QC_Warning = rep("Pass", 4L),
      LOD = rnorm(4L)
    )

    # missing SampleType

    expect_equal(
      object = df |>
        dplyr::select(
          -dplyr::all_of("SampleType")
        ) |>
        dplyr::compute() |>
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

    # missing LOD

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

    # missing LOD and SampleType

    expect_equal(
      object = df |>
        dplyr::select(
          -dplyr::all_of(c("SampleType", "LOD"))
        ) |>
        dplyr::compute() |>
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
    # one column name ----

    # multiple column names ----

    # break ties (multiple matches) ----
  }
)

# Test that function issues warning when multiple names for a column exist
# but preferred_names is not provided
test_that("multiple column names associated with lod",
          {
            df <- arrow::arrow_table(
              SampleID = c("A", "B", "C", "D"),
              OlinkID = rep("OID12345", 4),
              NPX = rnorm(4),
              PlateID = rep("plate1", 4),
              QC_Warning = rep("Pass", 4),
              PlateLOD = rnorm(4),
              MaxLOD = rnorm(4)
            )

            expect_error(check_npx_col_names(df),
                         "There are multiple column names associated with: lod")
          })

# Test that function issues warning when multiple names for a column exist
# and preferred_names is provided
test_that("PlateLOD is provided for lod",
          {
            df <- arrow::arrow_table(
              SampleID = c("A", "B", "C", "D"),
              OlinkID = rep("OID12345", 4),
              NPX = rnorm(4),
              PlateID = rep("plate1", 4),
              QC_Warning = rep("Pass", 4),
              PlateLOD = rnorm(4),
              MaxLOD = rnorm(4)
            )

            result <- list(
              sample_id = "SampleID",
              olink_id = "OlinkID",
              plate_id = "PlateID",
              qc_warning = "QC_Warning",
              quant = "NPX",
              lod = "PlateLOD"
            )

            expect_equal(check_npx_col_names(df, preferred_names = c(lod = "PlateLOD")),
                         result)
          })

# check_npx_update_col_names ----


