# Test that function works when unique column names found
test_that("all main columns exist",
          {
            df <- arrow::arrow_table(
              SampleID = c("A", "B", "C", "D"),
              OlinkID = rep("OID12345", 4),
              SampleType = rep("SAMPLE", 4),
              NPX = rnorm(4),
              PlateID = rep("plate1", 4),
              QC_Warning = rep("Pass", 4),
              LOD = rnorm(4)
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

            expect_equal(check_npx_col_names(df),
                         result)

          })

# Test that function works when some columns do not exist
test_that("some columns do not exist",
          {
            df <- arrow::arrow_table(
              SampleID = c("A", "B", "C", "D"),
              OlinkID = rep("OID12345", 4),
              NPX = rnorm(4),
              PlateID = rep("plate1", 4),
              QC_Warning = rep("Pass", 4),
              LOD = rnorm(4)
            )
            result <- list(
              sample_id = "SampleID",
              olink_id = "OlinkID",
              plate_id = "PlateID",
              qc_warning = "QC_Warning",
              lod = "LOD",
              quant = "NPX"
            )

            expect_equal(check_npx_col_names(df),
                         result)
          })

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
