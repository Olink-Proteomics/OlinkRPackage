test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})


#`clean_failed_assay
test <- clean_failed_assay(npx_data1)
setdiff(test, npx_data1)

test_df <- npx_data1 |>
  mutate(Normalization = if_else(Assay == "IL8", "PC", "EXCLUDED"))
test <- clean_failed_assay(test_df)

# npx_file <- system.file("extdata",
#                         "npx_data_ext.parquet",
#                         package = "OlinkAnalyze")
# npx_df <- OlinkAnalyze::read_npx(filename = npx_file)


test <- npx |> dplyr::rename(olink_id_test = OlinkID)
check_npx(test)



# test data
# dir <- "~/shared/olink/Data_Science/Support Projects/PC ratio check/Regeneron/NPX data"
# file_name <- "RGC_Ultima_Dat_Extended_NPX_2025-04-02.parquet"

dir <- "C:/Users/kang.dong/OneDrive - Olink Proteomics AB/Desktop/BIDMC"
file_name <- "BIDMC-JHS-plates31-40_Extended_NPX_2025-04-16_intensity norm.parquet"


# load data
npx <- read_npx(file.path(dir, file_name),
                out_df = "arrow")


# create invalid olink_id
npx <- npx |>
  dplyr::collect() |>
  dplyr::mutate(OlinkID = dplyr::recode(OlinkID,
                                        "OID40783" = "ABC123",
                                        "OID40784" = "ABC1234",
                                        "OID40785" = "ABC12345",
                                        "OID40786" = "ABC123456"))

npx <- npx |>
  dplyr::collect() |>
  dplyr::mutate(SampleID = dplyr::if_else(stringr::str_detect(SampleID, "NC"), "NC", SampleID))

# check NPX
checked_npx_log <- check_npx(npx)

# df <- npx
# check_npx_log <- checked_npx
#


# clean NPX
cleaned_npx <- clean_assay_na(npx, checked_npx_log)
cleaned_npx <- clean_invalid_oid(npx, checked_npx_log)
cleaned_npx <- clean_duplicate_sample_id(npx, checked_npx_log)
cleaned_npx <- clean_sample_type(npx, checked_npx_log)
cleaned_npx <- clean_assay_type(npx, checked_npx_log)
cleaned_npx <- clean_sample_qc(npx, checked_npx_log)

qc <- clean_npx(df = npx, check_npx_log = checked_npx_log)

qc <- clean_npx(df = npx_data1, check_npx_log = check_npx(npx_data1))
log <- check_npx(npx_data2)
qc <- clean_npx(df = npx_data2, check_npx_log = log)

# qc <- clean_control_sample_id(npx_data1, check_npx(npx_data1))

# cleaned_npx <- clean_excluded_assay(npx)
# cleaned_npx <- clean_sample_type_controls(npx, checked_npx)
# cleaned_npx <- clean_sample_id_controls(npx, checked_npx)
# cleaned_npx <- clean_qc_warning(npx, checked_npx)
#
#
# checked_npx <- check_npx(npx_data1)
# cleaned_npx <- clean_excluded_assay(npx_data1)
#
#
# cleaned_npx <- clean_sample_type_controls(npx_data1, checked_npx)
# cleaned_npx <- clean_sample_id_controls(npx_data1, checked_npx)
#
# cleaned_npx <- clean_npx(npx, checked_npx)
