test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})


#`clean_failed_assay
test <- clean_failed_assay(npx_data1)
setdiff(test, npx_data1)

test_df <- npx_data1 |>
  mutate(Normalization = if_else(Assay == "IL8", "PC", "EXCLUDED"))
test <- clean_failed_assay(test_df)


dir <- "~/shared/olink/Data_Science/Support Projects/PC ratio check/Regeneron/NPX data"
list.files(dir)
npx <- read_npx(file.path(dir, "RGC_Ultima_Dat_Extended_NPX_2025-04-02.parquet"))
npx <- filter(npx, Block %in% c("1", "3", "7", "8"))

checked_npx <- check_npx(npx)
cleaned_npx <- clean_excluded_assay(npx)


cleaned_npx <- clean_sample_type_controls(npx, checked_npx)
cleaned_npx <- clean_sample_id_controls(npx, checked_npx)
cleaned_npx <- clean_qc_warning(npx, checked_npx)


checked_npx <- check_npx(npx_data1)
cleaned_npx <- clean_excluded_assay(npx_data1)


cleaned_npx <- clean_sample_type_controls(npx_data1, checked_npx)
cleaned_npx <- clean_sample_id_controls(npx_data1, checked_npx)

cleaned_npx <- clean_npx(npx, checked_npx)
