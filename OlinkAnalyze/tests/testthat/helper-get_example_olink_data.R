# load example datasets for Olink Explore products
get_example_data <- function(filename) {
  ref_norm_res_file <- test_path("data", filename)
  #check that file exists
  expect_true(file.exists(ref_norm_res_file), label = ref_norm_res_file)
  # read rds data
  ref_norm_res_df <- readRDS(file = ref_norm_res_file)
  return(ref_norm_res_df)
}
