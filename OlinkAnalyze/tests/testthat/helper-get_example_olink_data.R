# load example datasets for Olink Explore products
get_example_data <- function(filename) {
  ref_norm_res_file <- test_path("data", filename)
  #check that file exists
  expect_true(file.exists(ref_norm_res_file), label = ref_norm_res_file)
  # read rds data
  ref_norm_res_df <- readRDS(file = ref_norm_res_file)
  return(ref_norm_res_df)
}

# check if snapshot exists. if not skip the test.
check_snap_exist <- function(test_dir_name, snap_name) {
  # check that "_snaps" exist, otherwise skip
  base_test_dir <- test_path("_snaps")
  skip_if_not(dir.exists(base_test_dir))

  # check that test-specific snaps directory exist. if not skip.
  test_dir <- test_path("_snaps", test_dir_name)
  skip_if_not(dir.exists(test_dir))

  # check that snapshot exist. if not skip
  stopifnot(rlang::is_scalar_character(snap_name))
  snap_name_clean <- gsub("[^a-z0-9]", "-", tolower(snap_name))
  snap_name_clean <- gsub(paste0("-", "-", "+"), "-", snap_name_clean)
  snap_name_clean <- gsub(paste0("^", "-", "|", "-", "$"), "", snap_name_clean)
  snap_name_clean <- paste0(snap_name_clean, ".svg")

  snap_path <- test_path("_snaps", test_dir_name, snap_name_clean)
  skip_if_not(file.exists(snap_path))
}
