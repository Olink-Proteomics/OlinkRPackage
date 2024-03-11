# function that retrieves the relevant pre-computed synthetic dataset of
# wide Olink data
get_wide_synthetic_data <- function(olink_platform,
                                    data_type,
                                    n_panels,
                                    n_assays,
                                    n_samples,
                                    show_dev_int_ctrl,
                                    show_int_ctrl,
                                    version) {
  # file name
  df_rand_file <- paste0("p=", n_panels, "_",
                         "a=", n_assays, "_",
                         "s=", n_samples, "_",
                         "ic=", show_int_ctrl, "_",
                         "dic=", show_dev_int_ctrl, "_",
                         "v=", version, ".rds")
  # file path
  df_rand_path <- test_path("fixtures",
                            olink_platform,
                            data_type,
                            df_rand_file)
  #check that file exists
  expect_true(file.exists(df_rand_path))
  # read rds data
  df_synthetic <- readRDS(df_rand_path)
  # return
  return(df_synthetic)
}
