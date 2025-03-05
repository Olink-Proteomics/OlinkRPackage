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
  # modify variables
  olink_p <- accepted_olink_platforms |>
    dplyr::filter(
      .data[["name"]] == .env[["olink_platform"]]
    ) |>
    dplyr::pull(
      .data[["short_name"]]
    )

  data_t <- ifelse(data_type == "Quantified", "quant", tolower(data_type))

  int_ctrl <- show_int_ctrl |>
    as.character() |>
    stringr::str_sub(start = 1L,
                     end = 1L)

  dev_int_ctrl <- show_dev_int_ctrl |>
    as.character() |>
    stringr::str_sub(start = 1L,
                     end = 1L)

  # file name
  df_rand_file <- paste0("p=", n_panels, "_",
                         "a=", n_assays, "_",
                         "s=", n_samples, "_",
                         "ic=", int_ctrl, "_",
                         "dic=", dev_int_ctrl, "_",
                         "v=", version, ".rds")
  # file path
  df_rand_path <- test_path("data",
                            "synthetic_dt_wide",
                            olink_p,
                            data_t,
                            df_rand_file)
  #check that file exists
  expect_true(file.exists(df_rand_path))
  # read rds data
  df_synthetic <- readRDS(df_rand_path)
  # return
  return(df_synthetic)
}

# load example datasets for Olink Explore products
get_example_data <- function(filename) {
  ref_norm_res_file <- test_path("data", filename)
  #check that file exists
  expect_true(file.exists(ref_norm_res_file),label = ref_norm_res_file)
  # read rds data
  readRDS(file = ref_norm_res_file)
}
