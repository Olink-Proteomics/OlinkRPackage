# function that retrieves the data of the relevant pre-computed synthetic
# dataset of wide Olink data
get_wide_synthetic_data <- function(olink_platform,
                                    data_type,
                                    n_panels,
                                    n_assays,
                                    n_samples,
                                    show_dev_int_ctrl,
                                    show_int_ctrl,
                                    version) {
  # check inputs
  check_olink_platform(x = olink_platform)
  check_olink_data_type(x = data_type)
  check_is_scalar_integer(x = n_panels, error = TRUE)
  check_is_scalar_integer(x = n_assays, error = TRUE)
  check_is_scalar_integer(x = n_samples, error = TRUE)
  check_is_scalar_boolean(x = show_dev_int_ctrl, error = TRUE)
  check_is_scalar_boolean(x = show_int_ctrl, error = TRUE)
  check_is_scalar_integer(x = version, error = TRUE)

  # get internal variables
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

  # get full path
  df_rand_path <- test_path("data",
                            "synthetic_dt_wide",
                            olink_p,
                            data_t,
                            df_rand_file)

  if (file.exists(df_rand_path)) {
    # return full file path
    return(readRDS(file = df_rand_path))
  } else {
    # if not, skip the test
    testthat::skip()
    return(NULL)
  }
}

# load example datasets for Olink Explore products
get_example_data <- function(filename) {
  # get file path under tests/testthat/data
  ref_norm_res_file <- test_path("data", filename)
  # check that file exists
  if (file.exists(ref_norm_res_file)) {
    # return full file path
    return(readRDS(file = ref_norm_res_file))
  } else {
    # if not, skip the test
    testthat::skip()
    return(NULL)
  }
}

# load example files living under inst/extdata
get_inst_extdata_file <- function(filename) {
  # get file path under inst/extdata
  sample_data_file <- system.file("extdata", filename,
                                  package = "OlinkAnalyze")
  # check that file exists
  if (file.exists(sample_data_file)) {
    # return full file path
    return(sample_data_file)
  } else {
    # if not, skip the test
    testthat::skip()
    return(NULL)
  }
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

  return(invisible(NULL))
}
