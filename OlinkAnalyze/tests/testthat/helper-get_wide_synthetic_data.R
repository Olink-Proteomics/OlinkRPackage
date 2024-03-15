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
  df_rand_path <- test_path("fixtures/",
                            "synth_dt_wide",
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

# function to get the format specifications for wide files
get_format_spec <- function(data_type) {
  format_spec <- olink_wide_spec |>
    dplyr::filter(.data[["data_type"]] == .env[["data_type"]])

  return(format_spec)
}

# Compute num of rows of output df
olink_wide2long_rows <- function(n_panels,
                                 n_assays,
                                 n_samples,
                                 has_int_ctrl,
                                 num_int_ctrl) {
  n_row <- n_panels * n_assays * n_samples

  if (has_int_ctrl == TRUE) {
    n_row_add <- n_panels * num_int_ctrl * n_samples
  } else {
    n_row_add <- 0L
  }

  n_row_out <- n_row + n_row_add

  return(n_row_out)
}
