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

# this function orders the columns
olink_wide_oder_cols <- function(list_df_wide) {

  # combine top, na, middle and bottom matrices
  data_no_head <- list_df_wide$df_top_wide |>
    dplyr::bind_rows(
      list_df_wide$df_na_wide
    ) |>
    dplyr::bind_rows(
      list_df_wide$df_middle_wide
    ) |>
    dplyr::bind_rows(
      list_df_wide$df_na_wide
    ) |>
    dplyr::bind_rows(
      list_df_wide$df_bottom_wide
    )

  # identify unique panels
  uniq_panels <- data_no_head |>
    dplyr::select(
      -dplyr::all_of("V1")
    ) |>
    (\(.x) .x[1L,])() |>
    as.character() |>
    unique() |>
    sort()

  # get index of unique panels
  pid_qc_index <- which(data_no_head[2L, ] %in% c("Plate ID", "QC Warning"))

  # order assays, internal controls and deviations from internal controls based
  # on panels. Output is the dataset ordered. It is missing V1 (SampleID) and
  # PlateID and QC_Warning.
  data_no_head_panel_order <- lapply(uniq_panels, function(.p) {
    panel_index <- which(data_no_head[1L, ] == .p)
    panel_index_move <- panel_index[!(panel_index %in% pid_qc_index)]

    data_no_head |>
      dplyr::select(
        dplyr::all_of(panel_index_move)
      )
  }) |>
    dplyr::bind_cols()

  # add V1 (SampleID) and columns with PlateID and QC_Warning
  data_no_head_ordered <- data_no_head |>
    dplyr::select(
      dplyr::all_of("V1")
    ) |>
    dplyr::bind_cols(
      data_no_head_panel_order
    ) |>
    dplyr::bind_cols(
      data_no_head |>
        dplyr::select(
          dplyr::all_of(pid_qc_index)
        )
    )

  # add head
  data_ordered <- list_df_wide$df_head_wide |>
    dplyr::bind_rows(
      data_no_head_ordered
    )

  return(data_ordered)
}
