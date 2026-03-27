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
olink_wide_order_cols <- function(list_df_wide) {

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
    (\(.x) .x[1L, ])() |>
    as.character() |>
    unique() |>
    sort()

  # order
  # - assays and internal controls grouped by panel
  # - plate_id grouped by panel
  # - qc_warning grouped by panel
  # - inc and det deviation from internal controls grouped by panel

  assay_int_ctrl_index <- lapply(uniq_panels, function(.p) {
    # get assay index (columns containing assays)
    assay_index <- which(data_no_head[1L, ] == .p
                         & grepl("^OID", data_no_head[4L, ]))
    # get internal controls index (columns containing internal controls)
    int_ctrl_index <- which(data_no_head[1L, ] == .p
                            & grepl("Inc Ctrl|Det Ctrl|Ext Ctrl",
                                    data_no_head[2L, ]))
    return(c(assay_index, int_ctrl_index))
  }) |>
    unlist()

  plate_id_index <- lapply(uniq_panels, function(.p) {
    # get plate_id index (columns containing plate_id)
    pid_index <- which(data_no_head[1L, ] == .p
                       & data_no_head[2L, ] == "Plate ID")
    return(pid_index)
  }) |>
    unlist()

  qc_warning_index <- lapply(uniq_panels, function(.p) {
    # get qc_warning index (columns containing qc_warning)
    qc_warn_index <- which(data_no_head[1L, ] == .p
                           & data_no_head[2L, ] == "QC Warning")
    return(qc_warn_index)
  }) |>
    unlist()

  dev_int_ctrl_index <- lapply(uniq_panels, function(.p) {
    # get deviation from internal controls index (columns containing deviations
    # from internal controls) - Incubation Control
    dev_int_ctrl_inc_index <- which(
      data_no_head[1L, ] == .p
      & data_no_head[2L, ] == "QC Deviation from median"
      & grepl("^Inc Ctrl", data_no_head[3L, ])
    )
    # get deviation from internal controls index (columns containing deviations
    # from internal controls) - Detection Control
    dev_int_ctrl_det_index <- which(
      data_no_head[1L, ] == .p
      & data_no_head[2L, ] == "QC Deviation from median"
      & grepl("^Det Ctrl", data_no_head[3L, ])
    )
    return(c(dev_int_ctrl_inc_index, dev_int_ctrl_det_index))
  }) |>
    unlist()

  index_order <- c(assay_int_ctrl_index,
                   plate_id_index,
                   qc_warning_index,
                   dev_int_ctrl_index)

  # add V1 (SampleID) and columns with PlateID and QC_Warning
  data_no_head_ordered <- data_no_head |>
    dplyr::select(
      dplyr::all_of("V1")
    ) |>
    dplyr::bind_cols(
      data_no_head |>
        dplyr::select(
          dplyr::all_of(index_order)
        )
    )

  # add head
  data_ordered <- list_df_wide$df_head_wide |>
    dplyr::bind_rows(
      data_no_head_ordered
    )

  return(data_ordered)
}

# transform expected long df to match legacy output
expected_vs_legacy_df_prep <- function(long_expected,
                                       long_legacy,
                                       olink_platform) {
  # modify df_synthetic long
  rename_lookup <- c("Plate_LOD" = "PlateLOD",
                     "Plate_LOD" = "Plate LOD",
                     "Plate_LOD" = "Plate_LOD",
                     "Plate_LQL" = "PlateLQL",
                     "Plate_LQL" = "Plate LQL",
                     "Plate_LQL" = "Plate_LQL")

  df_expected <- long_expected |>
    dplyr::rename(
      dplyr::any_of(rename_lookup)
    ) |>
    dplyr::select(
      dplyr::any_of(colnames(long_legacy))
    ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(c("MissingFreq", "LOD", "NPX", "Quantified_value",
                        "QC Deviation Det Ctrl",
                        "QC Deviation Inc Ctrl",
                        "Plate_LQL", "Plate_LOD", "LLOQ", "ULOQ")),
        ~ as.numeric(.x)
      ),
      Panel = stringr::str_replace_all(
        string = .data[["Panel"]],
        pattern = olink_platform,
        replacement = ""
      ) |>
        stringr::str_squish()
    ) |>
    dplyr::arrange(
      .data[["OlinkID"]], .data[["Assay"]], .data[["SampleID"]]
    )

  df_legacy <- long_legacy |>
    dplyr::select(
      -dplyr::all_of("Index")
    ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(c("MissingFreq")),
        ~ as.numeric(.x)
      ),
      UniProt = dplyr::if_else(grepl("Ctrl", .data[["OlinkID"]]),
                               NA_character_,
                               .data[["UniProt"]]),
      OlinkID = dplyr::if_else(grepl("Ctrl", .data[["OlinkID"]]),
                               NA_character_,
                               .data[["OlinkID"]])
    ) |>
    dplyr::arrange(
      .data[["OlinkID"]], .data[["Assay"]], .data[["SampleID"]]
    )

  return(
    list(
      df_expected = df_expected,
      df_legacy = df_legacy
    )
  )
}
