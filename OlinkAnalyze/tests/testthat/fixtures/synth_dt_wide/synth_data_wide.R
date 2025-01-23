library(rlang)

# Help functions ----

## Header matrix ----

# return the top 2x2 matrix in the first two rows of an Olink format wide file
olink_wide_head <- function(data_type) {

  df <- dplyr::tibble(
    "V1" = c("Project Name", data_type),
    "V2" = c("Test", NA_character_)
  )

  return(df)
}

# extract NPXS version
olink_wide_npxs_v <- function(df) {
  df |>
    dplyr::slice_head(
      n = 1L
    ) |>
    dplyr::pull(
      .data[["V2"]]
    ) |>
    as.character() |>
    (\(x) {
      strsplit(x = x, split = " ", fixed = TRUE) |>
        lapply(utils::tail, 1L) |>
        unlist()
    })()
}

## Top matrix ----

# computes the top matrix in an Olink format wide file. This matrix contains
# metadata about assays such as OlinkID, Assay name, UniProt ID, the Olink panel
# they belong to and the measuring unit.
# The right hand side of this matrix contains data about Plates, QC_Warnings and
# internal controls.
#
# Returns a matrix in wide format.
olink_wide_top <- function(olink_platform,
                           data_type,
                           n_panels,
                           n_assays,
                           show_dev_int_ctrl = TRUE,
                           show_int_ctrl = TRUE) {

  # basics ----

  # select set of internal controls
  sample_int_ctrl <- dplyr::tibble(
    int_ctrl = olink_wide_spec |>
      dplyr::pull(
        dplyr::all_of("top_matrix_assay_int_ctrl")
      ) |>
      unlist() |>
      unique()
  ) |>
    dplyr::mutate(
      int_ctrl_group = strsplit(x = int_ctrl, split = " ", fixed = TRUE) |>
        lapply(utils::head, 2L) |>
        lapply(paste, collapse = " ") |>
        unlist()
    ) |>
    dplyr::group_by(
      .data[["int_ctrl_group"]]
    ) |>
    dplyr::slice_sample(
      n = 1L
    ) |>
    dplyr::ungroup() |>
    dplyr::pull(
      .data[["int_ctrl"]]
    )

  # get format specifications
  format_spec <- olink_wide_spec |>
    dplyr::filter(
      .data[["data_type"]] == .env[["data_type"]]
    )

  # number of plates always corresponds to number of panels
  n_plates <- n_panels

  # number of QC warning is equal to number of panels if there are QC warnings
  n_qc_warns <- format_spec |>
    dplyr::pull(
      .data[["has_qc_data"]]
    ) |>
    (\(x) ifelse(x == TRUE, n_panels, 0L))()

  # columns with deviation from internal controls
  dev_int_ctrl <- format_spec |>
    dplyr::pull(
      .data[["top_matrix_uniprot_dev_int_ctrl"]]
    ) |>
    unlist() |>
    (\(x) x[x %in% sample_int_ctrl])()


  # internal controls if any
  int_ctrl <- format_spec |>
    dplyr::pull(
      .data[["top_matrix_assay_int_ctrl"]]
    ) |>
    unlist() |>
    (\(x) x[x %in% sample_int_ctrl])()

  # random df ----

  df <- dplyr::tibble(
    "V1" = c("Panel",
             rep(x = paste0("Olink ", olink_platform,
                            " Panel ", seq_len(n_panels)),
                 each = n_assays),
             rep(x = paste0("Olink ", olink_platform,
                            " Panel ", seq_len(n_panels)),
                 each = length(int_ctrl)),
             paste0(rep(paste0("Olink ", olink_platform, " Panel "),
                        times = n_plates),
                    seq_len(n_plates)),
             paste0(rep(paste0("Olink ", olink_platform, " Panel "),
                        times = n_qc_warns),
                    seq_len(n_qc_warns)),
             rep(x = paste0("Olink ", olink_platform,
                            " Panel ", seq_len(n_panels)),
                 each = length(dev_int_ctrl))),
    "V2" = c("Assay",
             paste0(rep(x = "Assay", times = (n_panels * n_assays)),
                    seq_len(n_panels * n_assays)),
             rep(x = int_ctrl, times = n_panels),
             rep(x = "Plate ID", times = n_plates),
             rep(x = "QC Warning", times = n_qc_warns),
             rep(x = "QC Deviation from median",
                 times = (length(dev_int_ctrl) * n_panels))),
    "V3" = c("Uniprot ID",
             paste0(rep(x = "Uniprot", times = (n_panels * n_assays)),
                    seq_len((n_panels * n_assays))),
             rep(x = NA_character_, times = (length(int_ctrl) * n_panels)),
             rep(x = NA_character_, times = n_plates),
             rep(x = NA_character_, times = n_qc_warns),
             rep(x = dev_int_ctrl, times = n_panels)),
    "V4" = c("OlinkID",
             paste0(rep(x = "OID", times = (n_panels * n_assays)),
                    seq_len((n_panels * n_assays))),
             rep(x = NA_character_, times = (length(int_ctrl) * n_panels)),
             rep(x = NA_character_, times = n_plates),
             rep(x = NA_character_, times = n_qc_warns),
             rep(x = NA_character_,
                 times = (length(dev_int_ctrl) * n_panels))),
    "V5" = c("Unit",
             rep(x = "pg/mL", times = (n_panels * n_assays)),
             rep(x = "pg/mL", times = (length(int_ctrl) * n_panels)),
             rep(x = NA_character_, times = n_plates),
             rep(x = NA_character_, times = n_qc_warns),
             rep(x = NA_character_,
                 times = (length(dev_int_ctrl) * n_panels)))
  ) |>
    dplyr::mutate(
      V6 = strsplit(x = .data[["V1"]], split = " ", fixed = TRUE) |>
        sapply(utils::tail, 1L) |>
        unlist(),
      V1 = dplyr::if_else(.data[["V1"]] == "Panel",
                          .data[["V1"]],
                          paste0(.data[["V1"]], "(v.",
                                 .data[["V6"]], ")"))
    ) |>
    dplyr::select(
      -dplyr::all_of("V6")
    )

  # modify df ----

  # if top matrix should contain another row "Unit"
  if (!("Unit" %in%  unlist(format_spec$top_matrix_v1))) {
    df <- df |>
      dplyr::select(
        -dplyr::all_of("V5")
      )
  }

  # if the internal controls should be shown
  if (show_int_ctrl == FALSE) {
    df <- df |>
      dplyr::filter(
        !grepl(pattern = "ctrl",
               x = .data[["V2"]],
               ignore.case = TRUE)
      )
  }

  # if internal controls and customer assays should be shuffled
  if (show_dev_int_ctrl == FALSE) {
    df <- df |>
      dplyr::filter(
        .data[["V2"]] != "QC Deviation from median"
      )
  }

  # transpose df ----

  df_t <- t(df)
  colnames(df_t) <- paste0("V", seq_len(ncol(df_t)))
  rownames(df_t) <- NULL
  df_t <- dplyr::as_tibble(df_t)

  # return ----

  return(df_t)

}

# This function converts the output of olink_wide_top to long to help with tests
# Input is the wide matrix, and output is the top matrix separated based on
# assays, plate_id & qc_warning and internal controls.
olink_wide_top_long <- function(df) {

  # modify df ----

  df <- t(df)
  colnames(df) <- df[1, ]

  df <- df |>
    dplyr::as_tibble(
      rownames = "col_index"
    ) |>
    dplyr::slice(
      2L:dplyr::n()
    ) |>
    dplyr::select(
      dplyr::all_of("col_index"), dplyr::everything()
    )

  # separate df ----

  df_oid <- df |>
    dplyr::filter(
      !is.na(.data[["OlinkID"]])
    )

  df_int_ctrl <- df |>
    dplyr::filter(
      is.na(.data[["OlinkID"]])
      & grepl(pattern = "ctrl",
              x = .data[["Assay"]],
              ignore.case = TRUE)
    )

  df_plate <- df |>
    dplyr::filter(
      is.na(.data[["OlinkID"]])
      & (.data[["Assay"]] %in% c("Plate ID"))
    ) |>
    dplyr::select(
      -dplyr::any_of(c("Uniprot ID", "OlinkID", "Unit"))
    ) |>
    dplyr::rename(
      "Var" = "Assay"
    )

  df_qc_warn <- df |>
    dplyr::filter(
      is.na(.data[["OlinkID"]])
      & (.data[["Assay"]] %in% c("QC Warning"))
    ) |>
    dplyr::select(
      -dplyr::any_of(c("Uniprot ID", "OlinkID", "Unit"))
    ) |>
    dplyr::rename(
      "Var" = "Assay"
    )

  df_dev_int_ctrl <- df |>
    dplyr::filter(
      is.na(.data[["OlinkID"]])
      & .data[["Assay"]] == "QC Deviation from median"
    ) |>
    dplyr::select(
      -dplyr::any_of(c("OlinkID", "Unit"))
    )

  # return ----

  return(
    list(
      df_oid = df_oid,
      df_int_ctrl = df_int_ctrl,
      df_plate = df_plate,
      df_qc_warn = df_qc_warn,
      df_dev_int_ctrl = df_dev_int_ctrl
    )
  )

}

## Middle matrix ----

# computes the middle matrix with assays measurements for each sample. The right
# hand side of it should contain information on Plate ID, QC_Warning and
# internal controls.
olink_wide_middle <- function(data_type,
                              n_panels,
                              n_assays,
                              n_samples,
                              show_dev_int_ctrl = TRUE,
                              show_int_ctrl = TRUE) {

  # pre-compute variables ----

  # get format specifications
  format_spec <- olink_wide_spec |>
    dplyr::filter(
      .data[["data_type"]] == .env[["data_type"]]
    )

  # number of plates
  n_base_plate <- 88L
  n_sample_plates <- ceiling(x = n_samples / n_base_plate) |> as.integer()

  # number of internal controls
  n_int_ctrl <- format_spec |>
    dplyr::pull(
      .data[["top_matrix_assay_int_ctrl"]]
    ) |>
    unlist() |>
    (\(x) {
      strsplit(x = x,
               split = " ",
               fixed = TRUE) |>
        sapply(function(y) {
          paste(y[1L:2L], collapse = " ")
        }) |>
        unlist()
    })() |>
    unique() |>
    length()

  # number of deviations from internal controls
  n_dev_int_ctrl <- format_spec |>
    dplyr::pull(
      .data[["top_matrix_uniprot_dev_int_ctrl"]]
    ) |>
    unlist() |>
    (\(x) {
      strsplit(x = x,
               split = " ",
               fixed = TRUE) |>
        sapply(function(y) {
          paste(y[1L:2L], collapse = " ")
        }) |>
        unlist()
    })() |>
    unique() |>
    length()

  # are there any QC data
  qc_data <- format_spec |>
    dplyr::pull(
      .data[["has_qc_data"]]
    )

  # create parts of df ----

  ## sample id ----

  df_s <- dplyr::tibble(
    "sample_id" = paste0("S", seq_len(n_samples))
  )

  df <- df_s
  rm(df_s)

  ## quantification values for customer assays ----

  df_q <- matrix(
    data = rnorm(n = n_samples * n_assays * n_panels),
    nrow = n_samples,
    ncol = (n_assays * n_panels),
    dimnames = list(
      paste0("S", seq_len(n_samples)),
      paste0("Q", seq_len(n_assays * n_panels))
    )
  ) |>
    dplyr::as_tibble()

  df <- df |>
    dplyr::bind_cols(
      df_q
    )
  rm(df_q)

  ## quantification values for internal controls ----

  if (n_int_ctrl > 0L) {
    df_i <- matrix(
      data = rnorm(n = n_samples * n_int_ctrl * n_panels),
      nrow = n_samples,
      ncol = (n_int_ctrl * n_panels),
      dimnames = list(
        paste0("S", seq_len(n_samples)),
        paste0("I", seq_len(n_int_ctrl * n_panels))
      )
    ) |>
      dplyr::as_tibble()

    df <- df |>
      dplyr::bind_cols(
        df_i
      )
    rm(df_i)
  }

  ## plate names ----

  plate_name <- paste0(
    rep(x = "Plate", times = (n_base_plate * n_sample_plates)),
    rep(x = seq_len(n_sample_plates), each = n_base_plate)
  ) |>
    (\(x) x[seq_len(n_samples)])()
  panel_name <- paste0("Panel", seq_len(n_panels))

  df_p <- sapply(panel_name, function(x) paste(plate_name, x, sep = "_")) |>
    dplyr::as_tibble() |>
    dplyr::slice_head(
      n = n_samples
    )
  colnames(df_p) <- paste0("P", seq_len(n_panels))

  df <- df |>
    dplyr::bind_cols(
      df_p
    )
  rm(plate_name, df_p)

  ## qc warning ----

  if (qc_data == TRUE) {
    df_w <- matrix(
      data = sample(x = c("Pass", "Warn"),
                    size = (n_samples * n_panels),
                    replace = TRUE),
      nrow = n_samples,
      ncol = n_panels,
      dimnames = list(
        paste0("S", seq_len(n_samples)),
        paste0("W", seq_len(n_panels))
      )
    ) |>
      dplyr::as_tibble()

    df <- df |>
      dplyr::bind_cols(
        df_w
      )
    rm(df_w)
  }

  ## deviation from internal controls ----

  if (n_dev_int_ctrl > 0L) {
    df_d <- matrix(
      data = rnorm(n = n_samples * n_dev_int_ctrl * n_panels),
      nrow = n_samples,
      ncol = (n_dev_int_ctrl * n_panels),
      dimnames = list(
        paste0("S", seq_len(n_samples)),
        paste0("D", seq_len(n_dev_int_ctrl * n_panels))
      )
    ) |>
      dplyr::as_tibble()

    df <- df |>
      dplyr::bind_cols(
        df_d
      )
    rm(df_d)
  }

  # modify df ----

  ## remove internal controls ----

  if (n_int_ctrl > 0L
      && show_int_ctrl == FALSE) {
    df <- df |>
      dplyr::select(
        -dplyr::starts_with("I")
      )
  }

  ## remove deviation from internal controls ----

  if (n_dev_int_ctrl > 0L
      && show_dev_int_ctrl == FALSE) {
    df <- df |>
      dplyr::select(
        -dplyr::starts_with("D")
      )
  }

  ## rename columns ----

  colnames(df) <- paste0("V", seq_len(ncol(df)))

  ## convert df to character ----

  df <- df |>
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(), ~ as.character(.x)
      )
    )

  # return ----

  return(df)
}

# takes as input the middle matrix and splits it into its components in long
# format to help with the testing.
#
# output contains df_oid, df_pid, df_qc_warn and df_int_ctrl
olink_wide_middle_long <- function(df,
                                   data_type,
                                   cname) {
  # add rid
  df <- df |>
    dplyr::mutate(
      rid = dplyr::row_number()
    )

  # split df ----

  ## customer assays ----

  df_oid <- df |>
    dplyr::select(
      dplyr::all_of(c("V1", "rid", cname$df_oid))
    ) |>
    dplyr::rename(
      "SampleID" = dplyr::all_of("V1")
    ) |>
    tidyr::pivot_longer(
      -dplyr::all_of(c("SampleID", "rid")),
      names_to = "col_index",
      values_to = data_type
    )

  list_df <- list(
    df_oid = df_oid
  )

  ## internal controls ----

  if ("df_int_ctrl" %in% names(cname)) {
    df_int_ctrl <- df |>
      dplyr::select(
        dplyr::all_of(c("V1", "rid", cname$df_int_ctrl))
      ) |>
      dplyr::rename(
        "SampleID" = dplyr::all_of("V1")
      ) |>
      tidyr::pivot_longer(
        -dplyr::all_of(c("SampleID", "rid")),
        names_to = "col_index",
        values_to = data_type
      )

    list_df <- append(x = list_df,
                      values = list(df_int_ctrl = df_int_ctrl))
  }

  ## plate id ----

  df_plate <- df |>
    dplyr::select(
      dplyr::all_of(c("V1", "rid", cname$df_plate))
    ) |>
    dplyr::rename(
      "SampleID" = dplyr::all_of("V1")
    ) |>
    tidyr::pivot_longer(
      -dplyr::all_of(c("SampleID", "rid")),
      names_to = "col_index",
      values_to = "PlateID"
    )

  list_df <- append(x = list_df,
                    values = list(df_plate = df_plate))

  ## qc warning ----

  if ("df_qc_warn" %in% names(cname)) {
    df_qc_warn <- df |>
      dplyr::select(
        dplyr::all_of(c("V1", "rid", cname$df_qc_warn))
      ) |>
      dplyr::rename(
        "SampleID" = dplyr::all_of("V1")
      ) |>
      tidyr::pivot_longer(
        -dplyr::all_of(c("SampleID", "rid")),
        names_to = "col_index",
        values_to = "QC_Warning"
      )

    list_df <- append(x = list_df,
                      values = list(df_qc_warn = df_qc_warn))
  }

  ## deviation from internal controls ----

  if ("df_dev_int_ctrl" %in% names(cname)) {
    df_dev_int_ctrl <- df |>
      dplyr::select(
        dplyr::all_of(c("V1", "rid", cname$df_dev_int_ctrl))
      ) |>
      dplyr::rename(
        "SampleID" = dplyr::all_of("V1")
      ) |>
      tidyr::pivot_longer(
        -dplyr::all_of(c("SampleID", "rid")),
        names_to = "col_index",
        values_to = data_type
      )

    list_df <- append(x = list_df,
                      values = list(df_dev_int_ctrl = df_dev_int_ctrl))
  }

  # return ----

  return(list_df)

}

## Bottom matrix ----

# computes the bottom matrix with LOD, ULOQ, LLOQ, and plate-specific QC metrics
olink_wide_bottom <- function(olink_platform,
                              data_type,
                              plates,
                              n_panels,
                              n_assays,
                              show_int_ctrl = TRUE,
                              version = 1L) {

  # pre-compute variables ----

  # get bottom matrix format specifications
  format_spec_bottom <- olink_wide_bottom_matrix |>
    dplyr::filter(
      .data[["data_type"]] == .env[["data_type"]]
      & .data[["olink_platform"]] == .env[["olink_platform"]]
    ) |>
    dplyr::mutate(
      variable_alt_names = lapply(.data[["variable_alt_names"]],
                                  sample,
                                  size = 1L) |>
        unlist()
    )
  if (!(version %in% format_spec_bottom$version)) {
    stop("Wrong version!")
  } else {
    format_spec_bottom <- format_spec_bottom |>
      dplyr::filter(
        .data[["version"]] %in% c(0L, .env[["version"]])
      )
  }

  # number of internal controls
  n_int_ctrl <- olink_wide_spec |>
    dplyr::filter(
      .data[["data_type"]] == .env[["data_type"]]
    ) |>
    dplyr::pull(
      .data[["top_matrix_assay_int_ctrl"]]
    ) |>
    unlist() |>
    (\(x) {
      strsplit(x = x, split = " ", fixed = TRUE) |>
        lapply(function(y) paste(y[1:2], collapse = " ")) |>
        unlist()
    })() |>
    unique() |>
    length()
  if (show_int_ctrl == FALSE) {
    n_int_ctrl <- 0L
  }

  # prepare bottom matrix ----

  ## plate-specific QC metrics ----

  # Assay warning
  df_assay_warn <- matrix(
    data = sample(x = c("Pass", "Warn"),
                  size = (nrow(plates) * n_panels * (n_assays + n_int_ctrl)),
                  replace = TRUE),
    nrow = nrow(plates),
    ncol = ((n_assays + n_int_ctrl) * n_panels),
    dimnames = list(
      rep(x = "Assay warning", times = nrow(plates)),
      paste0("V", 2L:(((n_assays + n_int_ctrl) * n_panels) + 1L))
    )
  ) |>
    dplyr::as_tibble(
      rownames = "V1"
    ) |>
    dplyr::bind_cols(
      plates
    )

  # Lowest quantifiable level
  df_low_quant_lvl <- matrix(
    data = rnorm(n = (nrow(plates) * n_panels * (n_assays + n_int_ctrl))),
    nrow = nrow(plates),
    ncol = ((n_assays + n_int_ctrl) * n_panels),
    dimnames = list(
      rep(x = "Lowest quantifiable level", times = nrow(plates)),
      paste0("V", 2L:(((n_assays + n_int_ctrl) * n_panels) + 1L))
    )
  ) |>
    dplyr::as_tibble(
      rownames = "V1"
    ) |>
    dplyr::bind_cols(
      plates
    ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(), ~ as.character(.x)
      )
    )

  # Plate LOD
  df_plate_lod <- matrix(
    data = rnorm(n = (nrow(plates) * n_panels * (n_assays + n_int_ctrl))),
    nrow = nrow(plates),
    ncol = ((n_assays + n_int_ctrl) * n_panels),
    dimnames = list(
      rep(x = "Plate LOD", times = nrow(plates)),
      paste0("V", 2L:(((n_assays + n_int_ctrl) * n_panels) + 1L))
    )
  ) |>
    dplyr::as_tibble(
      rownames = "V1"
    ) |>
    dplyr::bind_cols(
      plates
    ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        ~ as.character(.x)
      )
    )

  # Plate LOD v2
  df_plate_lod_v2 <- df_plate_lod |>
    dplyr::mutate(
      V1 = "PlateLOD"
    )

  # Plate LOD v3
  df_plate_lod_v3 <- df_plate_lod |>
    dplyr::mutate(
      V1 = "Plate_LOD"
    )

  df_plate_specific <- df_assay_warn |>
    dplyr::bind_rows(
      df_low_quant_lvl
    ) |>
    dplyr::bind_rows(
      df_plate_lod
    ) |>
    dplyr::bind_rows(
      df_plate_lod_v2
    ) |>
    dplyr::bind_rows(
      df_plate_lod_v3
    ) |>
    dplyr::filter(
      .data[["V1"]] %in% format_spec_bottom$variable_alt_names
    ) |>
    remove_all_na_cols()
  rm(df_assay_warn, df_low_quant_lvl, df_plate_lod,
     df_plate_lod_v2, df_plate_lod_v3)

  ## plate shared variables ----

  # LOD
  df_lod <- matrix(
    data = rnorm(n = (n_panels * (n_assays + n_int_ctrl))),
    nrow = 1L,
    ncol = ((n_assays + n_int_ctrl) * n_panels),
    dimnames = list(
      "LOD",
      paste0("V", 2L:(((n_assays + n_int_ctrl) * n_panels) + 1L))
    )
  ) |>
    dplyr::as_tibble(
      rownames = "V1"
    ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        ~ as.character(.x)
      )
    )

  # Max LOD
  df_max_lod <- df_lod |>
    dplyr::mutate(
      V1 = "Max LOD"
    )

  # Max LOD v2
  df_max_lod_v2 <- df_lod |>
    dplyr::mutate(
      V1 = "MaxLOD"
    )

  # Max LOD v3
  df_max_lod_v3 <- df_lod |>
    dplyr::mutate(
      V1 = "Max_LOD"
    )

  # LLOQ
  df_lloq <- matrix(
    data = rnorm(n = (n_panels * (n_assays + n_int_ctrl))),
    nrow = 1L,
    ncol = ((n_assays + n_int_ctrl) * n_panels),
    dimnames = list(
      "LLOQ",
      paste0("V", 2L:(((n_assays + n_int_ctrl) * n_panels) + 1L))
    )
  ) |>
    dplyr::as_tibble(
      rownames = "V1"
    ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        ~ as.character(.x)
      )
    )

  # ULOQ
  df_uloq <- matrix(
    data = rnorm(n = (n_panels * (n_assays + n_int_ctrl))),
    nrow = 1L,
    ncol = ((n_assays + n_int_ctrl) * n_panels),
    dimnames = list(
      "ULOQ",
      paste0("V", 2L:(((n_assays + n_int_ctrl) * n_panels) + 1L))
    )
  ) |>
    dplyr::as_tibble(
      rownames = "V1"
    ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        ~ as.character(.x)
      )
    )

  # Missing Data freq.
  df_miss_freq <- matrix(
    data = rnorm(n = (n_panels * (n_assays + n_int_ctrl))),
    nrow = 1L,
    ncol = ((n_assays + n_int_ctrl) * n_panels),
    dimnames = list(
      "Missing Data freq.",
      paste0("V", 2L:(((n_assays + n_int_ctrl) * n_panels) + 1L))
    )
  ) |>
    dplyr::as_tibble(
      rownames = "V1"
    ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(), ~ as.character(.x)
      )
    )

  # Normalization
  df_norm <- matrix(
    data = rep(x = "Intensity", times = (n_panels * (n_assays + n_int_ctrl))),
    nrow = 1L,
    ncol = ((n_assays + n_int_ctrl) * n_panels),
    dimnames = list(
      "Normalization",
      paste0("V", 2L:(((n_assays + n_int_ctrl) * n_panels) + 1L))
    )
  ) |>
    dplyr::as_tibble(
      rownames = "V1"
    )

  df_plate_shared <- df_lod |>
    dplyr::bind_rows(
      df_max_lod
    ) |>
    dplyr::bind_rows(
      df_max_lod_v2
    ) |>
    dplyr::bind_rows(
      df_max_lod_v3
    ) |>
    dplyr::bind_rows(
      df_lloq
    ) |>
    dplyr::bind_rows(
      df_uloq
    ) |>
    dplyr::bind_rows(
      df_miss_freq
    ) |>
    dplyr::bind_rows(
      df_norm
    ) |>
    dplyr::filter(
      .data[["V1"]] %in% format_spec_bottom$variable_alt_names
    ) |>
    remove_all_na_cols()
  rm(df_lod, df_max_lod, df_max_lod_v2, df_max_lod_v3, df_lloq, df_uloq,
     df_miss_freq, df_norm)

  # return ----

  df <- df_plate_specific |>
    dplyr::bind_rows(
      df_plate_shared
    )

  return(df)

}

olink_wide_bottom_long <- function(df,
                                   olink_platform,
                                   data_type,
                                   int_ctrl_cols,
                                   plate_cols,
                                   version = 1L,
                                   df_panel_plate) {

  # pre-compute variables ----

  # remove all NA columns
  df <- remove_all_na_cols(df = df)

  # get bottom matrix format specifications
  format_spec_bottom <- olink_wide_bottom_matrix |>
    dplyr::filter(
      .data[["data_type"]] == .env[["data_type"]]
      & .data[["olink_platform"]] == .env[["olink_platform"]]
    )
  if (!(version %in% format_spec_bottom$version)) {
    stop("Wrong version!")
  } else {
    format_spec_bottom <- format_spec_bottom |>
      dplyr::filter(
        .data[["version"]] %in% c(0L, .env[["version"]])
      )
  }

  # df bottom matrix with plate-specific data ----

  bottom_mat_plate_spec <- format_spec_bottom |>
    dplyr::filter(
      .data[["plate_specific"]] == TRUE
    ) |>
    dplyr::pull(
      .data[["variable_alt_names"]]
    ) |>
    unlist()

  df_plate_specific <- df |>
    dplyr::filter(
      .data[["V1"]] %in% bottom_mat_plate_spec
    )

  if (nrow(df_plate_specific) > 0L) {

    # for each variable in V1 and do a pivot_longer
    df_plate_specific <- lapply(unique(df_plate_specific$V1), function(x) {

      df_plate_specific_tmp <- df_plate_specific |>
        dplyr::filter(
          .data[["V1"]] == .env[["x"]]
        ) |>
        dplyr::select(
          -dplyr::all_of("V1")
        ) |>
        tidyr::pivot_longer(
          -dplyr::all_of(plate_cols),
          names_to = "col_index_oid",
          values_to = x
        )

      if (ncol(df_plate_specific_tmp) == 3L) {
        df_plate_specific_tmp <- df_plate_specific_tmp |>
          dplyr::rename(
            "PlateID" = dplyr::all_of(plate_cols)
          ) |>
          dplyr::mutate(
            col_index_plate = .env[["plate_cols"]]
          )
      } else {
        df_plate_specific_tmp <- df_plate_specific_tmp |>
          tidyr::pivot_longer(
            -dplyr::all_of(c("col_index_oid", x)),
            names_to = "col_index_plate",
            values_to = "PlateID"
          )
      }

      df_plate_specific_tmp <- df_plate_specific_tmp |>
        dplyr::inner_join(
          df_panel_plate,
          by = c("col_index_oid", "col_index_plate", "PlateID")
        ) |>
        dplyr::select(
          -dplyr::all_of(c("col_index_plate", "Panel"))
        ) |>
        dplyr::rename(
          "col_index" = "col_index_oid"
        )

      return(df_plate_specific_tmp)
    })

    # left join all data frames from the list
    df_plate_specific <- Reduce(f = function(df_1, df_2) {
      dplyr::left_join(x = df_1,
                       y = df_2,
                       by = c("col_index", "PlateID"),
                       relationship = "one-to-one")
    },
    x = df_plate_specific)
  }

  # df bottom matrix with plate-shared data ----

  bottom_mat_plate_shared <- format_spec_bottom |>
    dplyr::filter(
      .data[["plate_specific"]] == FALSE
    ) |>
    dplyr::pull(
      .data[["variable_alt_names"]]
    ) |>
    unlist()

  df_plate_shared <- df |>
    dplyr::filter(
      .data[["V1"]] %in% bottom_mat_plate_shared
    ) |>
    remove_all_na_cols()

  # transpose and add column names
  df_plate_shared <- t(df_plate_shared)
  colnames(df_plate_shared) <- df_plate_shared[1L, ]

  # fix column names and remove extra rows
  df_plate_shared <- df_plate_shared |>
    dplyr::as_tibble(
      rownames = "col_index"
    ) |>
    dplyr::slice(
      2L:dplyr::n()
    )

  # join df_plate_specific and df_plate_shared if needed ----

  if (nrow(df_plate_specific) > 0L) {
    df_out <- dplyr::left_join(
      x = df_plate_shared,
      y = df_plate_specific,
      by = "col_index",
      relationship = "one-to-many"
    )
  } else {
    df_out <- df_plate_shared
  }

  # modify df  ----

  # sort columns of df_out
  df_out <- df_out |>
    dplyr::select(
      order(colnames(df_out))
    )

  # return ----

  if (!is.null(int_ctrl_cols)) {
    list_df <- list(
      df_oid = df_out |>
        dplyr::filter(
          !(.data[["col_index"]] %in% .env[["int_ctrl_cols"]])
        ),
      df_int_ctrl = df_out |>
        dplyr::filter(
          .data[["col_index"]] %in% .env[["int_ctrl_cols"]]
        )
    )
  } else {
    list_df <- list(
      df_oid = df_out
    )
  }

  return(list_df)

}

## Full matrix ----

# combines the full matrix mimicing and Olink format wide file.
olink_wide <- function(olink_platform,
                       data_type,
                       n_panels,
                       n_assays,
                       n_samples,
                       show_dev_int_ctrl = TRUE,
                       show_int_ctrl = TRUE,
                       version = 1L) {

  # get format specifications
  format_spec <- olink_wide_spec |>
    dplyr::filter(
      .data[["data_type"]] == .env[["data_type"]]
    )

  # head ----

  df_head_wide <- olink_wide_head(data_type = data_type)

  # top matrix ----

  df_top_wide <- olink_wide_top(olink_platform = olink_platform,
                                data_type = data_type,
                                n_panels = n_panels,
                                n_assays = n_assays,
                                show_dev_int_ctrl = show_dev_int_ctrl,
                                show_int_ctrl = show_int_ctrl)

  # middle matrix ----

  df_middle_wide <- olink_wide_middle(data_type = data_type,
                                      n_panels = n_panels,
                                      n_assays = n_assays,
                                      n_samples = n_samples,
                                      show_dev_int_ctrl = show_dev_int_ctrl,
                                      show_int_ctrl = show_int_ctrl)

  # bottom matrix ----

  if (format_spec$has_qc_data == TRUE) {
    # get df top matrix in long format
    df_top_long <- olink_wide_top_long(df = df_top_wide)

    # get column names
    cname <- sapply(df_top_long, \(x) (x$col_index))
    cname <- cname[sapply(cname, length) > 0L]
    rm(df_top_long)

    # get middle matrix in long format
    df_middle_long <- olink_wide_middle_long(df = df_middle_wide,
                                             data_type = data_type,
                                             cname = cname)
    rm(cname)

    plates <- df_middle_long$df_plate |>
      dplyr::select(
        -dplyr::all_of(c("SampleID", "rid"))
      ) |>
      dplyr::distinct() |>
      tidyr::separate(
        col = dplyr::all_of("PlateID"),
        into = c("plate_id", "panel_id"),
        sep = "_",
        remove = FALSE
      ) |>
      dplyr::select(
        -dplyr::all_of("panel_id")
      ) |>
      tidyr::pivot_wider(
        names_from = dplyr::all_of("col_index"),
        values_from = dplyr::all_of("PlateID")
      ) |>
      dplyr::select(
        -dplyr::all_of("plate_id")
      )
    rm(df_middle_long)

    df_bottom_wide <- olink_wide_bottom(olink_platform = olink_platform,
                                        data_type = data_type,
                                        plates = plates,
                                        n_panels = n_panels,
                                        n_assays = n_assays,
                                        show_int_ctrl = show_int_ctrl,
                                        version = version)
    rm(plates)
  }

  # na rows ----

  df_na_wide <- dplyr::tibble(
    "X" = rep(x = NA_character_, times = ncol(df_top_wide))
  ) |>
    t()
  colnames(df_na_wide) <- paste0("V", seq_len(ncol(df_top_wide)))
  df_na_wide <- dplyr::as_tibble(df_na_wide)

  # combine df ----

  df_wide <- df_head_wide |>
    dplyr::bind_rows(
      df_top_wide
    ) |>
    dplyr::bind_rows(
      df_na_wide
    ) |>
    dplyr::bind_rows(
      df_middle_wide
    )

  if (format_spec$has_qc_data == TRUE) {
    df_wide <- df_wide |>
      dplyr::bind_rows(
        df_na_wide
      ) |>
      dplyr::bind_rows(
        df_bottom_wide
      )
  }

  # return ----

  list_df_wide <- list(
    df_wide = df_wide,
    df_top_wide = df_top_wide,
    df_middle_wide = df_middle_wide,
    df_head_wide = df_head_wide,
    df_na_wide = df_na_wide
  )

  if (format_spec$has_qc_data == TRUE) {
    list_df_wide <- append(x = list_df_wide,
                           values = list(df_bottom_wide = df_bottom_wide))
  }

  return(list_df_wide)

}

# converts the full wide format file to a long df.
olink_wide_to_long <- function(df_top_wide,
                               df_middle_wide,
                               df_bottom_wide,
                               df_head,
                               olink_platform,
                               data_type,
                               version = 1L) {

  # convert the top matrix to a list of long df ----

  df_top_long <- olink_wide_top_long(df = df_top_wide)

  # extract colnames to use below ----

  cname <- sapply(df_top_long, \(x) (x$col_index))
  cname <- cname[sapply(cname, length) > 0L]

  # convert the middle matrix to a list of long df ----

  df_middle_long <- olink_wide_middle_long(df = df_middle_wide,
                                           data_type = data_type,
                                           cname = cname)

  has_qc_warn <- "df_qc_warn" %in% names(df_middle_long) &&
    "df_qc_warn" %in% names(df_top_long)

  has_dev_int_ctrl <- "df_dev_int_ctrl" %in% names(df_middle_long) &&
    "df_dev_int_ctrl" %in% names(df_top_long)

  has_int_ctrl <- "df_int_ctrl" %in% names(cname) &&
    length(cname$df_int_ctrl) > 0L

  has_bottom_mat <- !is.null(df_bottom_wide)

  # convert the bottom matrix to a long df ----

  if (has_bottom_mat) {

    df_panel_plate <- df_top_long$df_oid
    if (has_int_ctrl) {
      df_panel_plate <- df_panel_plate |>
        dplyr::bind_rows(
          df_top_long$df_int_ctrl
        )
    }
    df_panel_plate <- df_panel_plate |>
      dplyr::select(
        dplyr::all_of(c("col_index_oid" = "col_index", "Panel"))
      ) |>
      dplyr::left_join(
        df_top_long$df_plate |>
          dplyr::select(
            dplyr::all_of(c("col_index_plate" = "col_index", "Panel"))
          ),
        by = "Panel",
        relationship = "many-to-one"
      ) |>
      dplyr::left_join(
        df_middle_long$df_plate |>
          dplyr::select(
            dplyr::all_of(c("col_index_plate" = "col_index", "PlateID"))
          ) |>
          dplyr::distinct(),
        by = "col_index_plate",
        relationship = "many-to-many"
      )

    # get column names of internal controls
    if (has_int_ctrl) {
      int_ctrl_cols <- cname$df_int_ctrl
    } else {
      int_ctrl_cols <- NULL
    }

    plate_cols <- unique(df_top_long$df_plate$col_index)

    df_bottom_long <- olink_wide_bottom_long(
      df = df_bottom_wide,
      olink_platform = olink_platform,
      data_type = data_type,
      int_ctrl_cols = int_ctrl_cols,
      plate_cols = plate_cols,
      version = version,
      df_panel_plate = df_panel_plate
    )
  }

  # prepare components of long df ----

  ## df oid ----

  df_oid <- df_middle_long$df_oid |>
    dplyr::left_join(
      df_top_long$df_oid,
      by = "col_index",
      relationship = "many-to-one"
    )

  ## df internal controls ----

  if (has_int_ctrl) {

    df_int_ctrl <- df_middle_long$df_int_ctrl |>
      dplyr::left_join(
        df_top_long$df_int_ctrl,
        by = "col_index",
        relationship = "many-to-one"
      )

    df_long <- df_oid |>
      dplyr::bind_rows(
        df_int_ctrl
      )
  } else {
    df_long <- df_oid
  }

  ## df plate id ----

  df_plate <- df_middle_long$df_plate |>
    dplyr::left_join(
      df_top_long$df_plate,
      by = "col_index",
      relationship = "many-to-one"
    ) |>
    dplyr::select(
      -dplyr::any_of(c("Var", "Unit", "col_index"))
    )

  ## df qc warning ----

  if (has_qc_warn) {

    df_qc_warn <- df_middle_long$df_qc_warn |>
      dplyr::left_join(
        df_top_long$df_qc_warn,
        by = "col_index",
        relationship = "many-to-one"
      ) |>
      dplyr::select(
        -dplyr::any_of(c("Var", "Unit", "col_index"))
      )

    df_plate_qc <- df_plate |>
      dplyr::left_join(
        df_qc_warn,
        by = c("SampleID", "rid", "Panel"),
        relationship = "one-to-one"
      )

  } else {
    df_plate_qc <- df_plate
  }

  ## combine oid and plate qc ----

  df_long <- df_long |>
    dplyr::left_join(
      df_plate_qc,
      by = c("SampleID", "rid", "Panel"),
      relationship = "many-to-one"
    )

  ## df deviation internal controls ----

  if (has_dev_int_ctrl) {

    df_dev_int_ctrl <- df_middle_long$df_dev_int_ctrl |>
      dplyr::left_join(
        df_top_long$df_dev_int_ctrl,
        by = "col_index",
        relationship = "many-to-one"
      ) |>
      dplyr::select(
        -dplyr::all_of(c("Assay", "col_index"))
      ) |>
      tidyr::pivot_wider(
        id_cols = dplyr::all_of(c("SampleID", "rid", "Panel")),
        names_from = dplyr::all_of("Uniprot ID"),
        values_from = dplyr::all_of(data_type)
      )

    df_long <- df_long |>
      dplyr::left_join(
        df_dev_int_ctrl,
        by = c("SampleID", "rid", "Panel"),
        relationship = "many-to-one"
      )
  }

  ## remove rid ----

  df_long <- df_long |>
    dplyr::select(
      -dplyr::all_of("rid")
    )

  ## df bottom ----

  if (has_bottom_mat) {

    df_bottom_merge <- df_bottom_long |>
      dplyr::bind_rows()

    if ("PlateID" %in% colnames(df_bottom_merge)) {
      df_long <- df_long |>
        dplyr::left_join(
          df_bottom_merge,
          by = c("col_index", "PlateID"),
          relationship = "many-to-many"
        )
    } else {
      df_long <- df_long |>
        dplyr::left_join(
          df_bottom_merge,
          by = "col_index",
          relationship = "many-to-many"
        )
    }
  }

  ## Compute Panel_Version and remove col_index ----

  npxs_v <- olink_wide_npxs_v(df = df_head)

  df_long <- df_long |>
    dplyr::mutate(
      Panel_Version = strsplit(x = .data[["Panel"]],
                               split = "(",
                               fixed = TRUE) |>
        lapply(utils::tail, 1L) |>
        unlist() |>
        (\(x) {
          sub(pattern = ")",
              replacement = "",
              x = x,
              fixed = TRUE)
        })()
    ) |>
    dplyr::mutate(
      Panel = strsplit(x = .data[["Panel"]],
                       split = "(",
                       fixed = TRUE) |>
        lapply(utils::head, -1L) |>
        lapply(paste, collapse = "(") |>
        unlist()
    ) |>
    dplyr::mutate(
      `Olink NPX Signature Version` = npxs_v
    ) |>
    dplyr::select(
      -dplyr::all_of("col_index")
    )

  # rename columns ----

  olink_wide_rename_npxs_tmp <- olink_wide_rename_npxs |>
    dplyr::filter(
      .data[["OA_internal"]] %in% colnames(df_long)
    )

  df_long <- df_long |>
    dplyr::rename_with(
      .fn = ~olink_wide_rename_npxs_tmp$NPXS,
      .cols = dplyr::all_of(olink_wide_rename_npxs_tmp$OA_internal)
    )

  # return ----

  list_df_long <- list(
    df_long = df_long,
    df_oid_long = df_oid,
    df_plate_long = df_plate
  )

  list_split_long <- list(
    df_top_long = df_top_long,
    df_middle_long = df_middle_long
  )

  if (has_bottom_mat) {
    list_df_long <- append(x = list_df_long,
                           values = list(df_qc_warn_long = df_qc_warn))

    list_split_long <- append(x = list_split_long,
                              values = list(df_bottom_long = df_bottom_long))
  }

  if (has_int_ctrl) {
    list_df_long <- append(x = list_df_long,
                           values = list(df_int_ctrl_long = df_int_ctrl))
  }

  if (has_dev_int_ctrl) {
    list_df_long <- append(
      x = list_df_long,
      values = list(df_dev_int_ctrl_long = df_dev_int_ctrl)
    )
  }

  list_df_long <- append(x = list_df_long,
                         values = list_split_long)

  return(list_df_long)

}

# returns a nested list with all the components of the long and the wide data
# frames, as well as the full matrices
olink_wide_synthetic <- function(olink_platform,
                                 data_type,
                                 n_panels,
                                 n_assays,
                                 n_samples,
                                 show_dev_int_ctrl = TRUE,
                                 show_int_ctrl = TRUE,
                                 version = 1L) {

  # list of wide df ----

  list_df_wide <- olink_wide(olink_platform = olink_platform,
                             data_type = data_type,
                             n_panels = n_panels,
                             n_assays = n_assays,
                             n_samples = n_samples,
                             show_dev_int_ctrl = show_dev_int_ctrl,
                             show_int_ctrl = show_int_ctrl,
                             version = version)

  # list of long df ----

  if (!("df_bottom_wide" %in% names(list_df_wide))) {
    df_bottom_wide <- NULL
  } else {
    df_bottom_wide <- list_df_wide$df_bottom_wide
  }

  list_df_long <- olink_wide_to_long(
    df_top_wide = list_df_wide$df_top_wide,
    df_middle_wide = list_df_wide$df_middle_wide,
    df_bottom_wide = df_bottom_wide,
    df_head = list_df_wide$df_head_wide,
    olink_platform = olink_platform,
    data_type = data_type,
    version = version
  )

  return(
    list(
      list_df_wide = list_df_wide,
      list_df_long = list_df_long
    )
  )
}
