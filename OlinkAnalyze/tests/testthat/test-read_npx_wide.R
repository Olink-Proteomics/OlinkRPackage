# Help functions ----

# generate vectors with names of columns for assays, plates, qc warning and
# internal controls
npx_wide_col_index <- function(n_panels,
                               n_assays,
                               is_shuffled) {

  n_int_ctrl <- 3L

  # if internal controls and customer assays are shuffled in cols
  if (is_shuffled) {

    assay_start <- 2L
    assay_end <- (n_panels * n_assays) + (n_panels * n_int_ctrl) + 1L
    assay_cols <- paste0("V", assay_start:assay_end)

  } else {

    assay_start <- 2L
    assay_end <- (n_panels * n_assays) + 1L
    assay_cols <- paste0("V", assay_start:assay_end)

  }

  # Plate ID columns
  plate_start <- assay_end + 1L
  plate_end <- plate_start + n_panels - 1L
  plate_cols <- paste0("V", plate_start:plate_end)

  # QC Warning columns
  qc_warn_start <- plate_end + 1L
  qc_warn_end <- qc_warn_start + n_panels - 1L
  qc_warn_cols <- paste0("V", qc_warn_start:qc_warn_end)

  # Internal controls columns
  if (is_shuffled) {
    # if assays and internal controls are shuffled then select randomly from
    # the assays

    int_ctrl_cols <- sample(x = assay_cols,
                            size = (n_panels * n_int_ctrl),
                            replace = FALSE)
    assay_cols <- assay_cols[!(assay_cols %in% int_ctrl_cols)]

  } else {

    int_ctrl_start <- qc_warn_end + 1L
    int_ctrl_end <- int_ctrl_start + (n_panels * n_int_ctrl) - 1L
    int_ctrl_cols <- paste0("V", int_ctrl_start:int_ctrl_end)

  }

  return(
    list(
      assay_cols = assay_cols,
      plate_cols = plate_cols,
      qc_warn_cols = qc_warn_cols,
      int_ctrl_cols = int_ctrl_cols
    )
  )
}

npx_wide_row_index <- function(data_type,
                               n_samples,
                               n_plates) {

  # top_df
  top_start <- 3L
  top_end <- ifelse(data_type == "Quantified", 7L, 6L)

  # df_mid
  sample_start <- top_end + 2L
  sample_end <- sample_start + n_samples - 1L

  # check that df_bottom works
  bottom_start <- sample_end + 2L
  bottom_end <- ifelse(data_type == "Quantified",
                       bottom_start + 4L + (3 * n_plates) - 1,
                       bottom_start + 3L - 1)

  return(
    list(
      top_start = top_start,
      top_end = top_end,
      sample_start = sample_start,
      sample_end = sample_end,
      bottom_start = bottom_start,
      bottom_end = bottom_end
    )
  )
}

## Header matrix ----

# return the top 2x2 matrix in the first two rows of an Olink exceli wide file
npx_wide_head <- function(data_type) {

  df <- dplyr::tibble(
    "V1" = c("Project Name", data_type),
    "V2" = c("Test", NA_character_)
  )

  return(df)
}

## Top matrix ----

# computes the top matrix in an Olink excel wide file. This matrix contains
# metadata about assays such as OlinkID, Assay name, UniProt ID, the Olink panel
# they belong to and the measuring unit.
# The right hand side of this matrix contains data about Plates, QC_Warnings and
# internal controls.
#
# Returns a matrix in wide format.
npx_wide_top <- function(olink_platform,
                         n_panels,
                         n_assays,
                         data_type,
                         show_int_ctrl = TRUE,
                         loc_int_ctrl = "V3",
                         shuffle_assays = FALSE) {

  n_plates <- n_panels

  # number of QC Warnings and Internal controls based on data_type
  if (data_type == "Ct") {
    n_qc_warns <- 0L
    int_ctrl <- character(0)
  } else {
    n_qc_warns <- n_panels
    if (data_type == "NPX") {
      int_ctrl <- character(0)
    } else if (data_type == "Quantified") {
      if (show_int_ctrl == TRUE) {
        int_ctrl <- c("Inc Ctrl", "Amp Ctrl", "Ext Ctrl")
      } else {
        int_ctrl <- character(0)
      }
    }
  }

  # random df ----

  df <- dplyr::tibble(
    "V1" = c("Panel",
             rep(x = paste("Olink ", olink_platform,
                           " Panel", seq_len(n_panels)),
                 each = n_assays),
             paste(rep(paste("Olink ", olink_platform, " Panel"),
                       times = n_plates),
                   seq_len(n_plates)),
             paste(rep(paste("Olink ", olink_platform, " Panel"),
                       times = n_qc_warns),
                   seq_len(n_qc_warns)),
             rep(x = paste("Olink ", olink_platform,
                           " Panel", seq_len(n_panels)),
                 each = length(int_ctrl))),
    "V2" = c("Assay",
             paste0(rep(x = "Assay", times = (n_panels * n_assays)),
                    seq_len(n_panels * n_assays)),
             rep(x = "Plate ID", times = n_plates),
             rep(x = "QC Warning", times = n_qc_warns),
             rep(x = "QC Deviation from median",
                 times = (length(int_ctrl) * n_panels))),
    "V3" = c("Uniprot ID",
             paste0(rep(x = "Uniprot", times = (n_panels * n_assays)),
                    seq_len((n_panels * n_assays))),
             rep(x = NA_character_, times = n_plates),
             rep(x = NA_character_, times = n_qc_warns),
             rep(x = int_ctrl, times = n_panels)),
    "V4" = c("OlinkID",
             paste0(rep(x = "OID", times = (n_panels * n_assays)),
                    seq_len((n_panels * n_assays))),
             rep(x = NA_character_, times = n_plates),
             rep(x = NA_character_, times = n_qc_warns),
             rep(x = NA_character_,
                 times = (length(int_ctrl) * n_panels)))
  )

  # modify df ----

  # if data_type is Quantified then add column "Unit"
  if (data_type == "Quantified") {
    df <- df |>
      dplyr::mutate(
        "V5" = c("Unit",
                 rep(x = "pg/mL", times = (n_panels * n_assays)),
                 rep(x = NA_character_, times = n_plates),
                 rep(x = NA_character_, times = n_qc_warns),
                 rep(x = NA_character_,
                     times = (length(int_ctrl) * n_panels)))
      )
  }

  # if the internal controls should be in column 2 instead of column 3
  if (loc_int_ctrl == "V2"
      && data_type == "Quantified"
      && show_int_ctrl == TRUE) {
    df <- df |>
      dplyr::mutate(
        V2 = dplyr::if_else(.data[["V2"]] == "QC Deviation from median",
                            .data[["V3"]],
                            .data[["V2"]]),
        V3 = dplyr::if_else(.data[["V2"]] %in% .env[["int_ctrl"]],
                            NA_character_,
                            .data[["V3"]])
      )
  }

  # if internal controls and customer assays should be shuffled
  if (shuffle_assays == TRUE) {

    df <- df |>
      # keep first row as is
      dplyr::slice_head(n = 1L) |>
      # shuffle rows with customer assays and internal controls
      dplyr::bind_rows(
        df |>
          dplyr::filter(
            grepl("^OID", .data[["V4"]])
            | .data[["V2"]] %in% .env[["int_ctrl"]]
            | .data[["V3"]] %in% .env[["int_ctrl"]]
          ) |>
          dplyr::slice_sample(prop = 1)
      ) |>
      # append rows with "Plate ID" or "QC Warning" in "V2"
      dplyr::bind_rows(
        df |>
          dplyr::filter(
            .data[["V2"]] %in% c("Plate ID", "QC Warning")
          )
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

# This function modifies the output of npx_wide_top to help with tests.
# Input is the wide matrix, and output is the top matrix separated based on
# assays, plate_id & qc_warning and internal controls.
npx_wide_top_test <- function(df) {

  # modify df ----

  c_names <- colnames(df)

  df <- df |> t()
  colnames(df) <- df[1, ]

  df <- df |>
    dplyr::as_tibble() |>
    dplyr::mutate(
      col_index = c_names
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

  df_plate <- df |>
    dplyr::filter(
      is.na(.data[["OlinkID"]])
      & .data[["Assay"]] %in% c("Plate ID")
    ) |>
    dplyr::select(
      -dplyr::all_of(c("Uniprot ID", "OlinkID"))
    ) |>
    dplyr::rename(
      "Var" = "Assay"
    )

  df_qc_warn <- df |>
    dplyr::filter(
      is.na(.data[["OlinkID"]])
      & .data[["Assay"]] %in% c("QC Warning")
    ) |>
    dplyr::select(
      -dplyr::all_of(c("Uniprot ID", "OlinkID"))
    ) |>
    dplyr::rename(
      "Var" = "Assay"
    )

  df_qc_dev <- df |>
    dplyr::filter(
      is.na(.data[["OlinkID"]])
      & (.data[["Assay"]] == "QC Deviation from median"
         | grepl("ctrl", .data[["Assay"]], ignore.case = TRUE))
    ) |>
    dplyr::select(
      -dplyr::all_of(c("OlinkID"))
    )

  # return ----

  return(
    list(
      df_oid = df_oid,
      df_plate = df_plate,
      df_qc_warn = df_qc_warn,
      df_qc_dev = df_qc_dev
    )
  )

}

## Middle matrix ----

# computes the middle matrix with assays measurements for each sample. The right
# hand side of it should contan information on Plate ID, QC_Warning and internal
# controls.
npx_wide_middle <- function(n_panels,
                            n_assays,
                            n_samples,
                            data_type,
                            show_int_ctrl = TRUE,
                            shuffle_assays = FALSE) {

  # pre-compute variables ----

  n_base_plate <- 88L
  n_sample_plates <- ceiling(x = (n_samples) / n_base_plate) |> as.integer()
  n_int_ctrl <- 3L

  if (data_type == "Ct") {
    qc_warns <- FALSE
    int_ctrl <- FALSE
  } else {
    qc_warns <- TRUE
    if (data_type == "NPX") {
      int_ctrl <- FALSE
    } else if (data_type == "Quantified") {
      if (show_int_ctrl == TRUE) {
        int_ctrl <- TRUE
      } else {
        int_ctrl <- FALSE
      }
    }
  }

  # create parts of df ----

  ## sample id ----

  df_s <- dplyr::tibble(
    "sample_id" = paste0("S", seq_len(n_samples))
  )

  ## quant values ----

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

  ## plate names ----

  plate_name <- paste0(
    rep(x = "Plate", times = (n_base_plate * n_sample_plates)),
    rep(x = seq_len(n_sample_plates), each = n_base_plate)
  )
  plate_name <- plate_name[seq_len(n_samples)]

  panel_name <- paste0("Panel", seq_len(n_panels))

  panel_plate_name <- expand.grid(plate_name,
                                  panel_name,
                                  stringsAsFactors = FALSE)
  colnames(panel_plate_name) <- c("X1", "X2")
  panel_plate_name <- panel_plate_name |>
    dplyr::as_tibble() |>
    dplyr::mutate(X = paste(.data[["X1"]], .data[["X2"]], sep = "_")) |>
    dplyr::pull(.data[["X"]])

  df_p <- matrix(
    data = panel_plate_name,
    nrow = n_samples,
    ncol = n_panels,
    dimnames = list(
      paste0("S", seq_len(n_samples)),
      paste0("P", seq_len(n_panels))
    )
  ) |>
    dplyr::as_tibble()

  rm(plate_name, panel_name, panel_plate_name)

  ## qc warning ----

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

  ## internal controls ----

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

  # combine df ----

  df <- df_s |>
    dplyr::bind_cols(
      df_q
    ) |>
    dplyr::bind_cols(
      df_p
    )

  if (qc_warns == TRUE) {
    df <- df |>
      dplyr::bind_cols(
        df_w
      )
  }

  if (int_ctrl == TRUE) {
    df <- df |>
      dplyr::bind_cols(
        df_i
      )
  }

  # modify df ----

  ## shuffle cols ----

  if (shuffle_assays == TRUE) {

    c_names <- colnames(df)
    c_names_shuffle <- c_names[grepl(pattern = "^Q|^I", x = c_names)]
    c_names_sid <- c_names[1]
    c_names_no_shuffle <- c_names[!(c_names %in% c(c_names_shuffle,
                                                   c_names_sid))]
    c_names_shuffle <- sample(x = c_names_shuffle,
                              size = length(c_names_shuffle),
                              replace = FALSE)
    rm(c_names)

    df <- df |>
      dplyr::select(
        dplyr::all_of(
          c(c_names_sid, c_names_shuffle, c_names_no_shuffle)
        )
      )

  }

  ## rename columns ----

  colnames(df) <- paste0("V", seq_len(ncol(df)))

  ## convert all to character ----

  df <- df |>
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(), ~ as.character(.x)
      )
    )

  # return ----

  return(df)
}

# takes as input the middle matrix and splits it into its components to help
# with the testing.
#
# output contains df_oid, df_pid, df_qc_warn and df_int_ctrl
npx_wide_middle_test <- function(df,
                                 n_panels,
                                 n_assays,
                                 data_type,
                                 is_shuffled,
                                 cname) {

  # columns to split df into ----

  if (is.null(cname)) {
    cname <- npx_wide_col_index(n_panels = n_panels,
                                n_assays = n_assays,
                                is_shuffled = is_shuffled)
  }

  # split df ----

  # assays
  df_oid <- df |>
    dplyr::select(
      dplyr::all_of(c("V1", cname$assay_cols))
    ) |>
    dplyr::rename(
      "SampleID" = dplyr::all_of("V1")
    ) |>
    tidyr::pivot_longer(
      -dplyr::all_of("SampleID"),
      names_to = "col_index",
      values_to = data_type
    )

  # plate id
  df_plate <- df |>
    dplyr::select(
      dplyr::all_of(c("V1", cname$plate_cols))
    ) |>
    dplyr::rename(
      "SampleID" = dplyr::all_of("V1")
    ) |>
    tidyr::pivot_longer(
      -dplyr::all_of("SampleID"),
      names_to = "col_index",
      values_to = "PlateID"
    )

  list_df <- list(
    df_oid = df_oid,
    df_plate = df_plate
  )

  if (data_type != "Ct") {

    # qc warning
    df_qc_warn <- df |>
      dplyr::select(
        dplyr::all_of(c("V1", cname$qc_warn_cols))
      ) |>
      dplyr::rename(
        "SampleID" = dplyr::all_of("V1")
      ) |>
      tidyr::pivot_longer(
        -dplyr::all_of("SampleID"),
        names_to = "col_index",
        values_to = "QC_Warning"
      )

    list_df <- append(x = list_df,
                      values = list(df_qc_warn = df_qc_warn))

    if (data_type == "Quantified") {

      # internal controls
      df_int_ctrl <- df |>
        dplyr::select(
          dplyr::all_of(c("V1", cname$int_ctrl_cols))
        ) |>
        dplyr::rename(
          "SampleID" = dplyr::all_of("V1")
        ) |>
        tidyr::pivot_longer(
          -dplyr::all_of("SampleID"),
          names_to = "col_index",
          values_to = data_type
        )

      list_df <- append(x = list_df,
                        values = list(df_int_ctrl = df_int_ctrl))
    }

  }

  # return ----

  return(list_df)

}

## Bottom matrix ----

# computes the bottom matrix with LOD, ULOQ, LLOQ, and plate-specific QC metrics
npx_wide_bottom <- function(n_plates,
                            n_panels,
                            n_assays,
                            data_type) {
  # this is the last column of the bottom matrix.
  # commonly it contains data when data is Quantified, and not
  # when it is NPX.
  last_v <- paste0("V", n_assays * n_panels + 2)

  # create matrix data that are specific to the data type
  if (data_type == "NPX") {

    # df_dt ----

    df_dt <- matrix(
      data = rnorm(n = (n_panels * n_assays)),
      nrow = 1L,
      ncol = (n_assays * n_panels),
      dimnames = list(
        "LOD",
        paste0("V", 2L:((n_assays * n_panels) + 1L))
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

  } else if (data_type == "Quantified") {

    # df_rep ("Assay warning", "Lowest quantifiable level", "Plate LOD") ----

    df_rep <- matrix(
      data = sample(x = c("Pass", "Warn"),
                    size = (n_plates * n_panels * n_assays),
                    replace = TRUE),
      nrow = n_plates,
      ncol = (n_assays * n_panels),
      dimnames = list(
        rep(x = "Assay warning", times = n_plates),
        paste0("V", 2L:((n_assays * n_panels) + 1L))
      )
    ) |>
      dplyr::as_tibble(
        rownames = "V1"
      ) |>
      dplyr::mutate(
        {{last_v}} := paste("Plate", seq_len(n_plates)) # nolint object_usage_linter
      ) |>
      dplyr::bind_rows(
        matrix(
          data = rnorm(n = (n_plates * n_panels * n_assays)),
          nrow = n_plates,
          ncol = (n_assays * n_panels),
          dimnames = list(
            rep(x = "Lowest quantifiable level", times = n_plates),
            paste0("V", 2L:((n_assays * n_panels) + 1L))
          )
        ) |>
          dplyr::as_tibble(
            rownames = "V1"
          ) |>
          dplyr::mutate(
            {{last_v}} := paste("Plate", seq_len(n_plates)) # nolint object_usage_linter
          ) |>
          dplyr::mutate(
            dplyr::across(
              dplyr::everything(), ~ as.character(.x)
            )
          )
      ) |>
      dplyr::bind_rows(
        matrix(
          data = rnorm(n = (n_plates * n_panels * n_assays)),
          nrow = n_plates,
          ncol = (n_assays * n_panels),
          dimnames = list(
            rep(x = "Plate LOD", times = n_plates),
            paste0("V", 2L:((n_assays * n_panels) + 1L))
          )
        ) |>
          dplyr::as_tibble(
            rownames = "V1"
          ) |>
          dplyr::mutate(
            {{last_v}} := paste("Plate", seq_len(n_plates)) # nolint object_usage_linter
          ) |>
          dplyr::mutate(
            dplyr::across(
              dplyr::everything(),
              ~ as.character(.x)
            )
          )
      )

    # df_spec ("LLOQ", "ULOQ")----
    df_spec <- matrix(
      data = rnorm(n = (n_panels * n_assays)),
      nrow = 1L,
      ncol = (n_assays * n_panels),
      dimnames = list(
        "LLOQ",
        paste0("V", 2L:((n_assays * n_panels) + 1L))
      )
    ) |>
      dplyr::as_tibble(
        rownames = "V1"
      ) |>
      dplyr::bind_rows(
        matrix(
          data = rnorm(n = (n_panels * n_assays)),
          nrow = 1L,
          ncol = (n_assays * n_panels),
          dimnames = list(
            "ULOQ",
            paste0("V", 2L:((n_assays * n_panels) + 1L))
          )
        ) |>
          dplyr::as_tibble(
            rownames = "V1"
          )
      ) |>
      dplyr::mutate(
        dplyr::across(
          dplyr::everything(), ~ as.character(.x)
        )
      )

    # df_dt ----

    df_dt <- dplyr::bind_rows(df_rep, df_spec)

  }

  # df shared ("Missing Data freq.", "Normalization") ----

  df_shared <- matrix(
    data = rnorm(n = (n_panels * n_assays)),
    nrow = 1L,
    ncol = (n_assays * n_panels),
    dimnames = list(
      "Missing Data freq.",
      paste0("V", 2L:((n_assays * n_panels) + 1L))
    )
  ) |>
    dplyr::as_tibble(
      rownames = "V1"
    ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(), ~ as.character(.x)
      )
    ) |>
    dplyr::bind_rows(
      matrix(
        data = rep(x = "Intensity", times = (n_panels * n_assays)),
        nrow = 1L,
        ncol = (n_assays * n_panels),
        dimnames = list(
          "Normalization",
          paste0("V", 2L:((n_assays * n_panels) + 1L))
        )
      ) |>
        dplyr::as_tibble(
          rownames = "V1"
        )
    )

  # df ----

  df <- dplyr::bind_rows(df_dt, df_shared)

  # return ----

  return(df)

}

npx_wide_bottom_test <- function(df,
                                 data_type,
                                 col_split) {

  # remove all NA columns ----

  df <- remove_all_na_cols(df = df)

  # df bottom matrix with plate-specific data ----

  # This is done for Quantified data only
  if (col_split %in% colnames(df)
      && data_type == "Quantified") {

    df_q <- df |>
      # keep only rows to be pivoted
      dplyr::filter(
        !is.na(.data[[col_split]])
      )

    # for each variable in V1 and do a pivot_longer
    df_q <- lapply(unique(df_q$V1), function(x) {
      df_q |>
        dplyr::filter(
          .data[["V1"]] == .env[["x"]]
        ) |>
        dplyr::select(
          -dplyr::all_of("V1")
        ) |>
        tidyr::pivot_longer(
          -dplyr::all_of(col_split),
          names_to = "col_index",
          values_to = x
        ) |>
        dplyr::rename(
          "Plate ID" = dplyr::all_of(col_split)
        )
    })

    # left join all data frames from the list
    df_q <- Reduce(f = function(df_1, df_2) {
      dplyr::left_join(x = df_1,
                       y = df_2,
                       by = c("Plate ID", "col_index"),
                       relationship = "one-to-one")
    },
    x = df_q)

    # remove the rows with plate-specific data and the col_split column
    df <- df |>
      dplyr::filter(
        is.na(.data[[col_split]])
      ) |>
      dplyr::select(
        -dplyr::all_of(col_split)
      )

  }

  # df without plate-specific info ----

  # transpose and add column names
  df_t <- t(df)
  colnames(df_t) <- df_t[1L, ]

  # fix column names and remove extra rows
  df_t <- df_t |>
    dplyr::as_tibble(
      rownames = "col_index"
    ) |>
    dplyr::mutate(
      col_index = dplyr::if_else(
        .data[["col_index"]] == "V1",
        "col_index",
        .data[["col_index"]]
      )
    ) |>
    dplyr::slice(
      2L:dplyr::n()
    )

  # join df_q and df_t if needed ----

  if (exists("df_q")) {

    df_t <- dplyr::left_join(
      x = df_t,
      y = df_q,
      by = "col_index",
      relationship = "one-to-many"
    )

  }

  # finalize df_t  ----

  # sort columns of df_t
  df_t <- df_t |>
    dplyr::select(
      order(colnames(df_t))
    )

  # return ----

  return(df_t)

}

## Full df ----

# combines the full matrix mimicing and Olink excel wide file.
npx_wide <- function(olink_platform,
                     data_type,
                     n_panels,
                     n_assays,
                     n_samples,
                     show_int_ctrl = TRUE,
                     loc_int_ctrl = "V3",
                     shuffle_assays = FALSE) {

  n_base_plate <- 88L
  n_plates <- ceiling(x = (n_samples) / n_base_plate) |> as.integer()

  # head ----

  df_head <- npx_wide_head(data_type = data_type)

  # top matrix ----

  df_top <- npx_wide_top(olink_platform = olink_platform,
                         n_panels = n_panels,
                         n_assays = n_assays,
                         data_type = data_type,
                         show_int_ctrl = show_int_ctrl,
                         loc_int_ctrl = loc_int_ctrl,
                         shuffle_assays = shuffle_assays)

  # middle matrix ----

  df_middle <- npx_wide_middle(n_panels = n_panels,
                               n_assays = n_assays,
                               n_samples = n_samples,
                               data_type = data_type,
                               show_int_ctrl = show_int_ctrl,
                               shuffle_assays = shuffle_assays)

  # bottom matrix ----

  if (data_type != "Ct") {
    df_bottom <- npx_wide_bottom(n_plates = n_plates,
                                 n_panels = n_panels,
                                 n_assays = n_assays,
                                 data_type = data_type)
  }

  # na rows ----

  df_na <- dplyr::tibble(
    "X" = rep(x = NA_character_, times = ncol(df_top))
  ) |>
    t()
  colnames(df_na) <- paste0("V", seq_len(ncol(df_top)))
  df_na <- dplyr::as_tibble(df_na)

  # combine df ----

  df <- df_head |>
    dplyr::bind_rows(
      df_top
    ) |>
    dplyr::bind_rows(
      df_na
    ) |>
    dplyr::bind_rows(
      df_middle
    )

  if (data_type != "Ct") {
    df <- df |>
      dplyr::bind_rows(
        df_na
      ) |>
      dplyr::bind_rows(
        df_bottom
      )
  }

  # return ----

  return(df)

}

# Test read_npx_wide_split_row ----

test_that(
  "read_npx_wide_split_row - works as expecetd",
  {
    ## NPX ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df
        data_t <- "NPX"
        n_sample <- 88L
        n_plates <- ceiling(x = (n_sample / 88L)) |> as.integer()

        df <- npx_wide(olink_platform = "Target 48",
                       data_type = data_t,
                       n_panels = 2L,
                       n_assays = 45L,
                       n_samples = n_sample,
                       show_int_ctrl = TRUE,
                       loc_int_ctrl = "V3",
                       shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write df
        writexl::write_xlsx(
          x = df,
          path = wide_excel,
          col_names = FALSE,
          format_headers = FALSE
        )

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs for NPX
        expect_no_condition(
          object = df_out <- read_npx_wide_split_row(file = wide_excel,
                                                     data_type = data_t,
                                                     format_spec = format_s)
        )

        # check that output exists
        expect_true(object = exists("df_out"))

        # get expected row indexes
        row_indx <- npx_wide_row_index(data_type = data_t,
                                       n_samples = n_sample,
                                       n_plates = n_plates)

        # check that df_top works
        expect_identical(
          object = df_out$df_top,
          expected = df |>
            dplyr::slice(
              row_indx$top_start:row_indx$top_end
            )
        )

        # check that df_mid works
        expect_identical(
          object = df_out$df_mid,
          expected = df |>
            dplyr::slice(
              row_indx$sample_start:row_indx$sample_end
            )
        )

        # check that df_bottom works
        expect_identical(
          object = df_out$df_bottom,
          expected = df |>
            dplyr::slice(
              row_indx$bottom_start:row_indx$bottom_end
            )
        )

      }
    )

    ## NPX shuffle ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df
        data_t <- "NPX"
        n_sample <- 88L
        n_plates <- ceiling(x = (n_sample / 88L)) |> as.integer()

        df <- npx_wide(olink_platform = "Target 48",
                       data_type = data_t,
                       n_panels = 2L,
                       n_assays = 45L,
                       n_samples = n_sample,
                       show_int_ctrl = TRUE,
                       loc_int_ctrl = "V3",
                       shuffle_assays = TRUE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write df
        writexl::write_xlsx(
          x = df,
          path = wide_excel,
          col_names = FALSE,
          format_headers = FALSE
        )

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs for NPX
        expect_no_condition(
          object = df_out <- read_npx_wide_split_row(file = wide_excel,
                                                     data_type = data_t,
                                                     format_spec = format_s)
        )

        # check that output exists
        expect_true(object = exists("df_out"))

        # get expected row indexes
        row_indx <- npx_wide_row_index(data_type = data_t,
                                       n_samples = n_sample,
                                       n_plates = n_plates)

        # check that df_top works
        expect_identical(
          object = df_out$df_top,
          expected = df |>
            dplyr::slice(
              row_indx$top_start:row_indx$top_end
            )
        )

        # check that df_mid works
        expect_identical(
          object = df_out$df_mid,
          expected = df |>
            dplyr::slice(
              row_indx$sample_start:row_indx$sample_end
            )
        )

        # check that df_bottom works
        expect_identical(
          object = df_out$df_bottom,
          expected = df |>
            dplyr::slice(
              row_indx$bottom_start:row_indx$bottom_end
            )
        )

      }
    )

    ## Ct ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df
        data_t <- "Ct"
        n_sample <- 88L
        n_plates <- ceiling(x = (n_sample / 88L)) |> as.integer()

        df <- npx_wide(olink_platform = "Target 48",
                       data_type = data_t,
                       n_panels = 2L,
                       n_assays = 45L,
                       n_samples = n_sample,
                       show_int_ctrl = TRUE,
                       loc_int_ctrl = "V3",
                       shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write df
        writexl::write_xlsx(
          x = df,
          path = wide_excel,
          col_names = FALSE,
          format_headers = FALSE
        )

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs for Ct
        expect_no_condition(
          object = df_out <- read_npx_wide_split_row(file = wide_excel,
                                                     data_type = data_t,
                                                     format_spec = format_s)
        )

        # check that output exists
        expect_true(object = exists("df_out"))

        # get expected row indexes
        row_indx <- npx_wide_row_index(data_type = data_t,
                                       n_samples = n_sample,
                                       n_plates = n_plates)

        # check that df_top works
        expect_identical(
          object = df_out$df_top,
          expected = df |>
            dplyr::slice(
              row_indx$top_start:row_indx$top_end
            )
        )

        # check that df_mid works
        expect_identical(
          object = df_out$df_mid,
          expected = df |>
            dplyr::slice(
              row_indx$sample_start:row_indx$sample_end
            )
        )

        # check that df_bottom works
        expect_true(object = is.null(df_out$df_bottom))

      }
    )

    ## Ct shuffle ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df
        data_t <- "Ct"
        n_sample <- 88L
        n_plates <- ceiling(x = (n_sample / 88L)) |> as.integer()

        df <- npx_wide(olink_platform = "Target 48",
                       data_type = data_t,
                       n_panels = 2L,
                       n_assays = 45L,
                       n_samples = n_sample,
                       show_int_ctrl = TRUE,
                       loc_int_ctrl = "V3",
                       shuffle_assays = TRUE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write df
        writexl::write_xlsx(
          x = df,
          path = wide_excel,
          col_names = FALSE,
          format_headers = FALSE
        )

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs for Ct
        expect_no_condition(
          object = df_out <- read_npx_wide_split_row(file = wide_excel,
                                                     data_type = data_t,
                                                     format_spec = format_s)
        )

        # check that output exists
        expect_true(object = exists("df_out"))

        # get expected row indexes
        row_indx <- npx_wide_row_index(data_type = data_t,
                                       n_samples = n_sample,
                                       n_plates = n_plates)

        # check that df_top works
        expect_identical(
          object = df_out$df_top,
          expected = df |>
            dplyr::slice(
              row_indx$top_start:row_indx$top_end
            )
        )

        # check that df_mid works
        expect_identical(
          object = df_out$df_mid,
          expected = df |>
            dplyr::slice(
              row_indx$sample_start:row_indx$sample_end
            )
        )

        # check that df_bottom works
        expect_true(object = is.null(df_out$df_bottom))

      }
    )

    ## Quantified wo int ctrl ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df
        data_t <- "Quantified"
        n_sample <- 88L
        n_plates <- ceiling(x = (n_sample / 88L)) |> as.integer()

        df <- npx_wide(olink_platform = "Target 48",
                       data_type = data_t,
                       n_panels = 2L,
                       n_assays = 45L,
                       n_samples = n_sample,
                       show_int_ctrl = FALSE,
                       loc_int_ctrl = "V3",
                       shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write file
        writexl::write_xlsx(
          x = df,
          path = wide_excel,
          col_names = FALSE,
          format_headers = FALSE
        )

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs for Quantified
        expect_no_condition(
          object = df_out <- read_npx_wide_split_row(file = wide_excel,
                                                     data_type = data_t,
                                                     format_spec = format_s)
        )

        # check that output exists
        expect_true(object = exists("df_out"))

        # get expected row indexes
        row_indx <- npx_wide_row_index(data_type = data_t,
                                       n_samples = n_sample,
                                       n_plates = n_plates)

        # check that df_top works
        expect_identical(
          object = df_out$df_top,
          expected = df |>
            dplyr::slice(
              row_indx$top_start:row_indx$top_end
            )
        )

        # check that df_mid works
        expect_identical(
          object = df_out$df_mid,
          expected = df |>
            dplyr::slice(
              row_indx$sample_start:row_indx$sample_end
            )
        )

        # check that df_bottom works
        expect_identical(
          object = df_out$df_bottom,
          expected = df |>
            dplyr::slice(
              row_indx$bottom_start:row_indx$bottom_end
            )
        )

      }
    )

    ## Quantified w int ctrl ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df
        data_t <- "Quantified"
        n_sample <- 88L
        n_plates <- ceiling(x = (n_sample / 88L)) |> as.integer()

        df <- npx_wide(olink_platform = "Target 48",
                       data_type = data_t,
                       n_panels = 2L,
                       n_assays = 45L,
                       n_samples = n_sample,
                       show_int_ctrl = TRUE,
                       loc_int_ctrl = "V3",
                       shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write file
        writexl::write_xlsx(
          x = df,
          path = wide_excel,
          col_names = FALSE,
          format_headers = FALSE
        )

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs for Quantified
        expect_no_condition(
          object = df_out <- read_npx_wide_split_row(file = wide_excel,
                                                     data_type = data_t,
                                                     format_spec = format_s)
        )

        # check that output exists
        expect_true(object = exists("df_out"))

        # get expected row indexes
        row_indx <- npx_wide_row_index(data_type = data_t,
                                       n_samples = n_sample,
                                       n_plates = n_plates)

        # check that df_top works
        expect_identical(
          object = df_out$df_top,
          expected = df |>
            dplyr::slice(
              row_indx$top_start:row_indx$top_end
            )
        )

        # check that df_mid works
        expect_identical(
          object = df_out$df_mid,
          expected = df |>
            dplyr::slice(
              row_indx$sample_start:row_indx$sample_end
            )
        )

        # check that df_bottom works
        expect_identical(
          object = df_out$df_bottom,
          expected = df |>
            dplyr::slice(
              row_indx$bottom_start:row_indx$bottom_end
            )
        )

      }
    )

    ## Quantified w int ctrl in V2 ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df
        data_t <- "Quantified"
        n_sample <- 88L
        n_plates <- ceiling(x = (n_sample / 88L)) |> as.integer()

        df <- npx_wide(olink_platform = "Target 48",
                       data_type = data_t,
                       n_panels = 2L,
                       n_assays = 45L,
                       n_samples = n_sample,
                       show_int_ctrl = TRUE,
                       loc_int_ctrl = "V2",
                       shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write file
        writexl::write_xlsx(
          x = df,
          path = wide_excel,
          col_names = FALSE,
          format_headers = FALSE
        )

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs for Quantified
        expect_no_condition(
          object = df_out <- read_npx_wide_split_row(file = wide_excel,
                                                     data_type = data_t,
                                                     format_spec = format_s)
        )

        # check that output exists
        expect_true(object = exists("df_out"))

        # get expected row indexes
        row_indx <- npx_wide_row_index(data_type = data_t,
                                       n_samples = n_sample,
                                       n_plates = n_plates)

        # check that df_top works
        expect_identical(
          object = df_out$df_top,
          expected = df |>
            dplyr::slice(
              row_indx$top_start:row_indx$top_end
            )
        )

        # check that df_mid works
        expect_identical(
          object = df_out$df_mid,
          expected = df |>
            dplyr::slice(
              row_indx$sample_start:row_indx$sample_end
            )
        )

        # check that df_bottom works
        expect_identical(
          object = df_out$df_bottom,
          expected = df |>
            dplyr::slice(
              row_indx$bottom_start:row_indx$bottom_end
            )
        )

      }
    )

    ## Quantified w int ctrl in V2 shuffled ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df
        data_t <- "Quantified"
        n_sample <- 88L
        n_plates <- ceiling(x = (n_sample / 88L)) |> as.integer()

        df <- npx_wide(olink_platform = "Target 48",
                       data_type = data_t,
                       n_panels = 2L,
                       n_assays = 45L,
                       n_samples = n_sample,
                       show_int_ctrl = TRUE,
                       loc_int_ctrl = "V2",
                       shuffle_assays = TRUE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write file
        writexl::write_xlsx(
          x = df,
          path = wide_excel,
          col_names = FALSE,
          format_headers = FALSE
        )

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs for Quantified
        expect_no_condition(
          object = df_out <- read_npx_wide_split_row(file = wide_excel,
                                                     data_type = data_t,
                                                     format_spec = format_s)
        )

        # check that output exists
        expect_true(object = exists("df_out"))

        # get expected row indexes
        row_indx <- npx_wide_row_index(data_type = data_t,
                                       n_samples = n_sample,
                                       n_plates = n_plates)

        # check that df_top works
        expect_identical(
          object = df_out$df_top,
          expected = df |>
            dplyr::slice(
              row_indx$top_start:row_indx$top_end
            )
        )

        # check that df_mid works
        expect_identical(
          object = df_out$df_mid,
          expected = df |>
            dplyr::slice(
              row_indx$sample_start:row_indx$sample_end
            )
        )

        # check that df_bottom works
        expect_identical(
          object = df_out$df_bottom,
          expected = df |>
            dplyr::slice(
              row_indx$bottom_start:row_indx$bottom_end
            )
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_split_row - no or too many all-NA rows",
  {
    ## No all-NA rows ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # write random df
        data_t <- "NPX"

        df <- npx_wide(olink_platform = "Target 48",
                       data_type = data_t,
                       n_panels = 2L,
                       n_assays = 45L,
                       n_samples = 88L,
                       show_int_ctrl = TRUE,
                       loc_int_ctrl = "V3",
                       shuffle_assays = FALSE) |>
          dplyr::filter(
            is.na(.data[["V1"]])
          )

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        writexl::write_xlsx(
          x = df,
          path = wide_excel,
          col_names = FALSE,
          format_headers = FALSE
        )

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs for NPX
        expect_error(
          object = read_npx_wide_split_row(file = wide_excel,
                                           data_type = data_t,
                                           format_spec = format_s),
          regexp = "We identified 0 rows with all columns `NA` in file"
        )
      }
    )

    ## Too many all-NA rows ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # write random df
        data_t <- "NPX"

        df <- npx_wide(olink_platform = "Target 48",
                       data_type = data_t,
                       n_panels = 2L,
                       n_assays = 45L,
                       n_samples = 88L,
                       show_int_ctrl = TRUE,
                       loc_int_ctrl = "V3",
                       shuffle_assays = FALSE) |>
          dplyr::mutate(
            V1 = dplyr::if_else(.data[["V1"]] == data_t,
                                NA_character_,
                                .data[["V1"]])
          )

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        writexl::write_xlsx(
          x = df,
          path = wide_excel,
          col_names = FALSE,
          format_headers = FALSE
        )

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs for NPX
        expect_error(
          object = read_npx_wide_split_row(file = wide_excel,
                                           data_type = data_t,
                                           format_spec = format_s),
          regexp = "We identified 3 rows with all columns `NA` in file"
        )
      }
    )

  }
)

test_that(
  "read_npx_wide_split_row - exactly as many all-NA rows as expected",
  {
    ## NPX ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # write random df
        data_t <- "Ct"

        npx_wide(olink_platform = "Target 48",
                 data_type = data_t,
                 n_panels = 2L,
                 n_assays = 45L,
                 n_samples = 88L,
                 show_int_ctrl = TRUE,
                 loc_int_ctrl = "V3",
                 shuffle_assays = FALSE) |>
          writexl::write_xlsx(
            path = wide_excel,
            col_names = FALSE,
            format_headers = FALSE
          )

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == "NPX")

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs for NPX
        expect_error(
          object = read_npx_wide_split_row(file = wide_excel,
                                           data_type = "NPX",
                                           format_spec = format_s),
          regexp = "while we expected 2"
        )

      }
    )

    ## Ct ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # write random df
        data_t <- "NPX"

        npx_wide(olink_platform = "Target 48",
                 data_type = data_t,
                 n_panels = 2L,
                 n_assays = 45L,
                 n_samples = 88L,
                 show_int_ctrl = TRUE,
                 loc_int_ctrl = "V3",
                 shuffle_assays = FALSE) |>
          writexl::write_xlsx(
            path = wide_excel,
            col_names = FALSE,
            format_headers = FALSE
          )

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == "Ct")

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs for Ct
        expect_error(
          object = read_npx_wide_split_row(file = wide_excel,
                                           data_type = "Ct",
                                           format_spec = format_s),
          regexp = "while we expected 1"
        )

      }
    )

    ## Quantified ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # write random df
        data_t <- "Ct"

        npx_wide(olink_platform = "Target 48",
                 data_type = data_t,
                 n_panels = 2L,
                 n_assays = 45L,
                 n_samples = 88L,
                 show_int_ctrl = TRUE,
                 loc_int_ctrl = "V3",
                 shuffle_assays = FALSE) |>
          writexl::write_xlsx(
            path = wide_excel,
            col_names = FALSE,
            format_headers = FALSE
          )

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == "Quantified")

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs for Quantified
        expect_error(
          object = read_npx_wide_split_row(file = wide_excel,
                                           data_type = "Quantified",
                                           format_spec = format_s),
          regexp = "while we expected 2"
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_split_row - all-NA rows are not consecutive",
  {
    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # write random df
        data_t <- "NPX"

        npx_wide(olink_platform = "Target 48",
                 data_type = data_t,
                 n_panels = 2L,
                 n_assays = 45L,
                 n_samples = 88L,
                 show_int_ctrl = TRUE,
                 loc_int_ctrl = "V3",
                 shuffle_assays = FALSE) |>
          dplyr::filter(
            !grepl(pattern = "^S", x = .data[["V1"]])
          ) |>
          writexl::write_xlsx(
            path = wide_excel,
            col_names = FALSE,
            format_headers = FALSE
          )

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs for NPX
        expect_error(
          object = read_npx_wide_split_row(file = wide_excel,
                                           data_type = data_t,
                                           format_spec = format_s),
          regexp = "Consecutive rows with all columns NA."
        )

      }
    )

  }
)

# Test read_npx_wide_check_top ----

test_that(
  "read_npx_wide_check_top - works as expecetd",
  {
    ## NPX ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df ----

        o_platform <- "Target 48"
        n_panel <- 3L
        n_assay <- 45L
        data_t <- "NPX"

        df <- npx_wide_top(olink_platform = o_platform,
                           n_panels = n_panel,
                           n_assays = n_assay,
                           data_type = data_t,
                           show_int_ctrl = FALSE,
                           loc_int_ctrl = "V3",
                           shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write file
        writeLines("foo", wide_excel)

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs for NPX
        expect_no_condition(
          object = read_npx_wide_check_top(df = df,
                                           file = wide_excel,
                                           data_type = data_t,
                                           format_spec = format_s)
        )

      }
    )

    ## Ct ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df ----

        o_platform <- "Target 48"
        n_panel <- 3L
        n_assay <- 45L
        data_t <- "Ct"

        df <- npx_wide_top(olink_platform = o_platform,
                           n_panels = n_panel,
                           n_assays = n_assay,
                           data_type = data_t,
                           show_int_ctrl = FALSE,
                           loc_int_ctrl = "V3",
                           shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write file
        writeLines("foo", wide_excel)

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs for NPX
        expect_no_condition(
          object = read_npx_wide_check_top(df = df,
                                           file = wide_excel,
                                           data_type = data_t,
                                           format_spec = format_s)
        )

      }
    )

    ## Quantified wo int ctrl ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df ----

        o_platform <- "Target 48"
        n_panel <- 3L
        n_assay <- 45L
        data_t <- "Quantified"

        df <- npx_wide_top(olink_platform = o_platform,
                           n_panels = n_panel,
                           n_assays = n_assay,
                           data_type = data_t,
                           show_int_ctrl = FALSE,
                           loc_int_ctrl = "V3",
                           shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write file
        writeLines("foo", wide_excel)

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs
        expect_no_condition(
          object = read_npx_wide_check_top(df = df,
                                           file = wide_excel,
                                           data_type = data_t,
                                           format_spec = format_s)
        )

      }
    )

    ## Quantified w int ctrl ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df ----

        o_platform <- "Target 48"
        n_panel <- 3L
        n_assay <- 45L
        data_t <- "Quantified"

        df <- npx_wide_top(olink_platform = o_platform,
                           n_panels = n_panel,
                           n_assays = n_assay,
                           data_type = data_t,
                           show_int_ctrl = TRUE,
                           loc_int_ctrl = "V3",
                           shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write file
        writeLines("foo", wide_excel)

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs
        expect_no_condition(
          object = read_npx_wide_check_top(df = df,
                                           file = wide_excel,
                                           data_type = data_t,
                                           format_spec = format_s)
        )

      }
    )

    ## Quantified w int ctrl shuffled ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df ----

        o_platform <- "Target 48"
        n_panel <- 3L
        n_assay <- 45L
        data_t <- "Quantified"

        df <- npx_wide_top(olink_platform = o_platform,
                           n_panels = n_panel,
                           n_assays = n_assay,
                           data_type = data_t,
                           show_int_ctrl = TRUE,
                           loc_int_ctrl = "V3",
                           shuffle_assays = TRUE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write file
        writeLines("foo", wide_excel)

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs
        expect_no_condition(
          object = read_npx_wide_check_top(df = df,
                                           file = wide_excel,
                                           data_type = data_t,
                                           format_spec = format_s)
        )

      }
    )

    ## Quantified w int ctrl in V2 ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df ----

        o_platform <- "Target 48"
        n_panel <- 3L
        n_assay <- 45L
        data_t <- "Quantified"

        df <- npx_wide_top(olink_platform = o_platform,
                           n_panels = n_panel,
                           n_assays = n_assay,
                           data_type = data_t,
                           show_int_ctrl = TRUE,
                           loc_int_ctrl = "V2",
                           shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write file
        writeLines("foo", wide_excel)

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs
        expect_no_condition(
          object = read_npx_wide_check_top(df = df,
                                           file = wide_excel,
                                           data_type = data_t,
                                           format_spec = format_s)
        )

      }
    )

    ## Quantified w int ctrl shuffled and in V2 ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df ----

        o_platform <- "Target 48"
        n_panel <- 3L
        n_assay <- 45L
        data_t <- "Quantified"

        df <- npx_wide_top(olink_platform = o_platform,
                           n_panels = n_panel,
                           n_assays = n_assay,
                           data_type = data_t,
                           show_int_ctrl = TRUE,
                           loc_int_ctrl = "V2",
                           shuffle_assays = TRUE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write file
        writeLines("foo", wide_excel)

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs
        expect_no_condition(
          object = read_npx_wide_check_top(df = df,
                                           file = wide_excel,
                                           data_type = data_t,
                                           format_spec = format_s)
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_check_top - missing labels in V1",
  {
    ## NPX or Ct ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df ----

        o_platform <- "Target 48"
        n_panel <- 3L
        n_assay <- 45L
        data_t <- "NPX"

        df <- npx_wide_top(olink_platform = o_platform,
                           n_panels = n_panel,
                           n_assays = n_assay,
                           data_type = data_t,
                           show_int_ctrl = FALSE,
                           loc_int_ctrl = "V3",
                           shuffle_assays = FALSE) |>
          dplyr::filter(
            .data[["V1"]] != "OlinkID"
          )

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write file
        writeLines("foo", wide_excel)

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs
        expect_error(
          object = read_npx_wide_check_top(df = df,
                                           file = wide_excel,
                                           data_type = data_t,
                                           format_spec = format_s),
          regexp = "Column 1 of of the top matrix with assay metadata in file"
        )

      }
    )

    ## Quantified ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df ----

        o_platform <- "Target 48"
        n_panel <- 3L
        n_assay <- 45L
        data_t <- "Quantified"

        df <- npx_wide_top(olink_platform = o_platform,
                           n_panels = n_panel,
                           n_assays = n_assay,
                           data_type = data_t,
                           show_int_ctrl = TRUE,
                           loc_int_ctrl = "V3",
                           shuffle_assays = FALSE)

        add_row <- dplyr::tibble(
          "X" = c("Extra_Row",
                  rep(x = "A", times = n_panel * n_assay),
                  rep(x = NA_character_, times = n_panel),
                  rep(x = NA_character_, times = n_panel),
                  rep(x = NA_character_, times = 3L * n_panel))
        ) |>
          t()
        rownames(add_row) <- NULL
        colnames(add_row) <- paste0("V", seq_len(ncol(df)))
        add_row <- dplyr::as_tibble(add_row)

        df <- df |>
          dplyr::bind_rows(
            add_row
          )

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write file
        writeLines("foo", wide_excel)

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs
        expect_error(
          object = read_npx_wide_check_top(df = df,
                                           file = wide_excel,
                                           data_type = data_t,
                                           format_spec = format_s),
          regexp = "Column 1 of of the top matrix with assay metadata in file"
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_check_top - missing labels in rows 2 & 3",
  {
    ## NPX ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df ----

        o_platform <- "Target 48"
        n_panel <- 3L
        n_assay <- 45L
        data_t <- "NPX"

        keep_cols <- paste0("V", seq_len((n_panel * n_assay) + 1L + n_panel))

        df <- npx_wide_top(olink_platform = o_platform,
                           n_panels = n_panel,
                           n_assays = n_assay,
                           data_type = data_t,
                           show_int_ctrl = FALSE,
                           loc_int_ctrl = "V3",
                           shuffle_assays = FALSE) |>
          dplyr::select(
            dplyr::all_of(keep_cols)
          )

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write file
        writeLines("foo", wide_excel)

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs
        expect_error(
          object = read_npx_wide_check_top(df = df,
                                           file = wide_excel,
                                           data_type = data_t,
                                           format_spec = format_s),
          regexp = "Columns 2 and 3 of of the top matrix with assay metadata in"
        )

      }
    )

    ## Ct ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df ----

        o_platform <- "Target 48"
        n_panel <- 3L
        n_assay <- 45L
        data_t <- "Ct"

        keep_cols <- paste0("V", seq_len((n_panel * n_assay) + 1L))

        df <- npx_wide_top(olink_platform = o_platform,
                           n_panels = n_panel,
                           n_assays = n_assay,
                           data_type = data_t,
                           show_int_ctrl = FALSE,
                           loc_int_ctrl = "V3",
                           shuffle_assays = FALSE) |>
          dplyr::select(
            dplyr::all_of(keep_cols)
          )

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write file
        writeLines("foo", wide_excel)

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs
        expect_error(
          object = read_npx_wide_check_top(df = df,
                                           file = wide_excel,
                                           data_type = data_t,
                                           format_spec = format_s),
          regexp = "Columns 2 and 3 of of the top matrix with assay metadata in"
        )

      }
    )

    ## Quantified ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df ----

        o_platform <- "Target 48"
        n_panel <- 3L
        n_assay <- 45L
        data_t <- "Quantified"

        keep_cols <- paste0("V", seq_len((n_panel * n_assay) + 1L + n_panel))

        df <- npx_wide_top(olink_platform = o_platform,
                           n_panels = n_panel,
                           n_assays = n_assay,
                           data_type = data_t,
                           show_int_ctrl = TRUE,
                           loc_int_ctrl = "V3",
                           shuffle_assays = FALSE) |>
          dplyr::select(
            dplyr::all_of(keep_cols)
          )

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write file
        writeLines("foo", wide_excel)

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs
        expect_error(
          object = read_npx_wide_check_top(df = df,
                                           file = wide_excel,
                                           data_type = data_t,
                                           format_spec = format_s),
          regexp = "Columns 2 and 3 of of the top matrix with assay metadata in"
        )

      }
    )

  }
)

# Test read_npx_wide_top_split ----

test_that(
  "read_npx_wide_top_split - T48 - works single panel file",
  {
    ## NPX ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df_top ----

        o_platform <- "Target 48"
        n_panel <- 1L
        n_assay <- 45L
        data_t <- "NPX"

        df <- npx_wide_top(olink_platform = o_platform,
                           n_panels = n_panel,
                           n_assays = n_assay,
                           data_type = data_t,
                           show_int_ctrl = FALSE,
                           loc_int_ctrl = "V3",
                           shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_no_condition(
          object = l_obj <- read_npx_wide_top_split(df = df,
                                                    file = wide_excel,
                                                    data_type = data_t,
                                                    olink_platform = o_platform,
                                                    format_spec = format_s)
        )

        # modify df so that we can test output ----

        l_exp <- npx_wide_top_test(df = df)

        # check that tmp df are identical to function output ----

        expect_identical(
          object = l_obj$df_oid,
          expected = l_exp$df_oid
        )

        expect_identical(
          object = l_obj$df_plate,
          expected = l_exp$df_plate
        )

        expect_identical(
          object = l_obj$df_qc_warn,
          expected = l_exp$df_qc_warn
        )

      }
    )

    ## Ct ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df_top ----

        o_platform <- "Target 48"
        n_panel <- 1L
        n_assay <- 45L
        data_t <- "Ct"

        df <- npx_wide_top(olink_platform = o_platform,
                           n_panels = n_panel,
                           n_assays = n_assay,
                           data_type = data_t,
                           show_int_ctrl = FALSE,
                           loc_int_ctrl = "V3",
                           shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_no_condition(
          object = l_obj <- read_npx_wide_top_split(df = df,
                                                    file = wide_excel,
                                                    data_type = data_t,
                                                    olink_platform = o_platform,
                                                    format_spec = format_s)
        )

        # modify df so that we can test output ----

        l_exp <- npx_wide_top_test(df = df)

        # check that tmp df are identical to function output ----

        expect_identical(
          object = l_obj$df_oid,
          expected = l_exp$df_oid
        )

        expect_identical(
          object = l_obj$df_plate,
          expected = l_exp$df_plate
        )

      }
    )

    ## Quantified w int ctrl ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df_top ----

        o_platform <- "Target 48"
        n_panel <- 1L
        n_assay <- 45L
        data_t <- "Quantified"

        df <- npx_wide_top(olink_platform = o_platform,
                           n_panels = n_panel,
                           n_assays = n_assay,
                           data_type = data_t,
                           show_int_ctrl = TRUE,
                           loc_int_ctrl = "V3",
                           shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_no_condition(
          object = l_obj <- read_npx_wide_top_split(df = df,
                                                    file = wide_excel,
                                                    data_type = data_t,
                                                    olink_platform = o_platform,
                                                    format_spec = format_s)
        )

        # modify df so that we can test output ----

        l_exp <- npx_wide_top_test(df = df)

        # check that tmp df are identical to function output ----

        expect_identical(
          object = l_obj$df_oid,
          expected = l_exp$df_oid
        )

        expect_identical(
          object = l_obj$df_plate,
          expected = l_exp$df_plate
        )

        expect_identical(
          object = l_obj$df_qc_warn,
          expected = l_exp$df_qc_warn
        )

        expect_identical(
          object = l_obj$df_qc_dev,
          expected = l_exp$df_qc_dev
        )

      }
    )

    ## Quantified wo int ctrl ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df_top ----

        o_platform <- "Target 48"
        n_panel <- 1L
        n_assay <- 45L
        data_t <- "Quantified"

        df <- npx_wide_top(olink_platform = o_platform,
                           n_panels = n_panel,
                           n_assays = n_assay,
                           data_type = data_t,
                           show_int_ctrl = FALSE,
                           loc_int_ctrl = "V3",
                           shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_no_condition(
          object = l_obj <- read_npx_wide_top_split(df = df,
                                                    file = wide_excel,
                                                    data_type = data_t,
                                                    olink_platform = o_platform,
                                                    format_spec = format_s)
        )

        # modify df so that we can test output ----

        l_exp <- npx_wide_top_test(df = df)

        # check that tmp df are identical to function output ----

        expect_identical(
          object = l_obj$df_oid,
          expected = l_exp$df_oid
        )

        expect_identical(
          object = l_obj$df_plate,
          expected = l_exp$df_plate
        )

        expect_identical(
          object = l_obj$df_qc_warn,
          expected = l_exp$df_qc_warn
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_top_split - T48 - works multiple panels file",
  {
    ## NPX ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df_top ----

        o_platform <- "Target 48"
        n_panel <- 3L
        n_assay <- 45L
        data_t <- "NPX"

        df <- npx_wide_top(olink_platform = o_platform,
                           n_panels = n_panel,
                           n_assays = n_assay,
                           data_type = data_t,
                           show_int_ctrl = FALSE,
                           loc_int_ctrl = "V3",
                           shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_no_condition(
          object = l_obj <- read_npx_wide_top_split(df = df,
                                                    file = wide_excel,
                                                    data_type = data_t,
                                                    olink_platform = o_platform,
                                                    format_spec = format_s)
        )

        # modify df so that we can test output ----

        l_exp <- npx_wide_top_test(df = df)

        # check that tmp df are identical to function output ----

        expect_identical(
          object = l_obj$df_oid,
          expected = l_exp$df_oid
        )

        expect_identical(
          object = l_obj$df_plate,
          expected = l_exp$df_plate
        )

        expect_identical(
          object = l_obj$df_qc_warn,
          expected = l_exp$df_qc_warn
        )

      }
    )

    ## Ct ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df_top ----

        o_platform <- "Target 48"
        n_panel <- 3L
        n_assay <- 45L
        data_t <- "Ct"

        df <- npx_wide_top(olink_platform = o_platform,
                           n_panels = n_panel,
                           n_assays = n_assay,
                           data_type = data_t,
                           show_int_ctrl = FALSE,
                           loc_int_ctrl = "V3",
                           shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_no_condition(
          object = l_obj <- read_npx_wide_top_split(df = df,
                                                    file = wide_excel,
                                                    data_type = data_t,
                                                    olink_platform = o_platform,
                                                    format_spec = format_s)
        )

        # modify df so that we can test output ----

        l_exp <- npx_wide_top_test(df = df)

        # check that tmp df are identical to function output ----

        expect_identical(
          object = l_obj$df_oid,
          expected = l_exp$df_oid
        )

        expect_identical(
          object = l_obj$df_plate,
          expected = l_exp$df_plate
        )

      }
    )

    ## Quantified w int ctrl ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df_top ----

        o_platform <- "Target 48"
        n_panel <- 3L
        n_assay <- 45L
        data_t <- "Quantified"

        df <- npx_wide_top(olink_platform = o_platform,
                           n_panels = n_panel,
                           n_assays = n_assay,
                           data_type = data_t,
                           show_int_ctrl = TRUE,
                           loc_int_ctrl = "V3",
                           shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_no_condition(
          object = l_obj <- read_npx_wide_top_split(df = df,
                                                    file = wide_excel,
                                                    data_type = data_t,
                                                    olink_platform = o_platform,
                                                    format_spec = format_s)
        )

        # modify df so that we can test output ----

        l_exp <- npx_wide_top_test(df = df)

        # check that tmp df are identical to function output ----

        expect_identical(
          object = l_obj$df_oid,
          expected = l_exp$df_oid
        )

        expect_identical(
          object = l_obj$df_plate,
          expected = l_exp$df_plate
        )

        expect_identical(
          object = l_obj$df_qc_warn,
          expected = l_exp$df_qc_warn
        )

        expect_identical(
          object = l_obj$df_qc_dev,
          expected = l_exp$df_qc_dev
        )

      }
    )

    ## Quantified wo int ctrl ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df_top ----

        o_platform <- "Target 48"
        n_panel <- 3L
        n_assay <- 45L
        data_t <- "Quantified"

        df <- npx_wide_top(olink_platform = o_platform,
                           n_panels = n_panel,
                           n_assays = n_assay,
                           data_type = data_t,
                           show_int_ctrl = FALSE,
                           loc_int_ctrl = "V3",
                           shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_no_condition(
          object = l_obj <- read_npx_wide_top_split(df = df,
                                                    file = wide_excel,
                                                    data_type = data_t,
                                                    olink_platform = o_platform,
                                                    format_spec = format_s)
        )

        # modify df so that we can test output ----

        l_exp <- npx_wide_top_test(df = df)

        # check that tmp df are identical to function output ----

        expect_identical(
          object = l_obj$df_oid,
          expected = l_exp$df_oid
        )

        expect_identical(
          object = l_obj$df_plate,
          expected = l_exp$df_plate
        )

        expect_identical(
          object = l_obj$df_qc_warn,
          expected = l_exp$df_qc_warn
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_top_split - T96 - works single panel file",
  {
    ## NPX ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df_top ----

        o_platform <- "Target 96"
        n_panel <- 1L
        n_assay <- 92L
        data_t <- "NPX"

        df <- npx_wide_top(olink_platform = o_platform,
                           n_panels = n_panel,
                           n_assays = n_assay,
                           data_type = data_t,
                           show_int_ctrl = FALSE,
                           loc_int_ctrl = "V3",
                           shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_no_condition(
          object = l_obj <- read_npx_wide_top_split(df = df,
                                                    file = wide_excel,
                                                    data_type = data_t,
                                                    olink_platform = o_platform,
                                                    format_spec = format_s)
        )

        # modify df so that we can test output ----

        l_exp <- npx_wide_top_test(df = df)

        # check that tmp df are identical to function output ----

        expect_identical(
          object = l_obj$df_oid,
          expected = l_exp$df_oid
        )

        expect_identical(
          object = l_obj$df_plate,
          expected = l_exp$df_plate
        )

        expect_identical(
          object = l_obj$df_qc_warn,
          expected = l_exp$df_qc_warn
        )

      }
    )

    ## Ct ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df_top ----

        o_platform <- "Target 96"
        n_panel <- 1L
        n_assay <- 92L
        data_t <- "Ct"

        df <- npx_wide_top(olink_platform = o_platform,
                           n_panels = n_panel,
                           n_assays = n_assay,
                           data_type = data_t,
                           show_int_ctrl = FALSE,
                           loc_int_ctrl = "V3",
                           shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_no_condition(
          object = l_obj <- read_npx_wide_top_split(df = df,
                                                    file = wide_excel,
                                                    data_type = data_t,
                                                    olink_platform = o_platform,
                                                    format_spec = format_s)
        )

        # modify df so that we can test output ----

        l_exp <- npx_wide_top_test(df = df)

        # check that tmp df are identical to function output ----

        expect_identical(
          object = l_obj$df_oid,
          expected = l_exp$df_oid
        )

        expect_identical(
          object = l_obj$df_plate,
          expected = l_exp$df_plate
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_top_split - T96 - works multiple panels file",
  {
    ## NPX ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df_top ----

        o_platform <- "Target 96"
        n_panel <- 3L
        n_assay <- 92L
        data_t <- "NPX"

        df <- npx_wide_top(olink_platform = o_platform,
                           n_panels = n_panel,
                           n_assays = n_assay,
                           data_type = data_t,
                           show_int_ctrl = FALSE,
                           loc_int_ctrl = "V3",
                           shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_no_condition(
          object = l_obj <- read_npx_wide_top_split(df = df,
                                                    file = wide_excel,
                                                    data_type = data_t,
                                                    olink_platform = o_platform,
                                                    format_spec = format_s)
        )

        # modify df so that we can test output ----

        l_exp <- npx_wide_top_test(df = df)

        # check that tmp df are identical to function output ----

        expect_identical(
          object = l_obj$df_oid,
          expected = l_exp$df_oid
        )

        expect_identical(
          object = l_obj$df_plate,
          expected = l_exp$df_plate
        )

        expect_identical(
          object = l_obj$df_qc_warn,
          expected = l_exp$df_qc_warn
        )

      }
    )

    ## Ct ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df_top ----

        o_platform <- "Target 96"
        n_panel <- 3L
        n_assay <- 92L
        data_t <- "Ct"

        df <- npx_wide_top(olink_platform = o_platform,
                           n_panels = n_panel,
                           n_assays = n_assay,
                           data_type = data_t,
                           show_int_ctrl = FALSE,
                           loc_int_ctrl = "V3",
                           shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_no_condition(
          object = l_obj <- read_npx_wide_top_split(df = df,
                                                    file = wide_excel,
                                                    data_type = data_t,
                                                    olink_platform = o_platform,
                                                    format_spec = format_s)
        )

        # modify df so that we can test output ----

        l_exp <- npx_wide_top_test(df = df)

        # check that tmp df are identical to function output ----

        expect_identical(
          object = l_obj$df_oid,
          expected = l_exp$df_oid
        )

        expect_identical(
          object = l_obj$df_plate,
          expected = l_exp$df_plate
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_top_split - Flex/Focus - works single panel file",
  {
    ## NPX ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df_top ----

        o_platform <- "Flex"
        n_panel <- 1L
        n_assay <- 33L
        data_t <- "NPX"

        df <- npx_wide_top(olink_platform = o_platform,
                           n_panels = n_panel,
                           n_assays = n_assay,
                           data_type = data_t,
                           show_int_ctrl = FALSE,
                           loc_int_ctrl = "V3",
                           shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_no_condition(
          object = l_obj <- read_npx_wide_top_split(df = df,
                                                    file = wide_excel,
                                                    data_type = data_t,
                                                    olink_platform = o_platform,
                                                    format_spec = format_s)
        )

        # modify df so that we can test output ----

        l_exp <- npx_wide_top_test(df = df)

        # check that tmp df are identical to function output ----

        expect_identical(
          object = l_obj$df_oid,
          expected = l_exp$df_oid
        )

        expect_identical(
          object = l_obj$df_plate,
          expected = l_exp$df_plate
        )

        expect_identical(
          object = l_obj$df_qc_warn,
          expected = l_exp$df_qc_warn
        )

      }
    )

    ## Ct ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df_top ----

        o_platform <- "Flex"
        n_panel <- 1L
        n_assay <- 33L
        data_t <- "Ct"

        df <- npx_wide_top(olink_platform = o_platform,
                           n_panels = n_panel,
                           n_assays = n_assay,
                           data_type = data_t,
                           show_int_ctrl = FALSE,
                           loc_int_ctrl = "V3",
                           shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_no_condition(
          object = l_obj <- read_npx_wide_top_split(df = df,
                                                    file = wide_excel,
                                                    data_type = data_t,
                                                    olink_platform = o_platform,
                                                    format_spec = format_s)
        )

        # modify df so that we can test output ----

        l_exp <- npx_wide_top_test(df = df)

        # check that tmp df are identical to function output ----

        expect_identical(
          object = l_obj$df_oid,
          expected = l_exp$df_oid
        )

        expect_identical(
          object = l_obj$df_plate,
          expected = l_exp$df_plate
        )

      }
    )

    ## Quantified w int ctrl ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df_top ----

        o_platform <- "Flex"
        n_panel <- 1L
        n_assay <- 33L
        data_t <- "Quantified"

        df <- npx_wide_top(olink_platform = o_platform,
                           n_panels = n_panel,
                           n_assays = n_assay,
                           data_type = data_t,
                           show_int_ctrl = TRUE,
                           loc_int_ctrl = "V3",
                           shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_no_condition(
          object = l_obj <- read_npx_wide_top_split(df = df,
                                                    file = wide_excel,
                                                    data_type = data_t,
                                                    olink_platform = o_platform,
                                                    format_spec = format_s)
        )

        # modify df so that we can test output ----

        l_exp <- npx_wide_top_test(df = df)

        # check that tmp df are identical to function output ----

        expect_identical(
          object = l_obj$df_oid,
          expected = l_exp$df_oid
        )

        expect_identical(
          object = l_obj$df_plate,
          expected = l_exp$df_plate
        )

        expect_identical(
          object = l_obj$df_qc_warn,
          expected = l_exp$df_qc_warn
        )

        expect_identical(
          object = l_obj$df_qc_dev,
          expected = l_exp$df_qc_dev
        )

      }
    )

    ## Quantified wo int ctrl ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df_top ----

        o_platform <- "Flex"
        n_panel <- 1L
        n_assay <- 33L
        data_t <- "Quantified"

        df <- npx_wide_top(olink_platform = o_platform,
                           n_panels = n_panel,
                           n_assays = n_assay,
                           data_type = data_t,
                           show_int_ctrl = FALSE,
                           loc_int_ctrl = "V3",
                           shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_no_condition(
          object = l_obj <- read_npx_wide_top_split(df = df,
                                                    file = wide_excel,
                                                    data_type = data_t,
                                                    olink_platform = o_platform,
                                                    format_spec = format_s)
        )

        # modify df so that we can test output ----

        l_exp <- npx_wide_top_test(df = df)

        # check that tmp df are identical to function output ----

        expect_identical(
          object = l_obj$df_oid,
          expected = l_exp$df_oid
        )

        expect_identical(
          object = l_obj$df_plate,
          expected = l_exp$df_plate
        )

        expect_identical(
          object = l_obj$df_qc_warn,
          expected = l_exp$df_qc_warn
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_top_split - Flex/Focus - works multiple panels file",
  {
    ## NPX ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df_top ----

        o_platform <- "Focus"
        n_panel <- 3L
        n_assay <- 21L
        data_t <- "NPX"

        df <- npx_wide_top(olink_platform = o_platform,
                           n_panels = n_panel,
                           n_assays = n_assay,
                           data_type = data_t,
                           show_int_ctrl = FALSE,
                           loc_int_ctrl = "V3",
                           shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_no_condition(
          object = l_obj <- read_npx_wide_top_split(df = df,
                                                    file = wide_excel,
                                                    data_type = data_t,
                                                    olink_platform = o_platform,
                                                    format_spec = format_s)
        )

        # modify df so that we can test output ----

        l_exp <- npx_wide_top_test(df = df)

        # check that tmp df are identical to function output ----

        expect_identical(
          object = l_obj$df_oid,
          expected = l_exp$df_oid
        )

        expect_identical(
          object = l_obj$df_plate,
          expected = l_exp$df_plate
        )

        expect_identical(
          object = l_obj$df_qc_warn,
          expected = l_exp$df_qc_warn
        )

      }
    )

    ## Ct ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df_top ----

        o_platform <- "Focus"
        n_panel <- 3L
        n_assay <- 21L
        data_t <- "Ct"

        df <- npx_wide_top(olink_platform = o_platform,
                           n_panels = n_panel,
                           n_assays = n_assay,
                           data_type = data_t,
                           show_int_ctrl = FALSE,
                           loc_int_ctrl = "V3",
                           shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_no_condition(
          object = l_obj <- read_npx_wide_top_split(df = df,
                                                    file = wide_excel,
                                                    data_type = data_t,
                                                    olink_platform = o_platform,
                                                    format_spec = format_s)
        )

        # modify df so that we can test output ----

        l_exp <- npx_wide_top_test(df = df)

        # check that tmp df are identical to function output ----

        expect_identical(
          object = l_obj$df_oid,
          expected = l_exp$df_oid
        )

        expect_identical(
          object = l_obj$df_plate,
          expected = l_exp$df_plate
        )

      }
    )

    ## Quantified w int ctrl ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df_top ----

        o_platform <- "Focus"
        n_panel <- 3L
        n_assay <- 21L
        data_t <- "Quantified"

        df <- npx_wide_top(olink_platform = o_platform,
                           n_panels = n_panel,
                           n_assays = n_assay,
                           data_type = data_t,
                           show_int_ctrl = TRUE,
                           loc_int_ctrl = "V3",
                           shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_no_condition(
          object = l_obj <- read_npx_wide_top_split(df = df,
                                                    file = wide_excel,
                                                    data_type = data_t,
                                                    olink_platform = o_platform,
                                                    format_spec = format_s)
        )

        # modify df so that we can test output ----

        l_exp <- npx_wide_top_test(df = df)

        # check that tmp df are identical to function output ----

        expect_identical(
          object = l_obj$df_oid,
          expected = l_exp$df_oid
        )

        expect_identical(
          object = l_obj$df_plate,
          expected = l_exp$df_plate
        )

        expect_identical(
          object = l_obj$df_qc_warn,
          expected = l_exp$df_qc_warn
        )

        expect_identical(
          object = l_obj$df_qc_dev,
          expected = l_exp$df_qc_dev
        )

      }
    )

    ## Quantified wo int ctrl ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df_top ----

        o_platform <- "Focus"
        n_panel <- 3L
        n_assay <- 21L
        data_t <- "Quantified"

        df <- npx_wide_top(olink_platform = o_platform,
                           n_panels = n_panel,
                           n_assays = n_assay,
                           data_type = data_t,
                           show_int_ctrl = FALSE,
                           loc_int_ctrl = "V3",
                           shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_no_condition(
          object = l_obj <- read_npx_wide_top_split(df = df,
                                                    file = wide_excel,
                                                    data_type = data_t,
                                                    olink_platform = o_platform,
                                                    format_spec = format_s)
        )

        # modify df so that we can test output ----

        l_exp <- npx_wide_top_test(df = df)

        # check that tmp df are identical to function output ----

        expect_identical(
          object = l_obj$df_oid,
          expected = l_exp$df_oid
        )

        expect_identical(
          object = l_obj$df_plate,
          expected = l_exp$df_plate
        )

        expect_identical(
          object = l_obj$df_qc_warn,
          expected = l_exp$df_qc_warn
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_top_split - unrecognizable tags",
  {
    # df containsan unrecognizeable tag ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df_top ----

        o_platform <- "Target 48"
        n_panel <- 1L
        n_assay <- 45L
        data_t <- "NPX"

        df <- npx_wide_top(olink_platform = o_platform,
                           n_panels = n_panel,
                           n_assays = n_assay,
                           data_type = data_t,
                           show_int_ctrl = FALSE,
                           loc_int_ctrl = "V3",
                           shuffle_assays = FALSE)

        # modify df and df_t to add new row/column with unknown label
        df_add_cname <- paste0("V", (ncol(df) + 1))
        df_add <- df |>
          dplyr::select(
            dplyr::all_of(paste0("V", ncol(df)))
          ) |>
          dplyr::rename(
            {{df_add_cname}} := dplyr::all_of(paste0("V", ncol(df)))
          ) |>
          dplyr::mutate(
            dplyr::across(
              dplyr::everything(),
              ~ dplyr::if_else(dplyr::row_number() == 2L, "Unknown", .x)
            )
          )

        df <- df |>
          dplyr::bind_cols(
            df_add
          )

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_error(
          object = read_npx_wide_top_split(df = df,
                                           file = wide_excel,
                                           data_type = data_t,
                                           olink_platform = o_platform,
                                           format_spec = format_s),
          regexp = "The top matrix with the assays metadata in file"
        )

      }
    )

    # df contains QC Warning with data type is Ct ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df_top ----

        o_platform <- "Target 48"
        n_panel <- 1L
        n_assay <- 45L
        data_t <- "NPX"

        df <- npx_wide_top(olink_platform = o_platform,
                           n_panels = n_panel,
                           n_assays = n_assay,
                           data_type = data_t,
                           show_int_ctrl = FALSE,
                           loc_int_ctrl = "V3",
                           shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == "Ct")

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_error(
          object = read_npx_wide_top_split(df = df,
                                           file = wide_excel,
                                           data_type = "Ct",
                                           olink_platform = o_platform,
                                           format_spec = format_s),
          regexp = "The top matrix with the assays metadata in file"
        )

      }
    )

    # df contains contains internal controls with data type is NPX ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df_top ----

        o_platform <- "Target 48"
        n_panel <- 1L
        n_assay <- 45L
        data_t <- "Quantified"

        df <- npx_wide_top(olink_platform = o_platform,
                           n_panels = n_panel,
                           n_assays = n_assay,
                           data_type = data_t,
                           show_int_ctrl = TRUE,
                           loc_int_ctrl = "V3",
                           shuffle_assays = FALSE) |>
          dplyr::slice(
            1L:4L
          )

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == "NPX")

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_error(
          object = read_npx_wide_top_split(df = df,
                                           file = wide_excel,
                                           data_type = "NPX",
                                           olink_platform = o_platform,
                                           format_spec = format_s),
          regexp = "The top matrix with the assays metadata in file"
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_top_split - NAs in OlinkID/Uniprot/Assay",
  {
    ## OlinkID = NA 1 instance ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df_top ----

        o_platform <- "Target 48"
        n_panel <- 1L
        n_assay <- 45L
        data_t <- "NPX"

        df <- npx_wide_top(olink_platform = o_platform,
                           n_panels = n_panel,
                           n_assays = n_assay,
                           data_type = data_t,
                           show_int_ctrl = FALSE,
                           loc_int_ctrl = "V3",
                           shuffle_assays = FALSE) |>
          # modify df and df_t to intrduce NA cells
          dplyr::mutate(
            V2 = dplyr::if_else(.data[["V2"]] %in% "Uniprot1",
                                NA_character_,
                                .data[["V2"]])
          )

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_error(
          object = read_npx_wide_top_split(df = df,
                                           file = wide_excel,
                                           data_type = data_t,
                                           olink_platform = o_platform,
                                           format_spec = format_s),
          regexp = "Detected 1 empty cells in columns"
        )

      }
    )

    ## OlinkID = NA 4 instances ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df_top ----

        o_platform <- "Target 48"
        n_panel <- 1L
        n_assay <- 45L
        data_t <- "NPX"

        df <- npx_wide_top(olink_platform = o_platform,
                           n_panels = n_panel,
                           n_assays = n_assay,
                           data_type = data_t,
                           show_int_ctrl = FALSE,
                           loc_int_ctrl = "V3",
                           shuffle_assays = FALSE) |>
          # modify df and df_t to intrduce NA cells
          dplyr::mutate(
            V2 = dplyr::if_else(
              .data[["V2"]] %in% paste0(c("Assay", "Uniprot"), 1L),
              NA_character_,
              .data[["V2"]]
            ),
            V3 = dplyr::if_else(
              .data[["V3"]] %in% paste0(c("Assay", "Uniprot"), 2L),
              NA_character_,
              .data[["V3"]]
            )
          )

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_error(
          object = read_npx_wide_top_split(df = df,
                                           file = wide_excel,
                                           data_type = data_t,
                                           olink_platform = o_platform,
                                           format_spec = format_s),
          regexp = "Detected 4 empty cells in columns"
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_top_split - wrong number of assays",
  {
    ## T48 1 panel ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df_top ----

        o_platform <- "Target 48"
        n_panel <- 1L
        n_assay <- 40L
        data_t <- "NPX"

        df <- npx_wide_top(olink_platform = o_platform,
                           n_panels = n_panel,
                           n_assays = n_assay,
                           data_type = data_t,
                           show_int_ctrl = FALSE,
                           loc_int_ctrl = "V3",
                           shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_error(
          object = read_npx_wide_top_split(df = df,
                                           file = wide_excel,
                                           data_type = data_t,
                                           olink_platform = o_platform,
                                           format_spec = format_s),
          regexp = "Detected 40 assays in 1 panels in file"
        )

      }
    )

    ## T48 2 panels ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df_top ----

        o_platform <- "Target 48"
        n_panel <- 2L
        n_assay <- 32L
        data_t <- "NPX"

        df <- npx_wide_top(olink_platform = o_platform,
                           n_panels = n_panel,
                           n_assays = n_assay,
                           data_type = data_t,
                           show_int_ctrl = FALSE,
                           loc_int_ctrl = "V3",
                           shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_error(
          object = read_npx_wide_top_split(df = df,
                                           file = wide_excel,
                                           data_type = data_t,
                                           olink_platform = o_platform,
                                           format_spec = format_s),
          regexp = "Detected 64 assays in 2 panels in file"
        )
      }
    )

    ## T96 1 panel ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df_top ----

        o_platform <- "Target 96"
        n_panel <- 1L
        n_assay <- 67L
        data_t <- "NPX"

        df <- npx_wide_top(olink_platform = o_platform,
                           n_panels = n_panel,
                           n_assays = n_assay,
                           data_type = data_t,
                           show_int_ctrl = FALSE,
                           loc_int_ctrl = "V3",
                           shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_error(
          object = read_npx_wide_top_split(df = df,
                                           file = wide_excel,
                                           data_type = data_t,
                                           olink_platform = o_platform,
                                           format_spec = format_s),
          regexp = "Detected 67 assays in 1 panels in file"
        )

      }
    )

    ## T96 2 panels ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df_top ----

        o_platform <- "Target 96"
        n_panel <- 2L
        n_assay <- 78L
        data_t <- "NPX"

        df <- npx_wide_top(olink_platform = o_platform,
                           n_panels = n_panel,
                           n_assays = n_assay,
                           data_type = data_t,
                           show_int_ctrl = FALSE,
                           loc_int_ctrl = "V3",
                           shuffle_assays = FALSE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_error(
          object = read_npx_wide_top_split(df = df,
                                           file = wide_excel,
                                           data_type = data_t,
                                           olink_platform = o_platform,
                                           format_spec = format_s),
          regexp = "Detected 156 assays in 2 panels in file"
        )
      }
    )


  }
)

test_that(
  "read_npx_wide_top_split - uneven number of Plate ID and QC_Warning cols",
  {
    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df_top ----

        o_platform <- "Target 48"
        n_panel <- 2L
        n_assay <- 45L
        data_t <- "NPX"

        remove_col <- paste0("V", (1L + (n_panel * n_assay) + (2L * n_panel)))

        df <- npx_wide_top(olink_platform = o_platform,
                           n_panels = n_panel,
                           n_assays = n_assay,
                           data_type = data_t,
                           show_int_ctrl = FALSE,
                           loc_int_ctrl = "V3",
                           shuffle_assays = FALSE) |>
          dplyr::select(
            -dplyr::all_of(remove_col)
          )

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_error(
          object = read_npx_wide_top_split(df = df,
                                           file = wide_excel,
                                           data_type = data_t,
                                           olink_platform = o_platform,
                                           format_spec = format_s),
          regexp = "Expected equal number of \"Plate ID\" and \"QC\ Warning\""
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_top_split - internal controls within customer assays",
  {
    ## Int Ctrl shuffled with customer assays ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df_top ----

        o_platform <- "Target 48"
        n_panel <- 1L
        n_assay <- 45L
        data_t <- "Quantified"

        df <- npx_wide_top(olink_platform = o_platform,
                           n_panels = n_panel,
                           n_assays = n_assay,
                           data_type = data_t,
                           show_int_ctrl = TRUE,
                           loc_int_ctrl = "V3",
                           shuffle_assays = TRUE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_no_condition(
          object = l_obj <- read_npx_wide_top_split(df = df,
                                                    file = wide_excel,
                                                    data_type = data_t,
                                                    olink_platform = o_platform,
                                                    format_spec = format_s)
        )

        # modify df so that we can test output ----

        l_exp <- npx_wide_top_test(df = df)

        # check that tmp df are identical to function output ----

        expect_identical(
          object = l_obj$df_oid,
          expected = l_exp$df_oid
        )

        expect_identical(
          object = l_obj$df_meta,
          expected = l_exp$df_meta
        )

        expect_identical(
          object = l_obj$df_qc_dev,
          expected = l_exp$df_qc_dev
        )

      }
    )

    ## Int Ctrl shuffled with customer assays  and in different row ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df_top ----

        o_platform <- "Target 48"
        n_panel <- 1L
        n_assay <- 45L
        data_t <- "Quantified"

        df <- npx_wide_top(olink_platform = o_platform,
                           n_panels = n_panel,
                           n_assays = n_assay,
                           data_type = data_t,
                           show_int_ctrl = TRUE,
                           loc_int_ctrl = "V2",
                           shuffle_assays = TRUE)

        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_no_condition(
          object = l_obj <- read_npx_wide_top_split(df = df,
                                                    file = wide_excel,
                                                    data_type = data_t,
                                                    olink_platform = o_platform,
                                                    format_spec = format_s)
        )

        # modify df so that we can test output ----

        l_exp <- npx_wide_top_test(df = df)

        # check that tmp df are identical to function output ----

        expect_identical(
          object = l_obj$df_oid,
          expected = l_exp$df_oid
        )

        expect_identical(
          object = l_obj$df_meta,
          expected = l_exp$df_meta
        )

        expect_identical(
          object = l_obj$df_qc_dev,
          expected = l_exp$df_qc_dev
        )

      }
    )

  }
)

# Test read_npx_wide_bottom ----

test_that(
  "read_npx_wide_bottom - T48 - works single panel file",
  {
    ## NPX ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df_bottom ----

        n_plate <- 1L
        n_panel <- 1L
        n_assay <- 45L
        data_t <- "NPX"

        df <- npx_wide_bottom(n_plates = n_plate,
                              n_panels = n_panel,
                              n_assays = n_assay,
                              data_type = data_t)

        col_s <- paste0("V", n_assay * n_panel + 2)
        a_cols <- paste0("V", 2L:((n_assay * n_panel) + 1L))
        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_no_condition(
          object = df_b <- read_npx_wide_bottom(df = df,
                                                file = wide_excel,
                                                data_type = data_t,
                                                col_split = col_s,
                                                assay_cols = a_cols,
                                                format_spec = format_s)
        )

        # modify df so that we can test output ----

        df_t <- npx_wide_bottom_test(df = df,
                                     data_type = data_t,
                                     col_split = col_s)

        # order columns on df_b
        df_b <- df_b |>
          dplyr::select(
            order(colnames(df_b))
          )

        # check that dfs are identical to function output ----

        expect_identical(
          object = df_b,
          expected = df_t
        )

      }
    )

    ## Quantified ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df_bottom ----

        n_plate <- 1L
        n_panel <- 1L
        n_assay <- 45L
        data_t <- "Quantified"

        df <- npx_wide_bottom(n_plates = n_plate,
                              n_panels = n_panel,
                              n_assays = n_assay,
                              data_type = data_t)

        col_s <- paste0("V", n_assay * n_panel + 2)
        a_cols <- paste0("V", 2L:((n_assay * n_panel) + 1L))
        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        # switched from expect_no_condition to the one below because of a very
        # weird silent message thrown by rlang when using group_by and arrange
        # in dplyr. This is documented here
        # https://github.com/aryoda/tryCatchLog/issues/62
        # Looks like it has been fixed, but we still get the error.
        expect_no_error(
          expect_no_warning(
            object = df_b <- read_npx_wide_bottom(df = df,
                                                  file = wide_excel,
                                                  data_type = data_t,
                                                  col_split = col_s,
                                                  assay_cols = a_cols,
                                                  format_spec = format_s)
          )
        )

        # modify df so that we can test output ----

        df_t <- npx_wide_bottom_test(df = df,
                                     data_type = data_t,
                                     col_split = col_s)

        # order columns on df_b
        df_b <- df_b |>
          dplyr::select(
            order(colnames(df_b))
          )

        # check that dfs are identical to function output ----

        expect_identical(
          object = df_b,
          expected = df_t
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_bottom - T48 - works multi panel and multi plate file",
  {
    ## NPX ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df_bottom ----

        n_plate <- 2L
        n_panel <- 4L
        n_assay <- 45L
        data_t <- "NPX"

        df <- npx_wide_bottom(n_plates = n_plate,
                              n_panels = n_panel,
                              n_assays = n_assay,
                              data_type = data_t)

        col_s <- paste0("V", n_assay * n_panel + 2)
        a_cols <- paste0("V", 2L:((n_assay * n_panel) + 1L))
        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_no_condition(
          object = df_b <- read_npx_wide_bottom(df = df,
                                                file = wide_excel,
                                                data_type = data_t,
                                                col_split = col_s,
                                                assay_cols = a_cols,
                                                format_spec = format_s)
        )

        # modify df so that we can test output ----

        df_t <- npx_wide_bottom_test(df = df,
                                     data_type = data_t,
                                     col_split = col_s)

        # order columns on df_b
        df_b <- df_b |>
          dplyr::select(
            order(colnames(df_b))
          )

        # check that dfs are identical to function output ----

        expect_identical(
          object = df_b,
          expected = df_t
        )

      }
    )

    ## Quantified ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df_bottom ----

        n_plate <- 2L
        n_panel <- 4L
        n_assay <- 45L
        data_t <- "Quantified"

        df <- npx_wide_bottom(n_plates = n_plate,
                              n_panels = n_panel,
                              n_assays = n_assay,
                              data_type = data_t)

        col_s <- paste0("V", n_assay * n_panel + 2)
        a_cols <- paste0("V", 2L:((n_assay * n_panel) + 1L))
        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        # switched from expect_no_condition to the one below because of a very
        # weird silent message thrown by rlang when using group_by and arrange
        # in dplyr. This is documented here
        # https://github.com/aryoda/tryCatchLog/issues/62
        # Looks like it has been fixed, but we still get the error.
        expect_no_error(
          expect_no_warning(
            object = df_b <- read_npx_wide_bottom(df = df,
                                                  file = wide_excel,
                                                  data_type = data_t,
                                                  col_split = col_s,
                                                  assay_cols = a_cols,
                                                  format_spec = format_s)
          )
        )

        # modify df so that we can test output ----

        df_t <- npx_wide_bottom_test(df = df,
                                     data_type = data_t,
                                     col_split = col_s)

        # order columns on df_b
        df_b <- df_b |>
          dplyr::select(
            order(colnames(df_b))
          )

        # check that dfs are identical to function output ----

        expect_identical(
          object = df_b,
          expected = df_t
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_bottom - T96 - works single panel file",
  {
    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df_bottom ----

        n_plate <- 1L
        n_panel <- 1L
        n_assay <- 92L
        data_t <- "NPX"

        df <- npx_wide_bottom(n_plates = n_plate,
                              n_panels = n_panel,
                              n_assays = n_assay,
                              data_type = data_t)

        col_s <- paste0("V", n_assay * n_panel + 2)
        a_cols <- paste0("V", 2L:((n_assay * n_panel) + 1L))
        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_no_condition(
          object = df_b <- read_npx_wide_bottom(df = df,
                                                file = wide_excel,
                                                data_type = data_t,
                                                col_split = col_s,
                                                assay_cols = a_cols,
                                                format_spec = format_s)
        )

        # modify df so that we can test output ----

        df_t <- npx_wide_bottom_test(df = df,
                                     data_type = data_t,
                                     col_split = col_s)

        # order columns on df_b
        df_b <- df_b |>
          dplyr::select(
            order(colnames(df_b))
          )

        # check that dfs are identical to function output ----

        expect_identical(
          object = df_b,
          expected = df_t
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_bottom - T96 - works multi panel and multi plate file",
  {
    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df_bottom ----

        n_plate <- 2L
        n_panel <- 4L
        n_assay <- 92L
        data_t <- "NPX"

        df <- npx_wide_bottom(n_plates = n_plate,
                              n_panels = n_panel,
                              n_assays = n_assay,
                              data_type = data_t)

        col_s <- paste0("V", n_assay * n_panel + 2)
        a_cols <- paste0("V", 2L:((n_assay * n_panel) + 1L))
        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_no_condition(
          object = df_b <- read_npx_wide_bottom(df = df,
                                                file = wide_excel,
                                                data_type = data_t,
                                                col_split = col_s,
                                                assay_cols = a_cols,
                                                format_spec = format_s)
        )

        # modify df so that we can test output ----

        df_t <- npx_wide_bottom_test(df = df,
                                     data_type = data_t,
                                     col_split = col_s)

        # order columns on df_b
        df_b <- df_b |>
          dplyr::select(
            order(colnames(df_b))
          )

        # check that dfs are identical to function output ----

        expect_identical(
          object = df_b,
          expected = df_t
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_bottom - Flex/Focus - works single panel file",
  {
    ## NPX ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df_bottom ----

        n_plate <- 1L
        n_panel <- 1L
        n_assay <- 33L
        data_t <- "NPX"

        df <- npx_wide_bottom(n_plates = n_plate,
                              n_panels = n_panel,
                              n_assays = n_assay,
                              data_type = data_t)

        col_s <- paste0("V", n_assay * n_panel + 2)
        a_cols <- paste0("V", 2L:((n_assay * n_panel) + 1L))
        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_no_condition(
          object = df_b <- read_npx_wide_bottom(df = df,
                                                file = wide_excel,
                                                data_type = data_t,
                                                col_split = col_s,
                                                assay_cols = a_cols,
                                                format_spec = format_s)
        )

        # modify df so that we can test output ----

        df_t <- npx_wide_bottom_test(df = df,
                                     data_type = data_t,
                                     col_split = col_s)

        # order columns on df_b
        df_b <- df_b |>
          dplyr::select(
            order(colnames(df_b))
          )

        # check that dfs are identical to function output ----

        expect_identical(
          object = df_b,
          expected = df_t
        )

      }
    )

    ## Quantified ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df_bottom ----

        n_plate <- 1L
        n_panel <- 1L
        n_assay <- 33L
        data_t <- "Quantified"

        df <- npx_wide_bottom(n_plates = n_plate,
                              n_panels = n_panel,
                              n_assays = n_assay,
                              data_type = data_t)

        col_s <- paste0("V", n_assay * n_panel + 2)
        a_cols <- paste0("V", 2L:((n_assay * n_panel) + 1L))
        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        # switched from expect_no_condition to the one below because of a very
        # weird silent message thrown by rlang when using group_by and arrange
        # in dplyr. This is documented here
        # https://github.com/aryoda/tryCatchLog/issues/62
        # Looks like it has been fixed, but we still get the error.
        expect_no_error(
          expect_no_warning(
            object = df_b <- read_npx_wide_bottom(df = df,
                                                  file = wide_excel,
                                                  data_type = data_t,
                                                  col_split = col_s,
                                                  assay_cols = a_cols,
                                                  format_spec = format_s)
          )
        )

        # modify df so that we can test output ----

        df_t <- npx_wide_bottom_test(df = df,
                                     data_type = data_t,
                                     col_split = col_s)

        # order columns on df_b
        df_b <- df_b |>
          dplyr::select(
            order(colnames(df_b))
          )

        # check that dfs are identical to function output ----

        expect_identical(
          object = df_b,
          expected = df_t
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_bottom - Flex/Focus - works multi panel & multi plate file",
  {
    ## NPX ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df_bottom ----

        n_plate <- 2L
        n_panel <- 4L
        n_assay <- 21L
        data_t <- "NPX"

        df <- npx_wide_bottom(n_plates = n_plate,
                              n_panels = n_panel,
                              n_assays = n_assay,
                              data_type = data_t)

        col_s <- paste0("V", n_assay * n_panel + 2)
        a_cols <- paste0("V", 2L:((n_assay * n_panel) + 1L))
        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_no_condition(
          object = df_b <- read_npx_wide_bottom(df = df,
                                                file = wide_excel,
                                                data_type = data_t,
                                                col_split = col_s,
                                                assay_cols = a_cols,
                                                format_spec = format_s)
        )

        # modify df so that we can test output ----

        df_t <- npx_wide_bottom_test(df = df,
                                     data_type = data_t,
                                     col_split = col_s)

        # order columns on df_b
        df_b <- df_b |>
          dplyr::select(
            order(colnames(df_b))
          )

        # check that dfs are identical to function output ----

        expect_identical(
          object = df_b,
          expected = df_t
        )

      }
    )

    ## Quantified ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df_bottom ----

        n_plate <- 2L
        n_panel <- 4L
        n_assay <- 21L
        data_t <- "Quantified"

        df <- npx_wide_bottom(n_plates = n_plate,
                              n_panels = n_panel,
                              n_assays = n_assay,
                              data_type = data_t)

        col_s <- paste0("V", n_assay * n_panel + 2)
        a_cols <- paste0("V", 2L:((n_assay * n_panel) + 1L))
        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        # switched from expect_no_condition to the one below because of a very
        # weird silent message thrown by rlang when using group_by and arrange
        # in dplyr. This is documented here
        # https://github.com/aryoda/tryCatchLog/issues/62
        # Looks like it has been fixed, but we still get the error.
        expect_no_error(
          expect_no_warning(
            object = df_b <- read_npx_wide_bottom(df = df,
                                                  file = wide_excel,
                                                  data_type = data_t,
                                                  col_split = col_s,
                                                  assay_cols = a_cols,
                                                  format_spec = format_s)
          )
        )

        # modify df so that we can test output ----

        df_t <- npx_wide_bottom_test(df = df,
                                     data_type = data_t,
                                     col_split = col_s)

        # order columns on df_b
        df_b <- df_b |>
          dplyr::select(
            order(colnames(df_b))
          )

        # check that dfs are identical to function output ----

        expect_identical(
          object = df_b,
          expected = df_t
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_bottom - T48 - error for unexpected values in V1",
  {
    ## NPX v1 ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df_bottom ----

        n_plate <- 1L
        n_panel <- 1L
        n_assay <- 45L
        data_t <- "NPX"

        df <- npx_wide_bottom(n_plates = n_plate,
                              n_panels = n_panel,
                              n_assays = n_assay,
                              data_type = data_t) |>
          # modify df with wrong V1
          dplyr::mutate(
            V1 = dplyr::if_else(.data[["V1"]] == "LOD", "LOD2", .data[["V1"]])
          )

        col_s <- paste0("V", n_assay * n_panel + 2)
        a_cols <- paste0("V", 2L:((n_assay * n_panel) + 1L))
        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_error(
          object = read_npx_wide_bottom(df = df,
                                        file = wide_excel,
                                        data_type = data_t,
                                        col_split = col_s,
                                        assay_cols = a_cols,
                                        format_spec = format_s),
          regexp = "Column 1 of the bottom matrix with assay metadata in file"
        )

      }
    )

    ## NPX v2 ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df_bottom ----

        n_plate <- 1L
        n_panel <- 1L
        n_assay <- 45L
        data_t <- "NPX"

        df <- npx_wide_bottom(n_plates = n_plate,
                              n_panels = n_panel,
                              n_assays = n_assay,
                              data_type = "Quantified")

        col_s <- paste0("V", n_assay * n_panel + 2)
        a_cols <- paste0("V", 2L:((n_assay * n_panel) + 1L))
        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_error(
          object = read_npx_wide_bottom(df = df,
                                        file = wide_excel,
                                        data_type = data_t,
                                        col_split = col_s,
                                        assay_cols = a_cols,
                                        format_spec = format_s),
          regexp = "Column 1 of the bottom matrix with assay metadata in file"
        )

      }
    )

    ## Quantified v1 ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df_bottom ----

        n_plate <- 1L
        n_panel <- 1L
        n_assay <- 45L
        data_t <- "Quantified"

        df <- npx_wide_bottom(n_plates = n_plate,
                              n_panels = n_panel,
                              n_assays = n_assay,
                              data_type = data_t) |>
          # modify df with wrong V1
          dplyr::mutate(
            V1 = dplyr::if_else(.data[["V1"]] == "Assay warning",
                                "I_am_Unexpected",
                                .data[["V1"]])
          )

        col_s <- paste0("V", n_assay * n_panel + 2)
        a_cols <- paste0("V", 2L:((n_assay * n_panel) + 1L))
        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_error(
          object = read_npx_wide_bottom(df = df,
                                        file = wide_excel,
                                        data_type = data_t,
                                        col_split = col_s,
                                        assay_cols = a_cols,
                                        format_spec = format_s),
          regexp = "Column 1 of the bottom matrix with assay metadata in file"
        )

      }
    )

    ## Quantified v2 ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df_bottom ----

        n_plate <- 1L
        n_panel <- 1L
        n_assay <- 45L
        data_t <- "Quantified"

        df <- npx_wide_bottom(n_plates = n_plate,
                              n_panels = n_panel,
                              n_assays = n_assay,
                              data_type = "NPX")

        col_s <- paste0("V", n_assay * n_panel + 2)
        a_cols <- paste0("V", 2L:((n_assay * n_panel) + 1L))
        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_error(
          object = read_npx_wide_bottom(df = df,
                                        file = wide_excel,
                                        data_type = data_t,
                                        col_split = col_s,
                                        assay_cols = a_cols,
                                        format_spec = format_s),
          regexp = "Column 1 of the bottom matrix with assay metadata in file"
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_bottom - T48 - works with additonal all NA col",
  {
    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df_bottom ----

        n_plate <- 1L
        n_panel <- 1L
        n_assay <- 45L
        data_t <- "NPX"

        df <- npx_wide_bottom(n_plates = n_plate,
                              n_panels = n_panel,
                              n_assays = n_assay,
                              data_type = data_t)

        col_s <- paste0("V", n_assay * n_panel + 2L)
        a_cols <- paste0("V", 2L:((n_assay * n_panel) + 1L))
        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # add new column with all NA
        df <- df |>
          dplyr::mutate(
            {{col_s}} := rep(x = NA_character_, times = nrow(df))
          )

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_no_condition(
          object = df_b <- read_npx_wide_bottom(df = df,
                                                file = wide_excel,
                                                data_type = data_t,
                                                col_split = col_s,
                                                assay_cols = a_cols,
                                                format_spec = format_s)
        )

        # modify df so that we can test output ----

        df_t <- npx_wide_bottom_test(df = df,
                                     data_type = data_t,
                                     col_split = col_s)

        # order columns on df_b
        df_b <- df_b |>
          dplyr::select(
            order(colnames(df_b))
          )

        # check that dfs are identical to function output ----

        expect_identical(
          object = df_b,
          expected = df_t
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_bottom - T48 - incorrect number of plates x QC data",
  {
    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df_bottom ----

        n_plate <- 2L
        n_panel <- 4L
        n_assay <- 45L
        data_t <- "Quantified"
        col_s <- paste0("V", n_assay * n_panel + 2)

        df <- npx_wide_bottom(n_plates = n_plate,
                              n_panels = n_panel,
                              n_assays = n_assay,
                              data_type = data_t) |>
          dplyr::filter(
            !(.data[["V1"]] == "Plate LOD"
              & .data[[col_s]] == "Plate 2")
          )

        a_cols <- paste0("V", 2L:((n_assay * n_panel) + 1L))
        format_s <- olink_wide_excel_spec |>
          dplyr::filter(.data[["data_type"]] == .env[["data_t"]])

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_error(
          object = read_npx_wide_bottom(df = df,
                                        file = wide_excel,
                                        data_type = data_t,
                                        col_split = col_s,
                                        assay_cols = a_cols,
                                        format_spec = format_s),
          regexp = "Column 1 of the bottom matrix contains uneven rows of plate"
        )

      }
    )

  }
)

# Test read_npx_wide_middle ----

test_that(
  "read_npx_wide_middle - works",
  {
    # NPX 1 panel ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {
        # write something in the excel file ----

        writeLines("foo", wide_excel)

        # create middle df ----

        n_panel <- 1L
        n_assay <- 45L
        data_t <- "NPX"
        n_sample <- 200L
        shuffle <- FALSE
        int_ctrl <- TRUE

        df <- npx_wide_middle(n_panels = n_panel,
                              n_assays = n_assay,
                              n_samples = n_sample,
                              data_type = data_t,
                              show_int_ctrl = int_ctrl,
                              shuffle_assays = shuffle)

        # get columns ----

        cname <- npx_wide_col_index(n_panels = n_panel,
                                    n_assays = n_assay,
                                    is_shuffled = shuffle)

        # run function ----

        expect_no_condition(
          object =
            l_obj <- read_npx_wide_middle(
              df = df,
              file = wide_excel,
              data_type = data_t,
              assay_cols = cname$assay_cols,
              plate_cols = cname$plate_cols,
              qc_warn_cols = cname$qc_warn_cols,
              int_ctrl_cols = NULL
            )
        )

        # modify df for tests ----

        l_exp <- npx_wide_middle_test(df = df,
                                      n_panels = n_panel,
                                      n_assays = n_assay,
                                      data_type = data_t,
                                      is_shuffled = shuffle,
                                      cname = cname)

        # test on output ----

        expect_identical(
          object = l_obj$df_oid,
          expected = l_exp$df_oid
        )

        expect_identical(
          object = l_obj$df_plate,
          expected = l_exp$df_plate
        )

        if (data_t != "Ct") {
          expect_identical(
            object = l_obj$df_qc_warn,
            expected = l_exp$df_qc_warn
          )
        }

        if (data_t != "Quantified"
            && int_ctrl == TRUE) {
          expect_identical(
            object = l_obj$df_int_ctrl,
            expected = l_exp$df_int_ctrl
          )
        }

      }
    )

    # NPX multi-panel ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {
        # write something in the excel file ----

        writeLines("foo", wide_excel)

        # create middle df ----

        n_panel <- 4L
        n_assay <- 45L
        data_t <- "NPX"
        n_sample <- 200L
        shuffle <- FALSE
        int_ctrl <- TRUE

        df <- npx_wide_middle(n_panels = n_panel,
                              n_assays = n_assay,
                              n_samples = n_sample,
                              data_type = data_t,
                              show_int_ctrl = int_ctrl,
                              shuffle_assays = shuffle)

        # get columns ----

        cname <- npx_wide_col_index(n_panels = n_panel,
                                    n_assays = n_assay,
                                    is_shuffled = shuffle)

        # run function ----

        expect_no_condition(
          object =
            l_obj <- read_npx_wide_middle(
              df = df,
              file = wide_excel,
              data_type = data_t,
              assay_cols = cname$assay_cols,
              plate_cols = cname$plate_cols,
              qc_warn_cols = cname$qc_warn_cols,
              int_ctrl_cols = NULL
            )
        )

        # modify df for tests ----

        l_exp <- npx_wide_middle_test(df = df,
                                      n_panels = n_panel,
                                      n_assays = n_assay,
                                      data_type = data_t,
                                      is_shuffled = shuffle,
                                      cname = cname)

        # test on output ----

        expect_identical(
          object = l_obj$df_oid,
          expected = l_exp$df_oid
        )

        expect_identical(
          object = l_obj$df_plate,
          expected = l_exp$df_plate
        )

        expect_identical(
          object = l_obj$df_qc_warn,
          expected = l_exp$df_qc_warn
        )

      }
    )

    # Ct 1 panel ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {
        # write something in the excel file ----

        writeLines("foo", wide_excel)

        # create middle df ----

        n_panel <- 1L
        n_assay <- 45L
        data_t <- "Ct"
        n_sample <- 200L
        shuffle <- FALSE
        int_ctrl <- TRUE

        df <- npx_wide_middle(n_panels = n_panel,
                              n_assays = n_assay,
                              n_samples = n_sample,
                              data_type = data_t,
                              show_int_ctrl = int_ctrl,
                              shuffle_assays = shuffle)

        # get columns ----

        cname <- npx_wide_col_index(n_panels = n_panel,
                                    n_assays = n_assay,
                                    is_shuffled = shuffle)

        # run function ----

        expect_no_condition(
          object =
            l_obj <- read_npx_wide_middle(
              df = df,
              file = wide_excel,
              data_type = data_t,
              assay_cols = cname$assay_cols,
              plate_cols = cname$plate_cols,
              qc_warn_cols = NULL,
              int_ctrl_cols = NULL
            )
        )

        # modify df for tests ----

        l_exp <- npx_wide_middle_test(df = df,
                                      n_panels = n_panel,
                                      n_assays = n_assay,
                                      data_type = data_t,
                                      is_shuffled = shuffle,
                                      cname = cname)

        # test on output ----

        expect_identical(
          object = l_obj$df_oid,
          expected = l_exp$df_oid
        )

        expect_identical(
          object = l_obj$df_plate,
          expected = l_exp$df_plate
        )

      }
    )

    # Ct multi-panel ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {
        # write something in the excel file ----

        writeLines("foo", wide_excel)

        # create middle df ----

        n_panel <- 4L
        n_assay <- 45L
        data_t <- "Ct"
        n_sample <- 200L
        shuffle <- FALSE
        int_ctrl <- TRUE

        df <- npx_wide_middle(n_panels = n_panel,
                              n_assays = n_assay,
                              n_samples = n_sample,
                              data_type = data_t,
                              show_int_ctrl = int_ctrl,
                              shuffle_assays = shuffle)

        # get columns ----

        cname <- npx_wide_col_index(n_panels = n_panel,
                                    n_assays = n_assay,
                                    is_shuffled = shuffle)

        # run function ----

        expect_no_condition(
          object =
            l_obj <- read_npx_wide_middle(
              df = df,
              file = wide_excel,
              data_type = data_t,
              assay_cols = cname$assay_cols,
              plate_cols = cname$plate_cols,
              qc_warn_cols = NULL,
              int_ctrl_cols = NULL
            )
        )

        # modify df for tests ----

        l_exp <- npx_wide_middle_test(df = df,
                                      n_panels = n_panel,
                                      n_assays = n_assay,
                                      data_type = data_t,
                                      is_shuffled = shuffle,
                                      cname = cname)

        # test on output ----

        expect_identical(
          object = l_obj$df_oid,
          expected = l_exp$df_oid
        )

        expect_identical(
          object = l_obj$df_plate,
          expected = l_exp$df_plate
        )

      }
    )

    # Quantified 1 panel ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {
        # write something in the excel file ----

        writeLines("foo", wide_excel)

        # create middle df ----

        n_panel <- 1L
        n_assay <- 45L
        data_t <- "Quantified"
        n_sample <- 200L
        shuffle <- FALSE
        int_ctrl <- TRUE

        df <- npx_wide_middle(n_panels = n_panel,
                              n_assays = n_assay,
                              n_samples = n_sample,
                              data_type = data_t,
                              show_int_ctrl = int_ctrl,
                              shuffle_assays = shuffle)

        # get columns ----

        cname <- npx_wide_col_index(n_panels = n_panel,
                                    n_assays = n_assay,
                                    is_shuffled = shuffle)

        # run function ----

        expect_no_condition(
          object =
            l_obj <- read_npx_wide_middle(
              df = df,
              file = wide_excel,
              data_type = data_t,
              assay_cols = cname$assay_cols,
              plate_cols = cname$plate_cols,
              qc_warn_cols = cname$qc_warn_cols,
              int_ctrl_cols = cname$int_ctrl_cols
            )
        )

        # modify df for tests ----

        l_exp <- npx_wide_middle_test(df = df,
                                      n_panels = n_panel,
                                      n_assays = n_assay,
                                      data_type = data_t,
                                      is_shuffled = shuffle,
                                      cname = cname)

        # test on output ----

        expect_identical(
          object = l_obj$df_oid,
          expected = l_exp$df_oid
        )

        expect_identical(
          object = l_obj$df_plate,
          expected = l_exp$df_plate
        )

        expect_identical(
          object = l_obj$df_qc_warn,
          expected = l_exp$df_qc_warn
        )

        expect_identical(
          object = l_obj$df_int_ctrl,
          expected = l_exp$df_int_ctrl
        )

      }
    )

    # Quantified multi-panel ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {
        # write something in the excel file ----

        writeLines("foo", wide_excel)

        # create middle df ----

        n_panel <- 4L
        n_assay <- 45L
        data_t <- "Quantified"
        n_sample <- 200L
        shuffle <- FALSE
        int_ctrl <- TRUE

        df <- npx_wide_middle(n_panels = n_panel,
                              n_assays = n_assay,
                              n_samples = n_sample,
                              data_type = data_t,
                              show_int_ctrl = int_ctrl,
                              shuffle_assays = shuffle)

        # get columns ----

        cname <- npx_wide_col_index(n_panels = n_panel,
                                    n_assays = n_assay,
                                    is_shuffled = shuffle)

        # run function ----

        expect_no_condition(
          object =
            l_obj <- read_npx_wide_middle(
              df = df,
              file = wide_excel,
              data_type = data_t,
              assay_cols = cname$assay_cols,
              plate_cols = cname$plate_cols,
              qc_warn_cols = cname$qc_warn_cols,
              int_ctrl_cols = cname$int_ctrl_cols
            )
        )

        # modify df for tests ----

        l_exp <- npx_wide_middle_test(df = df,
                                      n_panels = n_panel,
                                      n_assays = n_assay,
                                      data_type = data_t,
                                      is_shuffled = shuffle,
                                      cname = cname)

        # test on output ----

        expect_identical(
          object = l_obj$df_oid,
          expected = l_exp$df_oid
        )

        expect_identical(
          object = l_obj$df_plate,
          expected = l_exp$df_plate
        )

        expect_identical(
          object = l_obj$df_qc_warn,
          expected = l_exp$df_qc_warn
        )

        expect_identical(
          object = l_obj$df_int_ctrl,
          expected = l_exp$df_int_ctrl
        )

      }
    )

    # Quantified multi-panel shuffled ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {
        # write something in the excel file ----

        writeLines("foo", wide_excel)

        # create middle df ----

        n_panel <- 4L
        n_assay <- 45L
        data_t <- "Quantified"
        n_sample <- 200L
        shuffle <- TRUE
        int_ctrl <- TRUE

        df <- npx_wide_middle(n_panels = n_panel,
                              n_assays = n_assay,
                              n_samples = n_sample,
                              data_type = data_t,
                              show_int_ctrl = int_ctrl,
                              shuffle_assays = shuffle)

        # get columns ----

        cname <- npx_wide_col_index(n_panels = n_panel,
                                    n_assays = n_assay,
                                    is_shuffled = shuffle)

        # run function ----

        expect_no_condition(
          object =
            l_obj <- read_npx_wide_middle(
              df = df,
              file = wide_excel,
              data_type = data_t,
              assay_cols = cname$assay_cols,
              plate_cols = cname$plate_cols,
              qc_warn_cols = cname$qc_warn_cols,
              int_ctrl_cols = cname$int_ctrl_cols
            )
        )

        # modify df for tests ----

        l_exp <- npx_wide_middle_test(df = df,
                                      n_panels = n_panel,
                                      n_assays = n_assay,
                                      data_type = data_t,
                                      is_shuffled = shuffle,
                                      cname = cname)

        # test on output ----

        expect_identical(
          object = l_obj$df_oid,
          expected = l_exp$df_oid
        )

        expect_identical(
          object = l_obj$df_plate,
          expected = l_exp$df_plate
        )

        expect_identical(
          object = l_obj$df_qc_warn,
          expected = l_exp$df_qc_warn
        )

        expect_identical(
          object = l_obj$df_int_ctrl,
          expected = l_exp$df_int_ctrl
        )

      }
    )
  }
)

test_that(
  "read_npx_wide_middle - non-unique sample id",
  {
    # 1 duplicate ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # write something in the file ----
        writeLines("foo", wide_excel)

        # random middle df ----

        data_t <- "NPX"
        n_panel <- 2L
        n_assay <- 45L
        shuffle <- FALSE

        df <- npx_wide_middle(n_panels = n_panel,
                              n_assays = n_assay,
                              n_samples = 88L,
                              data_type = data_t,
                              show_int_ctrl = FALSE,
                              shuffle_assays = shuffle) |>
          dplyr::mutate(
            V1 = dplyr::if_else(.data[["V1"]] == "S2", "S1", .data[["V1"]])
          )

        cname <- npx_wide_col_index(n_panels = n_panel,
                                    n_assays = n_assay,
                                    is_shuffled = shuffle)

        # run function ----

        expect_error(
          object = read_npx_wide_middle(df = df,
                                        file = wide_excel,
                                        data_type = data_t,
                                        assay_cols = cname$assay_cols,
                                        plate_cols = cname$plate_cols,
                                        qc_warn_cols = cname$qc_warn_cols,
                                        int_ctrl_cols = NULL),
          regexp = "does not contain unique sample identifiers."
        )

      }
    )

    # 3 duplicates ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # write something in the file ----
        writeLines("foo", wide_excel)

        # random middle df ----

        data_t <- "NPX"
        n_panel <- 2L
        n_assay <- 45L
        shuffle <- FALSE

        df <- npx_wide_middle(n_panels = n_panel,
                              n_assays = n_assay,
                              n_samples = 88L,
                              data_type = data_t,
                              show_int_ctrl = FALSE,
                              shuffle_assays = shuffle) |>
          dplyr::mutate(
            V1 = dplyr::case_when(.data[["V1"]] %in% c("S2", "S3") ~ "S1",
                                  .data[["V1"]] %in% c("S4", "S5") ~ "S2",
                                  TRUE ~ .data[["V1"]],
                                  .default = .data[["V1"]])
          )

        cname <- npx_wide_col_index(n_panels = n_panel,
                                    n_assays = n_assay,
                                    is_shuffled = shuffle)

        # run function ----

        expect_error(
          object = read_npx_wide_middle(df = df,
                                        file = wide_excel,
                                        data_type = data_t,
                                        assay_cols = cname$assay_cols,
                                        plate_cols = cname$plate_cols,
                                        qc_warn_cols = cname$qc_warn_cols,
                                        int_ctrl_cols = NULL),
          regexp = "does not contain unique sample identifiers."
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_middle - uneven number of platid and qc_warning",
  {
    # Missing plateid column ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # write something in the file ----
        writeLines("foo", wide_excel)

        # random middle df ----

        data_t <- "NPX"
        n_panel <- 2L
        n_assay <- 45L
        shuffle <- FALSE

        df <- npx_wide_middle(n_panels = n_panel,
                              n_assays = n_assay,
                              n_samples = 88L,
                              data_type = data_t,
                              show_int_ctrl = FALSE,
                              shuffle_assays = shuffle)

        cname <- npx_wide_col_index(n_panels = n_panel,
                                    n_assays = n_assay,
                                    is_shuffled = shuffle)

        # remove one column with "Plate ID"
        df <- df |>
          dplyr::select(
            -dplyr::all_of(cname$plate_cols[1])
          )
        cname$plate_cols <- cname$plate_cols[cname$plate_cols
                                             != cname$plate_cols[1]]

        # run function ----

        expect_error(
          object = read_npx_wide_middle(df = df,
                                        file = wide_excel,
                                        data_type = data_t,
                                        assay_cols = cname$assay_cols,
                                        plate_cols = cname$plate_cols,
                                        qc_warn_cols = cname$qc_warn_cols,
                                        int_ctrl_cols = NULL),
          regexp = "Uneven number of entries of \"Plate ID\" and \"QC Warning\""
        )

      }
    )

    # Missing qc warning column ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # write something in the file ----
        writeLines("foo", wide_excel)

        # random middle df ----

        data_t <- "NPX"
        n_panel <- 2L
        n_assay <- 45L
        shuffle <- FALSE

        df <- npx_wide_middle(n_panels = n_panel,
                              n_assays = n_assay,
                              n_samples = 88L,
                              data_type = data_t,
                              show_int_ctrl = FALSE,
                              shuffle_assays = shuffle)

        cname <- npx_wide_col_index(n_panels = n_panel,
                                    n_assays = n_assay,
                                    is_shuffled = shuffle)

        # remove one column with "QC Warning"
        df <- df |>
          dplyr::select(
            -dplyr::all_of(cname$qc_warn_cols[1])
          )
        cname$qc_warn_cols <- cname$qc_warn_cols[cname$qc_warn_cols
                                                 != cname$qc_warn_cols[1]]

        # run function ----

        expect_error(
          object = read_npx_wide_middle(df = df,
                                        file = wide_excel,
                                        data_type = data_t,
                                        assay_cols = cname$assay_cols,
                                        plate_cols = cname$plate_cols,
                                        qc_warn_cols = cname$qc_warn_cols,
                                        int_ctrl_cols = NULL),
          regexp = "Uneven number of entries of \"Plate ID\" and \"QC Warning\""
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_middle - uneven number of internal controls",
  {
    # Not shuffled internal controls ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # write something in the file ----
        writeLines("foo", wide_excel)

        # random middle df ----

        data_t <- "Quantified"
        n_panel <- 2L
        n_assay <- 45L
        shuffle <- FALSE

        df <- npx_wide_middle(n_panels = n_panel,
                              n_assays = n_assay,
                              n_samples = 88L,
                              data_type = data_t,
                              show_int_ctrl = TRUE,
                              shuffle_assays = shuffle)

        cname <- npx_wide_col_index(n_panels = n_panel,
                                    n_assays = n_assay,
                                    is_shuffled = shuffle)

        # remove one column with "Internal Control"
        df <- df |>
          dplyr::select(
            -dplyr::all_of(cname$int_ctrl_cols[1])
          )
        cname$int_ctrl_cols <- cname$int_ctrl_cols[cname$int_ctrl_cols
                                                   != cname$int_ctrl_cols[1]]

        # run function ----

        expect_error(
          object = read_npx_wide_middle(df = df,
                                        file = wide_excel,
                                        data_type = data_t,
                                        assay_cols = cname$assay_cols,
                                        plate_cols = cname$plate_cols,
                                        qc_warn_cols = cname$qc_warn_cols,
                                        int_ctrl_cols = cname$int_ctrl_cols),
          regexp = "Uneven number of entries of \"Internal Control\" assays in"
        )

      }
    )

    # Shuffled internal controls ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # write something in the file ----
        writeLines("foo", wide_excel)

        # random middle df ----

        data_t <- "Quantified"
        n_panel <- 2L
        n_assay <- 45L
        shuffle <- TRUE

        df <- npx_wide_middle(n_panels = n_panel,
                              n_assays = n_assay,
                              n_samples = 88L,
                              data_type = data_t,
                              show_int_ctrl = TRUE,
                              shuffle_assays = shuffle)

        cname <- npx_wide_col_index(n_panels = n_panel,
                                    n_assays = n_assay,
                                    is_shuffled = shuffle)

        # remove one column with "Internal Control"
        df <- df |>
          dplyr::select(
            -dplyr::all_of(cname$int_ctrl_cols[1])
          )
        cname$int_ctrl_cols <- cname$int_ctrl_cols[cname$int_ctrl_cols
                                                   != cname$int_ctrl_cols[1]]

        # run function ----

        expect_error(
          object = read_npx_wide_middle(df = df,
                                        file = wide_excel,
                                        data_type = data_t,
                                        assay_cols = cname$assay_cols,
                                        plate_cols = cname$plate_cols,
                                        qc_warn_cols = cname$qc_warn_cols,
                                        int_ctrl_cols = cname$int_ctrl_cols),
          regexp = "Uneven number of entries of \"Internal Control\" assays in"
        )

      }
    )

    # 2 internal controls - NO ERROR ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # write something in the file ----
        writeLines("foo", wide_excel)

        # random middle df ----

        data_t <- "Quantified"
        n_panel <- 2L
        n_assay <- 45L
        shuffle <- TRUE

        df <- npx_wide_middle(n_panels = n_panel,
                              n_assays = n_assay,
                              n_samples = 88L,
                              data_type = data_t,
                              show_int_ctrl = TRUE,
                              shuffle_assays = shuffle)

        cname <- npx_wide_col_index(n_panels = n_panel,
                                    n_assays = n_assay,
                                    is_shuffled = shuffle)

        # remove one column with "Internal Control"
        df <- df |>
          dplyr::select(
            -dplyr::all_of(cname$int_ctrl_cols[1:2])
          )
        cname$int_ctrl_cols <- cname$int_ctrl_cols[!(
          cname$int_ctrl_cols %in% cname$int_ctrl_cols[1:2]
        )]

        # run function ----

        expect_no_condition(
          object = read_npx_wide_middle(df = df,
                                        file = wide_excel,
                                        data_type = data_t,
                                        assay_cols = cname$assay_cols,
                                        plate_cols = cname$plate_cols,
                                        qc_warn_cols = cname$qc_warn_cols,
                                        int_ctrl_cols = cname$int_ctrl_cols)
        )

      }
    )

    # 1 internal controls - NO ERROR ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # write something in the file ----
        writeLines("foo", wide_excel)

        # random middle df ----

        data_t <- "Quantified"
        n_panel <- 2L
        n_assay <- 45L
        shuffle <- FALSE

        df <- npx_wide_middle(n_panels = n_panel,
                              n_assays = n_assay,
                              n_samples = 88L,
                              data_type = data_t,
                              show_int_ctrl = TRUE,
                              shuffle_assays = shuffle)

        cname <- npx_wide_col_index(n_panels = n_panel,
                                    n_assays = n_assay,
                                    is_shuffled = shuffle)

        # remove one column with "Internal Control"
        df <- df |>
          dplyr::select(
            -dplyr::all_of(cname$int_ctrl_cols[1:4])
          )
        cname$int_ctrl_cols <- cname$int_ctrl_cols[!(
          cname$int_ctrl_cols %in% cname$int_ctrl_cols[1:4]
        )]

        # run function ----

        expect_no_condition(
          object = read_npx_wide_middle(df = df,
                                        file = wide_excel,
                                        data_type = data_t,
                                        assay_cols = cname$assay_cols,
                                        plate_cols = cname$plate_cols,
                                        qc_warn_cols = cname$qc_warn_cols,
                                        int_ctrl_cols = cname$int_ctrl_cols)
        )

      }
    )

  }
)
