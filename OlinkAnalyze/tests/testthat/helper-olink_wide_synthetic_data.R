# Help functions ----

## Header matrix ----

# return the top 2x2 matrix in the first two rows of an Olink excel wide file
olink_wide_head <- function(data_type) {

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
olink_wide_top <- function(olink_platform,
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

# This function converts the output of olink_wide_top to long to help with tests
# Input is the wide matrix, and output is the top matrix separated based on
# assays, plate_id & qc_warning and internal controls.
olink_wide_top_long <- function(df) {

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

  df_int_ctrl <- df |>
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
      df_int_ctrl = df_int_ctrl
    )
  )

}

## Middle matrix ----

# computes the middle matrix with assays measurements for each sample. The right
# hand side of it should contan information on Plate ID, QC_Warning and internal
# controls.
olink_wide_middle <- function(n_panels,
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

  df_p <- matrix(
    data = rep(plate_name, times = n_panels),
    nrow = n_samples,
    ncol = n_panels,
    dimnames = list(
      paste0("S", seq_len(n_samples)),
      paste0("P", seq_len(n_panels))
    )
  ) |>
    dplyr::as_tibble()

  rm(plate_name)

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

# takes as input the middle matrix and splits it into its components in long
# format to help with the testing.
#
# output contains df_oid, df_pid, df_qc_warn and df_int_ctrl
olink_wide_middle_long <- function(df,
                                   n_panels,
                                   n_assays,
                                   data_type,
                                   shuffle_assays,
                                   cname) {
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

    if (data_type == "Quantified" && "int_ctrl_cols" %in% names(cname)) {

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
olink_wide_bottom <- function(n_plates,
                              n_panels,
                              n_assays,
                              data_type,
                              show_int_ctrl = FALSE,
                              shuffle_assays = FALSE) {

  # add internal controls
  if (show_int_ctrl == TRUE && shuffle_assays == TRUE) {
    n_assays <- n_assays + 3L
  }

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
        {{last_v}} := paste0("Plate", seq_len(n_plates)) # nolint object_usage_linter
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
            {{last_v}} := paste0("Plate", seq_len(n_plates)) # nolint object_usage_linter
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
            {{last_v}} := paste0("Plate", seq_len(n_plates)) # nolint object_usage_linter
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

olink_wide_bottom_long <- function(df,
                                   data_type,
                                   col_split,
                                   int_ctrl_cols) {

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

  if (!is.null(int_ctrl_cols)) {
    l_df <- list(
      df_oid = df_t |>
        dplyr::filter(
          !(.data[["col_index"]] %in% int_ctrl_cols)
        ),
      df_int_ctrl = df_t |>
        dplyr::filter(
          .data[["col_index"]] %in% int_ctrl_cols
        )
    )
  } else {
    l_df <- list(
      df_oid = df_t
    )
  }

  return(l_df)

}

## Full matrix ----

# combines the full matrix mimicing and Olink excel wide file.
olink_wide <- function(olink_platform,
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

  df_head <- olink_wide_head(data_type = data_type)

  # top matrix ----

  df_top <- olink_wide_top(olink_platform = olink_platform,
                           n_panels = n_panels,
                           n_assays = n_assays,
                           data_type = data_type,
                           show_int_ctrl = show_int_ctrl,
                           loc_int_ctrl = loc_int_ctrl,
                           shuffle_assays = shuffle_assays)

  # middle matrix ----

  df_middle <- olink_wide_middle(n_panels = n_panels,
                                 n_assays = n_assays,
                                 n_samples = n_samples,
                                 data_type = data_type,
                                 show_int_ctrl = show_int_ctrl,
                                 shuffle_assays = shuffle_assays)

  # bottom matrix ----

  if (data_type != "Ct") {
    df_bottom <- olink_wide_bottom(
      n_plates = n_plates,
      n_panels = n_panels,
      n_assays = n_assays,
      data_type = data_type,
      show_int_ctrl = ifelse(data_type == "Quantified"
                             && show_int_ctrl == TRUE, TRUE, FALSE),
      shuffle_assays = ifelse(data_type == "Quantified"
                              && shuffle_assays == TRUE, TRUE, FALSE)
    )
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

  list_df <- list(
    df = df,
    df_top = df_top,
    df_middle = df_middle
  )

  if (data_type != "Ct") {
    list_df <- append(x = list_df,
                      values = list(df_bottom = df_bottom))
  }

  list_df <- append(x = list_df,
                    values = list(df_head = df_head,
                                  df_na = df_na))

  return(list_df)

}

# converts the full wide excel file to a long df.
olink_wide_long <- function(l_df_top,
                            l_df_middle,
                            l_df_bottom,
                            n_panels,
                            n_assays,
                            data_type,
                            shuffle_assays) {

  # convert the top matrix to a list of long df ----

  df_top <- olink_wide_top_long(df = l_df_top)

  # extract colnames to use below ----

  cname <- sapply(df_top, \(x) (x$col_index))
  n_cname <- c("assay_cols", "plate_cols", "qc_warn_cols", "int_ctrl_cols")
  cname <- cname[sapply(df_top, nrow) > 0L]
  n_cname <- n_cname[sapply(df_top, nrow) > 0L]
  names(cname) <- n_cname
  rm(n_cname)

  # convert the middle matrix to a list of long df ----

  df_middle <- olink_wide_middle_long(df = l_df_middle,
                                      n_panels = n_panels,
                                      n_assays = n_assays,
                                      data_type = data_type,
                                      shuffle_assays = shuffle_assays,
                                      cname = cname)

  # convert the bottom matrix to a long df ----

  if (!is.null(l_df_bottom)) {

    if (shuffle_assays == TRUE) {
      df_bottom <- olink_wide_bottom_long(
        df = l_df_bottom,
        data_type = data_type,
        col_split = colnames(l_df_bottom) |>
          tail(n = 1L),
        int_ctrl_cols = df_top$df_int_ctrl$col_index
      )
    } else {
      df_bottom <- olink_wide_bottom_long(
        df = l_df_bottom,
        data_type = data_type,
        col_split = colnames(l_df_bottom) |>
          tail(n = 1L),
        int_ctrl_cols = NULL
      )
    }
  }

  # prepare components of long df ----

  ## df oid ----

  df_oid <- df_middle$df_oid |>
    dplyr::left_join(
      df_top$df_oid,
      by = "col_index",
      relationship = "many-to-one"
    )

  ## df plate id ----

  df_plate <- df_middle$df_plate |>
    dplyr::left_join(
      df_top$df_plate,
      by = "col_index",
      relationship = "many-to-one"
    ) |>
    dplyr::select(
      -dplyr::any_of(c("Var", "Unit", "col_index"))
    )

  ## df qc warning ----

  if ("df_qc_warn" %in% names(df_middle)
      && "df_qc_warn" %in% names(df_top)) {

    df_qc_warn <- df_middle$df_qc_warn |>
      dplyr::left_join(
        df_top$df_qc_warn,
        by = "col_index",
        relationship = "many-to-one"
      ) |>
      dplyr::select(
        -dplyr::any_of(c("Var", "Unit", "col_index"))
      )

    df_plate_qc <- df_plate |>
      dplyr::left_join(
        df_qc_warn,
        by = c("SampleID", "Panel"),
        relationship = "one-to-one"
      )

  } else {
    df_plate_qc <- df_plate
  }

  ## combine oid and plate qc ----

  df_out <- df_oid |>
    dplyr::left_join(
      df_plate_qc,
      by = c("SampleID", "Panel"),
      relationship = "many-to-one"
    )

  ## df internal controls ----

  if ("df_int_ctrl" %in% names(df_middle)
      && "df_int_ctrl" %in% names(df_top)) {

    df_int_ctrl <- df_middle$df_int_ctrl |>
      dplyr::left_join(
        df_top$df_int_ctrl,
        by = "col_index",
        relationship = "many-to-one"
      )

    # move the names *Ctrl* to column Assay
    if (all(grepl(pattern = "ctrl",
                  x = df_int_ctrl$`Uniprot ID`,
                  ignore.case = TRUE))) {
      df_int_ctrl <- df_int_ctrl |>
        dplyr::mutate(
          Assay = .data[["Uniprot ID"]],
          `Uniprot ID` = NA_character_
        )
    }

    # join with the plate df
    df_int_ctrl_pid <- df_int_ctrl |>
      dplyr::left_join(
        df_plate_qc,
        by = c("SampleID", "Panel"),
        relationship = "many-to-one"
      )

    df_out <- df_out |>
      dplyr::bind_rows(df_int_ctrl_pid)
  }

  ## df bottom ----

  if (exists("df_bottom")) {

    df_bottom_merge <- df_bottom |>
      dplyr::bind_rows()

    if ("Plate ID" %in% colnames(df_bottom_merge)) {
      df_out <- df_out |>
        dplyr::left_join(
          df_bottom_merge,
          by = c("col_index" = "col_index",
                 "PlateID" = "Plate ID"),
          relationship = "many-to-many"
        )
    } else {
      df_out <- df_out |>
        dplyr::left_join(
          df_bottom_merge,
          by = c("col_index" = "col_index"),
          relationship = "many-to-many"
        )
    }
  }

  # remove col_index ----

  df_out <- df_out |>
    dplyr::select(
      -dplyr::all_of("col_index")
    )

  # return ----

  l_out <- list(
    df = df_out,
    df_oid = df_oid,
    df_plate = df_plate
  )

  l_long <- list(
    df_top = df_top,
    df_middle = df_middle
  )

  if (exists("df_bottom")) {
    l_out <- append(x = l_out, values = list(df_qc_warn = df_qc_warn))

    l_long <- append(x = l_long, values = list(df_bottom = df_bottom))
  }

  if (exists("df_int_ctrl")) {
    l_out <- append(x = l_out, values = list(df_int_ctrl = df_int_ctrl))
  }

  l_out <- append(x = l_out, values = l_long)

  return(l_out)

}

# returns a nested list with all the components of the long and the wide data
# frames, as well as the full matrices
olink_wide_synthetic <- function(olink_platform,
                                 data_type,
                                 n_panels,
                                 n_assays,
                                 n_samples,
                                 show_int_ctrl = TRUE,
                                 loc_int_ctrl = "V3",
                                 shuffle_assays = FALSE) {

  l_wide <- olink_wide(olink_platform = olink_platform,
                       data_type = data_type,
                       n_panels = n_panels,
                       n_assays = n_assays,
                       n_samples = n_samples,
                       show_int_ctrl = show_int_ctrl,
                       loc_int_ctrl = loc_int_ctrl,
                       shuffle_assays = shuffle_assays)

  if (!("df_bottom" %in% names(l_wide))) {
    l_wide_bottom <- NULL
  } else {
    l_wide_bottom <- l_wide$df_bottom
  }

  l_long <- olink_wide_long(l_df_top = l_wide$df_top,
                            l_df_middle = l_wide$df_middle,
                            l_df_bottom = l_wide_bottom,
                            n_panels = n_panels,
                            n_assays = n_assays,
                            data_type = data_type,
                            shuffle_assays = shuffle_assays)

  return(
    list(
      wide = l_wide,
      long = l_long
    )
  )
}
