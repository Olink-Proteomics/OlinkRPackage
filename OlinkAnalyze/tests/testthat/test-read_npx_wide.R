# Help functions ----

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

  df_meta <- df |>
    dplyr::filter(
      is.na(.data[["OlinkID"]])
      & .data[["Assay"]] %in% c("Plate ID", "QC Warning")
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
      df_meta = df_meta,
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
                                                     data_type = data_t)
        )

        # check that output exists
        expect_true(object = exists("df_out"))

        # check that df_top works
        top_start <- 3L
        top_end <- ifelse(data_t == "Quantified", 7L, 6L)
        expect_identical(
          object = df_out$df_top,
          expected = df |>
            dplyr::slice(
              top_start:top_end
            )
        )

        # check that df_mid works
        sample_start <- ifelse(data_t == "Quantified", 9L, 8L)
        sample_end <- sample_start + n_sample - 1L
        expect_identical(
          object = df_out$df_mid,
          expected = df |>
            dplyr::slice(
              sample_start:sample_end
            )
        )

        # check that df_bottom works
        bottom_start <- sample_end + 2L
        bottom_end <- ifelse(data_t == "Quantified",
                             bottom_start + 4L + (3 * n_plates) - 1,
                             bottom_start + 3L - 1)
        expect_identical(
          object = df_out$df_bottom,
          expected = df |>
            dplyr::slice(
              bottom_start:bottom_end
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
                                                     data_type = data_t)
        )

        # check that output exists
        expect_true(object = exists("df_out"))

        # check that df_top works
        top_start <- 3L
        top_end <- ifelse(data_t == "Quantified", 7L, 6L)
        expect_identical(
          object = df_out$df_top,
          expected = df |>
            dplyr::slice(
              top_start:top_end
            )
        )

        # check that df_mid works
        sample_start <- ifelse(data_t == "Quantified", 9L, 8L)
        sample_end <- sample_start + n_sample - 1L
        expect_identical(
          object = df_out$df_mid,
          expected = df |>
            dplyr::slice(
              sample_start:sample_end
            )
        )

        # check that df_bottom works
        bottom_start <- sample_end + 2L
        bottom_end <- ifelse(data_t == "Quantified",
                             bottom_start + 4L + (3 * n_plates) - 1,
                             bottom_start + 3L - 1)
        expect_identical(
          object = df_out$df_bottom,
          expected = df |>
            dplyr::slice(
              bottom_start:bottom_end
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
                                                     data_type = data_t)
        )

        # check that output exists
        expect_true(object = exists("df_out"))

        # check that df_top works
        top_start <- 3L
        top_end <- ifelse(data_t == "Quantified", 7L, 6L)
        expect_identical(
          object = df_out$df_top,
          expected = df |>
            dplyr::slice(
              top_start:top_end
            )
        )

        # check that df_mid works
        sample_start <- ifelse(data_t == "Quantified", 9L, 8L)
        sample_end <- sample_start + n_sample - 1L
        expect_identical(
          object = df_out$df_mid,
          expected = df |>
            dplyr::slice(
              sample_start:sample_end
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
                                                     data_type = data_t)
        )

        # check that output exists
        expect_true(object = exists("df_out"))

        # check that df_top works
        top_start <- 3L
        top_end <- ifelse(data_t == "Quantified", 7L, 6L)
        expect_identical(
          object = df_out$df_top,
          expected = df |>
            dplyr::slice(
              top_start:top_end
            )
        )

        # check that df_mid works
        sample_start <- ifelse(data_t == "Quantified", 9L, 8L)
        sample_end <- sample_start + n_sample - 1L
        expect_identical(
          object = df_out$df_mid,
          expected = df |>
            dplyr::slice(
              sample_start:sample_end
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
                                                     data_type = data_t)
        )

        # check that output exists
        expect_true(object = exists("df_out"))

        # check that df_top works
        top_start <- 3L
        top_end <- ifelse(data_t == "Quantified", 7L, 6L)
        expect_identical(
          object = df_out$df_top,
          expected = df |>
            dplyr::slice(
              top_start:top_end
            )
        )

        # check that df_mid works
        sample_start <- ifelse(data_t == "Quantified", 9L, 8L)
        sample_end <- sample_start + n_sample - 1L
        expect_identical(
          object = df_out$df_mid,
          expected = df |>
            dplyr::slice(
              sample_start:sample_end
            )
        )

        # check that df_bottom works
        bottom_start <- sample_end + 2L
        bottom_end <- ifelse(data_t == "Quantified",
                             bottom_start + 4L + (3 * n_plates) - 1,
                             bottom_start + 3L - 1)
        expect_identical(
          object = df_out$df_bottom,
          expected = df |>
            dplyr::slice(
              bottom_start:bottom_end
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
                                                     data_type = data_t)
        )

        # check that output exists
        expect_true(object = exists("df_out"))

        # check that df_top works
        top_start <- 3L
        top_end <- ifelse(data_t == "Quantified", 7L, 6L)
        expect_identical(
          object = df_out$df_top,
          expected = df |>
            dplyr::slice(
              top_start:top_end
            )
        )

        # check that df_mid works
        sample_start <- ifelse(data_t == "Quantified", 9L, 8L)
        sample_end <- sample_start + n_sample - 1L
        expect_identical(
          object = df_out$df_mid,
          expected = df |>
            dplyr::slice(
              sample_start:sample_end
            )
        )

        # check that df_bottom works
        bottom_start <- sample_end + 2L
        bottom_end <- ifelse(data_t == "Quantified",
                             bottom_start + 4L + (3 * n_plates) - 1,
                             bottom_start + 3L - 1)
        expect_identical(
          object = df_out$df_bottom,
          expected = df |>
            dplyr::slice(
              bottom_start:bottom_end
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
                                                     data_type = data_t)
        )

        # check that output exists
        expect_true(object = exists("df_out"))

        # check that df_top works
        top_start <- 3L
        top_end <- ifelse(data_t == "Quantified", 7L, 6L)
        expect_identical(
          object = df_out$df_top,
          expected = df |>
            dplyr::slice(
              top_start:top_end
            )
        )

        # check that df_mid works
        sample_start <- ifelse(data_t == "Quantified", 9L, 8L)
        sample_end <- sample_start + n_sample - 1L
        expect_identical(
          object = df_out$df_mid,
          expected = df |>
            dplyr::slice(
              sample_start:sample_end
            )
        )

        # check that df_bottom works
        bottom_start <- sample_end + 2L
        bottom_end <- ifelse(data_t == "Quantified",
                             bottom_start + 4L + (3 * n_plates) - 1,
                             bottom_start + 3L - 1)
        expect_identical(
          object = df_out$df_bottom,
          expected = df |>
            dplyr::slice(
              bottom_start:bottom_end
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
                                                     data_type = data_t)
        )

        # check that output exists
        expect_true(object = exists("df_out"))

        # check that df_top works
        top_start <- 3L
        top_end <- ifelse(data_t == "Quantified", 7L, 6L)
        expect_identical(
          object = df_out$df_top,
          expected = df |>
            dplyr::slice(
              top_start:top_end
            )
        )

        # check that df_mid works
        sample_start <- ifelse(data_t == "Quantified", 9L, 8L)
        sample_end <- sample_start + n_sample - 1L
        expect_identical(
          object = df_out$df_mid,
          expected = df |>
            dplyr::slice(
              sample_start:sample_end
            )
        )

        # check that df_bottom works
        bottom_start <- sample_end + 2L
        bottom_end <- ifelse(data_t == "Quantified",
                             bottom_start + 4L + (3 * n_plates) - 1,
                             bottom_start + 3L - 1)
        expect_identical(
          object = df_out$df_bottom,
          expected = df |>
            dplyr::slice(
              bottom_start:bottom_end
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
                                           data_type = data_t),
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
                                           data_type = data_t),
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

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs for NPX
        expect_error(
          object = read_npx_wide_split_row(file = wide_excel,
                                           data_type = "NPX"),
          regexp = "contains 1 row with all columns NA, but based on"
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

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs for Ct
        expect_error(
          object = read_npx_wide_split_row(file = wide_excel,
                                           data_type = "Ct"),
          regexp = "contains 2 rows with all columns NA, but based on"
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

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs for Quantified
        expect_error(
          object = read_npx_wide_split_row(file = wide_excel,
                                           data_type = "Quantified"),
          regexp = "contains 1 row with all columns NA, but based on"
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

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs for NPX
        expect_error(
          object = read_npx_wide_split_row(file = wide_excel,
                                           data_type = data_t),
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

        # write file
        writeLines("foo", wide_excel)

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs for NPX
        expect_no_condition(
          object = read_npx_wide_check_top(df = df,
                                           file = wide_excel,
                                           data_type = data_t)
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

        # write file
        writeLines("foo", wide_excel)

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs for NPX
        expect_no_condition(
          object = read_npx_wide_check_top(df = df,
                                           file = wide_excel,
                                           data_type = data_t)
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

        # write file
        writeLines("foo", wide_excel)

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs
        expect_no_condition(
          object = read_npx_wide_check_top(df = df,
                                           file = wide_excel,
                                           data_type = data_t)
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

        # write file
        writeLines("foo", wide_excel)

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs
        expect_no_condition(
          object = read_npx_wide_check_top(df = df,
                                           file = wide_excel,
                                           data_type = data_t)
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

        # write file
        writeLines("foo", wide_excel)

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs
        expect_no_condition(
          object = read_npx_wide_check_top(df = df,
                                           file = wide_excel,
                                           data_type = data_t)
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

        # write file
        writeLines("foo", wide_excel)

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs
        expect_no_condition(
          object = read_npx_wide_check_top(df = df,
                                           file = wide_excel,
                                           data_type = data_t)
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

        # write file
        writeLines("foo", wide_excel)

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs
        expect_no_condition(
          object = read_npx_wide_check_top(df = df,
                                           file = wide_excel,
                                           data_type = data_t)
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_check_top - unexpected number of assay rows",
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

        # write file
        writeLines("foo", wide_excel)

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs
        expect_error(
          object = read_npx_wide_check_top(df = df,
                                           file = wide_excel,
                                           data_type = data_t),
          regexp = "We identified 3 rows containing data about assays in file"
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

        # write file
        writeLines("foo", wide_excel)

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs
        expect_error(
          object = read_npx_wide_check_top(df = df,
                                           file = wide_excel,
                                           data_type = data_t),
          regexp = "We identified 6 rows containing data about assays in file"
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_check_top - non-matching number of assay rows",
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
                           shuffle_assays = FALSE)

        # write file
        writeLines("foo", wide_excel)

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs
        expect_error(
          object = read_npx_wide_check_top(df = df,
                                           file = wide_excel,
                                           data_type = "Quantified"),
          regexp = "while we expected 5"
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
                           show_int_ctrl = FALSE,
                           loc_int_ctrl = "V3",
                           shuffle_assays = FALSE) |>
          dplyr::filter(
            .data[["V1"]] != "OlinkID"
          )

        # write file
        writeLines("foo", wide_excel)

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs
        expect_error(
          object = read_npx_wide_check_top(df = df,
                                           file = wide_excel,
                                           data_type = data_t),
          regexp = "while we expected 5"
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_check_top - incorrect values in column 1",
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
          dplyr::mutate(
            V1 = dplyr::if_else(.data[["V1"]] == "OlinkID",
                                "OlinkID_2",
                                .data[["V1"]])
          )

        # write file
        writeLines("foo", wide_excel)

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs
        expect_error(
          object = read_npx_wide_check_top(df = df,
                                           file = wide_excel,
                                           data_type = data_t),
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
                           show_int_ctrl = FALSE,
                           loc_int_ctrl = "V3",
                           shuffle_assays = FALSE) |>
          dplyr::mutate(
            V1 = dplyr::if_else(.data[["V1"]] == "Unit",
                                "Unit_2",
                                .data[["V1"]])
          )

        # write file
        writeLines("foo", wide_excel)

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs
        expect_error(
          object = read_npx_wide_check_top(df = df,
                                           file = wide_excel,
                                           data_type = data_t),
          regexp = "Column 1 of of the top matrix with assay metadata in file"
        )

      }
    )

  }
)

# Test read_npx_wide_split_col ----

test_that(
  "read_npx_wide_split_col - NPX - works",
  {
    ## T48 ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df----

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

        # write something in the file
        writeLines("foo", wide_excel)

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs
        expect_no_condition(
          object = x_index <- read_npx_wide_split_col(df = df,
                                                      file = wide_excel)
        )

        # check that output exists
        expect_true(object = exists("x_index"))

        # check that df_top works
        expect_identical(
          object = stringr::str_sub(string = x_index, start = 2L) |>
            as.integer(),
          expected = (n_assay * n_panel) + 2L
        )

      }
    )

    ## T48 shuffled ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df----

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
                           shuffle_assays = TRUE)

        # write something in the file
        writeLines("foo", wide_excel)

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs
        expect_no_condition(
          object = x_index <- read_npx_wide_split_col(df = df,
                                                      file = wide_excel)
        )

        # check that output exists
        expect_true(object = exists("x_index"))

        # check that df_top works
        expect_identical(
          object = stringr::str_sub(string = x_index, start = 2L) |>
            as.integer(),
          expected = (n_assay * n_panel) + 2L
        )

      }
    )

    ## T48 multi-plate shuffled ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df----

        o_platform <- "Target 48"
        n_panel <- 4L
        n_assay <- 45L
        data_t <- "NPX"

        df <- npx_wide_top(olink_platform = o_platform,
                           n_panels = n_panel,
                           n_assays = n_assay,
                           data_type = data_t,
                           show_int_ctrl = FALSE,
                           loc_int_ctrl = "V3",
                           shuffle_assays = TRUE)

        # write something in the file
        writeLines("foo", wide_excel)

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs
        expect_no_condition(
          object = x_index <- read_npx_wide_split_col(df = df,
                                                      file = wide_excel)
        )

        # check that output exists
        expect_true(object = exists("x_index"))

        # check that df_top works
        expect_identical(
          object = stringr::str_sub(string = x_index, start = 2L) |>
            as.integer(),
          expected = (n_assay * n_panel) + 2L
        )

      }
    )

    ## T48 multi-plate shuffled fewer assays ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df----

        o_platform <- "Target 48"
        n_panel <- 4L
        n_assay <- 45L
        data_t <- "NPX"

        random_assays <- sample(
          x = paste0("V",
                     tail(x = seq_len((n_panel * n_assay) + 1L), n = -1L)),
          size = 100L,
          replace = FALSE
        )

        df <- npx_wide_top(olink_platform = o_platform,
                           n_panels = n_panel,
                           n_assays = n_assay,
                           data_type = data_t,
                           show_int_ctrl = FALSE,
                           loc_int_ctrl = "V3",
                           shuffle_assays = TRUE) |>
          dplyr::select(
            -dplyr::all_of(random_assays)
          )

        # write something in the file
        writeLines("foo", wide_excel)

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs
        expect_no_condition(
          object = x_index <- read_npx_wide_split_col(df = df,
                                                      file = wide_excel)
        )

        # check that output exists
        expect_true(object = exists("x_index"))

        # check that df_top works
        expect_identical(
          object = stringr::str_sub(string = x_index, start = 2L) |>
            as.integer(),
          expected = (n_assay * n_panel) + 2L
        )

      }
    )

    ## T96 ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df----

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

        # write something in the file
        writeLines("foo", wide_excel)

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs
        expect_no_condition(
          object = x_index <- read_npx_wide_split_col(df = df,
                                                      file = wide_excel)
        )

        # check that output exists
        expect_true(object = exists("x_index"))

        # check that df_top works
        expect_identical(
          object = stringr::str_sub(string = x_index, start = 2L) |>
            as.integer(),
          expected = (n_assay * n_panel) + 2L
        )

      }
    )

    ## T96 shuffled ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df----

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
                           shuffle_assays = TRUE)

        # write something in the file
        writeLines("foo", wide_excel)

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs
        expect_no_condition(
          object = x_index <- read_npx_wide_split_col(df = df,
                                                      file = wide_excel)
        )

        # check that output exists
        expect_true(object = exists("x_index"))

        # check that df_top works
        expect_identical(
          object = stringr::str_sub(string = x_index, start = 2L) |>
            as.integer(),
          expected = (n_assay * n_panel) + 2L
        )

      }
    )

    ## T96 multi-plate shuffled ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df----

        o_platform <- "Target 96"
        n_panel <- 4L
        n_assay <- 92L
        data_t <- "NPX"

        df <- npx_wide_top(olink_platform = o_platform,
                           n_panels = n_panel,
                           n_assays = n_assay,
                           data_type = data_t,
                           show_int_ctrl = FALSE,
                           loc_int_ctrl = "V3",
                           shuffle_assays = TRUE)

        # write something in the file
        writeLines("foo", wide_excel)

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs
        expect_no_condition(
          object = x_index <- read_npx_wide_split_col(df = df,
                                                      file = wide_excel)
        )

        # check that output exists
        expect_true(object = exists("x_index"))

        # check that df_top works
        expect_identical(
          object = stringr::str_sub(string = x_index, start = 2L) |>
            as.integer(),
          expected = (n_assay * n_panel) + 2L
        )

      }
    )

    ## T96 multi-plate shuffled fewer assays ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df----

        o_platform <- "Target 96"
        n_panel <- 4L
        n_assay <- 92L
        data_t <- "NPX"

        random_assays <- sample(
          x = paste0("V",
                     tail(x = seq_len((n_panel * n_assay) + 1L), n = -1L)),
          size = 100L,
          replace = FALSE
        )

        df <- npx_wide_top(olink_platform = o_platform,
                           n_panels = n_panel,
                           n_assays = n_assay,
                           data_type = data_t,
                           show_int_ctrl = FALSE,
                           loc_int_ctrl = "V3",
                           shuffle_assays = TRUE) |>
          dplyr::select(
            -dplyr::all_of(random_assays)
          )

        # write something in the file
        writeLines("foo", wide_excel)

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs
        expect_no_condition(
          object = x_index <- read_npx_wide_split_col(df = df,
                                                      file = wide_excel)
        )

        # check that output exists
        expect_true(object = exists("x_index"))

        # check that df_top works
        expect_identical(
          object = stringr::str_sub(string = x_index, start = 2L) |>
            as.integer(),
          expected = (n_assay * n_panel) + 2L
        )

      }
    )

    ## Flex/Focus ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df----

        o_platform <- "Focus"
        n_panel <- 1L
        n_assay <- 21L
        data_t <- "NPX"

        df <- npx_wide_top(olink_platform = o_platform,
                           n_panels = n_panel,
                           n_assays = n_assay,
                           data_type = data_t,
                           show_int_ctrl = FALSE,
                           loc_int_ctrl = "V3",
                           shuffle_assays = FALSE)

        # write something in the file
        writeLines("foo", wide_excel)

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs
        expect_no_condition(
          object = x_index <- read_npx_wide_split_col(df = df,
                                                      file = wide_excel)
        )

        # check that output exists
        expect_true(object = exists("x_index"))

        # check that df_top works
        expect_identical(
          object = stringr::str_sub(string = x_index, start = 2L) |>
            as.integer(),
          expected = (n_assay * n_panel) + 2L
        )

      }
    )

    ## Flex/Focus shuffled ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df----

        o_platform <- "Focus"
        n_panel <- 1L
        n_assay <- 21L
        data_t <- "NPX"

        df <- npx_wide_top(olink_platform = o_platform,
                           n_panels = n_panel,
                           n_assays = n_assay,
                           data_type = data_t,
                           show_int_ctrl = FALSE,
                           loc_int_ctrl = "V3",
                           shuffle_assays = TRUE)

        # write something in the file
        writeLines("foo", wide_excel)

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs
        expect_no_condition(
          object = x_index <- read_npx_wide_split_col(df = df,
                                                      file = wide_excel)
        )

        # check that output exists
        expect_true(object = exists("x_index"))

        # check that df_top works
        expect_identical(
          object = stringr::str_sub(string = x_index, start = 2L) |>
            as.integer(),
          expected = (n_assay * n_panel) + 2L
        )

      }
    )

    ## Flex/Focus multi-plate shuffled ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df----

        o_platform <- "Focus"
        n_panel <- 4L
        n_assay <- 21L
        data_t <- "NPX"

        df <- npx_wide_top(olink_platform = o_platform,
                           n_panels = n_panel,
                           n_assays = n_assay,
                           data_type = data_t,
                           show_int_ctrl = FALSE,
                           loc_int_ctrl = "V3",
                           shuffle_assays = TRUE)

        # write something in the file
        writeLines("foo", wide_excel)

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs
        expect_no_condition(
          object = x_index <- read_npx_wide_split_col(df = df,
                                                      file = wide_excel)
        )

        # check that output exists
        expect_true(object = exists("x_index"))

        # check that df_top works
        expect_identical(
          object = stringr::str_sub(string = x_index, start = 2L) |>
            as.integer(),
          expected = (n_assay * n_panel) + 2L
        )

      }
    )

    ## Flex/Focus multi-plate shuffled fewer assays ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df----

        o_platform <- "Focus"
        n_panel <- 4L
        n_assay <- 21L
        data_t <- "NPX"

        random_assays <- sample(
          x = paste0("V",
                     tail(x = seq_len((n_panel * n_assay) + 1L), n = -1L)),
          size = 50L,
          replace = FALSE
        )

        df <- npx_wide_top(olink_platform = o_platform,
                           n_panels = n_panel,
                           n_assays = n_assay,
                           data_type = data_t,
                           show_int_ctrl = FALSE,
                           loc_int_ctrl = "V3",
                           shuffle_assays = TRUE) |>
          dplyr::select(
            -dplyr::all_of(random_assays)
          )

        # write something in the file
        writeLines("foo", wide_excel)

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs
        expect_no_condition(
          object = x_index <- read_npx_wide_split_col(df = df,
                                                      file = wide_excel)
        )

        # check that output exists
        expect_true(object = exists("x_index"))

        # check that df_top works
        expect_identical(
          object = stringr::str_sub(string = x_index, start = 2L) |>
            as.integer(),
          expected = (n_assay * n_panel) + 2L
        )

      }
    )


  }
)

test_that(
  "read_npx_wide_split_col - Ct - works",
  {
    ## T48 ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df----

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

        # write something in the file
        writeLines("foo", wide_excel)

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs
        expect_no_condition(
          object = x_index <- read_npx_wide_split_col(df = df,
                                                      file = wide_excel)
        )

        # check that output exists
        expect_true(object = exists("x_index"))

        # check that df_top works
        expect_identical(
          object = stringr::str_sub(string = x_index, start = 2L) |>
            as.integer(),
          expected = (n_assay * n_panel) + 2L
        )

      }
    )

    ## T48 shuffled ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df----

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
                           shuffle_assays = TRUE)

        # write something in the file
        writeLines("foo", wide_excel)

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs
        expect_no_condition(
          object = x_index <- read_npx_wide_split_col(df = df,
                                                      file = wide_excel)
        )

        # check that output exists
        expect_true(object = exists("x_index"))

        # check that df_top works
        expect_identical(
          object = stringr::str_sub(string = x_index, start = 2L) |>
            as.integer(),
          expected = (n_assay * n_panel) + 2L
        )

      }
    )

    ## T48 multi-plate shuffled ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df----

        o_platform <- "Target 48"
        n_panel <- 4L
        n_assay <- 45L
        data_t <- "Ct"

        df <- npx_wide_top(olink_platform = o_platform,
                           n_panels = n_panel,
                           n_assays = n_assay,
                           data_type = data_t,
                           show_int_ctrl = FALSE,
                           loc_int_ctrl = "V3",
                           shuffle_assays = TRUE)

        # write something in the file
        writeLines("foo", wide_excel)

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs
        expect_no_condition(
          object = x_index <- read_npx_wide_split_col(df = df,
                                                      file = wide_excel)
        )

        # check that output exists
        expect_true(object = exists("x_index"))

        # check that df_top works
        expect_identical(
          object = stringr::str_sub(string = x_index, start = 2L) |>
            as.integer(),
          expected = (n_assay * n_panel) + 2L
        )

      }
    )

    ## T48 multi-plate shuffled fewer assays ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df----

        o_platform <- "Target 48"
        n_panel <- 4L
        n_assay <- 45L
        data_t <- "Ct"

        random_assays <- sample(
          x = paste0("V",
                     tail(x = seq_len((n_panel * n_assay) + 1L), n = -1L)),
          size = 100L,
          replace = FALSE
        )

        df <- npx_wide_top(olink_platform = o_platform,
                           n_panels = n_panel,
                           n_assays = n_assay,
                           data_type = data_t,
                           show_int_ctrl = FALSE,
                           loc_int_ctrl = "V3",
                           shuffle_assays = TRUE) |>
          dplyr::select(
            -dplyr::all_of(random_assays)
          )

        # write something in the file
        writeLines("foo", wide_excel)

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs
        expect_no_condition(
          object = x_index <- read_npx_wide_split_col(df = df,
                                                      file = wide_excel)
        )

        # check that output exists
        expect_true(object = exists("x_index"))

        # check that df_top works
        expect_identical(
          object = stringr::str_sub(string = x_index, start = 2L) |>
            as.integer(),
          expected = (n_assay * n_panel) + 2L
        )

      }
    )

    ## T96 ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df----

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

        # write something in the file
        writeLines("foo", wide_excel)

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs
        expect_no_condition(
          object = x_index <- read_npx_wide_split_col(df = df,
                                                      file = wide_excel)
        )

        # check that output exists
        expect_true(object = exists("x_index"))

        # check that df_top works
        expect_identical(
          object = stringr::str_sub(string = x_index, start = 2L) |>
            as.integer(),
          expected = (n_assay * n_panel) + 2L
        )

      }
    )

    ## T96 shuffled ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df----

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
                           shuffle_assays = TRUE)

        # write something in the file
        writeLines("foo", wide_excel)

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs
        expect_no_condition(
          object = x_index <- read_npx_wide_split_col(df = df,
                                                      file = wide_excel)
        )

        # check that output exists
        expect_true(object = exists("x_index"))

        # check that df_top works
        expect_identical(
          object = stringr::str_sub(string = x_index, start = 2L) |>
            as.integer(),
          expected = (n_assay * n_panel) + 2L
        )

      }
    )

    ## T96 multi-plate shuffled ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df----

        o_platform <- "Target 96"
        n_panel <- 4L
        n_assay <- 92L
        data_t <- "Ct"

        df <- npx_wide_top(olink_platform = o_platform,
                           n_panels = n_panel,
                           n_assays = n_assay,
                           data_type = data_t,
                           show_int_ctrl = FALSE,
                           loc_int_ctrl = "V3",
                           shuffle_assays = TRUE)

        # write something in the file
        writeLines("foo", wide_excel)

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs
        expect_no_condition(
          object = x_index <- read_npx_wide_split_col(df = df,
                                                      file = wide_excel)
        )

        # check that output exists
        expect_true(object = exists("x_index"))

        # check that df_top works
        expect_identical(
          object = stringr::str_sub(string = x_index, start = 2L) |>
            as.integer(),
          expected = (n_assay * n_panel) + 2L
        )

      }
    )

    ## T96 multi-plate shuffled fewer assays ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df----

        o_platform <- "Target 96"
        n_panel <- 4L
        n_assay <- 92L
        data_t <- "Ct"

        random_assays <- sample(
          x = paste0("V",
                     tail(x = seq_len((n_panel * n_assay) + 1L), n = -1L)),
          size = 100L,
          replace = FALSE
        )

        df <- npx_wide_top(olink_platform = o_platform,
                           n_panels = n_panel,
                           n_assays = n_assay,
                           data_type = data_t,
                           show_int_ctrl = FALSE,
                           loc_int_ctrl = "V3",
                           shuffle_assays = TRUE) |>
          dplyr::select(
            -dplyr::all_of(random_assays)
          )

        # write something in the file
        writeLines("foo", wide_excel)

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs
        expect_no_condition(
          object = x_index <- read_npx_wide_split_col(df = df,
                                                      file = wide_excel)
        )

        # check that output exists
        expect_true(object = exists("x_index"))

        # check that df_top works
        expect_identical(
          object = stringr::str_sub(string = x_index, start = 2L) |>
            as.integer(),
          expected = (n_assay * n_panel) + 2L
        )

      }
    )

    ## Flex/Focus ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df----

        o_platform <- "Focus"
        n_panel <- 1L
        n_assay <- 21L
        data_t <- "Ct"

        df <- npx_wide_top(olink_platform = o_platform,
                           n_panels = n_panel,
                           n_assays = n_assay,
                           data_type = data_t,
                           show_int_ctrl = FALSE,
                           loc_int_ctrl = "V3",
                           shuffle_assays = FALSE)

        # write something in the file
        writeLines("foo", wide_excel)

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs
        expect_no_condition(
          object = x_index <- read_npx_wide_split_col(df = df,
                                                      file = wide_excel)
        )

        # check that output exists
        expect_true(object = exists("x_index"))

        # check that df_top works
        expect_identical(
          object = stringr::str_sub(string = x_index, start = 2L) |>
            as.integer(),
          expected = (n_assay * n_panel) + 2L
        )

      }
    )

    ## Flex/Focus shuffled ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df----

        o_platform <- "Focus"
        n_panel <- 1L
        n_assay <- 21L
        data_t <- "Ct"

        df <- npx_wide_top(olink_platform = o_platform,
                           n_panels = n_panel,
                           n_assays = n_assay,
                           data_type = data_t,
                           show_int_ctrl = FALSE,
                           loc_int_ctrl = "V3",
                           shuffle_assays = TRUE)

        # write something in the file
        writeLines("foo", wide_excel)

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs
        expect_no_condition(
          object = x_index <- read_npx_wide_split_col(df = df,
                                                      file = wide_excel)
        )

        # check that output exists
        expect_true(object = exists("x_index"))

        # check that df_top works
        expect_identical(
          object = stringr::str_sub(string = x_index, start = 2L) |>
            as.integer(),
          expected = (n_assay * n_panel) + 2L
        )

      }
    )

    ## Flex/Focus multi-plate shuffled ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df----

        o_platform <- "Focus"
        n_panel <- 4L
        n_assay <- 21L
        data_t <- "Ct"

        df <- npx_wide_top(olink_platform = o_platform,
                           n_panels = n_panel,
                           n_assays = n_assay,
                           data_type = data_t,
                           show_int_ctrl = FALSE,
                           loc_int_ctrl = "V3",
                           shuffle_assays = TRUE)

        # write something in the file
        writeLines("foo", wide_excel)

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs
        expect_no_condition(
          object = x_index <- read_npx_wide_split_col(df = df,
                                                      file = wide_excel)
        )

        # check that output exists
        expect_true(object = exists("x_index"))

        # check that df_top works
        expect_identical(
          object = stringr::str_sub(string = x_index, start = 2L) |>
            as.integer(),
          expected = (n_assay * n_panel) + 2L
        )

      }
    )

    ## Flex/Focus multi-plate shuffled fewer assays ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df----

        o_platform <- "Focus"
        n_panel <- 4L
        n_assay <- 21L
        data_t <- "Ct"

        random_assays <- sample(
          x = paste0("V",
                     tail(x = seq_len((n_panel * n_assay) + 1L), n = -1L)),
          size = 50L,
          replace = FALSE
        )

        df <- npx_wide_top(olink_platform = o_platform,
                           n_panels = n_panel,
                           n_assays = n_assay,
                           data_type = data_t,
                           show_int_ctrl = FALSE,
                           loc_int_ctrl = "V3",
                           shuffle_assays = TRUE) |>
          dplyr::select(
            -dplyr::all_of(random_assays)
          )

        # write something in the file
        writeLines("foo", wide_excel)

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs
        expect_no_condition(
          object = x_index <- read_npx_wide_split_col(df = df,
                                                      file = wide_excel)
        )

        # check that output exists
        expect_true(object = exists("x_index"))

        # check that df_top works
        expect_identical(
          object = stringr::str_sub(string = x_index, start = 2L) |>
            as.integer(),
          expected = (n_assay * n_panel) + 2L
        )

      }
    )


  }
)

test_that(
  "read_npx_wide_split_col - Quantified - works",
  {
    ## T48 ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df----

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

        # write something in the file
        writeLines("foo", wide_excel)

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs
        expect_no_condition(
          object = x_index <- read_npx_wide_split_col(df = df,
                                                      file = wide_excel)
        )

        # check that output exists
        expect_true(object = exists("x_index"))

        # check that df_top works
        expect_identical(
          object = stringr::str_sub(string = x_index, start = 2L) |>
            as.integer(),
          expected = (n_assay * n_panel) + 2L
        )

      }
    )

    ## T48 shuffled ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df----

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

        # write something in the file
        writeLines("foo", wide_excel)

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs
        expect_no_condition(
          object = x_index <- read_npx_wide_split_col(df = df,
                                                      file = wide_excel)
        )

        # check that output exists
        expect_true(object = exists("x_index"))

        # check that df_top works
        expect_identical(
          object = stringr::str_sub(string = x_index, start = 2L) |>
            as.integer(),
          expected = (n_assay * n_panel) + 2L + (3L * n_panel)
        )

      }
    )

    ## T48 multi-plate shuffled ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df----

        o_platform <- "Target 48"
        n_panel <- 4L
        n_assay <- 45L
        data_t <- "Quantified"

        df <- npx_wide_top(olink_platform = o_platform,
                           n_panels = n_panel,
                           n_assays = n_assay,
                           data_type = data_t,
                           show_int_ctrl = TRUE,
                           loc_int_ctrl = "V3",
                           shuffle_assays = TRUE)

        # write something in the file
        writeLines("foo", wide_excel)

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs
        expect_no_condition(
          object = x_index <- read_npx_wide_split_col(df = df,
                                                      file = wide_excel)
        )

        # check that output exists
        expect_true(object = exists("x_index"))

        # check that df_top works
        expect_identical(
          object = stringr::str_sub(string = x_index, start = 2L) |>
            as.integer(),
          expected = (n_assay * n_panel) + 2L + (3L * n_panel)
        )

      }
    )

    ## T48 multi-plate shuffled fewer assays ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df----

        o_platform <- "Target 48"
        n_panel <- 4L
        n_assay <- 45L
        data_t <- "Quantified"

        random_assays <- sample(
          x = paste0("V",
                     tail(x = seq_len((n_panel * n_assay) + 1L), n = -1L)),
          size = 100L,
          replace = FALSE
        )

        df <- npx_wide_top(olink_platform = o_platform,
                           n_panels = n_panel,
                           n_assays = n_assay,
                           data_type = data_t,
                           show_int_ctrl = TRUE,
                           loc_int_ctrl = "V3",
                           shuffle_assays = TRUE) |>
          dplyr::select(
            -dplyr::all_of(random_assays)
          )

        # write something in the file
        writeLines("foo", wide_excel)

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs
        expect_no_condition(
          object = x_index <- read_npx_wide_split_col(df = df,
                                                      file = wide_excel)
        )

        # check that output exists
        expect_true(object = exists("x_index"))

        # check that df_top works
        expect_identical(
          object = stringr::str_sub(string = x_index, start = 2L) |>
            as.integer(),
          expected = ((n_assay * n_panel) + 2L) + (3L * n_panel)
        )

      }
    )

    ## T48 multi-plate shuffled fewer assays int ctrl in V2 ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df----

        o_platform <- "Target 48"
        n_panel <- 4L
        n_assay <- 45L
        data_t <- "Quantified"

        random_assays <- sample(
          x = paste0("V",
                     tail(x = seq_len((n_panel * n_assay) + 1L), n = -1L)),
          size = 100L,
          replace = FALSE
        )

        df <- npx_wide_top(olink_platform = o_platform,
                           n_panels = n_panel,
                           n_assays = n_assay,
                           data_type = data_t,
                           show_int_ctrl = TRUE,
                           loc_int_ctrl = "V2",
                           shuffle_assays = TRUE) |>
          dplyr::select(
            -dplyr::all_of(random_assays)
          )

        # write something in the file
        writeLines("foo", wide_excel)

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs
        expect_no_condition(
          object = x_index <- read_npx_wide_split_col(df = df,
                                                      file = wide_excel)
        )

        # check that output exists
        expect_true(object = exists("x_index"))

        # check that df_top works
        expect_identical(
          object = stringr::str_sub(string = x_index, start = 2L) |>
            as.integer(),
          expected = ((n_assay * n_panel) + 2L) + (3L * n_panel)
        )

      }
    )

    ## Flex/Focus ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df----

        o_platform <- "Focus"
        n_panel <- 1L
        n_assay <- 21L
        data_t <- "Quantified"

        df <- npx_wide_top(olink_platform = o_platform,
                           n_panels = n_panel,
                           n_assays = n_assay,
                           data_type = data_t,
                           show_int_ctrl = TRUE,
                           loc_int_ctrl = "V3",
                           shuffle_assays = FALSE)

        # write something in the file
        writeLines("foo", wide_excel)

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs
        expect_no_condition(
          object = x_index <- read_npx_wide_split_col(df = df,
                                                      file = wide_excel)
        )

        # check that output exists
        expect_true(object = exists("x_index"))

        # check that df_top works
        expect_identical(
          object = stringr::str_sub(string = x_index, start = 2L) |>
            as.integer(),
          expected = (n_assay * n_panel) + 2L
        )

      }
    )

    ## Flex/Focus shuffled ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df----

        o_platform <- "Focus"
        n_panel <- 1L
        n_assay <- 21L
        data_t <- "Quantified"

        df <- npx_wide_top(olink_platform = o_platform,
                           n_panels = n_panel,
                           n_assays = n_assay,
                           data_type = data_t,
                           show_int_ctrl = TRUE,
                           loc_int_ctrl = "V3",
                           shuffle_assays = TRUE)

        # write something in the file
        writeLines("foo", wide_excel)

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs
        expect_no_condition(
          object = x_index <- read_npx_wide_split_col(df = df,
                                                      file = wide_excel)
        )

        # check that output exists
        expect_true(object = exists("x_index"))

        # check that df_top works
        expect_identical(
          object = stringr::str_sub(string = x_index, start = 2L) |>
            as.integer(),
          expected = (n_assay * n_panel) + 2L + (3L * n_panel)
        )

      }
    )

    ## Flex/Focus multi-plate shuffled ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df----

        o_platform <- "Focus"
        n_panel <- 4L
        n_assay <- 21L
        data_t <- "Quantified"

        df <- npx_wide_top(olink_platform = o_platform,
                           n_panels = n_panel,
                           n_assays = n_assay,
                           data_type = data_t,
                           show_int_ctrl = TRUE,
                           loc_int_ctrl = "V3",
                           shuffle_assays = TRUE)

        # write something in the file
        writeLines("foo", wide_excel)

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs
        expect_no_condition(
          object = x_index <- read_npx_wide_split_col(df = df,
                                                      file = wide_excel)
        )

        # check that output exists
        expect_true(object = exists("x_index"))

        # check that df_top works
        expect_identical(
          object = stringr::str_sub(string = x_index, start = 2L) |>
            as.integer(),
          expected = (n_assay * n_panel) + 2L + (3L * n_panel)
        )

      }
    )

    ## Flex/Focus multi-plate shuffled fewer assays ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df----

        o_platform <- "Focus"
        n_panel <- 4L
        n_assay <- 21L
        data_t <- "Quantified"

        random_assays <- sample(
          x = paste0("V",
                     tail(x = seq_len((n_panel * n_assay) + 1L), n = -1L)),
          size = 50L,
          replace = FALSE
        )

        df <- npx_wide_top(olink_platform = o_platform,
                           n_panels = n_panel,
                           n_assays = n_assay,
                           data_type = data_t,
                           show_int_ctrl = TRUE,
                           loc_int_ctrl = "V3",
                           shuffle_assays = TRUE) |>
          dplyr::select(
            -dplyr::all_of(random_assays)
          )

        # write something in the file
        writeLines("foo", wide_excel)

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs
        expect_no_condition(
          object = x_index <- read_npx_wide_split_col(df = df,
                                                      file = wide_excel)
        )

        # check that output exists
        expect_true(object = exists("x_index"))

        # check that df_top works
        expect_identical(
          object = stringr::str_sub(string = x_index, start = 2L) |>
            as.integer(),
          expected = ((n_assay * n_panel) + 2L) + (3L * n_panel)
        )

      }
    )

    ## Flex/Focus multi-plate shuffled fewer assays int ctrl in V2 ----

    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df----

        o_platform <- "Focus"
        n_panel <- 4L
        n_assay <- 21L
        data_t <- "Quantified"

        random_assays <- sample(
          x = paste0("V",
                     tail(x = seq_len((n_panel * n_assay) + 1L), n = -1L)),
          size = 50L,
          replace = FALSE
        )

        df <- npx_wide_top(olink_platform = o_platform,
                           n_panels = n_panel,
                           n_assays = n_assay,
                           data_type = data_t,
                           show_int_ctrl = TRUE,
                           loc_int_ctrl = "V2",
                           shuffle_assays = TRUE) |>
          dplyr::select(
            -dplyr::all_of(random_assays)
          )

        # write something in the file
        writeLines("foo", wide_excel)

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs
        expect_no_condition(
          object = x_index <- read_npx_wide_split_col(df = df,
                                                      file = wide_excel)
        )

        # check that output exists
        expect_true(object = exists("x_index"))

        # check that df_top works
        expect_identical(
          object = stringr::str_sub(string = x_index, start = 2L) |>
            as.integer(),
          expected = ((n_assay * n_panel) + 2L) + (3L * n_panel)
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_split_col - no Assay=\"Plate ID\" columns",
  {
    withr::with_tempfile(
      new = "wide_excel",
      pattern = "test-excel-wide",
      fileext = ".xlsx",
      code = {

        # random df ----

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
          dplyr::select(
            -dplyr::all_of("V47")
          )

        # write something in the file
        writeLines("foo", wide_excel)

        # check that file exists
        expect_true(object = file.exists(wide_excel))

        # check that function runs
        expect_error(
          object = read_npx_wide_split_col(df = df,
                                           file = wide_excel),
          regexp = "Unexpected format of the top metadata in file"
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

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_no_condition(
          object = l_obj <- read_npx_wide_top_split(df = df,
                                                    file = wide_excel,
                                                    data_type = data_t,
                                                    olink_platform = o_platform)
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

        expect_true(object = is.null(l_obj$df_qc_dev))

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

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_no_condition(
          object = l_obj <- read_npx_wide_top_split(df = df,
                                                    file = wide_excel,
                                                    data_type = data_t,
                                                    olink_platform = o_platform)
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

        expect_true(object = is.null(l_obj$df_qc_dev))

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

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_no_condition(
          object = l_obj <- read_npx_wide_top_split(df = df,
                                                    file = wide_excel,
                                                    data_type = data_t,
                                                    olink_platform = o_platform)
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

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_no_condition(
          object = l_obj <- read_npx_wide_top_split(df = df,
                                                    file = wide_excel,
                                                    data_type = data_t,
                                                    olink_platform = o_platform)
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

        expect_true(object = is.null(l_obj$df_qc_dev))

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

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_no_condition(
          object = l_obj <- read_npx_wide_top_split(df = df,
                                                    file = wide_excel,
                                                    data_type = data_t,
                                                    olink_platform = o_platform)
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

        expect_true(object = is.null(l_obj$df_qc_dev))

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

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_no_condition(
          object = l_obj <- read_npx_wide_top_split(df = df,
                                                    file = wide_excel,
                                                    data_type = data_t,
                                                    olink_platform = o_platform)
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

        expect_true(object = is.null(l_obj$df_qc_dev))

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

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_no_condition(
          object = l_obj <- read_npx_wide_top_split(df = df,
                                                    file = wide_excel,
                                                    data_type = data_t,
                                                    olink_platform = o_platform)
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

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_no_condition(
          object = l_obj <- read_npx_wide_top_split(df = df,
                                                    file = wide_excel,
                                                    data_type = data_t,
                                                    olink_platform = o_platform)
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

        expect_true(object = is.null(l_obj$df_qc_dev))

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

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_no_condition(
          object = l_obj <- read_npx_wide_top_split(df = df,
                                                    file = wide_excel,
                                                    data_type = data_t,
                                                    olink_platform = o_platform)
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

        expect_true(object = is.null(l_obj$df_qc_dev))

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

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_no_condition(
          object = l_obj <- read_npx_wide_top_split(df = df,
                                                    file = wide_excel,
                                                    data_type = data_t,
                                                    olink_platform = o_platform)
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

        expect_true(object = is.null(l_obj$df_qc_dev))

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

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_no_condition(
          object = l_obj <- read_npx_wide_top_split(df = df,
                                                    file = wide_excel,
                                                    data_type = data_t,
                                                    olink_platform = o_platform)
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

        expect_true(object = is.null(l_obj$df_qc_dev))

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

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_no_condition(
          object = l_obj <- read_npx_wide_top_split(df = df,
                                                    file = wide_excel,
                                                    data_type = data_t,
                                                    olink_platform = o_platform)
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

        expect_true(object = is.null(l_obj$df_qc_dev))

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

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_no_condition(
          object = l_obj <- read_npx_wide_top_split(df = df,
                                                    file = wide_excel,
                                                    data_type = data_t,
                                                    olink_platform = o_platform)
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

        expect_true(object = is.null(l_obj$df_qc_dev))

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

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_no_condition(
          object = l_obj <- read_npx_wide_top_split(df = df,
                                                    file = wide_excel,
                                                    data_type = data_t,
                                                    olink_platform = o_platform)
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

        expect_true(object = is.null(l_obj$df_qc_dev))

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

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_no_condition(
          object = l_obj <- read_npx_wide_top_split(df = df,
                                                    file = wide_excel,
                                                    data_type = data_t,
                                                    olink_platform = o_platform)
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

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_no_condition(
          object = l_obj <- read_npx_wide_top_split(df = df,
                                                    file = wide_excel,
                                                    data_type = data_t,
                                                    olink_platform = o_platform)
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

        expect_true(object = is.null(l_obj$df_qc_dev))

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

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_no_condition(
          object = l_obj <- read_npx_wide_top_split(df = df,
                                                    file = wide_excel,
                                                    data_type = data_t,
                                                    olink_platform = o_platform)
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

        expect_true(object = is.null(l_obj$df_qc_dev))

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

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_no_condition(
          object = l_obj <- read_npx_wide_top_split(df = df,
                                                    file = wide_excel,
                                                    data_type = data_t,
                                                    olink_platform = o_platform)
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

        expect_true(object = is.null(l_obj$df_qc_dev))

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

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_no_condition(
          object = l_obj <- read_npx_wide_top_split(df = df,
                                                    file = wide_excel,
                                                    data_type = data_t,
                                                    olink_platform = o_platform)
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

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_no_condition(
          object = l_obj <- read_npx_wide_top_split(df = df,
                                                    file = wide_excel,
                                                    data_type = data_t,
                                                    olink_platform = o_platform)
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

        expect_true(object = is.null(l_obj$df_qc_dev))

      }
    )

  }
)

test_that(
  "read_npx_wide_top_split - unrecognizable tags",
  {

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

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_error(
          object = read_npx_wide_top_split(df = df,
                                           file = wide_excel,
                                           data_type = data_t,
                                           olink_platform = o_platform),
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

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_error(
          object = read_npx_wide_top_split(df = df,
                                           file = wide_excel,
                                           data_type = data_t,
                                           olink_platform = o_platform),
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

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_error(
          object = read_npx_wide_top_split(df = df,
                                           file = wide_excel,
                                           data_type = data_t,
                                           olink_platform = o_platform),
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

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_error(
          object = read_npx_wide_top_split(df = df,
                                           file = wide_excel,
                                           data_type = data_t,
                                           olink_platform = o_platform),
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

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_error(
          object = read_npx_wide_top_split(df = df,
                                           file = wide_excel,
                                           data_type = data_t,
                                           olink_platform = o_platform),
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

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_error(
          object = read_npx_wide_top_split(df = df,
                                           file = wide_excel,
                                           data_type = data_t,
                                           olink_platform = o_platform),
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

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_error(
          object = read_npx_wide_top_split(df = df,
                                           file = wide_excel,
                                           data_type = data_t,
                                           olink_platform = o_platform),
          regexp = "Detected 156 assays in 2 panels in file"
        )
      }
    )


  }
)

test_that(
  "read_npx_wide_top_split - QC_Warning on Ct data",
  {
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

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_error(
          object = read_npx_wide_top_split(df = df,
                                           file = wide_excel,
                                           data_type = "Ct",
                                           olink_platform = o_platform),
          regexp = "in the right-hand side of the top matrix in file"
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

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_no_condition(
          object = l_obj <- read_npx_wide_top_split(df = df,
                                                    file = wide_excel,
                                                    data_type = data_t,
                                                    olink_platform = o_platform)
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

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_no_condition(
          object = l_obj <- read_npx_wide_top_split(df = df,
                                                    file = wide_excel,
                                                    data_type = data_t,
                                                    olink_platform = o_platform)
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

# Test read_npx_wide_bottom_t ----

test_that(
  "read_npx_wide_top_split - T48 - works single panel file",
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
        col_s <- paste0("V", n_assay * n_panel + 2)

        df <- npx_wide_bottom(n_plates = n_plate,
                              n_panels = n_panel,
                              n_assays = n_assay,
                              data_type = data_t)

        a_cols <- paste0("V", 2L:((n_assay * n_panel) + 1L))

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_no_condition(
          object = df_b <- read_npx_wide_bottom_t(df = df,
                                                  file = wide_excel,
                                                  data_type = data_t,
                                                  col_split = col_s,
                                                  assay_cols = a_cols)
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
        col_s <- paste0("V", n_assay * n_panel + 2)

        df <- npx_wide_bottom(n_plates = n_plate,
                              n_panels = n_panel,
                              n_assays = n_assay,
                              data_type = data_t)

        a_cols <- paste0("V", 2L:((n_assay * n_panel) + 1L))

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
            expect_no_message(
              object = df_b <- read_npx_wide_bottom_t(df = df,
                                                      file = wide_excel,
                                                      data_type = data_t,
                                                      col_split = col_s,
                                                      assay_cols = a_cols)
            )
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
  "read_npx_wide_top_split - T48 - works multi panel and multi plate file",
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
        col_s <- paste0("V", n_assay * n_panel + 2)

        df <- npx_wide_bottom(n_plates = n_plate,
                              n_panels = n_panel,
                              n_assays = n_assay,
                              data_type = data_t)

        a_cols <- paste0("V", 2L:((n_assay * n_panel) + 1L))

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_no_condition(
          object = df_b <- read_npx_wide_bottom_t(df = df,
                                                  file = wide_excel,
                                                  data_type = data_t,
                                                  col_split = col_s,
                                                  assay_cols = a_cols)
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
        col_s <- paste0("V", n_assay * n_panel + 2)

        df <- npx_wide_bottom(n_plates = n_plate,
                              n_panels = n_panel,
                              n_assays = n_assay,
                              data_type = data_t)

        a_cols <- paste0("V", 2L:((n_assay * n_panel) + 1L))

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
            expect_no_message(
              object = df_b <- read_npx_wide_bottom_t(df = df,
                                                      file = wide_excel,
                                                      data_type = data_t,
                                                      col_split = col_s,
                                                      assay_cols = a_cols)
            )
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
  "read_npx_wide_top_split - T96 - works single panel file",
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
        n_assay <- 92L
        data_t <- "NPX"
        col_s <- paste0("V", n_assay * n_panel + 2)

        df <- npx_wide_bottom(n_plates = n_plate,
                              n_panels = n_panel,
                              n_assays = n_assay,
                              data_type = data_t)

        a_cols <- paste0("V", 2L:((n_assay * n_panel) + 1L))

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_no_condition(
          object = df_b <- read_npx_wide_bottom_t(df = df,
                                                  file = wide_excel,
                                                  data_type = data_t,
                                                  col_split = col_s,
                                                  assay_cols = a_cols)
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
  "read_npx_wide_top_split - T96 - works multi panel and multi plate file",
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
        n_assay <- 92L
        data_t <- "NPX"
        col_s <- paste0("V", n_assay * n_panel + 2)

        df <- npx_wide_bottom(n_plates = n_plate,
                              n_panels = n_panel,
                              n_assays = n_assay,
                              data_type = data_t)

        a_cols <- paste0("V", 2L:((n_assay * n_panel) + 1L))

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_no_condition(
          object = df_b <- read_npx_wide_bottom_t(df = df,
                                                  file = wide_excel,
                                                  data_type = data_t,
                                                  col_split = col_s,
                                                  assay_cols = a_cols)
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
  "read_npx_wide_top_split - Flex/Focus - works single panel file",
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
        col_s <- paste0("V", n_assay * n_panel + 2)

        df <- npx_wide_bottom(n_plates = n_plate,
                              n_panels = n_panel,
                              n_assays = n_assay,
                              data_type = data_t)

        a_cols <- paste0("V", 2L:((n_assay * n_panel) + 1L))

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_no_condition(
          object = df_b <- read_npx_wide_bottom_t(df = df,
                                                  file = wide_excel,
                                                  data_type = data_t,
                                                  col_split = col_s,
                                                  assay_cols = a_cols)
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
        col_s <- paste0("V", n_assay * n_panel + 2)

        df <- npx_wide_bottom(n_plates = n_plate,
                              n_panels = n_panel,
                              n_assays = n_assay,
                              data_type = data_t)

        a_cols <- paste0("V", 2L:((n_assay * n_panel) + 1L))

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
            expect_no_message(
              object = df_b <- read_npx_wide_bottom_t(df = df,
                                                      file = wide_excel,
                                                      data_type = data_t,
                                                      col_split = col_s,
                                                      assay_cols = a_cols)
            )
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
  "read_npx_wide_top_split - Flex/Focus - works multi panel & multi plate file",
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
        col_s <- paste0("V", n_assay * n_panel + 2)

        df <- npx_wide_bottom(n_plates = n_plate,
                              n_panels = n_panel,
                              n_assays = n_assay,
                              data_type = data_t)

        a_cols <- paste0("V", 2L:((n_assay * n_panel) + 1L))

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_no_condition(
          object = df_b <- read_npx_wide_bottom_t(df = df,
                                                  file = wide_excel,
                                                  data_type = data_t,
                                                  col_split = col_s,
                                                  assay_cols = a_cols)
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
        col_s <- paste0("V", n_assay * n_panel + 2)

        df <- npx_wide_bottom(n_plates = n_plate,
                              n_panels = n_panel,
                              n_assays = n_assay,
                              data_type = data_t)

        a_cols <- paste0("V", 2L:((n_assay * n_panel) + 1L))

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
            expect_no_message(
              object = df_b <- read_npx_wide_bottom_t(df = df,
                                                      file = wide_excel,
                                                      data_type = data_t,
                                                      col_split = col_s,
                                                      assay_cols = a_cols)
            )
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
  "read_npx_wide_top_split - T48 - error for Ct",
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
        col_s <- paste0("V", n_assay * n_panel + 2)

        df <- npx_wide_bottom(n_plates = n_plate,
                              n_panels = n_panel,
                              n_assays = n_assay,
                              data_type = data_t)

        a_cols <- paste0("V", 2L:((n_assay * n_panel) + 1L))

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_error(
          object = read_npx_wide_bottom_t(df = df,
                                          file = wide_excel,
                                          data_type = "Ct",
                                          col_split = col_s,
                                          assay_cols = a_cols),
          regexp = "contains a bottom matrix. Files with"
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_top_split - T48 - error for unexpected values in V1",
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
        col_s <- paste0("V", n_assay * n_panel + 2)

        df <- npx_wide_bottom(n_plates = n_plate,
                              n_panels = n_panel,
                              n_assays = n_assay,
                              data_type = data_t) |>
          # modify df with wrong V1
          dplyr::mutate(
            V1 = dplyr::if_else(.data[["V1"]] == "LOD", "LOD2", .data[["V1"]])
          )

        a_cols <- paste0("V", 2L:((n_assay * n_panel) + 1L))

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_error(
          object = read_npx_wide_bottom_t(df = df,
                                          file = wide_excel,
                                          data_type = data_t,
                                          col_split = col_s,
                                          assay_cols = a_cols),
          regexp = "Column 1 of the bottom matrix with assay metadata in file"
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
        col_s <- paste0("V", n_assay * n_panel + 2)

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

        a_cols <- paste0("V", 2L:((n_assay * n_panel) + 1L))

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_error(
          object = read_npx_wide_bottom_t(df = df,
                                          file = wide_excel,
                                          data_type = data_t,
                                          col_split = col_s,
                                          assay_cols = a_cols),
          regexp = "Column 1 of the bottom matrix with assay metadata in file"
        )

      }
    )

  }
)

test_that(
  "read_npx_wide_top_split - T48 - works with additonal all NA col",
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
        col_s <- paste0("V", n_assay * n_panel + 2L)

        df <- npx_wide_bottom(n_plates = n_plate,
                              n_panels = n_panel,
                              n_assays = n_assay,
                              data_type = data_t)

        # add new column with all NA
        df <- df |>
          dplyr::mutate(
            {{col_s}} := rep(x = NA_character_, times = nrow(df))
          )

        a_cols <- paste0("V", 2L:((n_assay * n_panel) + 1L))

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_no_condition(
          object = df_b <- read_npx_wide_bottom_t(df = df,
                                                  file = wide_excel,
                                                  data_type = data_t,
                                                  col_split = col_s,
                                                  assay_cols = a_cols)
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
  "read_npx_wide_top_split - T48 - incorrect number of plates x QC data",
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

        # write something in the file
        writeLines("foo", wide_excel)

        # run function ----

        expect_error(
          object = read_npx_wide_bottom_t(df = df,
                                          file = wide_excel,
                                          data_type = data_t,
                                          col_split = col_s,
                                          assay_cols = a_cols),
          regexp = "Column 1 of the bottom matrix contains uneven rows of plate"
        )

      }
    )

  }
)
