#' Read wide Olink files with NPX, Ct or Quantified data.
#'
#' @author Klev Diamanti
#'
#' @param file The input excel file.
#' @param data_type The quantification in which the data comes in. Expecting one
#' of NPX, Quantified or Ct.
#' @param olink_platform The Olink platform used to generate the input file.
#' Expecting "Target 96", "Target 48", "Flex" or "Focus".
#'
#' @return A tibble of the wide Olink file in long format.
#'
read_npx_wide <- function(file,
                          data_type,
                          olink_platform) {

  # initial checks ----

  check_file_exists(file = file,
                    error = TRUE)

  check_olink_data_type(x = data_type,
                        broader_platform = "qPCR")

  check_olink_platform(x = olink_platform,
                       broader_platform = "qPCR")

  # get expected format specifications ----

  format_spec <- olink_wide_excel_spec |>
    dplyr::filter(
      .data[["data_type"]] == .env[["data_type"]]
    )

  # split the file into sub-data frames ----

  df_split_row <- read_npx_wide_split_row(
    file = file,
    data_type = data_type,
    format_spec = format_spec
  )

  # header matrix NPXS version ----

  npxs_v <- read_npx_wide_npxs_version(df = df_split_row$df_head)

  # top list of df to long ----

  df_top_list <- read_npx_wide_top(
    df = df_split_row$df_top,
    file = file,
    olink_platform = olink_platform,
    format_spec = format_spec
  )

  col_names <- sapply(df_top_list, function(x) x$col_index)

  # middle list of df to long ----

  df_middle_list <- read_npx_wide_middle(
    df = df_split_row$df_mid,
    file = file,
    data_type = data_type,
    col_names = col_names
  )

  # combine top and middle matrices ----

  df_long <- red_npx_wide_top_mid_long(df_top_list = df_top_list,
                                       df_middle_list = df_middle_list,
                                       data_type = data_type,
                                       format_spec = format_spec)

  # add bottom df to long ----

  if (format_spec$has_qc_data == TRUE) {

    df_bottom <- read_npx_wide_bottom(
      df = df_split_row$df_bottom,
      file = file,
      olink_platform = olink_platform,
      data_type = data_type,
      col_names = col_names,
      format_spec = format_spec,
      df_plate_panel = df_long |>
        dplyr::select(
          dplyr::all_of(c("col_index", "PlateID", "Panel"))
        ) |>
        dplyr::distinct()
    )

    # add columns from bottom matrix
    if ("PlateID" %in% colnames(df_bottom$df_bottom_oid)) {

      df_long <- df_long |>
        dplyr::left_join(
          dplyr::bind_rows(df_bottom),
          by = c("col_index", "PlateID"),
          relationship = "many-to-one"
        )

    } else {

      df_long <- df_long |>
        dplyr::left_join(
          dplyr::bind_rows(df_bottom),
          by = "col_index",
          relationship = "many-to-one"
        )

    }

  }

  # modify output df ----
  df_long <- df_long |>
    # add Panel_Version
    dplyr::mutate(
      Panel_Version = strsplit(x = .data[["Panel"]],
                               split = "(",
                               fixed = TRUE) |>
        lapply(utils::tail, 1L) |>
        unlist() |>
        (\(x) sub(pattern = ")",
                  replacement = "",
                  x = x,
                  fixed = TRUE))()
    ) |>
    # modify Panel
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
    # remove col_index
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

  return(df_long)
}

#' Help function that extracts the version of the NPX Signature software from
#' Olink wide excel files.
#'
#' @param df The 2x2 header data frame from an Olink wide excel file.
#'
#' @return A scalar character vector with the version of the NPXS software from
#' cell B1 of the wide Olink file.
#'
read_npx_wide_npxs_version <- function(df) {

  # check input ----

  check_is_data_frame(df = df,
                      error = TRUE)

  # check necessary columns ----

  check_columns(df = df, col_list = list("V1", "V2"))

  # extract NPXS sw version ----

  npxs_sw_v <- df |>
    dplyr::slice_head(
      n = 1L
    ) |>
    dplyr::pull(
      .data[["V2"]]
    ) |>
    as.character()

  # return ----

  return(npxs_sw_v)

}

#' Help function the uses rows with all columns NA to split the wide Olink data
#' file into 2 or 3 data sets.
#'
#' @description
#' Wide Olink files contains 1 or 2 rows with all columns NA as a break between
#' subsections of the data. This function takes advantage of that feature and
#' splits the excel file into 2 or 3 data sets each of which will be used
#' downstream to create a long data frame.
#'
#' The function captures the rows with all columns NA and computes the row
#' indexes to split the dataset from the excel file into sub-data sets.
#'
#' @author Klev Diamanti
#'
#' @param file The input excel file.
#' @param data_type The quantification in which the data comes in. Expecting one
#' of NPX, Quantified or Ct.
#' @param format_spec A one-row data frame filtered from olink_wide_excel_spec
#' with the Olink wide excel file specifications.
#'
#' @return A named list of 2 or 3 tibbles.
#'
read_npx_wide_split_row <- function(file,
                                    data_type,
                                    format_spec) {

  # initial checks ----
  check_file_exists(file = file,
                    error = TRUE)

  check_olink_data_type(x = data_type,
                        broader_platform = "qPCR")

  check_is_data_frame(df = format_spec,
                      error = TRUE)

  # read data ----

  # read the entire wide excel file
  # we want to identify the first row with all values NA
  df <-  readxl::read_excel(path = file,
                            col_names = FALSE,
                            .name_repair = "minimal")
  colnames(df) <- paste0("V", seq_len(ncol(df)))

  # detect rows with all NA columns ----

  # Use the rows full of NAs in the excel file to compute the number of rows
  # that contain data about assays.
  na_row_index <- df |>
    # total number of columns
    dplyr::mutate(
      total_col = {
        dplyr::pick(dplyr::everything()) |>
          ncol()
      }
    ) |>
    # count number of NA in each row
    dplyr::mutate(
      row_number = dplyr::row_number(),
      num_na = {
        dplyr::pick(dplyr::everything()) |>
          is.na() |>
          rowSums()
      }
    ) |>
    # filter rows with all cols NA
    dplyr::filter(
      .data[["num_na"]] == .data[["total_col"]]
    ) |>
    # extract only the row_number as this is the index we are interested in
    dplyr::pull(
      .data[["row_number"]]
    )

  # check that row indexes are correct ----

  # check that there are 1 or 2 rows with all NA
  if (length(na_row_index) != format_spec$n_na_rows) {

    cli::cli_abort(
      message = c(
        "x" = "We identified
        {ifelse(identical(na_row_index, integer(0L)), 0L, length(na_row_index))}
        rows with all columns `NA` in file {.file {file}}, while we expected
        {format_spec$n_na_rows}!",
        "i" = "Has the excel file been modified manually?"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

  # check that rows are not consecutive
  if (length(na_row_index) == 2L
      && (na_row_index[2L] - na_row_index[1L] < 2)) {

    cli::cli_abort(
      message = c(
        "x" = "Consecutive rows with all columns NA.",
        "i" = "Has the excel file been modified manually?"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

  # split df from start and stop row indexes ----

  # mark starting and ending row indexes of each sub-matrix
  df_split_index <- dplyr::tibble(
    start_row = c(
      1L, # header
      3L, # skip first two rows
      na_row_index + 1L # all NA rows +1
    ),
    end_row = c(
      2L, # header
      na_row_index - 1L, # all NA rows -1
      nrow(df) # last row of file
    )
  )

  # extract sub-matrices
  list_df_split <- lapply(
    seq_len(nrow(df_split_index)),
    function(i) {
      df_s_i <- df_split_index |>
        dplyr::slice(
          i
        )

      df_split <- df |>
        dplyr::slice(
          df_s_i$start_row:df_s_i$end_row
        )

      return(df_split)
    }
  )

  # output is a list of 2 or 3 dataframes, depending on data_type
  # name each data frame
  if (length(na_row_index) == 1L) {
    names(list_df_split) <- c("df_head", "df_top", "df_mid")
  } else {
    names(list_df_split) <- c("df_head", "df_top", "df_mid", "df_bottom")
  }

  # return list of data frames ----

  return(list_df_split)

}

#' Help function to determine the number of rows with assay information in Olink
#' excel wide format files.
#'
#' @author Klev Diamanti
#'
#' @param df The top data frame from a split Olink wide excel file.
#' @param file The input excel file.
#' @param format_spec A one-row data frame filtered from olink_wide_excel_spec
#' with the Olink wide excel file specifications.
#'
#' @return An error inconsistency is spotted
#'
read_npx_wide_check_top <- function(df,
                                    file,
                                    format_spec) {

  # initial checks ----

  check_is_tibble(df = df,
                  error = TRUE)

  check_file_exists(file = file,
                    error = TRUE)

  check_is_data_frame(df = format_spec,
                      error = TRUE)

  # checks ----

  ## column 1 contains the expected values ----

  # check that df contains "V1"
  check_columns(df = df, col_list = list("V1"))

  # rows containing metadata about assays and panels based on the quantification
  # method in the top dataset.
  top_mat_v1 <- format_spec |>
    dplyr::pull(
      .data[["top_matrix_v1"]]
    ) |>
    unlist()

  if (!identical(dplyr::pull(df, .data[["V1"]]), top_mat_v1)) {

    top_v1_miss <- top_mat_v1[!(top_mat_v1 %in% df$V1)] # nolint object_usage_linter

    cli::cli_abort(
      message = c(
        "x" = "Column 1 of the top matrix with assay data in file
        {.file {file}} does not contain: {top_v1_miss}",
        "i" = "Has the excel file been modified manually?"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

  ## row 2 contains Plate ID (and QC Warning) ----

  # rows containing information on plate id and qc warning
  top_mat_assay_labels <- format_spec |>
    dplyr::pull(
      .data[["top_matrix_assay_labels"]]
    ) |>
    unlist()

  if (!all(top_mat_assay_labels %in% df[2L, ])) {

    top_mat_assay_miss <- top_mat_assay_labels[!(top_mat_assay_labels %in% df[2L, ])] # nolint object_usage_linter

    cli::cli_abort(
      message = c(
        "x" = "Row 2 of the top matrix with assay data in file {.file {file}}
        does not contain: {top_mat_assay_miss}",
        "i" = "Has the excel file been modified manually?"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

  ## row 2 contains the correct number of internal controls ----

  # list of expected names of internal control assays
  int_ctrl_list <- format_spec |>
    dplyr::pull(
      .data[["top_matrix_assay_int_ctrl"]]
    ) |>
    unlist()

  panel_list <- df[1L, 2L:ncol(df)] |>
    as.character() |>
    unique()

  # expected combos of int ctrl and panels
  panel_int_ctrl_exp <- expand.grid(
    "panel" = panel_list,
    "int_ctrl" = int_ctrl_list
  ) |>
    dplyr::as_tibble() |>
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        ~ as.character(.x)
      )
    ) |>
    dplyr::mutate(
      int_ctrl_strip = strsplit(x = .data[["int_ctrl"]],
                                split = " ",
                                fixed = TRUE) |>
        lapply(utils::head, 2L) |>
        lapply(paste, collapse = " ") |>
        unlist()
    )

  # check for internal control assays
  int_ctrl_df_col <- colnames(df)[which(df[2L, ] %in% int_ctrl_list)]

  # run only if there are internal controls
  if (length(int_ctrl_df_col) > 0L) {

    # data frame of internal controls present
    int_ctrl_df <- df |>
      dplyr::select(
        dplyr::all_of(int_ctrl_df_col)
      ) |>
      dplyr::slice(
        1L:2L
      ) |>
      t() |>
      `colnames<-`(c("panel", "int_ctrl")) |>
      dplyr::as_tibble(
        .name_repair = "minimal"
      ) |>
      dplyr::mutate(
        in_df = TRUE
      )

    # intersect real with expected datasets
    int_ctrl_df_present <- panel_int_ctrl_exp |>
      dplyr::inner_join(
        int_ctrl_df,
        by = c("panel", "int_ctrl")
      )

    # missing internal controls
    int_ctrl_df_missing <- panel_int_ctrl_exp |>
      dplyr::anti_join(
        int_ctrl_df,
        by = c("panel", "int_ctrl")
      ) |>
      dplyr::filter(
        .data[["int_ctrl"]] == .data[["int_ctrl_strip"]]
      ) |>
      dplyr::anti_join(
        int_ctrl_df_present,
        by = c("panel", "int_ctrl_strip")
      )

    if (nrow(int_ctrl_df_missing) > 0L) {

      cli::cli_abort(
        message = c(
          "x" = "Panel(s) {unique(int_ctrl_df_missing$panel)} {?is/are} missing
          one or more of the internal control assays
          {unique(int_ctrl_df_missing$int_ctrl)} from row 2 of the top matrix
          with assay data in file {.file {file}}!",
          "i" = "Has the excel file been modified manually?"
        ),
        call = rlang::caller_env(),
        wrap = FALSE
      )

    }

  }

  ## row 3 contains the correct number of deviation from internal controls ----

  # list of expected tags of deviations from internal controls
  dev_int_ctrl_tag <- format_spec |>
    dplyr::pull(
      .data[["top_matrix_assay_dev_int_ctrl"]]
    ) |>
    unlist()

  if (length(dev_int_ctrl_tag) > 0L) {

    # names of deviation from internal control assays
    dev_int_ctrl_list <- format_spec |>
      dplyr::pull(
        .data[["top_matrix_uniprot_dev_int_ctrl"]]
      ) |>
      unlist()

    panel_list <- df[1L, 2L:ncol(df)] |>
      as.character() |>
      unique()

    # expected combos of int ctrl and panels
    panel_dev_int_ctrl_exp <- expand.grid(
      "panel" = panel_list,
      "dev_int_ctrl" = dev_int_ctrl_list
    ) |>
      dplyr::as_tibble() |>
      dplyr::mutate(
        dplyr::across(
          dplyr::everything(),
          ~ as.character(.x)
        )
      ) |>
      dplyr::mutate(
        dev_int_ctrl_strip = strsplit(x = .data[["dev_int_ctrl"]],
                                      split = " ",
                                      fixed = TRUE) |>
          lapply(utils::head, 2L) |>
          lapply(paste, collapse = " ") |>
          unlist()
      )

    # check for deviation from internal control assays
    dev_int_ctrl_df_col <- colnames(df)[which(df[3L, ] %in% dev_int_ctrl_list)]

    # run only if there are deviations from internal controls
    if (length(dev_int_ctrl_df_col) > 0L) {

      # data frame of internal controls present
      dev_int_ctrl_df <- df |>
        dplyr::select(
          dplyr::all_of(dev_int_ctrl_df_col)
        ) |>
        dplyr::slice(
          c(1L, 3L)
        ) |>
        t() |>
        `colnames<-`(c("panel", "dev_int_ctrl")) |>
        dplyr::as_tibble(
          .name_repair = "minimal"
        ) |>
        dplyr::mutate(
          in_df = TRUE
        )

      # intersect real with expected datasets
      dev_int_ctrl_df_present <- panel_dev_int_ctrl_exp |>
        dplyr::inner_join(
          dev_int_ctrl_df,
          by = c("panel", "dev_int_ctrl")
        )

      # missing internal controls
      dev_int_ctrl_df_missing <- panel_dev_int_ctrl_exp |>
        dplyr::anti_join(
          dev_int_ctrl_df,
          by = c("panel", "dev_int_ctrl")
        ) |>
        dplyr::filter(
          .data[["dev_int_ctrl"]] == .data[["dev_int_ctrl_strip"]]
        ) |>
        dplyr::anti_join(
          dev_int_ctrl_df_present,
          by = c("panel", "dev_int_ctrl_strip")
        )

      if (nrow(dev_int_ctrl_df_missing) > 0L) {

        cli::cli_abort(
          message = c(
            "x" = "Panel(s) {unique(dev_int_ctrl_df_missing$panel)} {?is/are}
            missing one or more of the deviations from the internal control
            assays {unique(dev_int_ctrl_df_missing$dev_int_ctrl)} from row 3 of
            the top matrix with assay data in file {.file {file}}!",
            "i" = "Has the excel file been modified manually?"
          ),
          call = rlang::caller_env(),
          wrap = FALSE
        )

      }

    }

  }

}

#' Help function that splits df_top from a Olink wide excel file into 3 data
#' frames.
#'
#' @author Klev Diamanti
#'
#' @param df The top data frame from a split Olink wide excel file.
#' @param file The input excel file.
#' @param olink_platform The Olink platform used to generate the input file.
#' Expecting "Target 96", "Target 48", "Flex" or "Focus".
#' @param format_spec A one-row data frame filtered from olink_wide_excel_spec
#' with the Olink wide excel file specifications.
#'
#' @return List of the data frames (df_oid, df_meta, df_qc_dev and df_int_ctrl)
#' in long format that df_top is split on.
#'
read_npx_wide_top <- function(df,
                              file,
                              olink_platform,
                              format_spec) {

  # check input and top matrix ----

  check_olink_platform(x = olink_platform,
                       broader_platform = "qPCR")

  check_is_data_frame(df = format_spec,
                      error = TRUE)

  read_npx_wide_check_top(
    df = df,
    file = file,
    format_spec = format_spec
  )

  # transpose df to long ----

  df_t <- t(df) # transpose the wide df
  colnames(df_t) <- df_t[1, ] # add colnames
  df_t <- df_t |>
    dplyr::as_tibble(
      rownames = "col_index"
    ) |>
    dplyr::slice(
      2L:dplyr::n()
    )

  # we have checked in read_npx_wide_check_top that all expected columns based
  # on data_type are present

  # split df_t into its parts ----

  # extract customer assays from assay column
  df_top_oid <- df_t |>
    dplyr::filter(
      !is.na(.data[["OlinkID"]])
    )

  # extract plate_id and qc_warning from Assay column
  df_pid_qcw <- lapply(unlist(format_spec$top_matrix_assay_labels),
                       function(x) {
                         df_t |>
                           dplyr::filter(
                             is.na(.data[["OlinkID"]])
                             & .data[["Assay"]] %in% .env[["x"]]
                           ) |>
                           dplyr::select(
                             -dplyr::any_of(c("Uniprot ID", "OlinkID", "Unit"))
                           ) |>
                           dplyr::rename(
                             "Var" = "Assay"
                           )
                       })
  names(df_pid_qcw) <- paste("df_top", names(df_pid_qcw), sep = "_")

  # extract internal control from Assay column
  df_top_int_ctrl <- df_t |>
    dplyr::filter(
      is.na(.data[["OlinkID"]])
      & .data[["Assay"]] %in% unlist(format_spec$top_matrix_assay_int_ctrl)
    ) |>
    dplyr::select(
      -dplyr::any_of(c("Uniprot ID", "OlinkID", "Unit"))
    )

  # extract deviation from internal controls from Assay column
  df_top_dev_int_ctrl <- df_t |>
    dplyr::filter(
      is.na(.data[["OlinkID"]])
      & .data[["Assay"]] %in% unlist(format_spec$top_matrix_assay_dev_int_ctrl)
    ) |>
    dplyr::select(
      -dplyr::any_of(c("OlinkID", "Unit"))
    )

  # checks ----

  ## check sum of rows ----

  if (nrow(df_t) != (nrow(df_top_oid)
                     + sapply(df_pid_qcw, nrow) |> sum()
                     + nrow(df_top_int_ctrl)
                     + nrow(df_top_dev_int_ctrl))) {

    top_mat_unknown_cols <- setdiff(df_t$col_index, # nolint object_usage_linter
                                    c(df_top_oid$col_index,
                                      sapply(df_pid_qcw, \(x) x$col_index) |>
                                        unname() |>
                                        unlist(),
                                      df_top_int_ctrl$col_index,
                                      df_top_dev_int_ctrl$col_index))

    cli::cli_abort(
      message = c(
        "x" = "The top matrix with the assay data in file {.file {file}} in row
        `Assay` contains unrecognized values in columns:
        {top_mat_unknown_cols}!",
        "i" = "Has the excel file been modified manually?"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

  ## check df_top_oid ----

  # no NAs are allowed in df_top_oid
  if (any(is.na(df_top_oid))) {

    cli::cli_abort(
      message = c(
        "x" = "The top matrix with the assay data in file {.file {file}} expects
        no empty cells for assays other than internal controls. Identified
        { sum(is.na(df_top_oid)) } empty cells!",
        "i" = "Has the excel file been modified manually?"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

  # we will check number of assays only for Target 96 and 48 Olink platforms.
  # Other Olink platforms such as Flex and Focus allow a varying number of
  # assays, which does not allow us to check number of assays.
  expected_num_assays <- accepted_olink_platforms |>
    dplyr::filter(
      .data[["name"]] == .env[["olink_platform"]]
    ) |>
    dplyr::mutate(
      total_n = .data[["base_index"]] * length(unique(df_top_oid$Panel))
    ) |>
    dplyr::pull(
      .data[["total_n"]]
    )

  if (!is.na(expected_num_assays)
      && nrow(df_top_oid) != expected_num_assays) {

    cli::cli_abort(
      message = c(
        "x" = "Detected {nrow(df_top_oid)} assays in
        {length(unique(df_top_oid$Panel))} panels in file {.file {file}}, but
        expected {expected_num_assays}!",
        "i" = "Has the excel file been modified manually?"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

  ## check df_plate & df_qc_warn ----

  # when both elements of the list with required labels are present in the top
  # wide matrix, expect identical dimensions
  if (all(sapply(df_pid_qcw, nrow) > 0L)) {

    # check that that dimensions of the dfs in the list are identical
    if (sapply(df_pid_qcw, dim) |> unique(MARGIN = 2L) |> ncol()  != 1L) {

      cli::cli_abort(
        message = c(
          "x" = "Expected equal number of
          {unlist(format_spec$top_matrix_assay_labels) |>
          sapply(\\(x) paste0(\"`\", x, \"`\")) |>
          cli::ansi_collapse(last = \" and \")} columns in the top matrix with
          the assay data in file {.file {file}}!",
          "i" = "Has the excel file been modified manually?"
        ),
        call = rlang::caller_env(),
        wrap = FALSE
      )

    }

  }

  # return ----

  list_df_top <- append(x = df_pid_qcw,
                        values = list(df_top_oid = df_top_oid))

  # remove df with no rows
  list_df_top <- list_df_top[which(lapply(list_df_top, nrow) != 0L)]

  # add df_top_int_ctrl if not empty
  if (nrow(df_top_int_ctrl) != 0L) {
    list_df_top <- append(x = list_df_top,
                          values = list(df_top_int_ctrl = df_top_int_ctrl))
  }

  # add df_top_dev_int_ctrl if not empty
  if (nrow(df_top_dev_int_ctrl) != 0L) {
    list_df_top <- append(
      x = list_df_top,
      values = list(df_top_dev_int_ctrl = df_top_dev_int_ctrl)
    )
  }

  return(list_df_top)

}

#' Help function that splits df_mid from a Olink wide excel file into 4 data
#' frames.
#'
#' @author Klev Diamanti
#'
#' @param df The middle data frame from a split Olink wide excel file.
#' @param file The input excel file.
#' @param data_type The quantification in which the data comes in. Expecting one
#' of NPX or Quantified.
#' @param col_names Names list of character vector with the names of the columns
#' containing Olink data on assays, plates, QC Warnings, internal control
#' assays and deviations from internal controls.
#'
#' @return A list of data frames (df_oid, df_pid, df_qc_warn and df_int_ctrl) in
#' long format from the middle matrix of the Olink wide excel file.
#'
read_npx_wide_middle <- function(df,
                                 file,
                                 data_type,
                                 col_names) {

  # check input ----

  check_is_data_frame(df = df,
                      error = TRUE)

  check_file_exists(file = file,
                    error = TRUE)

  check_olink_data_type(x = data_type,
                        broader_platform = "qPCR")

  check_is_list(lst = col_names,
                error = TRUE)

  sapply(col_names, function(x) check_is_character(string = x, error = TRUE))

  # check unique sample id ----

  check_columns(df = df, col_list = list("V1"))

  n_uniq_sample <- dplyr::pull(df, .data[["V1"]]) |> unique() |> length()
  if (nrow(df) != n_uniq_sample) {

    cli::cli_abort(
      message = c(
        "x" = "The middle matrix in file {.file {file}} does not contain unique
        sample identifiers. Identified {nrow(df) - n_uniq_sample} duplicates!",
        "i" = "Has the excel file been modified manually?"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

  # check that all relevant columns exist ----

  check_columns(df = df,
                col_list = col_names |>
                  unlist() |>
                  unname() |>
                  as.list())

  # split datasets ----

  ## split assays and pivot to longer ----

  list_df_mid <- list()

  list_df_mid$df_mid_oid <- df |>
    dplyr::select(
      dplyr::all_of(c("V1", col_names$df_top_oid))
    ) |>
    dplyr::rename(
      "SampleID" = dplyr::all_of("V1")
    ) |>
    tidyr::pivot_longer(
      -dplyr::all_of("SampleID"),
      names_to = "col_index",
      values_to = data_type
    )

  ## split plates and pivot to longer ----

  list_df_mid$df_mid_plate <- df |>
    dplyr::select(
      dplyr::all_of(c("V1", col_names$df_top_plate))
    ) |>
    dplyr::rename(
      "SampleID" = dplyr::all_of("V1")
    ) |>
    tidyr::pivot_longer(
      -dplyr::all_of("SampleID"),
      names_to = "col_index",
      values_to = "PlateID"
    )

  ## split qc_warnings and pivot to longer ----

  # done only if the are columns with QC Warning
  if ("df_top_qc_warn" %in% names(col_names)) {

    list_df_mid$df_mid_qc_warn <- df |>
      dplyr::select(
        dplyr::all_of(c("V1", col_names$df_top_qc_warn))
      ) |>
      dplyr::rename(
        "SampleID" = dplyr::all_of("V1")
      ) |>
      tidyr::pivot_longer(
        -dplyr::all_of("SampleID"),
        names_to = "col_index",
        values_to = "QC_Warning"
      )

  }

  ## split internal_controls and pivot to longer ----

  # done only if the are columns with QC Warning
  if ("df_top_int_ctrl" %in% names(col_names)) {

    list_df_mid$df_mid_int_ctrl <- df |>
      dplyr::select(
        dplyr::all_of(c("V1", col_names$df_top_int_ctrl))
      ) |>
      dplyr::rename(
        "SampleID" = dplyr::all_of("V1")
      ) |>
      tidyr::pivot_longer(
        -dplyr::all_of("SampleID"),
        names_to = "col_index",
        values_to = data_type
      )
  }

  ## split deviation from internal_controls and pivot to longer ----

  # done only if the are columns with QC Warning
  if ("df_top_dev_int_ctrl" %in% names(col_names)) {

    list_df_mid$df_mid_dev_int_ctrl <- df |>
      dplyr::select(
        dplyr::all_of(c("V1", col_names$df_top_dev_int_ctrl))
      ) |>
      dplyr::rename(
        "SampleID" = dplyr::all_of("V1")
      ) |>
      tidyr::pivot_longer(
        -dplyr::all_of("SampleID"),
        names_to = "col_index",
        values_to = data_type
      )
  }

  # checks ---------

  ## check number of rows in plate and qc_warning data frames ----

  # done only if the are columns with QC Warning
  if ("df_mid_qc_warn" %in% names(list_df_mid)) {

    if (nrow(list_df_mid$df_mid_plate) != nrow(list_df_mid$df_mid_qc_warn)) {

      cli::cli_abort(
        message = c(
          "x" = "Uneven number of entries of \"Plate ID\" and \"QC Warning\" in
          the middle matrix of the Olink wide excel file {.file {file}}!",
          "i" = "Has the excel file been modified manually?"
        ),
        call = rlang::caller_env(),
        wrap = FALSE
      )

    }

  }

  ## check if all cols from df_mid were consumed ----

  col_names_mid <- sapply(list_df_mid, function(x) x$col_index) |>
    unlist() |>
    unname() |>
    unique()
  col_names_mid <- c("V1", col_names_mid) |> sort()

  if (!identical(x = colnames(df) |> sort(),
                 y = col_names_mid)) {

    col_mid_missing <- colnames(df)[!(colnames(df) %in% col_names_mid)]
    col_mid_missing <- sub(pattern = "V", replacement = "", x = col_mid_missing)

    cli::cli_abort(
      message = c(
        "x" = "Unable to assign column(s) {col_mid_missing} from the Olink wide
        excel file {.file {file}}!",
        "i" = "Has the excel file been modified manually?"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

  # return ----

  return(list_df_mid)

}

#' Combine top and middle matrices.
#'
#' @author Klev Diamanti
#'
#' @param df_top_list List of data frames from the top matrix. Output of
#' function `read_npx_wide_top`.
#' @param df_middle_list List of data frames from the middle matrix. Output of
#' function `read_npx_wide_middle`.
#' @param data_type The quantification in which the data comes in. Expecting one
#' of NPX or Quantified.
#' @param format_spec A one-row data frame filtered from olink_wide_excel_spec
#' with the Olink wide excel file specifications.
#'
#' @return A tibble combining the top and middle matrices in long format.
#'
red_npx_wide_top_mid_long <- function(df_top_list,
                                      df_middle_list,
                                      data_type,
                                      format_spec) {
  # check input ----

  check_is_list(lst = df_top_list)

  sapply(df_top_list, check_is_data_frame, error = TRUE)

  check_is_list(lst = df_middle_list)

  sapply(df_middle_list, check_is_data_frame, error = TRUE)

  check_is_tibble(df = format_spec,
                  error = TRUE)

  # prepare components of long df ----

  ## df oid ----

  df_long <- df_middle_list$df_mid_oid |>
    dplyr::left_join(
      df_top_list$df_top_oid,
      by = "col_index",
      relationship = "many-to-one"
    )

  ## df plate id ----

  df_plate <- df_middle_list$df_mid_plate |>
    dplyr::left_join(
      df_top_list$df_top_plate,
      by = "col_index",
      relationship = "many-to-one"
    ) |>
    dplyr::select(
      -dplyr::any_of(c("Var", "Unit", "col_index"))
    )

  df_long <- df_long |>
    dplyr::left_join(
      df_plate,
      by = c("SampleID", "Panel"),
      relationship = "many-to-one"
    )
  rm(df_plate)

  ## df qc warning ----

  if (format_spec$has_qc_data == TRUE) {

    df_qc_warn <- df_middle_list$df_mid_qc_warn |>
      dplyr::left_join(
        df_top_list$df_top_qc_warn,
        by = "col_index",
        relationship = "many-to-one"
      ) |>
      dplyr::select(
        -dplyr::any_of(c("Var", "Unit", "col_index"))
      )

    df_long <- df_long |>
      dplyr::left_join(
        df_qc_warn,
        by = c("SampleID", "Panel"),
        relationship = "many-to-one"
      )
    rm(df_qc_warn)
  }

  ## df internal controls ----

  if ("df_mid_int_ctrl" %in% names(df_middle_list)
      && "df_top_int_ctrl" %in% names(df_top_list)) {

    df_int_ctrl <- df_middle_list$df_mid_int_ctrl |>
      dplyr::left_join(
        df_top_list$df_top_int_ctrl,
        by = "col_index",
        relationship = "many-to-one"
      )

    # join with the plate df
    df_int_ctrl_pid <- df_int_ctrl |>
      dplyr::left_join(
        df_long |>
          dplyr::select(
            dplyr::any_of("QC_Warning"),
            dplyr::all_of(c("PlateID", "SampleID", "Panel"))
          ) |>
          dplyr::distinct(),
        by = c("SampleID", "Panel"),
        relationship = "many-to-one"
      )

    df_long <- df_long |>
      dplyr::bind_rows(
        df_int_ctrl_pid
      )
    rm(df_int_ctrl, df_int_ctrl_pid)
  }

  ## df deviation internal controls ----

  if ("df_mid_dev_int_ctrl" %in% names(df_middle_list)
      && "df_top_dev_int_ctrl" %in% names(df_top_list)) {

    df_dev_int_ctrl <- df_middle_list$df_mid_dev_int_ctrl |>
      dplyr::left_join(
        df_top_list$df_top_dev_int_ctrl,
        by = "col_index",
        relationship = "many-to-one"
      ) |>
      dplyr::select(
        -dplyr::all_of(c("Assay", "col_index"))
      ) |>
      tidyr::pivot_wider(
        id_cols = dplyr::all_of(c("SampleID", "Panel")),
        names_from = dplyr::all_of("Uniprot ID"),
        values_from = dplyr::all_of(data_type)
      )

    df_long <- df_long |>
      dplyr::left_join(
        df_dev_int_ctrl,
        by = c("SampleID", "Panel"),
        relationship = "many-to-one"
      )
    rm(df_dev_int_ctrl)
  }

  # return ----

  return(df_long)
}

#' Helper function that output all possible row names for the bottom matrix.
#'
#' @author Klev Diamanti
#'
#' @param data_type The quantification in which the data comes in. Expecting one
#' of NPX, Quantified or Ct.
#' @param olink_platform The Olink platform used to generate the input file.
#' Expecting "Target 96", "Target 48", "Flex" or "Focus".
#'
#' @return List of data frames with two columns each. Columns contain unique and
#' alternative expected names for the V1 of each row of the bottom matrix in an
#' Olink wide file.
#'
read_npx_wide_bottom_version <- function(data_type,
                                         olink_platform) {

  # extract all possible variable names from the global matrix
  format_spec_bottom <- olink_wide_excel_bottom_matrix |>
    dplyr::filter(
      .data[["olink_platform"]] == .env[["olink_platform"]]
      & .data[["data_type"]] == .env[["data_type"]]
    )

  # number of unique versions of names
  format_spec_bottom_v <- format_spec_bottom$version |> unique()

  # list dfs to store possible combinations of names in V1 of bottom matrix
  list_bottom_v <- list()

  # return if 0 is the only available version
  if (length(format_spec_bottom_v) == 1L
      && format_spec_bottom_v == 0L) {

    list_bottom_v$`0` <- format_spec_bottom |>
      dplyr::select(
        dplyr::all_of("plate_specific"),
        dplyr::starts_with("variable")
      )

  } else {

    # if 0 is not the only version, then create a list of df with each element
    # containing one version merged with 0 version
    format_spec_bottom_v <- format_spec_bottom_v[format_spec_bottom_v != 0L]

    list_bottom_v <- lapply(format_spec_bottom_v,
                            function(x) {
                              format_spec_bottom |>
                                dplyr::filter(
                                  .data[["version"]] %in% c(0L, x)
                                ) |>
                                dplyr::select(
                                  dplyr::all_of("plate_specific"),
                                  dplyr::starts_with("variable")
                                )
                            })
    names(list_bottom_v) <- format_spec_bottom_v

  }

  return(list_bottom_v)
}

#' Help function that converts df_bottom into long format.
#'
#' @author Klev Diamanti
#'
#' @param df The bottom data frame from a split Olink wide excel file.
#' @param file The input excel file.
#' @param olink_platform The Olink platform used to generate the input file.
#' Expecting "Target 96", "Target 48", "Flex" or "Focus".
#' @param data_type The quantification in which the data comes in. Expecting one
#' of NPX, Quantified or Ct.
#' @param col_names Names list of character vector with the names of the columns
#' containing Olink data on assays and internal control assays.
#' @param format_spec A one-row data frame filtered from olink_wide_excel_spec
#' with the Olink wide excel file specifications.
#' @param df_plate_panel Data frame with unique combinations of panels and
#' plates from the combination of top and middle data frames.
#'
#' @return The bottom matrix in long format.
#'
read_npx_wide_bottom <- function(df,
                                 file,
                                 olink_platform,
                                 data_type,
                                 col_names,
                                 format_spec,
                                 df_plate_panel) {
  # check input ----

  check_is_data_frame(df = df,
                      error = TRUE)

  check_file_exists(file = file,
                    error = TRUE)

  check_olink_platform(x = olink_platform,
                       broader_platform = "qPCR")

  check_olink_data_type(x = data_type,
                        broader_platform = "qPCR")

  check_is_list(lst = col_names,
                error = TRUE)

  sapply(col_names, function(x) check_is_character(string = x, error = TRUE))

  check_is_data_frame(df = format_spec,
                      error = TRUE)

  check_is_data_frame(df = df_plate_panel,
                      error = TRUE)

  # get first column options ----

  # list with all possible combinations
  format_spec_bottom <- read_npx_wide_bottom_version(
    data_type = data_type,
    olink_platform = olink_platform
  )

  # check first column ----

  # check that column "V1" exists in the df
  check_columns(df = df, col_list = list("V1"))

  # check that at least one of the alternatives combinations of names
  # contains all names in V1
  format_spec_bottom <- lapply(format_spec_bottom, function(x) {
    name_in_df <- lapply(x$variable_alt_names,
                         \(y) (y[y %in% df$V1])) |>
      lapply(\(y) (ifelse(length(y) == 0L, NA_character_, y))) |>
      unlist()

    x |>
      dplyr::mutate(
        variable_name_in_df = name_in_df,
        in_df = dplyr::if_else(
          is.na(.data[["variable_name_in_df"]]),
          FALSE,
          TRUE
        )
      )
  })

  names_in_v1 <- sapply(format_spec_bottom, \(x) (all(x$in_df))) |>
    (\(x) x[x == TRUE])()

  # if none or multiple combinations match
  # or, if df$V1 is a superset of the expected columns
  if (length(names_in_v1) != 1
      || nrow(format_spec_bottom[[names(names_in_v1)]])
      != length(unique(df$V1))) {

    bottom_mat_v1_expected <- sapply( # nolint object_usage_linter
      format_spec_bottom,
      function(x) {
        sapply(x$variable_alt_names, utils::head, 1L) |>
          cli::ansi_collapse()
      }
    ) |>
      cli::ansi_collapse(sep2 = ", or ", last = ", or ")

    cli::cli_abort(
      message = c(
        "x" = "Unexpected values in column 1 of the bottom matrix with QC data
        in file {.file {file}}. Expected on of the combos:
        {bottom_mat_v1_expected}",
        "i" = "Has the excel file been modified manually?"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

  # clean up format_spec_bottom for downstream use
  format_spec_bottom_df <- format_spec_bottom[[names(names_in_v1)]] |>
    dplyr::select(
      -dplyr::all_of(c("variable_name", "variable_alt_names", "in_df"))
    )

  # keep necessary columns ----

  # columns expected to be present in the df
  expected_cols <- c("V1", col_names$df_top_oid)
  if ("df_top_int_ctrl" %in% names(col_names)) {
    expected_cols <- c(expected_cols, col_names$df_top_int_ctrl)
  }
  if (any(format_spec_bottom_df$plate_specific == TRUE)) {
    expected_cols <- c(expected_cols, col_names$df_top_plate)
  }
  # check that columns in expected_cols exist in the df
  check_columns(df = df, col_list = as.list(expected_cols))

  df <- df |>
    # keep only columns absolutely required:
    # V1 that contains names of variables
    # columns that contain info for customer assays
    # columns that contain info for internal control assays
    # columns that might contain plate names in case of plate-specific QC data
    dplyr::select(
      dplyr::all_of(expected_cols)
    )

  # per-plate metrics ----

  if (any(format_spec_bottom_df$plate_specific == TRUE)) {

    # plate specific qc metrics
    format_spec_bottom_plate_spec <- format_spec_bottom_df |>
      dplyr::filter(
        .data[["plate_specific"]] == TRUE
      ) |>
      dplyr::pull(
        .data[["variable_name_in_df"]]
      )

    # extract rows with plate-specific metrics
    df_plate_spec <- df |>
      # keep only rows with -plate-specific info
      dplyr::filter(
        .data[["V1"]] %in% format_spec_bottom_plate_spec
      ) |>
      remove_all_na_cols()

    # equal number of rows for each QC metric at bottom matrix
    df_plate_spec_n_row <- df_plate_spec |>
      dplyr::pull(
        .data[["V1"]]
      ) |>
      table() |>
      unname() |>
      unique() |>
      length()

    if (df_plate_spec_n_row != 1L) {

      cli::cli_abort(
        message = c(
          "x" = "Column 1 of the bottom matrix does not contain the same number
          of  rows for plate-specific QC measurement(s)
          {format_spec_bottom_plate_spec} in file {.file {file}}!",
          "i" = "Has the excel file been modified manually?"
        ),
        call = rlang::caller_env(),
        wrap = FALSE
      )

    }

    # for each variable in V1 do a pivot_longer
    df_plate_spec <- lapply(
      format_spec_bottom_plate_spec,
      function(x) {
        df_plate_spec |>
          # keep only one Vq variable at a time
          dplyr::filter(
            .data[["V1"]] == .env[["x"]]
          ) |>
          # remove V1 columns to allow for pivoting
          dplyr::select(
            -dplyr::all_of("V1")
          ) |>
          # make the wide matrix into a long one
          # each PlateID will become a column
          tidyr::pivot_longer(
            -dplyr::all_of(col_names$df_top_plate),
            names_to = "col_index",
            values_to = x
          ) |>
          # one more pivot longer to make PlateID into a single column
          tidyr::pivot_longer(
            -dplyr::all_of(c("col_index", x)),
            names_to = "col_index_pid",
            values_to = "PlateID"
          ) |>
          # remove the PlateID index column
          dplyr::select(
            -dplyr::all_of("col_index_pid")
          )
      }
    )

    # left join all data frames from the list
    df_plate_spec <- Reduce(f = function(df_1, df_2) {
      dplyr::left_join(x = df_1,
                       y = df_2,
                       by = c("PlateID", "col_index"),
                       relationship = "one-to-one")
    },
    x = df_plate_spec)

  }

  # plates-shared metrics ----

  # plate shared qc metrics
  format_spec_bottom_plate_share <- format_spec_bottom_df |>
    dplyr::filter(
      .data[["plate_specific"]] == FALSE
    ) |>
    dplyr::pull(
      .data[["variable_name_in_df"]]
    )

  # remove rows processed earlier as plate-specific
  df_plate_shared <- df |>
    # keep only rows with plate-shared info
    dplyr::filter(
      .data[["V1"]] %in% format_spec_bottom_plate_share
    ) |>
    remove_all_na_cols()

  df_plate_shared <- t(df_plate_shared)
  colnames(df_plate_shared) <- df_plate_shared[1, ]
  df_plate_shared <- df_plate_shared |>
    dplyr::as_tibble(
      rownames = "col_index"
    ) |>
    dplyr::slice(
      2L:dplyr::n()
    )

  # join df_plate_shared and df_plate_spec ----

  if (exists("df_plate_spec")) {

    df_long <- df_plate_shared |>
      dplyr::full_join(
        y = df_plate_spec,
        by = "col_index",
        relationship = "one-to-many"
      ) |>
      # remove duplicates from the pivot_longer in PlateID
      dplyr::inner_join(
        df_plate_panel,
        by = c("col_index", "PlateID")
      ) |>
      dplyr::select(
        -dplyr::all_of(c("Panel"))
      )

  } else {

    df_long <- df_plate_shared

  }

  # output list_df ----

  # split into internal controls and assays
  if ("df_top_int_ctrl" %in% names(col_names)) {

    list_df <- list(
      df_bottom_oid = df_long |>
        dplyr::filter(
          .data[["col_index"]] %in% col_names$df_top_oid
        ),
      df_bottom_int_ctrl = df_long |>
        dplyr::filter(
          .data[["col_index"]] %in% col_names$df_top_int_ctrl
        )
    )

  } else {

    list_df <- list(
      df_bottom_oid = df_long
    )

  }

  return(list_df)

}
