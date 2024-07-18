#' Convert Olink data in wide format with NPX, Ct or Quantified data to long
#' format.
#'
#' @author
#'   Klev Diamanti
#'
#' @param df A tibble containing the full Olink dataset in wide format.
#' @param file Path to Olink software output file in wide or long format.
#' Expecting file extensions
#' `r cli::ansi_collapse(x = accepted_npx_file_ext[grepl("excel|delim", names(accepted_npx_file_ext))], last = " or ")`. # nolint
#' @param olink_platform Olink platform used to generate the input file.
#' One of
#' `r accepted_olink_platforms |> dplyr::filter(.data[["broader_platform"]] == "qPCR") |> dplyr::pull(.data[["name"]]) |> cli::ansi_collapse(last = " or ")`. # nolint
#' @param data_type Quantification method of the input data. One of
#' `r accepted_olink_platforms$quant_method |> unlist() |> unique() |> sort() |> cli::ansi_collapse(last = " or ")`. # nolint
#'
#' @return Tibble with Olink data in long format.
#'
#' @seealso
#'   \code{\link{read_npx_format}}
#'   \code{\link{read_npx_wide_split_row}}
#'   \code{\link{read_npx_wide_npxs_version}}
#'   \code{\link{read_npx_wide_top}}
#'   \code{\link{read_npx_wide_middle}}
#'   \code{\link{read_npx_wide_bottom}}
#'
read_npx_wide <- function(df,
                          file,
                          data_type,
                          olink_platform) {

  # initial checks ----

  check_is_tibble(df = df,
                  error = TRUE)

  check_file_exists(file = file,
                    error = TRUE)

  check_olink_data_type(x = data_type,
                        broader_platform = "qPCR")

  check_olink_platform(x = olink_platform,
                       broader_platform = "qPCR")

  # get expected format specifications ----

  format_spec <- olink_wide_spec |>
    dplyr::filter(
      .data[["data_type"]] == .env[["data_type"]]
    )

  # split the file into sub-data frames ----

  df_split_row <- read_npx_wide_split_row(
    df = df,
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

#' Split Olink wide files to sub-matrices.
#'
#' @description
#' Olink datasets in wide format contain 1 or 2 rows with all columns NA marking
#' sub-matrices of the data. This function takes advantage of that feature and
#' splits the dataset into 3 or 4 sub-matrices. Each sub-matrix is used
#' downstream to assemble a long data frame.
#'
#' Specifically:
#' \itemize{
#'   \item \strong{Head matrix} consists of the first 2 rows of the wide
#'   dataset. This matrix contains the project name, the NPX Signature version
#'   that was used to generate the wide dataset and the quantification method.
#'   \item \strong{Top matrix} consists of the next 4 or 5 rows of the wide
#'   dataset, depending on the quantification method. This matrix contains data
#'   on assays, panels, columns with plate identifiers, columns with sample QC
#'   warnings and column with deviations from the internal controls. Note that
#'   not all the columns are present in all datasets and for all quantification
#'   methods. The local environment variable \var{olink_wide_spec} marks all
#'   the expected configurations.
#'   \item \strong{Middle matrix} is marked by rows with all columns "NA: above
#'   and below. This matrix contains sample identifiers, quantification
#'   measurements for all assays, plate identifiers, sample QC warnings and
#'   deviations from the internal controls.
#'   \item \strong{Bottom matrix} is located below the middle matrix and
#'   contains information of LOD, missing frequency, assay warning and data
#'   normalization approach. Note that this matrix is not available for all
#'   quantification methods.
#' }
#'
#' @author
#'   Klev Diamanti
#'
#' @param df A tibble containing the full Olink dataset in wide format.
#' @param file Path to Olink software output file in wide or long format.
#' Expecting file extensions
#' `r cli::ansi_collapse(x = accepted_npx_file_ext[grepl("excel|delim", names(accepted_npx_file_ext))], last = " or ")`. # nolint
#' @param data_type Quantification method of the input data. One of
#' `r accepted_olink_platforms$quant_method |> unlist() |> unique() |> sort() |> cli::ansi_collapse(last = " or ")`. # nolint
#' @param format_spec A tibble derived from \var{olink_wide_spec} in the local
#' environment containing the expected format of the Olink wide file based on
#' the \var{olink_platform} and \var{data_type}.
#'
#' @return A named list of tibbles containing the sub-matrices of the Olink wide
#' format file split on:
#' \itemize{
#'   \item \strong{Head matrix} as \var{df_head}
#'   \item \strong{Top matrix} as \var{df_top}
#'   \item \strong{Middle} as \var{df_mid}
#'   \item \strong{Bottom matrix} as \var{df_bottom}
#' }
#'
#' @seealso
#'   \code{\link{read_npx_wide}}
#'   \code{\link{read_npx_wide_npxs_version}}
#'   \code{\link{read_npx_wide_top}}
#'   \code{\link{read_npx_wide_middle}}
#'   \code{\link{read_npx_wide_bottom}}
#'
read_npx_wide_split_row <- function(df,
                                    file,
                                    data_type,
                                    format_spec) {

  # initial checks ----

  check_is_tibble(df = df,
                  error = TRUE)

  check_file_exists(file = file,
                    error = TRUE)

  check_olink_data_type(x = data_type,
                        broader_platform = "qPCR")

  check_is_tibble(df = format_spec,
                  error = TRUE)

  # help gunction to fill NA ----

  # we are not using tidyr::fill because it still depends on dplyr::mutate_at
  # which is superseded, and throws error in test
  fill_na_with_previous <- function(x) {
    last_non_na <- NA
    for (i in seq_along(x)) {
      if (!is.na(x[i])) {
        last_non_na <- x[i]
      } else {
        x[i] <- last_non_na
      }
    }
    return(x)
  }

  # detect rows with all NA columns ----

  # Use the rows full of NAs in the file to compute the number of rows
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
    )

  if (nrow(na_row_index) > 0L) {
    na_row_index <- na_row_index |>
      # allow for consecutive all NA rows
      dplyr::mutate(
        dif_nxt_r = dplyr::lead(.data[["row_number"]]) - .data[["row_number"]],
        dif_nxt_r = fill_na_with_previous(x = .data[["dif_nxt_r"]])
      ) |>
      # create groups of consecutive rows with all cols NA
      dplyr::mutate(
        all_na_g = dplyr::case_when(
          dplyr::row_number() == 1L ~ paste0("g", .data[["row_number"]]),
          dplyr::lag(.data[["dif_nxt_r"]]) == 1L ~ NA_character_,
          TRUE ~ paste0("g", .data[["row_number"]]),
          .default = NA_character_
        ),
        all_na_g = fill_na_with_previous(x = .data[["all_na_g"]])
      ) |>
      # extract start and end of groups
      dplyr::group_by(.data[["all_na_g"]]) |>
      dplyr::summarise(
        start_g = min(.data[["row_number"]]),
        end_g = max(.data[["row_number"]]),
        .groups = "drop"
      ) |>
      dplyr::arrange(
        .data[["start_g"]], .data[["end_g"]]
      )
  }

  # check that row indexes are correct ----

  # check that there are 1 or 2 rows with all NA
  if (nrow(na_row_index) != format_spec$n_na_rows) {

    cli::cli_abort(
      message = c(
        "x" = "We identified
        {ifelse(identical(na_row_index, integer(0L)), 0L, nrow(na_row_index))}
        rows with all columns `NA` in file {.file {file}}, while we expected
        {format_spec$n_na_rows}!",
        "i" = "Has the file been modified manually?"
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
      na_row_index$end_g + 1L # all NA rows +1
    ),
    end_row = c(
      2L, # header
      na_row_index$start_g - 1L, # all NA rows -1
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
  if (nrow(na_row_index) == 1L) {
    names(list_df_split) <- c("df_head", "df_top", "df_mid")
  } else {
    names(list_df_split) <- c("df_head", "df_top", "df_mid", "df_bottom")
  }

  # return list of data frames ----

  return(list_df_split)

}

#' Extract version of NPX Signature from the head matrix of Olink datasets in
#' wide format.
#'
#' @author
#'   Klev Diamanti
#'
#' @param df Head matrix of Olink dataset in wide format \var{df_head}.
#'
#' @return The version of the NPX Signature software.
#'
#' @seealso
#'   \code{\link{read_npx_wide}}
#'   \code{\link{read_npx_wide_split_row}}
#'   \code{\link{read_npx_wide_npxs_version}}
#'   \code{\link{read_npx_wide_top}}
#'   \code{\link{read_npx_wide_middle}}
#'   \code{\link{read_npx_wide_bottom}}
#'
read_npx_wide_npxs_version <- function(df) {

  # check input ----

  check_is_tibble(df = df,
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
    as.character() |>
    (\(x) {
      strsplit(x = x,
               split = " ",
               fixed = TRUE) |>
        lapply(utils::tail, 1L) |>
        unlist()
    })()

  # return ----

  return(npxs_sw_v)

}

#' Additional checks of the top matrix of Olink dataset in wide format.
#'
#' @author
#'   Klev Diamanti
#'
#' @param df Top matrix of Olink datasets in wide format \var{df_top}.
#' @param file Path to Olink software output file in wide or long format.
#' Expecting file extensions
#' `r cli::ansi_collapse(x = accepted_npx_file_ext[grepl("excel|delim", names(accepted_npx_file_ext))], last = " or ")`. # nolint
#' @param format_spec A tibble derived from \var{olink_wide_spec} in the local
#' environment containing the expected format of the Olink wide file based on
#' the \var{olink_platform} and \var{data_type}.
#'
#' @return NULL unless an inconsistency is spotted.
#'
#' @seealso
#'   \code{\link{read_npx_wide_top}}
#'
read_npx_wide_check_top <- function(df,
                                    file,
                                    format_spec) {

  # initial checks ----

  check_is_tibble(df = df,
                  error = TRUE)

  check_file_exists(file = file,
                    error = TRUE)

  check_is_tibble(df = format_spec,
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

    top_v1_miss <- top_mat_v1[!(top_mat_v1 %in% df$V1)] # nolint

    cli::cli_abort(
      message = c(
        "x" = "Column 1 of the top matrix with assay data in file
        {.file {file}} does not contain: {top_v1_miss}",
        "i" = "Has the file been modified manually?"
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

    top_mat_assay_miss <- top_mat_assay_labels[!(top_mat_assay_labels %in% df[2L, ])] # nolint

    cli::cli_abort(
      message = c(
        "x" = "Row 2 of the top matrix with assay data in file {.file {file}}
        does not contain: {top_mat_assay_miss}",
        "i" = "Has the file been modified manually?"
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
          "i" = "Has the file been modified manually?"
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
            "i" = "Has the file been modified manually?"
          ),
          call = rlang::caller_env(),
          wrap = FALSE
        )

      }

    }

  }

}

#' Split the top matrix from Olink dataset in wide format.
#'
#' @description
#' The function splits the top matrix \var{df_top} into chunks of columns, each
#' of which contains separate information that will be combined with matching
#' chunks from \var{df_mid} to convert the wide dataset into a long one.
#'
#' @author
#'   Klev Diamanti
#'
#' @param df Top matrix of Olink dataset in wide format \var{df_top}.
#' @param file Path to Olink software output file in wide or long format.
#' Expecting file extensions
#' `r cli::ansi_collapse(x = accepted_npx_file_ext[grepl("excel|delim", names(accepted_npx_file_ext))], last = " or ")`. # nolint
#' @param olink_platform Olink platform used to generate the input file.
#' One of
#' `r accepted_olink_platforms |> dplyr::filter(.data[["broader_platform"]] == "qPCR") |> dplyr::pull(.data[["name"]]) |> cli::ansi_collapse(last = " or ")`. # nolint
#' @param format_spec A tibble derived from \var{olink_wide_spec} in the local
#' environment containing the expected format of the Olink wide file based on
#' the \var{olink_platform} and \var{data_type}.
#'
#' @return A list of data frames from top matrix in long format:
#' \itemize{
#'   \item Data frame containing Olink assays \var{df_top_oid}
#'   \item Data frame containing plate identifiers \var{df_top_pid}
#'   \item Data frame containing QC warnings \var{df_top_qc_warn}
#'   \item Data frame containing internal control assays \var{df_top_int_ctrl}
#'   \item Data frame containing deviation from internal control assays
#'   \var{df_top_dev_int_ctrl}
#' }
#'
#' @seealso
#'   \code{\link{read_npx_wide}}
#'   \code{\link{read_npx_wide_split_row}}
#'   \code{\link{read_npx_wide_npxs_version}}
#'   \code{\link{read_npx_wide_middle}}
#'   \code{\link{read_npx_wide_bottom}}
#'
read_npx_wide_top <- function(df,
                              file,
                              olink_platform,
                              format_spec) {

  # check input and top matrix ----

  check_olink_platform(x = olink_platform,
                       broader_platform = "qPCR")

  check_is_tibble(df = format_spec,
                  error = TRUE)

  read_npx_wide_check_top(
    df = df,
    file = file,
    format_spec = format_spec
  )

  # transpose df to long ----

  df_t <- t(df) # transpose the wide df
  colnames(df_t) <- df_t[1L, ] # add colnames
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

    top_mat_unknown_cols <- setdiff(df_t$col_index, # nolint
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
        "i" = "Has the file been modified manually?"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

  ## check df_top_oid ----

  # no NAs are allowed in df_top_oid in any column other than "Uniprot ID"
  # the latter because the assay NT-proBNP does not have a Uniprot ID
  if (any(is.na(dplyr::select(df_top_oid, -dplyr::all_of("Uniprot ID"))))) {

    cli::cli_abort(
      message = c(
        "x" = "The top matrix with the assay data in file {.file {file}} expects
        no empty cells for assays other than internal controls. Identified
        { sum(is.na(df_top_oid)) } empty cells!",
        "i" = "Has the file been modified manually?"
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
        "i" = "Has the file been modified manually?"
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
          "i" = "Has the file been modified manually?"
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

#' Split the middle matrix from Olink dataset in wide format.
#'
#' @description
#' Use chunks of columns from \code{\link{read_npx_wide_top}} to split the
#' middle matrix \var{df_mid} into corresponding chunks of columns.
#'
#' @author
#'   Klev Diamanti
#'
#' @param df Middle matrix of Olink dataset in wide format \var{df_mid}.
#' @param file Path to Olink software output file in wide or long format.
#' Expecting file extensions
#' `r cli::ansi_collapse(x = accepted_npx_file_ext[grepl("excel|delim", names(accepted_npx_file_ext))], last = " or ")`. # nolint
#' @param data_type Quantification method of the input data. One of
#' `r accepted_olink_platforms$quant_method |> unlist() |> unique() |> sort() |> cli::ansi_collapse(last = " or ")`. # nolint
#' @param col_names Names list of character vectors containing column names from
#' each chunk of columns \var{df_top} was split on in function.
#' \code{\link{read_npx_wide_top}}.
#'
#' @return A list of data frames (df_oid, df_pid, df_qc_warn and df_int_ctrl) in
#' long format from the middle matrix of an Olink wide file.
#'
#' A list of data frames from middle matrix in long format:
#' \itemize{
#'   \item Data frame containing measurements of Olink assays \var{df_mid_oid}
#'   \item Data frame containing plate identifiers \var{df_mid_pid}
#'   \item Data frame containing QC warnings \var{df_mid_qc_warn}
#'   \item Data frame containing measurements of internal control assays
#'   \var{df_mid_int_ctrl}
#'   \item Data frame containing measurements of deviations from internal
#'   control assays \var{df_mid_dev_int_ctrl}
#' }
#'
#' @seealso
#'   \code{\link{read_npx_wide}}
#'   \code{\link{read_npx_wide_split_row}}
#'   \code{\link{read_npx_wide_npxs_version}}
#'   \code{\link{read_npx_wide_top}}
#'   \code{\link{read_npx_wide_bottom}}
#'
read_npx_wide_middle <- function(df,
                                 file,
                                 data_type,
                                 col_names) {

  # check input ----

  check_is_tibble(df = df,
                  error = TRUE)

  check_file_exists(file = file,
                    error = TRUE)

  check_olink_data_type(x = data_type,
                        broader_platform = "qPCR")

  check_is_list(lst = col_names,
                error = TRUE)

  sapply(col_names, function(x) check_is_character(string = x, error = TRUE))

  # check columns ----

  check_columns(df = df, col_list = list("V1"))

  # check unique sample identifiers ----

  n_uniq_sample <- dplyr::pull(df, .data[["V1"]]) |> unique() |> length()
  if (nrow(df) != n_uniq_sample) {

    cli::cli_inform(
      message = c(
        "i" = "The middle matrix in file {.file {file}} does not contain unique
        sample identifiers. Identified {nrow(df) - n_uniq_sample} duplicates!"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

  # add a new column `rid` the will be used as a proxy to allow duplicated
  # sample identifiers as input.
  # Down stream we will be using SampleID and rid to match matrices.
  df <- df |>
    dplyr::mutate(
      rid = dplyr::row_number()
    )

  # check that all relevant columns exist ----

  check_columns(df = df,
                col_list = col_names |>
                  unlist() |>
                  unname() |>
                  (\(.x) c(.x, "rid"))() |> # adding rid to checked columns
                  as.list())

  # split datasets ----

  ## split assays and pivot to longer ----

  list_df_mid <- list()

  list_df_mid$df_mid_oid <- df |>
    dplyr::select(
      dplyr::all_of(c("V1", "rid", col_names$df_top_oid))
    ) |>
    dplyr::rename(
      "SampleID" = dplyr::all_of("V1")
    ) |>
    tidyr::pivot_longer(
      -dplyr::all_of(c("SampleID", "rid")),
      names_to = "col_index",
      values_to = data_type
    )

  ## split plates and pivot to longer ----

  list_df_mid$df_mid_plate <- df |>
    dplyr::select(
      dplyr::all_of(c("V1", "rid", col_names$df_top_plate))
    ) |>
    dplyr::rename(
      "SampleID" = dplyr::all_of("V1")
    ) |>
    tidyr::pivot_longer(
      -dplyr::all_of(c("SampleID", "rid")),
      names_to = "col_index",
      values_to = "PlateID"
    )

  ## split qc_warnings and pivot to longer ----

  # done only if the are columns with QC Warning
  if ("df_top_qc_warn" %in% names(col_names)) {

    list_df_mid$df_mid_qc_warn <- df |>
      dplyr::select(
        dplyr::all_of(c("V1", "rid", col_names$df_top_qc_warn))
      ) |>
      dplyr::rename(
        "SampleID" = dplyr::all_of("V1")
      ) |>
      tidyr::pivot_longer(
        -dplyr::all_of(c("SampleID", "rid")),
        names_to = "col_index",
        values_to = "QC_Warning"
      )

  }

  ## split internal_controls and pivot to longer ----

  # done only if the are columns with QC Warning
  if ("df_top_int_ctrl" %in% names(col_names)) {

    list_df_mid$df_mid_int_ctrl <- df |>
      dplyr::select(
        dplyr::all_of(c("V1", "rid", col_names$df_top_int_ctrl))
      ) |>
      dplyr::rename(
        "SampleID" = dplyr::all_of("V1")
      ) |>
      tidyr::pivot_longer(
        -dplyr::all_of(c("SampleID", "rid")),
        names_to = "col_index",
        values_to = data_type
      )
  }

  ## split deviation from internal_controls and pivot to longer ----

  # done only if the are columns with QC Warning
  if ("df_top_dev_int_ctrl" %in% names(col_names)) {

    list_df_mid$df_mid_dev_int_ctrl <- df |>
      dplyr::select(
        dplyr::all_of(c("V1", "rid", col_names$df_top_dev_int_ctrl))
      ) |>
      dplyr::rename(
        "SampleID" = dplyr::all_of("V1")
      ) |>
      tidyr::pivot_longer(
        -dplyr::all_of(c("SampleID", "rid")),
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
          the middle matrix of the Olink wide format file {.file {file}}!",
          "i" = "Has the file been modified manually?"
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
    unique() |>
    (\(.x) c("V1", "rid", .x))() |>
    sort()

  if (!identical(x = colnames(df) |> sort(),
                 y = col_names_mid)) {

    col_mid_missing <- colnames(df)[!(colnames(df) %in% col_names_mid)]
    col_mid_missing <- sub(pattern = "V", replacement = "", x = col_mid_missing)

    cli::cli_abort(
      message = c(
        "x" = "Unable to assign column(s) {col_mid_missing} from the Olink wide
        format file {.file {file}}!",
        "i" = "Has the file been modified manually?"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

  # return ----

  return(list_df_mid)

}

#' Help function to extract Panel_Version from Panel column.
#'
#' @param df A tibble containing the column \var{Panel}.
#'
#' @return Same tibble as input with additional column \var{Panel_Version}.
#'
read_npx_wide_panel_version <- function(df) {

  # check input ----

  check_is_tibble(df = df,
                  error = TRUE)

  # check columns ----

  check_columns(df = df,
                col_list = list("Panel"))

  # extarct Panel_Version and modify Panel ----

  df |>
    # add Panel_Version
    dplyr::mutate(
      # if else allows us to have Panel_Version NA when the pattern (v.X) is not
      # present
      Panel_Version = dplyr::if_else(
        grepl(pattern = "(", x = .data[["Panel"]], fixed = TRUE),
        strsplit(x = .data[["Panel"]],
                 split = "(",
                 fixed = TRUE) |>
          lapply(utils::tail, 1L) |>
          unlist() |>
          (\(x) {
            sub(pattern = ")",
                replacement = "",
                x = x,
                fixed = TRUE)
          })(),
        NA_character_
      )
    ) |>
    # modify Panel
    dplyr::mutate(
      # if else allows us to have Panel unchanged when the pattern (v.X) is not
      # present
      Panel = dplyr::if_else(
        grepl(pattern = "(", x = .data[["Panel"]], fixed = TRUE),
        strsplit(x = .data[["Panel"]],
                 split = "(",
                 fixed = TRUE) |>
          lapply(utils::head, -1L) |>
          lapply(paste, collapse = "(") |>
          unlist(),
        .data[["Panel"]]
      )
    )
}

#' Combine top and middle matrices in long format.
#'
#' @description
#' Combined corresponding chunks of columns from the top and middle matrix
#' that were computed from \code{\link{read_npx_wide_top}} and
#' \code{\link{read_npx_wide_middle}}, respectively.
#'
#' @author
#'   Klev Diamanti
#'
#' @param df_top_list List of data frames from the top matrix. Output of
#' function \code{\link{read_npx_wide_top}}.
#' @param df_middle_list List of data frames from the middle matrix. Output of
#' function \code{\link{read_npx_wide_middle}}.
#' @param data_type Quantification method of the input data. One of
#' `r accepted_olink_platforms$quant_method |> unlist() |> unique() |> sort() |> cli::ansi_collapse(last = " or ")`. # nolint
#' @param format_spec A tibble derived from \var{olink_wide_spec} in the local
#' environment containing the expected format of the Olink wide file based on
#' the \var{olink_platform} and \var{data_type}.
#'
#' @return Tibble in long format combining the top and middle matrices.
#'
#' @seealso
#'   \code{\link{read_npx_wide_top}}
#'   \code{\link{read_npx_wide_middle}}
#'
red_npx_wide_top_mid_long <- function(df_top_list,
                                      df_middle_list,
                                      data_type,
                                      format_spec) {
  # check input ----

  check_is_list(lst = df_top_list)

  sapply(df_top_list, check_is_tibble, error = TRUE)

  check_is_list(lst = df_middle_list)

  sapply(df_middle_list, check_is_tibble, error = TRUE)

  check_is_tibble(df = format_spec,
                  error = TRUE)

  # prepare components of long df ----

  ## df oid ----

  df_long <- df_middle_list$df_mid_oid |>
    dplyr::left_join(
      df_top_list$df_top_oid,
      by = "col_index",
      relationship = "many-to-one"
    ) |>
    read_npx_wide_panel_version()

  ## df internal controls ----

  if ("df_mid_int_ctrl" %in% names(df_middle_list)
      && "df_top_int_ctrl" %in% names(df_top_list)) {

    df_int_ctrl <- df_middle_list$df_mid_int_ctrl |>
      dplyr::left_join(
        df_top_list$df_top_int_ctrl,
        by = "col_index",
        relationship = "many-to-one"
      ) |>
      read_npx_wide_panel_version()

    df_long <- df_long |>
      dplyr::bind_rows(
        df_int_ctrl
      )
    rm(df_int_ctrl)
  }

  ## df plate id ----

  df_plate <- df_middle_list$df_mid_plate |>
    dplyr::left_join(
      df_top_list$df_top_plate,
      by = "col_index",
      relationship = "many-to-one"
    ) |>
    dplyr::select(
      -dplyr::any_of(c("Var", "Unit", "col_index"))
    ) |>
    read_npx_wide_panel_version() |>
    dplyr::select(
      -dplyr::all_of("Panel_Version")
    )

  df_long <- df_long |>
    dplyr::left_join(
      df_plate,
      by = c("SampleID", "rid", "Panel"),
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
      ) |>
      read_npx_wide_panel_version() |>
      dplyr::select(
        -dplyr::all_of("Panel_Version")
      )

    df_long <- df_long |>
      dplyr::left_join(
        df_qc_warn,
        by = c("SampleID", "rid", "Panel"),
        relationship = "many-to-one"
      )
    rm(df_qc_warn)
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
        id_cols = dplyr::all_of(c("SampleID", "rid", "Panel")),
        names_from = dplyr::all_of("Uniprot ID"),
        values_from = dplyr::all_of(data_type)
      ) |>
      read_npx_wide_panel_version() |>
      dplyr::select(
        -dplyr::all_of("Panel_Version")
      )

    df_long <- df_long |>
      dplyr::left_join(
        df_dev_int_ctrl,
        by = c("SampleID", "rid", "Panel"),
        relationship = "many-to-one"
      )
    rm(df_dev_int_ctrl)
  }

  # remove rid ----

  df_long <- df_long |>
    dplyr::select(
      -dplyr::all_of("rid")
    )

  # return ----

  return(df_long)
}

#' Additional checks of the bottom matrix of Olink dataset in wide format.
#'
#' @description
#' The rows included in the bottom matrix have evolved through the years. For us
#' to be able to support as many such versions as possible we have used the
#' local environment variable \var{olink_wide_bottom_matrix} to mark these
#' different versions. This function extract these version and allows us to
#' check the validity of the data.
#'
#' @author
#'   Klev Diamanti
#'
#' @param df Bottom matrix of Olink dataset in wide format \var{df_bottom}.
#' @param file Path to Olink software output file in wide or long format.
#' Expecting file extensions
#' `r cli::ansi_collapse(x = accepted_npx_file_ext[grepl("excel|delim", names(accepted_npx_file_ext))], last = " or ")`. # nolint
#' @param data_type Quantification method of the input data. One of
#' `r accepted_olink_platforms$quant_method |> unlist() |> unique() |> sort() |> cli::ansi_collapse(last = " or ")`. # nolint
#' @param olink_platform Olink platform used to generate the input file.
#' One of
#' `r accepted_olink_platforms |> dplyr::filter(.data[["broader_platform"]] == "qPCR") |> dplyr::pull(.data[["name"]]) |> cli::ansi_collapse(last = " or ")`. # nolint
#'
#' @return Tibble with the bottom matrix specifications for the Olink wide file.
#'
#' @seealso
#'   \code{\link{read_npx_wide_bottom}}
#'
read_npx_wide_bottom_version <- function(df,
                                         file,
                                         data_type,
                                         olink_platform) {

  # extract all possible variable names from the global matrix
  format_spec_bottom <- olink_wide_bottom_matrix |>
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

  # list with all possible combinations
  format_spec_bottom <- list_bottom_v

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

  names_in_v1 <- lapply(format_spec_bottom, function(.x) {
    .x |>
      dplyr::mutate(
        total_n = dplyr::n(),
        true_n = sum(.data[["in_df"]])
      ) |>
      dplyr::select(
        dplyr::all_of(c("total_n", "true_n"))
      ) |>
      dplyr::distinct()
  }) |>
    dplyr::bind_rows(
      .id = "combo"
    ) |>
    dplyr::filter(
      .data[["total_n"]] == .data[["true_n"]]
    ) |>
    dplyr::arrange(
      dplyr::desc(.data[["total_n"]])
    ) |>
    dplyr::slice_head(
      n = 1L
    ) |>
    dplyr::pull(
      .data[["combo"]]
    )

  # if none or multiple combinations match
  # or, if df$V1 is a superset of the expected columns
  if (length(names_in_v1) != 1L
      || nrow(format_spec_bottom[[names_in_v1]]) != length(unique(df$V1))) {

    bottom_mat_v1_expected <- sapply( # nolint
      format_spec_bottom,
      function(x) {
        sapply(x$variable_alt_names, utils::head, 1L) |>
          cli::ansi_collapse() |>
          (\(.x) paste("*", .x))()
      }
    )

    cli::cli_abort(
      message = c(
        "x" = "Unexpected values in column 1 of the bottom matrix with QC data
        in file {.file {file}}.",
        "Expected one of the combos:",
        bottom_mat_v1_expected,
        "i" = "Has the file been modified manually?"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

  format_spec_bottom[[names_in_v1]] <- format_spec_bottom[[names_in_v1]] |>
    dplyr::mutate(
      version = .env[["names_in_v1"]]
    )

  return(format_spec_bottom[[names_in_v1]])
}

#' Convert the bottom matrix from Olink dataset in wide format to long.
#'
#' @description
#' Use chunks of columns from \code{\link{read_npx_wide_top}} to covert the
#' bottom matrix \var{df_bottom} into a long format tibble.
#'
#' @author
#'   Klev Diamanti
#'
#' @param df Bottom matrix of Olink dataset in wide format \var{df_bottom}.
#' @param file Path to Olink software output file in wide or long format.
#' Expecting file extensions
#' `r cli::ansi_collapse(x = accepted_npx_file_ext[grepl("excel|delim", names(accepted_npx_file_ext))], last = " or ")`. # nolint
#' @param olink_platform Olink platform used to generate the input file.
#' One of
#' `r accepted_olink_platforms |> dplyr::filter(.data[["broader_platform"]] == "qPCR") |> dplyr::pull(.data[["name"]]) |> cli::ansi_collapse(last = " or ")`. # nolint
#' @param data_type Quantification method of the input data. One of
#' `r accepted_olink_platforms$quant_method |> unlist() |> unique() |> sort() |> cli::ansi_collapse(last = " or ")`. # nolint
#' @param col_names Names list of character vectors containing column names from
#' each chunk of columns \var{df_top} was split on in function.
#' @param format_spec A tibble derived from \var{olink_wide_spec} in the local
#' environment containing the expected format of the Olink wide file based on
#' the \var{olink_platform} and \var{data_type}.
#' @param df_plate_panel Tibble with unique combinations of panels and plates
#' from the combination of top and middle data frames.
#'
#' @return A tibble with the bottom matrix of an Olink wide file in long format.
#'
#' @seealso
#'   \code{\link{read_npx_wide}}
#'   \code{\link{read_npx_wide_split_row}}
#'   \code{\link{read_npx_wide_npxs_version}}
#'   \code{\link{read_npx_wide_top}}
#'   \code{\link{read_npx_wide_middle}}
#'
read_npx_wide_bottom <- function(df,
                                 file,
                                 olink_platform,
                                 data_type,
                                 col_names,
                                 format_spec,
                                 df_plate_panel) {
  # check input ----

  check_is_tibble(df = df,
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

  check_is_tibble(df = format_spec,
                  error = TRUE)

  check_is_tibble(df = df_plate_panel,
                  error = TRUE)

  # get first column options ----

  # clean up format_spec_bottom for downstream use
  format_spec_bottom_df <- read_npx_wide_bottom_version(
    df = df,
    file = file,
    data_type = data_type,
    olink_platform = olink_platform
  ) |>
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
          "i" = "Has the file been modified manually?"
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
