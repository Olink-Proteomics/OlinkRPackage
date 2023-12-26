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

  # split the file into sub- data frames ----

  df_split <- read_npx_wide_split_row(
    file = file,
    data_type = data_type
  )

  # top list of df to long ----

  df_top_list <- read_npx_wide_top_split(
    df = df_split$df_top,
    file = file,
    data_type = data_type,
    olink_platform = olink_platform
  )

  ## top list of df colnames ----

  # assay columns
  assay_cols <- df_top_list$df_oid |>
    dplyr::pull(
      .data[["col_index"]]
    )

  # plate columns
  plate_cols <- df_top_list$df_plate |>
    dplyr::pull(
      .data[["col_index"]]
    )

  # qc warning columns
  if (data_type != "Ct") {
    qc_warn_cols <- df_top_list$df_qc_warn |>
      dplyr::pull(
        .data[["col_index"]]
      )
  } else {
    qc_warn_cols <- NULL
  }

  # internal controls columns
  if (data_type == "Quantified"
      && !is.null(df_top_list$df_qc_dev)) {
    int_ctrl_cols <- df_top_list$df_qc_dev |>
      dplyr::pull(
        .data[["col_index"]]
      )
  } else {
    int_ctrl_cols <- NULL
  }

  # get col_split that splits left from right hand side matrix
  col_split <- df_top_list$df_plate |>
    dplyr::arrange(
      .data[["col_index"]]
    ) |>
    dplyr::slice_head(
      n = 1L
    ) |>
    dplyr::pull(
      .data[["col_index"]]
    )

  # bottom df to long ----

  if (data_type != "Ct"
      && length(df_split) == 3L) {

    df_bottom <- read_npx_wide_bottom( # nolint object_usage_linter
      df = df_split$df_bottom,
      file = file,
      data_type = data_type,
      col_split = col_split,
      assay_cols = assay_cols
    )

  }

  # middle list of df to long ----

  df_middle <- read_npx_wide_middle( # nolint object_usage_linter
    df = df_split$df_mid,
    file = file,
    data_type = data_type,
    assay_cols = assay_cols,
    plate_cols = plate_cols,
    qc_warn_cols = qc_warn_cols,
    int_ctrl_cols = int_ctrl_cols
  )

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
#'
#' @return A named list of 2 or 3 tibbles.
#'
read_npx_wide_split_row <- function(file,
                                    data_type) {

  # initial checks ----
  check_file_exists(file = file,
                    error = TRUE)

  check_olink_data_type(x = data_type,
                        broader_platform = "qPCR")

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
  if (!(length(na_row_index) %in% c(1L, 2L))) {

    cli::cli_abort(
      message = c(
        "x" = "We identified
        {ifelse(identical(na_row_index, integer(0L)), 0L, length(na_row_index))}
        rows with all columns `NA` in file {.file {file}} while we expected 1
        (for Ct data) or 2 (for NPX or Quantified data)",
        "i" = "Has the excel file been modified manually?"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

  # We want this to kick in only if there is only 1 row with all NA OR
  # data_type is Ct. This checks catches all possible scenarios:
  # NROW   data_type  Outcome
  #    1          Ct       OK
  #    1         NPX    ERROR
  #    1  Quantified    ERROR
  #    2          Ct    ERROR
  #    2         NPX       OK
  #    2  Quantified       OK
  if (xor(x = length(na_row_index) == 1L,
          y = data_type == "Ct")) {

    cli::cli_abort(
      message = c(
        "x" = "The file {.file {file}} contains {length(na_row_index)} row{?s}
        with all columns NA, but based on {.arg data_type} = {.val {data_type}}
        we expected {ifelse(data_type == \"Ct\", 1L, 2L)}.",
        "i" = "Has the excel file been modified manually?"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

  # check that rows are not consecutive
  if (length(na_row_index) == 2L
      && na_row_index[2L] - na_row_index[1L] < 2) {

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
      3L, # skip first two rows
      na_row_index + 1L # all NA rows +1
    ),
    end_row = c(
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
    names(list_df_split) <- c("df_top", "df_mid")
  } else {
    names(list_df_split) <- c("df_top", "df_mid", "df_bottom")
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
#' @param data_type The quantification in which the data comes in. Expecting one
#' of NPX, Quantified or Ct.
#'
#' @return An error inconsistency is spotted
#'
read_npx_wide_check_top <- function(df,
                                    file,
                                    data_type) {

  # initial checks ----

  check_is_tibble(df = df,
                  error = TRUE)

  check_file_exists(file = file,
                    error = TRUE)

  check_olink_data_type(x = data_type,
                        broader_platform = "qPCR")

  # expected number of assay rows ----

  # number of rows containing metadata about assays and panels based on
  # the quantification method in the top dataset:
  # If relative quantification (NPX, Ct) we expect 4 rows
  # If absolute quantification (Quantified) we expect 5 rows
  n_top_rows_expected <- accepted_olink_platforms |>
    dplyr::select(
      dplyr::all_of(
        c("quant_method", "quant_type")
      )
    ) |>
    tidyr::unnest(
      cols = dplyr::everything()
    ) |>
    dplyr::distinct() |>
    dplyr::filter(
      .data[["quant_method"]] == .env[["data_type"]]
    ) |>
    dplyr::pull(
      .data[["quant_type"]]
    ) |>
    (\(x) ifelse(x == "relative", 4L, 5L))()

  # checks ----

  # check that number of rows of the top matrix from the file is expected
  accepted_assay_row_num <- c(4L, 5L)
  if (!(nrow(df) %in% accepted_assay_row_num)) {

    cli::cli_abort(
      message = c(
        "x" = "We identified {nrow(df)} rows containing data about assays
        in file {.file {file}} while we expected
        { cli::ansi_collapse(x = accepted_assay_row_num,
                             sep = \", \",
                             last = \" or \") }!",
        "i" = "Has the excel file been modified manually?"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

  # check that expected and actual rows match
  if (nrow(df) != n_top_rows_expected) {

    cli::cli_abort(
      message = c(
        "x" = "We identified {nrow(df)} rows containing data about assays
        in {.file {file}} while we expected {n_top_rows_expected}!",
        "i" = "Has the excel file been modified manually?"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

  # check that column 1 contains the expected values
  accepted_vals_v1 <- c("Panel", "Assay", "Uniprot ID", "OlinkID")
  if (nrow(df) == 5L) {

    # in case of quantified data top df contains 1 additional row
    accepted_vals_v1 <- c(accepted_vals_v1, "Unit")

  }

  # check that df contains "V1"
  check_columns(df = df, col_list = list("V1"))


  if (!identical(dplyr::pull(df, .data[["V1"]]), accepted_vals_v1)) {

    cli::cli_abort(
      message = c(
        "x" = "Column 1 of of the top matrix with assay metadata in file
        {.file {file}} does not contain the expected values:
        {accepted_vals_v1}",
        "i" = "Has the excel file been modified manually?"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

}

#' Help function that splits df_top from a Olink wide excel file into 3 data
#' frames.
#'
#' @author Klev Diamanti
#'
#' @param df The top data frame from a split Olink wide excel file.
#' @param file The input excel file.
#' @param data_type The quantification in which the data comes in. Expecting one
#' of NPX, Quantified or Ct.
#' @param olink_platform The Olink platform used to generate the input file.
#' Expecting "Target 96", "Target 48", "Flex" or "Focus".
#'
#' @return List of the data frames (df_oid, df_meta and df_qc_dev) in long
#' format that df_top is split on.
#'
read_npx_wide_top_split <- function(df,
                                    file,
                                    data_type,
                                    olink_platform) {

  # check input and top matrix ----

  check_olink_platform(x = olink_platform,
                       broader_platform = "qPCR")

  read_npx_wide_check_top(
    df = df,
    file = file,
    data_type = data_type
  )

  # transpose df to long ----

  df_t <- t(df) # transpose the wide df
  colnames(df_t) <- df_t[1, ] # add colnames
  df_t <- df_t |>
    dplyr::as_tibble(rownames = "col_index") |>
    dplyr::slice(
      2L:dplyr::n()
    )

  # check columns of df
  check_columns(df = df_t,
                col_list = list("Panel", "Assay", "Uniprot ID", "OlinkID"))

  # split df_t into its parts ----

  # data frame containing assay info
  # Panel, Assay, Uniprot ID and Olink ID (and Unit if data_type=Quantified)
  # index
  df_oid <- df_t |>
    dplyr::filter(
      !is.na(.data[["OlinkID"]])
    )

  # data frame containing assay info
  # Panel, Assay, Uniprot ID and Olink ID (and Unit if data_type=Quantified)
  df_plate <- df_t |>
    dplyr::filter(
      .data[["Assay"]] %in% c("Plate ID")
      & is.na(.data[["OlinkID"]])
    ) |>
    dplyr::select(
      -dplyr::all_of(c("OlinkID", "Uniprot ID"))
    ) |>
    dplyr::rename(
      "Var" = "Assay"
    )

  df_qc_warn <- df_t |>
    dplyr::filter(
      .data[["Assay"]] %in% c("QC Warning")
      & is.na(.data[["OlinkID"]])
    ) |>
    dplyr::select(
      -dplyr::all_of(c("OlinkID", "Uniprot ID"))
    ) |>
    dplyr::rename(
      "Var" = "Assay"
    )

  df_qc_dev <- df_t |>
    dplyr::filter(
      (.data[["Assay"]] == "QC Deviation from median"
       | grepl(pattern = "ctrl", x = .data[["Assay"]], ignore.case = TRUE))
      & is.na(.data[["OlinkID"]])
    ) |>
    dplyr::select(
      -dplyr::all_of(c("OlinkID"))
    )

  # checks ----

  ## check sum of rows ----

  if (nrow(df_t) != (nrow(df_oid) + nrow(df_plate) + nrow(df_qc_warn) + nrow(df_qc_dev))) { # nolint object_usage_linter

    cli::cli_abort(
      message = c(
        "x" = "The top matrix with the assays metadata in file {.file {file}}
        contains unexpected values!",
        "i" = "Has the excel file been modified manually?"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

  ## check df_oid ----

  # no NAs are allowed in df_oid
  if (any(is.na(df_oid))) {

    cli::cli_abort(
      message = c(
        "x" = "Detected {sum(is.na(df_oid))} empty cells in columns of file
        {.file {file}}. Expected no empty cells!",
        "i" = "Has the excel file been modified manually?"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

  # we will check number of assays only for Target 96 and 48 Olink platforms.
  # Other Olink platforms such as Flex and Focus allow a varying number of
  # assays, which does not allow us to check number of assays.
  expected_num_assays <- dplyr::tibble(platform = c("Target 96",
                                                    "Target 48"),
                                       n = c(92L,
                                             45L)) |>
    dplyr::mutate(
      total_n = .data[["n"]] * length(unique(df_oid$Panel))
    ) |>
    dplyr::filter(
      .data[["platform"]] == .env[["olink_platform"]]
    ) |>
    dplyr::pull(
      .data[["total_n"]]
    )
  if (identical(expected_num_assays, integer(0L))) {
    expected_num_assays <- NA_integer_
  }

  if (!is.na(expected_num_assays)
      && nrow(df_oid) != expected_num_assays) {

    cli::cli_abort(
      message = c(
        "x" = "Detected {nrow(df_oid)} assays in {length(unique(df_oid$Panel))}
        panels in file {.file {file}}, but expected {expected_num_assays}!",
        "i" = "Has the excel file been modified manually?"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

  ## check df_qc_warn ----

  # when data_type = Ct we do not expect QC Warning
  if (xor(x = data_type == "Ct",
          y = nrow(df_qc_warn) == 0L)) {

    cli::cli_abort(
      message = c(
        "x" = "Column \"QC Warning\" in the right-hand side of the top matrix
        is expected only for \"NPX\" and \"Quantified\" data!",
        "i" = "Has the excel file been modified manually?"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

  ## check df_plate & df_qc_warn ----

  # when the data_type is NPX or Quantified, "QC Warning" is also expected
  if (data_type != "Ct") {

    if (!identical(dim(df_plate), dim(df_qc_warn))) {

      cli::cli_abort(
        message = c(
          "x" = "Expected equal number of \"Plate ID\" and \"QC\ Warning\"
          columns in the right-hand side of the top matrix in file
          {.file {file}}!",
          "i" = "Has the excel file been modified manually?"
        ),
        call = rlang::caller_env(),
        wrap = FALSE
      )

    }

  }

  ## check df_qc_dev ----

  # only when the data_type is Quantified we expect internal controls
  if (data_type != "Quantified"
      && nrow(df_qc_dev) != 0L) {

    cli::cli_abort(
      message = c(
        "x" = "Columns for \"Internal controls\" in the right-hand side of the
        top matrix are expected only for \"Quantified\" data!",
        "i" = "Has the excel file been modified manually?"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

  # return ----

  list_df <- list(
    df_oid = df_oid,
    df_plate = df_plate
  )

  # add df_qc_warn if not empty
  if (nrow(df_qc_warn) != 0L) {
    list_df <- append(x = list_df,
                      values = list(df_qc_warn = df_qc_warn))
  }

  # add df_qc_dev if not empty
  if (nrow(df_qc_warn) != 0L) {
    list_df <- append(x = list_df,
                      values = list(df_qc_dev = df_qc_dev))
  }

  return(list_df)

}

#' Help function that converts df_bottom into long format.
#'
#' @author Klev Diamanti
#'
#' @param df The bottom data frame from a split Olink wide excel file.
#' @param file The input excel file.
#' @param data_type The quantification in which the data comes in. Expecting one
#' of NPX or Quantified.
#' @param col_split The name of the column that splits the Olink wide excel file
#' into left (assay info) and right  hand side (PlateID and QC_Warning info).
#' @param assay_cols Character vector with the names of the columns containing
#' Olink assays.
#'
#' @return The bottom matrix in long format.
#'
read_npx_wide_bottom <- function(df,
                                 file,
                                 data_type,
                                 col_split,
                                 assay_cols) {
  # check input ----

  check_is_data_frame(df = df,
                      error = TRUE)

  check_file_exists(file = file,
                    error = TRUE)

  check_olink_data_type(x = data_type,
                        broader_platform = "qPCR")

  check_is_scalar_character(string = col_split,
                            error = TRUE)

  check_is_character(string = assay_cols,
                     error = TRUE)

  # check if Ct ----

  if (data_type == "Ct") {

    cli::cli_abort(
      message = c(
        "x" = "The Olink wide excel file {.file {file}} contains a bottom
        matrix. Files with {.arg data_type} = {.val {data_type}} should not have
        one!",
        "i" = "Has the excel file been modified manually?"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

  # check first column ----

  # check that column "V1" exists in the df
  check_columns(df = df, col_list = list("V1"))

  accepted_vals_v1 <- c("Missing Data freq.", "Normalization")
  if (data_type == "NPX") {
    accepted_vals_q <- character(0)
    accepted_vals_v1 <- c(accepted_vals_v1, "LOD")
  } else {
    accepted_vals_q <- c("Assay warning", "Lowest quantifiable level",
                         "Plate LOD", "LLOQ", "ULOQ")
    accepted_vals_v1 <- c(accepted_vals_v1,
                          accepted_vals_q)
  }

  if (!identical(dplyr::pull(df, .data[["V1"]]) |> unique() |> sort(),
                 accepted_vals_v1 |> sort())) {

    cli::cli_abort(
      message = c(
        "x" = "Column 1 of the bottom matrix with assay metadata in file
        {.file {file}} contains unexpected values. Expected:
        {accepted_vals_v1}",
        "i" = "Has the excel file been modified manually?"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

  # keep necessary columns ----

  # columns expected to be present in the df
  expected_cols <- c("V1", assay_cols)
  if (data_type == "Quantified") {
    expected_cols <- c(expected_cols, col_split)
  }
  # check that columns in expected_cols exist in the df
  check_columns(df = df, col_list = as.list(expected_cols))

  df <- df |>
    # keep only columns absolutely required:
    # V1 that contains names of variables
    # assay_cols that contains info for customer assays only
    # col_split the column that might contain plate names in case of Quantified
    dplyr::select(
      dplyr::all_of(expected_cols)
    ) |>
    # The col_split was selected but it contains only NA values the remove it.
    # This might be the case in NPX files.
    remove_all_na_cols()

  # per-plate metrics ----

  if (data_type == "Quantified") {

    # if it is Quantified data
    df_q <- df |>
      # keep only rows to be pivoted
      dplyr::filter(
        .data[["V1"]] %in% .env[["accepted_vals_q"]]
        & !is.na(.data[[col_split]])
      )

    # check that each row with plate-specific QC metrics contains the same
    # number of plate
    df_q_n_r <- df_q |>
      dplyr::count(
        .data[[col_split]],
        name = "n_plate"
      ) |>
      dplyr::pull(
        .data[["n_plate"]]
      ) |>
      unique() |>
      length()

    if (df_q_n_r != 1L) {

      cli::cli_abort(
        message = c(
          "x" = "Column 1 of the bottom matrix contains uneven rows of plates
          and plate-specific QC measurements in file {.file {file}}!",
          "i" = "Has the excel file been modified manually?"
        ),
        call = rlang::caller_env(),
        wrap = FALSE
      )

    }

    # for each variable in V1 and do a pivot_longer
    df_q <- lapply(unique(df_q$V1),
                   function(x) {
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

  }

  # across plates metrics ----

  # remove rows processed earlier
  # these rows are now columns df_q
  if (exists("df_q")) {

    df <- df |>
      dplyr::filter(
        !(.data[["V1"]] %in% colnames(df_q))
      ) |>
      dplyr::select(
        -dplyr::all_of(col_split)
      )

  }

  df_t <- t(df)
  colnames(df_t) <- df_t[1, ]
  df_t <- df_t |>
    dplyr::as_tibble(
      rownames = "col_index"
    ) |>
    dplyr::slice(
      2L:dplyr::n()
    )

  # return ----

  if (exists("df_q")) {

    df_long <- df_t |>
      dplyr::full_join(
        y = df_q,
        by = "col_index",
        relationship = "one-to-many"
      )

  } else {

    df_long <- df_t

  }

  return(df_long)

}

#' Help function to check df_mid for potential inconsistencies.
#'
#' @author Klev Diamanti
#'
#' @param df The middle data frame from a split Olink wide excel file.
#' @param file The input excel file.
#' @param data_type The quantification in which the data comes in. Expecting one
#' of NPX or Quantified.
#' @param assay_cols Character vector with the names of the columns containing
#' Olink assays.
#' @param plate_cols Character vector with the names of the columns containing
#' "Plate ID".
#' @param qc_warn_cols Character vector with the names of the columns containing
#' "QC Warning".
#' @param int_ctrl_cols Character vector with the names of the columns
#' containing "Internal Controls".
#'
#' @return Nothing or an error if inconsistencies are detected.
#'
read_npx_wide_check_middle <- function(df,
                                       file,
                                       data_type,
                                       assay_cols,
                                       plate_cols,
                                       qc_warn_cols,
                                       int_ctrl_cols) {
  # check input ----

  check_is_data_frame(df = df,
                      error = TRUE)

  check_file_exists(file = file,
                    error = TRUE)

  check_olink_data_type(x = data_type,
                        broader_platform = "qPCR")

  if (!is.null(assay_cols)) {
    check_is_character(string = assay_cols,
                       error = TRUE)
  }

  if (!is.null(plate_cols)) {
    check_is_character(string = plate_cols,
                       error = TRUE)
  }

  if (!is.null(qc_warn_cols)) {
    check_is_character(string = qc_warn_cols,
                       error = TRUE)
  }

  if (!is.null(int_ctrl_cols)) {
    check_is_character(string = int_ctrl_cols,
                       error = TRUE)
  }

  # check matches of data_type to input ----

  # creating a control string from bits of the input flags.
  # This was done to lower the complexity of the function.
  assay_str <- ifelse(is.null(assay_cols), "0", "1")
  plate_id_str <- ifelse(is.null(plate_cols), "0", "1")
  qc_warn_str <- ifelse(is.null(qc_warn_cols), "0", "1")
  int_sctrl_str <- ifelse(is.null(int_ctrl_cols), "0", "1")
  cols_str <- paste0(assay_str, plate_id_str, qc_warn_str, int_sctrl_str)

  if (data_type == "Ct"
      && cols_str != "1100") {
    # when data_type is Ct then assay_cols and plate_cols are required to have
    # values.
    # qc_warn_cols and int_ctrl_cols should be NULL.

    cli::cli_abort(
      message = c(
        "x" = "The middle matrix of the Olink wide excel file {.file {file}}
        contains unexpected components. Files with {.arg data_type} =
        {.val {data_type}} should have assay measurements and plate id!",
        "i" = "Has the excel file been modified manually?"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  } else if (data_type == "NPX"
             && cols_str != "1110") {
    # when data_type is NPX then assay_cols, plate_cols and qc_warn_cols are
    # required to have values.
    # int_ctrl_cols should be NULL.

    cli::cli_abort(
      message = c(
        "x" = "The middle matrix of the Olink wide excel file {.file {file}}
        contains unexpected components. Files with {.arg data_type} =
        {.val {data_type}} should have assay measurements, plate id and QC
        warning!",
        "i" = "Has the excel file been modified manually?"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  } else if (data_type == "Quantified"
             && !(cols_str %in% c("1110", "1111"))) {
    # when data_type is NPX then assay_cols, pid_cols and qc_warn_cols are
    # required to have values.
    # int_ctrl_cols may be NULL or contain values.

    cli::cli_abort(
      message = c(
        "x" = "The middle matrix of the Olink wide excel file {.file {file}}
        contains unexpected components. Files with {.arg data_type} =
        {.val {data_type}} should have assay measurements, plate id and QC
        warning, and, not necessarily, internal controls!",
        "i" = "Has the excel file been modified manually?"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

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
#' @param assay_cols Character vector with the names of the columns containing
#' Olink assays.
#' @param plate_cols Character vector with the names of the columns containing
#' "Plate ID".
#' @param qc_warn_cols Character vector with the names of the columns containing
#' "QC Warning".
#' @param int_ctrl_cols Character vector with the names of the columns
#' containing "Internal Controls".
#'
#' @return A list of data frames (df_oid, df_pid, df_qc_warn and df_int_ctrl) in
#' long format from the middle matrix of the Olink wide excel file.
#'
read_npx_wide_middle <- function(df,
                                 file,
                                 data_type,
                                 assay_cols,
                                 plate_cols,
                                 qc_warn_cols,
                                 int_ctrl_cols) {

  # check input and middle matrix ----

  read_npx_wide_check_middle(df = df,
                             file = file,
                             data_type = data_type,
                             assay_cols = assay_cols,
                             plate_cols = plate_cols,
                             qc_warn_cols = qc_warn_cols,
                             int_ctrl_cols = int_ctrl_cols)

  # split datasets ----

  ## split assays and pivot to longer ----

  # check all columns exist
  check_columns(df = df, col_list = as.list(assay_cols))

  df_oid <- df |>
    dplyr::select(
      dplyr::all_of(c("V1", assay_cols))
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

  # check all columns exist
  check_columns(df = df, col_list = as.list(plate_cols))

  df_pid <- df |>
    dplyr::select(
      dplyr::all_of(c("V1", plate_cols))
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
  if (!is.null(qc_warn_cols)) {

    # check all columns exist
    check_columns(df = df, col_list = as.list(qc_warn_cols))

    df_qc_warn <- df |>
      dplyr::select(
        dplyr::all_of(c("V1", qc_warn_cols))
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
  if (!is.null(int_ctrl_cols)) {

    # check all columns exist
    check_columns(df = df, col_list = as.list(int_ctrl_cols))

    df_int_ctrl <- df |>
      dplyr::select(
        dplyr::all_of(c("V1", int_ctrl_cols))
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
  if (!is.null(qc_warn_cols)) {

    if (nrow(df_pid) != nrow(df_qc_warn)) {

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

  ## check correct number of internal controls ----

  if (!is.null(int_ctrl_cols)) {

    # identify number of internal control assays
    int_ctrl_num <- c(3L, 2L, 1L)
    mod_int_ctrl <- (length(int_ctrl_cols) / length(plate_cols)) %% int_ctrl_num
    names(mod_int_ctrl) <- int_ctrl_num
    n_int_ctrl <- utils::head(x = mod_int_ctrl[mod_int_ctrl == 0L],
                              n = 1L) |>
      names() |>
      as.integer()
    # if none of the modulos works assume 1 internal control
    n_int_ctrl <- ifelse(identical(n_int_ctrl, integer(0L)), 1L, n_int_ctrl)

    # number of unique samples
    n_uniq_sample <- dplyr::pull(df, .data[["V1"]]) |> unique() |> length()

    # check
    if (nrow(df_int_ctrl) != (n_uniq_sample * n_int_ctrl * length(plate_cols))) { # nolint object_usage_linter

      cli::cli_abort(
        message = c(
          "x" = "Uneven number of entries of \"Internal Control\" assays in
          the middle matrix of the Olink wide excel file {.file {file}}!",
          "i" = "Has the excel file been modified manually?"
        ),
        call = rlang::caller_env(),
        wrap = FALSE
      )

    }

  }

  # return ----

  list_df <- list(
    df_oid = df_oid,
    df_pid = df_pid
  )

  if (!is.null(qc_warn_cols)) {
    list_df <- append(x = list_df,
                      values = list(df_qc_warn = df_qc_warn))
  }

  if (!is.null(int_ctrl_cols)) {
    list_df <- append(x = list_df,
                      values = list(df_int_ctrl = df_int_ctrl))
  }

  return(list_df)

}
