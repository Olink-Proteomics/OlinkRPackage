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

  # check top df ----

  read_npx_wide_check_top(
    df = df_split$df_top,
    file = file,
    data_type = data_type
  )

  # get x-axis split index ----

  col_index_split <- read_npx_wide_split_col(df = df_split$df_top, file = file)

  # split top df ----

  df_top_list <- read_npx_wide_top_split(df = df_split$df_top,
                                         file = file,
                                         data_type = data_type,
                                         olink_platform = olink_platform)

  df_top_list

  # bottom df to long ----

  read_npx_wide_bottom_t(df = df_split$df_bottom,
                         file = file,
                         data_type = data_type,
                         col_split = paste0("V", col_index_split))

  # if (data_type != "Ct"
  #     & length(df_split) == 3L) {
  #
  #   # check df_split$df_split
  #
  # }

  # function vector of SampleID
  # function vector of OlinkID
  # function df_mid meta

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

  # check that number of rows is expected
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
  if (!identical(dplyr::pull(df, 1L), accepted_vals_v1)) {

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

#' Help function to determine the number of columns with assay measurements in
#' Olink excel wide format files.
#'
#' @author Klev Diamanti
#'
#' @param df The top data frame from a split Olink wide excel file.
#' @param file The input excel file.
#'
#' @return The index at which the columns of the Olink wide file are split into
#' the assay measurements and other metadata.
#'
read_npx_wide_split_col <- function(df,
                                    file) {

  # initial checks ----

  check_is_tibble(df = df,
                  error = TRUE)
  check_file_exists(file = file,
                    error = TRUE)

  # identify x axis split index ----

  df_t <- t(df) # transpose the wide df
  colnames(df_t) <- df_t[1, ] # add colnames
  df_t <- df_t |>
    dplyr::as_tibble() |>
    dplyr::slice(
      2L:dplyr::n()
    )

  # check that the transposed dataset contains column "Assay"
  check_columns(df = df_t,
                col_list = list("Assay"))

  # identify the split index
  col_index_split <- df_t |>
    dplyr::mutate(
      row_index = dplyr::row_number() + 1L
    ) |>
    dplyr::filter(
      .data[["Assay"]] == "Plate ID"
    ) |>
    dplyr::slice_head(
      n = 1L
    ) |>
    dplyr::pull(
      .data[["row_index"]]
    )

  # check ----

  # no rows contained OlinkID == NA
  if (identical(col_index_split, integer(0L))) {

    cli::cli_abort(
      message = c(
        "x" = "Unexpected format of the top metadata in file {.file {file}}!",
        "i" = "Expected to identify rows with {.arg OlinkID} = {.val NA}. Found
        0 such cases."
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

  # return ----

  return(col_index_split)

}

#' Help function that splits df_top from a Olink wide excel file into 3 data
#' frames.
#'
#' @param df The top data frame from a split Olink wide excel file.
#' @param file The input excel file.
#' @param data_type The quantification in which the data comes in. Expecting one
#' of NPX, Quantified or Ct.
#' @param olink_platform The Olink platform used to generate the input file.
#' Expecting "Target 96", "Target 48", "Flex" or "Focus".
#'
#' @return List of the data frames (df_oid, df_meta and df_qc_dev) that df_top
#' is split on.
#'
read_npx_wide_top_split <- function(df,
                                    file,
                                    data_type,
                                    olink_platform) {

  # check input ----

  check_is_data_frame(df = df,
                      error = TRUE)

  check_file_exists(file = file,
                    error = TRUE)

  check_olink_data_type(x = data_type,
                        broader_platform = "qPCR")

  check_olink_platform(x = olink_platform,
                       broader_platform = "qPCR")

  # transpose df to long ----

  df_t <- t(df) # transpose the wide df
  colnames(df_t) <- df_t[1, ] # add colnames
  df_t <- df_t |>
    dplyr::as_tibble() |>
    dplyr::slice(
      2L:dplyr::n()
    ) |>
    dplyr::mutate(
      # column 1 became colnames of df_t
      col_index = dplyr::row_number() + 1L,
      col_index = paste0("V", .data[["col_index"]])
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
  df_meta <- df_t |>
    dplyr::filter(
      .data[["Assay"]] %in% c("Plate ID", "QC Warning")
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

  if (nrow(df_t) != (nrow(df_oid) + nrow(df_meta) + nrow(df_qc_dev))) {

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
  if (olink_platform == "Target 96") {
    expected_num_assays <- 92L * length(unique(df_oid$Panel))
  } else if (olink_platform == "Target 48") {
    expected_num_assays <- 45L * length(unique(df_oid$Panel))
  } else {
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

  ## check df_meta ----

  # check that "Plate ID" is there in any file
  expected_meta_var <- c("Plate ID")
  # when the data_type is NPX or Quantified, "QC Warning" is also exepcted
  if (data_type != "Ct") {
    expected_meta_var <- c(expected_meta_var, "QC Warning")
  }

  if (!identical(dplyr::pull(df_meta, .data[["Var"]]) |> unique() |> sort(),
                 sort(expected_meta_var))) {

    cli::cli_abort(
      message = c(
        "x" = "Expected {.val {expected_meta_var}} in the right-hand side of the
        top matrix in file {.file {file}}, but detected
        {.val {dplyr::pull(df_meta, .data[[\"Var\"]]) |> unique()}}",
        "i" = "Has the excel file been modified manually?"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

  ## check df_qc_dev ----

  # make df_qc_dev NULL if it has no rows
  if (nrow(df_qc_dev) == 0L) {
    df_qc_dev <- NULL
  }

  # return ----

  return(
    list(
      df_oid = df_oid,
      df_meta = df_meta,
      df_qc_dev = df_qc_dev
    )
  )

}

#' Help function that converts df_bottom into long format.
#'
#' @param df The bottom data frame from a split Olink wide excel file.
#' @param file The input excel file.
#' @param data_type The quantification in which the data comes in. Expecting one
#' of NPX or Quantified.
#' @param col_split The column the splits the Olink wide excel file into left
#' and right.
#'
#' @return The bottom matrix in long format.
#'
read_npx_wide_bottom_t <- function(df,
                                   file,
                                   data_type,
                                   col_split) {
  # check input ----

  check_is_data_frame(df = df,
                      error = TRUE)

  check_file_exists(file = file,
                    error = TRUE)

  check_olink_data_type(x = data_type,
                        broader_platform = "qPCR")

  check_is_scalar_character(string = col_split,
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

  if (!identical(dplyr::pull(df, 1L) |> unique() |> sort(),
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

  # remove all NA columns ----

  df <- remove_all_na_cols(df = df)

  # per-plate metrics ----

  if (xor(col_split %in% colnames(df),
          data_type == "Quantified")) {
    # if only one of the above is true then throw and error as we expect either
    # both to be true or both to be false.

    cli::cli_abort(
      message = c(
        "x" = "The bottom matrix with the assays metrics in file {.file {file}}
        does not contain plate information!",
        "i" = "Has the excel file been modified manually?"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  } else if (col_split %in% colnames(df)
             && data_type == "Quantified") {
    # if it is Quantified data
    df_q <- df |>
      # keep only rows to be pivoted
      dplyr::filter(
        .data[["V1"]] %in% .env[["accepted_vals_q"]]
        & !is.na(.data[[col_split]])
      )

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

  # remove all NA columns
  df <- remove_all_na_cols(df = df)

  df_t <- t(df)
  colnames(df_t) <- df_t[1, ]
  df_t <- df_t |>
    dplyr::as_tibble(
      rownames = "col_index"
    ) |>
    dplyr::slice(
      2L:dplyr::n()
    )

  # checks ----

  ## rows of df_t and df_q match ----

  if (exists("df_q")) {

    if ((nrow(df_q) %% nrow(df_t)) != 0L) {

      cli::cli_abort(
        message = c(
          "x" = "Some full columns in the bottom matrix with assay metadata in
        file {.file {file}} do not contain any values!",
          "i" = "Has the excel file been modified manually?"
        ),
        call = rlang::caller_env(),
        wrap = FALSE
      )

    }

  }

  # return ----

  if (exists("df_q")) {

    df_t <- df_t |>
      dplyr::full_join(
        y = df_q,
        by = "col_index",
        relationship = "one-to-many"
      )

  }

  return(df_t)

}
