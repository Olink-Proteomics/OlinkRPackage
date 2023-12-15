#' Help function to determine the number of rows with assay information in Olink
#' excel long or wide format files.
#'
#' @author
#'   Klev Diamanti
#'
#' @param file The input excel file.
#' @param olink_platform The Olink platform used to generate the input file.
#' Expecting "Target 96", "Target 48", "Flex" or "Focus".
#' @param data_type The quantification in which the data comes in. Expecting one
#' of NPX, Quantified or Ct.
#'
#' @return Integer number of rows containing info about assays in Olink excel
#' wide files.
#'
read_npx_wide_assay_nrow <- function(file,
                                     olink_platform = NULL,
                                     data_type = NULL) {

  # initial checks ----
  check_file_exists(file = file,
                    error = TRUE)

  # expected number of assay rows ----

  # number of rows containing metadata about assays and panels based on
  # the quantification method.
  # If relative quantification (NPX, Ct) we expect 4 rows
  # If absolute quantification (Quantified) we expect 5 rows
  n_max_meta_data_expected <- accepted_olink_platforms |>
    dplyr::filter(
      .data[["name"]] == .env[["olink_platform"]]
    ) |>
    dplyr::select(
      dplyr::all_of(c("quant_method", "quant_type"))
    ) |>
    tidyr::unnest(
      cols = dplyr::everything()
    ) |>
    dplyr::filter(
      .data[["quant_method"]] == .env[["data_type"]]
    ) |>
    dplyr::pull(
      .data[["quant_type"]]
    ) |>
    (\(x) ifelse(x == "relative", 4L, 5L))()

  # number of assay rows in file ----

  # read the entire wide excel file
  # we want to identify the first row with all values NA
  df <-  readxl::read_excel(path = file,
                            col_names = FALSE,
                            .name_repair = "minimal")
  colnames(df) <- paste0("V", seq_len(ncol(df)))

  # Use the rows full of NAs in the excel file to compute the number of rows
  # that contain data about assays.
  n_max_meta_data <- df |>
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
    # keep only the first instance marking the end of assay metadata and
    # sample measurements
    dplyr::slice_head(
      n = 1L
    ) |>
    # extract only the row_number as this is the index we are interested in
    dplyr::pull(
      .data[["row_number"]]
    ) |>
    # subtract 3 based on the following
    # 2 for the very first two rows that mark software version and quant method
    # 1 row_number is exactly the next column of the one we want to extract
    (\(x) x - 3L)()

  # checks ----

  # check that there are nover different number of rows
  accepted_assay_row_num <- c(4L, 5L)
  if (!(n_max_meta_data %in% accepted_assay_row_num)) {

    cli::cli_abort(
      message = c(
        "x" = "We identified {n_max_meta_data} rows containing data about assays
        in file {.file {file}} while we expected
        { cli::ansi_collapse(x = accepted_assay_row_num,
                             sep = \", \",
                             last = \" or \") }",
        "i" = "Has the excel file been modified manually?"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

  # error if the calculated value in less than 1
  if (n_max_meta_data != n_max_meta_data_expected) {

    cli::cli_abort(
      message = c(
        "x" = "We identified {n_max_meta_data} rows containing data about assays
        in {.file {file}} while we expected {n_max_meta_data_expected}!",
        "i" = "Has the excel file been modified manually?"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

  # return ----

  return(n_max_meta_data)

}
