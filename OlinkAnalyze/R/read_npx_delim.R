#' Help function to read `r ansi_collapse_quot(get_olink_data_types())` data
#' from delimited Olink software output files in R.
#'
#' @description
#' The function can handle delimited files in long and wide format.
#'
#' @author
#'   Klev Diamanti;
#'   Christoffer Cambronero;
#'   Kathleen Nevola
#'
#' @param file Path to Olink software output delimited file in wide or long
#' format. Expecting file extensions
#' `r get_file_ext(name_sub = "delim") |> ansi_collapse_quot()`.
#' @param out_df The class of output data frame. One of
#' `r ansi_collapse_quot(read_npx_df_output)`.
#'
#' @return `r ansi_collapse_quot(x = get_df_output_print(), sep = "or")` with
#' Olink data in long or wide format.
#'
#' @seealso
#'   \code{\link{read_npx}}
#'   \code{\link{read_npx_parquet}}
#'   \code{\link{read_npx_zip}}
#'   \code{\link{read_npx_excel}}
#'   \code{\link{read_npx_format}}
#'
read_npx_delim <- function(file,
                           out_df = "arrow") {

  # check input ----

  # check if file exists
  check_file_exists(file = file,
                    error = TRUE)

  # check that the requested output df is ok
  check_out_df_arg(out_df = out_df)

  # read file ----

  # using two tryCatch to check if file can be read as long format
  df_olink <- tryCatch(
    {
      data.table::fread(
        file = file,
        header = TRUE,
        stringsAsFactors = FALSE,
        na.strings = c("NA", ""),
        check.names = FALSE,
        data.table = FALSE
      ) |>
        dplyr::as_tibble()
    }, error = function(msg) {
      cli::cli_abort( # nolint return_linter
        c(
          "x" = "Unable to open delimited file: {.file {file}}!",
          "i" = "Check if {.arg file} is delimited or corrupt."
        ),
        call = rlang::caller_env(),
        wrap = FALSE
      )
    }
  )

  # if long has only the first two columns not matching VX, where X is
  # the column name, then then file is in wide format
  if (identical(x = utils::tail(x = names(df_olink), n = -2L),
                y = utils::tail(x = paste0("V", seq_len(ncol(df_olink))),
                                n = -2L))
      && nrow(df_olink) > 2L) {
    df_olink <- data.table::fread(
      file = file,
      header = FALSE,
      stringsAsFactors = FALSE,
      na.strings = c("NA", ""),
      check.names = FALSE,
      data.table = FALSE
    ) |>
      dplyr::as_tibble()
    names(df_olink) <- paste0("V", seq_len(ncol(df_olink)))
  }

  if (ncol(df_olink) == 1L) {
    cli::cli_warn(
      message = c(
        "i" = "The delimited file {.file {file}} has only one column!"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )
  }

  # top row is as expected for the corresponding format

  read_npx_format_colnames(df = df_olink, file = file)

  # convert df class ----

  # if needed convert the object to the requested output
  df_olink <- convert_read_npx_output(df = df_olink,
                                      out_df = out_df)

  # return ----

  return(df_olink)
}

#' @rdname read_npx_delim
read_npx_csv <- read_npx_delim
