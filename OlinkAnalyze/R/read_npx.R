#' Read Olink data in R.
#'
#' @description
#' Imports a file exported from Olink software that quantifies protein levels in
#' NPX, Ct or absolute quantification.
#'
#' \strong{Note:} Do not modify the Olink software output file prior to
#' importing it with \code{\link{read_npx}} as it might fail.
#'
#' @author
#'   Klev Diamanti;
#'   Kathleen Nevola;
#'   Pascal Pucholt;
#'   Christoffer Cambronero;
#'   Boxi Zhang;
#'   Olof Mansson;
#'   Marianne Sandin
#'
#' @param filename Path to Olink software output file in wide or long format.
#' Expecting extensions `r get_file_ext_summary()`.
#' @param out_df The class of the output dataset. One of
#' `r ansi_collapse_quot(read_npx_df_output)`. (default = "tibble")
#' @param sep Character separator of delimited input file. One of `NULL`
#' (default) for auto-detection, or `r ansi_collapse_quot(accepted_field_sep)`.
#' Used only for delimited output files from Olink software.
#' @param long_format Boolean marking format of input file. One of `NULL`
#' (default) for auto-detection, or `TRUE` for long format and `FALSE` for
#' wide format files.
#' @param olink_platform Olink platform used to generate the input file.
#' One of `NULL` (default) for auto-detection,
#' or `r ansi_collapse_quot(x = accepted_olink_platforms$name)`.
#' @param data_type Quantification method of the input data. One of `NULL`
#' (default) for auto-detection, `r ansi_collapse_quot(get_olink_data_types())`.
#' @param .ignore_files Character vector of files included in the zip-compressed
#' Olink software output files that should be ignored. Used only for
#' zip-compressed input files (default = \emph{c("README.txt")}).
#' @param quiet Boolean to print a confirmation message when reading the input
#' file. Applies to excel or delimited input only. `TRUE` (default) to not print
#' and `FALSE` to print.
#' @param legacy Boolean to run the legacy version of the read_npx function.
#' \strong{Important: should be used only to wide format files from Target 96 or
#' Target 48 with NPX Software version earlier than 1.8!} (default `FALSE`).
#'
#' @return Dataset, `r get_df_output_print()`, with Olink data in long format.
#'
#' @keywords
#'   NPX;
#'   parquet;
#'   csv;
#'   zip;
#'   xlsx;
#'   Olink;
#'   Olink Explore;
#'   Olink Explore 1536;
#'   Olink Explore 3072;
#'   Olink Explore HT;
#'   Olink Target 96;
#'   Olink Target 48;
#'   Olink Flex;
#'   Olink Focus
#'
#' @export
#'
#' @examples
#' \donttest{
#' file <- system.file("extdata",
#'                     "npx_data_long_csv.csv",
#'                     package = "OlinkAnalyze")
#'
#' read_NPX(filename = file,
#'          sep = NULL)
#'
#' read_NPX(filename = file,
#'          sep = ";")
#' }
#'
read_npx <- function(filename,
                     out_df = "tibble",
                     sep = NULL,
                     long_format = NULL,
                     olink_platform = NULL,
                     data_type = NULL,
                     .ignore_files = c("README.txt"),
                     quiet = TRUE,
                     legacy = FALSE) {

  # check input ----

  # check if the input file exists
  check_file_exists(file = filename,
                    error = TRUE)

  # check that the requested putput df is ok
  check_out_df_arg(out_df = out_df)

  check_is_scalar_boolean(bool = legacy,
                          error = TRUE)

  # sep and .ignore_file are checked in the functions they target

  # check file extension ----

  # check what type of label the extension of the input matches to
  f_label <- check_file_extension(file = filename)

  # read data ----

  if (grepl(pattern = "excel|delim", x = f_label)) {
    # Input is an excel or a delimited file

    # Run legacy read_npx function
    if (legacy == TRUE) {

      df_olink <- read_npx_legacy(file = filename,
                                  out_df = out_df,
                                  olink_platform = olink_platform,
                                  data_type = data_type,
                                  quiet = quiet)

    } else {

      df_olink <- read_npx_format(file = filename,
                                  out_df = out_df,
                                  sep = sep,
                                  long_format = long_format,
                                  olink_platform = olink_platform,
                                  data_type = data_type,
                                  quiet = quiet,
                                  legacy = FALSE)

    }

  } else if (grepl(pattern = "parquet", x = f_label)) {

    # Input is a parquet file
    df_olink <- read_npx_parquet(file = filename)

  } else if (grepl(pattern = "compressed", x = f_label)) {

    # Input is a zip-compressed file
    df_olink <- read_npx_zip(
      file = filename,
      out_df = out_df,
      sep = sep,
      long_format = long_format,
      olink_platform = olink_platform,
      data_type = data_type,
      .ignore_files = .ignore_files,
      quiet = quiet
    )

  }

  # convert and return ----

  # if needed convert the object to the requested output
  df_olink <- convert_read_npx_output(df = df_olink,
                                      out_df = out_df)

  return(df_olink)
}

#' @rdname read_npx
#' @export
read_NPX <- read_npx
