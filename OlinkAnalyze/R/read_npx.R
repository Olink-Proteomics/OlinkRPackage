#' Read NPX, Ct or absolute quantification data in R.
#'
#' @description
#' Imports an NPX, Ct or Quantification file exported from Olink software.
#'
#' \strong{Note:} Do not modify the Olink software output file prior to
#' importing it with `read_npx()` as it might fail.
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
#' Expecting file extensions `csv`, `txt`, `xls`, `xlsx`, `parquet` or `zip`.
#' @param out_df The class of output data frame. One of `tibble` (default) or
#' `arrow` for ArrowObject.
#' @param sep Character separator of delimited input file. One of `NULL` for
#' auto-detection (default), `,` for comma or `;` for semicolon. Used only for
#' delimited output files from Olink software.
#' @param long_format Boolean marking format of input file. One of `NULL`
#' (default) for auto-detection, `TRUE` for long format files or `FALSE` for
#' wide format files.
#' @param olink_platform Olink platform used to generate the input file.
#' One of `NULL` (default), `Explore 3072`, `Explore HT`, `Target 96`,
#' `Target 48`, `Flex` or `Focus`.
#' @param data_type Quantification method of the input data. One of `NULL`
#' (default), `NPX`, `Quantified` or `Ct`.
#' @param .ignore_files Character vector of files included in the zip-compressed
#' Olink software output files that should be ignored. Applies only to
#' zip-compressed input files. `c("README.txt")` (default).
#' @param quiet Boolean to print a confirmation message when reading the input
#' file. Applies to excel or delimited input only. `TRUE` (default) to not print
#' and `FALSE` to print.
#'
#' @return Tibble or ArrowObject with Olink data in long format.
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
#'                     "Example_NPX_Data2_1.csv",
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
                     quiet = TRUE) {

  # check input ----

  # check if the input file exists
  check_file_exists(file = filename,
                    error = TRUE)

  # check that the requested putput df is ok
  check_out_df_arg(out_df = out_df)

  # sep and .ignore_file are checked in the functions they target

  # check file extension ----

  # get the extension of the input file
  f_ext <- tools::file_ext(x = filename)

  # check what type of label the extension of the input matches to
  f_label <- accepted_npx_file_ext[accepted_npx_file_ext == f_ext] |>
    names()

  # read data ----

  # if the extension of the input file was within the accepted ones it should
  # be a scalar character
  if (check_is_scalar_character(string = f_label, error = FALSE)) {

    if (grepl(pattern = "excel|delim", x = f_label)) {

      # Input is an excel or a delimited file
      df_olink <- read_npx_format(file = filename,
                                  out_df = out_df,
                                  sep = sep,
                                  long_format = long_format,
                                  olink_platform = olink_platform,
                                  data_type = data_type,
                                  quiet = quiet)

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

  } else {

    cli::cli_abort(
      message = c(
        "x" = "Unable to recognize format from file extension!",
        "i" = "Acceptable file extensions: {accepted_npx_file_ext}"
      ),
      call = NULL,
      wrap = FALSE
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
read_NPX <- read_npx # nolint
