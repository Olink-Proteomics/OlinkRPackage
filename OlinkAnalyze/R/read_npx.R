#' Function to read NPX data into R in long format.
#'
#' @description
#' Imports an NPX or QUANT file exported from Olink software.
#'
#' __Note__ Please do not alter the Olink software output file prior to
#' importing it using `read_NPX()` as it might fail.
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
#' @param filename Path to Olink software output file.
#' @param out_df The class of output data frame to be returned. Accepted values
#' are "tibble" (default) and "arrow".
#' @param sep The separator of the delimited file: NULL (autodetect), comma (,)
#' or semicolon (;). Used only for delimited Olink software output files.
#' @param long_format Boolean marking if input file is in long (TRUE) or wide
#' (FALSE) format. Ignored for non-excel input files.
#' @param olink_platform The Olink platform used to generate the input file.
#' Expecting "Target 96", "Target 48", "Flex" or "Focus". Ignored for non-excel
#' input files.
#' @param data_type The quantification in which the data comes in. Expecting one
#' of NPX, Quantified or Ct. Ignored for non-excel input files.
#' @param .ignore_files Files to ignore in the zip-compressed Olink software
#' output files. Used only for zip-compressed Olink software output files.
#' @param quiet Print a confirmation message after reading in the input file.
#'
#' @return A tibble in long format or an ArrowObject.
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
                     quiet = FALSE) {

  # check if the input file exists
  check_file_exists(file = filename,
                    error = TRUE)

  # check that the requested putput df is ok
  check_out_df_arg(out_df = out_df)


  # sep and .ignore_file are checked in the functions they target

  # get the extension of the input file
  f_ext <- tools::file_ext(x = filename)

  # check what type of label the extension of the input matches to
  f_label <- accepted_npx_file_ext[accepted_npx_file_ext == f_ext] |>
    names()

  # if the extension of the input file was within the accepted ones it should
  # be a scalar character
  if (check_is_scalar_character(string = f_label, error = FALSE)) {

    if (grepl("excel", f_label)) {

      # Input is an excel file
      df_olink <- read_npx_excel(file = filename,
                                 out_df = out_df,
                                 long_format = long_format,
                                 olink_platform = olink_platform,
                                 data_type = data_type,
                                 quiet = quiet)

    } else if (grepl("delim", f_label)) {

      # Input is a delimited file
      df_olink <- read_npx_delim(file = filename,
                                 sep = sep)

    } else if (grepl("parquet", f_label)) {

      # Input is a parquet file
      df_olink <- read_npx_parquet(file = filename)

    } else if (grepl("compressed", f_label)) {

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

  # if needed convert the object to the requested output
  df_olink <- convert_read_npx_output(df = df_olink,
                                      out_df = out_df)

  return(df_olink)
}

#' @rdname read_npx
#' @export
read_NPX <- read_npx # nolint object_usage_linter
#' @rdname read_npx
