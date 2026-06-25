#' Read Olink data in R.
#'
#' @description
#' Imports a file exported from Olink software that quantifies protein levels in
#' NPX, Ct or absolute quantification.
#'
#' \strong{Note:} Do not modify the Olink software output file prior to
#' importing it with \code{\link{read_npx}} as it might fail.
#'
#' @details
#' OlinkAnalyze uses pre-defined names of columns of data frames to perform
#' downstream analyses. At the same time, different Olink platforms export data
#' with different column names (e.g. different protein quantification metric).
#' The output of this function aims to instruct each function of OlinkAnalyze on
#' the column it should be using for the downstream analysis. This should be
#' seamless for data exported from Olink Software and imported to R using the
#' `read_npx()` or processed using the `clean_npx()` function.
#'
#' However, in certain cases the columns of interest might be named differently.
#' The argument \var{preferred_names} allows assigning custom-named columns of a
#' data frame to internally expected variables that will in turn instruct Olink
#' Analyze functions to use them for downstream analysis. For example, if one
#' wished to use the column \var{PCNormalizedNPX} for their analysis instead of
#' the column \var{NPX}, then they can assign this new name to the internal
#' variable \var{quant} to inform the package that in the downstream analysis
#' \var{PCNormalizedNPX} should be used. See examples below.
#'
#' The argument \var{preferred_names} is a named character vector with internal
#' column names as names and column names of the current data set as values.
#' Names of the input vector can be one or more of the following:
#' `r ansi_collapse_quot(x = column_name_dict$col_key)`
#'
#' @author
#' Klev Diamanti
#' Kathleen Nevola
#' Pascal Pucholt
#' Christoffer Cambronero
#' Boxi Zhang
#' Olof Mansson
#' Marianne Sandin
#'
#' @inherit .read_npx_args params return
#'
#' @keywords NPX parquet csv zip xlsx xls
#'
#' @export
#'
#' @examples
#' \donttest{
#' file <- system.file("extdata",
#'                     "npx_data_ext.parquet",
#'                     package = "OlinkAnalyze")
#'
#' # use NPX by default as quantification
#' OlinkAnalyze::read_npx(filename = file)
#'
#' # use PCNormalizedNPX as quantification
#' OlinkAnalyze::read_npx(
#'   filename = file,
#'   preferred_names = c("quant" = "PCNormalizedNPX")
#' )
#' }
#'
read_npx <- function(filename,
                     preferred_names = NULL,
                     out_df = "tibble",
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

  check_is_scalar_boolean(x = legacy,
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
      long_format = long_format,
      olink_platform = olink_platform,
      data_type = data_type,
      .ignore_files = .ignore_files,
      quiet = quiet
    )

  }

  # convert and return ----

  # Convert to requested output format, run check_npx, and attach check_log
  df_olink <- attach_check_log(
    df = df_olink,
    out_df = out_df,
    preferred_names = preferred_names
  )

  return(df_olink)
}

#' @rdname read_npx
#' @export
read_NPX <- read_npx  # nolint: object_name_linter
