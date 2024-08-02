#' Help function to read NPX, Ct or absolute quantification data from excel
#' Olink software output files in R.
#'
#' @author
#'   Klev Diamanti;
#'   Christoffer Cambronero;
#'   Kathleen Nevola
#'
#' @param file Path to Olink software output excel file in wide or long format.
#' Expecting file extensions
#' `r accepted_npx_file_ext[grepl("excel", names(accepted_npx_file_ext))] |> cli::ansi_collapse(sep2 = " or ", last = ", or ")`. # nolint
#' @param out_df The class of output data frame. One of "tibble" (default) or
#' "arrow" for ArrowObject.
#'
#' @return Tibble or ArrowObject with Olink data in wide or long format.
#'
#' @seealso
#'   \code{\link{read_npx}}
#'   \code{\link{read_npx_parquet}}
#'   \code{\link{read_npx_zip}}
#'   \code{\link{read_npx_format}}
#'   \code{\link{read_npx_delim}}
#'
read_npx_excel <- function(file,
                           out_df = "arrow") {

  # Check input ----

  # check if file exists
  check_file_exists(file = file,
                    error = TRUE)

  # check that the requested output df is ok
  check_out_df_arg(out_df = out_df)

  # check if readxl is available ----

  check_library_installed(libraries = "readxl",
                          error = TRUE)

  # read data ----

  # tryCatch in case reading the delimited file fails
  tryCatch(
    {

      # read the entire excel file
      df_olink <-  readxl::read_excel(path = file,
                                      col_names = FALSE,
                                      .name_repair = "minimal")

      # if the first row has 1 or more NAs we can safely guess that this is a
      # Olink file in wide format so we have to call the column names V1, V2...
      if (any(is.na(df_olink[1L, ]))) {

        colnames(df_olink) <- paste0("V", seq_len(ncol(df_olink)))
        df_olink <- df_olink |>
          dplyr::as_tibble()

      } else {

        colnames(df_olink) <- df_olink[1L, ] |> as.character()
        df_olink <- df_olink |>
          dplyr::as_tibble() |>
          dplyr::slice(
            2L:dplyr::n()
          )
      }

    }, error = function(msg) {

      cli::cli_abort(
        c(
          "x" = "Unable to open excel file: {.file {file}}",
          "i" = "Check if the input {.arg file} is an excel file."
        ),
        call = rlang::caller_env(),
        wrap = FALSE
      )

    }
  )

  if (length(names(df_olink)) == 1L) {

    cli::cli_warn(
      message = c(
        "i" = "The excel file {.file {file}} has only one column!"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

  # top row is as expected for the corresponding format

  read_npx_format_colnames(df = df_olink, file = file)

  # if needed convert the object to the requested output
  df_olink <- convert_read_npx_output(df = df_olink,
                                      out_df = out_df)

  # return ----

  return(df_olink)

}
