#' Help function to read excel and delimited Olink data files in R and determine
#' their format, data type and platform.
#'
#' @description
#' This function processes Olink software excel or delimited files regardless of
#' data type, platform or format.
#'
#' \strong{Olink software excel files} with the extension
#' `r ansi_collapse_quot(get_file_ext(name_sub = "excel"), sep = "or")`
#' are imported in R by the function \code{\link{read_npx_excel}}.
#'
#' \strong{Olink software delimited files} with suffix
#' `r ansi_collapse_quot(get_file_ext(name_sub = "delim"), sep = "or")`
#' are imported in R by the functions \code{\link{read_npx_delim}} or
#' \code{\link{read_npx_csv}}.
#'
#' Files in wide format are subsequently handled by the function
#' \code{\link{read_npx_wide}}.
#'
#' \strong{Olink software files in wide format} always originate from the Olink
#' qPCR platforms, and are further processed by
#' \code{\link{read_npx_format_get_format}} and
#' \code{\link{read_npx_format_get_quant}} to determine the data type and Olink
#' platform, respectively.
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
#' @param file Path to Olink software output file in wide or long format.
#' Expecting file extensions
#' `r get_file_ext(name_sub = c("excel", "delim")) |> ansi_collapse_quot()`.
#' @param out_df The class of output data frame. One of
#' `r ansi_collapse_quot(read_npx_df_output)`.
#' @param sep Character separator of delimited input file. One of `NULL`
#' (default) for auto-detection, or `r ansi_collapse_quot(accepted_field_sep)`.
#' Used only for delimited output files from Olink software.
#' @param long_format Boolean marking format of input file. One of `NULL`
#' (default) for auto-detection, `TRUE` for long format files or `FALSE` for
#' wide format files.
#' @param olink_platform Olink platform used to generate the input file.
#' One of `NULL` (default) for auto-detection,
#' `r get_olink_platforms(broad_platform = "qPCR") |> ansi_collapse_quot()`.
#' @param data_type Quantification method of the input data. One of `NULL`
#' (default) for auto-detection, `r ansi_collapse_quot(get_olink_data_types())`.
#' @param quiet Boolean to print a confirmation message when reading the input
#' file. `TRUE` (default) to skip printing, and `FALSE` to print.
#' @param legacy Boolean to enforce returning a list containing olink_platform,
#' data_type and long_format information together with the dataset. Used only
#' when \code{\link{read_npx_format}} is called from
#' \code{\link{read_npx_legacy}}.
#'
#' @return `r ansi_collapse_quot(x = get_df_output_print(), sep = "or")` with
#' Olink data in long or wide format.
#'
#' @seealso
#'   \code{\link{read_npx}}
#'   \code{\link{read_npx_format_read}}
#'   \code{\link{read_npx_format_get_format}}
#'   \code{\link{read_npx_format_get_platform}}
#'   \code{\link{read_npx_format_get_quant}}
#'   \code{\link{read_npx_legacy}}
#'
read_npx_format <- function(file,
                            out_df = "arrow",
                            sep = NULL,
                            long_format = NULL,
                            olink_platform = NULL,
                            data_type = NULL,
                            quiet = FALSE,
                            legacy = FALSE) {

  # Check input ----

  # check if the input file exists
  check_file_exists(file = file,
                    error = TRUE)

  # check that the requested output df is ok
  check_out_df_arg(out_df = out_df)

  # check long format input
  if (!is.null(long_format)) {
    check_is_scalar_boolean(bool = long_format,
                            error = TRUE)
  }

  # check olink_platform
  if (!is.null(olink_platform)) {
    check_olink_platform(x = olink_platform)
  }

  # check data type
  if (!is.null(data_type)) {
    check_olink_data_type(x = data_type)
  }

  check_is_scalar_boolean(bool = quiet,
                          error = TRUE)

  # Determine data format, Olink platform and quant method from excel file ----

  ## read the file and its top n rows ----

  list_df_read <- read_npx_format_read(file = file,
                                       sep = sep,
                                       read_n = 3L)

  ## Determine long or wide format ----

  file_format_check <- read_npx_format_get_format(
    df_top_n = list_df_read$df_top_n,
    file = file,
    long_format = long_format
  )

  ## Determine Olink platform & quantification method ----

  # this is of interest only if the file format is wide

  if (file_format_check$is_long_format == FALSE) {

    # get platform
    file_olink_platform <- read_npx_format_get_platform(
      df_top_n = list_df_read$df_top_n,
      file = file,
      olink_platform = olink_platform
    )

    # get quantification method
    file_quant_method <- read_npx_format_get_quant(
      file = file,
      data_type = data_type,
      data_cells = file_format_check$data_cells
    )

    out_msg <- "Detected \"{file_quant_method}\" data from
        \"Olink {file_olink_platform}\" in wide format!"

  } else {

    out_msg <- "Detected data in long format!"

    # set these variables for completion
    file_olink_platform <- NULL
    file_quant_method <- NULL

  }

  # Message of data detection ----

  if (!quiet) {

    cli::cli_inform(
      message = out_msg,
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

  # return ----

  # this flag is on when read_npx_legacy calls this function
  if (legacy == TRUE) {
    return(
      list(
        olink_platform = file_olink_platform,
        long_format = file_format_check$is_long_format,
        data_type = file_quant_method,
        df = convert_read_npx_output( # read_npx_legacy expects tibble
          df = list_df_read$df,
          out_df = "tibble"
        )
      )
    )
  }

  if (file_format_check$is_long_format == TRUE) {
    # if data is in long format

    # if needed convert the object to the requested output
    df_olink <- convert_read_npx_output(df = list_df_read$df,
                                        out_df = out_df)

  } else if (file_format_check$is_long_format == FALSE) {
    # if data is in wide format send it read_npx_wide

    df_olink <- convert_read_npx_output( # read_npx_wide expects tibble
      df = list_df_read$df,
      out_df = "tibble"
    ) |>
      read_npx_wide(
        file = file,
        data_type = file_quant_method,
        olink_platform = file_olink_platform
      ) |>
      # convert to what the user asked for
      convert_read_npx_output(
        out_df = out_df
      )

  }

  return(df_olink)

}

#' Help function to read excel and delimited Olink data files in R.
#'
#' @description
#' This function reads Olink software excel or delimited files regardless of
#' data type, platform or format.
#'
#' \strong{Olink software excel files} with the extension
#' `r ansi_collapse_quot(get_file_ext(name_sub = "excel"), sep = "or")`
#' are imported in R by the function \code{\link{read_npx_excel}}.
#'
#' \strong{Olink software delimited files} with suffix
#' `r ansi_collapse_quot(get_file_ext(name_sub = "delim"), sep = "or")`
#' are imported in R by the functions \code{\link{read_npx_delim}} or
#' \code{\link{read_npx_csv}}.
#'
#' \itemize{
#' \item Files in long format are read with the column row as column names.
#' \item Files in wide format are read with column names as V1, V2, etc.
#' }
#'
#' This function also extracts the first \var{read_n} rows of the dataset read
#' earlier to determine the Olink platform that generated the file, the data
#' type and and file format.
#'
#' @author
#'   Klev Diamanti
#'
#' @param file Path to Olink software output file in wide or long format.
#' Expecting file extensions
#' `r get_file_ext(name_sub = c("excel", "delim")) |> ansi_collapse_quot()`.
#' @param sep Character separator of delimited input file. One of `NULL`
#' (default) for auto-detection, or `r ansi_collapse_quot(accepted_field_sep)`.
#' Used only for delimited output files from Olink software.
#' @param read_n Number of top rows to read.
#'
#' @return A list with two elements:
#' \itemize{
#' \item An ArrowObject (\var{df}) containing the full dataset.
#' \item A tibble (\var{df_top_n}) containing the \var{read_n} rows of the full
#' dataset. This subset of data is used to determine \var{long_format},
#' \var{olink_platform} and \var{data_type}.
#' }
#'
#' @seealso
#'   \code{\link{read_npx_format}}
#'   \code{\link{read_npx_format_get_format}}
#'   \code{\link{read_npx_format_get_platform}}
#'   \code{\link{read_npx_format_get_quant}}
#'
read_npx_format_read <- function(file,
                                 sep = NULL,
                                 read_n = 3L) {

  # check input ----

  check_file_exists(file = file,
                    error = TRUE)

  check_is_scalar_integer(int = read_n,
                          error = TRUE)

  # sep is checked in the delim function

  # read file ----

  # get the extension of the input file
  f_ext <- tools::file_ext(x = file)

  # check what type of label the extension of the input matches to
  f_label <- accepted_npx_file_ext[accepted_npx_file_ext == f_ext] |>
    names()

  if (grepl(pattern = "excel", x = f_label)) {

    df <- read_npx_excel(file = file,
                         out_df = "arrow")

  } else if (grepl(pattern = "delim", x = f_label)) {

    df <- read_npx_delim(file = file,
                         out_df = "arrow",
                         sep = sep)

  } else {

    cli::cli_abort(
      message = c(
        "x" = "Unable to recognize format from file extension of
        {.file {file}}!"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

  # extract the read_n top rows ----

  # create top_n data frame with top read_n rows
  # the first row of this data frame are the column names in case of a long
  # format file, followed by the first two rows
  # in case of a wide fornat file it it simple the first three rows
  if (all(grepl(pattern = "^V", x = names(df)))) { # wide format

    df_top_n <- df |>
      dplyr::slice_head(
        n = read_n
      ) |>
      dplyr::mutate(
        dplyr::across(
          dplyr::everything(),
          ~ as.character(.x)
        )
      ) |>
      dplyr::collect() |>
      remove_all_na_cols()

  } else { # long format

    df_top_n <- dplyr::tibble(X = names(df)) |>
      t()
    colnames(df_top_n) <- paste0("V", seq_len(ncol(df_top_n)))

    df_names_v <- df_top_n[1L, ] |> as.character()
    names(df_names_v) <- colnames(df_top_n)
    df_top_n <- df_top_n |>
      dplyr::as_tibble() |>
      dplyr::bind_rows(
        df |>
          dplyr::slice_head(
            n = read_n - 1L
          ) |>
          dplyr::mutate(
            dplyr::across(
              dplyr::everything(),
              ~ as.character(.x)
            )
          ) |>
          dplyr::collect() |>
          dplyr::rename(
            dplyr::all_of(df_names_v)
          )
      ) |>
      remove_all_na_cols()
  }

  # return ----

  return(
    list(
      df = df,
      df_top_n = df_top_n
    )
  )

}

#' Help function to determine wide or long file format.
#'
#' @description
#' The function uses the first \var{read_n} rows of the input dataset to
#' determine the format of the input file.
#'
#' The user can provide the file format as an input argument. If the user did
#' not provide an input with regards to the file format, then it is
#' auto-determined exclusively from the function. If user provided the file
#' format as input, then the function cross-checks that it's auto-detection
#' matches user's input. If not, it will throw a warning, and accept user's
#' input as the correct answer.
#'
#' @author
#'   Klev Diamanti
#'
#' @param df_top_n A tibble containing the first \var{read_n} rows of the input
#' Olink file.
#' @param file Path to Olink software output file in wide or long format.
#' Expecting file extensions
#' `r get_file_ext(name_sub = c("excel", "delim")) |> ansi_collapse_quot()`.
#' @param long_format Boolean marking format of input file. One of `NULL`
#' (default) for auto-detection, `TRUE` for long format files or `FALSE` for
#' wide format files.
#'
#' @return A list with two elements:
#' \itemize{
#' \item A scalar boolean (\var{is_long_format}) marking if the input file is in
#' long (`TRUE`) or wide (`FALSE`) format.
#' \item A character vector (\var{data_cells}) from the input file which allows
#' detection of the quantification method. Used in function
#' \code{\link{read_npx_format_get_quant}}.
#' }
#'
#' @seealso
#'   \code{\link{read_npx_format}}
#'   \code{\link{read_npx_format_read}}
#'   \code{\link{read_npx_format_get_platform}}
#'   \code{\link{read_npx_format_get_quant}}
#'
read_npx_format_get_format <- function(df_top_n,
                                       file,
                                       long_format = NULL) {

  # Check inputs ----

  check_is_tibble(df = df_top_n,
                  error = TRUE)

  check_file_exists(file = file,
                    error = TRUE)

  # check long format input
  if (!is.null(long_format)) {
    check_is_scalar_boolean(bool = long_format,
                            error = TRUE)
  }

  # Read in cells to determine format ----

  # Read specific cells of the excel files to determine wide or long format,
  # and protein level metric (NPX, Quant or Ct)
  data_cells_wide <- df_top_n |>
    dplyr::select(
      dplyr::all_of("V1")
    ) |>
    dplyr::slice(
      2L
    ) |>
    # convert to character to simplify operations below
    as.character()

  data_cells_long <- df_top_n |>
    dplyr::slice_head(
      n = 1L
    ) |>
    # convert to character to simplify operations below
    as.character() |>
    # exclude column NPX Signature Version as it results in the function not
    # recognizing the format
    (\(.x) .x[!grepl(pattern = "Version", x = .x, ignore.case = TRUE)])()

  # Determine long or wide format from file ----

  is_data_wide <- grepl(pattern = paste(get_olink_data_types(), collapse = "|"),
                        x = data_cells_wide,
                        ignore.case = FALSE)
  is_data_long <- grepl(pattern = paste(get_olink_data_types(), collapse = "|"),
                        x = data_cells_long,
                        ignore.case = FALSE)

  if (is_data_wide == FALSE && any(is_data_long == TRUE)) {
    # in long format files we expect the quantification method to appear in
    # cells L1:O1 and we also expect no matches to cell A2. This is what marks
    # wide format files.

    detected_long_format <- TRUE
    data_cells <- data_cells_long

  } else if (is_data_wide == TRUE && all(is_data_long == FALSE)) {
    # in wide format files we expect the quantification method to appear in
    # cell A2 and we also expect cells L1:O1 to be empty.

    detected_long_format <- FALSE
    data_cells <- data_cells_wide

  } else {
    # Throw an error only if the user has not provided any info if the file is
    # in wide or long format.
    # The validity of the argument long_format has been checked in the parent
    # function .

    if (is.null(long_format)) {

      cli::cli_abort(
        message = c(
          "x" = "Unable to recognize the format of the input file:
          {.file {file}}!",
          "i" = "Consider setting {.arg long_format}"
        ),
        call = rlang::caller_env(),
        wrap = FALSE
      )

    } else {

      detected_long_format <- NULL
      data_cells <- NULL

    }

  }

  # Checks ----

  ## Check if the detected file format matches the customer input ----

  if (!is.null(long_format)) {

    # check if the format from the long_format argument input matches what we
    # detected in the data file
    if (is.null(detected_long_format)) {

      # warning if we were unable to determine format from the file but the user
      # has already provided it as input.
      cli::cli_warn(
        message = c(
          "i" = "Unable to confirm the
          \"{ifelse(long_format,\"long\",\"wide\")}\" format from the input
          file: {.file {file}}. We trust your input!"
        ),
        call = rlang::caller_env(),
        wrap = FALSE
      )

    } else if (long_format != detected_long_format) {

      # warning if what we detected and the user provided as input differ.
      cli::cli_warn(
        message = c(
          "i" = "Based on {.arg long_format} we were expecting
          \"{ifelse(long_format,\"long\",\"wide\")}\" format data, but instead
          detected \"{ifelse(detected_long_format,\"long\",\"wide\")}\". We
          trust your input!"
        ),
        call = rlang::caller_env(),
        wrap = FALSE
      )

    }

    is_long_format <- long_format
    if (is_long_format) {
      data_cells <- data_cells_long
    } else {
      data_cells <- data_cells_wide
    }

  } else {

    # if long_format input is NULL then set it from what we auto-detected
    is_long_format <- detected_long_format
    data_cells <- data_cells

  }

  ## Check that long format data do not have NA colnames ----

  check_na_colname <- df_top_n[1L, ] |>
    as.character() |>
    is.na() |>
    any() && (is_long_format == TRUE)

  if (check_na_colname) {

    cli::cli_abort(
      message = c(
        "x" = "`NA` column names in long format file: {.file {file}}!",
        "i" = "Please inspect the input file!"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

  # Return ----

  # return the cells of the data that determine the file format and a boolean
  # marking the format
  return(
    list(
      is_long_format = is_long_format,
      data_cells = data_cells
    )
  )

}

#' Help function to determine Olink platform from the input file in wide format.
#'
#' @description
#' This function uses the panel name from Olink software files in wide format to
#' determine the qPCR platform that was used for the project that this dataset
#' represents.
#'
#' @author
#'   Klev Diamanti
#'
#' @param df_top_n A tibble containing the first \var{read_n} rows of the input
#' Olink file.
#' @param file Path to Olink software output file in wide format. Expecting file
#' extensions
#' `r get_file_ext(name_sub = c("excel", "delim")) |> ansi_collapse_quot()`.
#' @param olink_platform Olink platform used to generate the input file.
#' One of `NULL` (default) for auto-detection,
#' `r get_olink_platforms(broad_platform = "qPCR") |> ansi_collapse_quot()`.
#'
#' @return The name of the Olink platform. One of
#' `r ansi_collapse_quot(get_olink_platforms(broad_platform = "qPCR"))`.
#'
#' @seealso
#'   \code{\link{read_npx_format}}
#'   \code{\link{read_npx_format_read}}
#'   \code{\link{read_npx_format_get_format}}
#'   \code{\link{read_npx_format_get_quant}}
#'
read_npx_format_get_platform <- function(df_top_n,
                                         file,
                                         olink_platform = NULL) {

  # Checks inputs ----

  check_is_tibble(df = df_top_n,
                  error = TRUE)

  check_file_exists(file = file,
                    error = TRUE)

  # help vars
  broad_platform <- "qPCR"

  check_is_scalar_character(string = broad_platform,
                            error = TRUE)

  # check olink platform
  if (!is.null(olink_platform)) {
    check_olink_platform(x = olink_platform,
                         broad_platform = broad_platform)
  }

  # Determine olink platform from input file ----

  # From cell B3 in the wide file read panel name

  check_columns(df = df_top_n, col_list = list("V2"))

  panel_name <- df_top_n |> # nolint object_usage_linter
    dplyr::select(
      dplyr::all_of("V2")
    ) |>
    dplyr::slice(
      3L
    ) |>
    as.character()

  # run the platform-specific regular expression from the global variable to
  # determine that platform.
  olink_platform_auto <- accepted_olink_platforms |>
    dplyr::filter(
      .data[["broader_platform"]] == .env[["broad_platform"]]
    ) |>
    dplyr::mutate(
      detected_platform = stringr::str_detect(
        string = .env[["panel_name"]],
        pattern = .data[["regexp"]]
      )
    ) |>
    dplyr::filter(
      .data[["detected_platform"]] == TRUE
    ) |>
    dplyr::pull(
      .data[["name"]]
    )

  # Confirm that detected platform matches user input ----

  if (is.null(olink_platform)) {
    # if olink_platform is NULL

    # check how many platforms match regular expression
    if (length(olink_platform_auto) == 0L) {
      # if platform cannot be determined from input file throw an error

      cli::cli_abort(
        message = c(
          "x" = "Unable to recognize the Olink platform from the input file:
        {.file {file}}!",
          "i" = "Expected one of:
          {.val {get_olink_platforms(broad_platform = broad_platform)}}.
          Consider setting {.arg olink_platform}."
        ),
        call = rlang::caller_env(),
        wrap = FALSE
      )

    } else if (length(olink_platform_auto) > 1L) {
      # if too many platform matches cannot from input file throw an error

      cli::cli_abort(
        message = c(
          "x" = "Unable to recognize the Olink platform from the input file:
        {.file {file}}!",
          "i" = "Too many matches from:
          {.val {get_olink_platforms(broad_platform = broad_platform)}}.
          Consider setting {.arg olink_platform}."
        ),
        call = rlang::caller_env(),
        wrap = FALSE
      )

    } else {

      # if data format input is NULL then set it from autodetect
      out_olink_platform <- olink_platform_auto

    }

  } else {
    # if olink_platform is not NULL

    # check how many platforms match regular expression
    if (length(olink_platform_auto) == 0L) {
      # if platform cannot be determined from input file throw a warning

      cli::cli_warn(
        message = c(
          "i" = "Unable to recognize the Olink platform from the input file:
        {.file {file}}! No matches. We trust your input!"
        ),
        call = rlang::caller_env(),
        wrap = FALSE
      )

    } else if (length(olink_platform_auto) > 1L) {
      # if too many platform matches from input file throw a warning

      cli::cli_warn(
        message = c(
          "i" = "Unable to recognize the Olink platform from the input file:
        {.file {file}}! Too many matches. We trust your input!"
        ),
        call = rlang::caller_env(),
        wrap = FALSE
      )

    } else if (olink_platform != olink_platform_auto) {

      cli::cli_warn(
        message = c(
          "i" = "Based on {.arg olink_platform} we were expecting Olink
          {.val {olink_platform}} data, but instead we detected
          {.val {olink_platform_auto}} data. We trust your input!"
        ),
        call = rlang::caller_env(),
        wrap = FALSE
      )

    }

    # in any case we trust the user input
    out_olink_platform <- olink_platform

  }

  # Return ----

  return(out_olink_platform)

}

#' Help function to determine the type of quantification from the input file in
#' wide format.
#'
#' @description
#' This function uses information from the cell A2 from Olink software files in
#' wide format to determine the quantification method of the data that the
#' dataset contains.
#'
#' @author
#'   Klev Diamanti
#'
#' @param file Path to Olink software output file in wide format. Expecting file
#' extensions
#' `r get_file_ext(name_sub = c("excel", "delim")) |> ansi_collapse_quot()`.
#' @param data_type Quantification method of the input data. One of `NULL`
#' (default) for auto-detection, `r ansi_collapse_quot(get_olink_data_types())`.
#' @param data_cells A character vector with the contents of the cell \emph{A2}
#' from the Olink software file in wide format indicating the quantification
#' method.
#'
#' @return The name of the data type. One of
#' `r ansi_collapse_quot(get_olink_data_types(broad_platform = "qPCR"))`.
#'
#' @seealso
#'   \code{\link{read_npx_format}}
#'   \code{\link{read_npx_format_read}}
#'   \code{\link{read_npx_format_get_format}}
#'   \code{\link{read_npx_format_get_platform}}
#'
read_npx_format_get_quant <- function(file,
                                      data_type = NULL,
                                      data_cells) {

  # Checks inputs ----

  check_file_exists(file = file,
                    error = TRUE)

  check_is_character(string = data_cells,
                     error = TRUE)

  # help vars
  broad_platform <- "qPCR"

  # check data type
  if (!is.null(data_type)) {
    check_olink_data_type(x = data_type,
                          broad_platform = broad_platform)
  }

  # Determine quantification method from excel file ----

  # In wide format data_cells is of length 1 as we read only cell A2
  # the solution below is expected to catch all cases
  quant_matches <- sapply(
    X = get_olink_data_types(broad_platform = broad_platform),
    FUN = grepl,
    x = data_cells,
    ignore.case = FALSE,
    fixed = TRUE,
    USE.NAMES = TRUE,
    simplify = FALSE
  ) |>
    dplyr::as_tibble() |>
    dplyr::mutate(
      input_string = .env[["data_cells"]]
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(get_olink_data_types(
        broad_platform = broad_platform
      )),
      names_to = "q_method",
      values_to = "is_in"
    ) |>
    dplyr::filter(
      .data[["is_in"]] == TRUE
    ) |>
    dplyr::pull(
      .data[["q_method"]]
    )

  # Confirm that detected quant method matches user input ----

  if (is.null(data_type)) {
    # if data_type is null

    if (length(quant_matches) == 0L) {
      # check if there are no matches

      cli::cli_abort(
        message = c(
          "x" = "Unable to recognize the quantification method from the input
        file: {.file {file}}!",
          "i" = "Expected one of:
          {.val {get_olink_data_types(broad_platform = broad_platform)}}.
          Consider setting {.arg data_type}."
        ),
        call = rlang::caller_env(),
        wrap = FALSE
      )

    } else if (length(quant_matches) > 1L) {
      # check if there are multiple matches

      cli::cli_abort(
        message = c(
          "x" = "Unable to recognize the quantification method from the input
        file: {.file {file}}!",
          "i" = "Too many occurrences of:
          {.val {get_olink_data_types(broad_platform = broad_platform)}}.
          Consider setting {.arg data_type}."
        ),
        call = rlang::caller_env(),
        wrap = FALSE
      )

    } else {

      # if data_type input is NULL then set it from autodetect
      out_data_type <- quant_matches

    }

  } else {
    # if data_type is null

    # check how many platforms match regular expression
    if (length(quant_matches) == 0L) {
      # if quant method cannot be determined from input file throw a warning

      cli::cli_warn(
        message = c(
          "i" = "Unable to recognize the quantification method from the input
          file: {.file {file}}! No matches. We trust your input!"
        ),
        call = rlang::caller_env(),
        wrap = FALSE
      )

    } else if (length(quant_matches) > 1L) {
      # if too many quant method matches from input file throw a warning

      cli::cli_warn(
        message = c(
          "i" = "Unable to recognize the quantification method from the input
          file: {.file {file}}! Too many matches. We trust your input!"
        ),
        call = rlang::caller_env(),
        wrap = FALSE
      )

    } else if (data_type != quant_matches) {

      cli::cli_warn(
        message = c(
          "i" = "Based on {.arg data_type} we were expecting {.val {data_type}}
          format data, but instead detected {.val {quant_matches}}. We trust
          your input!"
        ),
        call = rlang::caller_env(),
        wrap = FALSE
      )

    }

    # in any case we trust the user input
    out_data_type <- data_type

  }

  # Return ----

  return(out_data_type)

}
