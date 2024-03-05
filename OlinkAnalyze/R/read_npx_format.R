#' Help function to identify the format, data type and platform from the input
#' Olink file.
#'
#' @description
#' This function should be processing all Olink excel or delimited files
#' regardless of data type, platform or wide/long format. Files in wide format
#' should be handled by read_npx_wide; files in long format delimited by comma
#' or semicolon (csv) should be handled by read_npx_delim; and files in long
#' format in excel (xlsx) should be handled by read_npx_excel. Only files in
#' wide format that always originate from qPCR platforma should be further
#' investigated for data type and olink platform.
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
#' @param file The input Olink file in excel or delimited format.
#' @param out_df The class of output data frame to be returned. Accepted values
#' are "tibble" (default) and "arrow".
#' @param sep The separator of the file: NULL (autodetect), comma (,) or
#' semicolon (;). Ignored if file is in excel format.
#' @param long_format Boolean marking if input file is in long (TRUE) or wide
#' (FALSE) format.
#' @param olink_platform The Olink platform used to generate the input file.
#' Expecting "Explore 3072", "Explore HT", "Target 96", "Target 48", "Flex" or
#' "Focus".
#' @param data_type The quantification in which the data comes in. Expecting one
#' of NPX, Quantified or Ct.
#' @param quiet Print a confirmation message after reading in the input file.
#'
#' @return  A tibble in long format or an ArrowObject.
#'
read_npx_format <- function(file,
                            out_df = "arrow",
                            sep = NULL,
                            long_format = NULL,
                            olink_platform = NULL,
                            data_type = NULL,
                            quiet = FALSE) {

  # Check input ----

  # check if the input file exists
  check_file_exists(file = file,
                    error = TRUE)

  # check that the requested putput df is ok
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

  file_format_check <- read_npx_get_format(
    df = list_df_read$df_top_n,
    file = file,
    long_format = long_format,
    quant_methods = accepted_olink_platforms$quant_method |>
      unlist() |>
      unique()
  )

  ## Determine Olink platform & quantification method ----

  # this is of interest only if the file format is wide

  if (file_format_check$is_long_format == FALSE) {

    # default variables ----

    olink_broader_platform <- "qPCR" # nolint object_usage_linter

    # filter the global variable accepted_olink_platforms to have a collection
    # of platforms and quant methods available.
    # Note that only Target platforms can report data in excel.
    olink_platforms_wide <- accepted_olink_platforms |>
      dplyr::filter(
        .data[["broader_platform"]] == .env[["olink_broader_platform"]]
      )

    # get platform
    file_olink_platform <- read_npx_get_platform(
      df = list_df_read$df_top_n,
      file = file,
      olink_platform = olink_platform,
      olink_platforms_wide = olink_platforms_wide
    )

    # get quantification method
    file_quant_method <- read_npx_get_quant(
      file = file,
      data_type = data_type,
      data_cells = file_format_check$data_cells,
      quant_methods_expected = olink_platforms_wide |>
        dplyr::filter(
          .data[["name"]] == .env[["file_olink_platform"]]
        ) |>
        dplyr::pull(
          .data[["quant_method"]]
        ) |>
        unlist()
    )

    out_msg <- "Detected \"{file_quant_method}\" data from
        \"Olink {file_olink_platform}\" in wide format!"

  } else {

    out_msg <- "Detected data in long format!"

  }

  # Message of data detection ----

  if (!quiet) {

    cli::cli_inform(
      message = out_msg,
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

  # Read data in ----

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

#' Help function that reads the first two rows of an Olink delimited or excel
#' file.
#'
#' @description
#' This function extracts the first two rows of an Olink output file to detect
#' the Olink platform that generated the file, the data type and and file
#' format. The input file format may be wide or long, and the input file type
#' can be excel (xls/xlsx) or delimited (csv/txt).
#'
#' In case of long format input, the function will return the row of the column
#' names and the first row with data.
#'
#' In case of wide format inpiut, the function will return the first two rows of
#' the inpit file.
#'
#' @author Klev Diamanti
#'
#' @param file Path to Olink software output in excel, txt or csv.
#' @param sep The separator of the file: NULL (autodetect), comma (,) or
#' semicolon (;). Ignored if file is in excel format.
#' @param read_n Number of top rows to read.
#'
#' @return A list with one tibble (df_top_n) and one ArrowObject (df). The
#' ArrowObject contains the full dataset and the tibble the 3 rows that will
#' allow us to determine the characteristics of the input file.
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
      call = NULL,
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

#' Help function to determine the wide or long file format.
#'
#' @author Klev Diamanti
#'
#' @param df Tibble containing the first three rows of the input Olink file.
#' @param file The input Olink file.
#' @param long_format Boolean marking if input file is in long (TRUE) or wide
#' (FALSE) format.
#' @param quant_methods Character vector with the Olink protein
#' quantification methods.
#'
#' @return A list of two variables: is_long_format and data_cells The first one
#' is a boolean noting the file format and the second one is the character
#' vector from the input file where the format was determined from.
#' An error is returned if the format cannot be determined, unless the user has
#' defined the file format from the input.
#'
read_npx_get_format <- function(df,
                                file,
                                long_format = NULL,
                                quant_methods) {

  # Check inputs ----

  check_is_data_frame(df = df,
                      error = TRUE)

  check_file_exists(file = file,
                    error = TRUE)

  check_is_character(string = quant_methods,
                     error = TRUE)

  # check long format input
  if (!is.null(long_format)) {
    check_is_scalar_boolean(bool = long_format,
                            error = TRUE)
  }

  # Read in cells to determine format ----

  # Read specific cells of the excel files to determine wide or long format,
  # and protein level metric (NPX, Quant or Ct)
  data_cells_wide <- df |>
    dplyr::select(
      1L
    ) |>
    dplyr::slice(
      2L
    ) |>
    # convert to character to simplify operations below
    as.character()

  data_cells_long <- df |>
    dplyr::slice_head(
      n = 1L
    ) |>
    # convert to character to simplify operations below
    as.character()

  # Determine long or wide format from file ----

  is_data_wide <- grepl(pattern = paste(quant_methods, collapse = "|"),
                        x = data_cells_wide,
                        ignore.case = FALSE)
  is_data_long <- grepl(pattern = paste(quant_methods, collapse = "|"),
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

  # Check if the detected file format matches the customer input ----

  if (!is.null(long_format)) {

    # check if the format from the long_format argument input matches what we
    # detected in the data file
    if (is.null(detected_long_format)) {

      # warning if we were unable to determine format from the file but the user
      # has already provided it as input.
      cli::cli_warn(
        message = c(
          "Unable to confirm the \"{ifelse(long_format,\"long\",\"wide\")}\"
          format from the input file: {.file {file}}.",
          "i" = "We trust your input!"
        ),
        call = rlang::caller_env(),
        wrap = FALSE
      )

    } else if (long_format != detected_long_format) {

      # warning if what we detected and the user provided as input differ.
      cli::cli_warn(
        message = c(
          "Based on {.arg long_format} we were expecting
    \"{ifelse(long_format,\"long\",\"wide\")}\" format data, but instead
    detected \"{ifelse(detected_long_format,\"long\",\"wide\")}\".",
          "i" = "We trust your input!"
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

#' Help function to determine the Olink platform from the input Olink wide file.
#'
#' @author Klev Diamanti
#'
#' @param df Tibble containing the first three rows of the input Olink file in
#' wide format.
#' @param file The input excel file.
#' @param olink_platform The Olink platform used to generate the input file in
#' wide format. Expecting "Target 96", "Target 48", "Flex" or "Focus".
#' @param olink_platforms_wide Data frame from the global variable
#' accepted_olink_platforms.
#'
#' @return The name of the Olink platform detected automatically or set by the
#' user.
#'
read_npx_get_platform <- function(df,
                                  file,
                                  olink_platform = NULL,
                                  olink_platforms_wide) {

  # Checks inputs ----

  check_is_data_frame(df = df,
                      error = TRUE)

  check_file_exists(file = file,
                    error = TRUE)

  check_is_data_frame(df = olink_platforms_wide,
                      error = TRUE)

  # check olink platform
  if (!is.null(olink_platform)) {
    check_olink_platform(x = olink_platform,
                         broader_platform = "qPCR")
  }

  # Determine olink platform from input file ----

  # From cell B3 in the wide file read panel name

  check_columns(df = df, col_list = list("V2"))

  panel_name <- df |>
    dplyr::select(
      dplyr::all_of("V2")
    ) |>
    dplyr::slice(
      3L
    ) |>
    as.character()

  # run the platform-specific regular expression from the global variable to
  # determine that platform.
  olink_platform_df <- olink_platforms_wide |>
    dplyr::mutate(
      detected_platform = stringr::str_detect(
        string = panel_name,
        pattern = .data[["regexp"]]
      )
    ) |>
    dplyr::filter(
      .data[["detected_platform"]] == TRUE
    )

  # Confirm that detected platform matches user input ----

  if (is.null(olink_platform)) {
    # if olink_platform is NULL

    # check how many platforms match regular expression
    if (nrow(olink_platform_df) == 0L) {
      # if platform cannot be determined from input file throw an error

      cli::cli_abort(
        message = c(
          "x" = "Unable to recognize the Olink platform from the input file:
        {.file {file}}!",
          "i" = "Expected one of: {olink_platforms_wide$name}. Consider setting
        {.arg olink_platform}."
        ),
        call = rlang::caller_env(),
        wrap = FALSE
      )

    } else if (nrow(olink_platform_df) > 1L) {
      # if too many platform matches cannot from input file throw an error

      cli::cli_abort(
        message = c(
          "x" = "Unable to recognize the Olink platform from the input file:
        {.file {file}}!",
          "i" = "Too many matches from: {olink_platforms_wide$name}. Consider
        setting {.arg olink_platform}."
        ),
        call = rlang::caller_env(),
        wrap = FALSE
      )

    } else {

      # if data format input is NULL then set it from autodetect
      out_olink_platform <- olink_platform_df$name

    }

  } else {
    # if olink_platform is not NULL

    # check how many platforms match regular expression
    if (nrow(olink_platform_df) == 0L) {
      # if platform cannot be determined from input file throw a warning

      cli::cli_warn(
        message = c(
          "Unable to recognize the Olink platform from the input file:
        {.file {file}}! No matches!",
          "i" = "We trust your input!"
        ),
        call = rlang::caller_env(),
        wrap = FALSE
      )

    } else if (nrow(olink_platform_df) > 1L) {
      # if too many platform matches from input file throw a warning

      cli::cli_warn(
        message = c(
          "Unable to recognize the Olink platform from the input file:
        {.file {file}}! Too many matches!",
          "i" = "We trust your input!"
        ),
        call = rlang::caller_env(),
        wrap = FALSE
      )

    } else if (olink_platform != olink_platform_df$name) {

      cli::cli_warn(
        message = c(
          "Based on {.arg olink_platform} we were expecting Olink
          {olink_platform} data, but instead we detected
          {olink_platform_df$name} data.",
          "i" = "We trust your input!"
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

#' Help function to determine the the type of the quantification of the data in
#' the excel file.
#'
#' @author Klev Diamanti
#'
#' @param file The input excel file.
#' @param data_type The quantification in which the data comes in. Expecting one
#' of NPX, Quantified or Ct.
#' @param data_cells A character vector with the contents of the cells
#' indicating the quantification method.
#' @param quant_methods_expected Character vector with the Olink protein
#' quantification methods.
#'
#' @return The quantification method detected in the file.
#' An error is returned if the format cannot be determined.
#'
read_npx_get_quant <- function(file,
                               data_type = NULL,
                               data_cells,
                               quant_methods_expected) {

  # Checks inputs ----

  check_file_exists(file = file,
                    error = TRUE)

  check_is_character(string = data_cells,
                     error = TRUE)

  check_is_character(string = quant_methods_expected,
                     error = TRUE)

  # check data type
  if (!is.null(data_type)) {
    check_olink_data_type(x = data_type,
                          broader_platform = "qPCR")
  }

  # Determine quantification method from excel file ----

  # In wide format data_cells is of length 1 as we read only cell A2
  # the solution below is expected to catch all cases
  quant_matches <- sapply(
    X = quant_methods_expected,
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
      cols = dplyr::all_of(quant_methods_expected),
      names_to = "q_method",
      values_to = "is_in"
    ) |>
    dplyr::filter(
      .data[["is_in"]] == TRUE
    )

  # Confirm that detected quant method matches user input ----

  if (is.null(data_type)) {
    # if data_type is null

    if (nrow(quant_matches) == 0L) {
      # check if there are no matches

      cli::cli_abort(
        message = c(
          "x" = "Unable to recognize the quantification method from the input
        file: {.file {file}}!",
          "i" = "Expected one of: {quant_methods_expected} Consider setting
          {.arg data_type}."
        ),
        call = rlang::caller_env(),
        wrap = FALSE
      )

    } else if (nrow(quant_matches) > 1L) {
      # check if there are multiple matches

      cli::cli_abort(
        message = c(
          "x" = "Unable to recognize the quantification method from the input
        file: {.file {file}}!",
          "i" = "Too many occurrences of: {quant_methods_expected}. Consider
          setting {.arg data_type}."
        ),
        call = rlang::caller_env(),
        wrap = FALSE
      )

    } else {

      # if data_type input is NULL then set it from autodetect
      out_data_type <- quant_matches$q_method

    }

  } else {
    # if data_type is null

    # check how many platforms match regular expression
    if (nrow(quant_matches) == 0L) {
      # if quant method cannot be determined from input file throw a warning

      cli::cli_warn(
        message = c(
          "Unable to recognize the quantification method from the input file:
          {.file {file}}! No matches!",
          "i" = "We trust your input!"
        ),
        call = rlang::caller_env(),
        wrap = FALSE
      )

    } else if (nrow(quant_matches) > 1L) {
      # if too many quant method matches from input file throw a warning

      cli::cli_warn(
        message = c(
          "Unable to recognize the quantification method from the input file:
          {.file {file}}! Too many matches!",
          "i" = "We trust your input!"
        ),
        call = rlang::caller_env(),
        wrap = FALSE
      )

    } else if (data_type != quant_matches$q_method) {

      cli::cli_warn(
        message = c(
          "Based on {.arg data_type} we were expecting \"{data_type}\" format
          data, but instead detected \"{quant_matches$q_method}\".",
          "i" = "We trust your input!"
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
