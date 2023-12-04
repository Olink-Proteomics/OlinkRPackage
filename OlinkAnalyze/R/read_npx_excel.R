#' Help function to read Olink excel long or wide format files.
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
#' @param file The input excel file.
#' @param out_df The class of output data frame to be returned. Accepted values
#' are "tibble" (default) and "arrow".
#' @param long_format Boolean marking if input file is in long (TRUE) or wide
#' (FALSE) format.
#' @param olink_platform The Olink platform used to generate the input file.
#' Expecting "Target 96", "Target 48", "Flex" or "Focus".
#' @param data_type The quantification in which the data comes in. Expecting one
#' of NPX, Quantified or Ct.
#' @param quiet Print a confirmation message after reading in the input file.
#'
#' @return  A tibble in long format or an ArrowObject.
#'
read_npx_excel <- function(file,
                           out_df = "tibble",
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

  ## prep input ----

  # filter the global variable accepted_olink_platforms to have a collection
  # of platforms and quant methods available.
  # Note that only Target platforms can report data in excel.
  olink_platforms_excel <- accepted_olink_platforms |>
    dplyr::filter(.data[["broader_platform"]] == "qPCR")

  # extract quantification methods for Target data
  quant_methods_excel <- olink_platforms_excel$quant_method |>
    unlist() |>
    unique()

  ## check that input data specifics are correct ----

  # check long format input
  if (!is.null(long_format)) {

    check_is_scalar_boolean(bool = long_format,
                            error = TRUE)

  }

  # check olink platform
  if (!is.null(olink_platform)) {

    check_is_scalar_character(string = olink_platform,
                              error = TRUE)

    # Throw an error if unexpected platform
    if (!(olink_platform %in% olink_platforms_excel$name)) {

      cli::cli_abort(
        message = c(
          "x" = "Unexpected value for {.arg olink_platform}!",
          "i" = "Should be one of: {olink_platforms_excel$name}"
        ),
        call = rlang::caller_env(),
        wrap = FALSE
      )

    }

  }

  # check data type
  if (!is.null(data_type)) {

    check_is_scalar_character(string = data_type,
                              error = TRUE)

    # Throw an error if unexpected quantification method
    if (!(data_type %in% quant_methods_excel)) {

      cli::cli_abort(
        message = c(
          "x" = "Unexpected value for {.arg data_type}!",
          "i" = "Should be one of: {quant_methods_excel}"
        ),
        call = rlang::caller_env(),
        wrap = FALSE
      )

    }

  }

  # Determine data format, Olink platform and quant method from excel file ----

  ## Determine long or wide format ----

  file_format_check <- read_npx_excel_format(
    file = file,
    long_format = long_format,
    quant_methods_excel = quant_methods_excel
  )

  ## Determine Olink platform ----

  file_olink_platform <- read_npx_excel_platform( # nolint object_usage_linter
    file = file,
    olink_platform = olink_platform,
    is_long_format = file_format_check$is_long_format,
    olink_platforms_excel = olink_platforms_excel
  )

  ## Determine quantification method ----

  # auto-detect quantification method
  file_quant_method <- read_npx_excel_quant( # nolint object_usage_linter
    file = file,
    data_type = data_type,
    data_cells = file_format_check$data_cells,
    quant_methods_expected = olink_platforms_excel |>
      dplyr::filter(.data[["name"]] == .env[["file_olink_platform"]]) |>
      dplyr::pull(.data[["quant_method"]]) |>
      unlist()
  )

  # Message of data detection ----

  if (!quiet) {

    cli::cli_inform(
      message = c(
        "Detected \"{file_quant_method}\" data from
        \"Olink {file_olink_platform}\" in
        {ifelse(file_format_check$is_long_format, \"long\", \"wide\")}
        format!"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

}

#' Help function to determine the wide/long excel file format.
#'
#' @author Klev Diamanti
#'
#' @param file The input excel file.
#' @param long_format Boolean marking if input file is in long (TRUE) or wide
#' (FALSE) format.
#' @param quant_methods_excel Character vector with the Olink protein
#' quantification methods.
#'
#' @return A list of two variables: is_long_format and data_cells The first one
#' is a boolean noting the file format and the second one is the character
#' vector from the input file where the format was determined from.
#' An error is returned if the format cannot be determined, unless the user has
#' defined the file format from the input.
#'
read_npx_excel_format <- function(file,
                                  long_format = NULL,
                                  quant_methods_excel) {

  # Check inputs ----

  check_file_exists(file = file, error = TRUE)
  check_is_character(string = quant_methods_excel, error = TRUE)

  # check long format input
  if (!is.null(long_format)) {

    check_is_scalar_boolean(bool = long_format,
                            error = TRUE)

  }

  # Read in cells to determine format ----

  # Read specific cells of the excel files to determine wide or long format,
  # and protein level metric (NPX, Quant or Ct)
  data_cells_wide <- readxl::read_excel(path = file,
                                        range = "A2",
                                        col_names = FALSE,
                                        .name_repair = "minimal") |>
    # convert to character to simplify operations below
    as.character()

  data_cells_long <- readxl::read_excel(path = file,
                                        range = "L1:O1",
                                        col_names = FALSE,
                                        .name_repair = "minimal") |>
    # convert to character to simplify operations below
    as.character()

  # Determine long or wide format from file ----

  is_data_wide <- grepl(pattern = paste(quant_methods_excel, collapse = "|"),
                        x = data_cells_wide,
                        ignore.case = FALSE)
  is_data_long <- grepl(pattern = paste(quant_methods_excel, collapse = "|"),
                        x = data_cells_long,
                        ignore.case = FALSE)

  if (!is_data_wide && any(is_data_long)) {
    # in long format files we expect the quantification method to appear in
    # cells L1:O1 and we also expect no matches to cell A2. This is what marks
    # wide format files.

    detected_long_format <- TRUE
    data_cells <- data_cells_long

  } else if (is_data_wide && length(data_cells_long) == 0L) {
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
          "x" = "Unable to recognize the format of the excel file:
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
          format from the excel file: {.file {file}}.",
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

#' Help function to determine the Olink platform from the excel file.
#'
#' @author Klev Diamanti
#'
#' @param file The input excel file.
#' @param olink_platform The Olink platform used to generate the input file.
#' Expecting "Target 96", "Target 48", "Flex" or "Focus".
#' @param is_long_format Boolean marking if input file is in long (TRUE) or wide
#' (FALSE) format.
#' @param olink_platforms_excel Data frame from the global variable
#' accepted_olink_platforms.
#'
#' @return The name of the Olink platform detected automatically or set by the
#' user.
#'
read_npx_excel_platform <- function(file,
                                    olink_platform = NULL,
                                    is_long_format,
                                    olink_platforms_excel) {

  # Checks inputs ----

  check_file_exists(file = file, error = TRUE)
  check_is_scalar_boolean(bool = is_long_format, error = TRUE)
  check_is_tibble(df = olink_platforms_excel, error = TRUE)

  # check olink platform
  if (!is.null(olink_platform)) {

    check_is_scalar_character(string = olink_platform,
                              error = TRUE)

    # Throw an error if unexpected platform
    if (!(olink_platform %in% olink_platforms_excel$name)) {

      cli::cli_abort(
        message = c(
          "x" = "Unexpected value for {.arg olink_platform}!",
          "i" = "Should be one of: {olink_platforms_excel$name}"
        ),
        call = rlang::caller_env(),
        wrap = FALSE
      )

    }

  }

  # Determine olink platform from input file ----

  # Based on data format read the applicable cell to determine the platform
  panel_name <- readxl::read_excel(path = file,
                                   range = ifelse(is_long_format, "G2", "B3"),
                                   col_names = FALSE,
                                   .name_repair = "minimal") |>
    as.character()

  # run the platform-specific regular expression from the global variable to
  # determine that platform.
  olink_platform_df <- olink_platforms_excel |>
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
          "x" = "Unable to recognize the Olink platform from the excel file:
        {.file {file}}!",
          "i" = "Expected one of: {olink_platforms_excel$name}. Consider setting
        {.arg olink_platform}."
        ),
        call = rlang::caller_env(),
        wrap = FALSE
      )

    } else if (nrow(olink_platform_df) > 1L) {
      # if too many platform matches cannot from input file throw an error

      cli::cli_abort(
        message = c(
          "x" = "Unable to recognize the Olink platform from the excel file:
        {.file {file}}!",
          "i" = "Too many matches from: {olink_platforms_excel$name}. Consider
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
          "Unable to recognize the Olink platform from the excel file:
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
          "Unable to recognize the Olink platform from the excel file:
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
read_npx_excel_quant <- function(file,
                                 data_type = NULL,
                                 data_cells,
                                 quant_methods_expected) {

  # Checks inputs ----

  check_file_exists(file = file, error = TRUE)
  check_is_character(string = data_cells, error = TRUE)
  check_is_character(string = quant_methods_expected, error = TRUE)

  # check data type
  if (!is.null(data_type)) {

    check_is_scalar_character(string = data_type,
                              error = TRUE)

    # Throw an error if unexpected quantification method
    if (!(data_type %in% quant_methods_expected)) {

      cli::cli_abort(
        message = c(
          "x" = "Unexpected value for {.arg data_type}!",
          "i" = "Should be one of: {quant_methods_expected}"
        ),
        call = rlang::caller_env(),
        wrap = FALSE
      )

    }

  }

  # Determine quantification method from excel file ----

  # If the format is long then data_cells should have length 4 as we read cells
  # L1 to O1.
  #
  # If the format is wide then data_cells is of length 1 as we read only cell A2
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
          "x" = "Unable to recognize the quantification method from the excel
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
          "x" = "Unable to recognize the quantification method from the excel
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
          "Unable to recognize the quantification method from the excel file:
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
          "Unable to recognize the quantification method from the excel file:
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
