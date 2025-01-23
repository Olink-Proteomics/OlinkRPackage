#' Help function checking that the requested output class of the read_npx*
#' functions is acceptable.
#'
#' @author
#'   Klev Diamanti
#'
#' @param out_df Class of output data frame. One of `r get_df_output()`.
#'
#' @return Error if \var{out_df} is not one of `r get_df_output()`.
#'
check_out_df_arg <- function(out_df) {

  # check that out_df is a string
  check_is_scalar_character(string = out_df,
                            error = TRUE)

  if (!(out_df %in% read_npx_df_output)) {

    cli::cli_abort(
      message = c(
        "x" = "Unknown output argument {.val {out_df}}!",
        "i" = "Expecting one of {.val {read_npx_df_output}}"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

}

#' Help function converting the output data frame from a read_npx* function to
#' `r get_df_output_print()`.
#'
#' @author
#'   Klev Diamanti
#'
#' @param df Dataset to be converted.
#' @param out_df Class of output data frame. One of `r get_df_output()`.
#'
#' @return The dataset in the requested class.
#'
convert_read_npx_output <- function(df,
                                    out_df) {

  # check that out_df is ok
  check_out_df_arg(out_df = out_df)

  if (check_is_dataset(df = df, error = FALSE)) {

    if (out_df == "tibble") {

      return(dplyr::as_tibble(df))

    } else if (out_df == "arrow") {

      return(arrow::as_arrow_table(df))

    }

  } else {

    # if nont of the above throw an error
    cli::cli_abort(
      message = c(
        "x" = "Unexpected input dataset {.arg df}!",
        "i" = "Expecting: { cli::ansi_collapse(x = read_npx_df_output,
                                               sep2 = \" or \",
                                               last = \", or \") }"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

}

#' Help function checking that the olink_platform is acceptable.
#'
#' @author
#'   Klev Diamanti
#'
#' @param x The name of the Olink platform. One of
#' `r cli::ansi_collapse(x = accepted_olink_platforms$name)`.
#' @param broader_platform Name of the broader Olink platform. One of
#' `r unique(accepted_olink_platforms$broader_platform) |> cli::ansi_collapse()`. # nolint
#'
#' @return
#' `NULL` if platform is ok, otherwise an error.
#'
check_olink_platform <- function(x,
                                 broader_platform = NULL) {

  # input check ----

  check_is_scalar_character(string = x,
                            error = TRUE)

  if (!is.null(broader_platform)) {
    check_olink_broader_platform(x = broader_platform)
  }

  # check platform ----

  # filter the global variable accepted_olink_platforms to have a collection
  # of platforms available.
  if (is.null(broader_platform)) {

    olink_platforms <- accepted_olink_platforms

  } else {

    olink_platforms <- accepted_olink_platforms |>
      dplyr::filter(.data[["broader_platform"]] == .env[["broader_platform"]])

  }

  # Throw an error if unexpected platform
  if (!(x %in% olink_platforms$name)) {

    cli::cli_abort(
      message = c(
        "x" = "Unexpected Olink platform {.arg {rlang::caller_arg(x)}}!",
        "i" = "Expected one of: {olink_platforms$name}"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

}

#' Help function checking that the Olink data_type is acceptable.
#'
#' @author
#'   Klev Diamanti
#'
#' @param x The name of the Olink data type. One of
#' `r accepted_olink_platforms$quant_method |> unlist() |> unique() |> sort() |> cli::ansi_collapse(sep2 = " or ", last = ", or ")`. # nolint
#' @param broader_platform Name of the broader Olink platform. One of
#' `r unique(accepted_olink_platforms$broader_platform) |> cli::ansi_collapse(sep2 = " or ", last = ", or ")`. # nolint
#'
#' @return
#' `NULL` if data_type is ok, otherwise an error.
#'
check_olink_data_type <- function(x,
                                  broader_platform = NULL) {

  # input check ----

  check_is_scalar_character(string = x,
                            error = TRUE)

  if (!is.null(broader_platform)) {
    check_olink_broader_platform(x = broader_platform)
  }

  # check data_type ----

  # filter the global variable accepted_olink_platforms to have a collection
  # of data types available.
  if (is.null(broader_platform)) {

    olink_quant_methods <- accepted_olink_platforms

  } else {

    olink_quant_methods <- accepted_olink_platforms |>
      dplyr::filter(
        .data[["broader_platform"]] == .env[["broader_platform"]]
      )
  }

  olink_quant_methods <- olink_quant_methods |>
    dplyr::pull(
      .data[["quant_method"]]
    ) |>
    unlist() |>
    unique()

  # Throw an error if unexpected data_type
  if (!(x %in% olink_quant_methods)) {

    cli::cli_abort(
      message = c(
        "x" = "Unexpected Olink data type {.arg {rlang::caller_arg(x)}}!",
        "i" = "Expected one of: {olink_quant_methods}"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

}

#' Help function checking that the broader Olink platform is acceptable.
#'
#' @author
#'   Klev Diamanti
#'
#' @param x Name of the broader Olink platform. One of
#' `r unique(accepted_olink_platforms$broader_platform) |> cli::ansi_collapse(sep2 = " or ", last = ", or ")`. # nolint
#'
#' @return
#' `NULL` if broader Olink platform is ok, otherwise an error.
#'
check_olink_broader_platform <- function(x) {

  # input check ----

  check_is_scalar_character(string = x,
                            error = TRUE)

  # check broader platform ----

  if (!(x %in% unique(accepted_olink_platforms$broader_platform))) {

    cli::cli_abort(
      message = c(
        "x" = "Unexpected Olink broader platform
        {.arg {rlang::caller_arg(x)}}!",
        "i" = "Expected one of:
        {unique(accepted_olink_platforms$broader_platform)}"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

}

#' Help function checking whether a data set contains NA or empty strings on
#' its column names
#'
#' @author
#'   Klev Diamanti
#'
#' @param df Tibble or ArrowObject with Olink data in wide or long format.
#' @param file Path to Olink software output file in wide or long format.
#' Expecting file extensions
#' `r cli::ansi_collapse(x = accepted_npx_file_ext, sep2 = " or ", last = ", or ")`. # nolint
#'
#' @return Error is file contains problematic column names. `NULL` otherwise.
#'
read_npx_format_colnames <- function(df,
                                     file) {

  # check input ----
  check_is_dataset(df = df,
                   error = TRUE)

  # check columns names ----

  # check if column names are correct
  # in wide format we expect only cells A1 and B1 to be populated
  # in long format no column names should not be empty
  if (all(grepl(pattern = "^V", x = names(df)))) { # wide format

    # get first row of df
    df_row_1 <- df |>
      dplyr::slice_head(n = 1L) |>
      dplyr::collect()

    num_of_cells_with_vals <- ncol(df_row_1) -
      sum(is.na(df_row_1)) - sum(df_row_1 == "", na.rm = TRUE)

    if (ncol(df_row_1) < 3L || num_of_cells_with_vals != 2L) {

      cli::cli_abort(
        message = c(
          "x" = "Unexpected first row in file {.file {file}}!",
          "i" = "Detected file in wide format. Expected only cells in A1 and B1
          to be populated."
        ),
        call = rlang::caller_env(),
        wrap = FALSE
      )

    }

  } else { # long format

    if (any(names(df) == "") == TRUE
        || check_is_character(string = names(df),
                              error = FALSE) == FALSE) {

      cli::cli_abort(
        message = c(
          "x" = "Unexpected columns in file {.file {file}}!",
          "i" = "The dataset contains column names that are `NA` or `empty
          string` (\"\")."
        ),
        call = rlang::caller_env(),
        wrap = FALSE
      )

    }

  }

}
