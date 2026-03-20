#' Utility function removing columns with all values NA from a dataset.
#'
#' @author
#'   Klev Diamanti
#'
#' @param df An Olink dataset.
#'
#' @keywords internal
#'
#' @return The input Olink dataset without all-NA columns.
#'
remove_all_na_cols <- function(df) {

  # input check ----

  check_is_dataset(x = df,
                   error = TRUE)

  # identify all NA cols ----

  na_cols <- sapply(df, \(x) sum(is.na(x)) == nrow(df))
  na_cols <- na_cols[na_cols == TRUE]
  na_cols <- names(na_cols)

  # remove all NA cols ----

  if (length(na_cols) > 0L) {
    df <- df |>
      dplyr::select(
        -dplyr::all_of(na_cols)
      )
  }

  # return ----

  return(df)
}

#' Utility function that adds quotation marks on elements printed by
#' ansi_collapse from cli.
#'
#' @param x Character vector.
#' @param sep One of "or" and "and".
#'
#' @keywords internal
#'
#' @return Scalar character vector collapsed by "and" or "or".
#'
ansi_collapse_quot <- function(x,
                               sep = "and") {
  x_paste <- paste0("\"", x, "\"")

  if (sep == "or") {
    x <- cli::ansi_collapse(x = x_paste, sep2 = " or ", last = ", or ")
  } else {
    x <- cli::ansi_collapse(x = x_paste)
  }
  return(x)
}

#' Utility function to check OSI values for validity
#'
#' @param df An Olink dataset.
#' @param check_log Output log of check_npx(). Defaults to NULL.
#' @param osi_score Name of OSI column to check. Defaults to NULL.
#'
#' @keywords internal
#'
#' @return An Olink dataset with the OSI column checked and cleaned
#'
check_osi <- function(df,
                      check_log = NULL,
                      osi_score = NULL) {

  if (missing(df)) {
    cli::cli_abort(
      c(
        "x" = "Missing required argument {.arg df}!"
      ),
      call = rlang::caller_env(),
      wrap = TRUE
    )
  } else {
    check_is_dataset(x = df,
                     error = TRUE)
  }

  check_is_scalar_character(x = osi_score,
                            error = TRUE)

  osi_cat_cols <- c("OSICategory")
  osi_cont_cols <- c("OSITimeToCentrifugation",
                     "OSIPreparationTemperature",
                     "OSISummary")

  osi_cols <- c(osi_cat_cols, osi_cont_cols)

  if (is.null(osi_score) || !(osi_score %in% c(osi_cols))) {
    cli::cli_abort(
      c(
        "x" = "Invalid value for {.arg osi_score} = {.val {osi_score}}!",
        "i" = "Expected one of {.or {.val {osi_cols}}}."
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )
  }

  # Check if OSI columns are present in dataaset
  check_columns(df = df, col_list = list(osi_score))

  # Check if columns are all NA
  if (all(is.na(df[[osi_score]]))) {
    cli::cli_abort(
      c(
        "x" = "All values are 'NA' in the column {.val {osi_score}} of the
        dataset {.arg df}!",
        "i" = "Please check your {.arg df} to confirm OSI data is present."
      ),
      call = rlang::caller_env(),
      wrap = TRUE
    )
  }

  # Categorical checks
  if (osi_score %in% osi_cat_cols) {

    # Check for invalid values
    v_chr <- df |>
      dplyr::pull(.data[[osi_score]]) |>
      as.character() |>
      unique()

    # Check that values are in allowed range
    allowed <- as.character(0L:4L)
    invalid_vals <- unique(v_chr[!is.na(v_chr) & !(v_chr %in% allowed)])

    if (length(invalid_vals) > 0L) {
      cli::cli_abort(
        c(
          "x" = "Invalid values detected in column {.val {osi_score}}
          of {.arg df}!",
          "i" = "Expected one of {.or {.val {allowed}}}! Found:
          {.val {invalid_vals}}!"
        ),
        call = rlang::caller_env(),
        wrap = TRUE
      )
    }

    # Convert to factor if needed
    if (!is.factor(df[[osi_score]])) {
      df <- df |>
        dplyr::mutate(
          !!osi_score := factor(as.character(.data[[osi_score]]),
                                levels = allowed)
        )
    }
  }

  # Continuous checks
  if (osi_score %in% osi_cont_cols) {

    # Detect non-numeric entries (introduced NA after coercion)
    v_num <- suppressWarnings(as.numeric(df[[osi_score]]))

    # Check if numeric
    if (!all(is.numeric(df[[osi_score]]))) {

      non_numeric_idx <- which( # nolint: object_usage_linter
        !is.na(df[[osi_score]]) & is.na(v_num)
      )

      cli::cli_abort(
        c(
          "x" = "Non-numeric values detected in column {.val {osi_score}}
          of {.arg df}!",
          "i" = "Expected continuous numeric values between 0 and 1!
          Found: {.val {length(non_numeric_idx)}} non-numeric value{?s}!"
        ),
        call = rlang::caller_env(),
        wrap = TRUE
      )
    }

    # Detect out-of-range values
    out_of_range_idx <- which(
      !is.na(v_num) & (v_num < 0L | v_num > 1L)
    )

    if (length(out_of_range_idx) > 0L) {
      cli::cli_abort(
        c(
          "x" = "Out of range values detected in column {.val {osi_score}}
          of {.arg df}!",
          "i" = "Expected continuous numeric values between 0 and 1!
          Found: {.val {length(out_of_range_idx)}} out of range value{?s}!"
        ),
        call = rlang::caller_env(),
        wrap = TRUE
      )
    }
  }

  return(df)
}

#' Common parameters for check functions.
#'
#' @author
#'   Klev Diamanti
#'
#' @param x Variable to check.
#' @param error Scalar boolean to return an error instead of a `FALSE`
#' (`default = FALSE`).
#'
#' @keywords internal
#'
#' @return Boolean, `TRUE` or `FALSE`, if the variable is of the correct class;
#' If output is `FALSE` and \var{error} = `TRUE`, an error is thrown.
#'
#' @seealso
#'   \code{\link{check_is_character}}
#'   \code{\link{check_is_integer}}
#'   \code{\link{check_is_numeric}}
#'   \code{\link{check_is_boolean}}
#'   \code{\link{check_is_scalar_character}}
#'   \code{\link{check_is_scalar_integer}}
#'   \code{\link{check_is_scalar_numeric}}
#'   \code{\link{check_is_scalar_boolean}}
#'   \code{\link{check_is_tibble}}
#'   \code{\link{check_is_dataset}}
#'   \code{\link{check_is_arrow_object}}
#'   \code{\link{check_is_list}}
#'
.check_params <- function(x, error) {}

#' Common parameters for read_npx-related functions.
#'
#' @author
#'   Klev Diamanti
#'
#' @param filename Path to Olink software output file in wide or long format.
#' Expecting extensions `r get_file_ext_summary()`.
#' @param file Path to Olink software output file in wide or long format.
#' Expecting extensions `r get_file_ext_summary()`.
#' @param out_df The class of the output dataset. One of
#' `r ansi_collapse_quot(x = read_npx_df_output, sep = "or")`. Defaults to
#' "tibble".
#' @param long_format Boolean marking format of input file. One of `TRUE` for
#' long format and `FALSE` for wide format files. Defaults to `NULL` for
#' auto-detection.
#' @param olink_platform Olink platform used to generate the input file. One of
#' `r ansi_collapse_quot(x = accepted_olink_platforms$name, sep = "or")`.
#' Defaults to `NULL` for auto-detection.
#' @param data_type Quantification method of the input data. One of
#' `r ansi_collapse_quot(x = get_olink_data_types(), sep = "or")`. Defaults to
#' `NULL` for auto-detection.
#' @param .ignore_files Character vector of files included in the zip-compressed
#' Olink software output files that should be ignored. Used only for
#' zip-compressed input files (default = \emph{c("README.txt")}).
#' @param quiet Boolean to print a confirmation message when reading the input
#' file. Applies to excel or delimited input only. `TRUE` skips printing the
#' message, and `FALSE` otherwise.
#' @param legacy Boolean to run the legacy version of the read_npx function.
#' \strong{Important: should be used only to wide format files from Target 96 or
#' Target 48 with NPX Software version earlier than 1.8!} (default `FALSE`).
#'
#' @keywords internal
#'
#' @return Dataset,
#' `r ansi_collapse_quot(x = get_df_output_print(), sep = "or")`, with Olink
#' data in long format.
#'
.read_npx_args <- function(filename,
                           file,
                           out_df,
                           long_format,
                           olink_platform,
                           data_type,
                           .ignore_files,
                           quiet,
                           legacy) {}

#' Common parameters for downstream anaysis functions.
#'
#' @author
#'   Klev Diamanti
#'
#' @param df A `r ansi_collapse_quot(x = get_df_output_print(), sep = "or")`
#' from \code{\link{read_npx}}.
#' @param check_log A named list returned by [`check_npx()`]. If `NULL`,
#' [`check_npx()`] will be run internally using `df`.
#'
#' @keywords internal
#'
.downstream_fun_args <- function(df,
                                 check_log) {}
