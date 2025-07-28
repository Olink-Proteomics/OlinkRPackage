#' Help function checking that the requested output class of the read_npx*
#' functions is acceptable.
#'
#' @author
#'   Klev Diamanti
#'
#' @inheritParams .read_npx_args
#'
#' @return Error if \var{out_df} is not one of
#' `r ansi_collapse_quot(x = read_npx_df_output, sep = "and")`.
#'
check_out_df_arg <- function(out_df) {

  # check that out_df is a string
  check_is_scalar_character(x = out_df,
                            error = TRUE)

  if (!(out_df %in% read_npx_df_output)) {

    cli::cli_abort( # nolint return_linter
      message = c(
        "x" = "Unknown output argument {.arg out_df}!",
        "i" = "Expected one of {.val {read_npx_df_output}}"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

}

#' Help function converting the output dataset from read_npx* functions to
#' `r ansi_collapse_quot(x = get_df_output_print(), sep = "or")`.
#'
#' @author
#'   Klev Diamanti
#'
#' @inheritParams .read_npx_args
#' @inheritParams .downstream_fun_args
#'
#' @return The dataset in the requested class.
#'
convert_read_npx_output <- function(df,
                                    out_df) {

  # check that out_df is ok
  check_out_df_arg(out_df = out_df)

  if (check_is_dataset(x = df, error = FALSE)) {

    if (out_df == "tibble") {

      return(dplyr::as_tibble(df))

    } else if (out_df == "arrow") {

      return(arrow::as_arrow_table(df))

    }

  } else {

    # if nont of the above throw an error
    cli::cli_abort( # nolint return_linter
      message = c(
        "x" = "Unexpected input dataset {.arg df}!",
        "i" = "Expected one of: {.val {read_npx_df_output}}."
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )

  }

}

#' Help function checking whether a dataset contains NA or empty strings on
#' its column names
#'
#' @author
#'   Klev Diamanti
#'
#' @inheritParams .read_npx_args
#' @inheritParams .downstream_fun_args
#'
#' @return Error is file contains problematic column names. `NULL` otherwise.
#'
read_npx_format_colnames <- function(df,
                                     file) {

  # check input ----
  check_is_dataset(x = df,
                   error = TRUE)

  # check columns names ----

  # check if column names are correct
  # in wide format we expect only cells A1 and B1 to be populated
  # in long format no column names should not be empty
  if (all(grepl(pattern = "^V", x = names(df))) && ncol(df) > 1L) { # wideformat

    # get first row of df
    df_row_1 <- df |>
      dplyr::slice_head(n = 1L) |>
      dplyr::collect()

    num_of_cells_with_vals <- ncol(df_row_1) -
      sum(is.na(df_row_1)) - sum(df_row_1 == "", na.rm = TRUE)

    if (ncol(df_row_1) < 3L || num_of_cells_with_vals != 2L) {

      cli::cli_abort( # nolint return_linter
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
        || check_is_character(x = names(df),
                              error = FALSE) == FALSE) {

      cli::cli_abort( # nolint return_linter
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
