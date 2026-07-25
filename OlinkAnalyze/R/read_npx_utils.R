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
#' @keywords internal
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

      cli::cli_abort( # nolint: return_linter
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

      cli::cli_abort( # nolint: return_linter
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
