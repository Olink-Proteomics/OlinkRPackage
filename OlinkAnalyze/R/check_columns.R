#' Check that an input data frame contains the required columns
#'
#' `check_columns` checks if the data frame `data` contains all of the columns
#' in the list `col_list`.
#'
#' @author
#'   Klev Diamanti
#'   Albin Lundin
#'   Lei Conze
#'   Pascal Pucholt
#'   Gil Henriques
#'
#' @param df A data frame.
#' @param col_list A list, where each element is a character vector (usually of
#' length one). The function will check whether the column names of `data`
#' include the elements of `col_list`. When an element of `col_list` is a vector
#' of length higher than one, the function will check whether the column names
#' of `data` include *at least one* of the elements of that vector.
#'
#' @examples
#' \dontrun{
#' tmp_data <- dplyr::tibble(
#'   "A" = c(1,2,3),
#'   "B" = c(T,T,F),
#'   "C" = c("A", "B", "C"),
#'   "D" = c(F,F,T)
#' )
#'
#' # OK
#' check_columns(df = tmp_data,
#'               col_list = list("A", "B"))
#'
#' # ERROR: E is missing
#' check_columns(df = tmp_data,
#'               col_list = list("A", "E"))
#'
#' # ERROR: E and F are missing
#' check_columns(df = tmp_data,
#'               col_list = list("A", "E", "F"))
#'
#' # OK
#' check_columns(df = tmp_data,
#'               col_list = list("A", c("B", "C")))
#'
#' # OK
#' check_columns(df = tmp_data,
#'               col_list = list("A", c("B", "E")))
#'
#' # ERROR: c(F, E) are missing
#' check_columns(df = tmp_data,
#'               col_list = list("A", c("F", "E")))
#'
#' # ERROR: c(F, E) and c(M, N) are missing
#' check_columns(df = tmp_data,
#'               col_list = list("A", c("F", "E"), c("M", "N")))
#' }
#'
#' @returns
#' Nothing or an error message if any column is missing.
#'
check_columns <- function(df,
                          col_list) {

  # Check input ----
  check_is_data_frame(df = df,
                      error = TRUE)
  check_is_list(lst = col_list,
                error = TRUE)

  # check that each element of the  col_list is a character vector
  col_list_char <- sapply(col_list, check_is_character, error = FALSE)
  if (any(col_list_char == FALSE)) {

    col_list_not_char <- col_list[!col_list_char] # nolint object_usage_linter

    # error if lst is not a list
    cli::cli_abort(
      c(
        "x" = "{.arg col_list} contains { length(col_list_not_char) }
        element{?s} that {?is/are} not character vector{?s}!",
        "i" = "Incorrect elements are located in index poisiton
        { seq_along(1L:length(col_list))[!col_list_char] } of {.arg col_list}."
      ),
      call = rlang::caller_env(),
      wrap = TRUE
    )

  }

  # Check columns ----

  # get column of the data frame
  df_column_names <- colnames(df)

  ## Columns that _must_ be present ----

  required_cols <- col_list[sapply(col_list, \(x) length(x) == 1L)] |>
    as.character()

  if (length(required_cols) > 0L) {

    missing_cols <- required_cols[!(required_cols %in% df_column_names)]

    if (length(missing_cols) > 0L) {

      cli::cli_abort(
        c(
          "x" = "The data frame {.arg {rlang::caller_arg(df)}} is missing the
        required columns: {missing_cols}!",
          "i" = "Please make sure they are part of the input dataset."
        ),
        call = rlang::caller_env(),
        wrap = TRUE
      )

    }

  }

  ## Columns such that _at least one_ must be present ----

  option_cols <- col_list[sapply(col_list, \(x) length(x) > 1L)]

  if (length(option_cols) > 0L) {

    missing_one <- option_cols[sapply(option_cols, \(x) !any(x %in% df_column_names))] # nolint object_usage_linter

    if (length(missing_one) > 0L) {

      cli::cli_abort(c(
        "i" = "The data frame {.arg {rlang::caller_arg(df)}} is missing at least
        one of the vectors of required columns: { missing_one }!",
        "i" = "Please make sure they are part of the input dataset."
      ),
      call = NULL,
      wrap = FALSE
      )

    }

  }

  return(invisible(NULL))
}
