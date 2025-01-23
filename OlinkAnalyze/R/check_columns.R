#' Check presence of columns in dataset.
#'
#' @description
#' Check if the input dataset (tibble or ArrowObject) \var{df} contains columns
#' specified in \var{col_list}. \var{col_list} supports both exact matches of
#' column names and alternative column names. In the latter case, alternative
#' column names are elements of a character vector, and exactly one of the
#' elements is required to be present.
#'
#' @author
#'   Klev Diamanti
#'   Albin Lundin
#'   Lei Conze
#'   Pascal Pucholt
#'   Gil Henriques
#'
#' @param df An Olink dataset (tibble or ArrowObject).
#' @param col_list A list of character vectors.
#'
#' @details
#' \var{col_list} contains a collection of character vectors. If a character
#' vector is scalar (length = 1), the element of the vector is expected to be
#' present among the column names of \var{df}. When an element of \var{col_list}
#' contains more than one elements, the function will check whether the column
#' names of \var{df} include \strong{at least one} of the elements of that
#' vector.
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

  check_is_arrow_or_tibble(df = df,
                           error = TRUE)

  check_is_list(lst = col_list,
                error = TRUE)

  # check that each element of the  col_list is a character vector
  col_list_char <- sapply(col_list, check_is_character, error = FALSE)
  if (any(col_list_char == FALSE)) {

    col_list_not_char <- col_list[!col_list_char] # nolint

    # error if lst is not a list
    cli::cli_abort(
      c(
        "x" = "{.arg col_list} contains {length(col_list_not_char)}
        {cli::qty(col_list_not_char)} element{?s} that {?is/are} not character
        vector{?s}!",
        "i" = "Non-character vector elements are located in index
        {cli::qty(col_list_not_char)} poisiton{?s}
        {seq_along(1L:length(col_list))[!col_list_char]} of {.arg col_list}."
      ),
      call = rlang::caller_env(),
      wrap = TRUE
    )

  }

  # Check columns ----

  # get column of the data frame
  df_column_names <- names(df)

  ## Columns that _must_ be present ----

  required_cols <- col_list[sapply(col_list, \(x) length(x) == 1L)] |>
    as.character()

  if (length(required_cols) > 0L) {

    missing_cols <- required_cols[!(required_cols %in% df_column_names)]

    if (length(missing_cols) > 0L) {

      cli::cli_abort(
        c(
          "x" = "The data frame {.arg {rlang::caller_arg(df)}} is missing the
        required columns: {.val {missing_cols}}!",
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

    missing_one <- option_cols[sapply(option_cols, \(x) !any(x %in% df_column_names))] # nolint

    if (length(missing_one) > 0L) {

      cli::cli_abort(
        c(
          "i" = "The data frame {.arg {rlang::caller_arg(df)}} is missing
          columns that should be present in at least one of the vectors:
          {.val {missing_one}}!",
          "i" = "Please make sure they are part of the input dataset."
        ),
        call = rlang::caller_env(),
        wrap = FALSE
      )

    }

  }

  return(invisible(NULL))
}
