#' S3 class for Olink NPX data with attached check log
#'
#' @description
#' The `olink_class` class is a tibble subclass that carries the output of
#' [`check_npx()`] as an attribute. This allows downstream functions to
#' automatically access the check log without the user having to pass it
#' explicitly.
#'
#' For ArrowObjects, check log metadata is stored in the Arrow table's
#' schema-level metadata under the key `"olink_check_log"` as a serialized
#' JSON string.
#'
#' @details
#' The class is designed to be fully backward-compatible. All downstream
#' functions that accept `check_log = NULL` will first attempt to extract the
#' check log from the data object itself. If not found, they fall back to the
#' existing behavior of checking if it was provided by the user through
#' `check_log`, and if that is also not found it will run [`check_npx()`]
#' internally.
#'
#' The `olink_class` class survives most dplyr operations (e.g.
#' [`dplyr::filter()`], [`dplyr::mutate()`], [`dplyr::select()`]) through the
#' [`dplyr::dplyr_reconstruct()`] mechanism. Note that operations that combine
#' multiple data frames (e.g. [`dplyr::bind_rows()`]) may strip the class and
#' check log.
#'
#' @name olink_class
#' @keywords internal
#'
NULL

# Constructor ----

#' Create an `olink_class` object from a tibble and a check log
#'
#' @description
#' Attaches the result of [`check_npx()`] to a tibble as an attribute,
#' creating an `olink_class` subclass. This allows downstream functions to
#' automatically extract the check log from the data.
#'
#' @param df A tibble containing Olink NPX data.
#' @param check_log A named list returned by [`check_npx()`].
#'
#' @return An object of class `olink_class`, which inherits from
#' `tbl_df`, `tbl`, and `data.frame`, with the check log stored as an
#' attribute.
#'
#' @seealso [olink_extract_check_log()] for retrieving the check log from an
#' object.
#'
#' @export
#'
new_olink_class <- function(df,
                            check_log) {

  # validate inputs
  check_is_tibble(x = df, error = TRUE)

  # check that we are not overwriting an existing olink_class S3 object
  if (rlang::inherits_any(x = df, class = "olink_class")) {
    cli::cli_warn(
      c(
        "{.arg df} is already an {.cls olink_class} object!",
        "i" = "Use {.fn rm_check_log} to remove the existing check log before
        creating a new one."
      ),
      call = rlang::caller_env()
    )
  }

  validate_check_log(df = df, check_log = check_log)

  # construct the subclass
  df_olink <- tibble::new_tibble(
    x = df,
    check_log = check_log,
    class = "olink_class"
  )

  return(df_olink)
}

# Validator ----

#' Validate a check log list
#'
#' @description
#' Checks that `check_log` is a named list with all elements expected by
#' the package internals.
#'
#' @param df A tibble or ArrowObject containing Olink NPX data.
#' @param check_log A named list returned by [`check_npx()`].
#'
#' @return `TRUE` invisibly if validation passes, otherwise throws an error.
#'
#' @keywords internal
#'
validate_check_log <- function(df, check_log) {

  check_is_list(x = check_log, error = TRUE)

  # check that check_log has names ----

  if (is.null(names(check_log))) {
    cli::cli_abort(
      c(
        "x" = "{.arg check_log} is a list with no names!",
        "i" = "Ensure that {.arg check_log} is the output of {.fn check_npx}
        for dataset {.arg df}!"
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )
  }

  # check that all expected elements in check_log are in place ----

  check_log_missing <- setdiff(
    x = check_npx_lst_names,
    y = names(check_log)
  )
  if (length(check_log_missing) > 0L) {
    cli::cli_abort(
      c(
        "x" = "Element{?s} {.val {check_log_missing}} are missing from
        {.arg check_log}!",
        "i" = "Ensure that {.arg check_log} is the output of {.fn check_npx}
        for dataset {.arg df}!"
      ),
      call = rlang::caller_env()
    )
  }

  # check if check_log contains additional elements ----

  check_log_additional <- setdiff(
    x = names(check_log),
    y = check_npx_lst_names
  )
  if (length(check_log_additional) > 0L) {
    cli::cli_abort(
      c(
        "x" = "Additional element{?s} {.val {check_log_additional}} detected in
        {.arg check_log}!",
        "i" = "Ensure that {.arg check_log} is the output of {.fn check_npx}
        for dataset {.arg df}!"
      ),
      call = rlang::caller_env()
    )
  }

  ## check that df column names are in place ----

  # missing required column keys
  check_log_cnames_missing <- setdiff(
    x = column_name_dict |> # required column names
      dplyr::filter(
        .data[["col_miss"]] == FALSE
      ) |>
      dplyr::pull(
        .data[["col_key"]]
      ),
    y = names(check_log$col_names)
  )
  if (length(check_log_cnames_missing) > 0L) {
    df_req_cols_miss <- column_name_dict |>
      dplyr::filter(
        .data[["col_miss"]] == FALSE
        & .data[["col_key"]] %in% .env[["check_log_cnames_missing"]]
      )

    miss_cols <- paste0(
      "* \"", df_req_cols_miss$col_key, "\": One of ",
      sapply(df_req_cols_miss$col_names,
             ansi_collapse_quot,
             sep = "or"), "."
    )

    cli::cli_abort(
      c(
        "x" = "{cli::qty(df_req_cols_miss$col_key)} There {?is/are} no column
        name{?s} associated with the following key{?s}:",
        miss_cols,
        "i" = "Ensure that {.arg check_log} is the output of {.fn check_npx}
        for dataset {.arg df}!"
      ),
      call = rlang::caller_env()
    )
  }

  # additional unexpected column names ----

  check_log_cnames_additional <- setdiff(
    x = names(check_log$col_names),
    y = column_name_dict |> # all column names
      dplyr::pull(
        .data[["col_key"]]
      )
  )
  if (length(check_log_cnames_additional) > 0L) {
    cli::cli_abort(
      c(
        "x" = "Unexpected key{?s} {.val {check_log_cnames_additional}}
        corresponding to column names detected in {.arg check_log$col_names}!",
        "i" = "Ensure that {.arg check_log} is the output of {.fn check_npx}
        for dataset {.arg df}!"
      ),
      call = rlang::caller_env()
    )
  }

  # check that actual column names are in place - sort of security check that
  # check_log corresponds to the current df
  check_log_cols_miss <- setdiff(
    x = unlist(x = check_log$col_names,
               recursive = TRUE,
               use.names = FALSE),
    y = names(df)
  )
  if (length(check_log_cols_miss) > 0L) {
    cli::cli_abort(
      c(
        "x" = "Column name{?s} {.val {check_log_cols_miss}} from
        {.arg check_log} {?is/are} missing from the dataset {.arg df}!",
        "i" = "Ensure that {.arg check_log} is the output of {.fn check_npx}
        for dataset {.arg df}!"),
      call = rlang::caller_env(),
      wrap = FALSE
    )
  }

  return(invisible(TRUE))

}

# Accessor ----

#' Retrieve the check log from an Olink data object
#'
#' @description
#' Extracts the check log from an `olink_class` tibble or an ArrowObject with
#' embedded metadata. Returns `NULL` if no check log is found.
#'
#' @param df A data object, typically from [`read_npx()`], [`clean_npx()`] or
#' [`olink_normalization()`].
#'
#' @return A named list (the check log) or `NULL` if no check log is attached.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # read and check data
#'   npx_file <- system.file(
#'     "extdata",
#'     "npx_data_ext.parquet",
#'     package = "OlinkAnalyze"
#'   )
#'
#'   npx_df <- OlinkAnalyze::read_npx(
#'     filename = npx_file
#'   )
#'
#'   check_log <- OlinkAnalyze::check_npx(
#'     df = npx_df
#'   )
#'
#'   # attach check_log to the data
#'   npx_obj <- OlinkAnalyze::new_olink_class(
#'     df = npx_df,
#'     check_log = check_log
#'   )
#'
#'   # retrieve check_log
#'   OlinkAnalyze::olink_extract_check_log(
#'     df = npx_obj
#'   )
#' }
#'
olink_extract_check_log <- function(df) {

  if (inherits(x = df, what = "olink_class")) {

    return(attr(x = df, which = "check_log", exact = TRUE))

  } else if (check_is_arrow_object(x = df, error = FALSE)) {

    check_log_encoded <- df$metadata[["olink_check_log"]]

    if (is.null(check_log_encoded)) {
      cli::cli_warn(
        c(
          "No {.arg olink_check_log} metadata found in the ArrowObject.",
          "i" = "Consider generating it using {.fn check_npx}."
        )
      )
      return(NULL)
    }

    # deserialize the base64-encoded string back to a list
    check_log <- deserialize_check_log(check_log_encoded)

    return(check_log)

  }

  return(NULL)

}

#' Convert an `olink_class` object to a plain tibble
#'
#' @description
#' Strips the `olink_class` class and removes the attached check log, returning
#' a plain tibble.
#'
#' @param x An `olink_class` object.
#' @param ... Additional arguments passed to `as_tibble()`.
#'
#' @return A tibble (`tbl_df`) without the `olink_class` class or the check log
#' attribute.
#'
#' @exportS3Method tibble::as_tibble
#'
#' @keywords internal
#' @noRd
#'
as_tibble.olink_class <- function(x, ...) { # nolint: object_name_linter

  attr(x = x, which = "check_log") <- NULL
  class(x) <- setdiff(x = class(x), y = "olink_class")

  x <- tibble::as_tibble(x, ...)

  return(x)

}

#' Remove the check log metadata from an ArrowObject
#'
#' @description
#' Strips the `olink_check_log` key from the schema-level metadata of an
#' ArrowObject, returning a plain Arrow table without any attached check log.
#'
#' @param df An ArrowObject (e.g. `arrow::Table` or `arrow::Dataset`).
#'
#' @return The ArrowObject with the `olink_check_log` metadata removed.
#'
#' @keywords internal
#' @noRd
#'
rm_check_log_arrow <- function(df) {

  check_is_arrow_object(x = df, error = TRUE)

  existing_metadata <- df$metadata

  if ("olink_check_log" %in% names(existing_metadata)) {
    existing_metadata[["olink_check_log"]] <- NULL
    df$metadata <- existing_metadata
  }

  return(df)

}

#' Strip check log from an Olink data object
#'
#' @description
#' Removes the attached check log from either an `olink_class` tibble or an
#' ArrowObject. For tibbles this converts the object to a plain tibble (removing
#' the `olink_class` class and the `check_log` attribute). For ArrowObjects it
#' removes the `olink_check_log` metadata key. If the input carries no check
#' log, it is returned unchanged.
#'
#' @param df An `olink_class` tibble or an ArrowObject with `olink_check_log`
#' metadata.
#'
#' @return The data without the check log: a plain tibble (`tbl_df`) when given
#' an `olink_class` tibble, or the ArrowObject with the `olink_check_log`
#' metadata key removed when given an ArrowObject. Other inputs are returned
#' unchanged.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # read NPX data (returns olink_class object)
#'   npx_file <- system.file(
#'     "extdata",
#'     "npx_data_ext.parquet",
#'     package = "OlinkAnalyze"
#'   )
#'
#'   npx_obj <- OlinkAnalyze::read_npx(
#'     filename = npx_file
#'   )
#'
#'   # strip check log, returning a plain tibble
#'   npx_tbl <- OlinkAnalyze::rm_check_log(
#'     df = npx_obj
#'   )
#'
#'   class(npx_tbl)
#'
#'   OlinkAnalyze::olink_extract_check_log(
#'     df = npx_tbl
#'   )
#' }
#'
rm_check_log <- function(df) {

  if (inherits(x = df, what = "olink_class")) {

    return(as_tibble.olink_class(x = df))

  } else if (check_is_arrow_object(x = df, error = FALSE)) {

    return(rm_check_log_arrow(df = df))

  }

  return(df)

}

#' Update the attached check log for an Olink dataset
#'
#' @author Klev Diamanti
#'
#' @description
#' Refreshes the `check_log` attached to an `olink_class` tibble or ArrowObject.
#' This is useful after manually modifying or cleaning an Olink dataset, where
#' the attached check log may no longer describe the current data.
#'
#' The function can also be used to update only the column-name choices stored
#' in the attached check log by supplying `preferred_names`, even when the data
#' itself has not changed. The supplied names are validated by the same
#' machinery used by [`check_npx()`].
#'
#' If `df` already carries a check log, the existing log is used to preserve
#' current preferred column-name choices before the log is regenerated. If no
#' attached check log is found, `check_log` is used when supplied; otherwise
#' [`check_npx()`] is run on `df`. This means `update_check_log()` can also be
#' used to attach a check log to a plain tibble or ArrowObject, returning an
#' `olink_class` object for tibble input or an ArrowObject with
#' `olink_check_log` metadata for Arrow input.
#'
#' @inheritParams .downstream_fun_args
#' @inheritParams check_npx
#'
#' @return The input data in its original output format with an updated check
#' log attached. Tibbles are returned as `olink_class` objects and ArrowObjects
#' are returned with refreshed `olink_check_log` schema metadata.
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # get file
#' npx_file <- system.file(
#'   "extdata",
#'   "npx_data1.xlsx",
#'   package = "OlinkAnalyze"
#' )
#'
#' # Example 1: manually modify the dataset and update the check log accordingly
#'
#' # read file
#' npx_df <- OlinkAnalyze::read_npx(
#'   filename = npx_file,
#'   olink_platform = "Target 96"
#' )
#'
#' # manually cleanup the data based on the warning messages from read_npx
#' npx_df <- npx_df |>
#'   # remove duplicated samples
#'   dplyr::filter(
#'     !grepl("^CONTROL", .data[["SampleID"]])
#'   ) |>
#'   # convert NPX and LOD columns to numeric
#'   dplyr::mutate(
#'     NPX = as.numeric(.data[["NPX"]]),
#'     LOD = as.numeric(.data[["LOD"]])
#'   )
#' # same result achieved by using clean_npx
#'
#' # run update_check_log so that it describes the current status of the dataset
#' npx_df <- OlinkAnalyze::update_check_log(
#'   df = npx_df
#' )
#'
#' # Example 2: change preferred column names to be used in the analyses
#'
#' # update preferred column names without otherwise modifying the dataset
#' npx_df <- npx_df |>
#'   dplyr::mutate(
#'     PCNormalizedNPX = .data[["NPX"]]
#'   )
#'
#' npx_df <- OlinkAnalyze::update_check_log(
#'   df = npx_df,
#'   preferred_names = c("quant" = "PCNormalizedNPX")
#' )
#'
#' # Example 3: attach an existing check log to a plain tibble or ArrowObject
#'
#' # attach an existing check log to a plain tibble or ArrowObject
#' npx_tbl <- OlinkAnalyze::rm_check_log(
#'   df = npx_df
#' )
#' check_log <- OlinkAnalyze::check_npx(
#'   df = npx_df,
#'   preferred_names = c("quant" = "PCNormalizedNPX")
#' )
#'
#' # attach an existing check log to a plain tibble
#' npx_obj <- OlinkAnalyze::update_check_log(
#'   df = npx_tbl,
#'   check_log = check_log
#' )
#'
#' # attach an existing check log to an ArrowObject
#' npx_arrow <- npx_tbl |>
#'   arrow::as_arrow_table() |>
#'   OlinkAnalyze::update_check_log(
#'     check_log = check_log
#'   )
#'
#' # inspect ArrowObject has a check_log
#' names(npx_arrow$metadata)
#'
#' }
#'
update_check_log <- function(df,
                             check_log = NULL,
                             preferred_names = NULL) {

  check_is_dataset(x = df, error = TRUE)

  # get current check_log
  check_log <- get_check_npx(
    df = df,
    check_log = check_log,
    preferred_names = preferred_names
  )

  # get current preferred names from check_log
  existing_preferred_names <- get_preferred_names(
    df = df,
    check_log = check_log
  )

  # append new preferred_names, if provided, to the existing preferred_names
  # and remove duplicates, keeping the last occurrence of each name
  # essentially preferred_names provided to update_check_log() will override any
  # existing preferred_names in the check_log.
  if (!is.null(preferred_names)) {
    existing_preferred_names <- c(existing_preferred_names, preferred_names)
    existing_preferred_names <- existing_preferred_names[
      !duplicated(names(existing_preferred_names), fromLast = TRUE)
    ]
  }

  # if no preferred_names, set to NULL to avoid passing an empty vector
  if (length(existing_preferred_names) == 0L) {
    existing_preferred_names <- NULL
  }

  # get the format of the input data (tibble or ArrowObject) to retain that
  # format after updating the check log
  out_df <- get_read_npx_output(df = df)

  # remove current check_log from the data object before attaching the new one
  df <- rm_check_log(df = df)

  df <- attach_check_log(
    df = df,
    out_df = out_df,
    preferred_names = existing_preferred_names
  )

  return(df)
}

# Arrow metadata helpers ----

#' Attach check log to an ArrowObject via schema metadata
#'
#' @description
#' Serializes the check log to a base64-encoded string and stores it in the
#' Arrow table's schema-level metadata.
#'
#' @param df An ArrowObject (e.g. `arrow::Table`).
#' @param check_log A named list returned by [`check_npx()`].
#'
#' @return The ArrowObject with check log metadata attached.
#'
#' @keywords internal
#'
attach_check_log_arrow <- function(df,
                                   check_log) {

  check_is_arrow_object(x = df, error = TRUE)

  validate_check_log(df = df, check_log = check_log)

  # serialize check_log to base64
  check_log_encoded <- serialize_check_log(check_log)

  # attach to the Arrow table metadata
  existing_metadata <- df$metadata
  existing_metadata[["olink_check_log"]] <- check_log_encoded
  df$metadata <- existing_metadata

  return(df)

}

#' Convert output format, run check_npx, and attach the result to an Olink
#' data object
#'
#' @description
#' Convenience helper that first converts `data` to the requested output format
#' (via [`convert_read_npx_output()`]), then calls [`check_npx()`] and attaches
#' the resulting check log to the object.  For tibbles the result is an
#' [`olink_class`] object (via [`new_olink_class()`]); for ArrowObjects the
#' check log is stored as schema metadata (via [`attach_check_log_arrow()`]).
#' Any other type is returned unchanged.
#'
#' @param df A tibble or ArrowObject containing Olink NPX data.
#' @param out_df A string specifying the desired output format. Forwarded to
#'   [`convert_read_npx_output()`]. Defaults to `"tibble"`.
#' @param preferred_names An optional named character vector forwarded to
#'   [`check_npx()`].
#'
#' @return `df` converted to `out_df` format with the check log attached.
#'
#' @keywords internal
#'
attach_check_log <- function(df,
                             out_df = "tibble",
                             preferred_names = NULL) {

  # if needed convert the object to the requested output
  df <- convert_read_npx_output(df = df, out_df = out_df)

  # run a fresh check_npx
  check_log <- check_npx(df = df, preferred_names = preferred_names)

  if (check_is_tibble(x = df, error = FALSE)) {
    df <- new_olink_class(df = df, check_log = check_log)
  } else if (check_is_arrow_object(x = df, error = FALSE)) {
    df <- attach_check_log_arrow(df = df, check_log = check_log)
  }

  return(df)

}

#' Serialize check log to a base64-encoded raw string
#'
#' @description
#' Uses R's native `serialize()` / `unserialize()` for safe and reliable
#' round-tripping of the check log structure through Arrow metadata.
#'
#' @param check_log A named list returned by [`check_npx()`].
#'
#' @return A single base64-encoded string.
#'
#' @keywords internal
#'
serialize_check_log <- function(check_log) {

  raw_bytes <- serialize(object = check_log, connection = NULL)
  ser_check_log <- base64_encode(raw_bytes)

  return(ser_check_log)
}

#' Deserialize check log from a base64-encoded raw string
#'
#' @param encoded_str A single base64-encoded string from
#' [serialize_check_log()].
#'
#' @return A named list matching the check log structure.
#'
#' @keywords internal
#' @noRd
#'
deserialize_check_log <- function(encoded_str) {

  tryCatch(
    expr = {
      raw_bytes <- base64_decode(encoded_str)
      result <- unserialize(connection = raw_bytes)
      if (is.list(result)) {
        return(result)
      }
    }, # nolint: return_linter
    error = function(e) {
      NULL # nolint: return_linter
    }
  )

  cli::cli_warn(
    c(
      "!" = "Could not deserialize {.arg check_log} from Arrow metadata.",
      "i" = "The check log will need to be regenerated using {.fn check_npx}."
    )
  )

  return(NULL)

}

# Base64 encoding helpers ----

#' Encode raw bytes to a base64 string
#'
#' @description
#' A minimal base64 encoder that does not require external packages.
#'
#' @param raw_bytes A raw vector to encode.
#'
#' @return A single character string.
#'
#' @keywords internal
#' @noRd
#'
base64_encode <- function(raw_bytes) {

  if (length(raw_bytes) == 0L) return("")

  base64_chars <- c(LETTERS, letters, 0L:9L, "+", "/")

  n <- length(raw_bytes)
  int_vals <- as.integer(raw_bytes)

  # pad to multiple of 3

  pad <- (3L - n %% 3L) %% 3L
  int_vals <- c(int_vals, rep(0L, pad))

  result <- character(0L)
  for (i in seq(from = 1L, to = length(int_vals), by = 3L)) {
    byte1 <- int_vals[i]
    byte2 <- int_vals[i + 1L]
    byte3 <- int_vals[i + 2L]

    result <- c(
      result,
      base64_chars[bitwShiftR(byte1, 2L) + 1L],
      base64_chars[bitwOr(bitwShiftL(bitwAnd(byte1, 3L), 4L),
                          bitwShiftR(byte2, 4L)) + 1L],
      base64_chars[bitwOr(bitwShiftL(bitwAnd(byte2, 15L), 2L),
                          bitwShiftR(byte3, 6L)) + 1L],
      base64_chars[bitwAnd(byte3, 63L) + 1L]
    )
  }

  # replace padding
  if (pad > 0L) {
    result[(length(result) - pad + 1L):length(result)] <- "="
  }

  result <- paste0(result, collapse = "")

  return(result)

}

#' Decode a base64 string to raw bytes
#'
#' @description
#' A minimal base64 decoder that does not require external packages.
#'
#' @param encoded_str A single base64-encoded character string.
#'
#' @return A raw vector.
#'
#' @keywords internal
#' @noRd
#'
base64_decode <- function(encoded_str) {

  if (!nzchar(encoded_str)) return(raw(0L))

  base64_chars <- c(LETTERS, letters, 0L:9L, "+", "/")
  lookup <- stats::setNames(object = seq_along(base64_chars) - 1L,
                            nm = base64_chars)

  chars <- strsplit(x = encoded_str, split = "")[[1L]]

  # count padding
  pad <- sum(chars == "=")
  chars[chars == "="] <- "A"  # treat padding as zero

  vals <- lookup[chars]

  result <- raw(0L)
  for (i in seq(from = 1L, to = length(vals), by = 4L)) {
    val1 <- vals[i]
    val2 <- vals[i + 1L]
    val3 <- vals[i + 2L]
    val4 <- vals[i + 3L]

    result <- c(
      result,
      as.raw(bitwOr(bitwShiftL(val1, 2L), bitwShiftR(val2, 4L))),
      as.raw(bitwOr(bitwShiftL(bitwAnd(val2, 15L), 4L),
                    bitwShiftR(val3, 2L))),
      as.raw(bitwOr(bitwShiftL(bitwAnd(val3, 3L), 6L), val4))
    )
  }

  # remove padding bytes
  if (pad > 0L) {
    result <- result[seq_len(length(result) - pad)]
  }

  return(result)

}

# dplyr compatibility ----

#' Reconstruct an `olink_class` object after dplyr operations
#'
#' @description
#' This method is called by dplyr after every verb (e.g. `filter`, `mutate`,
#' `select`) to preserve the `olink_class` class and its check log attribute.
#'
#' @param data The result of the dplyr operation (a tibble).
#' @param template The original `olink_class` object.
#'
#' @return An `olink_class` object with the check log from `template`.
#'
#' @exportS3Method dplyr::dplyr_reconstruct
#'
#' @keywords internal
#' @noRd
#'
dplyr_reconstruct.olink_class <- function(data, template) {

  check_log <- attr(x = template, which = "check_log", exact = TRUE)

  df_olink <- tibble::new_tibble(
    x = data,
    check_log = check_log,
    class = "olink_class"
  )

  return(df_olink)
}

# Print method ----

#' Provide a custom header for `olink_class` objects
#'
#' @param x An `olink_class` object.
#' @param ... Additional arguments passed to the tibble method.
#'
#' @return A named character vector of summary items.
#'
#' @exportS3Method pillar::tbl_sum
#'
#' @keywords internal
#' @noRd
#'
#' @importFrom pillar tbl_sum
#'
tbl_sum.olink_class <- function(x, ...) {

  default_header <- NextMethod()

  check_log <- attr(x = x, which = "check_log", exact = TRUE)

  if (!is.null(check_log)) {
    olink_class_name <- c(default_header,
                          "olink_check_log" = "attached")
  } else {
    olink_class_name <- c(default_header,
                          "olink_check_log" = "missing")
  }

  return(olink_class_name)
}
