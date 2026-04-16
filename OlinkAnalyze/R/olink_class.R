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
#' existing behavior of running [`check_npx()`] internally.
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
#' @param data A tibble containing Olink NPX data.
#' @param check_log A named list returned by [`check_npx()`].
#'
#' @return An object of class `olink_class`, which inherits from
#' `tbl_df`, `tbl`, and `data.frame`, with the check log stored as an
#' attribute.
#'
#' @seealso [olink_check_log()] for retrieving the check log from an object.
#'
#' @keywords internal
#'
new_olink_class <- function(data,
                          check_log) {

  # validate inputs
  if (!inherits(x = data, what = "tbl_df")) {
    cli::cli_abort(
      c(
        "x" = "{.arg data} must be a tibble to create an {.cls olink_class}
        object."
      ),
      call = rlang::caller_env()
    )
  }

  validate_check_log(check_log = check_log)

  # construct the subclass
  tibble::new_tibble(
    x = data,
    check_log = check_log,
    class = "olink_class"
  )

}

# Validator ----

#' Validate a check log list
#'
#' @description
#' Checks that `check_log` is a named list with all elements expected by
#' the package internals.
#'
#' @param check_log A named list to validate.
#'
#' @return `TRUE` invisibly if validation passes, otherwise throws an error.
#'
#' @keywords internal
#'
validate_check_log <- function(check_log) {

  check_is_list(x = check_log, error = TRUE)

  if (is.null(names(check_log))) {
    cli::cli_abort(
      c(
        "x" = "{.arg check_log} must be a named list.",
        "i" = "Expected names: {.val {check_npx_lst_names}}."
      ),
      call = rlang::caller_env()
    )
  }

  check_log_missing <- setdiff(
    x = check_npx_lst_names,
    y = names(check_log)
  )

  if (length(check_log_missing) > 0L) {
    cli::cli_abort(
      c(
        "x" = "Element{?s} {.val {check_log_missing}} are missing from
        {.arg check_log}.",
        "i" = "Ensure {.arg check_log} is the output of {.fn check_npx}."
      ),
      call = rlang::caller_env()
    )
  }

  invisible(TRUE)

}

# Accessor ----

#' Retrieve the check log from an Olink data object
#'
#' @description
#' Extracts the check log from an `olink_class` tibble or an ArrowObject with
#' embedded metadata. Returns `NULL` if no check log is found.
#'
#' @param x A data object, typically from [`read_npx()`] or [`clean_npx()`].
#'
#' @return A named list (the check log) or `NULL` if no check log is attached.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # read and check data
#' npx_file <- system.file("extdata",
#'                         "npx_data_ext.parquet",
#'                         package = "OlinkAnalyze")
#' npx_df <- read_npx(filename = npx_file)
#' check_log <- check_npx(df = npx_df)
#'
#' # attach check_log to the data
#' npx_obj <- new_olink_class(data = npx_df, check_log = check_log)
#'
#' # retrieve check_log
#' olink_check_log(npx_obj)
#' }
#'
olink_check_log <- function(x) {

  if (inherits(x = x, what = "olink_class")) {

    return(attr(x = x, which = "check_log", exact = TRUE))

  } else if (inherits(x = x, what = c("ArrowObject", "arrow_dplyr_query"))) {

    check_log_encoded <- x$metadata[["olink_check_log"]]

    if (is.null(check_log_encoded)) {
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
#' Strips the `olink_class` class and removes the attached check log, returning a
#' plain tibble.
#'
#' @param x An `olink_class` object.
#' @param ... Additional arguments (currently ignored).
#'
#' @return A tibble (`tbl_df`) without the `olink_class` class or the check log
#' attribute.
#'
#' @keywords internal
#' @exportS3Method tibble::as_tibble
#'
as_tibble.olink_class <- function(x, ...) { # nolint: object_name_linter

  attr(x = x, which = "check_log") <- NULL
  class(x) <- setdiff(x = class(x), y = "olink_class")

  x

}

#' Remove the check log metadata from an ArrowObject
#'
#' @description
#' Strips the `olink_check_log` key from the schema-level metadata of an
#' ArrowObject, returning a plain Arrow table without any attached check log.
#'
#' @param data An ArrowObject (e.g. `arrow::Table` or `arrow::Dataset`).
#'
#' @return The ArrowObject with the `olink_check_log` metadata removed.
#'
#' @keywords internal
#'
strip_check_log_arrow <- function(data) {

  if (!inherits(x = data, what = c("ArrowObject", "arrow_dplyr_query"))) {
    cli::cli_abort(
      c(
        "x" = "{.arg data} must be an ArrowObject."
      ),
      call = rlang::caller_env()
    )
  }

  existing_metadata <- data$metadata

  if ("olink_check_log" %in% names(existing_metadata)) {
    existing_metadata[["olink_check_log"]] <- NULL
    data$metadata <- existing_metadata
  }

  return(data)

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
#' @param data An `olink_class` tibble or an ArrowObject with
#' `olink_check_log` metadata.
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
#' npx_file <- system.file("extdata",
#'                         "npx_data_ext.parquet",
#'                         package = "OlinkAnalyze")
#' npx_obj <- read_npx(filename = npx_file)
#'
#' # strip check log, returning a plain tibble
#' npx_tbl <- strip_check_log(npx_obj)
#' class(npx_tbl)
#' olink_check_log(npx_tbl)
#' }
#'
strip_check_log <- function(data) {

  if (inherits(x = data, what = "olink_class")) {

    return(as_tibble.olink_class(x = data))

  } else if (inherits(x = data,
                       what = c("ArrowObject", "arrow_dplyr_query"))) {

    return(strip_check_log_arrow(data = data))

  }

  return(data)

}

# Arrow metadata helpers ----

#' Attach check log to an ArrowObject via schema metadata
#'
#' @description
#' Serializes the check log to a base64-encoded string and stores it in the
#' Arrow table's schema-level metadata.
#'
#' @param data An ArrowObject (e.g. `arrow::Table`).
#' @param check_log A named list returned by [`check_npx()`].
#'
#' @return The ArrowObject with check log metadata attached.
#'
#' @keywords internal
#'
attach_check_log_arrow <- function(data,
                                   check_log) {

  if (!inherits(x = data, what = c("ArrowObject", "arrow_dplyr_query"))) {
    cli::cli_abort(
      c(
        "x" = "{.arg data} must be an ArrowObject."
      ),
      call = rlang::caller_env()
    )
  }

  validate_check_log(check_log = check_log)

  # serialize check_log to base64
  check_log_encoded <- serialize_check_log(check_log)

  # attach to the Arrow table metadata
  existing_metadata <- data$metadata
  existing_metadata[["olink_check_log"]] <- check_log_encoded
  data$metadata <- existing_metadata

  return(data)

}

#' Serialize check log to a base64-encoded raw string
#'
#' @description
#' Uses R's native `serialize()` / `unserialize()` for safe and reliable
#' round-tripping of the check log structure through Arrow metadata.
#'
#' @param check_log A named list.
#'
#' @return A single base64-encoded string.
#'
#' @keywords internal
#'
serialize_check_log <- function(check_log) {

  raw_bytes <- serialize(object = check_log, connection = NULL)
  base64_encode(raw_bytes)

}

#' Deserialize check log from a base64-encoded raw string
#'
#' @param encoded_str A single base64-encoded string from
#' [serialize_check_log()].
#'
#' @return A named list matching the check log structure.
#'
#' @keywords internal
#'
deserialize_check_log <- function(encoded_str) {

  tryCatch(
    expr = {
      raw_bytes <- base64_decode(encoded_str)
      result <- unserialize(connection = raw_bytes)
      if (is.list(result)) {
        return(result)
      }
    },
    error = function(e) {
      NULL
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
#'
base64_encode <- function(raw_bytes) {

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

  paste0(result, collapse = "")

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
#'
base64_decode <- function(encoded_str) {

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

  result

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
#' @keywords internal
#' @exportS3Method dplyr::dplyr_reconstruct
#'
dplyr_reconstruct.olink_class <- function(data, template) {

  check_log <- attr(x = template, which = "check_log", exact = TRUE)

  tibble::new_tibble(
    x = data,
    check_log = check_log,
    class = "olink_class"
  )

}

# Print method ----

#' Provide a custom header for `olink_class` objects
#'
#' @param x An `olink_class` object.
#' @param ... Additional arguments passed to the tibble method.
#'
#' @return A named character vector of summary items.
#'
#' @keywords internal
#' @exportS3Method pillar::tbl_sum
#'
tbl_sum.olink_class <- function(x, ...) {

  default_header <- NextMethod()

  check_log <- attr(x = x, which = "check_log", exact = TRUE)

  if (!is.null(check_log)) {
    c(default_header,
      "Check log" = "attached")
  } else {
    c(default_header,
      "Check log" = "missing")
  }

}

# Downstream helper ----

#' Quietly clean proteomics data quantified with Olink's PEA technology
#'
#' @description
#' Internal wrapper around [`clean_npx()`] that runs the same cleaning pipeline
#' while suppressing messages and warnings emitted during processing.
#'
#' Unlike [`clean_npx()`], this function is intended for internal use in cases
#' where a quiet cleaning step is needed. If rows are removed, a single
#' [`cli::cli_alert()`] message is shown. If no rows are removed, no output is
#' printed.
#'
#' @details
#' This function forwards all arguments to [`clean_npx()`] and forces
#' `verbose = FALSE`.
#'
#' If any rows are removed during cleaning, the function prints a single alert
#' indicating how many entries were removed and instructing the user to run
#' [`clean_npx()`] directly to inspect which rows were removed.
#'
#' @inheritParams clean_npx
#' @inherit clean_npx params return
#'
#' @author
#'   Kang Dong
#'   Klev Diamanti
#'
#' @keywords internal
#'
run_clean_npx <- function(df, ...) {

  dots <- list(...)

  # remove user-supplied verbose if present
  dots$verbose <- NULL

  valid_args <- names(formals(clean_npx))
  unknown_args <- setdiff(x = names(dots), y = valid_args)

  if (length(unknown_args) > 0L) {
    cli::cli_abort(
      c(
        "x" = "{cli::qty(unknown_args)} Unknown argument{?s}:
        {.val {unknown_args}}.",
        "i" = "Check the documentation of {.fn clean_npx} for valid arguments."
      ),
      call = rlang::caller_env(),
      wrap = TRUE
    )
  }

  n_before <- nrow(df)

  cleaned_df <- withCallingHandlers(
    expr = do.call(clean_npx, c(list(df = df), dots, list(verbose = FALSE))),
    message = function(m) invokeRestart("muffleMessage"),
    warning = function(w) invokeRestart("muffleWarning")
  )

  n_after <- nrow(cleaned_df)

  if (n_after != n_before) {
    n_removed <- n_before - n_after # nolint: object_usage_linter
    cli::cli_inform(
      c("{.val {n_removed}} entr{?y/ies} removed by {.fn clean_npx} from the
      input dataset {.arg df}. Run {.fn clean_npx} on your dataset with
      {.arg verbose = TRUE} to inspect which rows were removed.")
    )
  }

  return(cleaned_df)
}
