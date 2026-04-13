#' S3 class for Olink NPX data with attached check log
#'
#' @description
#' The `olink_npx` class is a tibble subclass that carries the output of
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
#' The `olink_npx` class survives most dplyr operations (e.g.
#' [`dplyr::filter()`], [`dplyr::mutate()`], [`dplyr::select()`]) through the
#' [`dplyr::dplyr_reconstruct()`] mechanism. Note that operations that combine
#' multiple data frames (e.g. [`dplyr::bind_rows()`]) may strip the class and
#' check log.
#'
#' @name olink_npx
#' @keywords internal
#'
NULL

# Constructor ----

#' Create an `olink_npx` object from a tibble and a check log
#'
#' @description
#' Attaches the result of [`check_npx()`] to a tibble as an attribute,
#' creating an `olink_npx` subclass. This allows downstream functions to
#' automatically extract the check log from the data.
#'
#' @param data A tibble containing Olink NPX data.
#' @param check_log A named list returned by [`check_npx()`].
#'
#' @return An object of class `olink_npx`, which inherits from
#' `tbl_df`, `tbl`, and `data.frame`, with the check log stored as an
#' attribute.
#'
#' @seealso [olink_check_log()] for retrieving the check log from an object.
#'
#' @keywords internal
#'
new_olink_npx <- function(data,
                          check_log) {

  # validate inputs
  if (!inherits(x = data, what = "tbl_df")) {
    cli::cli_abort(
      c(
        "x" = "{.arg data} must be a tibble to create an {.cls olink_npx}
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
    class = "olink_npx"
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
#' Extracts the check log from an `olink_npx` tibble or an ArrowObject with
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
#' npx_obj <- new_olink_npx(data = npx_df, check_log = check_log)
#'
#' # retrieve check_log
#' olink_check_log(npx_obj)
#' }
#'
olink_check_log <- function(x) {

  if (inherits(x = x, what = "olink_npx")) {

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

#' Reconstruct an `olink_npx` object after dplyr operations
#'
#' @description
#' This method is called by dplyr after every verb (e.g. `filter`, `mutate`,
#' `select`) to preserve the `olink_npx` class and its check log attribute.
#'
#' @param data The result of the dplyr operation (a tibble).
#' @param template The original `olink_npx` object.
#'
#' @return An `olink_npx` object with the check log from `template`.
#'
#' @keywords internal
#' @export
#'
dplyr_reconstruct.olink_npx <- function(data, template) {

  check_log <- attr(x = template, which = "check_log", exact = TRUE)

  tibble::new_tibble(
    x = data,
    check_log = check_log,
    class = "olink_npx"
  )

}

# Print method ----

#' Provide a custom header for `olink_npx` objects
#'
#' @param x An `olink_npx` object.
#' @param ... Additional arguments passed to the tibble method.
#'
#' @return A named character vector of summary items.
#'
#' @keywords internal
#' @export
#'
tbl_sum.olink_npx <- function(x, ...) {

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

#' Check if data needs cleaning and auto-clean if necessary
#'
#' @description
#' Inspects the check log attached to an `olink_npx` object (or an
#' ArrowObject with embedded metadata) to determine whether the data still
#' contains issues that need cleaning. If the check log is missing or the data
#' has not been cleaned, a minimal cleaning pass is performed automatically via
#' [`clean_npx()`].
#'
#' This function is called at the top of every downstream analysis function
#' (e.g. [`olink_ttest()`], [`olink_anova()`]) to ensure the data is in a
#' usable state. Users who have already called [`clean_npx()`] will see no
#' additional processing.
#'
#' @details
#' The function checks whether the attached check log reports any of the
#' following issues:
#' - Invalid Olink identifiers (`oid_invalid`)
#' - Assays with all `NA` quantification (`assay_na`)
#' - Duplicate sample identifiers (`sample_id_dups`)
#'
#' If any of these are non-empty, the function calls [`clean_npx()`]
#' with conservative defaults (keeping control samples, QC warnings, and
#' assay warnings). If no check log is found, it runs [`check_npx()`] first
#' and then [`clean_npx()`].
#'
#' @param df A dataset, ideally an `olink_npx` object from [`read_npx()`]
#' or [`clean_npx()`].
#' @param check_log **Deprecated.** A named list returned by [`check_npx()`].
#' Retained for backward compatibility. If provided, it is used directly
#' instead of extracting from `df`.
#'
#' @return A named list with two elements:
#' \itemize{
#'   \item \strong{df} The (possibly cleaned) dataset as an `olink_npx` object.
#'   \item \strong{check_log} The check log for the returned dataset.
#' }
#'
#' @seealso [clean_npx()], [check_npx()], [olink_check_log()]
#'
#' @keywords internal
#'
ensure_clean_npx <- function(df,
                             check_log = NULL) {

  # ---- handle deprecated check_log argument ----
  if (!is.null(check_log)) {
    # backward compat: user passed check_log explicitly
    # validate it and return as-is
    validate_check_log(check_log = check_log)
    return(
      list(
        df = df,
        check_log = check_log
      )
    )
  }

  # ---- extract check_log from the data object ----
  check_log <- olink_check_log(x = df)

  if (is.null(check_log)) {
    # no check_log attached — need to run check_npx
    cli::cli_inform(
      c(
        "No check log found on {.arg df}. Running {.fn check_npx}.",
        "i" = "For best performance, use {.fn read_npx} which attaches the
        check log automatically."
      )
    )
    check_log <- check_npx(df = df)

    # attach it
    if (check_is_tibble(x = df, error = FALSE)) {
      df <- new_olink_npx(data = df, check_log = check_log)
    } else if (check_is_arrow_object(x = df, error = FALSE)) {
      df <- attach_check_log_arrow(data = df, check_log = check_log)
    }
  }

  # ---- check if data still needs cleaning ----
  needs_clean <- (
    length(check_log$oid_invalid) > 0L ||
    length(check_log$assay_na) > 0L ||
    length(check_log$sample_id_dups) > 0L
  )

  if (needs_clean) {
    cli::cli_inform(
      c(
        "Data contains issues that require cleaning. Running {.fn clean_npx}
        automatically.",
        "i" = "To avoid automatic cleaning, call {.fn clean_npx} explicitly
        before downstream analysis."
      )
    )

    df <- clean_npx(
      df = df,
      check_log = check_log,
      remove_assay_na = TRUE,
      remove_invalid_oid = TRUE,
      remove_dup_sample_id = TRUE,
      remove_control_assay = FALSE,
      remove_control_sample = FALSE,
      remove_qc_warning = FALSE,
      remove_assay_warning = FALSE,
      convert_nonunique_uniprot = TRUE,
      out_df = "tibble",
      verbose = FALSE
    ) |>
      suppressMessages()

    check_log <- olink_check_log(x = df)
  }

  return(
    list(
      df = df,
      check_log = check_log
    )
  )

}
