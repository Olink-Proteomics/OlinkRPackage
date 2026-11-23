#' Reshape Olink data to samples by assays or assays by samples
#'
#' @param df A tibble, olink_class object, or Arrow object in long format.
#' @param check_log Optional check log used to resolve unspecified column roles.
#' @param sample_col,assay_col,value_col Optional input column names. Explicit
#'   names override the corresponding role in the check log.
#' @param orientation Whether assays or samples form the output columns.
#' @param id_cols Optional additional columns identifying output rows.
#' @param drop_missing_values Remove missing measurements before pivoting.
#' @param drop_assays,drop_samples Remove assays or samples with any missing
#'   measurement after pivoting. Assays are removed first.
#' @param assay_missingness_cutoff Optional proportion between 0 and 1. After
#'   the drop flags are applied, remove assays above this missingness fraction.
#' @param names_sort Sort generated column names.
#' @param out Return a tibble or numeric matrix.
#' @param return_details Return the data and identifiers removed by the drop
#'   flags as a list. Identifiers lost before pivoting are not reported.
#' @return A tibble, a matrix, or a list containing data, dropped_assays and
#'   dropped_samples, and dropped_assays_missingness. Matrix output requires unique, non-missing row IDs and
#'   numeric measurement columns. Arrow input is collected in memory.
#' @keywords internal
#' @noRd
shape_long_to_wide <- function(
  df,
  check_log = NULL,
  sample_col = NULL,
  assay_col = NULL,
  value_col = NULL,
  orientation = c("assays_in_columns", "samples_in_columns"),
  id_cols = NULL,
  drop_missing_values = FALSE,
  drop_assays = FALSE,
  drop_samples = FALSE,
  assay_missingness_cutoff = NULL,
  names_sort = FALSE,
  out = c("tibble", "matrix"),
  return_details = FALSE
) {
  check_is_dataset(x = df, error = TRUE)
  orientation <- match.arg(orientation)
  out <- match.arg(out)
  check_is_scalar_boolean(x = drop_missing_values, error = TRUE)
  check_is_scalar_boolean(x = drop_assays, error = TRUE)
  check_is_scalar_boolean(x = drop_samples, error = TRUE)
  check_is_scalar_boolean(x = names_sort, error = TRUE)
  check_is_scalar_boolean(x = return_details, error = TRUE)
  if (
    !is.null(assay_missingness_cutoff) &&
      (!is.numeric(assay_missingness_cutoff) ||
        length(assay_missingness_cutoff) != 1L ||
        is.na(assay_missingness_cutoff) ||
        !is.finite(assay_missingness_cutoff) ||
        assay_missingness_cutoff < 0 ||
        assay_missingness_cutoff > 1)
  ) {
    cli::cli_abort(
      "{.arg assay_missingness_cutoff} must be NULL or a number between 0 and 1."
    )
  }

  roles <- list(
    sample_col = sample_col,
    assay_col = assay_col,
    value_col = value_col
  )
  if (any(vapply(roles, is.null, logical(1L)))) {
    log <- get_check_npx(df = df, check_log = check_log)
    defaults <- log$col_names[c("sample_id", "olink_id", "quant")]
    for (i in seq_along(roles)) {
      if (is.null(roles[[i]])) roles[i] <- list(defaults[[i]])
    }
  } else if (!is.null(check_log)) {
    cli::cli_abort(
      "A check log is unnecessary when all column roles are explicit."
    )
  }

  valid_role <- vapply(
    roles,
    function(x) {
      is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
    },
    logical(1L)
  )
  if (!all(valid_role)) {
    cli::cli_abort(
      "Each column role must resolve to one non-missing column name."
    )
  }
  sample_col <- roles$sample_col
  assay_col <- roles$assay_col
  value_col <- roles$value_col
  if (
    !is.null(id_cols) &&
      (!is.character(id_cols) || anyNA(id_cols) || any(!nzchar(id_cols)))
  ) {
    cli::cli_abort("{.arg id_cols} must be a character vector of column names.")
  }
  cols <- c(sample_col, assay_col, value_col, id_cols)
  if (anyDuplicated(cols)) {
    cli::cli_abort(
      "Column roles and additional row identifiers must be distinct."
    )
  }
  if (!all(cols %in% names(df))) {
    cli::cli_abort("The requested columns are not all present in {.arg df}.")
  }
  if (
    length(id_cols) > 0L &&
      (drop_assays ||
        drop_samples ||
        !is.null(assay_missingness_cutoff) ||
        out == "matrix")
  ) {
    cli::cli_abort(
      "Additional row identifiers cannot be used with drop flags or matrix output."
    )
  }

  df <- convert_read_npx_output(df = df, out_df = "tibble") |>
    dplyr::select(dplyr::all_of(cols))
  if (!is.numeric(df[[value_col]])) {
    cli::cli_abort("The measurement column must be numeric.")
  }
  row_col <- if (orientation == "assays_in_columns") sample_col else assay_col
  col_col <- if (orientation == "assays_in_columns") assay_col else sample_col
  if (out == "matrix" && (anyNA(df[[row_col]]) || anyNA(df[[col_col]]))) {
    cli::cli_abort("Matrix row and column identifiers must not be missing.")
  }
  if (drop_missing_values) {
    df <- df |> dplyr::filter(!is.na(.data[[value_col]]))
  }
  df_wide <- df |>
    tidyr::pivot_wider(
      id_cols = dplyr::all_of(c(row_col, id_cols)),
      names_from = dplyr::all_of(col_col),
      values_from = dplyr::all_of(value_col),
      names_sort = names_sort
    )

  value_names <- setdiff(names(df_wide), c(row_col, id_cols))
  if (
    (drop_assays || drop_samples || !is.null(assay_missingness_cutoff)) &&
      any(vapply(df_wide[value_names], is.list, logical(1L)))
  ) {
    cli::cli_abort("Drop flags require unique measurements for each cell.")
  }
  filtered <- shape_filter_wide(
    df = df_wide,
    row_col = row_col,
    orientation = orientation,
    id_cols = id_cols,
    drop_assays = drop_assays,
    drop_samples = drop_samples,
    assay_missingness_cutoff = assay_missingness_cutoff
  )
  df_wide <- filtered$data

  if (out == "matrix") {
    row_ids <- as.character(df_wide[[row_col]])
    if (anyNA(row_ids) || any(!nzchar(row_ids)) || anyDuplicated(row_ids)) {
      cli::cli_abort("Matrix row identifiers must be unique and non-missing.")
    }
    values <- df_wide[, setdiff(names(df_wide), row_col), drop = FALSE]
    if (!all(vapply(values, is.numeric, logical(1L)))) {
      cli::cli_abort(
        "Matrix measurement columns must be numeric and unique per cell."
      )
    }
    df_wide <- as.matrix(values)
    rownames(df_wide) <- row_ids
  }
  if (return_details) {
    return(list(
      data = df_wide,
      dropped_assays = filtered$dropped_assays,
      dropped_samples = filtered$dropped_samples,
      dropped_assays_missingness = filtered$dropped_assays_missingness
    ))
  }
  return(df_wide)
}

# Filter an already widened measurement table in the order used for PCA/UMAP.
# Kept separate so callers can validate and report between filtering stages.
shape_filter_wide <- function(
  df,
  row_col,
  orientation,
  id_cols = NULL,
  drop_assays = FALSE,
  drop_samples = FALSE,
  assay_missingness_cutoff = NULL
) {
  dropped_assays <- character(0L)
  dropped_samples <- character(0L)
  dropped_assays_missingness <- character(0L)

  for (step in c("assay", "sample", "missingness")) {
    enabled <- switch(
      step,
      assay = drop_assays,
      sample = drop_samples,
      missingness = !is.null(assay_missingness_cutoff)
    )
    if (!enabled) {
      next
    }
    dimension <- if (step == "missingness") "assay" else step
    is_column <- (dimension == "assay") == (orientation == "assays_in_columns")
    value_names <- setdiff(names(df), c(row_col, id_cols))
    if (is_column) {
      fractions <- if (nrow(df) == 0L) {
        rep(0, length(value_names))
      } else {
        vapply(df[value_names], function(x) mean(is.na(x)), numeric(1L))
      }
      remove <- if (step == "missingness") {
        fractions > assay_missingness_cutoff
      } else {
        fractions > 0
      }
      removed <- value_names[remove]
      df <- df |> dplyr::select(-dplyr::all_of(removed))
    } else {
      fractions <- if (length(value_names) == 0L) {
        rep(0, nrow(df))
      } else {
        rowMeans(is.na(df[value_names]))
      }
      remove <- if (step == "missingness") {
        fractions > assay_missingness_cutoff
      } else {
        fractions > 0
      }
      removed <- as.character(df[[row_col]][remove])
      df <- df[!remove, , drop = FALSE]
    }
    if (step == "assay") {
      dropped_assays <- removed
    }
    if (step == "sample") {
      dropped_samples <- removed
    }
    if (step == "missingness") dropped_assays_missingness <- removed
  }
  return(list(
    data = df,
    dropped_assays = dropped_assays,
    dropped_samples = dropped_samples,
    dropped_assays_missingness = dropped_assays_missingness
  ))
}
