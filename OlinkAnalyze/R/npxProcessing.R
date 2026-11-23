#### Internal functions ####

npxProcessing_forDimRed <- function(df, # nolint: object_name_linter
                                    check_log = NULL,
                                    color_g = "QC_Warning",
                                    drop_assays = FALSE,
                                    drop_samples = FALSE,
                                    verbose = FALSE) {
  # Check if check_log is correct
  check_log <- get_check_npx(df = df, check_log = check_log)

  # other checks
  check_is_dataset(x = df, error = TRUE)
  check_is_scalar_character(x = color_g, error = TRUE)
  check_is_scalar_boolean(x = drop_assays, error = TRUE)
  check_is_scalar_boolean(x = drop_samples, error = TRUE)
  check_is_scalar_boolean(x = verbose, error = TRUE)

  # Make sure data is a tibble
  df <- convert_read_npx_output(df = df, out_df = "tibble")

  # Sort order is dependent on locale -> set locale here to make code
  # deterministic
  old_collate <- Sys.getlocale("LC_COLLATE")
  Sys.setlocale("LC_COLLATE", "C")

  #### Set up plotting colors ####

  if (color_g == check_log$col_names$qc_warning) {
    df_temp <- df |>
      olink_summarize_qc_warning(
        qc_warning = check_log$col_names$qc_warning,
        group = check_log$col_names$sample_id
      )
  } else {

    check_columns(df = df, col_list = list(color_g))

    n_sample_w_multiple_colors <- df |>
      dplyr::group_by(
        dplyr::across(
          dplyr::all_of(check_log$col_names$sample_id)
        )
      ) |>
      dplyr::summarise(
        n_colors = dplyr::n_distinct(.data[[color_g]], na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::filter(.data[["n_colors"]] > 1L) |>
      nrow()

    if (n_sample_w_multiple_colors > 0L) {
      cli::cli_abort(
        "There are {n_sample_w_multiple_colors} samples that do
        not have a unique color. Only one color per sample is allowed."
      )
    } else {
      df_temp <- df
    }
  }

  plotColors <- df_temp |> # nolint: object_name_linter
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(check_log$col_names$sample_id)
      )
    ) |>
    dplyr::summarise(
      colors = unique(.data[[color_g]]),
      .groups = "drop"
    )

  # df is no longer needed
  rm(df)

  #### Remove assays with 0 variance ####
  df_temp <- df_temp |>
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(check_log$col_names$olink_id)
      )
    ) |>
    dplyr::mutate(
      assay_var = stats::var(x = .data[[check_log$col_names$quant]],
                             na.rm = TRUE)
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(
      !(.data[["assay_var"]] == 0 | is.na(.data[["assay_var"]]))
    ) |>
    dplyr::select(
      -dplyr::all_of("assay_var")
    ) |>
    dplyr::arrange(
      .data[[check_log$col_names$sample_id]]
    )

  # Widen and optionally remove assays with any missing measurements.
  shaped <- shape_long_to_wide(
    df = df_temp,
    sample_col = check_log$col_names$sample_id,
    assay_col = check_log$col_names$olink_id,
    value_col = check_log$col_names$quant,
    drop_missing_values = TRUE,
    drop_assays = drop_assays,
    names_sort = TRUE,
    return_details = TRUE
  )
  df_wide <- shaped$data
  if (drop_assays) {
    dropped_assays.na <- shaped$dropped_assays # nolint: object_name_linter
    if (verbose) {
      cli::cli_warn(
        "{length(dropped_assays.na)} assay{?s} contain NA and
        {?is/are} dropped."
      )
    }
    if (ncol(df_wide) < 4) {
      cli::cli_abort(
        "Too many assays were removed. Set `drop_assays = FALSE` to use
        imputation."
      )
    }
  } else {
    dropped_assays.na <- NULL # nolint: object_name_linter
  }

  # Remove samples after the assay check so errors and warnings retain order.
  if (drop_samples) {
    n_samples_before <- nrow(df_wide)
    shaped <- shape_filter_wide(
      df = df_wide,
      row_col = check_log$col_names$sample_id,
      orientation = "assays_in_columns",
      drop_samples = TRUE
    )
    df_wide <- shaped$data
    if (verbose) {
      # Preserve the existing warning count, which used the logical mask length.
      cli::cli_warn(
        "{n_samples_before} sample{?s} contain NA and
        {?is/are} dropped."
      )
    }
    if (nrow(df_wide) < 2) {
      cli::cli_abort(
        "Too many samples were removed. Set `drop_assays = FALSE` to use
        imputation."
      )
    }
  }

  # The cutoff is selected after sample removal and before imputation.
  PERCENT_CUTOFF <- 0.1 # nolint: object_name_linter
  if (nrow(df_wide) <= 88) {
    PERCENT_CUTOFF <- 0.05 # nolint: object_name_linter
  }
  shaped <- shape_filter_wide(
    df = df_wide,
    row_col = check_log$col_names$sample_id,
    orientation = "assays_in_columns",
    assay_missingness_cutoff = PERCENT_CUTOFF
  )
  df_wide <- shaped$data
  if (length(shaped$dropped_assays_missingness) > 0L) {
    dropped_assays.missingness <- shaped$dropped_assays_missingness # nolint: object_name_linter
    if (verbose) {
      cli::cli_warn(
        "{length(dropped_assays.missingness)} assay{?s} were dropped due
        to high missingness (>{round(PERCENT_CUTOFF * 100)}%)."
      )
    }
  } else {
    dropped_assays.missingness <- NULL # nolint: object_name_linter
  }
  percent_missingness <- colSums(is.na(df_wide[, -c(1)])) / nrow(df_wide)

  #### Impute remaining missing values by the assay median ####
  if (any(percent_missingness <= PERCENT_CUTOFF & percent_missingness > 0)) {
    imputed_assays_index <- which(
      percent_missingness <= PERCENT_CUTOFF &
        percent_missingness > 0
    )
    percent_missingness <- percent_missingness[-imputed_assays_index]

    imputed_assays_index <- imputed_assays_index + 1
    imputed_assays <- colnames(df_wide)[imputed_assays_index]

    df_wide <- df_wide |>
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(imputed_assays),
          ~ dplyr::if_else(is.na(.x),
                           stats::median(x = .x, na.rm = TRUE),
                           .x)
        )
      )

    if (verbose) {
      cli::cli_warn(
        "{length(imputed_assays)} assay{?s} were imputed using
        their median values."
      )
    }
  }

  if (!all(colSums(is.na(df_wide[, -c(1)])) == 0)) {
    cli::cli_abort(
      "Missingness imputation failed."
    )
  }

  #### Format data and wrap up results ####
  df_wide <- df_wide |>
    dplyr::left_join(
      plotColors,
      by = "SampleID",
      relationship = "one-to-one"
    ) |>
    dplyr::select(
      dplyr::all_of(
        c(check_log$col_names$sample_id,
          "colors")
      ),
      dplyr::everything()
    )

  df_wide_matrix <- df_wide |>
    dplyr::select(
      -dplyr::all_of("colors")
    ) |>
    tibble::column_to_rownames(
      var = "SampleID"
    ) |>
    as.matrix()

  Sys.setlocale("LC_COLLATE", old_collate)

  return(
    list(
      df_wide = df_wide,
      df_wide_matrix = df_wide_matrix,
      dropped_assays.na = dropped_assays.na,
      dropped_assays.missingness = dropped_assays.missingness
    )
  )
}
