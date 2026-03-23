#### Internal functions ####

npxProcessing_forDimRed <- function(df, # nolint: object_name_linter
                                    check_log = NULL,
                                    color_g = "QC_Warning",
                                    drop_assays = FALSE,
                                    drop_samples = FALSE,
                                    verbose = FALSE) {
  # Check if check_log is correct
  check_log <- run_check_npx(df = df, check_log = check_log)

  # other checks
  check_is_dataset(df= df, error = TRUE)
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
      dplyr::group_by(
        dplyr::across(
          dplyr::all_of(check_log$col_names$sample_id)
        )
      ) |>
      dplyr::mutate(
        QC_Warning = dplyr::if_else(
          any(grepl(pattern = "warn",
                    x = .data[[check_log$col_names$qc_warning]],
                    ignore.case = TRUE)),
          "Warning",
          "Pass"
        )
      ) |>
      dplyr::ungroup()

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
  } else {
    n_sample_w_multiple_colors <- df |>
      dplyr::group_by(
        dplyr::across(
          dplyr::all_of(check_log$col_names$sample_id)
        )
      ) |>
      dplyr::summarise(
        n_colors = dplyr::n_distinct(.data[["color_g"]], na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::filter(.data[["n_colors"]] > 1L) |>
      nrow()

    if (n_sample_w_multiple_colors > 0L) {
      cli::cli_abort(
        "There are {number_of_sample_w_more_than_one_color} samples that do
        not have a unique color. Only one color per sample is allowed."
      )
    } else {
      df_temp <- df

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
    }
  }
  # df is no longer needed
  rm(df)

  #### Remove assays with 0 variance ####
  df_temp <- df_temp |>
    dplyr::group_by(OlinkID) |> # nolint object_usage_linter
    dplyr::mutate(assay_var = var(NPX, na.rm = TRUE)) |> # nolint object_usage_linter
    dplyr::ungroup() |>
    dplyr::filter(!(assay_var == 0 | is.na(assay_var))) |> # nolint object_usage_linter
    dplyr::select(-assay_var) |>
    dplyr::arrange(SampleID) # nolint object_usage_linter

  # wide format
  df_wide <- df_temp |>
    dplyr::select(SampleID, OlinkID, NPX) |> # nolint object_usage_linter
    dplyr::filter(!is.na(NPX)) |>
    tidyr::pivot_wider(
      names_from = OlinkID,
      values_from = NPX,
      names_sort = TRUE
    )

  #### If drop_assays == T, drop assays with any missing values ####
  if (drop_assays) {
    dropped_assays.na <- colnames(df_wide[, -c(1)])[apply( # nolint object_name_linter
      df_wide[, -c(1)],
      2,
      anyNA
    )]

    df_wide <- df_wide |>
      dplyr::select(-tidyselect::all_of(dropped_assays.na))

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
    dropped_assays.na <- NULL # nolint object_name_linter
  }

  #### If drop_samples == T, drop samples with any missing values ####
  if (drop_samples) {
    dropped_samples <- apply(df_wide[, -c(1)], 1, anyNA)
    df_wide <- df_wide[!dropped_samples, ]

    if (verbose) {
      cli::cli_warn(
        "{length(dropped_samples)} sample{?s} contain NA and
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

  #### Drop assays with too many missing values ####
  # Missingness per assay
  percent_missingness <- colSums(is.na(df_wide[, -c(1)])) / nrow(df_wide)

  # assays with missingness > 10% are dropped from the PCA
  PERCENT_CUTOFF <- 0.1 # nolint object_name_linter

  # If there are fewer samples than one plate (88), the PERCENT_CUTOFF is 0.05
  if (nrow(df_wide) <= 88) {
    PERCENT_CUTOFF <- 0.05 # nolint object_name_linter
  }

  if (any(percent_missingness > PERCENT_CUTOFF)) {
    removed_assays_index <- which(percent_missingness > PERCENT_CUTOFF)
    percent_missingness <- percent_missingness[-removed_assays_index]

    removed_assays_index <- removed_assays_index + 1
    dropped_assays.missingness <- colnames(df_wide)[removed_assays_index] # nolint object_name_linter

    df_wide <- df_wide[, -removed_assays_index]

    if (verbose) {
      cli::cli_warn(
        "There are {length(dropped_assays.missingness)} assay{?s} dropped due
        to high missingness (>{round(PERCENT_CUTOFF * 100)}%)."
      )
    }
  } else {
    dropped_assays.missingness <- NULL # nolint object_name_linter
  }

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
      dplyr::mutate(dplyr::across(
        dplyr::all_of(imputed_assays),
        ~ ifelse(is.na(.x), median(.x, na.rm = TRUE), .x)
      ))

    if (verbose) {
      cli::cli_warn(
        "There are {length(imputed_assays)} assay{?s} were imputed using
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
    dplyr::left_join(plotColors, by = "SampleID") |>
    dplyr::select(SampleID, colors, dplyr::everything()) # nolint object_usage_linter

  df_wide_matrix <- df_wide |>
    dplyr::select(-colors) |>
    tibble::column_to_rownames("SampleID") |>
    as.matrix()

  Sys.setlocale("LC_COLLATE", old_collate)

  return(list(
    df_wide = df_wide,
    df_wide_matrix = df_wide_matrix,
    dropped_assays.na = dropped_assays.na,
    dropped_assays.missingness = dropped_assays.missingness
  ))
}
