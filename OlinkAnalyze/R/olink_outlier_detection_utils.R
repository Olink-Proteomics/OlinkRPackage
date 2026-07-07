#' Compute inter-quartile range (IQR) of multiplied by a fixed value
#'
#' @param df Olink dataset
#' @param quant_col Character vector of name of quantification column
#' @param iqr_group Grouping for which to compute IQR for
#' @param iqr_sd Fixed value to multiply IQR with
#'
#' @return Input dataset with two additional columns, iqr and iqr_sd
#' 
#' @keywords internal
#' @noRd
#'
olink_iqr <- function(df,
                      quant_col,
                      iqr_group,
                      iqr_sd) {
  df_iqr <- df |>
    dplyr::group_by(
      dplyr::pick(
        dplyr::all_of(
          iqr_group
        )
      )
    ) |>
    dplyr::mutate(
      iqr = stats::IQR(x = .data[[quant_col]],
                       na.rm = TRUE)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      iqr_sd = .data[["iqr"]] * .env[["iqr_sd"]]
    )
  return(df_iqr)
}

#' Compute median of quantified value
#'
#' @param df Olink dataset
#' @param quant_col Character vector of name of quantification column
#' @param median_group Grouping for which to compute median for
#'
#' @return Input dataset with one additional columns, median
#' 
#' @keywords internal
#' @noRd
#'
olink_median <- function(df,
                         quant_col,
                         median_group) {
  df_med <- df |>
    dplyr::group_by(
      dplyr::pick(
        dplyr::all_of(
          median_group
        )
      )
    ) |>
    dplyr::mutate(
      median = stats::median(x = .data[[quant_col]],
                             na.rm = TRUE)
    ) |>
    dplyr::ungroup()
  return(df_med)
}

#' Compute outliers based on median +/- iqr_sd * IQR
#'
#' @param df Olink dataset
#' @param quant_col Character vector of name of quantification column
#' @param group Grouping for which to compute median for
#' @param iqr_sd Fixed value to multiply IQR with
#'
#' @return Boolean vector with length equal to the number of input rows
#' indicating outlier.
#' 
#' @keywords internal
#' @noRd
#'
olink_median_iqr_outlier <- function(df,
                                     quant_col,
                                     group,
                                     iqr_sd) {
  df_outlier <- df |>
    olink_iqr(
      quant_col = quant_col,
      iqr_group = group,
      iqr_sd = iqr_sd
    ) |>
    olink_median(
      quant_col = quant_col,
      median_group = group
    ) |>
    dplyr::mutate(
      is_outlier = dplyr::if_else(
        .data[[quant_col]] < (.data[["median"]] - .data[["iqr_sd"]])
        | .data[[quant_col]] > (.data[["median"]] + .data[["iqr_sd"]]),
        TRUE,
        FALSE
      )
    ) |>
    dplyr::pull(
      .data[["is_outlier"]]
    )
  return(df_outlier)
}

#' Summarize sample QC warning flags
#'
#' @param df Olink dataset
#' @param qc_warning Character value indicating the QC warning column.
#' @param group Character vector of grouping columns to summarize within.
#' @param output_col Character value indicating where to write the summarized
#' QC warning. Defaults to overwriting `qc_warning`.
#'
#' @return Input dataset with summarized QC warning values.
#' 
#' @keywords internal
#' @noRd
#'
olink_summarize_qc_warning <- function(df,
                                       qc_warning,
                                       group = NULL,
                                       output_col = qc_warning) {
  # check if columns are present in the dataset
  check_columns(df = df, col_list = list(c(qc_warning, group)))
  # get data type to convert the output accordingly at the end of the function
  out_df <- get_read_npx_output(df = df)
  # make sure data is a tibble
  df <- convert_read_npx_output(df = df, out_df = "tibble")

  # ineternal function working on each group
  summarize_qc_warning <- function(x) {
    qc_flags <- trimws(as.character(x))
    qc_flags <- qc_flags[!is.na(qc_flags)]
    qc_flags_upper <- toupper(qc_flags)

    qc_summary <- dplyr::case_when(
      any(qc_flags_upper == "FAIL") ~ "Fail",
      any(grepl(pattern = "WARN", x = qc_flags_upper, fixed = TRUE))
        ~ "Warning",
      any(qc_flags_upper == "PASS") ~ "Pass",
      TRUE ~ "Unknown"
    )

    return(qc_summary)
  }

  if (length(group) > 0L) {
    df <- df |>
      dplyr::group_by(
        dplyr::pick(
          dplyr::all_of(group)
        )
      )
  }

  df <- df |>
    dplyr::mutate(
      "..olink_qc_warning_summary" = summarize_qc_warning(
        x = .data[[qc_warning]]
      )
    ) |>
    dplyr::ungroup()

  if (any(df[["..olink_qc_warning_summary"]] == "Unknown")) {
    df_unknown <- df |> 
      dplyr::filter(
        .data[["..olink_qc_warning_summary"]] == "Unknown"
      ) |> 
      dplyr::select(
        dplyr::all_of(
          group
        )
      ) |> 
      dplyr::distinct() |> 
      nrow()
          
    cli::cli_warn(
      "{.val {df_unknown}} groups were assigned QC status {.val {\"Unknown\"}}.
      Their QC flags did not match any of
      {.val {c(\"Fail\", \"Warning\", \"Pass\")}}."
    )
  }

  df <- df |>
    dplyr::mutate(
      !!output_col := .data[["..olink_qc_warning_summary"]]
    ) |>
    dplyr::select(
      -dplyr::all_of("..olink_qc_warning_summary")
    ) |> 
    convert_read_npx_output(
      out_df = out_df
    )

  return(df)
}
