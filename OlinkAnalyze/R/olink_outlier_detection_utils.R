#' Compute inter-quartile range (IQR) of multiplied by a fixed value
#'
#' @param df Olink dataset
#' @param quant_col Character vector of name of quantification column
#' @param iqr_group Grouping for which to compute IQR for
#' @param iqr_sd Fixed value to multiply IQR with
#'
#' @return Input dataset with two additional columns, iqr and iqr_sd
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
