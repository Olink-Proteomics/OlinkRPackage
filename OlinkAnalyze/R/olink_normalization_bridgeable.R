#' Identify if assays shared between Olink Explore 3072 and Olink Explore HT can
#' be bridged
#'
#' @author
#'   Amrita Kar
#'   Marianne Sandin
#'   Danai G. Topouza
#'   Klev Diamanti
#'
#' @description
#' The function uses a dataset from Olink Explore 3072 and a dataset from Olink
#' Explore HT, and examines if the matched assays between the two products can
#' be normalized to each other. The input datasets should be exported from Olink
#' software and should not be altered prior to importing them to this function.
#'
#' @details
#' All processes below assume that the first element from \var{lst_df} is the
#' reference dataset (e.g. Olink Explore HT), and the other element of the list
#' is the non-reference dataset (e.g. Olink Explore 3072). The input datasets
#' \strong{have to be pre-processed} by \code{\link{olink_norm_input_check}}
#' which will take care of mapping of assay identifiers and various checks.
#' Also, the input datasets should exclusively contain datapoints from bridge
#' samples. When this function is called from the function
#' \code{\link{olink_normalization}}, then the list is created seamlessly in the
#' background, and the datasets have been already processed by
#' \code{\link{olink_norm_input_check}}.
#'
#' The input \var{ref_cols} is a named list masking column names of the
#' reference dataset. This list is generated automatically from
#' \code{\link{olink_norm_input_check}} when it is called from
#' \code{\link{olink_normalization}}. In addition,
#' \code{\link{olink_normalization}} has also utilized
#' \code{\link{norm_internal_rename_cols}} to rename the columns of the
#' non-reference dataset according to the ones of the reference dataset, hence
#' all column names should match.
#'
#' @param lst_df A named list of the 2 input datasets. First element should be
#' the reference dataset from Olink Explore HT and the second element should
#' originate from Olink Explore 3072.
#' @param ref_cols A named list with the column names to use. Exported from
#' olink_norm_input_check.
#' @param seed Integer random seed (Default: seek = 1).
#'
#' @return A "tibble" in long format with the following columns:
#' \itemize{
#'    \item{OlinkID:} Underscore-separated Olink identifiers of matching assays
#'    between Olink Explore HT and Olink Explore 3072.
#'    \item{BridgingRecommendation:} A character vector indicating whether the
#'    matching assays are considered as bridgeable or not, and the recommended
#'    type of normalization to perform.
#' }
#'
#' @keywords NPX Bridging
#'
#' @examples
#' \donttest{
#' # check input datasets
#' data_explore_check <- OlinkAnalyze:::olink_norm_input_check(
#'   df1 = OlinkAnalyze:::data_3k_small,
#'   df2 = OlinkAnalyze:::data_ht_small,
#'   overlapping_samples_df1 = intersect(
#'     x = unique(OlinkAnalyze:::data_3k_small$SampleID),
#'     y = unique(OlinkAnalyze:::data_ht_small$SampleID)
#'   ) |>
#'     (\(x) x[!grepl("CONTROL", x)])() |>
#'     head(20L),
#'   overlapping_samples_df2 = NULL,
#'   df1_project_nr = "P1",
#'   df2_project_nr = "P2",
#'   reference_project = "P2",
#'   reference_medians = NULL
#' )
#'
#' # create lst_df
#' lst_df <- list(
#'   data_explore_check$ref_df,
#'   data_explore_check$not_ref_df
#' )
#' names(lst_df) <- c(data_explore_check$ref_name,
#'                    data_explore_check$not_ref_name)
#'
#' # create ref_cols
#' ref_cols <- data_explore_check$ref_cols
#'
#' # run olink_normalization_bridgeable
#' is_bridgeable_result <- OlinkAnalyze:::olink_normalization_bridgeable(
#'   lst_df = lst_df,
#'   ref_cols = ref_cols,
#'   seed = 1
#' )
#' }
#'
olink_normalization_bridgeable <- function(lst_df,
                                           ref_cols,
                                           seed = 1) {
  # help vars ----

  set.seed(seed = seed)

  # variables to mark outliers, filter counts and calculate outlier limits
  iqr_sd <- 3L # nolint
  min_datapoint_cnt <- 10L # nolint
  med_cnt <- 150L # nolint

  # column names from each product quantification to allow computations
  quant_names <- paste(ref_cols$quant, names(lst_df), sep = "_")

  # cleanup input datasets ----

  lst_df_clean <- lapply(
    lst_df,
    function(l_df) {
      l_df |>
        dplyr::filter(
          # only customer samples
          .data[["SampleType"]] == "SAMPLE"
          # remove interal control assays
          & .data[["AssayType"]] == "assay"
          # remove datapoints with very few counts
          & .data[["Count"]] > .env[["min_datapoint_cnt"]]
        ) |>
        # keep only relevant columns (e.g. SampleID, OlinkID, NPX, Count)
        dplyr::select(
          dplyr::all_of(
            c(
              ref_cols$sample_id,
              ref_cols$olink_id,
              ref_cols$quant,
              "Count"
            )
          )
        )
    }
  )

  # append the two datasets to each other ----

  df_combo <- lst_df_clean |>
    # append the datasets to each other
    dplyr::bind_rows(
      .id = "df_name"
    ) |>
    # pivot to wider so that there is one entry for each concatenated assay and
    # sample identifier
    tidyr::pivot_wider(
      names_from = dplyr::all_of(
        c("df_name")
      ),
      values_from = dplyr::all_of(
        c(
          ref_cols$quant,
          "Count"
        )
      )
    ) |>
    # drop NA values not matching between datasets
    # this is not intended to remove data from non-matching sample identifiers,
    # but to remove datapoints non-matching because they were filtered out.
    tidyr::drop_na()
  rm(lst_df_clean)

  # check range and low counts ----

  # column names from each product quantification to allow computing the range
  # difference and low median counts
  range_names <- paste("range", ref_cols$quant, names(lst_df), sep = "_")
  med_cnt_names <- paste("low_cnt_Count", names(lst_df), sep = "_")

  df_combo <- df_combo |>
    dplyr::group_by(
      .data[[ref_cols$olink_id]]
    ) |>
    dplyr::mutate(
      # quantification range
      dplyr::across(
        dplyr::starts_with(ref_cols$quant),
        list(
          range = ~ quantile(x = .x, probs = 0.9, names = FALSE, na.rm = TRUE) -
            quantile(x = .x, probs = 0.1, names = FALSE, na.rm = TRUE)
        ),
        .names = "{fn}_{col}"
      ),
      # median counts
      dplyr::across(
        dplyr::starts_with("Count"),
        list(
          low_cnt = ~ median(x = .x, na.rm = TRUE) < .env[["med_cnt"]]
        ),
        .names = "{fn}_{col}"
      )
    ) |>
    dplyr::ungroup() |>
    # compute the actual range and the low count flag
    dplyr::mutate(
      range_diff = abs(.data[[range_names[1L]]] - .data[[range_names[2L]]]),
      low_cnt = .data[[med_cnt_names[1L]]] | .data[[med_cnt_names[2L]]]
    ) |>
    # remove additional columns
    dplyr::select(
      -dplyr::all_of(
        c(range_names, med_cnt_names)
      )
    )
  rm(range_names, med_cnt_names)

  # check and remove outlier samples ----

  df_combo <- df_combo |>
    dplyr::mutate(
      is_outlier_ref = olink_median_iqr_outlier(
        df = dplyr::pick(
          dplyr::all_of(
            c(ref_cols$olink_id, quant_names[1L])
          )
        ),
        quant_col = quant_names[1L],
        group = ref_cols$olink_id,
        iqr_sd = .env[["iqr_sd"]]
      ),
      is_outlier_notref = olink_median_iqr_outlier(
        df = dplyr::pick(
          dplyr::all_of(
            c(ref_cols$olink_id, quant_names[2L])
          )
        ),
        quant_col = quant_names[2L],
        group = ref_cols$olink_id,
        iqr_sd = .env[["iqr_sd"]]
      ),
      is_outlier = .data[["is_outlier_ref"]] | .data[["is_outlier_notref"]]
    ) |>
    dplyr::select(
      -dplyr::all_of(
        c("is_outlier_ref", "is_outlier_notref")
      )
    ) |>
    dplyr::filter(
      .data[["is_outlier"]] == FALSE
    )

  # correlation of quantifications and comparison of their distributions ----

  df_combo <- df_combo |>
    dplyr::group_by(
      dplyr::pick(
        ref_cols$olink_id
      )
    ) |>
    dplyr::mutate(
      r2_lm = stats::cor(
        x = .data[[quant_names[1L]]],
        y = .data[[quant_names[2L]]],
        use = "everything",
        method = "pearson"
      ) ^ 2L,
      ks_stat = stats::ks.test(
        x = .data[[quant_names[1L]]],
        y = .data[[quant_names[2L]]],
        alternative = "two.sided",
        exact = NULL,
        simulate.p.value = FALSE,
        B = 2000L
      )$statistic
    ) |>
    dplyr::ungroup()

  # define is bridgeable ----

  df_bridge_recommendation <- df_combo |>
    dplyr::select(
      dplyr::all_of(
        c(ref_cols$olink_id, "range_diff", "low_cnt", "r2_lm", "ks_stat")
      )
    ) |>
    dplyr::distinct() |>
    # decide if it's bridgeable based on range, low counts and correlation
    dplyr::mutate(
      IsBridgeable = dplyr::if_else(
        .data[["range_diff"]] > 1 &
          .data[["low_cnt"]] == TRUE &
          .data[["r2_lm"]] < 0.8,
        FALSE,
        TRUE
      )
    ) |>
    # add assays that might have been removed from filtering above
    dplyr::full_join(
      lst_df[[1L]] |>
        dplyr::select(
          dplyr::all_of(ref_cols$olink_id)
        ) |>
        dplyr::distinct(),
      by = ref_cols$olink_id
    ) |>
    # decide normalization methods between median centering (bridge
    # normalization) and QQ normalization (quantile smoothing)
    dplyr::mutate(
      BridgingRecommendation = dplyr::case_when(
        is.na(.data[["IsBridgeable"]]) ~ "NotBridgeable",
        .data[["IsBridgeable"]] == TRUE &
          .data[["ks_stat"]] <= 0.2 ~ "MedianCentering",
        .data[["IsBridgeable"]] == TRUE &
          .data[["ks_stat"]] > 0.2 ~ "QuantileSmoothing",
        .data[["IsBridgeable"]] == FALSE ~ "NotBridgeable",
        TRUE ~ NA_character_,
        .default = NA_character_
      )
    ) |>
    # keep only relevant columns
    dplyr::select(
      dplyr::all_of(
        c(ref_cols$olink_id, "BridgingRecommendation")
      )
    )

  return(df_bridge_recommendation)

}
