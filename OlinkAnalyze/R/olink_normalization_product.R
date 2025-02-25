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
#' @param not_ref_cols A named list with the column names from the non-reference
#' dataset. Exported from olink_norm_input_check.
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
#' not_ref_cols <- data_explore_check$not_ref_cols
#'
#' # run olink_normalization_bridgeable
#' is_bridgeable_result <- OlinkAnalyze:::olink_normalization_bridgeable(
#'   lst_df = lst_df,
#'   ref_cols = ref_cols,
#'   not_ref_cols = not_ref_cols,
#'   seed = 1
#' )
#' }
#'
olink_normalization_bridgeable <- function(lst_df,
                                           ref_cols,
                                           not_ref_cols,
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
          dplyr::if_any(tidyselect::any_of(c(ref_cols$sample_type,
                                             not_ref_cols$sample_type)
                                           ),
                 ~ . =="SAMPLE")
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
    # normalization) and QS normalization (quantile smoothing)
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

#' Quantile smoothing normalization of all proteins between two NPX projects.
#'
#' @author
#'   Amrita Kar
#'   Marianne Sandin
#'   Masoumeh Sheikhi
#'   Klev Diamanti
#'
#' @description
#' This function uses bridge samples to map quantiles of the non-reference
#' dataset to the ones of the reference dataset. Mapped quantiles are used to
#' transform the quantifications of the the non-reference dataset to the
#' reference.
#'
#' @details
#' In the case when a study is separated into multiple projects,
#' an additional normalization step is needed to allow the data to be
#' comparable across projects. Across different Olink products, some of the
#' assays exist in corresponding but distinct NPX spaces. For those assays,
#' the median of paired differences is insufficient for bridging as it only
#' considers one anchor point (the median/50% quantile). Instead, quantile
#' smoothing (QS) using multiple anchor points (5%, 10%, 25%, 50%, 75%, 90%
#' and 95% quantiles) is favored to map the Explore 3072 data to the Explore
#' HT distribution. The `olink_normalization_qs()` performs quantile smoothing
#' bridging normalization between datasets from two Olink products (for example
#' Olink Explore 3072 and Olink Explore HT) by performing the following
#' steps: \cr
#'
#' \itemize{
#'
#'  \item An empirical cumulative distribution function is used to map
#'  datapoints for the bridging samples from one product to the equivalent
#'  space in the other product.
#'  \item A spline regression model is constructed using unmapped
#'  and mapped data from one product, using anchor points from the
#'  quantiles defined above.
#'  \item The spline regression model is used to predict the normalized
#'  NPX values for all datapoints
#'
#' }
#'
#' More information on quantile smoothing and between product normalization
#' can be found in the Bridging Olink Explore 3072 to Olink Explore HT tutorial.
#'
#' @param lst_df A named list of the 2 input datasets. First element should be
#' the reference dataset from Olink Explore HT and the second element should
#' originate from Olink Explore 3072. (required)
#' @param ref_cols A named list with the column names to use. Exported from
#' olink_norm_input_check. (required)
#' @param not_ref_cols A named list with the column names from the non-reference
#'  dataset. Exported from olink_norm_input_check. (required)
#' @param bridge_samples Character vector of samples to be used for the
#' quantile mapping. (required)
#' @param ref_product Name of reference product. Must be one of "HT" or
#' "Reveal".
#'
#' @return A "tibble" of Olink data in long format containing both input
#' datasets with the quantile normalized quantifications.
#'
#' @keywords Normalization Quantile Smoothing
#'
#'
#' @examples
#' \donttest{
#' # Bridge samples
#' bridge_samples <- intersect(
#'   x = unique(OlinkAnalyze:::data_ht_small$SampleID),
#'   y = unique(OlinkAnalyze:::data_3k_small$SampleID)
#' ) |>
#'   (\(x) x[!grepl("CONTROL", x)])()
#'
#' # Run the internal function olink_norm_input_check
#' check_norm <- OlinkAnalyze:::olink_norm_input_check(
#'   df1 = OlinkAnalyze:::data_ht_small,
#'   df2 = OlinkAnalyze:::data_3k_small,
#'   overlapping_samples_df1 = bridge_samples,
#'   overlapping_samples_df2 = NULL,
#'   df1_project_nr = "P1",
#'   df2_project_nr = "P2",
#'   reference_project = "P1",
#'   reference_medians = NULL
#' )
#'
#' # Named list of input datasets
#' lst_df <- list(
#'   check_norm$ref_df,
#'   check_norm$not_ref_df
#' )
#' names(lst_df) <- c(check_norm$ref_name, check_norm$not_ref_name)
#'
#' ref_cols <- check_norm$ref_cols
#' not_ref_cols <- check_norm$not_ref_cols
#'
#' qs_result <- OlinkAnalyze:::olink_normalization_qs(
#'  lst_df = lst_df,
#'  ref_cols = ref_cols,
#'  not_ref_cols = not_ref_cols,
#'  bridge_samples = bridge_samples,
#'  ref_product = "HT"
#' )
#' }
#'
olink_normalization_qs <- function(lst_df,
                                   ref_cols,
                                   not_ref_cols,
                                   bridge_samples,
                                   ref_product) {

  if (ref_product == "HT") {
    num_samples <- 40L
  } else if (ref_product == "Reveal") {
    num_samples <- 32L
  } else {
    cli::cli_abort(
      "Reference product must be HT or Reveal."
    )
  }

  # main QS normalization function
  ecdf_transform_npx <- function(data,
                                 quant_col,
                                 count_ref_col,
                                 num_samples) {
    # Briefly:
    # Take the ECDF of the reference quantification (e.g. NPX from Olink Explore
    # HT). The inverse of the ECDF will be in x ~ y OR
    # ECDF(not_ref_quant) ~ ref_quant. Using this inverse, the Olink Explore 3k
    # quantification ECDF values should be x/not_ref_quant, and the Olink
    # Explore HT quantification values should be y/ref_quant.

    # remove outliers based on low counts and keep only bridge samples to model
    model_data_joined <- data |>
      dplyr::filter(
        .data[[count_ref_col]] > 10L
        & .data[["bridge_sample"]] == TRUE
      )

    # Minimal number of bridge samples required to for the function to work. If
    # not met, we just return NA.
    if (nrow(model_data_joined) < num_samples) {
      return(rep(x = NA_real_, times = nrow(data)))
    }

    # quantiles of not reference quantification (e.g. NPX from 3K) mapped to
    # quantiles of reference quantification (e.g. NPX from HT)
    notref_ecdf <- environment(
      fun = model_data_joined |>
        dplyr::pull(
          .data[[quant_col$notref]]
        ) |>
        stats::ecdf()
    )
    # the inverse if the ECDF are the quantiles
    notref_map_quantiles <- model_data_joined |> # nolint
      dplyr::pull(
        .data[[quant_col$ref]]
      ) |>
      stats::quantile(
        probs = notref_ecdf$y,
        names = FALSE
      )

    # numeric vector of quantifications for the non-reference dataset
    #
    # NOTE: this variable is used in "notref_knots" and "spline_model". We need
    # to make sure that the name of the variable matches the column name of the
    # data.frame provided as input in the argument "newdata" when the
    # "spline_model" function is called. If the names fo not match, there will
    # be an error!
    notref_quant <- model_data_joined |>
      dplyr::pull(
        .data[[quant_col$notref]]
      ) |>
      unique() |>
      sort()

    # quantile points used for adapting the non linear spline
    notref_knots <- stats::quantile( # nolint
      x = notref_quant,
      probs = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95),
      names = FALSE
    )

    # the non linear model
    spline_model <- stats::lm(
      formula = notref_map_quantiles ~ splines::ns(x = notref_quant,
                                                   knots = notref_knots)
    )

    # output - predict
    # here we use the the input from data to predict values from all the
    # samples, bridging and non-bridging
    notref_predictions <- stats::predict(
      object = spline_model,
      newdata = data |>
        dplyr::select(
          dplyr::all_of(
            c("notref_quant" = quant_col$notref)
          )
        )
    ) |>
      # removing names of the predictions
      unname()

    return(notref_predictions)
  }

  # cleanup input datasets ----

  # also keep relevant columns only
  lst_df_clean <- lapply(
    lst_df,
    function(l_df) {
      l_df |>
        dplyr::filter(
          # only customer samples
          dplyr::if_any(tidyselect::any_of(c(ref_cols$sample_type,
                                             not_ref_cols$sample_type)),
                 ~ . =="SAMPLE")
          # remove internal control assays
          & .data[["AssayType"]] == "assay"
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

  # this also adds
  df_combo <- lst_df_clean |>
    # append the datasets to each other
    dplyr::bind_rows(
      .id = "Project"
    ) |>
    # pivot to wider so that there is one entry for each concatenated assay and
    # sample identifier
    tidyr::pivot_wider(
      names_from = dplyr::all_of(
        c("Project")
      ),
      values_from = dplyr::all_of(
        c(
          ref_cols$quant,
          "Count"
        )
      )
    ) |>
    dplyr::mutate(
      bridge_sample = .data[[ref_cols$sample_id]] %in% .env[["bridge_samples"]]
    )
  rm(lst_df_clean)

  # remove non-bridge samples from reference dataset ----

  # bridge samples between the reference dataset and the non-reference dataset
  # are used to predict the NPX values of the non-bridge samples from the
  # non-reference dataset. For that reason the non-bridge samples from reference
  # dataset should be removed from the QS normalization.
  df_combo <- df_combo |>
    dplyr::filter(
      # keep bridge samples
      .data[["bridge_sample"]] == TRUE
      # and samples in the non-reference dataset
      | .data[[ref_cols$sample_id]] %in%
        unique(dplyr::pull(lst_df[[2L]], .data[[ref_cols$sample_id]]))
    )

  # ecdf transformation ----

  # help functions that allow masking of column names from the input datasets
  # after the pivot_wider
  quant_col <- paste(ref_cols$quant, names(lst_df), sep = "_") |>
    as.list()
  names(quant_col) <- c("ref", "notref")
  cnt_ref_col <- paste("Count", names(lst_df[1L]), sep = "_")

  # transform the data
  ecdf_transform <- df_combo |>
    # compute by assay identifier
    dplyr::group_by(
      .data[[ref_cols$olink_id]]
    ) |>
    dplyr::mutate(
      QSNormalizedNPX = ecdf_transform_npx(
        data = dplyr::pick(
          dplyr::all_of(
            c(ref_cols$sample_id,
              unname(unlist(quant_col)),
              cnt_ref_col,
              "bridge_sample")
          )
        ),
        quant_col = .env[["quant_col"]],
        count_ref_col = .env[["cnt_ref_col"]],
        num_samples = num_samples
      )
    ) |>
    dplyr::ungroup()

  # warning message for assays that were not QS normalized because of lack of
  # bridge samples (bridge samples < 40)
  num_notref_samples <- df_combo$SampleID |> unique() |> length()

  num_not_qs_norm_assays <- ecdf_transform |>
    dplyr::group_by(
      .data[[ref_cols$olink_id]]
    ) |>
    dplyr::summarise(
      na_npx = sum(is.na(.data[[quant_col$notref]]), na.rm = TRUE),
      na_qs = sum(is.na(.data[["QSNormalizedNPX"]]), na.rm = TRUE)
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(
      .data[["na_npx"]] == 0L
      & .data[["na_qs"]] == .env[["num_notref_samples"]]
    )

  # warning
  if (nrow(num_not_qs_norm_assays) > 0L) {
    cli::cli_warn(
      message = c(
        "Insufficient number of bridge samples to perform QS normalization.
        {cli::qty(num_not_qs_norm_assays)} There {?is/are}
        {.val {nrow(num_not_qs_norm_assays)}} assay{?s} with fewer than
        {.val {num_samples}} bridge samples for QS normalization!"
      ),
      wrap = FALSE
    )
  }
  rm(num_notref_samples, num_not_qs_norm_assays)

  # keep only relevant columns
  ecdf_transform <- ecdf_transform |>
    dplyr::select(
      dplyr::all_of(
        c(ref_cols$sample_id, ref_cols$olink_id, "QSNormalizedNPX")
      )
    ) |>
    # add column "Project"
    dplyr::mutate(
      Project = names(lst_df)[2L]
    )

  # ref columns for output ----

  df_ref_output <- lst_df[[1L]] |>
    dplyr::filter(
      # only customer samples
      .data[["SampleType"]] == "SAMPLE"
      # remove internal control assays
      & .data[["AssayType"]] == "assay"
    ) |>
    dplyr::select(
      dplyr::all_of(
        c("SampleID", "OlinkID", "QSNormalizedNPX" = "NPX")
      )
    ) |>
    dplyr::mutate(
      Project = names(lst_df)[1L]
    )

  # output dataset ----

  df_qq_norm <- df_ref_output |>
    dplyr::bind_rows(ecdf_transform)

  return(df_qq_norm)
}



#' Formatting the output of olink_normalization_product for seamless use with
#' downstream Olink Analyze functions.
#'
#' @author
#' Danai G. Topouza
#'
#' @description
#' Replaces the NPX values of the non-reference project by the Median Centered
#' or QS Normalized NPX, according to the Bridging Recommendation. Edits the
#' BridgingRecommendation column to indicate whether an assay is NotBridgeable,
#' NotOverlapping, MedianCentering, or QuantileSmoothing bridged. Replaces
#' OlinkID by the concatenation of the Explore HT and Explore 3072 OlinkIDs to
#' record the OlinkIDs from both projects for bridgeable assays. Assays that are
#' NotBridgeable or NotOverlapping retain their original non-reference OlinkIDs
#' and NPX values. Replaces SampleID with the concatenation of SampleID and
#' Project to make unique sample IDs for downstream analysis. Removes internal
#' and external controls. Removes  MedianCenteredNPX, QSNormalizedNPX,
#' OlinkID_E3072 columns.
#'
#' @param df_norm A "tibble" of Olink data in long format resulting from the
#' olink_normalization_product function.
#' @param df1 First dataset to be used for normalization, pre-normalization.
#' Must match df1 used in olink_normalization product bridging.
#' @param df2 Second dataset to be used for normalization, pre-normalization.
#' Must match df2 used in olink_normalization product bridging.
#' @param df1_project_nr Project name of first dataset. Must match name used in
#' olink_normalization product bridging.
#' @param df2_project_nr Project name of second dataset. Must match name used in
#' olink_normalization product bridging.
#' @param reference_project Project name of reference project. Must match name
#' used in olink_normalization product bridging and be one of df1_project_nr or
#' df2_project_nr.
#' @param ref_product Name of reference product. Must be one of "HT" or
#' "Reveal".
#'
#' @return A "tibble" of Olink data in long format containing both input
#' datasets with the bridged NPX quantifications, with the above
#' modifications.
#'
#' @examples
#' \donttest{
#' # bridge samples
#' bridge_samples <- intersect(
#'   x = unique(OlinkAnalyze:::data_ht_small$SampleID),
#'   y = unique(OlinkAnalyze:::data_3k_small$SampleID)
#' ) |>
#'   (\(x) x[!grepl("CONTROL", x)])()
#'
#' # run olink_normalization
#' df_norm <- olink_normalization(
#'   df1 = OlinkAnalyze:::data_ht_small,
#'   df2 = OlinkAnalyze:::data_3k_small,
#'   overlapping_samples_df1 = bridge_samples,
#'   df1_project_nr = "Explore HT",
#'   df2_project_nr = "Explore 3072",
#'   reference_project = "Explore HT"
#' )
#'
#' # format output
#' OlinkAnalyze:::olink_normalization_product_format(
#'   df_norm = df_norm,
#'   df1 = OlinkAnalyze:::data_ht_small,
#'   df2 = OlinkAnalyze:::data_3k_small,
#'   df1_project_nr = "Explore HT",
#'   df2_project_nr = "Explore 3072",
#'   reference_project = "Explore HT",
#'   ref_product = "HT"
#' )
#' }
#'
olink_normalization_product_format <- function(df_norm,
                                               df1,
                                               df1_project_nr,
                                               df2,
                                               df2_project_nr,
                                               reference_project,
                                               ref_product) {

  # Extract data for assays = "NotBridgeable" ----

  df_not_bridgeable <- df_norm |>
    dplyr::filter(
      .data[["SampleType"]] == "SAMPLE" # Remove controls
      & .data[["AssayType"]] == "assay"
      & .data[["BridgingRecommendation"]] == "NotBridgeable"
    ) |>
    dplyr::mutate(
      OlinkID = dplyr::if_else(
        .data[["Project"]] == .env[["reference_project"]],
        .data[["OlinkID"]],
        .data[["OlinkID_E3072"]]
      )
    )

  # Extract data from non-overlapping assays ----

  oid_ref_notref <- mapping_file_id(ref_product = ref_product) |>
    dplyr::select(
      dplyr::starts_with("OlinkID_")
    ) |>
    unlist(use.names = FALSE)

  df1_no_overlap <- df1 |>
    dplyr::filter(
      .data[["SampleType"]] == "SAMPLE" # Remove controls
      & !(.data[["OlinkID"]] %in% .env[["oid_ref_notref"]])
    ) |>
    dplyr::mutate(
      Project = .env[["df1_project_nr"]],
      BridgingRecommendation = "NotOverlapping"
    )

  df2_no_overlap <- df2 |>
    dplyr::filter(
      .data[["SampleType"]] == "SAMPLE" # Remove controls
      & !(.data[["OlinkID"]] %in% .env[["oid_ref_notref"]])
    ) |>
    dplyr::mutate(
      Project = .env[["df2_project_nr"]],
      BridgingRecommendation = "NotOverlapping"
    )

  # Keep the data following BridgingRecommendation for bridgeable assays ----

  df_format <- df_norm |>
    dplyr::filter(
      .data[["SampleType"]] == "SAMPLE"
      & .data[["BridgingRecommendation"]] != "NotBridgeable"
      & .data[["AssayType"]] == "assay"
    ) |> # Remove controls
    dplyr::mutate(
      OlinkID = paste0(.data[["OlinkID"]], "_", .data[["OlinkID_E3072"]]),
      NPX = dplyr::case_when(
        .data[["BridgingRecommendation"]] == "MedianCentering" ~
          .data[["MedianCenteredNPX"]],
        .data[["BridgingRecommendation"]] == "QuantileSmoothing" ~
          .data[["QSNormalizedNPX"]],
        .default = .data[["NPX"]]
      )
    )

  # combine data and sort ----

  df_full <- df_format |>
    dplyr::bind_rows(
      df_not_bridgeable
    ) |>
    dplyr::bind_rows(
      df1_no_overlap
    ) |>
    dplyr::bind_rows(
      df2_no_overlap
    ) |>
    dplyr::mutate(
      SampleID = paste0(.data[["SampleID"]], "_", .data[["Project"]])
    ) |>
    dplyr::select( # Remove extra columns
      -dplyr::any_of(
        c("MedianCenteredNPX", "QSNormalizedNPX", "OlinkID_E3072")
      )
    ) |>
    dplyr::arrange(
      .data[["Project"]], .data[["SampleID"]]
    )

  return(df_full)
}
