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
#' @param bridge_samples Character vector of samples to be used for the
#' quantile mapping. (required)
#'
#' @return A "tibble" of Olink data in long format containing both input
#' datatsets with the quantile normalized quantifications.
#'
#' @keywords Normalization; Quantile ; Smoothing
#'
#'
#' @examples
#' \dontrun{
#' \donttest{
#'# Bridge samples
#' bridge_samples <- intersect(
#' x = unique(data_ht$SampleID),
#' y = unique(data_3k$SampleID)) |>
#'   (\(x) x[!grepl("CONTROL", x)])()
#'
#'# Run the internal function olink_norm_input_check()
#' ref_col <- olink_norm_input_check (
#' df1 = data_ht, df2 = data_3k,
#' overlapping_samples_df1 = bridge_samples,
#' overlapping_samples_df2 = NULL,
#' df1_project_nr = "P1",
#' df2_project_nr = "P2",
#' reference_project = "P1",
#' reference_medians = NULL
#' )
#'
#'# Named list of input datasets
#' lst_df <- list (
#' P1 = ref_col$ref_df,
#' P2 = ref_col$not_ref_df
#' )
#'
#' ref_cols <- ref_col$ref_cols
#'
#' qs_result <- olink_normalization_qs (
#' lst_df = lst_df,
#' ref_cols = ref_cols,
#' bridge_samples = bridge_samples
#' )
#' }
#' }
#'
#'
olink_normalization_qs <- function(lst_df,
                                   ref_cols,
                                   bridge_samples) {

  # main QQ normalization function
  ecdf_transform_npx <- function(data,
                                 quant_col,
                                 count_ref_col,
                                 num_samples = 24L) {

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
          .data[["SampleType"]] == "SAMPLE"
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
  # dataset should be removed from the QQ normalization.
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
        count_ref_col = .env[["cnt_ref_col"]]
      )
    ) |>
    dplyr::ungroup() |>
    # keep only relevant columns
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
