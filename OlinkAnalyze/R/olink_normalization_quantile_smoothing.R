#' Quantile smoothing normalization of all proteins between two NPX projects.
#'
#' Normalizes two NPX projects (data frames) using shared samples.\cr\cr
#' @param exploreht_df Data frame of the first project, must be Explore HT
#' data (required). (default: reference)
#' @param explore3072_df Data frame of the second project, must be Explore
#' 3072 data (required). (default : new)
#' @param bridge_samples Named list of 2 arrays containing, SampleID of shared
#' samples to be used for the calculation of adjustment factor. The
#' names of the two arrays should be DF_ht and DF_3k corresponding to projects
#' HT and 3072, respectively. Arrays should be of equal length and index of each
#' entry should correspond to the same sample. (required)
#' @param exploreht_name Name of Explore HT project (default: reference)
#' @param explore3072_name Name of Explore 3072  project (default: new)
#'
#' @return A "tibble" of NPX data in long format containing qs normalized NPX
#' values, including adjustment factors and name of project.
#'
#' @keywords Normalization; Quantile ; Smoothing
#' @author Amrita Kar
#' @author Marianne Sandin
#' @author Masoumeh Sheikhi
#'
olink_normalization_qs <- function(exploreht_df,
                                   explore3072_df,
                                   bridge_samples,
                                   exploreht_name = "reference",
                                   explore3072_name = "new") {

  # main QQ normalization function
  ecdf_transform_npx <- function(data,
                                 quant_col,
                                 count_ref_col) {

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
      )

    # If number of datapoints are < 24, no spline is fitted
    if (nrow(model_data_joined) < 24) {

      preds <- data |>
        dplyr::select(SampleID)

      preds$QSNormalizedNPX <- NA

      return(preds)
    }

    # quantiles of not reference quantification (e.g. NPX from 3K) mapped to
    # quantiles of reference quantification (e.g. NPX from HT)
    notref_ecdf <- environment(
      fun = model_data |>
        dplyr::pull(
          .data[[quant_col$notref]]
        ) |>
        stats::ecdf()
    )
    # the inverse if the ECDF are the quantiles
    mapped_3k <- model_data |>
      dplyr::pull(
        .data[[quant_col$ref]]
      ) |>
      stats::quantile(
        probs = notref_ecdf$y,
        names = FALSE
      )

    npx_3k <- sort(unique(model_data_joined$NPX_3k))

    #The quantile points used for adapting the nonelinear spline
    knots_npx3k <- stats::quantile(npx_3k, probs = c(0.05, 0.1, 0.25, 0.5, 0.75,
                                                     0.9, 0.95))

    #The nonlinear model
    spline_model <- lm(mapped_3k ~ splines::ns(npx_3k, knots = knots_npx3k))

    #Output (just making sure that correct points are output)
    newdata <- as.data.frame(c(explore3072_df$NPX))
    colnames(newdata) <- "npx_3k"
    preds <- as.data.frame(stats::predict(spline_model, newdata = newdata))
    colnames(preds) <- "QSNormalizedNPX"
    preds$SampleID <- explore3072_df$SampleID
    preds$OlinkID <- explore3072_df$OlinkID

    return(preds)
  }

  # place bridge samples side by side in a data frame
  update_sampleid <- dplyr::bind_cols(bridge_samples) |>
    dplyr::rename("SampleID_df1" = "DF_ht",
                  "SampleID_df2" = "DF_3k")

  # change the SampleID of the non-reference data frame to match the bridging
  # samples from the reference data frame. This is done because the
  # OlinkAnalyze::olink_normalization function requires so.

  explore3072_df <- explore3072_df |>
    dplyr::left_join(update_sampleid, by = c("SampleID" = "SampleID_df2")) |>
    dplyr::mutate(SampleID_df1 = dplyr::if_else(is.na(SampleID_df1),
                                                SampleID,
                                                SampleID_df1)) |>
    dplyr::select(-SampleID) |>
    dplyr::rename("SampleID" = "SampleID_df1")

  # Creating concatenated OlinkID containing HT and 3K OlinkID that is unique
  exploreht_df   <- map_oid_ht_3k(exploreht_df) |>
    dplyr::filter(!is.na(OlinkID)) |>
    dplyr::mutate(
      Project = exploreht_name
    )
  explore3072_df <- map_oid_ht_3k(explore3072_df) |>
    dplyr::filter(!is.na(OlinkID)) |>
    dplyr::mutate(
      Project = explore3072_name
    )

  model_explore3072_df <- explore3072_df |>
    dplyr::filter(SampleID %in% bridge_samples$DF_3k) |>
    dplyr::filter(!(is.na(NPX))) |>
    dplyr::rename(NPX_3k = NPX) |>
    dplyr::select(all_of(c("SampleID", "OlinkID", "NPX_3k"))) |>
    dplyr::distinct()

  model_exploreht_df <- exploreht_df |>
    dplyr::filter(SampleID %in% bridge_samples$DF_ht) |>
    dplyr::filter(!(is.na(NPX))) |>
    dplyr::rename(NPX_ht = NPX) |>
    # count is only observed for HT
    dplyr::select(all_of(c("SampleID", "OlinkID", "NPX_ht", "Count"))) |>
    dplyr::distinct()

  model_data_joined <- model_explore3072_df |>
    dplyr::inner_join(model_exploreht_df, by = c("SampleID", "OlinkID")) |>
    na.omit() |>
    dplyr::distinct()

  ecdf_transform <- model_data_joined |>
    dplyr::reframe(ecdf_transform_npx(data = model_data_joined),
                   by = OlinkID) |>
    dplyr::mutate(Project = explore3072_name) |>
    dplyr::ungroup() |>
    dplyr::select(-by)

  all_ht <- exploreht_df |>
    dplyr::mutate(QSNormalizedNPX = NPX) |>
    dplyr::select(all_of(c("SampleID", "OlinkID", "QSNormalizedNPX"))) |>
    dplyr::distinct() |>
    dplyr::mutate(Project = exploreht_name)

  df_ht_3k_w_ecdf <- dplyr::bind_rows(all_ht, ecdf_transform) |>
    dplyr::mutate(OlinkID_ExploreHT = substr(OlinkID, 1, 8),
                  OlinkID_Explore3K = substr(OlinkID, 10, 18)) |>
    dplyr::rename(OlinkID_concat = OlinkID) |>
    dplyr::select(.data[["SampleID"]],
                  .data[["Project"]],
                  .data[["OlinkID_concat"]],
                  .data[["QSNormalizedNPX"]])

  return(df_ht_3k_w_ecdf)
}
