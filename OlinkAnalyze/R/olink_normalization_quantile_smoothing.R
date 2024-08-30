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

  map_oid_ht_3k <- function(explore_df) {
    oid_ht_3k_mapping <- eHT_e3072_mapping

    oid_ht_3k_mapping <- oid_ht_3k_mapping |>
      mutate(OlinkID_HT_3K = paste(.data[["OlinkID_HT"]],
                                   .data[["OlinkID_E3072"]], sep = "_")) |>
      select(.data[["OlinkID_HT"]], .data[["OlinkID_E3072"]],
             .data[["OlinkID_HT_3K"]])

    if (all(explore_df |>
            distinct(Panel) |>
            pull() |>
            na.omit() %in% c("Explore_HT", "Explore HT"))) {
      explore_df_linked_oid <- explore_df |>
        dplyr::filter(
          stringr::str_detect(string = .data[["SampleType"]],
                              pattern = "SAMPLE"),
          stringr::str_detect(string = .data[["AssayType"]],
                              pattern = "assay")
        ) |>
        inner_join(oid_ht_3k_mapping,
                   relationship = "many-to-many",
                   by = c("OlinkID" = "OlinkID_HT")
        ) |>
        mutate(OlinkID = .data[["OlinkID_HT_3K"]]) |>
        select(-.data[["OlinkID_HT_3K"]], -.data[["OlinkID_E3072"]]) |>
        distinct()
    } else if (all(explore_df |>
                   distinct(Panel) |>
                   pull() |>
                   na.omit() %in% c(
                     "Cardiometabolic", "Cardiometabolic_II",
                     "Inflammation", "Inflammation_II",
                     "Neurology", "Neurology_II",
                     "Oncology", "Oncology_II"
                   ))) {
      explore_df_linked_oid <- explore_df |>
        dplyr::filter(
          stringr::str_detect(string = .data[["SampleType"]],
                              pattern = "SAMPLE"),
          stringr::str_detect(string = .data[["AssayType"]],
                              pattern = "assay")
        ) |>
        inner_join(oid_ht_3k_mapping,
                   relationship = "many-to-many",
                   by = c("OlinkID" = "OlinkID_E3072")
        ) |>
        mutate(OlinkID = .data[["OlinkID_HT_3K"]]) |>
        select(-.data[["OlinkID_HT_3K"]], -.data[["OlinkID_HT"]]) |>
        distinct()
    } else {
      explore_df_linked_oid <- explore_df
      print("The provided data frame is not an Explore HT or Explore 384 data frame. No changes made.")
    }

    return(explore_df_linked_oid)
  }


  ecdf_transform_npx <- function(data = data) {

    # Outlier removal based on low counts threshold, think the trimodal assays
    model_data_joined <- data |> dplyr::filter(Count > 10)

    # If number of datapoints are < 24, no spline is fitted
    if (nrow(model_data_joined) < 24) {

      preds <- data |>
        dplyr::select(SampleID)

      preds$preds <- NA

      return(preds)
    }

    #Quantiles of 3k mapped to quantiles of HT
    mapped_3k <- stats::quantile(model_data_joined$NPX_ht,
      sort(stats::ecdf(model_data_joined$NPX_3k)(model_data_joined$NPX_3k))
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

  # Filter low count samples for both platforms
  model_explore3072_df <- explore3072_df |>
    dplyr::filter(SampleID %in% bridge_samples$DF_3k) |>
    dplyr::filter(!(is.na(NPX))) |> # count is only observed for HT
    dplyr::rename(NPX_3k = NPX) |>
    dplyr::select(all_of(c("SampleID", "OlinkID", "NPX_3k"))) |>
    dplyr::distinct()

  model_exploreht_df <- exploreht_df |>
    dplyr::filter(SampleID %in% bridge_samples$DF_ht) |>
    dplyr::filter(!(is.na(NPX))) |>
    dplyr::rename(NPX_ht = NPX) |>
    dplyr::select(all_of(c("SampleID", "OlinkID", "NPX_ht", "Count"))) |>
    dplyr::distinct()

  model_data_joined <- model_explore3072_df |>
    dplyr::inner_join(model_exploreht_df, by = c("SampleID", "OlinkID")) |>
    na.omit() |>
    dplyr::distinct()

  ecdf_transform <- model_data_joined |>
    dplyr::group_by(OlinkID) |>
    dplyr::reframe(ecdf_transform_npx(dplyr::pick(everything()))) |>
    dplyr::ungroup() |>
    dplyr::mutate(Project = explore3072_name)

  # TODO : check with median normalized NPX, should it be the NPX value or NA??
  all_ht <- exploreht_df |>
    dplyr::mutate(QSNormalizedNPX = NPX) |>
    dplyr::select(all_of(c("SampleID", "OlinkID", "QSNormalizedNPX"))) |>
    dplyr::distinct() |>
    dplyr::mutate(Project = exploreht_name)

  df_ht_3k_w_ecdf <- dplyr::bind_rows(all_ht, ecdf_transform) |>
    dplyr::mutate(OlinkID_ExploreHT = substr(OlinkID, 1, 8),
                  OlinkID_Explore3K = substr(OlinkID, 10, 18)) |>
    dplyr::rename(OlinkID_concat = OlinkID)

  return(df_ht_3k_w_ecdf)
}
