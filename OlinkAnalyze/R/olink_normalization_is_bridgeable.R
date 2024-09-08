#' Identify if assays shared between Olink Explore 3072 and Olink Explore HT are
#' can be bridged
#'
#' @author
#'   Amrita Kar
#'   Marianne Sandin
#'   Danai G. Topouza
#'   Klev Diamanti
#'
#' @description
#' The function uses a dataset from Olink Explore 3072 and a datasets from Olink
#' Explore HT, and examines if the matched assays between the two products can
#' be normalized to each other. The input datasets should be exported from Olink
#' software and should not be altered prior to importing them to this function.
#'
#' @param data_HT Olink Explore HT dataset to be used for normalization
#' (required).
#' @param data_Explore384 Olink Explore 3072 dataset to be used for
#' normalization (required).
#'
#' @return A "tibble" in long format with the following columns:
#' \itemize{
<<<<<<< HEAD:OlinkAnalyze/R/bridgeable.R
#'    \item{OlinkID:} Underscore-separated Olink identifiers of matching assays
#'    between Olink Explore HT and Olink Explore 3072.
#'    \item{IsBridgeable:} A boolean flag indicating whether the matching assays
#'    are considered as bridgeable or not.
=======
#'    \item{OlinkID_concat:} Concatenated Olink Explore HT and Explore 3072 IDs
#'    \item{Bridgeable:} Bridging Flag
>>>>>>> 52c0f61618323ec5e3ba1cb416552cd66a826d0a:OlinkAnalyze/R/olink_normalization_is_bridgeable.R
#' }
#'
#' @keywords NPX Bridging
#'
#' @examples
#' \donttest{
#' results <- OlinkAnalyze:::olink_normalization_is_bridgeable(
#'   data_Explore384 = OlinkAnalyze:::data_3k,
#'   data_HT = OlinkAnalyze:::data_ht
#' )
#' }
#'
<<<<<<< HEAD:OlinkAnalyze/R/bridgeable.R
bridgeable <- function(data_Explore384,
                       data_HT) {
=======
#' @author Amrita Kar
#' @author Marianne Sandin
#' @author Danai G. Topouza


olink_normalization_is_bridgeable <- function(data_Explore384,
                                              data_HT) {

  set.seed(123)

>>>>>>> 52c0f61618323ec5e3ba1cb416552cd66a826d0a:OlinkAnalyze/R/olink_normalization_is_bridgeable.R
  # add a concatenated version of the 3K-HT OlinkIDs to the reference and non-
  # reference data frames. This is done because the olink_normalization
  # function requires unique, overlapping OlinkIDs. This approach allows us to
  # create unique IDs even with the repeated correlation assays in HT and 3K.

  map_oid_ht_3k <- function(explore_df) {
<<<<<<< HEAD:OlinkAnalyze/R/bridgeable.R
    oid_ht_3k_mapping <- eHT_e3072_mapping |>
=======

    oid_ht_3k_mapping <- OlinkAnalyze:::eHT_e3072_mapping |>
>>>>>>> 52c0f61618323ec5e3ba1cb416552cd66a826d0a:OlinkAnalyze/R/olink_normalization_is_bridgeable.R
      dplyr::mutate(OlinkID_HT_3K =
                      paste(.data[["OlinkID_HT"]],
                            .data[["OlinkID_E3072"]], sep = "_")) |>
      dplyr::select(.data[["OlinkID_HT"]], .data[["OlinkID_E3072"]],
                    .data[["OlinkID_HT_3K"]])

    if (all(explore_df |>
            dplyr::distinct(Panel) |>
            dplyr::pull() |>
            na.omit() %in%
            c("Explore_HT", "Explore HT"))) {
      explore_df_linked_oid <- explore_df |>
        dplyr::filter(.data[["SampleType"]] == "SAMPLE" &
                        .data[["AssayType"]] == "assay") |>
        dplyr::inner_join(oid_ht_3k_mapping, relationship = "many-to-many",
                          by = c("OlinkID" = "OlinkID_HT")
        ) |>
        dplyr::mutate(OlinkID = .data[["OlinkID_HT_3K"]]) |>
        dplyr::select(-.data[["OlinkID_HT_3K"]], -.data[["OlinkID_E3072"]]) |>
        dplyr::distinct()
    }else if (all(explore_df |>
                  dplyr::distinct(Panel) |>
                  dplyr::pull() |>
                  na.omit() %in% c("Cardiometabolic", "Cardiometabolic_II",
                                   "Inflammation", "Inflammation_II",
                                   "Neurology", "Neurology_II",
                                   "Oncology", "Oncology_II"))) {
      explore_df_linked_oid <- explore_df |>
        dplyr::filter(.data[["SampleType"]] == "SAMPLE" &
                        .data[["AssayType"]] == "assay") |>
        dplyr::inner_join(oid_ht_3k_mapping, relationship = "many-to-many",
                          by = c("OlinkID" = "OlinkID_E3072")
        ) |>
        dplyr::mutate(OlinkID = .data[["OlinkID_HT_3K"]]) |>
        dplyr::select(-.data[["OlinkID_HT_3K"]], -.data[["OlinkID_HT"]]) |>
        dplyr::distinct()
    } else {
      explore_df_linked_oid <- explore_df
      print("The provided data frame is not an Explore HT or Explore 384 data
            frame. No changes made.")
    }

    return(explore_df_linked_oid)
  }

  outlier_removal_iqr <- function(data = data, iqr_value) {
    data_out <- data |>
      dplyr::filter(.data[["NPX_3k"]] < median(.data[["NPX_3k"]]) +
                      .env[["iqr_value"]] * IQR(.data[["NPX_3k"]])) |>
      dplyr::filter(.data[["NPX_3k"]] > median(.data[["NPX_3k"]]) -
                      .env[["iqr_value"]] * IQR(.data[["NPX_3k"]])) |>
      dplyr::filter(.data[["NPX_ht"]] < median(.data[["NPX_ht"]]) +
                      .env[["iqr_value"]] * IQR(.data[["NPX_ht"]])) |>
      dplyr::filter(.data[["NPX_ht"]] > median(.data[["NPX_ht"]]) -
                      .env[["iqr_value"]] * IQR(.data[["NPX_ht"]]))

    return(data_out)
  }

  # Replaced group_modify with reframe()
  r2_results <- function(data = data) {

    data_out <- data |>
      dplyr::group_by(dplyr::pick(dplyr::all_of(c("OlinkID")))) |>
      dplyr::reframe(outlier_removal_iqr(dplyr::pick(everything()),
                                         iqr_value = 3)) |>
      dplyr::group_by(dplyr::pick(dplyr::all_of(c("OlinkID")))) |>
      filter(.data[["Count_3k"]] > 10 & .data[["Count_ht"]] > 10) |>
      dplyr::mutate(R2_lm = stats::cor(x = .data[["NPX_3k"]],
                                       y = .data[["NPX_ht"]])^2) |>
      dplyr::ungroup()

    return(data_out)
  }


  ks_results <- function(data = data) {

    data_out <- data |>
      #dplyr::group_by(.data[["Gene"]], .data[["ids"]]) |>
      dplyr::group_by(dplyr::pick(dplyr::all_of(c("OlinkID")))) |>
      dplyr::reframe(outlier_removal_iqr(dplyr::pick(everything()),
                                         iqr_value = 3)) |>
      #dplyr::group_by(.data[["Gene"]], .data[["ids"]]) |>
      dplyr::group_by(dplyr::pick(dplyr::all_of(c("OlinkID")))) |>
      filter(.data[["Count_3k"]] > 10 & .data[["Count_ht"]] > 10) |>
      dplyr::summarise(ks = stats::ks.test(
        .data[["NPX_ht"]], .data[["NPX_3k"]])$statistic, .groups = "keep") |>
      dplyr::ungroup()

    return(data_out)
  }


<<<<<<< HEAD:OlinkAnalyze/R/bridgeable.R
  data_Explore384 <- data_Explore384 |>
    dplyr::filter(
      stringr::str_detect(string = .data[["SampleType"]], pattern = "SAMPLE"),
      stringr::str_detect(string = .data[["AssayType"]], pattern = "assay")
    ) |>
    map_oid_ht_3k() |>
    dplyr::mutate(OlinkID_Explore384 = substr(.data[["OlinkID"]], 10, 18)) |>
    dplyr::rename(
      Gene = .data[["Assay"]],
      NPX_3k = .data[["NPX"]],
      ids = .data[["OlinkID"]]
    ) |>
    dplyr::select(.data[["SampleID"]],
                  .data[["ids"]],
                  .data[["OlinkID_Explore384"]],
                  .data[["NPX_3k"]],
                  .data[["Count"]]) |>
    dplyr::rename(Count_3k = .data[["Count"]]) |>
    distinct()

  # Count refers to HT counts
  data_HT <- data_HT |>
    dplyr::filter(
      stringr::str_detect(string = .data[["SampleType"]], pattern = "SAMPLE"),
      stringr::str_detect(string = .data[["AssayType"]], pattern = "assay")
    ) |>
    map_oid_ht_3k() |>
    dplyr::rename(
      Gene = .data[["Assay"]],
      NPX_ht = .data[["NPX"]],
      ids = .data[["OlinkID"]]
    ) |>
    dplyr::mutate(OlinkID = substr(.data[["ids"]], 1, 8)) |>
    dplyr::select(.data[["SampleID"]],
                  .data[["Gene"]],
                  .data[["OlinkID"]],
                  .data[["ids"]],
                  .data[["UniProt"]],
                  .data[["NPX_ht"]],
                  .data[["Count"]]) |>
    dplyr::mutate(Count_ht = .data[["Count"]]) |>
    distinct()

=======
>>>>>>> 52c0f61618323ec5e3ba1cb416552cd66a826d0a:OlinkAnalyze/R/olink_normalization_is_bridgeable.R
  data_mapped <- data_Explore384 |>
    map_oid_ht_3k() |>
    dplyr::filter(.data[["SampleID"]] %in% data_HT$SampleID) |> # Sample IDs must match
    bind_rows(data_HT |>
                map_oid_ht_3k() |>
                mutate(Block = as.character(.data[["Block"]]))) |>
    mutate(Platform = ifelse(Panel == "Explore HT", "ht", "3k")) |>
    dplyr::filter(.data[["SampleType"]] == "SAMPLE" &
                    .data[["AssayType"]] == "assay") |>
    dplyr::mutate(OlinkID_Explore384 = substr(.data[["OlinkID"]], 10, 18)) |>
    dplyr::mutate(OlinkID_HT = substr(.data[["OlinkID"]], 1, 8)) |>
    dplyr::select(.data[["SampleID"]],
                  .data[["Assay"]],
                  .data[["OlinkID"]],
                  .data[["OlinkID_HT"]],
                  .data[["OlinkID_Explore384"]],
                  .data[["UniProt"]],
                  .data[["NPX"]],
                  .data[["Count"]],
                  .data[["Platform"]]) |>
    tidyr::pivot_wider(names_from = .data[["Platform"]],
                       values_from = c(.data[["NPX"]], .data[["Count"]]),
                       names_glue = "{.value}_{Platform}") |>
    dplyr::group_by(.data[["OlinkID"]]) |>
    # Drop assays with count < 10
    dplyr::filter(.data[["Count_3k"]] > 10 & .data[["Count_3k"]] > 10) |>
    dplyr::mutate(range_ht =
                    stats::quantile(.data[["NPX_ht"]],
                                    probs = 0.9) -
                    stats::quantile(.data[["NPX_ht"]],
                                    probs = 0.1),
                  # using 10% to 90% quantiles for checking range /IQR
                  range_3k = stats::quantile(.data[["NPX_3k"]],
                                             probs = 0.9) -
                    stats::quantile(.data[["NPX_3k"]],
                                    probs = 0.1),
                  range_diff = abs(.data[["range_ht"]] - .data[["range_3k"]]),
                  median_count_3k =
                    median(.data[["Count_3k"]],
                           na.rm = TRUE),
                  # if count less than 10 in 1 sample that assay is dropped
                  median_count_ht =
                    median(.data[["Count_ht"]],
                           na.rm = TRUE),
                  # if count less than 10 in 1 sample that assay is dropped
                  low_counts_flag = .data[["median_count_3k"]] < 150 |
                    .data[["median_count_ht"]] < 150) |>
    dplyr::ungroup()

  # selecting only required columns
  data_condensed <- data_mapped |>
    dplyr::select(.data[["SampleID"]],
                  .data[["Assay"]],
                  .data[["UniProt"]],
                  .data[["NPX_3k"]],
                  .data[["NPX_ht"]],
                  .data[["Count_ht"]],
                  .data[["Count_3k"]],
                  .data[["range_diff"]],
                  .data[["low_counts_flag"]],
                  .data[["OlinkID"]]) |>
    dplyr::ungroup() |>
    r2_results()

  not_bridgeable <- data_condensed |>
    dplyr::filter(.data[["range_diff"]] > 1 &
                    .data[["low_counts_flag"]] == TRUE &
                    .data[["R2_lm"]] < 0.8) |>
    dplyr::pull(.data[["OlinkID"]]) |>
    unique()

  # Adding type of normalization
  ks <- ks_results(data = data_mapped)

<<<<<<< HEAD:OlinkAnalyze/R/bridgeable.R
  bridge_table <- eHT_e3072_mapping |>
=======
  bridge_table <- OlinkAnalyze:::eHT_e3072_mapping |>
>>>>>>> 52c0f61618323ec5e3ba1cb416552cd66a826d0a:OlinkAnalyze/R/olink_normalization_is_bridgeable.R
    dplyr::mutate(Bridgeable =
                    dplyr::if_else(.data[["OlinkID"]] %in%
                                     not_bridgeable, "Not Bridgeable",
                                   "Yes")) |>
    dplyr::select(.data[["OlinkID"]],
                  .data[["Bridgeable"]]) |>
    dplyr::inner_join(ks,
                      by = c("OlinkID"))

  # Adding concatenated ID
  concat_ids <- data_mapped |>
    dplyr::select(.data[["OlinkID"]],
                  .data[["OlinkID_Explore384"]],
                  .data[["OlinkID_HT"]]) |>
    dplyr::distinct() # Keeps distinct list of OlinkIDs, instead of one per sample

  bridge_table <- bridge_table |>
    dplyr::mutate(Bridgeable = case_when(.data[["Bridgeable"]] == "Yes" &
                                           ks <= 0.2 ~ "Median Centered",
                                         .data[["Bridgeable"]] == "Yes" &
                                           ks > 0.2 ~ "Quantile Smoothing",
                                         .default =
                                           as.character(.data[["Bridgeable"]]))
    ) |>
    dplyr::left_join(concat_ids,
                     by = c("OlinkID")) |>
    dplyr::rename(BridgingRecommendation = .data[["Bridgeable"]],
                  OlinkID_concat = .data[["OlinkID"]]) |>
    dplyr::select(.data[["OlinkID_concat"]],
                  .data[["BridgingRecommendation"]])

  return(bridge_table)
}
