#' Function to find bridgeable assays from 3K to HT
#'
#' Imports an Explore 384 and Explore HT NPX file exported from Olink Software
#' No alterations to the output format is allowed.
#'
#' @param data_Explore384 A Explore384 NPX file
#' @param data_HT An Explore HT NPX file
#'
#' @return A "tibble" in long format. Columns include:
#' \itemize{
#'    \item{OlinkID_concat:} Concatenated Olink Explore HT and Explore 3072 IDs
#'    \item{Bridgeable:} Bridging Flag
#' }
#' Additional columns may be present or missing depending on the platform
#' @keywords NPX Bridging
#' @examples
#' \donttest{
#' results <- OlinkAnalyze:::olink_normalization_is_bridgeable(
#'   data_Explore384 = OlinkAnalyze:::data_3k,
#'   data_HT = OlinkAnalyze:::data_ht
#' )
#' }
#'
#' @author Amrita Kar
#' @author Marianne Sandin
#' @author Danai G. Topouza


olink_normalization_is_bridgeable <- function(data_Explore384,
                                              data_HT) {

  set.seed(123)

  # add a concatenated version of the 3K-HT OlinkIDs to the reference and non-
  # reference data frames. This is done because the olink_normalization
  # function requires unique, overlapping OlinkIDs. This approach allows us to
  # create unique IDs even with the repeated correlation assays in HT and 3K.

  map_oid_ht_3k <- function(explore_df) {

    oid_ht_3k_mapping <- OlinkAnalyze:::eHT_e3072_mapping |>
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

  bridge_table <- OlinkAnalyze:::eHT_e3072_mapping |>
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
