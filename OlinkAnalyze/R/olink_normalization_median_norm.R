#' Median normalization of all proteins between two NPX projects, where the
#' first is an Explore HT dataset and the second is an Explore 3072 datset.
#'
#' Normalizes two NPX projects (data frames) using shared samples.\cr\cr
#'
#' This function is a wrapper of olink_normalization.\cr\cr
#'
#' In bridging normalization one of the projects is adjusted to another using
#' shared samples (bridge samples). It is not necessary for the shared
#' samples to be named the same in each project. Adjustment between the two
#' projects is made using the median of the paired differences between the
#' shared samples. The two data frames are inputs project_1_df and project_2_df,
#' the one being adjusted to is specified in the input project_ref_name and the
#' shared samples are specified in bridge_samples.\cr\cr
#'
#' @param exploreht_df Data frame of the first project, must be Explore HT
#' data (required). Explore HT data will always be the reference data.
#' @param explore3072_df Data frame of the second project, must be Explore
#' 3072 data (required). Explore 3072 data will always be normalized to the
#' Explore HT data.
#' @param bridge_samples Named list of 2 arrays containing, SampleID of shared
#' samples to be used for the calculation of adjustment factor. The
#' names of the two arrays should be DF1 and DF2 corresponding to projects HT
#' and 3072, respectively. Arrays should be of equal length and index of each
#' entry should correspond to the same sample. (required)
#' @param exploreht_df Name of the Explore HT project (default: reference).
#' @param explore3072_df Name of the Explore 3072 project (default: new).
#'
#' @return A "tibble" of the median-adjusted NPX data in long format containing
#' median-normalized NPX values, including Explore HT OlinkIDs, Explore 3072
#' OlinkIDs, and name of project.
#'
#' @export
#'
#' @keywords Normalization; median normalization
#'
#' @importFrom dplyr bind_cols rename left_join mutate select case_when if_else

olink_normalization_median_norm <- function(exploreht_df,
                                            explore3072_df,
                                            bridge_samples,
                                            exploreht_name = "reference",
                                            explore3072_name = "new") {

  # Check that exploreht_df is HT and that explore3072_df is 3K.
  # If not, stop function.

  # detect if exploreht_df is HT data from panels
  df1_ht_check <- ifelse(all(exploreht_df |>
                               select(Panel) |>
                               distinct() |>
                               na.omit() |>
                               pull() %in% c("Explore HT", "Explore_HT")),
                         TRUE,
                         FALSE)

  # detect if explore3072_df is 3K data from panels
  df2_3k_check <- ifelse(all(explore3072_df |>
                               select(Panel) |>
                               distinct() |>
                               na.omit() |>
                               pull() %in% c(
                                 "Cardiometabolic", "Cardiometabolic_II",
                                 "Inflammation", "Inflammation_II",
                                 "Neurology", "Neurology_II",
                                 "Oncology", "Oncology_II")
  ),
  TRUE,
  FALSE)

  if (df1_ht_check != TRUE) {
    stop("exploreht_df must be an Explore HT dataset.")
  }

  if (df2_3k_check != TRUE) {
    stop("explore3072_df must be an Explore 3072 dataset.")
  }


  # place bridge samples side by side in a data frame
  update_sampleid <- dplyr::bind_cols(bridge_samples) |>
    dplyr::rename("SampleID_df1" = "DF1",
                  "SampleID_df2" = "DF2")

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

  # Creating a concatenated OlinkID which contains HT and 3K OlinkID -
  # necessary for unique OlinkIDs

  exploreht_df   <- map_oid_ht_3k(exploreht_df)   |> filter(!is.na(OlinkID))
  explore3072_df <- map_oid_ht_3k(explore3072_df) |> filter(!is.na(OlinkID))

  # Identifying overlapping assays between HT and 3K datasets
  overlapping_assays <- intersect(exploreht_df$OlinkID, explore3072_df$OlinkID)

  # bridge normalize the two data frames
  norm_df <- olink_normalization(
    df1 = exploreht_df |> filter(OlinkID %in% overlapping_assays),
    df2 = explore3072_df |> filter(OlinkID %in% overlapping_assays),
    overlapping_samples_df1 = bridge_samples$DF1,
    overlapping_samples_df2 = NULL,
    df1_project_nr = exploreht_name,
    df2_project_nr = explore3072_name,
    reference_project = exploreht_name,
    reference_medians = NULL
  )

  # switch back to the original non-reference project's SampleID
  norm_df <- norm_df |>
    dplyr::left_join(update_sampleid, by = c("SampleID" = "SampleID_df1")) |>
    dplyr::mutate(SampleID_df2 = dplyr::case_when(
      is.na(SampleID_df2) ~ SampleID,
      !is.na(SampleID_df2) & Project == exploreht_name ~ SampleID,
      !is.na(SampleID_df2) & Project != exploreht_name ~ SampleID_df2,
      TRUE ~ NA_character_)
    ) |>
    dplyr::select(-SampleID) |>
    dplyr::rename("SampleID" = "SampleID_df2")

  rm(update_sampleid)

  # switch back both reference and non-reference projects to original
  # OlinkIDs, with both HT and 3K OlinkIDs
  norm_df <- norm_df |>
    mutate(OlinkIDconcat = OlinkID) |>
    mutate(OlinkID = if_else(Panel == "Explore HT",
                             substr(OlinkIDconcat, 1, 8),
                             substr(OlinkIDconcat, 10, 18))) |>
    mutate(MedianCenteredNPX = NPX + Adj_factor) |>
    select(-Adj_factor)

  return(norm_df)
}
