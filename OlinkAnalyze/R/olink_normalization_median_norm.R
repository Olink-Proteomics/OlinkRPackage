### Median normalization - base taken from olink_normalization_n, olink_normalization_bridge

#' Median normalization of all proteins between two NPX projects.
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
#' @return A "tibble" of NPX data in long format containing normalized NPX
#' values, including adjustment factors and name of project.
#'
#' @export
#'
#' @keywords Normalization; median normalization
#'
#' @importFrom dplyr bind_cols rename left_join mutate select case_when if_else

olink_normalization_median_ref <- function(exploreht_df,
                                           explore3072_df,
                                           bridge_samples,
                                           exploreht_name = 'reference',
                                           explore3072_name = 'new') {
  
  
  check_project_name <- olink_normalization_project_name_check(
    project_1_name = exploreht_name,
    project_2_name = explore3072_name,
    project_ref_name = exploreht_name)
  if (check_project_name != "TRUE") {
    stop(check_project_name)
  }
  rm(check_project_name)
  
  check_bridge_samples <- olink_normalization_sample_check(
    list_samples = bridge_samples,
    check_mode = "bridge",
    project_1_all_samples = { exploreht_df$SampleID |> unique() },
    project_2_all_samples = { explore3072_df$SampleID |> unique() })
  if (check_bridge_samples != "TRUE") {
    stop(check_bridge_samples)
  }
  rm(check_bridge_samples)
  
  # place bridge samples side by side in a data frame
  update_sampleid <- dplyr::bind_cols(bridge_samples) |>
    dplyr::rename("SampleID_df1" = "DF1",
                  "SampleID_df2" = "DF2")
  
  
  # change the SampleID of the non-reference data frame to match the bridging
  # samples from the reference data frame. This is done because the
  # OlinkAnalyze::olink_normalization function requires so.
  explore3072_df <- explore3072_df |>
    dplyr::left_join(update_sampleid, by = c('SampleID' = 'SampleID_df2')) |>
    dplyr::mutate(SampleID_df1 = dplyr::if_else(is.na(SampleID_df1),
                                                SampleID,
                                                SampleID_df1)) |>
    dplyr::select(-SampleID) |>
    dplyr::rename("SampleID" = "SampleID_df1")
  
  
  # Creating a concatenated OlinkID which contains HT and 3K OlinkID - necessary for unique OlinkIDs
  #### NEED TO GET A CUSTOMER-FACING MAPPING FUNCTION - FOR NOW USING INTERNAL SOURCES
  map_oid <- npxexplorer::oid_map |> # this is the olink_ids_match_3k_ht.csv file
    mutate(OlinkID_HT_3K = paste(olink_id_ht, olink_id_3k, sep = "_")) |>
    select(olink_id_ht, olink_id_3k, OlinkID_HT_3K)
  
  
  # add a concatenated version of the 3K-HT OlinkIDs to the reference and non-
  # reference data frames. This is done because the OlinkAnalyze::olink_normalization 
  # function requires unique, overlapping OlinkIDs. This approach allows us to
  # create unique IDs even with the repeated correlation assays in HT and 3K. 
  
   exploreht_df <- exploreht_df |>
    left_join(map_oid,
              relationship = "many-to-many",
              by = c("OlinkID" = "olink_id_ht")) |>
    mutate(OlinkID = OlinkID_HT_3K) |>
     select(-OlinkID_HT_3K, -olink_id_3k)
  
  explore3072_df <- explore3072_df |>
    left_join(map_oid, 
              relationship = "many-to-many",
              by = c("OlinkID" = "olink_id_3k")) |>
    mutate(OlinkID = OlinkID_HT_3K) |>
    select(-OlinkID_HT_3K, -olink_id_ht)
  
  
  
  # bridge normalize the two data frames
  norm_df <- olink_normalization(
    df1 = exploreht_df,
    df2 = explore3072_df,
    overlapping_samples_df1 = bridge_samples$DF1,
    overlapping_samples_df2 = NULL,
    df1_project_nr = exploreht_name,
    df2_project_nr = explore3072_name,
    reference_project = exploreht_name,
    reference_medians = NULL
  )
  
  # switch back to the original non-reference project's SampleID
  norm_df <- norm_df |>
    dplyr::left_join(update_sampleid, by = c('SampleID' = 'SampleID_df1')) |>
    dplyr::mutate(SampleID_df2 = dplyr::case_when(
      is.na(SampleID_df2) ~ SampleID,
      !is.na(SampleID_df2) & Project == exploreht_name ~ SampleID,
      !is.na(SampleID_df2) & Project != exploreht_name ~ SampleID_df2,
      TRUE ~ NA_character_)) |>
    dplyr::select(-SampleID) |>
    dplyr::rename("SampleID" = "SampleID_df2")
  rm(update_sampleid)
  
  # switch back both reference and non-reference projects to original
  # OlinkIDs, matched by platform
  norm_df <- norm_df |>
    dplyr::left_join(map_oid, by = c('OlinkID' = 'OlinkID_HT_3K')) |>
    dplyr::mutate(OlinkID = dplyr::case_when(
      Project == exploreht_name ~ olink_id_ht,
      Project != exploreht_name ~ olink_id_3k)) |>
      dplyr::select(-olink_id_ht, -olink_id_3k)
  rm(map_oid)
  
  return(df_adjusted_data)
  
}
