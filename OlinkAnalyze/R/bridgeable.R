#' Function to find bridgeable assays from 3K to HT
#'
#' Imports an Explore 384 and a Explore HT NPX file exported from Olink Software.
#' No alterations to the output format is allowed.
#'
#' @param data_Explore384 A Explore384 NPX file
#' @param data_HT An Explore HT NPX file
#'
#' @return A "tibble" in long format. Columns include:
#' \itemize{
#'    \item{OlinkIDconcat:} Concatenated Olink Explore HT and Explore 3072 IDs
#'    \item{OlinkID_HT:} Olink ID HT
#'    \item{OlinkID_E3072:} Olink ID Explore 3072
#'    \item{Gene:} Assay HT
#'    \item{Bridgeable:} Bridging Flag
#' }
#' Additional columns may be present or missing depending on the platform
#' @keywords NPX Bridging 
#' @examples
#' \donttest{
#' #test_file <- '../tests/data/bridgeable_data.rda'
#' #load(test_file)
#' #results <- bridgeable(data_Explore384 = data_3k, data_HT = data_ht)
#' }
#' 
#' @importFrom dplyr n filter group_by summarise ungroup pull n_distinct do select arrange mutate n
#' @importFrom stats cor ks.test quantile
#' @importFrom rlang .data .env
#' 
#' @author Amrita Kar
#' @author Marianne Sandin
#' @author Danai G. Topouza


bridgeable <- function(data_Explore384, 
                       data_HT){
  
  # add a concatenated version of the 3K-HT OlinkIDs to the reference and non-
  # reference data frames. This is done because the OlinkAnalyze::olink_normalization 
  # function requires unique, overlapping OlinkIDs. This approach allows us to
  # create unique IDs even with the repeated correlation assays in HT and 3K.
  
  map_oid_ht_3k <- function(explore_df) {
    
    oid_ht_3k_mapping <- eHT_e3072_mapping
    
    oid_ht_3k_mapping <- oid_ht_3k_mapping |>
      mutate(OlinkID_HT_3K = paste(.data[["OlinkID_HT"]],.data[["OlinkID_E3072"]], sep = "_")) |>
      select(.data[["OlinkID_HT"]], .data[["OlinkID_E3072"]],.data[["OlinkID_HT_3K"]])
    
    if (all(explore_df |> distinct(Panel) |> pull() |> na.omit() %in% c("Explore_HT", "Explore HT"))) {
      
      explore_df_linkedOID <- explore_df |>
        dplyr::filter(stringr::str_detect(string = .data[["SampleType"]], pattern = "SAMPLE"),
                      stringr::str_detect(string = .data[["AssayType"]], pattern = "assay")) |>
        inner_join(oid_ht_3k_mapping,
                   relationship = "many-to-many",
                   by = c("OlinkID" = "OlinkID_HT")) |>
        mutate(OlinkID = .data[["OlinkID_HT_3K"]]) |>
        select(-.data[["OlinkID_HT_3K"]], -.data[["OlinkID_E3072"]]) |>
        distinct()
      
    } else if (all(explore_df |> distinct(Panel) |> pull() |> na.omit() %in% c("Cardiometabolic", "Cardiometabolic_II",
                                                                               "Inflammation", "Inflammation_II",
                                                                               "Neurology", "Neurology_II",
                                                                               "Oncology", "Oncology_II"))) {
      explore_df_linkedOID <- explore_df |>
        dplyr::filter(stringr::str_detect(string = .data[["SampleType"]], pattern = "SAMPLE"),
                      stringr::str_detect(string = .data[["AssayType"]], pattern = "assay")) |>
        inner_join(oid_ht_3k_mapping,
                   relationship = "many-to-many",
                   by = c("OlinkID" = "OlinkID_E3072")) |>
        mutate(OlinkID = .data[["OlinkID_HT_3K"]]) |>
        select(-.data[["OlinkID_HT_3K"]], -.data[["OlinkID_HT"]]) |>
        distinct()
      
    } else {
      
      explore_df_linkedOID <- explore_df
      print("The provided data frame is not an Explore HT or Explore 384 data frame. No changes made.")
      
    }
    
    return(explore_df_linkedOID)
  }
  
  outlier_removal_iqr <- function(data = data, iqr_value){
    
    data_out <- data |>
      dplyr::filter(.data[["NPX_3k"]] < median(.data[["NPX_3k"]]) + .env[["iqr_value"]]*IQR(.data[["NPX_3k"]])) |>
      dplyr::filter(.data[["NPX_3k"]]> median(.data[["NPX_3k"]]) - .env[["iqr_value"]]*IQR(.data[["NPX_3k"]])) |>
      dplyr::filter(.data[["NPX_ht"]] < median(.data[["NPX_ht"]]) + .env[["iqr_value"]]*IQR(.data[["NPX_ht"]])) |>
      dplyr::filter(.data[["NPX_ht"]] > median(.data[["NPX_ht"]]) - .env[["iqr_value"]]*IQR(.data[["NPX_ht"]]))
    
    return(data_out)
  }
  
  # Replaced group_modify with reframe()
  r2_results <- function(data = data){
    
    message(paste0("Finding assay correlation in 3K and HT"))
    
    data_out <- data |>
      dplyr::group_by(.data[["Gene"]]) |>
      #dplyr::group_modify(~outlier_removal_iqr(.x, iqr_value = 3)) |>
      #purrr::modify(~., outlier_removal_iqr(~., iqr_value = 3)) |>
      dplyr::reframe(outlier_removal_iqr(dplyr::pick(everything()), iqr_value = 3)) |>
      dplyr::group_by(.data[["Gene"]]) |>
      dplyr::mutate(R2_lm = stats::cor(x = .data[["NPX_3k"]][Count > 10], y = .data[["NPX_ht"]][Count > 10])^2) |>
      dplyr::ungroup() |>
      dplyr::distinct()
    
    return(data_out)
  }
  
  
  ks_results <- function(data = data){
    
    message(paste0("Finding normalization method for bridgeable assays"))
    
    data_out <- data |>
      dplyr::group_by(.data[["Gene"]], .data[["ids"]]) |>
      # dplyr::group_modify(~outlier_removal_iqr(.x, iqr_value = 3)) |>
      #purrr::modify(~., outlier_removal_iqr(~., iqr_value = 3)) |>
      dplyr::reframe(outlier_removal_iqr(dplyr::pick(everything()), iqr_value = 3)) |>
      dplyr::group_by(.data[["Gene"]], .data[["ids"]]) |>
      summarise(ks = ks.test(.data[["NPX_ht"]][Count > 10], .data[["NPX_3k"]][Count > 10])$statistic,
                .groups = "keep") |>
      dplyr::ungroup() |>
      dplyr::distinct()
    
    return(data_out)
  }
  

  e3k_eHT_mapping <- eHT_e3072_mapping
    
  data_Explore384 <- data_Explore384 |>
    dplyr::filter(stringr::str_detect(string = .data[["SampleType"]], pattern = "SAMPLE"),
                  stringr::str_detect(string = .data[["AssayType"]], pattern = "assay")) |>
    map_oid_ht_3k() |>
    dplyr::mutate(OlinkID_Explore384 = substr(.data[["OlinkID"]], 10, 18)) |>
    dplyr::rename(Gene = .data[["Assay"]], 
                  NPX_3k = .data[["NPX"]], 
                  ids = .data[["OlinkID"]]) |>
    dplyr::select(.data[["SampleID"]], .data[["ids"]], .data[["OlinkID_Explore384"]], .data[["NPX_3k"]]) |> 
    distinct()
  
  # Count refers to HT counts  
  data_HT <- data_HT |>
    dplyr::filter(stringr::str_detect(string = .data[["SampleType"]], pattern = "SAMPLE"),
                  stringr::str_detect(string = .data[["AssayType"]], pattern = "assay")) |>
    map_oid_ht_3k() |>
    dplyr::rename(Gene = .data[["Assay"]], 
                  NPX_ht = .data[["NPX"]], 
                  ids = .data[["OlinkID"]]) |>
    dplyr::mutate(OlinkID = substr(.data[["ids"]], 1, 8)) |>
    dplyr::select(.data[["SampleID"]],.data[["Gene"]], .data[["OlinkID"]], .data[["ids"]], .data[["UniProt"]], .data[["NPX_ht"]], .data[["Count"]]) |>
    distinct()
  
  data_mapped <- data_Explore384 |>
    dplyr::inner_join(data_HT, by = c("SampleID", "ids"), relationship = "many-to-many") |>
    na.omit() |>
    dplyr::group_by(.data[["ids"]]) |>
    dplyr::mutate(range_ht = quantile(.data[["NPX_ht"]][Count > 10], probs = 0.9) - quantile(.data[["NPX_ht"]][Count > 10], probs = 0.1), # using 10% to 90% quantiles for checking range /IQR
                  range_3k = quantile(.data[["NPX_3k"]][Count > 10], probs = 0.9) - quantile(.data[["NPX_3k"]][Count > 10], probs = 0.1),
                  range_diff = abs(.data[["range_ht"]] - .data[["range_3k"]]),
                  iqr_ht = IQR(.data[["NPX_ht"]]), iqr_3k = IQR(.data[["NPX_3k"]]),
                  median_count = median(.data[["Count"]][Count > 10], na.rm = T), # if count less than 10 in 1 sample that assay is dropped
                  low_counts_flag = .data[["median_count"]] < 150)  |> 
    dplyr::ungroup() |>
    dplyr::distinct()
  
  # selecting only required columns 
  data_condensed <- data_mapped |>
    dplyr::select(.data[["SampleID"]], .data[["Gene"]], .data[["UniProt"]], .data[["NPX_3k"]], .data[["NPX_ht"]], .data[["Count"]], .data[["range_diff"]], .data[["low_counts_flag"]], .data[["ids"]]) |>
    dplyr::ungroup() |>
    dplyr::distinct() |>
    r2_results()
  
  not_bridgeable <- data_condensed |>
    dplyr::filter(
      .data[["range_diff"]] > 1,
      .data[["low_counts_flag"]] == TRUE,
             .data[["R2_lm"]] < 0.8) |>
    dplyr::pull(.data[["ids"]]) |>
    unique() |>
    as.data.frame() |>
    dplyr::rename(ids = 1) |>
    tidyr::extract(.data[["ids"]], into = c("OlinkID", "OlinkID_Explore384"), "(.*)_([^_]+)$", remove = TRUE)
  
  # Adding type of normalization
  ks <- ks_results(data = data_mapped) |>
    tidyr::extract(.data[["ids"]], into = c("OlinkID", "OlinkID_Explore384"), "(.*)_([^_]+)$", remove = TRUE)
  
  bridge_table <- e3k_eHT_mapping |>
    dplyr::mutate(Bridgeable = dplyr::if_else(.data[["OlinkID_HT"]] %in% not_bridgeable$OlinkID, "Not Bridgeable", "Yes")) |>
    dplyr::select(.data[["OlinkID_HT"]], .data[["OlinkID_E3072"]], .data[["Bridgeable"]]) |> 
    dplyr::distinct() |>
    dplyr::inner_join(ks, by = c("OlinkID_HT" = "OlinkID", "OlinkID_E3072" = "OlinkID_Explore384"))
  
  # Adding concatenated ID
  concat_ids <- data_mapped |>
    dplyr::select(.data[["ids"]], .data[["OlinkID_Explore384"]], .data[["OlinkID"]]) |>
    dplyr::distinct()
  
  message(paste0("Generating bridging recommendation table"))
  bridge_table <- bridge_table |>
    dplyr::mutate(Bridgeable = case_when(.data[["Bridgeable"]] == "Yes" & ks < 0.2 ~ "Median Centered", 
                                                    .data[["Bridgeable"]] == "Yes" & ks > 0.2 ~ "Quantile Smoothing", 
                                                    .default = as.character(.data[["Bridgeable"]]))) |>
    dplyr::left_join(concat_ids, by = c("OlinkID_HT" = "OlinkID", "OlinkID_E3072" = "OlinkID_Explore384"))|>
    dplyr::rename(BridgingRecommendation = .data[["Bridgeable"]],
                  OlinkIDconcat = .data[["ids"]],
                  Assay = .data[["Gene"]]) |>
    dplyr::select(.data[["OlinkIDconcat"]], .data[["OlinkID_HT"]], .data[["OlinkID_E3072"]], .data[["Assay"]], .data[["BridgingRecommendation"]])
  
  return(bridge_table)
}
