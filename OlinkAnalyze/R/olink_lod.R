#' Calculate LOD using Negative Controls or Fixed LOD
#'
#' @param data npx data file
#' @param lod_file_path location of lod file from Olink (only needed if lod_method = "FixedLOD")
#' @param lod_method method for calculating LOD using either "FixedLOD" or negative controls ("NCLOD")
#'
#' @return A dataframe with 2 additional columns, LOD and PCNormalizedLOD. When Normalization = "Plate Control", LOD and PCNormalizedLOD are equivalent
#' @export
#'
#' @examples
#'
#' \dontrun{
#'   \donttest{
#'     # Estimate LOD from negative controls
#'     npx_data_lod_NC <– olink_lod(data = npx_data,  lod_method = "NCLOD")
#'
#'     # Estimate LOD from fixed LOD
#'     npx_data_lod_Fixed <– olink_lod(data = npx_data,
#'                                     lod_file_path = "path/to/lod_file",
#'                                     lod_method = "FixedLOD")
#'   }
#' }
#'

olink_lod <- function(data, lod_file_path = NA, lod_method = "NCLOD"){
  original_columns <- names(data)


  if (lod_method == "FixedLOD"){
    if(missing(lod_file_path)){
      stop("lod_file_path must be specified for lod_method \"FixedLOD\"")
    }
    lod_file <- read.table(lod_file_path, sep = ";", header = TRUE)
    lod_data <- olink_fixed_lod(data, lod_file)
  }
  if (lod_method == "NCLOD"){
    lod_data <- olink_nc_lod(data)
  }

  if(!(lod_method %in% c("FixedLOD", "NCLOD"))){
    stop("lod_method must be one of \"FixedLOD\" or \"NCLOD\"")
  }

  # If NPX is intensity normalized, than intensity normalize LOD
  data<-int_norm_count(data, lod_data) |>
    mutate(LOD = ifelse(.data[["Normalization"]] ==  "Intensity",
                        .data[["LOD"]],
                        ifelse(.data[["Normalization"]] == "Plate control",
                               .data[["PCNormalizedLOD"]],
                               NA)))

  data<-data |>
    dplyr::select(c(original_columns, "LOD", "PCNormalizedLOD"))
  return(data)
}

olink_fixed_lod <- function(data, lod_file){
  lod_data <- lod_file |>
    dplyr::filter(.data[["DataAnalysisRefID"]] %in% data$DataAnalysisRefID) |>
    dplyr::select(c("OlinkID","DataAnalysisRefID", "LodNPX", "LodCount", "LodMethod"))

}


olink_nc_lod <- function(data) {
  # Calculate LOD in counts and NPX

  lod_data<-data |>
    dplyr::filter(
      .data[["SampleType"]] == "NEGATIVE_CONTROL",
      .data[["AssayQC"]] == "PASS",
      .data[["SampleQC"]] == "PASS",
      .data[["AssayType"]] == "assay",
      !is.na(.data[["NPX"]])
    )

  if(length(unique(lod_data$SampleID)) < 10){
    stop("Insufficient Negative Control data to calculate LOD.
         At least 10 Negative Controls are required to calculate LOD from Negative Controls.")
  }

  lod_data <- lod_data |>
    dplyr::group_by(dplyr::pick(dplyr::all_of(c("OlinkID", "DataAnalysisRefID")))) |>
    dplyr::summarise(MaxCount = max(.data[["Count"]]),
                     LodNPX = median(.data[["PCNormalizedNPX"]], na.rm = TRUE) +
                       max(
                         0.2, 3 * sd(.data[["PCNormalizedNPX"]], na.rm = TRUE)
                       ),
                     LodCount = max(
                       150, max(.data[["Count"]] * 2)
                     )) |>
    dplyr::mutate(LodMethod = dplyr::case_when(
      .data[["MaxCount"]] > 150 ~ "lod_npx",
      TRUE ~ "lod_count"
    )) |>
    dplyr::ungroup() |>
    dplyr::select(-dplyr::all_of(c("MaxCount")))

  return(lod_data)


}




pc_norm_count <- function(data, lod_data){
  # Calculate PC median for normalization

  pc_median <- data |>
  dplyr::group_by(dplyr::pick(dplyr::all_of("OlinkID"))) |>
  dplyr::summarise(PCMedian =
                  median(.data[["ExtNPX"]][.data[["SampleType"]] == "PLATE_CONTROL"], na.rm = TRUE)) |>
    dplyr::select("OlinkID", "PCMedian")
  if(nrow(pc_median) == 0){
  stop("Insufficient Plate Control data for normalization of LOD.")
  }
  # Calculate ExtCount for normalization
  ext_count <- data |>
    dplyr::filter(.data[["AssayType"]] == "ext_ctrl") |>
    dplyr::mutate(ExtCount = .data[["Count"]]) |>
    dplyr::select("SampleID", "Panel", "Block", "SampleType", "ExtCount")

  data <- data |>
    dplyr::left_join(lod_data, by = c("OlinkID", "DataAnalysisRefID")) |>
    dplyr::left_join(pc_median, by = c("OlinkID")) |>
    dplyr::left_join(ext_count, by = c("SampleID", "Panel", "Block", "SampleType"))
    # Convert count LOD to PC norm NPX

  data <- data |>
    dplyr::mutate(PCNormalizedLOD = ifelse(!is.na(.data[["NPX"]]),
                             ifelse(.data[["LodMethod"]] == "lod_npx",
                                    .data[["LodNPX"]],
                                    log2(.data[["LodCount"]] / .data[["ExtCount"]]) - .data[["PCMedian"]]
                              ), NA))
  return(data)
}



int_norm_count <- function(data, lod_data){
  data <- pc_norm_count(data, lod_data)
  plate_median <- data |>
    dplyr::filter(.data[["SampleType"]] =="SAMPLE") |>
    dplyr::group_by(dplyr::pick(dplyr::all_of("OlinkID"))) |>
    dplyr::summarise(PlateMedianNPX = median(.data[["PCNormalizedNPX"]])) |>
    dplyr::select("OlinkID", "PlateMedianNPX")

  if(nrow(plate_median) == 0){
    stop("Insufficient data for intensity normalization of LOD.")
  }

  data <- data |>
    dplyr::left_join(plate_median, by = "OlinkID") |>
    dplyr::mutate(LOD = .data[["PCNormalizedLOD"]] - .data[["PlateMedianNPX"]])
  return(data)

}





