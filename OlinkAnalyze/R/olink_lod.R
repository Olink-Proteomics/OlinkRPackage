#' Calculate LOD using Negative Controls or Fixed LOD
#'
#' @param data npx data file
#' @param lod_file_path location of lod file from Olink. Only needed if
#' lod_method = "FixedLOD". Default `NULL`.
#' @param lod_method method for calculating LOD using either "FixedLOD" or
#' negative controls ("NCLOD"). Default `NCLOD`.
#'
#' @return A dataframe with 2 additional columns, LOD and PCNormalizedLOD. When
#' `Normalization = "Plate Control"`, LOD and PCNormalizedLOD are identical.
#' 
#' @export
#' @examples
#' \dontrun{
#'    \donttest{
#'   try({ # This will fail if the files do not exist.
#'
#'   # Import NPX data
#'     npx_data <- read_NPX("path/to/npx_file")
#'
#'   # Estimate LOD from negative controls
#'     npx_data_lod_NC <- olink_lod(data = npx_data,  lod_method = "NCLOD")
#'
#'   # Estimate LOD from fixed LOD
#'   ## Locate the fixed LOD file
#'     lod_file_path <- "path/to/lod_file"
#'
#'     npx_data_lod_Fixed <- olink_lod(data = npx_data,
#'                                     lod_file_path = lod_file_path,
#'                                     lod_method = "FixedLOD")
#'      })
#'   }
#' }
#'
olink_lod <- function(data, lod_file_path = NULL, lod_method = "NCLOD"){

  # store original column names of `data` to restore them later
  original_columns <- names(data)

  # check the type of LOD calculation to perform and compute or extract:
  # LODNPX, LODCount and LODMethod

  lod_methods <- list(fix_lod = "FixedLOD",
                      nc_lod = "NCLOD")

  if (lod_method == lod_methods$fix_lod) {

    if(missing(lod_file_path)) {
      stop(paste0("lod_file_path must be specified for lod_method = \"",
                  lod_methods$fix_lod, "\""))
    }

    lod_file <- read.table(file = lod_file_path, sep = ";", header = TRUE)
    lod_data <- olink_fixed_lod(data_analysis_ref_id = data$DataAnalysisRefID,
                                lod_file = lod_file)

  } else if (lod_method == lod_methods$nc_lod) {

    lod_data <- olink_nc_lod(data = data)

  } else {

    stop(paste0("lod_method must be one of \"", lod_methods$fix_lod, "\" or \"",
                lod_methods$nc_lod, "\""))

  }

  # If NPX is intensity normalized, then intensity normalize LOD
  data <- int_norm_count(
    data = data,
    lod_data = lod_data
  ) |>
    dplyr::mutate(
      LOD = dplyr::if_else(
        .data[["Normalization"]] == "Intensity",
        .data[["LOD"]],
        dplyr::if_else(
          .data[["Normalization"]] == "Plate control",
          .data[["PCNormalizedLOD"]],
          NA_real_
        )
      )
    ) |>
    dplyr::select(
      dplyr::all_of(
        c(original_columns, "LOD", "PCNormalizedLOD")
      )
    )

  return(data)
}

# extract LODNPX, LODCount and LODMethods from reference file
olink_fixed_lod <- function(data_analysis_ref_id, lod_file) {

  lod_file |>
    dplyr::filter(
      .data[["DataAnalysisRefID"]] %in% data_analysis_ref_id
    ) |>
    dplyr::select(
      dplyr::all_of(
        c("OlinkID", "DataAnalysisRefID", "LODNPX", "LODCount", "LODMethod")
      )
    )

}

# compute LODNPX, LODCount and LODMethods from NCs
olink_nc_lod <- function(data, min_num_nc = 10L) {
  # Calculate LOD in counts and NPX

  # rows from NC
  lod_data <- data |>
    dplyr::filter(
      .data[["SampleType"]] == "NEGATIVE_CONTROL"
      & .data[["SampleQC"]] == "PASS"
      & .data[["AssayType"]] == "assay"
      & !is.na(.data[["NPX"]])
    )

  # check that we have at least `min_num_nc` NCs
  if(length(unique(lod_data$SampleID)) < min_num_nc){
    stop(paste("At least", min_num_nc, "Negative Controls are required to",
               "calculate LOD from Negative Controls."))
  }

  # compute LOD
  lod_data <- lod_data |>
    # LOD is comnputed per assay and lot of reagents
    dplyr::group_by(
      OlinkID, DataAnalysisRefID
    ) |>
    # compute LOD on counts and NPX
    dplyr::summarise(
      MaxCount = max(.data[["Count"]], na.rm = TRUE),
      LODNPX = median(.data[["PCNormalizedNPX"]], na.rm = TRUE) +
        max(0.2, 3L * sd(.data[["PCNormalizedNPX"]], na.rm = TRUE)),
      LODCount = max(150L, max(.data[["Count"]] * 2L, na.rm = TRUE))
    ) |>
    dplyr::mutate(
      LODMethod = dplyr::if_else(
        .data[["MaxCount"]] > 150L, "lod_npx", "lod_count"
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::select(
      -dplyr::all_of(
        c("MaxCount")
      )
    )

  return(lod_data)

}

pc_norm_count <- function(data, lod_data){
  # Calculate PC median for normalization

  pc_median <- data |>
    dplyr::group_by(
      OlinkID, PlateID
    ) |>
    dplyr::summarise(
      PCMedian = median(.data[["ExtNPX"]][.data[["SampleType"]] == "PLATE_CONTROL"],
                        na.rm = TRUE)
    ) |>
    dplyr::select(
      dplyr::all_of(
        c("OlinkID", "PlateID", "PCMedian")
      )
    )

  if(nrow(pc_median) == 0L){
    stop("Insufficient Plate Control data for normalization of LOD.")
  }

  # Calculate ExtCount for normalization
  ext_count <- data |>
    dplyr::filter(
      .data[["AssayType"]] == "ext_ctrl"
    ) |>
    dplyr::select(
      dplyr::all_of(
        c("SampleID", "WellID", "Panel", "Block", "SampleType", "PlateID",
          "ExtCount" = "Count")
      )
    )

  data <- data |>
    dplyr::left_join(
      lod_data,
      by = c("OlinkID", "DataAnalysisRefID")
    ) |>
    dplyr::left_join(
      pc_median,
      by = c("OlinkID", "PlateID")
    ) |>
    dplyr::left_join(
      ext_count,
      by = c("SampleID", "WellID", "Panel", "Block", "SampleType", "PlateID")
    )

  # Convert count LOD to PC norm NPX

  data <- data |>
    dplyr::mutate(
      PCNormalizedLOD = dplyr::case_when(
        is.na(.data[["NPX"]]) ~ NA_real_,
        !is.na(.data[["NPX"]]) & .data[["LODMethod"]] == "lod_npx" ~
          .data[["LODNPX"]],
        TRUE ~ log2(.data[["LODCount"]] / .data[["ExtCount"]]) - .data[["PCMedian"]],
        .default = NA_real_
      )
    ) |> 
    dplyr::mutate(LOD = PCNormalizedLOD)

  return(data)
}

int_norm_count <- function(data, lod_data){

  data <- pc_norm_count(data, lod_data)

  if(any(data[["Normalization"]]=="Intensity")){
    plate_median <- data |>
      dplyr::filter(
        .data[["SampleType"]] =="SAMPLE"
    ) |>
    dplyr::group_by(
      OlinkID, PlateID
    ) |>
    dplyr::summarise(
      PlateMedianNPX = median(.data[["PCNormalizedNPX"]], na.rm = TRUE)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(
      dplyr::all_of(
        c("OlinkID", "PlateID", "PlateMedianNPX"))
    )

  if(nrow(plate_median) == 0L){
    stop("Insufficient data for intensity normalization of LOD.")
  }

  data <- data |>
    dplyr::left_join(
      plate_median,
      by = c("OlinkID", "PlateID")
    ) |>
    dplyr::mutate(
      LOD = .data[["PCNormalizedLOD"]] - .data[["PlateMedianNPX"]]
    )
  }

return(data)

}
