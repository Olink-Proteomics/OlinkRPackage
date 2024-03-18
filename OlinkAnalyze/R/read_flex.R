#' Read in flex data
#'
#' Called by read_NPX
#'
#' @param filename where the file is located
#'
#' @return tibble of data
read_flex <- function(filename) {

  data_type <- readxl::read_excel(path = filename,
                                  range = "A2",
                                  col_names = FALSE,
                                  .name_repair = "minimal")
  
  is_npx_data <- ifelse(grepl(pattern = "NPX", x = data_type, fixed = TRUE), TRUE, FALSE) 
  
  # Extract Panel Names
  panel_name <-  unlist(as.vector(readxl::read_excel(path = filename,
                                    skip = 2,
                                    n_max = 1,
                                    col_names = FALSE,
                                    .name_repair = "minimal") ))
  panel_number <- data.frame(Panel = panel_name) |> 
    dplyr::filter(Panel != "Panel" ) |> 
    unique() |> 
    nrow()
  
  n_max_meta_data <- 4
  
  assay_data <- readxl::read_excel(path = filename,
                                   skip = 2,
                                   n_max = n_max_meta_data,
                                   col_names = F,
                                   .name_repair = "minimal") 
  # Pivot meta data
  metadata <- data.frame(Panel = unlist(as.vector(assay_data[1,])),
                         Assay = unlist(as.vector(assay_data[2,])),
                         UniProt = unlist(as.vector(assay_data[3,])),
                         OlinkID = unlist(as.vector(assay_data[4,]))) |> 
    dplyr::slice(-1) |> 
    dplyr::mutate(OlinkID = ifelse(stringr::str_detect(Assay, "Ctrl"), paste0("OID_",Assay), OlinkID)) |> 
    dplyr::filter(!is.na(OlinkID))
  
  
  # Is QC Deviation data present 
  qc_dev_cols <- readxl::read_excel(path = filename, 
                     skip = 2+1,
                     col_names = FALSE,
                     .name_repair="minimal",
                     n_max = 2) |>
    as.matrix() |> 
    t() |> 
    as.data.frame() |> 
    dplyr::mutate(cols = paste(V1,V2)) |> 
    dplyr::filter(stringr::str_detect(cols, "QC Deviation")) |> 
    dplyr::select(cols) |> 
    as.vector() |> 
    unlist() |> 
    unique()
  
    
  
  # Load NPX or QUANT data including the last rows of meta data
  dat <- readxl::read_excel(path = filename,
                            skip = n_max_meta_data + 2 + 1,
                            col_names = FALSE,
                            .name_repair="minimal",
                            col_types = NULL)
  
  data_cols <- c("SampleID", metadata$OlinkID, paste0(rep("PlateID", panel_number), 1:panel_number), paste0(rep("QC_Warning", panel_number), 1:panel_number))
  if( length(qc_dev_cols) != 0){
    names(dat) <- c(data_cols, paste0(rep(qc_dev_cols), 1:panel_number))
  } else{
    names(dat) <- data_cols
  }
  
  # Calculate number of plates
 
  nr_plates <- dat |> 
    dplyr::select(PlateID1) |> 
    dplyr::filter(!is.na(PlateID1)) |> 
    unique() |> 
    nrow()
  
  # Extract ending metadata

  missfreq <- dat |> dplyr::filter(stringr::str_detect(SampleID, "Missing Data freq.")) |> 
    tidyr::pivot_longer(cols = tidyselect::starts_with("OID"),
                        names_to = "OlinkID",
                        values_to = "MissingFreq") |> 
    dplyr::select(-SampleID, -tidyselect::starts_with("PlateID"), -tidyselect::starts_with("QC"))
  norm_method <- dat |> dplyr::filter(stringr::str_detect(SampleID, "Normalization")) |> 
    tidyr::pivot_longer(cols = tidyselect::starts_with("OID"),
                        names_to = "OlinkID",
                        values_to = "Normalization") |> 
    dplyr::select(-SampleID, -tidyselect::starts_with("PlateID"), -tidyselect::starts_with("QC"))
  
  if (!is_npx_data) {
    assay_warning <- dat |> dplyr::filter(stringr::str_detect(SampleID, "Assay warning")) |> 
      tidyr::pivot_longer(cols = tidyselect::starts_with("OID"),
                          names_to = "OlinkID",
                          values_to = "Assay_Warning") |> 
      dplyr::select(-SampleID, -tidyselect::starts_with("QC"))
    
    # # of rows = # of plates
    Plate_LQL <- dat |> dplyr::filter(stringr::str_detect(SampleID, "Lowest quantifiable level")) |> 
      tidyr::pivot_longer(cols = tidyselect::starts_with("OID"),
                          names_to = "OlinkID",
                          values_to = "Plate_LQL") |> 
      dplyr::select(-tidyselect::starts_with("QC")) |> 
      dplyr::select(-SampleID)
    
    # # of rows = # of plates
    LOD <- dat |> dplyr::filter(stringr::str_detect(SampleID, "Plate LOD")) |> 
      tidyr::pivot_longer(cols = tidyselect::starts_with("OID"),
                          names_to = "OlinkID",
                          values_to = "Plat_LOD") |> 
      dplyr::select(-tidyselect::starts_with("QC")) |> 
      dplyr::select(-SampleID)
    
    LLOQ <- dat |> dplyr::filter(stringr::str_detect(SampleID, "LLOQ"))  |> 
      tidyr::pivot_longer(cols = tidyselect::starts_with("OID"),
                          names_to = "OlinkID",
                          values_to = "LLOQ") |> 
      dplyr::select(-SampleID, -tidyselect::starts_with("PlateID"), -tidyselect::starts_with("QC"))
    
    ULOQ <- dat |> dplyr::filter(stringr::str_detect(SampleID, "ULOQ"))  |> 
      tidyr::pivot_longer(cols = tidyselect::starts_with("OID"),
                          names_to = "OlinkID",
                          values_to = "ULOQ") |> 
      dplyr::select(-SampleID, -tidyselect::starts_with("PlateID"), -tidyselect::starts_with("QC"))
    
    meta_data_per_assay <- dplyr::left_join(missfreq,norm_method, by = "OlinkID") |> 
      dplyr::left_join(assay_warning, by = "OlinkID", multiple ="all") |>
      dplyr::left_join(LLOQ, by = c("OlinkID")) |> 
      dplyr::left_join(ULOQ, by = c("OlinkID")) |> 
      dplyr::left_join(Plate_LQL, by = c("OlinkID", "PlateID1")) |> 
      dplyr::left_join(LOD, by = c("OlinkID", "PlateID1")) 
  } else {
    LOD <- dat |> dplyr::filter(stringr::str_detect(SampleID, "LOD")) |> 
      dplyr::select(-SampleID, -tidyselect::starts_with("PlateID"), -tidyselect::starts_with("QC")) |> 
      tidyr::pivot_longer(cols = tidyselect::starts_with("OID"),
                          names_to = "OlinkID",
                          values_to = "LOD") 
    meta_data_per_assay <- dplyr::left_join(missfreq,norm_method, by = c("OlinkID")) |> 
      dplyr::left_join(LOD, by = c("OlinkID")) |> 
      dplyr::select(-tidyselect::starts_with("PlateID"), -tidyselect::starts_with("QC"))
  }
  
  
  # pivot longer
  dat<- dat |> 
    dplyr::filter(!stringr::str_detect(SampleID, paste("Missing Data freq.",
                                               "Normalization",
                                               "Assay warning",
                                               "Lowest quantifiable level",
                                               "Plate LOD",
                                               "LLOQ",
                                               "ULOQ",
                                               "LOD", 
                                               sep = "|"))) |>  # remove end meta data
    tidyr::pivot_longer(cols = tidyselect::starts_with("OID"),
                        names_to = "OlinkID",
                        values_to = ifelse(is_npx_data, "NPX", "Quantified_value")) |> 
    dplyr::left_join(metadata, by = c("OlinkID")) |> 
    dplyr::left_join(meta_data_per_assay, by = c("OlinkID", "PlateID1"), multiple = "all") |> 
    dplyr::mutate(PlateID = PlateID1) |> 
    dplyr::mutate(QC_Warning = QC_Warning1) |> 
    dplyr::select(SampleID, OlinkID,
                  UniProt, Assay, MissingFreq,
                  Panel,PlateID,
                  QC_Warning,
                  dplyr::any_of(dplyr::matches(c("Plate_LQL",
                                          "LOD",
                                          "Plat_LOD",
                                          "LLOQ",
                                          "ULOQ",
                                          "NPX",
                                          "Quantified_value",
                                          "Unit",
                                          "Assay_Warning",
                                          "Normalization",
                                          "*Inc Ctrl*",
                                          "*Det Ctrl*")))) |> 
    dplyr::mutate(suppressWarnings(dplyr::across(dplyr::any_of(dplyr::matches(c("MissingFreq",
                                  "Plate_LQL",
                                  "LOD",
                                  "Plat_LOD",
                                  "LLOQ",
                                  "ULOQ",
                                  "NPX",
                                  "Quantified_value",
                                  "*Inc Ctrl*",
                                  "*Det Ctrl*"))), as.numeric))) |> 
    dplyr::mutate(OlinkID = ifelse(stringr::str_detect(OlinkID,"Ctrl"), NA, OlinkID))
          
  message(paste(panel_number, "Flex panel(s) were detected with", nr_plates, 
                "plate(s) of samples."))
  return(dat)
  
}
