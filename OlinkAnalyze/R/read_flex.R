#' Read in flex data
#'
#' Called by read_NPX
#'
#' @param filename where the file is located
#'
#' @return tibble of data
read_npflex <- function(filename) {

  project <- readxl::read_excel(path = filename,
                                range = "A1",
                                col_names = F,
                                .name_repair = "minimal")
  
  version <- readxl::read_excel(path = filename,
                               range = "B1",
                               col_names = F,
                               .name_repair = "minimal")
  
  data_type <- readxl::read_excel(path = filename,
                                  range = "A2",
                                  col_names = F,
                                  .name_repair = "minimal")
  
  is_npx_data <- ifelse(grepl(pattern = "NPX", x = data_type, fixed = TRUE), TRUE, FALSE) 
  
  # Extract Panel Names
  panel_name <-  unlist(as.vector(readxl::read_excel(path = filename,
                                    skip = 2,
                                    n_max = 1,
                                    col_names = F,
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
    dplyr::filter(!is.na(OlinkID))
  
  # Define number of proteins per panel
  assays_per_panel <- metadata |> 
    dplyr::select(Panel) |> 
    table() |> 
    as.data.frame()
  
  # Load NPX or QUANT data including the last rows of meta data
  dat <- readxl::read_excel(path = filename,
                            skip = n_max_meta_data + 2 + 1,
                            col_names = FALSE,
                            .name_repair="minimal",
                            col_types = c('text'))
  names(dat) <- c("SampleID", metadata$OlinkID, paste0(rep("PlateID", panel_number), 1:panel_number), paste0(rep("QC_Warning", panel_number), 1:panel_number))
  
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
    dplyr::select(-SampleID, -tidyselect::starts_with("PlateID"), -tidyselect::starts_with("QC_Warning"))
  norm_method <- dat |> dplyr::filter(stringr::str_detect(SampleID, "Normalization")) |> 
    tidyr::pivot_longer(cols = tidyselect::starts_with("OID"),
                        names_to = "OlinkID",
                        values_to = "Normalization") |> 
    dplyr::select(-SampleID, -tidyselect::starts_with("PlateID"), -tidyselect::starts_with("QC_Warning"))
  
  if (!is_npx_data) {
    assay_warning <- dat |> dplyr::filter(stringr::str_detect(SampleID, "Assay warning")) |> 
      tidyr::pivot_longer(cols = tidyselect::starts_with("OID"),
                          names_to = "OlinkID",
                          values_to = "Assay_Warning") |> 
      dplyr::select(-SampleID, -tidyselect::starts_with("QC_Warning"))
    
    # # of rows = # of plates
    Plate_LQL <- dat |> dplyr::filter(stringr::str_detect(SampleID, "Lowest quantifiable level")) |> 
      tidyr::pivot_longer(cols = tidyselect::starts_with("OID"),
                          names_to = "OlinkID",
                          values_to = "Plate_LQL") |> 
      dplyr::select(-tidyselect::starts_with("QC_Warning")) |> 
      dplyr::select(-SampleID)
    
    # # of rows = # of plates
    LOD <- dat |> dplyr::filter(stringr::str_detect(SampleID, "Plate LOD")) |> 
      tidyr::pivot_longer(cols = tidyselect::starts_with("OID"),
                          names_to = "OlinkID",
                          values_to = "Plat_LOD") |> 
      dplyr::select(-tidyselect::starts_with("QC_Warning")) |> 
      dplyr::select(-SampleID)
    
    LLOQ <- dat |> dplyr::filter(stringr::str_detect(SampleID, "LLOQ"))  |> 
      tidyr::pivot_longer(cols = tidyselect::starts_with("OID"),
                          names_to = "OlinkID",
                          values_to = "LLOQ") |> 
      dplyr::select(-SampleID, -tidyselect::starts_with("PlateID"), -tidyselect::starts_with("QC_Warning"))
    
    ULOQ <- dat |> dplyr::filter(stringr::str_detect(SampleID, "ULOQ"))  |> 
      tidyr::pivot_longer(cols = tidyselect::starts_with("OID"),
                          names_to = "OlinkID",
                          values_to = "ULOQ") |> 
      dplyr::select(-SampleID, -tidyselect::starts_with("PlateID"), -tidyselect::starts_with("QC_Warning"))
    
    meta_data_per_assay <- dplyr::left_join(missfreq,norm_method, by = "OlinkID") |> 
      dplyr::left_join(assay_warning, by = "OlinkID", multiple ="all") |>
      dplyr::left_join(LLOQ, by = c("OlinkID")) |> 
      dplyr::left_join(Plate_LQL, by = c("OlinkID", "PlateID1")) |> 
      dplyr::left_join(LOD, by = c("OlinkID", "PlateID1")) 
  } else {
    LOD <- dat |> dplyr::filter(stringr::str_detect(SampleID, "LOD")) |> 
      dplyr::select(-SampleID, -tidyselect::starts_with("PlateID"), -tidyselect::starts_with("QC_Warning")) |> 
      tidyr::pivot_longer(cols = tidyselect::starts_with("OID"),
                          names_to = "OlinkID",
                          values_to = "LOD") 
    meta_data_per_assay <- dplyr::left_join(missfreq,norm_method, by = c("OlinkID")) |> 
      dplyr::left_join(LOD, by = c("OlinkID")) |> 
      dplyr::select(-tidyselect::starts_with("PlateID"), -tidyselect::starts_with("QC_Warning"))
  }
  
  
  # pivot longer
  dat<- dat |> 
    dplyr::filter(!str_detect(SampleID, paste( "Missing Data freq.",
                                               "Normalization",
                                               "Assay warning",
                                               "Lowest quantifiable level",
                                               "Plate LOD",
                                               "LLOQ",
                                               "ULOQ",
                                               "LOD", 
                                               collapse = "|"))) |>  # remove end meta data
    tidyr::pivot_longer(cols = tidyselect::starts_with("OID"),
                        names_to = "OlinkID",
                        values_to = ifelse(is_npx_data, "NPX", "Quantified_value")) |> 
    dplyr::left_join(metadata, by = c("OlinkID")) |> 
    dplyr::left_join(meta_data_per_assay, multiple = "all") |> 
    dplyr::mutate(PlateID = PlateID1) |> 
    dplyr::mutate(QC_Warning = QC_Warning1) |> 
    dplyr::select(SampleID, OlinkID,
                  UniProt, Assay, MissingFreq,
                  Panel,PlateID,
                  QC_Warning,dplyr::matches("Plate_LQL"),
                  dplyr::matches("LOD"),
                  dplyr::matches("Plat_LOD"),
                  dplyr::matches("LLOQ"),
                  dplyr::matches("ULOQ"),
                  dplyr::matches("NPX"),
                  dplyr::matches("Quantified_value"),
                  dplyr::matches("Unit"),
                  dplyr::matches("Assay_Warning"),
                  dplyr::matches("Normalization"),
                  dplyr::matches("*Inc Ctrl*"),
                  dplyr::matches("*Det Ctrl*"))
  message(paste(panel_number, "Flex panel(s) were detected with", nr_plates, 
                "plate(s) of samples."))
  return(dat)
  
}
