#' Function to read NPX data into long format
#'
#' Imports an NPX file exported from NPX Manager. 
#' No alterations to the output NPX Manager format is allowed.
#'
#' @param filename Path to file NPX Manager output file.
#' @return A tibble in long format.
#' @keywords NPX
#' @export
#' @examples \donttest{
#' file <- system.file("extdata", "Example_NPX_data.csv", package = "OlinkAnalyze)
#' read_NPX(file)
#' }
#' @import dplyr stringr tidyr

read_NPX <- function(filename){
  
  # If the file is csv or txt, read_NPX assumes that the file is explore data in long format
  if (tools::file_ext(filename) %in% c("csv","txt")) {
    #read file using ; as delimiter
    out <- read.table(filename, header = T, sep=";", stringsAsFactors = F,
                      na.strings = c("NA",""))
    if (is.data.frame(out) & ncol(out) == 1) {
      #if only one column in the data, wrong delimiter. use , as delimiter
      out <- read.table(filename, header = T, sep=",", stringsAsFactors = F,
                        na.strings = c("NA",""))
    }
    #check that all colnames are present
    match_old_header <- all(c("SampleID", "Index", "OlinkID", "UniProt", "Assay",
                              "MissingFreq", "Panel", "Panel_Version", "PlateID",
                              "QC_Warning", "LOD", "NPX") %in% colnames(out))
    match_new_header <- all(c("SampleID", "Index", "OlinkID", "UniProt", "Assay",
                              "MissingFreq", "Panel", "Panel_Lot_Nr", "PlateID",
                              "QC_Warning", "LOD", "NPX") %in% colnames(out))
    if (match_old_header || match_new_header) {
      out$NPX <- as.numeric(out$NPX)
      out$LOD <- as.numeric(out$LOD)
      out$MissingFreq <- as.numeric(out$MissingFreq)
      out$SampleID <- as.character(out$SampleID)
      return(as_tibble(out))
    } else {
      missing.cols <- setdiff(c("SampleID", "Index", "OlinkID", "UniProt", "Assay", "MissingFreq", "Panel", "Panel_Lot_Nr", "PlateID", "QC_Warning", "LOD", "NPX"),
                              colnames(out))
      #If columns are missing, stop and print out which are missing
      stop(paste0("Cannot find columns ", paste(missing.cols,collapse=",")))
    }
  }
  NORM_FLAG <-  F
  
  # Check if the data is npx or concentration as well as if it is tg48 or tg96
  
  data_type <- readxl::read_excel(filename, range='A2',
                                  col_names = F, .name_repair="minimal")
  if (grepl('NPX', data_type, fixed=TRUE)) {
    is_npx_data <- TRUE
    n_max_meta_data <- 4
    
    # Check whether it is target 48 or 96
    panel_name <- readxl::read_excel(filename, range='B3',
                                     col_names = F, .name_repair="minimal")
    if (grepl('Target 48', panel_name, fixed=TRUE)) {
      target_type <- '48'
      BASE_INDEX <- 45
    } else {
      target_type <- '96'
      BASE_INDEX <- 92
    }
  } else if (grepl('Quantified', data_type, fixed=TRUE)) {
    # Quant data given, which also means it is target 48
    is_npx_data <- FALSE
    n_max_meta_data <- 5
    target_type <- '48'
    BASE_INDEX <- 45
  } else {
    stop("Cannot find whether the given data is NPX or concentration")
  }
  
  # Load initial meta data (the first rows of the wide file)
  meta_dat <-  readxl::read_excel(filename, skip = 2, n_max = n_max_meta_data,
                                  col_names = F, .name_repair="minimal")
  meta_dat[4,1] <- 'SampleID'
  NR_DEVIATIONS <- sum(stringr::str_detect(meta_dat[2,],
                                           'QC Deviation from median'))
  control_index <- (stringr::str_detect(meta_dat[2,], 'Det Ctrl') |
                      stringr::str_detect(meta_dat[2,], 'Inc Ctrl 2') |
                      stringr::str_detect(meta_dat[2,], 'Inc Ctrl 1') |
                      stringr::str_detect(meta_dat[2,], 'Ext Ctrl'))
  meta_dat[4, control_index] <- meta_dat[2, control_index]
  meta_dat[3, control_index] <- '-'
  NR_CONTROLS <- sum(control_index)
  nr_panel<-(ncol(meta_dat)-1-NR_DEVIATIONS-NR_CONTROLS)/(BASE_INDEX+2)
  
  nr_col <- ncol(meta_dat)
  names(meta_dat) <- as.character(1:nr_col)
  
  meta_dat <- meta_dat %>%
    rename(Name = `1`)
  
  # Load NPX or QUANT data including the last rows of meta data
  dat <- readxl::read_excel(filename, skip = n_max_meta_data+2, col_names = F,
                            .name_repair="minimal", col_types = c('text'))
  
  nr_col <- ncol(dat)
  names(dat) <- as.character(1:nr_col)
  
  dat<-dat %>%
    rename(Name = `1`)
  
  # Calc nbr of plates
  plates <- dat[,nr_col-nr_panel] %>% distinct() %>% na.omit() %>% pull()
  nr_plates <- length(plates)
  
  # Extract the meta data from the last rows of data
  missfreq<-dat %>% filter(stringr::str_detect(Name, "Missing Data freq."))
  norm_method <- dat %>% filter(stringr::str_detect(Name, "Normalization"))
  if (!is_npx_data) {
    assay_warning <- dat %>% filter(stringr::str_detect(Name, "Assay warning"))
    Plate_LQL <- dat %>% filter(stringr::str_detect(Name, 
                                                    "Lowest quantifiable level"))
    LOD <- dat %>% filter(stringr::str_detect(Name, "Plate LOD"))
    LLOQ <- dat %>% filter(stringr::str_detect(Name, "LLOQ"))
    ULOQ <- dat %>% filter(stringr::str_detect(Name, "ULOQ"))
  } else {
    LOD <- dat %>% filter(stringr::str_detect(Name, "LOD"))
  }
  
  # Add the new meta data to ´meta_dat´
  meta_dat <- rbind(meta_dat,missfreq)
  if (!is_npx_data) {
    meta_dat <- rbind(meta_dat,LLOQ,ULOQ,assay_warning,Plate_LQL)
  }
  meta_dat <- rbind(meta_dat,LOD,norm_method)
  
  # Remove the meta data from dat
  if (is_npx_data) {
    nbr_meta_data_rows_bottom <- 3
  } else {
    nbr_meta_data_rows_bottom <- 4+3*nr_plates
  }
  if (nrow(norm_method) == 0) {
    nbr_meta_data_rows_bottom <- nbr_meta_data_rows_bottom - 1
  } else {
    NORM_FLAG <- T
  }
  dat <- dat[c(-1*(nrow(dat) - nbr_meta_data_rows_bottom):nrow(dat)),]
  
  # Create index vector
  SampleID <- dat$Name
  Index_nr <- c(1:length(SampleID))
  
  # Initiate lists for later use
  panel_data <- list() ##NPX values to be stored
  QC_list <- list()    ##QC data
  meta_data_list <- list() ## meta data
  panel_list <- list()  ## combination of panel data and QC
  assay_name_list <- list()
  panel_list_long <- list()
  deviations_list <- list()
  
  if (NR_CONTROLS > 0) {
    BASE_INDEX <- BASE_INDEX + NR_CONTROLS/nr_panel
  }
  
  # Construct a list of tibbles that match the long format
  for (i in 1:nr_panel) {
    
    panel_data[[i]]<-dat[,(2+((i-1)*BASE_INDEX)):((BASE_INDEX+1)+((i-1)*BASE_INDEX))]
    
    if (NR_DEVIATIONS == 0) {
      QC_list[[i]]<-dat[,c((2+((nr_panel)*BASE_INDEX)+(i-1)),
                           (2+((nr_panel)*BASE_INDEX)+(i-1))+nr_panel)]
      
      meta_data_list[[i]]<-meta_dat[,c((2+((i-1)*BASE_INDEX)):((BASE_INDEX+1)+((i-1)*BASE_INDEX)),
                                       (2+((nr_panel)*BASE_INDEX)+(i-1)),
                                       (2+((nr_panel)*BASE_INDEX)+(i-1))+nr_panel)]
      
      
    } else {
      
      QC_list[[i]]<-dat[,c((2+((nr_panel)*BASE_INDEX)+(i-1)),
                           (2+((nr_panel)*BASE_INDEX)+(i-1))+nr_panel, 
                           (2+((nr_panel)*BASE_INDEX)+(i-1))+2*nr_panel+(i-1), 
                           (2+((nr_panel)*BASE_INDEX)+(i-1))+2*nr_panel+(i-1)+1)]
      
      meta_data_list[[i]]<-meta_dat[,c((2+((i-1)*BASE_INDEX)):((BASE_INDEX+1)+((i-1)*BASE_INDEX)),
                                       (2+((nr_panel)*BASE_INDEX)+(i-1)),
                                       (2+((nr_panel)*BASE_INDEX)+(i-1))+nr_panel,
                                       (2+((nr_panel)*BASE_INDEX)+(i-1))+2*nr_panel, 
                                       (2+((nr_panel)*BASE_INDEX)+(i-1))+3*nr_panel)]
      
      meta_data_list[[i]][4,(BASE_INDEX+3)] <- "QC Deviation Inc Ctrl"
      meta_data_list[[i]][4,(BASE_INDEX+4)] <- "QC Deviation Det Ctrl"
      
      
    }
    
    meta_data_list[[i]][4,(BASE_INDEX+1)] <- meta_data_list[[i]][2,(BASE_INDEX+1)]
    meta_data_list[[i]][4,(BASE_INDEX+2)] <- meta_data_list[[i]][2,(BASE_INDEX+2)]
    
    
    panel_list[[i]]<-cbind(panel_data[[i]],QC_list[[i]])
    
    colnames(panel_list[[i]]) <- unlist(meta_data_list[[i]][4,])
    
    panel_list[[i]][,c(-(BASE_INDEX+1),-(BASE_INDEX+2))] <- lapply(panel_list[[i]][,c(-(BASE_INDEX+1),-(BASE_INDEX+2))],
                                                                   function(x) as.numeric(stringr::str_replace_all(x, c('#' = '', ',' = '.', 'No Data' = NA, '> ULOQ' = NA, '< LLOQ' = NA))))
    
    # Remove the last two columns since they contain redundant meta data and
    # will only cause warnings
    meta_data_list[[i]] <- meta_data_list[[i]][,c(-(BASE_INDEX+1),-(BASE_INDEX+2))]
    
    assay_name_list[[i]]<-tibble(ID=c(t(meta_data_list[[i]][4,])),
                                 Name=c(t(meta_data_list[[i]][2,])),
                                 UniProt = c(t(meta_data_list[[i]][3,])),
                                 Panel=c(t(meta_data_list[[i]][1,])))
    
    if (is_npx_data) {
      assay_name_list[[i]] <- bind_cols(assay_name_list[[i]],
                                        MissingFreq=c(t(meta_data_list[[i]][5,])),
                                        LOD = as.numeric(c(t(meta_data_list[[i]][6,]))))
      
      if (NORM_FLAG == TRUE) {
        assay_name_list[[i]] <- bind_cols(assay_name_list[[i]], 
                                          Normalization = c(t(meta_data_list[[i]][7,])))
      }
      
      panel_list_long[[i]]<- panel_list[[i]] %>%
        mutate(SampleID = SampleID) %>%
        mutate(Index = Index_nr) %>%
        gather(Assay, NPX, -SampleID,-`QC Warning`,-`Plate ID`,-Index,-matches("QC Deviation Inc Ctrl"), -matches("QC Deviation Det Ctrl")) %>%
        left_join(assay_name_list[[i]], by = c('Assay' = 'ID')) %>%
        select(SampleID, Index, Assay, UniProt, Name, MissingFreq, Panel,`Plate ID`,`QC Warning`, LOD, NPX, matches("Assay_Warning"), matches("Normalization"), matches("QC Deviation Inc Ctrl"), matches("QC Deviation Det Ctrl")) %>%
        rename(PlateID = `Plate ID`) %>%
        rename(QC_Warning = `QC Warning`) %>%
        rename(OlinkID = Assay, Assay = Name) 
    } else {
      for (j in 1:nr_plates) {
        assay_name_by_plate <- bind_cols(assay_name_list[[i]],
                                         Unit=c(t(meta_data_list[[i]][5,])),
                                         MissingFreq=c(t(meta_data_list[[i]][6,])),
                                         LLOQ = as.numeric(c(t(meta_data_list[[i]][7,]))),
                                         ULOQ = as.numeric(c(t(meta_data_list[[i]][8,]))),
                                         Assay_Warning = c(t(meta_data_list[[i]][(9+(j-1)),])),
                                         Plate_LQL = as.numeric(c(t(meta_data_list[[i]][(9+nr_plates+(j-1)),]))),
                                         LOD = as.numeric(c(t(meta_data_list[[i]][(9+2*nr_plates+(j-1)),]))))
        if(NORM_FLAG == T){
          assay_name_by_plate <- bind_cols(assay_name_by_plate, 
                                           Normalization = c(t(meta_data_list[[i]][9+3*nr_plates,])))
        }
        panel_list_long[[(i-1)*j+j]]<- panel_list[[i]] %>%
          mutate(SampleID = SampleID) %>%
          mutate(Index = Index_nr) %>%
          gather(Assay, NPX, -SampleID,-`QC Warning`,-`Plate ID`,-Index,-matches("QC Deviation Inc Ctrl"), -matches("QC Deviation Det Ctrl")) %>%
          filter(`Plate ID` == plates[[j]]) %>% 
          left_join(assay_name_by_plate, by = c('Assay' = 'ID')) %>%
          select(SampleID, Index, Assay, UniProt, Name, MissingFreq, Panel,`Plate ID`,`QC Warning`, LOD, NPX, Unit, ULOQ, LLOQ, Plate_LQL, Assay_Warning, matches("Normalization"), matches("QC Deviation Inc Ctrl"), matches("QC Deviation Det Ctrl")) %>%
          rename(PlateID = `Plate ID`) %>%
          rename(QC_Warning = `QC Warning`) %>%
          rename(OlinkID = Assay, Assay = Name)
      }
    }
  }
  
  if (!is_npx_data) {
    for (i in 1:(nr_panel*nr_plates)) {
      panel_list_long[[i]] <- panel_list_long[[i]] %>%
        rename(Plate_LOD = LOD) %>%
        rename(Quantified_value = NPX)
    }
  }
  
  bind_rows(panel_list_long) %>%
    filter(!is.na(SampleID)) %>%
    as_tibble() %>% 
    mutate(Panel_Version = gsub(".*\\(","",Panel)) %>% 
    mutate(Panel_Version = gsub("\\)","",Panel_Version)) %>% 
    mutate(Panel =  gsub("\\(.*\\)","",Panel)) %>% 
    select(SampleID, Index, OlinkID, UniProt, Assay, MissingFreq, Panel,Panel_Version,PlateID, QC_Warning,matches("Plate_LQL"),matches("LOD"),matches("Plat_LOD"),matches("LLOQ"),matches("ULOQ"),matches("NPX"),matches("Quantified_value"),matches("Unit"),matches("Assay_Warning"),matches("Normalization"), matches("*Inc Ctrl*"), matches("*Det Ctrl*"))
  
}
