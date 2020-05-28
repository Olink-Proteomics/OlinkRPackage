#' Function to read NPX data into long format
#'
#' Imports an NPX file exported from NPX Manager. 
#' No alterations to the output NPX Manager format is allowed.
#'
#' @param filename Path to file NPX Manager output file.
#' @return A tibble in long format.
#' @keywords NPX
#' @export
#' @examples \donttest{read_NPX("~/NPX data.xlsx")}
#' @import dplyr stringr tidyr

read_NPX <- function(filename){
  
  NORM_FLAG <-  F
  
  meta_dat <-  readxl::read_excel(filename, skip = 2, n_max = 4,col_names = F,.name_repair="minimal")
  meta_dat[4,1] <- 'SampleID'
  
  NR_DEVIATIONS <- sum(stringr::str_detect(meta_dat[2,], 'QC Deviation from median'))
  control_index <- (stringr::str_detect(meta_dat[2,], 'Det Ctrl') |
                      stringr::str_detect(meta_dat[2,], 'Inc Ctrl 2') |
                      stringr::str_detect(meta_dat[2,], 'Inc Ctrl 1') |
                      stringr::str_detect(meta_dat[2,], 'Ext Ctrl'))
  meta_dat[4, control_index] <- meta_dat[2, control_index]
  meta_dat[3, control_index] <- '-'
  NR_CONTROLS <- sum(control_index)
  
  
  nr_col<-ncol(meta_dat)
  names(meta_dat)<-as.character(1:nr_col)
  
  meta_dat<-meta_dat %>%
    rename(Name = `1`)
  
  dat <- readxl::read_excel(filename, skip = 6, col_names = F,.name_repair="minimal", col_types = c('text'))
  
  nr_col<-ncol(dat)
  names(dat)<-as.character(1:nr_col)
  
  dat<-dat %>%
    rename(Name = `1`)
  
  missfreq<-dat %>% filter(stringr::str_detect(Name, "Missing Data freq."))
  LOD<-dat %>% filter(stringr::str_detect(Name, "LOD"))
  norm_method <- dat %>% filter(stringr::str_detect(Name, "Normalization"))
  
  if(nrow(norm_method) == 0){
    dat <- dat[c(-1*(nrow(dat) - 2):nrow(dat)),]
  }else{
    dat <- dat[c(-1*(nrow(dat) - 3):nrow(dat)),]
    NORM_FLAG <- T
  }
  
  meta_dat<-rbind(meta_dat,missfreq,LOD,norm_method)
  nr_panel<-(ncol(meta_dat)-1-NR_DEVIATIONS-NR_CONTROLS)/94
  
  SampleID<-dat$Name
  
  Index_nr<-c(1:length(SampleID))
  
  panel_data<-list() ##NPX values to be stored
  QC_list<-list()    ##QC data
  meta_data_list<-list() ## meta data
  panel_list<-list()  ## combination of panel data and QC
  assay_name_list<-list()
  panel_list_long<-list()
  deviations_list <- list()
  
  BASE_INDEX <- 92
  
  if(NR_CONTROLS > 0){
    BASE_INDEX <- BASE_INDEX + NR_CONTROLS/nr_panel
  }
  
  
  for (i in 1:nr_panel) {
    
    panel_data[[i]]<-dat[,(2+((i-1)*BASE_INDEX)):((BASE_INDEX+1)+((i-1)*BASE_INDEX))]
    
    if(NR_DEVIATIONS == 0){
      
      QC_list[[i]]<-dat[,c((2+((nr_panel)*BASE_INDEX)+(i-1)),
                           (2+((nr_panel)*BASE_INDEX)+(i-1))+nr_panel)]
      
      meta_data_list[[i]]<-meta_dat[,c((2+((i-1)*BASE_INDEX)):((BASE_INDEX+1)+((i-1)*BASE_INDEX)),
                                       (2+((nr_panel)*BASE_INDEX)+(i-1)),
                                       (2+((nr_panel)*BASE_INDEX)+(i-1))+nr_panel)]
      
      
    }else{
      
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
                                                                   function(x) as.numeric(stringr::str_replace_all(x, c('#' = '', ',' = '.', 'No Data' = NA))))
    
    
    assay_name_list[[i]]<-tibble(ID=c(t(meta_data_list[[i]][4,])),
                                 Name=c(t(meta_data_list[[i]][2,])),
                                 UniProt = c(t(meta_data_list[[i]][3,])),
                                 Panel=c(t(meta_data_list[[i]][1,])),
                                 MissingFreq=c(t(meta_data_list[[i]][5,])),
                                 LOD = as.numeric(c(t(meta_data_list[[i]][6,]))))
    
    if(NORM_FLAG == T){
      assay_name_list[[i]] <- bind_cols(assay_name_list[[i]], 
                                        Normalization = c(t(meta_data_list[[i]][7,])))
    }
    
    
    panel_list_long[[i]]<- panel_list[[i]] %>%
      mutate(SampleID = SampleID) %>%
      mutate(Index = Index_nr) %>%
      gather(Assay, NPX, -SampleID,-`QC Warning`,-`Plate ID`,-Index,-matches("QC Deviation Inc Ctrl"), -matches("QC Deviation Det Ctrl")) %>%
      left_join(assay_name_list[[i]], by = c('Assay' = 'ID')) %>%
      select(SampleID,Index,Assay, UniProt, Name,MissingFreq,Panel,`Plate ID`,`QC Warning`,LOD,NPX,matches("Normalization"), matches("QC Deviation Inc Ctrl"), matches("QC Deviation Det Ctrl")) %>%
      rename(PlateID =`Plate ID`) %>%
      rename(QC_Warning = `QC Warning`) %>%
      rename(OlinkID = Assay, Assay = Name) 
    
  }
  
  bind_rows(panel_list_long) %>%
    filter(!is.na(SampleID)) %>%
    tbl_df %>% 
    mutate(Panel_Version = gsub(".*\\(","",Panel)) %>% 
    mutate(Panel_Version = gsub("\\)","",Panel_Version)) %>% 
    mutate(Panel =  gsub("\\(.*\\)","",Panel)) %>% 
    select(SampleID, Index, OlinkID, UniProt, Assay, MissingFreq, Panel,Panel_Version,PlateID, QC_Warning,LOD,NPX,matches("Normalization"), matches("*Inc Ctrl*"), matches("*Det Ctrl*"))
  
}

