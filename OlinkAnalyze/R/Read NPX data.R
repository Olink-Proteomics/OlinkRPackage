#' Function to read NPX data into long format
#'
#' Imports an NPX file exported from NPX manager. Must be an MS Excel file.
#'
#' @param filename filename of NPX file exported from NPX manger.
#' @return A tibble in long format.
#' @keywords NPX
#' @export
#' @examples \donttest{read_NPX("~/NPX data.xlsx")}
#' @import dplyr stringr tidyr

read_NPX <- function(filename){
  dat <- readxl::read_excel(filename, skip = 6, col_names = F,.name_repair="minimal")

  nr_col<-ncol(dat)
  names(dat)<-as.character(1:nr_col)

  dat<-dat %>%
    rename(Name = `1`)

  missfreq<-dat %>% filter(stringr::str_detect(Name, "Missing Data freq."))
  LOD<-dat %>% filter(stringr::str_detect(Name, "LOD"))

  dat <- dat[c(-1*(nrow(dat) - 2):nrow(dat)),]

  meta_dat <-  readxl::read_excel(filename, skip = 2, n_max = 4,col_names = F,.name_repair="minimal")
  meta_dat[4,1] <- 'SampleID'

  nr_col<-ncol(meta_dat)
  names(meta_dat)<-as.character(1:nr_col)

  meta_dat<-meta_dat %>%
    rename(Name = `1`)

  meta_dat<-rbind(meta_dat,missfreq,LOD)


  nr_panel<-(ncol(meta_dat)-1)/94

  SampleID<-dat$Name

  Index_nr<-c(1:length(SampleID))

  panel_data<-list() ##NPX values to be stored
  QC_list<-list()    ##QC data
  meta_data_list<-list() ## meta data
  panel_list<-list()  ## combination of panel data and QC
  assay_name_list<-list()
  panel_list_long<-list()

  for (i in 1:nr_panel) {
    panel_data[[i]]<-dat[,(2+((i-1)*92)):(93+((i-1)*92))]
    QC_list[[i]]<-dat[,c((2+((nr_panel)*92)+(i-1)),(2+((nr_panel)*92)+(i-1))+nr_panel)]
    meta_data_list[[i]]<-meta_dat[,c((2+((i-1)*92)):(93+((i-1)*92)),
                                     (2+((nr_panel)*92)+(i-1)),
                                     (2+((nr_panel)*92)+(i-1))+nr_panel)]
    panel_list[[i]]<-cbind(panel_data[[i]],QC_list[[i]])

    meta_data_list[[i]][4,93] <- meta_data_list[[i]][2,93]
    meta_data_list[[i]][4,94] <- meta_data_list[[i]][2,94]



    colnames(panel_list[[i]]) <- unlist(meta_data_list[[i]][4,])
    panel_list[[i]] <- panel_list[[i]][,!is.na(stringr::str_detect(colnames(panel_list[[i]]), 'SampleID|OID[0-9]{5}'))]


    panel_list[[i]][,c(-93,-94)] <- lapply(panel_list[[i]][,c(-93,-94)],
                                           function(x) as.numeric(stringr::str_replace_all(x, c('#' = '', ',' = '.', 'No Data' = NA))))


    assay_name_list[[i]]<-tibble(ID=c(t(meta_data_list[[i]][4,])),
                                     Name=c(t(meta_data_list[[i]][2,])),
                                     UniProt = c(t(meta_data_list[[i]][3,])),
                                     Panel=c(t(meta_data_list[[i]][1,])),
                                     MissingFreq=c(t(meta_data_list[[i]][5,])),
                                     LOD = as.numeric(c(t(meta_data_list[[i]][6,]))))


    panel_list_long[[i]]<- panel_list[[i]] %>%
      mutate(SampleID = SampleID) %>%
      mutate(Index = Index_nr) %>%
      gather(Assay, NPX, -SampleID,-`QC Warning`,-`Plate ID`,-Index) %>%
      left_join(assay_name_list[[i]], by = c('Assay' = 'ID')) %>%
      select(SampleID,Index,Assay, UniProt, Name,MissingFreq,Panel,`Plate ID`,`QC Warning`,LOD,NPX) %>%
      rename(PlateID =`Plate ID`) %>%
      rename(QC_Warning = `QC Warning`) %>%
      rename(OlinkID = Assay, Assay = Name) 
      
  }

  bind_rows(panel_list_long) %>%
    filter(!is.na(SampleID)) %>%
    tbl_df
}

