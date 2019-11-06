#' Function to normalize two datasets together
#'
#' Median normalizes all proteins (by OlinkID) in two dataframes. If using bridging samples, adjustment is made using the median of the paired differences between the two data frames. If intensity normalizing, adjustment is done using the difference of the medians.
#' @param df1 First dataframe to be used in normalization
#' @param df2 Second dataframe to be used in normalization
#' @param overlapping_sample Character vector of sample ID's to be used for bridge normalization. Sample ID's must be present in both df1 and df2. If not provided, intensity normalization will be performed.
#' @param df1_project_nr Project name of first dataset.
#' @param df2_project_nr Project name of second dataset.
#' @param centring If TRUE, medians are adjusted to the centerpoint between the median for df1 and the median for df2. If FALSE, df2 is adjusted to have the same median as df1 (default TRUE)
#' @return A data frame of NPX data in long format containing both df1 and df2 and normalized NPX values. 
#' @keywords Normalization
#' @export
#' @examples \donttest{
#' data_1 <- read_NPX("~/NPX data1.xlsx")
#' data_2 <- read_NPX("~/NPX data2.xlsx")
#' olink_normalization(data_1,data_2,df1_project_nr='Project 1', df2_project_nr='Project 2',centering = T)}
#' @import dplyr stringr tidyr

olink_normalization <- function(df1,df2,overlapping_sample,df1_project_nr='P1',df2_project_nr='P2', centering = T) {
  
  
  if(!missing(overlapping_sample)){
    
    if(!all(overlapping_sample %in% df1$SampleID)){
      
      missing.sample.ids <- setdiff(overlapping_sample,df1$SampleID)
      stop(paste0("SampleID's not found in df1: ",paste(missing.sample.ids,collapse=", ")))
      
    }
    
    if(!all(overlapping_sample %in% df2$SampleID)){
      
      missing.sample.ids <- setdiff(overlapping_sample,df2$SampleID)
      stop(paste0("SampleID's not found in df2: ",paste(missing.sample.ids,collapse=", ")))
      
    }
  }
  
  df1<-df1 %>%
    mutate(Project = 'P1')
  
  df2<-df2 %>%
    mutate(Project = 'P2')
  
  if(missing(overlapping_sample)){
    adj_factor_df<-df1 %>% ##maybe remove control samples?
      rbind(df2) %>%
      select(SampleID,OlinkID,UniProt,Panel,QC_Warning,NPX,Project) %>%
      mutate(Panel=str_replace(Panel,'\\(.+', '')) %>%
      group_by(Project,OlinkID) %>%
      summarise(Median=median(NPX,na.rm = T)) %>%
      spread(Project,Median) %>%
      mutate(Adj_factor = if_else(is.na(P1 - P2), 0, P1-P2)) %>% 
      select(OlinkID,Adj_factor)
  } else{
    adj_factor_df<-df1 %>%
      rbind(df2) %>%
      filter(SampleID %in% overlapping_sample) %>%
      select(SampleID,OlinkID,UniProt,Panel,QC_Warning,NPX,Project) %>% 
      mutate(Panel=str_replace(Panel,'\\(.+', '')) %>%
      spread(Project,NPX) %>%
      mutate(Diff = if_else(is.na(P1 - P2), 0, P1-P2)) %>% 
      group_by(OlinkID) %>%
      summarise(Adj_factor=median(Diff,na.rm = T))
  }
  
  if (centering == TRUE) {
    df_adjusted_data<-df1 %>%
      rbind(df2) %>%
      mutate(Panel=str_replace(Panel,'\\(.+', '')) %>%
      left_join(adj_factor_df,by='OlinkID') %>%
      mutate(Adj_factor = if_else(Project == 'P1',-Adj_factor/2,Adj_factor/2)) %>% ###change the adjustment factor to 0 for on project
      mutate(NPX = NPX + Adj_factor) %>%  ### add adjustment factor
      mutate(LOD = LOD + Adj_factor) %>%
      group_by(OlinkID) %>%
      mutate(LOD = max(LOD)) %>%
      mutate(MissingFreq = sum(NPX<LOD,na.rm=T)/n()) %>% ## calculate missing freq
      ungroup()
  } else {
    df_adjusted_data<-df1 %>%
      rbind(df2) %>%
      mutate(Panel=str_replace(Panel,'\\(.+', '')) %>%
      left_join(adj_factor_df,by='OlinkID') %>%
      mutate(Adj_factor = if_else(Project == 'P1',0,Adj_factor)) %>% ###change the adjustment factor to 0 for on project
      mutate(NPX = NPX + Adj_factor) %>%  ### add adjustment factor
      mutate(LOD = LOD + Adj_factor) %>%
      group_by(OlinkID) %>%
      mutate(LOD = max(LOD)) %>%
      mutate(MissingFreq = sum(NPX<LOD,na.rm=T)/n()) %>% ## calculate missing freq
      ungroup()
  }
  
  ## change name from df1 to df2
  change_list<-df_adjusted_data %>% 
    select(OlinkID,Assay,Project) %>%
    unique() %>% 
    group_by(OlinkID,Assay) %>% 
    filter(n()==1) %>% 
    group_by(OlinkID) %>% 
    filter(n()>1) %>% 
    spread(Project,Assay)
  
  if (nrow(change_list)>0) {
    
    df_adjusted_data<-df_adjusted_data %>% 
      left_join(change_list,by = 'OlinkID') %>% 
      mutate(Name = if_else(Name == P1 & !is.na(P1), P2, Name)) %>%
      select(-P1, -P2)
  }
  
  df_adjusted_data<-df_adjusted_data %>% 
  mutate(Project = if_else(Project == 'P1',df1_project_nr,df2_project_nr))   ###rename project
    
  
  return(df_adjusted_data)
  
}
