#' Normalization of all proteins (by OlinkID). 
#' 
#' Normalizes NPX data frames to another data frame or to reference medians. If two dataframes are normalized to one another, the Olink default is using the older dataframe as reference. 
#' The function handles four different types of normalization: \cr\cr
#' Bridging normalization: One of the dataframes is adjusted to another using overlapping samples (bridge samples). 
#' The overlapping samples need to be named the same between the dataframes and adjustment is made using the median of the paired differences between the bridge samples in the two data frames. 
#' The two dataframes are inputs df1 and df2, the one being adjusted to is specified in the input reference_project and the overlapping samples are specified in overlapping_samples_df1. 
#' Only overlapping_samples_df1 should be input, no matter which dataframe is used as reference_project.  \cr\cr
#' Subset normalization: One of the dataframes is adjusted to the dataframe set as reference_project using a sample subset. 
#' Adjustment is made using the differences in median between the subsets from the two data frames. 
#' Both overlapping_samples_df1 and overlapping_samples_df2 need to be input. 
#' The samples do not need to be named the same. \cr\cr
#' Intensity normalization: A version of subset normalization where all samples (except control samples) from the dataframes are input as overlapping_samples_df1 and overlapping_samples_df2, respectively. \cr\cr 
#' Reference median normalization: Working only on one dataframe. This is effectively subset normalization, but using difference of medians to pre-recorded median values.
#' df1, overlapping_samples_df1 and reference_medians need to be specified. Adjustment of df1 is made using the differences in median between the overlapping samples and the reference medians.
#'
#' @param df1 First dataframe to be used in normalization (required).
#' @param df2 Second dataframe to be used in normalization
#' @param df1_project_nr Project name of first dataset.
#' @param df2_project_nr Project name of second dataset.
#' @param overlapping_samples_df1 Samples to be used for adjustment factor calculation in df1 (required).
#' @param overlapping_samples_df2 Samples to be used for adjustment factor calculation in df1.
#' @param reference_project Project name of reference_project. Needs to be the same as either df1_project_nr or df2_project_nr. The project to which the second project is adjusted to.
#' @param reference_medians Dataframe which needs to contain columns "OlinkID", and "Reference_NPX". Used for reference median normalization.
#'
#' @return A tibble of NPX data in long format containing normalized NPX values, including adjustment factors.
#' @keywords Normalization
#' @export
#' @examples
#' \donttest{
#' npx_df1 <- npx_data1 %>% mutate(Project = 'P1')
#' npx_df2 <- npx_data2 %>% mutate(Project = 'P2')
#'
#' #Bridging normalization:
#' # Find overlaping samples, but exclude Olink controls
#' overlap_samples <- intersect((npx_df1 %>% filter(!grepl("control", SampleID, ignore.case=T)))$SampleID,
#'                              (npx_df2 %>% filter(!grepl("control", SampleID, ignore.case=T)))$SampleID)
#' # Normalize
#' > olink_normalization(df1 = npx_df1,
#'                       df2 = npx_df2,
#'                       overlapping_samples_df1 = overlap_samples,
#'                       df1_project_nr = 'P1',
#'                       df2_project_nr = 'P2',
#'                       reference_project = 'P1')
#'
#' #Subset normalization:
#' # Find a suitable subset of samples from both projects, but exclude Olink controls
#' df1_sampleIDs <- (npx_df1 %>%
#'     filter(!grepl("control", SampleID, ignore.case=T)) %>%
#'     select(SampleID) %>%
#'     distinct())$SampleID
#' df2_sampleIDs <- (npx_df2 %>%
#'     filter(!grepl("control", SampleID, ignore.case=T)) %>%
#'     select(SampleID) %>%
#'     distinct())$SampleID
#' some_samples_df1 <- sample(df1_sampleIDs, 16)
#' some_samples_df2 <- sample(df2_sampleIDs, 16)
#' # Normalize
#' olink_normalization(df1 = npx_df1,
#'                     df2 = npx_df2,
#'                     overlapping_samples_df1 = some_samples_df1,
#'                     overlapping_samples_df2 = some_samples_df2)
#'
#' #Reference median normalization:
#' # For the sake of this example, set the reference median to 1
#' ref_median_df <- npx_df1 %>%
#'     select(OlinkID) %>%
#'     distinct() %>%
#'     mutate(Reference_NPX = 1)
#' # Normalize
#' olink_normalization(df1 = npx_df1,
#'                     overlapping_samples_df1 = some_samples_df1,
#'                     reference_medians = ref_median_df)
#' }
#' @import dplyr stringr tidyr

olink_normalization <- function(df1,
                                df2 = NULL,
                                overlapping_samples_df1,
                                overlapping_samples_df2 = NULL, 
                                df1_project_nr = 'P1',
                                df2_project_nr = 'P2', 
                                reference_project = 'P1',
                                reference_medians = NULL) {
  
  #Filtering on valid OlinkID
  df1 <- df1 %>%
    filter(stringr::str_detect(OlinkID,
                               "OID[0-9]{5}"))
  
  if(!is.null(df2)){
    
    df2 <- df2 %>%
      filter(stringr::str_detect(OlinkID,
                                 "OID[0-9]{5}"))
  }
  
  #median of difference flag
  MOD_FLAG <- T
  
  if(!missing(overlapping_samples_df1)){
    
    if(!all(overlapping_samples_df1 %in% df1$SampleID)){
      
      missing.sample.ids <- setdiff(overlapping_samples_df1,df1$SampleID)
      stop(paste0("SampleID's not found in df1: ",paste(missing.sample.ids,collapse=", ")))
      
    }
    
    if(is.null(overlapping_samples_df2)){
      
      #Testing for reference median normalization, working only on df1
      if(!is.null(reference_medians)){
        
        message('Reference median normalization will be performed.')
        
        shared_oids <- intersect(df1$OlinkID, reference_medians$OlinkID)
        
        missing_df1_oid <- setdiff(df1$OlinkID, shared_oids)
        
        if(length(missing_df1_oid) > 0){
          warning(paste0("There are no reference medians for these assays in df1: ",
                         paste(missing_df1_oid,collapse=", "),
                         '. They will not be normalized.'))
          
        }
        
        
        missing_ref_oid <- setdiff(reference_medians$OlinkID, shared_oids)
        
        if(length(missing_ref_oid) > 0){
          warning(paste0("These OlinkID:s from the reference_medians are not contained in df1: ",
                         paste(missing_df1_oid,collapse=", "),
                         '. They will not be used for normalization.'))
        }
        
        
        adj_factor_df <- df1 %>%
          filter(SampleID %in% overlapping_samples_df1) %>%
          left_join(reference_medians, by= c('OlinkID')) %>% 
          group_by(OlinkID) %>%
          mutate(Assay_Median=median(NPX, na.rm = T)) %>%
          ungroup() %>%
          mutate(Adj_factor = if_else(is.na(Reference_NPX - Assay_Median),
                                      0,
                                      Reference_NPX - Assay_Median)) %>%
          select(OlinkID, Adj_factor) %>%
          distinct()
        
        
        df_adjusted_data <- df1 %>%
          mutate(Panel=str_replace(Panel,'\\(.+', '')) %>%
          left_join(adj_factor_df,by='OlinkID') %>%
          mutate(NPX = NPX + Adj_factor) %>%
          mutate(LOD = LOD + Adj_factor)
        
        return(df_adjusted_data)
        
      }
      
      #MOD bridge normalization
      if(!all(overlapping_samples_df1 %in% df2$SampleID)){
        
        missing.sample.ids <- setdiff(overlapping_samples_df1,df2$SampleID)
        stop(paste0("SampleID's not found in df2: ",paste(missing.sample.ids,collapse=", ")))
      }
      
    }else{
      
      MOD_FLAG <- F
      
      if(!all(overlapping_samples_df2 %in% df2$SampleID)){
        
        missing.sample.ids <- setdiff(overlapping_samples_df2, df2$SampleID)
        stop(paste0("SampleID's not found in df2: ",paste(missing.sample.ids,collapse=", ")))
      }
    }
    
  }else{
    
    stop('An array of SampleID:s (overlapping_samples_df1) needs to be provided for normalization. ')
    
  }
  
  
  if(!(reference_project == df1_project_nr) & !(reference_project == df2_project_nr)){
    
    stop('The reference project needs to be one of the included project numbers in variables df1_project_nr or df2_project_nr.')
    
  }
  
  
  if(MOD_FLAG){
    
    message('Bridging normalization with overlapping samples will be performed.')
    
  }else{
    
    message('Bridging normalization with subcohorts or intensity normalization will be performed.')
    
  }
  
  df1<-df1 %>%
    mutate(Project = df1_project_nr)
  
  df2<-df2 %>%
    mutate(Project = df2_project_nr)
  
  if(MOD_FLAG){
    
    #Median of differences
    
    #Calculate adjustment factors
    adj_factor_df<-df1 %>%
      rbind(df2) %>%
      filter(SampleID %in% overlapping_samples_df1) %>%
      select(SampleID,OlinkID,UniProt,Panel,QC_Warning,NPX,Project) %>% 
      mutate(Panel=str_replace(Panel,'\\(.+', '')) %>%
      spread(Project,NPX)
    
    if(reference_project == df1_project_nr){
      adj_factor_df <- adj_factor_df %>%
        mutate(Diff = if_else(is.na(!!rlang::ensym(df1_project_nr) - !!rlang::ensym(df2_project_nr)), 0, !!rlang::ensym(df1_project_nr)-!!rlang::ensym(df2_project_nr))) 
    }else{
      adj_factor_df <- adj_factor_df %>%
        mutate(Diff = if_else(is.na(!!rlang::ensym(df2_project_nr) - !!rlang::ensym(df1_project_nr)), 0, !!rlang::ensym(df2_project_nr)-!!rlang::ensym(df1_project_nr)))
    }
    
    adj_factor_df <- adj_factor_df %>%
      group_by(OlinkID) %>%
      summarise(Adj_factor=median(Diff,na.rm = T))
    
  }else{
    
    #Difference of medians
    
    #Calculate adjustment factors
    adj_factor_df <- df1 %>% 
      filter(SampleID %in% overlapping_samples_df1) %>%
      rbind(df2 %>% filter(SampleID %in% overlapping_samples_df2)) %>%
      select(SampleID,OlinkID,UniProt,Panel,QC_Warning,NPX,Project) %>% 
      mutate(Panel=str_replace(Panel,'\\(.+', '')) %>%
      group_by(Project, OlinkID) %>%
      summarise(Median=median(NPX,na.rm = T)) %>%
      ungroup() %>%
      spread(Project,Median)
    
    if(reference_project == df1_project_nr){
      adj_factor_df <- adj_factor_df %>%
        mutate(Adj_factor = if_else(is.na(!!rlang::ensym(df1_project_nr) - !!rlang::ensym(df2_project_nr)), 0, !!rlang::ensym(df1_project_nr)-!!rlang::ensym(df2_project_nr))) 
    }else{
      adj_factor_df <- adj_factor_df %>%
        mutate(Adj_factor = if_else(is.na(!!rlang::ensym(df2_project_nr) - !!rlang::ensym(df1_project_nr)), 0, !!rlang::ensym(df2_project_nr)-!!rlang::ensym(df1_project_nr)))
    }
    
    adj_factor_df <- adj_factor_df %>%
      select(OlinkID, Adj_factor)
  }
  
  
  #Apply adjustment factors
  df_adjusted_data<-df1 %>%
    rbind(df2) %>%
    mutate(Panel=str_replace(Panel,'\\(.+', '')) %>%
    left_join(adj_factor_df,by='OlinkID') %>%
    mutate(Adj_factor = if_else(Project == reference_project,0,Adj_factor)) %>% 
    mutate(NPX = NPX + Adj_factor) %>%  
    mutate(LOD = LOD + Adj_factor)
  
  
  return(df_adjusted_data)
  
}

