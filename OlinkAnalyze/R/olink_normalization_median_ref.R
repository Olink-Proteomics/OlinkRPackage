### Median normalization - base taken from olink_normalization

olink_normalization_median_ref <- function(df1,
                                           df2 = NULL,
                                           overlapping_samples_df1,
                                           overlapping_samples_df2 = NULL,
                                           df1_project_nr = 'P1',
                                           df2_project_nr = 'P2',
                                           reference_project = 'P1',
                                           reference_medians = NULL) {
  
  
  # Creating a concatenated OlinkID which contains HT and 3K OlinkID - necessary for unique OlinkIDs
  #### NEED TO GET A CUSTOMER-FACING MAPPING FUNCTION - FOR NOW USING INTERNAL SOURCES
  map_oid <- npxexplorer::oid_map |> # this is the olink_ids_match_3k_ht.csv file
    mutate(OlinkID_HT_3K = paste(olink_id_ht, olink_id_3k, sep = "_")) |>
    select(olink_id_ht, olink_id_3k, OlinkID_HT_3K)
  
  # Checks if 1 DF is provided for bridge normalization - REQUIRES REFERENCE MEDIANS
  
  if(is.null(df2)){ # If df2 is NOT provided
    
    if(is.null(reference_medians)) {
      stop("If normalizing only one data frame, you must supply reference medians.")
    }
    
    if(!all(df1 |> select(Panel) |> distinct() |> na.omit() |> pull() %in% 
            c("Cardiometabolic", "Cardiometabolic_II",
              "Inflammation", "Inflammation_II",
              "Neurology", "Neurology_II",
              "Oncology", "Oncology_II"))) {
      stop("When supplying a single data frame, df1 must contain Explore 3072 data in parquet format.")
    }
    
    if(!"Count" %in% colnames(df1)) {
      stop("When supplying a single data frame, df1 must contain Explore 3072 data in parquet format.")
    }
    
    # If all criteria passed, add assay mapping with df1 3K
    df1 <- df1 |> 
      left_join(map_oid |> select(-olink_id_ht), 
                by = c("OlinkID" = "olink_id_3k"),
                relationship = "many-to-many")
  }
  
  # Checks if 2 DFs provided for bridge normalization
  if(!is.null(df2)){ # If df2 is provided
    
    # If 2 data frames are supplied, make sure they have the same column names, if not, add missing column names.
    
    if(length(c(setdiff(names(df1), names(df2)),setdiff(names(df2), names(df1)))) != 0){ # and the column names dont match
      missing_df2 <- setdiff(names(df1), names(df2))
      if(length(missing_df2) > 0){
        warning(paste("The following columns are found in df1 but not df2: \n", paste(unlist(missing_df2), collapse = ",")))
      }
      missing_df1 <- setdiff(names(df2), names(df1))
      if(length(missing_df1) > 0){
        warning(paste("The following columns are found in df2 but not df1: \n", paste(unlist(missing_df1), collapse = ",")))
      }
      message("Adding missing columns...")
    }
    
    # If 2 data frames are supplied, make sure that DF1 is HT and DF2 is 3K in parquet format
    if(length(unique(df1$Panel)) > 1 | !(df1 |> select(Panel) |> distinct() |> na.omit() |> pull() %in% "Explore_HT")) {
      stop("When supplying two data frames, df1 must contain Explore HT data.")
    }
    
    if(!all(df2 |> select(Panel) |> distinct() |> na.omit() |> pull() %in% 
            c("Cardiometabolic", "Cardiometabolic_II",
              "Inflammation", "Inflammation_II",
              "Neurology", "Neurology_II",
              "Oncology", "Oncology_II"))) {
      stop("When supplying two data frames, df2 must contain Explore 3072 data in parquet format.")
    }
    if(!"Count" %in% colnames(df2)) {
      stop("When supplying two data frames, df2 must contain Explore 3072 data in parquet format.")
    }
    
    # If all criteria passed, add assay mapping with df1 HT and df2 3K
    df1 <- df1 |> 
      left_join(map_oid |> select(-olink_id_3k), 
                by = c("OlinkID" = "olink_id_ht"),
                relationship = "many-to-many")
    
    df2 <- df2 |> 
      left_join(map_oid |> select(-olink_id_ht), 
                by = c("OlinkID" = "olink_id_3k"),
                relationship = "many-to-many")
    
  }
  
  #Filtering on valid OlinkID
  df1 <- df1 %>%
    dplyr::filter(stringr::str_detect(OlinkID,
                                      "OID[0-9]{5}"))
  
  if(!is.null(df2)){
    
    df2 <- df2 %>%
      dplyr::filter(stringr::str_detect(OlinkID,
                                        "OID[0-9]{5}"))
  }
  
  
  ####
  
  #median of difference flag
  MOD_FLAG <- TRUE
  
  if(!missing(overlapping_samples_df1)){
    
    if(!all(overlapping_samples_df1 %in% df1$SampleID)){
      
      missing.sample.ids <- setdiff(overlapping_samples_df1,df1$SampleID)
      stop(paste0("SampleID's not found in df1: ",paste(missing.sample.ids,collapse=", ")))
      
    }
    
    if(is.null(overlapping_samples_df2)){
      
      #Testing for reference median normalization, working only on df1
      if(!is.null(reference_medians)){
        
        message('Reference median normalization will be performed.')
        
        
        if(any((unique(reference_medians$OlinkID)) %in% unique(map_oid$olink_id_3k))) {
      
          stop("reference_medians must be Explore HT data.")
          
        }
        
        reference_medians <- reference_medians |> 
          left_join(map_oid |> select(-olink_id_3k), 
                    by = c("OlinkID" = "olink_id_ht"),
                    relationship = "many-to-many")
        
        shared_oids <- intersect(df1$OlinkID_HT_3K, reference_medians$OlinkID_HT_3K)
        
        missing_df1_oid_HT_3K <- setdiff(df1$OlinkID_HT_3K, shared_oids)
        missing_df1_oid_HT_3K <- missing_df1_oid_HT_3K[!is.na(missing_df1_oid_HT_3K)]
        
        if(length(missing_df1_oid_HT_3K) > 0){
          
          missing_df1_oid_3K <- as.data.frame(missing_df1_oid_HT_3K) |>
            left_join(map_oid, by = c("missing_df1_oid_HT_3K" = "OlinkID_HT_3K")) |>
            pull(olink_id_3k)
          
          warning(paste0("There are no reference medians for these assays in df1: ",
                         paste(olink_id_3k,collapse=", "),
                         '. They will not be normalized.'))
          
        }
        
        
        missing_ref_oid_HT_3K <- setdiff(reference_medians$OlinkID_HT_3K, shared_oids)
        missing_ref_oid_HT_3K <- missing_ref_oid_HT_3K[!is.na(missing_ref_oid_HT_3K)]
        
        
        if(length(missing_ref_oid_HT_3K) > 0){
          
          missing_ref_oid_HT <- as.data.frame(missing_ref_oid_HT_3K) |>
            left_join(map_oid, by = c("missing_ref_oid_HT_3K" = "OlinkID_HT_3K")) |>
            pull(olink_id_ht)
          
          warning(paste0("These OlinkIDs from the reference_medians are not contained in df1: ",
                         paste(missing_ref_oid_HT,collapse=", "),
                         '. They will not be used for normalization.'))
        }
        
        
        adj_factor_df <- df1 %>%
          dplyr::filter(SampleID %in% overlapping_samples_df1) %>%
          dplyr::left_join(reference_medians, by= c('OlinkID_HT_3K')) %>%
          dplyr::group_by(OlinkID_HT_3K) %>%
          dplyr::mutate(Assay_Median=median(NPX, na.rm = TRUE)) %>%
          ungroup() %>%
          dplyr::mutate(Adj_factor = dplyr::if_else(is.na(Reference_NPX - Assay_Median),
                                                    0,
                                                    Reference_NPX - Assay_Median)) %>%
          dplyr::select(OlinkID_HT_3K, Adj_factor) %>%
          dplyr::distinct()
        
        
        df_adjusted_data <- df1 %>%
          dplyr::mutate(Panel=stringr::str_replace(Panel,'\\(.+', '')) %>%
          dplyr::left_join(adj_factor_df,by='OlinkID_HT_3K') %>%
          dplyr::mutate(MedianAdjustedNPX = NPX + Adj_factor) %>%
          select(-OlinkID_HT_3K)
        
        # Ignoring LOD in 3K-HT bridging (for now)
        # if("LOD" %in% names(df_adjusted_data)){
        #   df_adjusted_data <- df_adjusted_data %>%
        #     dplyr::mutate(LOD = LOD + Adj_factor)
        # }
        # 
        # # For Alt LOD Names
        # alt_plate_lods <- c("Plate LOD", "PlateLOD", "plateLOD", "Plate_LOD")
        # alt_max_lods <- c("Max LOD", "MaxLOD", "maxLOD", "Max_LOD")
        # 
        # if(any(alt_max_lods %in% names(df_adjusted_data))){
        #   df_adjusted_data <- df_adjusted_data %>%
        #     dplyr::mutate(dplyr::across(any_of(alt_max_lods), ~ .x + Adj_factor))
        # }
        # 
        # if(any(alt_plate_lods %in% names(df_adjusted_data))){
        #   df_adjusted_data <- df_adjusted_data %>%
        #     dplyr::mutate(dplyr::across(any_of(alt_plate_lods), ~ .x + Adj_factor))
        # }
        
        return(df_adjusted_data)
        
      }
      
      #MOD bridge normalization
      if(!all(overlapping_samples_df1 %in% df2$SampleID)){
        
        missing.sample.ids <- setdiff(overlapping_samples_df1,df2$SampleID)
        stop(paste0("SampleID's not found in df2: ",paste(missing.sample.ids,collapse=", ")))
      }
      
    }else{
      
      MOD_FLAG <- FALSE
      
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
  
  # cols_excluded_assay <- c("QC_Warning", "Normalization", "Assay_Warning")
  # if (any(df1$QC_Warning == "EXCLUDED")) {
  #   stop("df1 contains excluded assays (QC_Warning == \"EXCLUDED\"). Please remove them to proceed!")
  # } else if (any(df2$QC_Warning == "EXCLUDED")) {
  #   stop("df2 contains excluded assays (QC_Warning == \"EXCLUDED\"). Please remove them to proceed!")
  # }
  
  # check if both df1 and df2 were normalized with the same method
  # "Intensity" or "Plate control"
  if (!("Normalization" %in% colnames(df1)) || !("Normalization" %in% colnames(df2))) {
    if (!("Normalization" %in% colnames(df1)) && !("Normalization" %in% colnames(df2))) {
      warning("Variable \"Normalization\" not present in df1 and df2")
    } else if (!("Normalization" %in% colnames(df1))) {
      warning("Variable \"Normalization\" not present in df1.")
    } else if (!("Normalization" %in% colnames(df2))) {
      warning("Variable \"Normalization\" not present in df2.")
    }
  } else if (("Normalization" %in% colnames(df1)) && ("Normalization" %in% colnames(df2))) {
    df1_norm <- df1 |>
      # ignore excluded assays
      dplyr::filter(Normalization != "EXCLUDED") |>
      dplyr::select(OlinkID_HT_3K, Normalization_df1 = Normalization) |>
      dplyr::distinct()
    df2_norm <- df2 |>
      # ignore excluded assays
      dplyr::filter(Normalization != "EXCLUDED") |>
      dplyr::select(OlinkID_HT_3K, Normalization_df2 = Normalization) |>
      dplyr::distinct()
    df_norm_diff <- df1_norm |>
      dplyr::inner_join(df2_norm,
                        by = 'OlinkID_HT_3K') |>
      dplyr::filter(Normalization_df1 != Normalization_df2)
    rm(df1_norm, df2_norm)
    
    if (nrow(df_norm_diff) > 0 && nrow(df_norm_diff) <= 10) {
      if (length(df_norm_diff$OlinkID_HT_3K) == 1) {
        warn_msg <- paste("Assay",
                          df_norm_diff$OlinkID_HT_3K,
                          "is not normalized with the same approach. Consider renormalizing.")
      } else if (length(df_norm_diff$OlinkID) == 2) {
        warn_msg <- paste("Assays",
                          paste(df_norm_diff$OlinkID_HT_3K, collapse = " and "),
                          "are not normalized with the same approach. Consider renormalizing.")
      } else {
        warn_msg <- paste("Assays",
                          paste(head(df_norm_diff$OlinkID_HT_3K, -1), collapse = ", "),
                          "and",
                          tail(df_norm_diff$OlinkID_HT_3K, 1),
                          "are not normalized with the same approach. Consider renormalizing.")
      }
      warning(warn_msg)
    } else if (nrow(df_norm_diff) > 10) {
      warning(paste("There are",
                    nrow(df_norm_diff),
                    "assays not normalized with the same approach. Consider renormalizing."))
    }
    rm(df_norm_diff)
  }
  
  if(MOD_FLAG){
    
    message('Bridging normalization with overlapping samples will be performed.')
    
  }else{
    
    message('Bridging normalization with subset normalization will be performed.')
    
  }
  
  df1<-df1 %>%
    dplyr::mutate(Project = df1_project_nr)
  
  df2<-df2 %>%
    dplyr::mutate(Project = df2_project_nr)
  
  if(MOD_FLAG){
    
    #Median of differences
    
    #Calculate adjustment factors
    adj_factor_df<-df1 %>%
      dplyr::bind_rows(df2) %>%
      dplyr::filter(SampleID %in% overlapping_samples_df1) %>%
      dplyr::select(SampleID,OlinkID_HT_3K, NPX, Project) %>%
      tidyr::spread(Project,NPX)
    
    if (reference_project == df1_project_nr) {
      adj_factor_df <- adj_factor_df %>%
        dplyr::mutate(Diff = !!rlang::ensym(df1_project_nr) - !!rlang::ensym(df2_project_nr))
    } else {
      adj_factor_df <- adj_factor_df %>%
        dplyr::mutate(Diff = !!rlang::ensym(df2_project_nr) - !!rlang::ensym(df1_project_nr))
    }
    
    adj_factor_df <- adj_factor_df %>%
      dplyr::group_by(OlinkID_HT_3K) %>%
      dplyr::summarise(Adj_factor = dplyr::if_else(is.na(median(Diff, na.rm = TRUE)),
                                                   0,
                                                   median(Diff, na.rm = TRUE)))
    
  }else{
    
    #Difference of medians
    
    #Calculate adjustment factors
    adj_factor_df <- df1 %>%
      dplyr::filter(SampleID %in% overlapping_samples_df1) %>%
      dplyr::bind_rows(df2 %>%
                         dplyr::filter(SampleID %in% overlapping_samples_df2)) %>%
      dplyr::select(SampleID,OlinkID_HT_3K,UniProt,NPX,Project) %>%
      dplyr::group_by(Project, OlinkID_HT_3K) %>%
      dplyr::summarise(Median=median(NPX,na.rm = TRUE)) %>%
      ungroup() %>%
      tidyr::spread(Project,Median)
    
    if (reference_project == df1_project_nr) {
      adj_factor_df <- adj_factor_df %>%
        dplyr::mutate(Adj_factor = dplyr::if_else(is.na(!!rlang::ensym(df1_project_nr) - !!rlang::ensym(df2_project_nr)),
                                                  0,
                                                  !!rlang::ensym(df1_project_nr) - !!rlang::ensym(df2_project_nr)))
    } else {
      adj_factor_df <- adj_factor_df %>%
        dplyr::mutate(Adj_factor = dplyr::if_else(is.na(!!rlang::ensym(df2_project_nr) - !!rlang::ensym(df1_project_nr)),
                                                  0,
                                                  !!rlang::ensym(df2_project_nr) - !!rlang::ensym(df1_project_nr)))
    }
    
    adj_factor_df <- adj_factor_df %>%
      dplyr::select(OlinkID_HT_3K, Adj_factor)
  }
  
  
  #Apply adjustment factors
  df_adjusted_data<-df1 %>%
    dplyr::bind_rows(df2) %>%
    # dplyr::mutate(Panel=stringr::str_replace(Panel,'\\(.+', '')) %>%
    dplyr::left_join(adj_factor_df,by='OlinkID_HT_3K') %>%
    dplyr::mutate(Adj_factor = dplyr::if_else(Project == reference_project,0,Adj_factor)) %>%
    dplyr::mutate(MedianNormalizedNPX = NPX + Adj_factor) %>%
    select(-OlinkID_HT_3K)
  
  # Ignoring LOD in 3K-HT bridging (for now)
  # if("LOD" %in% names(df_adjusted_data)){
  #   df_adjusted_data <- df_adjusted_data %>%
  #     dplyr::mutate(LOD = LOD + Adj_factor) 
  
  # }
  # # For Alt LOD Names
  # alt_plate_lods <- c("Plate LOD", "PlateLOD", "plateLOD", "Plate_LOD")
  # alt_max_lods <- c("Max LOD", "MaxLOD", "maxLOD", "Max_LOD")
  # 
  # if(any(alt_max_lods %in% names(df_adjusted_data))){
  #   df_adjusted_data <- df_adjusted_data %>%
  #     dplyr::mutate(dplyr::across(any_of(alt_max_lods), ~ .x + Adj_factor)) 
  # }
  # 
  # if(any(alt_plate_lods %in% names(df_adjusted_data))){
  #   df_adjusted_data <- df_adjusted_data %>%
  #     dplyr::mutate(dplyr::across(any_of(alt_plate_lods), ~ .x + Adj_factor))
  # }
  
  
  return(df_adjusted_data)
  
}
