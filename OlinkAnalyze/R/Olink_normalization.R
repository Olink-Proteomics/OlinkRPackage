#' Normalization of all proteins (by OlinkID).
#'
#' Normalizes NPX data frames to another data frame or to reference medians. If two dataframes are normalized to one another, Olink's default is using the older dataframe as reference.
#' The function handles three different types of normalization: \cr\cr
#' Bridging normalization: One of the dataframes is adjusted to another using overlapping samples (bridge samples).
#' The overlapping samples need to be named the same between the dataframes and adjustment is made using the median of the paired differences between the bridge samples in the two data frames.
#' The two dataframes are inputs df1 and df2, the one being adjusted to is specified in the input reference_project and the overlapping samples are specified in overlapping_samples_df1.
#' Only overlapping_samples_df1 should be input, no matter which dataframe is used as reference_project.  \cr\cr
#' Subset normalization: One of the dataframes is adjusted to another dataframe
#' using a sample subset. Adjustment is made using the differences in median
#' between the subsets from the two dataframes. Both overlapping_samples_df1 and
#' overlapping_samples_df2 need to be input. The samples do not need to be
#' named the same. \cr
#' A special case of subset normalization are to use all samples (except control
#' samples and samples with QC warning) from df1 as input in overlapping_samples_df1
#' and all samples from df2 as input in overlapping_samples_df2.  \cr\cr
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
#' @return A "tibble" of NPX data in long format containing normalized NPX values, including adjustment factors.
#' Columns include same as df1/df2 with additional column Adj_factor which includes the adjustment factor in the normalization.
#' @keywords Normalization
#' @export
#' @examples
#' \donttest{
#'
#' library(dplyr)
#'
#' npx_df1 <- npx_data1 %>% dplyr::mutate(Project = 'P1')
#' npx_df2 <- npx_data2 %>% dplyr::mutate(Project = 'P2')
#'
#' #Bridging normalization:
#' # Find overlapping samples, but exclude Olink control
#' overlap_samples <- intersect((npx_df1 %>%
#'                                dplyr::filter(!grepl("control", SampleID,
#'                                                      ignore.case=TRUE)))$SampleID,
#'                              (npx_df2 %>%
#'                                dplyr::filter(!grepl("control", SampleID,
#'                                                      ignore.case=TRUE)))$SampleID)
#' # Normalize
#' olink_normalization(df1 = npx_df1,
#'                     df2 = npx_df2,
#'                     overlapping_samples_df1 = overlap_samples,
#'                     df1_project_nr = 'P1',
#'                     df2_project_nr = 'P2',
#'                     reference_project = 'P1')
#'
#' #Subset normalization:
#' # Find a suitable subset of samples from both projects, but exclude Olink controls
#' # and samples which do not pass QC.
#' df1_sampleIDs <- npx_df1 %>%
#'     dplyr::filter(QC_Warning == 'Pass') %>%
#'     dplyr::filter(!stringr::str_detect(SampleID, 'CONTROL_SAMPLE')) %>%
#'     dplyr::select(SampleID) %>%
#'     unique() %>%
#'     dplyr::pull(SampleID)
#' df2_sampleIDs <- npx_df2 %>%
#'     dplyr::filter(QC_Warning == 'Pass') %>%
#'     dplyr::filter(!stringr::str_detect(SampleID, 'CONTROL_SAMPLE')) %>%
#'     dplyr::select(SampleID) %>%
#'     unique() %>%
#'     dplyr::pull(SampleID)
#' some_samples_df1 <- sample(df1_sampleIDs, 16)
#' some_samples_df2 <- sample(df2_sampleIDs, 16)
#'
#' olink_normalization(df1 = npx_df1,
#'                     df2 = npx_df2,
#'                     overlapping_samples_df1 = some_samples_df1,
#'                     overlapping_samples_df2 = some_samples_df2)
#'
#'
#' ## Special case of subset normalization when using all samples.
#' olink_normalization(df1 = npx_df1,
#'                     df2 = npx_df2,
#'                     overlapping_samples_df1 = df1_sampleIDs,
#'                     overlapping_samples_df2 = df2_sampleIDs)
#'
#'
#' #Reference median normalization:
#' # For the sake of this example, set the reference median to 1
#' ref_median_df <- npx_df1 %>%
#'     dplyr::select(OlinkID) %>%
#'     dplyr::distinct() %>%
#'     dplyr::mutate(Reference_NPX = 1)
#' # Normalize
#' olink_normalization(df1 = npx_df1,
#'                     overlapping_samples_df1 = some_samples_df1,
#'                     reference_medians = ref_median_df)
#' }
#' @importFrom magrittr %>%
#' @importFrom stringr str_detect str_replace
#' @importFrom dplyr filter left_join group_by mutate select distinct summarise if_else bind_rows
#' @importFrom tidyr spread
#' @importFrom rlang ensym

olink_normalization <- function(df1,
                                df2 = NULL,
                                overlapping_samples_df1,
                                overlapping_samples_df2 = NULL,
                                df1_project_nr = 'P1',
                                df2_project_nr = 'P2',
                                reference_project = 'P1',
                                reference_medians = NULL) {

  # If 2 data frames are supplied, make sure they have the same column names, if not, add missing column names.
  
  if(!is.null(df2)){ # If df2 is provided
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
          dplyr::filter(SampleID %in% overlapping_samples_df1) %>%
          dplyr::left_join(reference_medians, by= c('OlinkID')) %>%
          dplyr::group_by(OlinkID) %>%
          dplyr::mutate(Assay_Median=median(NPX, na.rm = TRUE)) %>%
          ungroup() %>%
          dplyr::mutate(Adj_factor = dplyr::if_else(is.na(Reference_NPX - Assay_Median),
                                                    0,
                                                    Reference_NPX - Assay_Median)) %>%
          dplyr::select(OlinkID, Adj_factor) %>%
          dplyr::distinct()


        df_adjusted_data <- df1 %>%
          dplyr::mutate(Panel=stringr::str_replace(Panel,'\\(.+', '')) %>%
          dplyr::left_join(adj_factor_df,by='OlinkID') %>%
          dplyr::mutate(NPX = NPX + Adj_factor) %>%
          dplyr::mutate(LOD = LOD + Adj_factor)

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

  # check if both df1 and df2 were normalized with the same method
  # "Intensity" or "Plate control"
  if (!("Normalization" %in% colnames(df1)) || !("Normalization" %in% colnames(df2))) {
    if (!("Normalization" %in% colnames(df1)) && !("Normalization" %in% colnames(df2))) {
      warning("Variable \"Normalization\" not present in df1 and df2")
    } else if (!("Normalization" %in% colnames(df1))) {
      warning("Variable \"Normalization\" not present in df1. Removing column from df2.")
      df2 <- dplyr::select(df2, -Normalization)
    } else if (!("Normalization" %in% colnames(df2))) {
      warning("Variable \"Normalization\" not present in df2. Removing column from df1.")
      df1 <- dplyr::select(df1, -Normalization)
    }
  } else if (("Normalization" %in% colnames(df1)) && ("Normalization" %in% colnames(df2))) {
    if ({ df1$Normalization |> unique() |> length() } != 1 ||
        { df2$Normalization |> unique() |> length() } != 1 ||
        unique(df1$Normalization) != unique(df2$Normalization)) {
      warning("df1 and df2 are not normalized with the same approach. Consider renormalizing.")
    }
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
      dplyr::select(SampleID,OlinkID,UniProt,NPX,Project) %>%
      tidyr::spread(Project,NPX)

    if (reference_project == df1_project_nr) {
      adj_factor_df <- adj_factor_df %>%
        dplyr::mutate(Diff = !!rlang::ensym(df1_project_nr) - !!rlang::ensym(df2_project_nr))
    } else {
      adj_factor_df <- adj_factor_df %>%
        dplyr::mutate(Diff = !!rlang::ensym(df2_project_nr) - !!rlang::ensym(df1_project_nr))
    }

    adj_factor_df <- adj_factor_df %>%
      dplyr::group_by(OlinkID) %>%
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
      dplyr::select(SampleID,OlinkID,UniProt,NPX,Project) %>%
      dplyr::group_by(Project, OlinkID) %>%
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
      dplyr::select(OlinkID, Adj_factor)
  }


  #Apply adjustment factors
  df_adjusted_data<-df1 %>%
    dplyr::bind_rows(df2) %>%
    dplyr::mutate(Panel=stringr::str_replace(Panel,'\\(.+', '')) %>%
    dplyr::left_join(adj_factor_df,by='OlinkID') %>%
    dplyr::mutate(Adj_factor = dplyr::if_else(Project == reference_project,0,Adj_factor)) %>%
    dplyr::mutate(NPX = NPX + Adj_factor) %>%
    dplyr::mutate(LOD = LOD + Adj_factor)


  return(df_adjusted_data)

}
