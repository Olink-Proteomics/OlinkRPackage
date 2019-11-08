#'Function which performs an ANOVA per protein
#'
#'Performs an ANOVA test on every protein (by OlinkID) for a given grouping variable
#'
#' @param df NPX data frame in long format with atleast protein name (Assay), OlinkID, UniProt and a factor with at least 3 levels.
#' @param variable Character value indicating which column should be used as the grouping variable. Needs have  3 or more levels
#' @return A data frame containing the ANOVA results for every protein. 
#' @export
#' @examples
#' \donttest{anova_results <- olink_anova(df = npx.data, variable = 'Group')}

olink_anova <- function(df,variable){

  df[[variable]] <- as.factor(df[[variable]])
  
  #Every sample needs to have a unique level of the factor
  number_of_samples_w_more_than_one_level <- df %>% 
    group_by(SampleID, Index) %>% 
    summarise(n_levels = n_distinct(!!rlang::ensym(variable), na.rm = T)) %>% 
    ungroup() %>% 
    filter(n_levels > 1) %>% 
    nrow(.)
  
  if (number_of_samples_w_more_than_one_level > 0) {
    stop(paste0("There are ", number_of_samples_w_more_than_one_level, 
                " samples that do not have a unique level for your variable. Only one level per sample is allowed."))
  }
  
  #Not testing assays that have all NA:s or all NA:s in one level
  all_nas <- df  %>%
    group_by(OlinkID) %>%
    summarise(n = n(), n_na = sum(is.na(NPX))) %>%
    ungroup() %>%
    filter(n == n_na) %>%
    pull(OlinkID)
  
  
  if(length(all_nas) > 0) {
    
    warning(paste0('The assays ',
                   paste(all_nas, collapse = ', '),
                   ' have only NA:s. They will not be tested.'),
            call. = F)
    
  }
  
  nas_in_level <- df  %>%
    filter(!(OlinkID %in% all_nas)) %>%
    group_by(OlinkID, !!rlang::ensym(variable)) %>%
    summarise(n = n(), n_na = sum(is.na(NPX))) %>%
    ungroup() %>%
    filter(n == n_na) %>%
    distinct(OlinkID) %>%
    pull(OlinkID)
  
  
  if(length(nas_in_level) > 0) {
    
    warning(paste0('The assays ',
                   paste(nas_in_level, collapse = ', '),
                   ' have only NA:s in atleast one level of the factor. They will not be tested.'),
            call. = F)
    
  }
  
  
  p.val<-df %>%
    filter(!(OlinkID %in% all_nas)) %>%
    filter(!(OlinkID %in% nas_in_level)) %>%
    group_by(Assay, OlinkID, UniProt, Panel) %>%
    do(tidy(aov(NPX~!!rlang::ensym(variable),data=.))) %>%
    ungroup() %>%
    filter(term != 'Residuals') %>%
    mutate(Adjusted_pval=p.adjust(p.value,method="fdr")) %>%
    mutate(Threshold  = ifelse(Adjusted_pval<0.05,"Significant","Non-significant")) %>%
    arrange(p.value)

  return(p.val)
}

#'Function which performs an ANOVA posthoc test per protein
#'
#'This function allows you to perform an ANOVA posthoc test per protein (by OlinkID).
#'
#' @param df tibble/data frame in long format with atleast protein name (Assay), OlinkID, and a variable with at least 3 levels.
#' @param variable Variable to performed the ANOVA on.
#' @param olinkid_list Vector of significant proteins (OlinkID)
#' @export
#' @examples
#' \donttest{anova_results <- olink_anova(df, "Group")
#' significant_assays <- anova_results %>% 
#' filter(Threshold == 'Significant') %>%
#' pull(OlinkID)
#' anova_posthoc_results <- olink_anova_posthoc(df, "Group", significant_assay)}

olink_anova_posthoc <- function(df, variable, olinkid_list){
  
  df[[variable]] <- as.factor(df[[variable]])

  anova_posthoc_results <- df %>%
    filter(OlinkID %in% olinkid_list) %>%
    mutate(OlinkID = factor(OlinkID, levels = olinkid_list)) %>%
    group_by(Assay, OlinkID, UniProt, Panel) %>%
    do(tidy(TukeyHSD(aov(NPX~!!rlang::ensym(variable),data=.)))) %>%
    ungroup() %>%
    rename(Adjusted_pval = adj.p.value)
  
  return(anova_posthoc_results)
}
