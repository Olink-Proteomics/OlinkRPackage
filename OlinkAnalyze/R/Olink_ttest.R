#'Function which performs a t-test per protein
#'
#'Performs a 2-sample t-test on for every protein (by OlinkID) for a given grouping variable.
#'
#' @param df NPX data frame in long format with at least protein name (Assay), OlinkID, UniProt and a factor with 2 levels.
#' @param variable Character value indicating which column should be used as the grouping variable. Needs to have exactly 2 levels
#' @param ... Options to be passed to t.test. See ?t.test for more.
#' @return A data frame containing the t-test results for every protein.
#' @export
#' @examples
#' \donttest{ttest_results <- olink_ttest(df=npx.data, variable = 'Group', alternative = 'two.sided')}
#' @import dplyr stringr tidyr broom


olink_ttest <- function(df, variable, ...){
  
  df[[variable]] <- as.factor(df[[variable]])
  var_levels <- levels(df[[variable]])
  number_of_levels <- length(var_levels)
  
  if(!(number_of_levels == 2)){
    
    stop(paste0('The number of levels in the factor need to be 2. Your factor has ', number_of_levels, 
                ' levels.'))
    
  }
  
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
    pull(OlinkID)
  
  
  if(length(nas_in_level) > 0) {
    
    warning(paste0('The assays ',
                   paste(nas_in_level, collapse = ', '),
                   ' have only NA:s in one level of the factor. They will not be tested.'),
            call. = F)
    
  }
  
  
  p.val <- df %>%
    filter(!(OlinkID %in% all_nas)) %>%
    filter(!(OlinkID %in% nas_in_level)) %>%
    group_by(Assay, OlinkID, UniProt, Panel) %>%
    do(tidy(t.test(NPX ~ !!rlang::ensym(variable), data = ., ...))) %>%
    ungroup() %>%
    mutate(Adjusted_pval = p.adjust(p.value, method = "fdr")) %>%
    mutate(Threshold = ifelse(Adjusted_pval < 0.05, "Significant", "Non-significant")) %>%
    rename(`:=`(!!var_levels[1], estimate1)) %>%
    rename(`:=`(!!var_levels[2], estimate2)) %>%
    arrange(p.value)
  
  return(p.val)
  
}
