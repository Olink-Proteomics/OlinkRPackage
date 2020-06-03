#'Function which performs a linear mixed model per protein
#'
#'Fits a linear mixed effects model for every protein (by OlinkID) in every panel, using lmerTest::lmer and stats::anova.
#'The function handles both factor and numerical variables and/or covariates. \cr\cr
#'Samples that have no variable information or missing factor levels are automatically removed from the analysis (specified in a messsage if verbose = T).
#'Character columns in the input dataframe are automatically converted to factors (specified in a message if verbose = T). \cr\cr
#'Crossed analysis, i.e. A*B formula notation, is inferred from the variable argument in the following cases: \cr
#'\itemize{ 
#' \item c('A','B')
#' \item c('A:B') 
#' \item c('A:B', 'B') or c('A:B', 'A')
#'}
#'Inference is specified in a message if verbose = T. \cr 
#'For covariates, crossed analyses need to be specified explicity, i.e. two main effects will not be expaned with a c('A','B') notation. Main effects present in the variable takes precedence. \cr 
#'The random variable only takes main effect(s). \cr
#'The formula notation of the final model is specified in a message if verbose = T. \cr\cr
#'Output p-values are adjusted by stats::p.adjust according to the Benjamini-Hochberg method (“fdr”). 
#'Adjusted p-values are logically evaluated towards adjusted p-value<0.05. 
#'
#' @param df NPX data frame in long format with at least protein name (Assay), OlinkID, UniProt, 1-2 variables with at least 2 levels.
#' @param variable Single character value or character array. 
#' Variable(s) to test. If length > 1, the included variable names will be used in crossed analyses .
#' Also takes ':'/'*' notation. 
#' @param outcome Character. The dependent variable. Default: NPX.
#' @param random Single character value or character array.
#' @param covariates Single character value or character array. Default: NULL.
#' Covariates to include. Takes ':'/'*' notation. Crossed analysis will not be inferred from main effects.
#' @param return.covariates Boolean. Default: False. Returns results for the covariates. Note: Adjusted p-values will be NA for the covariates.
#' @param verbose Boolean. Deafult: True. If information about removed samples, factor conversion and final model formula is to be printed to the console. 
#'
#' @return A tibble containing the results of fitting the linear mixed effects model to every protein by OlinkID, ordered by ascending p-value. 
#' @export
#' @examples
#' \donttest{
#' # Results in model NPX~Time*Treatment+(1|Subject)+(1|Site)
#' lmer_results <- olink_lmer(df = npx_df,
#' variable=c("Time", 'Treatment'),
#' random = c('Subject', 'Site'))
#' }
#' @import dplyr stringr tidyr lmerTest

olink_lmer <- function(df,                        
                       variable,                  
                       outcome="NPX",            
                       random,                    
                       covariates = NULL,         
                       return.covariates=F,       
                       verbose=T                  
) {  
  
  if(missing(df) | missing(variable) | missing(random)){
    stop('The df and variable and random arguments need to be specified.')
  }
  
  withCallingHandlers({
    
    #Filtering on valid OlinkID
    df <- df %>%
      filter(stringr::str_detect(OlinkID,
                                 "OID[0-9]{5}"))
    
    #Allow for :/* notation in covariates
    variable <- gsub("\\*",":",variable)
    if(!is.null(covariates)) covariates <- gsub("\\*",":",covariates)
    
    add.main.effects <- NULL
    if(any(grepl(":",covariates))){
      tmp <- unlist(strsplit(covariates,":"))
      add.main.effects <- c(add.main.effects,setdiff(tmp,covariates))
      covariates <- union(covariates,add.main.effects)
    }
    if(any(grepl(":",variable))){
      tmp <- unlist(strsplit(variable,":"))
      add.main.effects <- c(add.main.effects,setdiff(tmp,variable))
      variable <- union(variable,unlist(strsplit(variable,":")))
      variable <- variable[!grepl(":",variable)]
    }
    #If variable is in both variable and covariate, keep it in variable or will get removed from final table
    covariates <- setdiff(covariates,variable)
    add.main.effects <- setdiff(add.main.effects, variable)
    
    #Variables to be checked
    variable_testers <- intersect(c(variable,covariates,random), names(df))
    
    ##Remove rows where variables or covariate is NA (cant include in analysis anyway)
    removed.sampleids <- NULL
    for(i in variable_testers){
      removed.sampleids <- unique(c(removed.sampleids,df$SampleID[is.na(df[[i]])]))
      df <- df[!is.na(df[[i]]),]
    }
    
    #Not testing assays that have all NA:s
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
    
    ##Convert character vars to factor
    converted.vars <- NULL
    num.vars <- NULL
    for(i in variable_testers){
      if(is.character(df[[i]])){
        df[[i]] <- factor(df[[i]])
        converted.vars <- c(converted.vars,i)
      } else if(is.numeric(df[[i]])){
        num.vars <- c(num.vars,i)
      }
    }
    
    
    #Not testing assays that have all NA:s in one level
    #Every sample needs to have a unique level of the factor
    
    nas_in_var <- character(0)
    
    if(!is.null(covariates)){
      factors_in_df <- names(df)[sapply(df, is.factor)] 
      single_fixed_effects <- c(variable,
                                intersect(covariates,
                                          factors_in_df))
    }else{
      single_fixed_effects <- variable
    }
    
    
    for(effect in single_fixed_effects){
      
      current_nas <- df %>%
        filter(!(OlinkID %in% all_nas)) %>%
        group_by(OlinkID, !!rlang::ensym(effect)) %>%
        summarise(n = n(), n_na = sum(is.na(NPX))) %>%
        ungroup() %>%
        filter(n == n_na) %>%
        distinct(OlinkID) %>%
        pull(OlinkID)
      
      if(length(current_nas) > 0) {
        
        nas_in_var <- c(nas_in_var, current_nas)
        
        warning(paste0('The assay(s) ',
                       current_nas,
                       ' has only NA:s in atleast one level of ',
                       effect,
                       '. It will not be tested.'),
                call. = F)
      }
      
      number_of_samples_w_more_than_one_level <- df %>% 
        group_by(SampleID, Index) %>% 
        summarise(n_levels = n_distinct(!!rlang::ensym(effect), na.rm = T)) %>% 
        ungroup() %>% 
        filter(n_levels > 1) %>% 
        nrow(.)
      
      if (number_of_samples_w_more_than_one_level > 0) {
        stop(paste0("There are ", 
                    number_of_samples_w_more_than_one_level, 
                    " samples that do not have a unique level for the effect ",
                    effect, 
                    ". Only one level per sample is allowed."))
      }
      
      
    }
    
    if(!is.null(covariates)){
      formula_string <- paste0(outcome, "~", 
                               paste(variable,collapse="*"),
                               "+", 
                               paste(covariates, sep = '', collapse = '+'),
                               "+",
                               paste(paste0("(1|",random,")"),collapse="+"))
    }else{
      
      formula_string <- paste0(outcome, "~", paste(variable,collapse="*"),
                               "+",
                               paste(paste0("(1|",random,")"),collapse="+"))
    }
    
    #Get factors
    fact.vars <- sapply(variable_testers, function(x) is.factor(df[[x]]))
    fact.vars <- names(fact.vars)[fact.vars]
    
    
    #Print verbose message
    if(verbose){
      if(!is.null(add.main.effects) & length(add.main.effects) > 0){
        message("Missing main effects added to the model formula: ",
                paste(add.main.effects,collapse=", "))
      }
      if(!is.null(removed.sampleids) & length(removed.sampleids) >0){
        message("Samples removed due to missing variable or covariate levels: ",
                paste(removed.sampleids,collapse=", "))
      }
      if(!is.null(converted.vars)){
        message(paste0("Variables and covariates converted from character to factors: ",
                       paste(converted.vars,collapse = ", ")))
      }
      if(!is.null(num.vars)){
        message(paste0("Variables and covariates treated as numeric: ",
                       paste(num.vars,collapse = ", ")))
      }
      message("Linear mixed effects model fit to each assay: ",formula_string)
    }
    
    if(!is.null(covariates) & any(grepl(":", covariates))){
      covariate_filter_string <- covariates[str_detect(covariates, ':')]
      covariate_filter_string <- sub("(.*)\\:(.*)$", "\\2:\\1", covariate_filter_string)
      covariate_filter_string <- c(covariates, covariate_filter_string)
    
    }else{
      covariate_filter_string <- covariates
    }
    
    ##make LMM
    lmer_model<-df %>%
      filter(!(OlinkID %in% all_nas)) %>%
      filter(!(OlinkID %in% nas_in_var)) %>%
      group_by(Assay, OlinkID, UniProt, Panel) %>%
      group_modify(~tidy(anova(single_lmer(data=.x, formula_string = formula_string)))) %>%
      ungroup() %>%
      mutate(covariates = term %in% covariate_filter_string) %>% 
      group_by(covariates) %>% 
      mutate(Adjusted_pval=p.adjust(p.value,method="fdr")) %>%
      mutate(Threshold  = ifelse(Adjusted_pval<0.05,"Significant","Non-significant")) %>%
      mutate(Adjusted_pval = ifelse(covariates,NA,Adjusted_pval),
             Threshold = ifelse(covariates,NA,Threshold)) %>% 
      ungroup() %>% 
      select(-covariates) %>% 
      arrange(p.value)
    
    if(return.covariates){
      return(lmer_model)
    } else{
      return(lmer_model %>% filter(!term%in%covariate_filter_string))
    }
    
  }, warning = function(w) {
    if (grepl(x = w, pattern = glob2rx("*not recognized or transformed: NumDF, DenDF*")) |
        grepl(x = w, pattern = glob2rx("*contains implicit NA, consider using*"))){
      invokeRestart("muffleWarning") 
    }
  })
  
}

single_lmer <- function(data, formula_string){
  
  out.model <- tryCatch(lmerTest::lmer(as.formula(formula_string),
                                       data=data,
                                       REML=F,
                                       control = lmerControl(check.conv.singular = "ignore")),
                        warning = function(w){
                          return(
                            lmerTest::lmer(as.formula(formula_string),
                                           data=data,
                                           REML=F,
                                           control=lmerControl(optimizer = "Nelder_Mead",
                                                               check.conv.singular = "ignore"))
                          )
                          
                        }
  )
  
  
  if(class(out.model)=="lmerModLmerTest"){
    return(out.model)
  } else{
    stop("Convergence issue not caught by single_lmer")
  }
}

#'Function which performs a linear mixed model posthoc per protein.
#'
#'Similar to olink_lmer but performs a post hoc analysis based on a linear mixed model effects model using lmerTest::lmer and emmeans::emmeans on proteins. 
#'See \code{olink_lmer} for details of input notation. \cr\cr
#'The function handles both factor and numerical variables and/or covariates. 
#'Differences in estimated marginal means are calculated for all pairwise levels of a given variable. 
#'Degrees of freedom are estimated using Satterthwaite’s approximation. 
#'The posthoc test for a numerical variable compares the difference in means of the outcome variable (default: NPX) for 1 standard deviation difference in the numerical variable, e.g.
#'mean NPX at mean(numerical variable) versus mean NPX at mean(numerical variable) + 1*SD(numerical variable).
#'The output tibble is arranged by ascending Tukey adjusted p-values.
#'
#' @param df NPX data frame in long format with at least protein name (Assay), OlinkID, UniProt, 1-2 variables with at least 2 levels and subject ID.
#' @param olinkid_list Character vector of OlinkID's on which to perform post hoc analysis. If not specified, all assays in df are used.
#' @param effect Term on which to perform post-hoc. Character vector. Must be subset of or identical to variable.  
#' @param outcome Character. The dependent variable. Default: NPX.
#' @param random Single character value or character array.
#' @param covariates Single character value or character array. Default: NULL.
#' Covariates to include. Takes ':'/'*' notation. Crossed analysis will not be inferred from main effects.
#' @param mean_return Boolean. If true, returns the mean of each factor level rather than the difference in means (default). Note that no p-value is returned for mean_return = T.
#' @param verbose Boolean. Deafult: True. If information about removed samples, factor conversion and final model formula is to be printed to the console. 
#' @param variable Single character value or character array. 
#' Variable(s) to test. If length > 1, the included variable names will be used in crossed analyses .
#' Also takes ':'/'*' notation. 
#'
#' @return A tibble containing the results of the pairwise comparisons between given variable levels for proteins specified in olinkid_list (or full df).
#' @export
#' @examples
#' \donttest{lmer_results <- olink_lmer(df = npx_df,
#' variable=c("Time", 'Treatment'),
#' random = c('Subject'))
#' 
#' assay_list <- lmer_results %>%
#' filter(Threshold == 'Significant' & term == 'Time:Treatment') %>%
#' select(OlinkID) %>%
#' distinct() %>%
#' pull()
#' 
#' results_lmer_posthoc <- local_lmer_posthoc(df = NPX, 
#' olinkid_list = assay_list, 
#' variable=c("Time", 'Treatment'),
#' effect = 'Time:Treatment', 
#' random = 'Subject',
#' verbose = T)
#'}
#' @import dplyr stringr tidyr broom

olink_lmer_posthoc <- function(df,                        
                               variable,                 
                               olinkid_list = NULL,              
                               effect,                   
                               outcome="NPX",             
                               random,                   
                               covariates = NULL,         
                               mean_return=F,             
                               verbose=T                  
){
  
  
  if(missing(df) | missing(variable) | missing(effect) | missing(random)){
    stop('The df, variable, random and effect arguments need to be specified.')
  }
  
  tmp <- unique(unlist(strsplit(effect,":")))
  if(!all(tmp %in% unique(unlist(strsplit(variable,"[\\*:]"))))) { 
    stop("All effect terms must be included in the variable argument.")
  }
  
  withCallingHandlers({
    #Filtering on valid OlinkID
    df <- df %>%
      filter(stringr::str_detect(OlinkID,
                                 "OID[0-9]{5}"))
    
    if(is.null(olinkid_list)){
      olinkid_list <- df %>%
        select(OlinkID) %>%
        distinct() %>%
        pull()
    }
    
    #Allow for :/* notation in covariates
    variable <- gsub("\\*",":",variable)
    if(!is.null(covariates)) covariates <- gsub("\\*",":",covariates)
    
    
    add.main.effects <- NULL
    if(any(grepl(":",covariates))){
      tmp <- unlist(strsplit(covariates,":"))
      add.main.effects <- c(add.main.effects,setdiff(tmp,covariates))
      covariates <- union(covariates,add.main.effects)
    }
    if(any(grepl(":",variable))){
      tmp <- unlist(strsplit(variable,":"))
      add.main.effects <- c(add.main.effects,setdiff(tmp,variable))
      variable <- union(variable,unlist(strsplit(variable,":")))
      variable <- variable[!grepl(":",variable)]
    }
    #If variable is in both variable and covariate, keep it in variable or will get removed from final table
    covariates <- setdiff(covariates,variable)
    add.main.effects <- setdiff(add.main.effects, variable)
    
    variable_testers <- intersect(c(variable,covariates), names(df))
    ##Remove rows where variables or covariate is NA (cant include in analysis anyway)
    removed.sampleids <- NULL
    for(i in variable_testers){
      removed.sampleids <- unique(c(removed.sampleids,df$SampleID[is.na(df[[i]])]))
      df <- df[!is.na(df[[i]]),]
    }
    
    ##Convert character vars to factor
    converted.vars <- NULL
    num.vars <- NULL
    for(i in variable_testers){
      if(is.character(df[[i]])){
        df[[i]] <- factor(df[[i]])
        converted.vars <- c(converted.vars,i)
      } else if(is.numeric(df[[i]])){
        num.vars <- c(num.vars,i)
      }
    }
    
    
    if(!is.null(covariates)){
      formula_string <- paste0(outcome, "~", 
                               paste(variable,collapse="*"),
                               "+", 
                               paste(covariates, sep = '', collapse = '+'),
                               "+",
                               paste(paste0("(1|",random,")"),collapse="+"))
    }else{
      
      formula_string <- paste0(outcome, "~", paste(variable,collapse="*"),
                               "+",
                               paste(paste0("(1|",random,")"),collapse="+"))
    }
    
    #Print verbose message
    if(verbose){
      if(!is.null(add.main.effects) & length(add.main.effects) > 0){
        message("Missing main effects added to the model formula: ",
                paste(add.main.effects,collapse=", "))
      }
      if(!is.null(removed.sampleids) & length(removed.sampleids) >0){
        message("Samples removed due to missing variable or covariate levels: ",
                paste(removed.sampleids,collapse=", "))
      }
      if(!is.null(converted.vars)){
        message(paste0("Variables and covariates converted from character to factors: ",
                       paste(converted.vars,collapse = ", ")))
      }
      if(!is.null(num.vars)){
        message(paste0("Variables and covariates treated as numeric: ",
                       paste(num.vars,collapse = ", ")))
      }
      if(any(variable %in% num.vars)){
        message(paste0("Numeric variables post-hoc performed using Mean and Mean + 1SD: ",
                       paste(num.vars[num.vars%in%variable],collapse = ", ")))
      }
      message(paste("Means estimated for each assay from linear mixed effects model: ",formula_string))
    }
    
    
    
    
    output_df <- df %>%
      filter(OlinkID %in% olinkid_list) %>%
      group_by(Assay, OlinkID, UniProt, Panel) %>%
      group_modify(~single_posthoc(data = .x, 
                                   formula_string=formula_string, 
                                   effect = effect,
                                   mean_return = mean_return)) %>%
      ungroup() %>%
      mutate(term=paste(effect,collapse=":"))  %>%
      select(Assay, OlinkID, UniProt, Panel, term, everything())
    
    return(output_df)
    
  }, warning = function(w) {
    if (grepl(x = w, pattern = glob2rx("*contains implicit NA, consider using*"))) 
      invokeRestart("muffleWarning")
  })
  
}

single_posthoc <- function(data, formula_string, effect, mean_return){
  
  the_model <- emmeans::emmeans(single_lmer(data, formula_string),
                                specs=as.formula(paste0("pairwise~", paste(effect,collapse="+"))),
                                cov.reduce = function(x) round(c(mean(x),mean(x)+sd(x)),4),
                                lmer.df="satterthwaite")
  the_model <- summary(the_model,infer=c(T,T),
                       adjust="tukey")
  
  if(mean_return){
    tmp <- unique(unlist(strsplit(effect,":")))
    return(as_tibble(the_model$emmeans) %>%  
             rename(conf.low=lower.CL,
                    conf.high=upper.CL) %>% 
             select(all_of(c(tmp, "emmean", "conf.low", "conf.high")))
    )
    
  }else{
    return(as_tibble(the_model$contrasts) %>% 
             rename(Adjusted_pval = p.value) %>%
             mutate(Threshold = if_else(Adjusted_pval < 0.05,
                                        'Significant',
                                        'Non-significant')) %>%
             rename(conf.low=lower.CL,
                    conf.high=upper.CL) %>% 
             select(contrast, estimate, conf.low, conf.high, Adjusted_pval,Threshold) %>%
             arrange(Adjusted_pval)
    )
    
  }
  
}


#'Function which performs a point-range plot per protein on a linear mixed model
#'
#'Generates a point-range plot faceted by Assay using ggplot and ggplot2::geom_pointrange based on a linear mixed effects model using lmerTest:lmer and emmeans::emmeans.
#'See \code{olink_lmer} for details of input notation. 
#'
#' @param df NPX data frame in long format with at least protein name (Assay), OlinkID, UniProt, 1-2 variables with at least 2 levels.
#' @param olinkid_list Character vector indicating which proteins (by OlinkID) for which to create figures.
#' @param number_of_proteins_per_plot Number plots to include in the list of point-range plots. Defaults to 6 plots per figure
#' @param variable Single character value or character array. 
#' Variable(s) to test. If length > 1, the included variable names will be used in crossed analyses .
#' Also takes ':'/'*' notation. 
#' @param outcome Character. The dependent variable. Default: NPX.
#' @param random Single character value or character array.
#' @param covariates Single character value or character array. Default: NULL.
#' Covariates to include. Takes ':'/'*' notation. Crossed analysis will not be inferred from main effects.
#' @param x_axis_variable Character. Which main effect to use as x-axis in the plot.
#' @param col_variable Character. If provided, the interaction effect col_variable:x_axis_variable will be plotted with x_axis_variable on the x-axis and col_variable as color. 
#' @param verbose Boolean. Deafult: True. If information about removed samples, factor conversion and final model formula is to be printed to the console. 
#'
#' @return A list of objects of class "ggplot"
#' @export
#' @examples
#' \donttest{lmer_results <- olink_lmer(df = npx_df,
#' variable=c("Time", 'Treatment'),
#' random = c('Subject'))
#' 
#' assay_list <- lmer_results %>%
#' filter(Threshold == 'Significant' & term == 'Time:Treatment') %>%
#' select(OlinkID) %>%
#' distinct() %>%
#' pull()
#' 
#' list_of_pointrange_plots <- olink_lmer_plot(df = npx_df,                               
#' variable=c("Time", 'Treatment'),
#' random = c('Subject'),
#' x_axis_variable = 'Time',                  
#' col_variable = 'Treatment',              
#' verbose=T,
#' olinkid_list = assay_list,
#' number_of_proteins_per_plot = 10)}
#' @import dplyr stringr tidyr broom

olink_lmer_plot <- function(df,                              
                            variable,                        
                            outcome="NPX",                   
                            random,                           
                            olinkid_list = NULL,                    
                            covariates = NULL,                
                            x_axis_variable,                 
                            col_variable = NULL,              
                            number_of_proteins_per_plot = 6,  
                            verbose=F
                            ){
  
  if(missing(df) | missing(variable) | missing(x_axis_variable) | missing(random)){
    stop('The df, variable, random and x_axis_variable arguments need to be specified.')
  }
  
  if(!all(x_axis_variable %in% unique(unlist(strsplit(variable,"[\\*:]"))))) { 
    stop("The x axis variable must be included in the variable argument.")
  }
  
  if(!is.null(col_variable)){
    if(!all(col_variable %in% unique(unlist(strsplit(variable,"[\\*:]"))))){
      stop("The color variable must be included in the variable argument.")
    }
  }
  
  #Filtering on valid OlinkID
  df <- df %>%
    filter(stringr::str_detect(OlinkID,
                               "OID[0-9]{5}"))
  
  if(is.null(olinkid_list)){
    olinkid_list <- df %>%
      select(OlinkID) %>%
      distinct() %>%
      pull()
  }
  
  #Setting up what needs to be plotted
  
  if(is.null(col_variable)){
    
    current_fixed_effect <- x_axis_variable
    color_for_plot <- x_axis_variable
    
  }else{
    
    current_fixed_effect <- paste0(x_axis_variable, ':', col_variable)
    color_for_plot <- col_variable
    
  }
  
  
  lm.means <- olink_lmer_posthoc(df = df,
                                 variable = variable,
                                 random = random,
                                 outcome = outcome,
                                 olinkid_list = olinkid_list,
                                 covariates=covariates,
                                 effect = current_fixed_effect,
                                 mean_return = T,
                                 verbose=verbose) %>%
    mutate(Name_Assay = paste0(Assay,"_",OlinkID)) 
  
  
  #Keep olinkid_list input order
  assay_name_list <- lm.means %>%
    mutate(OlinkID = factor(OlinkID, 
                            levels = olinkid_list)) %>% 
    arrange(OlinkID) %>% 
    pull(Name_Assay) %>% 
    unique()
  
  lm.means <- lm.means %>%
    mutate(Name_Assay = factor(Name_Assay, 
                               levels = assay_name_list))
  
  
  #Setup
  topX <- length(assay_name_list)
  
  protein_index <- seq(from = 1,
                       to = topX,
                       by = number_of_proteins_per_plot)
  
  list_of_plots <- list()
  COUNTER <- 1
  
  #loops
  for (i in c(1:length(protein_index))){
    
    
    from_protein <- protein_index[i]
    to_protein <- NULL
    
    if((protein_index[i] + number_of_proteins_per_plot) > topX){
      to_protein <- topX +1
    }else{
      to_protein <- protein_index[i+1]
    }
    
    assays_for_plotting <- assay_name_list[c(from_protein:(to_protein-1))]
    
    
    lmerplot <- lm.means %>%
      filter(Name_Assay %in% assays_for_plotting) %>%
      ggplot()+
      theme(axis.title.x=element_blank())+
      ylab("NPX")+
      theme(axis.text.x =  element_text(size = 10))+
      geom_pointrange(aes(x = as.factor(!!rlang::ensym(x_axis_variable)),
                          y = emmean,
                          ymin = conf.low,
                          ymax = conf.high,
                          color = as.factor(!!rlang::ensym(color_for_plot))),
                      position = position_dodge(width=0.4), size=0.8)+
      facet_wrap(~ Name_Assay,scales = "free_y")+
      set_plot_theme()+
      labs(x=x_axis_variable,color=color_for_plot)
    
    list_of_plots[[COUNTER]] <- lmerplot
    COUNTER <- COUNTER + 1
  }
  
  return(invisible(list_of_plots))
}