#'Function which performs an ANOVA per protein
#'
#'Performs an ANOVA F-test for each assay (by OlinkID) in every panel using car::Anova and Type III sum of squares.
#'The function handles both factor and numerical variables and/or covariates. \cr\cr
#'Samples that have no variable information or missing factor levels are automatically removed from the analysis (specified in a messsage if verbose = T).
#'Character columns in the input dataframe are automatically converted to factors (specified in a message if verbose = T).
#'Numerical variables are not converted to factors.
#'If a numerical variable is to be used as a factor, this conversion needs to be done on the dataframe before the function call. \cr\cr
#'Crossed analysis, i.e. A*B formula notation, is inferred from the variable argument in the following cases: \cr
#'\itemize{
#' \item c('A','B')
#' \item c('A: B')
#' \item c('A: B', 'B') or c('A: B', 'A')
#'}
#'Inference is specified in a message if verbose = T. \cr
#'For covariates, crossed analyses need to be specified explicity, i.e. two main effects will not be expaned with a c('A','B') notation. Main effects present in the variable takes precedence.
#'The formula notation of the final model is specified in a message if verbose = T. \cr\cr
#'Adjusted p-values are calculated by stats::p.adjust according to the Benjamini & Hochberg (1995) method (“fdr”).
#'The threshold is determined by logic evaluation of Adjusted_pval < 0.05. Covariates are not included in the p-value adjustment.
#'
#'
#' @param df NPX data frame in long format with at least protein name (Assay), OlinkID, UniProt, Panel and a factor with at least 3 levels.
#' @param variable Single character value or character array.
#' Variable(s) to test. If length > 1, the included variable names will be used in crossed analyses .
#' Also takes ':'/'*' notation.
#' @param outcome Character. The dependent variable. Default: NPX.
#' @param covariates Single character value or character array. Default: NULL.
#' Covariates to include. Takes ':'/'*' notation. Crossed analysis will not be inferred from main effects.
#' @param return.covariates Boolean. Deafult: False. Returns F-test results for the covariates. Note: Adjusted p-values will be NA for the covariates.
#' @param verbose Boolean. Default: True. If information about removed samples, factor conversion and final model formula is to be printed to the console.
#'
#' @return A tibble containing the ANOVA results for every protein.
#' The tibble is arranged by ascending p-values.
#' @export
#' @examples
#' \donttest{
#'
#' library(dplyr)
#'
#' npx_df <- npx_data1 %>% filter(!grepl('control',SampleID, ignore.case = TRUE))
#'
#' #One-way ANOVA, no covariates.
#' #Results in a model NPX~Time
#' anova_results <- olink_anova(df = npx_df, variable = "Time")
#'
#' #Two-way ANOVA, one main effect covariate.
#' #Results in model NPX~Treatment*Time+Site.
#' anova_results <- olink_anova(df = npx_df,
#'                              variable=c("Treatment:Time"),
#'                              covariates="Site")
#'
#' #One-way ANOVA, interaction effect covariate.
#' #Results in model NPX~Treatment+Site:Time+Site+Time.
#' anova_results <- olink_anova(df = npx_df,
#'                              variable="Treatment",
#'                              covariates="Site:Time")}
#' @importFrom dplyr n filter group_by summarise ungroup pull n_distinct do select arrange mutate n
#' @importFrom stringr str_detect
#' @importFrom generics tidy
#' @importFrom car Anova
#' @importFrom stats IQR as.formula contr.sum lm median na.omit p.adjust sd t.test var
#' @importFrom utils glob2rx read.table globalVariables


olink_anova <- function(df,
                        variable,
                        outcome="NPX",
                        covariates = NULL,
                        return.covariates=F,
                        verbose=T
){

  if(missing(df) | missing(variable)){
    stop('The df and variable arguments need to be specified.')
  }

  withCallingHandlers({

    #Filtering on valid OlinkID
    df <- df %>%
      dplyr::filter(stringr::str_detect(OlinkID,
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

    #Variables to check
    variable_testers <- intersect(c(variable,covariates), names(df))

    ##Remove rows where variables or covariate is NA (cant include in analysis anyway)
    removed.sampleids <- NULL
    for(i in variable_testers){
      removed.sampleids <- unique(c(removed.sampleids,df$SampleID[is.na(df[[i]])]))
      df <- df[!is.na(df[[i]]),]
    }

    #Not testing assays that have all NA:s
    all_nas <- df  %>%
      dplyr::group_by(OlinkID) %>%
      dplyr::summarise(n = dplyr::n(), n_na = sum(is.na(NPX))) %>%
      dplyr::ungroup() %>%
      dplyr::filter(n == n_na) %>%
      dplyr::pull(OlinkID)

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
        dplyr::filter(!(OlinkID %in% all_nas)) %>%
        dplyr::group_by(OlinkID, !!rlang::ensym(effect)) %>%
        dplyr::summarise(n = dplyr::n(), n_na = sum(is.na(NPX))) %>%
        dplyr::ungroup() %>%
        dplyr::filter(n == n_na) %>%
        dplyr::distinct(OlinkID) %>%
        dplyr::pull(OlinkID)


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
        dplyr::group_by(SampleID, Index) %>%
        dplyr::summarise(n_levels = dplyr::n_distinct(!!rlang::ensym(effect), na.rm = T)) %>%
        dplyr::ungroup() %>%
        dplyr::filter(n_levels > 1) %>%
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
                               paste(covariates, sep = '', collapse = '+'))
    }else{

      formula_string <- paste0(outcome, "~", paste(variable,collapse="*"))
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
      message(paste("ANOVA model fit to each assay: "),formula_string)
    }


    if(!is.null(covariates) & any(grepl(":", covariates))){
      covariate_filter_string <- covariates[str_detect(covariates, ':')]
      covariate_filter_string <- sub("(.*)\\:(.*)$", "\\2:\\1", covariate_filter_string)
      covariate_filter_string <- c(covariates, covariate_filter_string)

    }else{
      covariate_filter_string <- covariates
    }

    p.val <- df %>%
      dplyr::filter(!(OlinkID %in% all_nas)) %>%
      dplyr::filter(!(OlinkID %in% nas_in_var)) %>%
      dplyr::group_by(Assay, OlinkID, UniProt, Panel) %>%
      dplyr::do(generics::tidy(car::Anova(stats::lm(as.formula(formula_string),
                            data=.,
                            contrasts = sapply(fact.vars,function(x) return(contr.sum),
                                               simplify = FALSE)),type=3))) %>%

      dplyr::ungroup() %>%
      dplyr::filter(!term %in% c('(Intercept)','Residuals')) %>%
      dplyr::mutate(covariates = term %in% covariate_filter_string) %>%
      dplyr::group_by(covariates) %>%
      dplyr::mutate(Adjusted_pval = p.adjust(p.value,method="fdr")) %>%
      dplyr::mutate(Threshold  = ifelse(Adjusted_pval<0.05,"Significant","Non-significant")) %>%
      dplyr::mutate(Adjusted_pval = ifelse(covariates,NA,Adjusted_pval),
             Threshold = ifelse(covariates,NA,Threshold)) %>%
      dplyr::ungroup() %>%
      dplyr::select(-covariates) %>%
      dplyr::mutate(meansq=sumsq/df) %>%
      dplyr::select(Assay,OlinkID,UniProt,Panel,term,df,sumsq,
                    meansq,statistic,p.value,Adjusted_pval,Threshold) %>%
      dplyr::arrange(Adjusted_pval)


    if(return.covariates){
      return(p.val)
    } else{
      return(p.val %>%
               dplyr::filter(!term%in%covariate_filter_string))
    }

  }, warning = function(w) {
    if (grepl(x = w, pattern = glob2rx("*contains implicit NA, consider using*")))
      invokeRestart("muffleWarning")
  })
}

#'Function which performs an ANOVA posthoc test per protein.
#'
#'Performs a post hoc ANOVA test using emmeans::emmeans with Tukey p-value adjustment per assay (by OlinkID) for each panel at confidence level 0.95.
#'See \code{olink_anova} for details of input notation. \cr\cr
#'The function handles both factor and numerical variables and/or covariates.
#'The posthoc test for a numerical variable compares the difference in means of the outcome variable (default: NPX) for 1 standard deviation difference in the numerical variable, e.g.
#'mean NPX at mean(numerical variable) versus mean NPX at mean(numerical variable) + 1*SD(numerical variable).
#'
#' @param df NPX data frame in long format with at least protein name (Assay), OlinkID, UniProt, Panel and a factor with at least 3 levels.
#' @param olinkid_list Character vector of OlinkID's on which to perform post hoc analysis. If not specified, all assays in df are used.
#' @param variable Single character value or character array.
#' Variable(s) to test. If length > 1, the included variable names will be used in crossed analyses .
#' Also takes ':' notation.
#' @param covariates Single character value or character array. Default: NULL.
#' Covariates to include. Takes ':'/'*' notation. Crossed analysis will not be inferred from main effects.
#' @param outcome Character. The dependent variable. Default: NPX.
#' @param effect Term on which to perform post-hoc. Character vector. Must be subset of or identical to variable.
#' @param mean_return Boolean. If true, returns the mean of each factor level rather than the difference in means (default). Note that no p-value is returned for mean_return = T and no adjustment is performed.
#' @param verbose Boolean. Default: True. If information about removed samples, factor conversion and final model formula is to be printed to the console.
#'
#' @return Tibble of posthoc tests for specified effect, arranged by ascending adjusted p-values.
#' @export
#' @examples \donttest{
#'
#' library(dplyr)
#' 
#' npx_df <- npx_data1 %>% filter(!grepl('control',SampleID, ignore.case = TRUE))
#'
#' #Two-way ANOVA, one main effect (Site) covariate.
#' #Results in model NPX~Treatment*Time+Site.
#' anova_results <- olink_anova(df = npx_df,
#'                              variable=c("Treatment:Time"),
#'                              covariates="Site")
#'
#' Posthoc test for the model NPX~Treatment*Time+Site, on the interaction effect Treatment:Time with covariate Site.
#'
#' #Filtering out significant and relevant results.
#' significant_assays <- anova_results %>%
#' filter(Threshold == 'Significant' & term == 'Treatment:Time') %>%
#' select(OlinkID) %>%
#' distinct() %>%
#' pull()
#' 
#' #Posthoc
#' anova_posthoc_results <- olink_anova_posthoc(npx_df,
#' variable=c("Treatment:Time"),
#' covariates="Site",
#' olinkid_list = significant_assays,
#' effect = "Treatment:Time")}
#' @importFrom dplyr filter group_by ungroup pull do select arrange mutate
#' @importFrom stringr str_detect


olink_anova_posthoc <- function(df,
                                olinkid_list = NULL,
                                variable,
                                covariates = NULL,
                                outcome="NPX",
                                effect,
                                mean_return=F,
                                verbose=T
){



  if(missing(df) | missing(variable) | missing(effect)){
    stop('The df and variable and effect arguments need to be specified.')
  }

  tmp <- unique(unlist(strsplit(effect,":")))
  if(!all(tmp %in% unique(unlist(strsplit(variable,"[\\*:]"))))) {
    stop("All effect terms must be included in the variable argument.")
  }


  withCallingHandlers({

    #Filtering on valid OlinkID
    df <- df %>%
      dplyr::filter(stringr::str_detect(OlinkID,
                                 "OID[0-9]{5}"))

    if(is.null(olinkid_list)){
      olinkid_list <- df %>%
        dplyr::select(OlinkID) %>%
        dplyr::distinct() %>%
        dplyr::pull()
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
                               paste(covariates, sep = '', collapse = '+'))
    }else{
      formula_string <- paste0(outcome, "~", paste(variable,collapse="*"))
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
      message(paste("Means estimated for each assay from ANOVA model: ",formula_string))
    }

    anova_posthoc_results <- df %>%
      dplyr::filter(OlinkID %in% olinkid_list) %>%
      dplyr::mutate(OlinkID = factor(OlinkID, levels = olinkid_list)) %>%
      dplyr::group_by(Assay, OlinkID, UniProt, Panel) %>%
      dplyr::do(data.frame(emmeans::emmeans(stats::lm(as.formula(formula_string),data=.),
                                    specs=as.formula(paste0("pairwise~", paste(effect,collapse="+"))),
                                    cov.reduce = function(x) round(c(mean(x),mean(x)+sd(x)),4),
                            infer=c(T,T),
                            adjust="tukey")[[c("contrasts","emmeans")[1+as.numeric(mean_return)]]],
                    stringsAsFactors=F)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(term=paste(effect,collapse=":"))  %>%
      dplyr::rename(conf.low=lower.CL,
             conf.high=upper.CL)

    if(mean_return){
      anova_posthoc_results <- anova_posthoc_results %>%
        dplyr::select(all_of(c("Assay", "OlinkID", "UniProt", "Panel", "term",
                               effect, "emmean", "conf.low", "conf.high")))
    } else if(!mean_return){
      anova_posthoc_results <- anova_posthoc_results %>%
        dplyr::rename(Adjusted_pval = p.value) %>%
        dplyr::arrange(Adjusted_pval) %>%
        dplyr::mutate(Threshold = if_else(Adjusted_pval < 0.05,
                                   'Significant',
                                   'Non-significant')) %>%
        dplyr::select(Assay, OlinkID, UniProt, Panel, term,  contrast, estimate,
                      conf.low, conf.high, Adjusted_pval,Threshold)
    }

    return(anova_posthoc_results)

  }, warning = function(w) {
    if (grepl(x = w, pattern = glob2rx("*contains implicit NA, consider using*")))
      invokeRestart("muffleWarning")
  })
}
