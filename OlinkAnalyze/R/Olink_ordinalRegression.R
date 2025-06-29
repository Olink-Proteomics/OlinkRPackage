#'Function which A two-way ordinal analysis of variance can address an experimental design with two independent variables, each of which is a factor variable.  The main effect of each independent variable can be tested, as well as the effect of the interaction of the two factors.
#'
#'Performs an ANOVA F-test for each assay (by OlinkID) in every panel using stats::Anova and Type III sum of squares. Dependent variable will be treated as ordered factor.
#'The function handles only factor and/or covariates. \cr\cr
#'Samples that have no variable information or missing factor levels are automatically removed from the analysis (specified in a message if verbose = T).
#'Character columns in the input dataframe are automatically converted to factors (specified in a message if verbose = T).
#'Crossed analysis, i.e. A*B formula notation, is inferred from the variable argument in the following cases: \cr
#'\itemize{
#' \item c('A','B')
#' \item c('A: B')
#' \item c('A: B', 'B') or c('A: B', 'A')
#'}
#'Inference is specified in a message if verbose = T. \cr
#'The formula notation of the final model is specified in a message if verbose = T. \cr\cr
#'Adjusted p-values are calculated by stats::p.adjust according to the Benjamini & Hochberg (1995) method (“fdr”).
#'The threshold is determined by logic evaluation of Adjusted_pval < 0.05. Covariates are not included in the p-value adjustment.
#'
#'
#' @param df NPX or Quantified_value data frame in long format with at least protein name (Assay), OlinkID, UniProt, Panel and a factor with at least 3 levels.
#' @param variable Single character value or character array.
#' Variable(s) to test. If length > 1, the included variable names will be used in crossed analyses .
#' Also takes ':'/'*' notation.
#' @param covariates Single character value or character array. Default: NULL.
#' Covariates to include. Takes ':'/'*' notation. Crossed analysis will not be inferred from main effects.
#' @param return.covariates Logical. Default: False. Returns F-test results for the covariates. Note: Adjusted p-values will be NA for the covariates.
#' @param verbose Logical. Default: True. If information about removed samples, factor conversion and final model formula is to be printed to the console.
#' @return A tibble containing the ANOVA results for every protein.
#' The tibble is arranged by ascending p-values.
#'
#' Columns include:
#' \itemize{
#'  \item{Assay:} "character" Protein symbol
#'  \item{OlinkID:} "character" Olink specific ID
#'  \item{UniProt:} "character" UniProt ID
#'  \item{Panel:} "character" Name of Olink Panel
#'  \item{term:} "character" term in model
#'  \item{statistic:} "numeric" value of the statistic
#'  \item{p.value:} "numeric" nominal p-value
#'  \item{Adjusted_pval:} "numeric" adjusted p-value for the test
#'  \item{Threshold:} "character" if adjusted p-value is significant or not (< 0.05)
#' }
#'
#' @export
#' @examples \donttest{
#' library(dplyr)
#'
#' try({ # May fail if dependencies are not installed.
#' #Two-way Ordinal Regression with CLM.
#' #Results in model NPX~Treatment+Time+Treatment:Time.
#'    ordinalRegression_results <- olink_ordinalRegression(df = npx_data1,
#'                                                        variable="Treatment:Time")
#' })
#' }
#'
#' @importFrom dplyr filter group_by ungroup pull do select arrange mutate
#' @importFrom stringr str_detect
#' @importFrom rstatix convert_as_factor
#' @importFrom utils glob2rx read.table globalVariables

olink_ordinalRegression <- function(df,
                                    variable,
                                    covariates = NULL,
                                    return.covariates=F,
                                    verbose=T){
  # Is Package installed
  if(!requireNamespace("ordinal", quietly = TRUE) ){
    stop("Ordinal Regression requires the ordinal package.
         Please install ordinal before continuing.")
  }

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

    #Check data format
    npxCheck <- npxCheck(df)
    data_type <- npxCheck$data_type #Temporary fix to avoid issues with rlang::ensym downstream

    # Rename duplicate UniProts
    df <- uniprot_replace(df, npxCheck)

    ## Convert outcome to factor
    if (data_type == 'NPX') {
      df$NPX <- factor(df$NPX, ordered = TRUE)
    } else if (data_type == 'Quantified_value') {
      df$Quantified_value <- factor(df$Quantified_value, ordered = TRUE)
    }

    ##Convert character vars to factor
    converted.vars <- NULL
    num.vars <- NULL
    for(i in variable_testers){
      if(is.character(df[[i]])){
        df[[i]] <- factor(df[[i]])
        converted.vars <- c(converted.vars,i)
      }else if(is.numeric(df[[i]])){
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
        dplyr::filter(!(OlinkID %in% npxCheck$all_nas)) %>%
        dplyr::group_by(OlinkID, !!rlang::ensym(effect)) %>%
        dplyr::summarise(n = dplyr::n(), n_na = sum(is.na(!!rlang::ensym(data_type)))) %>%
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
        dplyr::group_by(SampleID) %>%
        dplyr::summarise(n_levels = n_distinct(!!rlang::ensym(effect), na.rm = T)) %>%
        dplyr::ungroup() %>%
        dplyr::filter(n_levels > 1) %>%
        dplyr::summarise(n = dplyr::n()) %>%
        dplyr::pull(n)

      if (number_of_samples_w_more_than_one_level > 0) {
        stop(paste0("There are ",
                    number_of_samples_w_more_than_one_level,
                    " samples that do not have a unique level for the effect ",
                    effect,
                    ". Only one level per sample is allowed."))
      }
    }

    if(!is.null(covariates)){
      formula_string <- paste0(data_type, "~",
                               paste(variable,collapse="*"),
                               "+",
                               paste(covariates, sep = '', collapse = '+'))
    }else{
      formula_string <- paste0(data_type, "~", paste(variable,collapse="*"))
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
      dplyr::filter(!(OlinkID %in% npxCheck$all_nas)) %>%
      dplyr::filter(!(OlinkID %in% nas_in_var)) %>%
      dplyr::group_by(Assay, OlinkID, UniProt, Panel) %>%
      dplyr::mutate(!!data_type := rank(!!rlang::ensym(data_type))) %>%
      rstatix::convert_as_factor(!!rlang::ensym(data_type))%>%
      dplyr::do(generics::tidy(stats::anova(ordinal::clm(as.formula(formula_string),
                                                       data=.,
                                                       threshold = "flexible"
                                                  ),type=3))) %>%
      dplyr::ungroup() %>%
      dplyr::filter(!term %in% c('(Intercept)','Residuals')) %>%
      dplyr::mutate(covariates = term %in% covariate_filter_string) %>%
      dplyr::group_by(covariates) %>%
      dplyr::mutate(Adjusted_pval=p.adjust(p.value,method="fdr")) %>%
      dplyr::mutate(Threshold  = ifelse(Adjusted_pval<0.05,"Significant","Non-significant")) %>%
      dplyr::mutate(Adjusted_pval = ifelse(covariates,NA,Adjusted_pval),
                    Threshold = ifelse(covariates,NA,Threshold)) %>%
      dplyr::ungroup()%>%
      dplyr::select(-covariates) %>%
      dplyr::select(Assay,OlinkID,UniProt,Panel,term,df,statistic,p.value,Adjusted_pval,Threshold) %>%
      dplyr::arrange(p.value)

    if(return.covariates){
      return(p.val)
    } else{
      return(p.val %>% dplyr::filter(!term %in% covariate_filter_string))
    }

  }, warning = function(w) {
    if (grepl(x = w, pattern = glob2rx("*contains implicit NA, consider using*")))
      invokeRestart("muffleWarning")
  })
}

#'Function which performs an posthoc test per protein.
#'
#'Performs a post hoc ANOVA test using emmeans::emmeans with Tukey p-value adjustment per assay (by OlinkID) for each panel at confidence level 0.95.
#'See \code{olink_anova} for details of input notation. \cr\cr
#'The function handles both factor and numerical variables and/or covariates.
#'The posthoc test for a numerical variable compares the difference in means of the ordinal outcome variable (default: NPX) for 1 standard deviation difference in the numerical variable, e.g.
#'mean ordinal NPX at mean(numerical variable) versus mean NPX at mean(numerical variable) + 1*SD(numerical variable).
#'
#' @param df NPX data frame in long format with at least protein name (Assay), OlinkID, UniProt, Panel and a factor with at least 3 levels.
#' @param olinkid_list Character vector of OlinkID's on which to perform post hoc analysis. If not specified, all assays in df are used.
#' @param variable Single character value or character array.
#' Variable(s) to test. If length > 1, the included variable names will be used in crossed analyses .
#' Also takes ':' notation.
#' @param covariates Single character value or character array. Default: NULL.
#' Covariates to include. Takes ':'/'*' notation. Crossed analysis will not be inferred from main effects.
#' @param effect Term on which to perform post-hoc. Character vector. Must be subset of or identical to variable.
#' @param effect_formula (optional) A character vector specifying the names of the predictors over which estimated marginal means are desired as defined in the \code{emmeans} package. May also be a formula. If provided, this will override the \code{effect} argument. See \code{?emmeans::emmeans()} for more information.
#' @param mean_return Boolean. If true, returns the mean of each factor level rather than the difference in means (default). Note that no p-value is returned for mean_return = TRUE and no adjustment is performed.
#' @param post_hoc_padjust_method P-value adjustment method to use for post-hoc comparisons within an assay. Options include \code{tukey}, \code{sidak}, \code{bonferroni} and \code{none}.
#' @param verbose Boolean. Default: True. If information about removed samples, factor conversion and final model formula is to be printed to the console.
#'
#' @return Tibble of posthoc tests for specified effect, arranged by ascending adjusted p-values.
#'
#' #' Columns include:
#' \itemize{
#'  \item{Assay:} "character" Protein symbol
#'  \item{OlinkID:} "character" Olink specific ID
#'  \item{UniProt:} "character" UniProt ID
#'  \item{Panel:} "character" Name of Olink Panel
#'  \item{term:} "character" term in model
#'  \item{contrast:} "character" the groups that were compared
#'  \item{estimate:} "numeric" difference in mean of the ordinal NPX between groups
#'  \item{Adjusted_pval:} "numeric" adjusted p-value for the test
#'  \item{Threshold:} "character" if adjusted p-value is significant or not (< 0.05)
#' }
#'
#' @export
#' @examples \donttest{
#' library(dplyr)
#' #Two-way Ordinal Regression.
#' #Results in model NPX~Treatment*Time.
#' try({ # May not work if dependencies are not installed.
#' ordinalRegression_results <- olink_ordinalRegression(df = npx_data1,
#'                               variable="Treatment:Time")
#'
#' #Filtering out significant and relevant results.
#' significant_assays <- ordinalRegression_results %>%
#'   filter(Threshold == 'Significant' & term == 'Time') %>%
#'   select(OlinkID) %>%
#'   distinct() %>%
#'   pull()
#'
#' #Posthoc test for the model NPX~Treatment*Time,
#' #on the effect Time.
#'
#' #Posthoc
#' ordinalRegression_results_posthoc_results <- olink_ordinalRegression_posthoc(npx_data1,
#'                                                    variable=c("Treatment:Time"),
#'                                                    olinkid_list = significant_assays,
#'                                                    effect = "Time")
#'                                                    })
#'                                                    }
#'
#' @importFrom dplyr filter group_by ungroup pull do select arrange mutate
#' @importFrom stringr str_detect
#' @importFrom rstatix convert_as_factor



olink_ordinalRegression_posthoc <- function(df,
                                            olinkid_list = NULL,
                                            variable,
                                            covariates = NULL,
                                            effect,
                                            effect_formula,
                                            mean_return = FALSE,
                                            post_hoc_padjust_method="tukey",
                                            verbose=T){

  if(!requireNamespace("ordinal", quietly = TRUE) ){
    stop("Ordinal Regression requires the ordinal package.
         Please install ordinal before continuing.")
  }


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

    #Check data format
    npxCheck <- npxCheck(df)
    data_type <- npxCheck$data_type #Temporary fix to avoid issues with rlang::ensym downstream
    # Rename duplicate UniProts
    df <- uniprot_replace(df, npxCheck)

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

    ## Convert outcome to factor
    if (data_type == 'NPX') {
      df$NPX <- factor(df$NPX, ordered = TRUE)
    } else if (data_type == 'Quantified_value') {
      df$Quantified_value <- factor(df$Quantified_value, ordered = TRUE)
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
      formula_string <- paste0(data_type, "~",
                               paste(variable,collapse="*"),
                               "+",
                               paste(covariates, sep = '', collapse = '+'))
    }else{
      formula_string <- paste0(data_type, "~", paste(variable,collapse="*"))
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

    if(!missing(effect_formula)){
      e_form <- as.formula(effect_formula)
    } else if(missing(effect_formula)){
      e_form <- as.formula(paste0("pairwise~", paste(effect,collapse="+")))
    }

    anova_posthoc_results <- df %>%
      dplyr::filter(OlinkID %in% olinkid_list) %>%
      dplyr::filter(!(OlinkID %in% npxCheck$all_nas)) %>% #Exclude assays where all samples have NPX=NA
      dplyr::mutate(OlinkID = factor(OlinkID, levels = olinkid_list)) %>%
      dplyr::group_by(Assay, OlinkID, UniProt, Panel) %>%
      dplyr::mutate(!!data_type := rank(!!rlang::ensym(data_type)))%>%
      rstatix::convert_as_factor(!!rlang::ensym(data_type))%>%
      dplyr::do(data.frame(emmeans::emmeans(ordinal::clm(as.formula(formula_string),data=.),
                                                    specs=e_form,
                                                    cov.reduce = function(x) round(c(mean(x),mean(x)+sd(x)),4),
                                   infer=c(TRUE,TRUE),
                                   adjust=post_hoc_padjust_method)[[c("contrasts","emmeans")[1+as.numeric(mean_return)]]],
                           stringsAsFactors=FALSE)) %>%
      dplyr::ungroup()%>%
      dplyr::mutate(term=paste(effect,collapse=":"))



    anova_posthoc_results <- anova_posthoc_results %>%
      dplyr::rename(Adjusted_pval = p.value) %>%
      dplyr::arrange(Adjusted_pval) %>%
      dplyr::mutate(Threshold = if_else(Adjusted_pval < 0.05,
                                        'Significant',
                                        'Non-significant')) %>%
      dplyr::select(Assay, OlinkID, UniProt, Panel, term,contrast, estimate, Adjusted_pval,Threshold)


    return(anova_posthoc_results)
  }, warning = function(w) {
    if (grepl(x = w, pattern = glob2rx("*contains implicit NA, consider using*")))
      invokeRestart("muffleWarning")
  })
}



