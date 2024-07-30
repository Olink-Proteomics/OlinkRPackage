#'Function which performs an ANOVA per protein
#'
#'Performs an ANOVA F-test for each assay (by OlinkID) in every panel using car::Anova and Type III sum of squares.
#'The function handles both factor and numerical variables and/or covariates. \cr\cr
#'Samples that have no variable information or missing factor levels are automatically removed from the analysis (specified in a message if verbose = TRUE).
#'Character columns in the input dataframe are automatically converted to factors (specified in a message if verbose = TRUE).
#'Numerical variables are not converted to factors.
#'Control samples (SampleType is not "SAMPLE, or SampleID contains "control" or "ctrl") should be removed before using this function.
#'Control assays (AssayType is not "assay", or Assay contains "control" or "ctrl") should be removed before using this function.
#'If a numerical variable is to be used as a factor, this conversion needs to be done on the dataframe before the function call. \cr\cr
#'Crossed analysis, i.e. A*B formula notation, is inferred from the variable argument in the following cases: \cr
#'\itemize{
#' \item c('A','B')
#' \item c('A: B')
#' \item c('A: B', 'B') or c('A: B', 'A')
#'}
#'Inference is specified in a message if verbose = TRUE. \cr
#'For covariates, crossed analyses need to be specified explicitly, i.e. two main effects will not be expanded with a c('A','B') notation. Main effects present in the variable takes precedence.
#'The formula notation of the final model is specified in a message if verbose = TRUE. \cr\cr
#'Adjusted p-values are calculated by stats::p.adjust according to the Benjamini & Hochberg (1995) method (“fdr”).
#'The threshold is determined by logic evaluation of Adjusted_pval < 0.05. Covariates are not included in the p-value adjustment.
#'
#'
#' @param df NPX data frame in long format with at least protein name (Assay), OlinkID, UniProt, Panel and a factor with at least 3 levels.
#' @param variable Single character value or character array.
#' Variable(s) to test. If length > 1, the included variable names will be used in crossed analyses .
#' Also takes ':' or '*' notation.
#' @param outcome Character. The dependent variable. Default: NPX.
#' @param covariates Single character value or character array. Default: NULL.
#' Covariates to include. Takes ':' or '*' notation. Crossed analysis will not be inferred from main effects.
#' @param model_formula (optional) Symbolic description of the model to be fitted in standard formula notation (e.g. "NPX~A*B"). If provided, this will override the \code{outcome}, \code{variable} and \code{covariates} arguments. Can be a string or of class \code{stats::formula()}.
#' @param return.covariates Boolean. Default: False. Returns F-test results for the covariates. Note: Adjusted p-values will be NA for the covariates.
#' @param verbose Boolean. Default: True. If information about removed samples, factor conversion and final model formula is to be printed to the console.
#'
#' @return A "tibble" containing the ANOVA results for every protein. The tibble is arranged by ascending p-values.
#' Columns include:
#'
#' \itemize{
#'  \item{Assay:} "character" Protein symbol
#'  \item{OlinkID:} "character" Olink specific ID
#'  \item{UniProt:} "character" UniProt ID
#'  \item{Panel:} "character" Name of Olink Panel
#'  \item{term:} "character" term in model
#'  \item{df:} "numeric" degrees of freedom
#'  \item{sumsq:} "numeric" sum of square
#'  \item{meansq:} "numeric" mean of square
#'  \item{statistic:} "numeric" value of the statistic
#'  \item{p.value:} "numeric" nominal p-value
#'  \item{Adjusted_pval:} "numeric" adjusted p-value for the test (Benjamini&Hochberg)
#'  \item{Threshold:} "character" if adjusted p-value is significant or not (< 0.05)
#' }
#'
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
                        model_formula,
                        return.covariates = FALSE,
                        verbose = TRUE
){

  if(!missing(model_formula)){
    if("formula" %in% class(model_formula)) model_formula <- deparse(model_formula) #Convert to string if is formula
    tryCatch(as.formula(model_formula),error=function(e) stop(paste0(model_formula," is not a recognized formula."))) #If cannot be coerced into formula, error
    #If variable and covariates were included, message that they will not be used
    if(!missing(variable) | !is.null(covariates)) message("model_formula overriding variable and covariate arguments.")
    #Parse formula so checks on the  variable and outcome objects can continue as usual
    model_formula <- gsub(" ","",model_formula)
    splt_form <- strsplit(model_formula,c("\\+|~|\\*|:"))[[1]]
    if("-1" %in% splt_form) splt_form <- splt_form[-which(splt_form=="-1")]
    outcome <- splt_form[1]
    variable <- splt_form[-1]
    covariates <- NULL
  }

  if(missing(df) | missing(variable)){
    stop('The df and variable arguments need to be specified.')
  }

  # Stop if internal controls (assays) have not been removed
  if ("AssayType" %in% names(df)) {
    if (any(df$AssayType != "assay")) {
      ctrl_assays <- df |>
        dplyr::filter(AssayType != "assay")
      
      stop(paste0('Control assays have not been removed from the dataset.\n  Assays with AssayType != "assay" should be excluded.\n  The following ', length(unique(ctrl_assays$Assay)) ,' control assays were found:\n  ',
                  paste(strwrap(toString(unique(ctrl_assays$Assay)), width = 80), collapse = "\n")))
    }
  } else if (any(stringr::str_detect(df$Assay, stringr::regex("control|ctrl", ignore_case = TRUE)))) {
    ctrl_assays <- df |>
      dplyr::filter(stringr::str_detect(df$Assay, stringr::regex("control|ctrl", ignore_case = TRUE)))
    
    stop(paste0('Control assays have not been removed from the dataset.\n  Assays with "control" in their Assay field should be excluded.\n  The following ', length(unique(ctrl_assays$Assay)) ,' control assays were found:\n  ',
                paste(strwrap(toString(unique(ctrl_assays$Assay)), width = 80), collapse = "\n")))
  }
  
  # Stop if external controls (samples) have not been removed
  if ("SampleType" %in% names(df)) {
    if (any(df$SampleType != "SAMPLE")) {
      ctrl_samples <- df |>
        dplyr::filter(SampleType != "SAMPLE")
      
      stop(paste0(
        'Control samples have not been removed from the dataset.\n  Samples with SampleType != "SAMPLE" should be excluded.\n  The following ', length(unique(ctrl_samples$SampleID)), " control samples were found:\n  ",
        paste(strwrap(toString(unique(ctrl_samples$SampleID)), width = 80), collapse = "\n")
      ))
    }
  } else if (any(stringr::str_detect(df$SampleID, stringr::regex("control|ctrl", ignore_case = TRUE)))) {
    ctrl_samples <- df |>
      dplyr::filter(stringr::str_detect(df$SampleID, stringr::regex("control|ctrl", ignore_case = TRUE)))
    
    stop(paste0(
      'Control samples have not been removed from the dataset.\n  Samples with "control" or "ctrl" in their SampleID field should be excluded.\n  The following ', length(unique(ctrl_samples$SampleID)), " control samples were found:\n  ",
      paste(strwrap(toString(unique(ctrl_samples$SampleID)), width = 80), collapse = "\n")
    ))
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
        dplyr::filter(!(OlinkID %in% npxCheck$all_nas)) %>% #Exclude assays that have all NA:s
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
                call. = FALSE)
      }

      number_of_samples_w_more_than_one_level <- df %>%
        dplyr::group_by(SampleID) %>%
        dplyr::summarise(n_levels = dplyr::n_distinct(!!rlang::ensym(effect), na.rm = TRUE)) %>%
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

    if(missing(model_formula)){
      if(!is.null(covariates)){
        formula_string <- paste0(outcome, "~",
                                 paste(variable,collapse="*"),
                                 "+",
                                 paste(covariates, sep = '', collapse = '+'))
      }else{

        formula_string <- paste0(outcome, "~", paste(variable,collapse="*"))
      }
    } else if(!missing(model_formula)){
      formula_string <- model_formula
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
      covariate_filter_string <- covariates[stringr::str_detect(covariates, ':')]
      covariate_filter_string <- sub("(.*)\\:(.*)$", "\\2:\\1", covariate_filter_string)
      covariate_filter_string <- c(covariates, covariate_filter_string)

    }else{
      covariate_filter_string <- covariates
    }

    p.val <- df %>%
      dplyr::filter(!(OlinkID %in% npxCheck$all_nas)) %>% #Exclude assays that have all NA:s
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
#'Control samples (SampleType is not "SAMPLE, or SampleID contains "control" or "ctrl") should be removed before using this function.
#'Control assays (AssayType is not "assay", or Assay contains "control" or "ctrl") should be removed before using this function.
#'The posthoc test for a numerical variable compares the difference in means of the outcome variable (default: NPX) for 1 standard deviation difference in the numerical variable, e.g.
#'mean NPX at mean(numerical variable) versus mean NPX at mean(numerical variable) + 1*SD(numerical variable).
#'
#' @param df NPX data frame in long format with at least protein name (Assay), OlinkID, UniProt, Panel and a factor with at least 3 levels.
#' @param olinkid_list Character vector of OlinkID's on which to perform post hoc analysis. If not specified, all assays in df are used.
#' @param variable Single character value or character array.
#' Variable(s) to test. If length > 1, the included variable names will be used in crossed analyses .
#' Also takes ':' notation.
#' @param covariates Single character value or character array. Default: NULL. Covariates to include. Takes ':' or '*' notation. Crossed analysis will not be inferred from main effects.
#' @param outcome Character. The dependent variable. Default: NPX.
#' @param model_formula (optional) Symbolic description of the model to be fitted in standard formula notation (e.g. "NPX~A*B"). If provided, this will override the \code{outcome}, \code{variable} and \code{covariates} arguments. Can be a string or of class \code{stats::formula()}.
#' @param effect Term on which to perform post-hoc. Character vector. Must be subset of or identical to variable.
#' @param effect_formula (optional) A character vector specifying the names of the predictors over which estimated marginal means are desired as defined in the \code{emmeans} package. May also be a formula. If provided, this will override the \code{effect} argument. See \code{?emmeans::emmeans()} for more information.
#' @param mean_return Boolean. If true, returns the mean of each factor level rather than the difference in means (default). Note that no p-value is returned for mean_return = TRUE and no adjustment is performed.
#' @param post_hoc_padjust_method P-value adjustment method to use for post-hoc comparisons within an assay. Options include \code{tukey}, \code{sidak}, \code{bonferroni} and \code{none}.
#' @param verbose Boolean. Default: True. If information about removed samples, factor conversion and final model formula is to be printed to the console.
#'
#' @return
#' A "tibble" of posthoc tests for specified effect, arranged by ascending adjusted p-values.
#' Columns include:
#' \itemize{
#'  \item{Assay:} "character" Protein symbol
#'  \item{OlinkID:} "character" Olink specific ID
#'  \item{UniProt:} "character" UniProt ID
#'  \item{Panel:} "character" Name of Olink Panel
#'  \item{term:} "character" term in model
#'  \item{contrast:} "character" the groups that were compared
#'  \item{estimate:} "numeric" difference in mean NPX between groups
#'  \item{conf.low:} "numeric" confidence interval for the mean (lower end)
#'  \item{conf.high:} "numeric" confidence interval for the mean (upper end)
#'  \item{Adjusted_pval:} "numeric" adjusted p-value for the test
#'  \item{Threshold:} "character" if adjusted p-value is significant or not (< 0.05)
#' }
#'
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
#' #Posthoc test for the model NPX~Treatment*Time+Site,
#' #on the interaction effect Treatment:Time with covariate Site.
#'
#' #Filtering out significant and relevant results.
#' significant_assays <- anova_results %>%
#' filter(Threshold == 'Significant' & term == 'Treatment:Time') %>%
#' select(OlinkID) %>%
#' distinct() %>%
#' pull()
#'
#' #Posthoc, all pairwise comparisons
#' anova_posthoc_results <- olink_anova_posthoc(npx_df,
#' variable=c("Treatment:Time"),
#' covariates="Site",
#' olinkid_list = significant_assays,
#' effect = "Treatment:Time")
#'
#'
#' #Posthoc, treated vs untreated at each timepoint, adjusted for Site effect
#' anova_posthoc_results <- olink_anova_posthoc(npx_df,
#' model_formula = "NPX~Treatment*Time+Site",
#' olinkid_list = significant_assays,
#' effect_formula = "pairwise~Treatment|Time")
#'
#'
#' }
#' @importFrom dplyr filter group_by ungroup pull do select arrange mutate
#' @importFrom stringr str_detect
#' @importFrom tidyselect any_of


olink_anova_posthoc <- function(df,
                                olinkid_list = NULL,
                                variable,
                                covariates = NULL,
                                outcome = "NPX",
                                model_formula,
                                effect,
                                effect_formula,
                                mean_return = FALSE,
                                post_hoc_padjust_method="tukey",
                                verbose = TRUE
){

  if(!missing(model_formula)){
    if("formula" %in% class(model_formula)) model_formula <- deparse(model_formula) #Convert to string if is formula
    tryCatch(as.formula(model_formula),error=function(e) stop(paste0(model_formula," is not a recognized formula."))) #If cannot be coerced into formula, error
    #If variable and covariates were included, message that they will not be used
    if(!missing(variable) | !is.null(covariates)) message("model_formula overriding variable and covariate arguments.")
    #Parse formula so checks on the  variable and outcome objects can continue as usual
    model_formula <- gsub(" ","",model_formula)
    splt_form <- strsplit(model_formula,c("\\+|~|\\*|:"))[[1]]
    if("-1" %in% splt_form) splt_form <- splt_form[-which(splt_form=="-1")]
    outcome <- splt_form[1]
    variable <- splt_form[-1]
    covariates <- NULL
  }

  if(!missing(effect_formula)){

    if(length(effect_formula)==1){
      #Parse formula so the check on the effect object can continue as usual
      if(!missing(effect)) message("effect_formula overriding effect argument.")
      if("formula" %in% class(effect_formula)) effect_formula <- deparse(effect_formula)
      splt_effect <- effect_formula
      if(grepl("~",splt_effect)) splt_effect <- strsplit(splt_effect,"~")[[1]][2] #Pull out variables from right hand side of formula. e.g. pairwise~A+B|C = "A+B|C"
      if(grepl("\\||+|\\*",splt_effect)) splt_effect <- strsplit(splt_effect,"\\||\\+|\\*")[[1]] #Split rhs of formula into vector of variables. e.g. "A+B|C"=c("A","B","C")


      effect <- splt_effect
    } else{
      stop("Unrecognized effect formula. Should be a character string of length 1. If listing in the form c('A','B'), use the effects argument.")
    }
  }



  if(missing(df) | missing(variable) | missing(effect)){
    stop('The df and variable and effect arguments need to be specified.')
  }

  tmp <- unique(unlist(strsplit(effect,":")))
  if(!all(tmp %in% unique(unlist(strsplit(variable,"[\\*:]"))))) {
    stop("All effect terms must be included in the variable argument or model formula.")
  }

  # Stop if internal controls (assays) have not been removed
  if ("AssayType" %in% names(df)) {
    if (any(df$AssayType != "assay")) {
      ctrl_assays <- df |>
        dplyr::filter(AssayType != "assay")
      
      stop(paste0(
        'Control assays have not been removed from the dataset.\n  Assays with AssayType != "assay" should be excluded.\n  The following ', length(unique(ctrl_assays$Assay)), " control assays were found:\n  ",
        paste(strwrap(toString(unique(ctrl_assays$Assay)), width = 80), collapse = "\n")
      ))
    }
  } else if (any(stringr::str_detect(df$Assay, stringr::regex("control|ctrl", ignore_case = TRUE)))) {
    ctrl_assays <- df |>
      dplyr::filter(stringr::str_detect(df$Assay, stringr::regex("control|ctrl", ignore_case = TRUE)))
    
    stop(paste0(
      'Control assays have not been removed from the dataset.\n  Assays with "control" in their Assay field should be excluded.\n  The following ', length(unique(ctrl_assays$Assay)), " control assays were found:\n  ",
      paste(strwrap(toString(unique(ctrl_assays$Assay)), width = 80), collapse = "\n")
    ))
  }

# Stop if external controls (samples) have not been removed
if ("SampleType" %in% names(df)) {
  if (any(df$SampleType != "SAMPLE")) {
    ctrl_samples <- df |>
      dplyr::filter(SampleType != "SAMPLE")

    stop(paste0(
      'Control samples have not been removed from the dataset.\n  Samples with SampleType != "SAMPLE" should be excluded.\n  The following ', length(unique(ctrl_samples$SampleID)), " control samples were found:\n  ",
      paste(strwrap(toString(unique(ctrl_samples$SampleID)), width = 80), collapse = "\n")
    ))
  }
} else if (any(stringr::str_detect(df$SampleID, stringr::regex("control|ctrl", ignore_case = TRUE)))) {
  ctrl_samples <- df |>
    dplyr::filter(stringr::str_detect(df$SampleID, stringr::regex("control|ctrl", ignore_case = TRUE)))

  stop(paste0(
    'Control samples have not been removed from the dataset.\n  Samples with "control" or "ctrl" in their SampleID field should be excluded.\n  The following ', length(unique(ctrl_samples$SampleID)), " control samples were found:\n  ",
    paste(strwrap(toString(unique(ctrl_samples$SampleID)), width = 80), collapse = "\n")
  ))
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


    if(missing(model_formula)){
      if(!is.null(covariates)){
        formula_string <- paste0(outcome, "~",
                                 paste(variable,collapse="*"),
                                 "+",
                                 paste(covariates, sep = '', collapse = '+'))
      }else{

        formula_string <- paste0(outcome, "~", paste(variable,collapse="*"))
      }
    } else if(!missing(model_formula)){
      formula_string <- model_formula
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
      dplyr::filter(!(OlinkID %in% npxCheck$all_nas)) %>% #Exclude assays that have all NA:s
      dplyr::mutate(OlinkID = factor(OlinkID, levels = olinkid_list)) %>%
      dplyr::group_by(Assay, OlinkID, UniProt, Panel) %>%
      dplyr::do(data.frame(emmeans::emmeans(stats::lm(as.formula(formula_string),data=.),
                                    specs=e_form,
                                    cov.reduce = function(x) round(c(mean(x),mean(x)+sd(x)),4),
                            infer=c(TRUE,TRUE),
                            adjust=post_hoc_padjust_method)[[c("contrasts","emmeans")[1+as.numeric(mean_return)]]],
                    stringsAsFactors=FALSE)) %>%
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
        dplyr::select(tidyselect::any_of(c("Assay", "OlinkID", "UniProt", "Panel", "term",  "contrast", effect, "estimate",
                      "conf.low", "conf.high", "Adjusted_pval","Threshold")))

      if(post_hoc_padjust_method=="none") anova_posthoc_results <- anova_posthoc_results %>% rename(pvalue=Adjusted_pval)
    }

    return(anova_posthoc_results)

  }, warning = function(w) {
    if (grepl(x = w, pattern = glob2rx("*contains implicit NA, consider using*")))
      invokeRestart("muffleWarning")
  })
}

