#'Function which performs a Kruskal-Wallis Test or Friedman Test per protein
#'
#'Performs an Kruskal-Wallis Test for each assay (by OlinkID) in every panel using stats::kruskal.test.
#'Performs an Friedman Test for each assay (by OlinkID) in every panel using rstatix::friedman_test.
#'The function handles factor variable. \cr\cr
#'Samples that have no variable information or missing factor levels are automatically removed from the analysis (specified in a message if verbose = T).
#'Character columns in the input dataframe are automatically converted to factors (specified in a message if verbose = T).
#'Numerical variables are not converted to factors.
#'If a numerical variable is to be used as a factor, this conversion needs to be done on the dataframe before the function call. \cr\cr
#'Inference is specified in a message if verbose = T. \cr
#'The formula notation of the final model is specified in a message if verbose = T. \cr\cr
#'Adjusted p-values are calculated by stats::p.adjust according to the Benjamini & Hochberg (1995) method (“fdr”).
#'The threshold is determined by logic evaluation of Adjusted_pval < 0.05.
#'
#'
#' @param df NPX or Quantified_value data frame in long format with at least protein name (Assay), OlinkID, UniProt, Panel and a factor with at least 3 levels.
#' @param variable Single character value.
#' @param dependence Logical. Default: FALSE. When the groups are independent, the kruskal-Wallis will run, when the groups are dependent, the Friedman test will run.
#' @param verbose Logical. Default: True. If information about removed samples, factor conversion and final model formula is to be printed to the console.
#'
#' @return A tibble containing the Kruskal-Wallis Test or Friedman Test results for every protein.
#' The tibble is arranged by ascending p-values.
#' @export
#' @examples \donttest{
#'
#' library(dplyr)
#'
#' npx_df <- npx_data1 %>% filter(!grepl('control', SampleID, ignore.case = TRUE))
#'
#' #One-way Kruskal-Wallis Test.
#' #Results in a model NPX~Time
#' Kruskal_results <- olink_one_non_parametric(df = npx_df, variable = "Time")
#'
#' #One-way Friedman Test.
#' #Results in a model NPX~Time
#' Friedman_results <- olink_one_non_parametric(df = npx_df, variable = "Time", dependence = TRUE)}
#'
#' @importFrom dplyr n filter group_by summarise ungroup pull n_distinct do select arrange mutate
#' @importFrom stringr str_detect
#' @importFrom generics tidy
#' @importFrom stats kruskal.test IQR as.formula contr.sum lm median na.omit p.adjust sd t.test var
#' @importFrom rstatix friedman_test convert_as_factor
#' @importFrom utils glob2rx read.table globalVariables

olink_one_non_parametric <- function(df,
                                     variable,
                                     dependence = FALSE,
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

    #Variables to check
    variable_testers <- intersect(c(variable), names(df))

    ##Remove rows where variables is NA (cant include in analysis anyway)
    removed.sampleids <- NULL

    #removed.sampleids <- unique(c(removed.sampleids,df$SampleID[is.na(df[[variable_testers]])]))
    for(i in variable_testers){
      removed.sampleids <- unique(c(removed.sampleids,df$SampleID[is.na(df[[i]])]))
      df <- df[!is.na(df[[i]]),]
    }

    df <- df[!is.na(df[[variable_testers]]),]

    # # Check whether it is NPX or QUANT

    if ('NPX' %in% colnames(df)) {
      data_type <- 'NPX'
    } else if ('Quantified_value' %in% colnames(df)) {
      data_type <- 'Quantified_value'
    } else {
      stop('The NPX or Quantified_value is not in the df.')}

    #Not testing assays that have all NA:s
    all_nas <- df  %>%
      dplyr::group_by(OlinkID) %>%
      dplyr::summarise(n = dplyr::n(), n_na = sum(is.na(!!rlang::ensym(data_type)))) %>%
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


    single_fixed_effects <- variable



    for(effect in single_fixed_effects){

      current_nas <- df %>%
        dplyr::filter(!(OlinkID %in% all_nas)) %>%
        dplyr::group_by(OlinkID, !!rlang::ensym(effect)) %>%
        dplyr::summarise(n = dplyr::n(), n_na = sum(is.na(!!rlang::ensym(data_type))),.groups="drop") %>%
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
        dplyr::summarise(n_levels = dplyr::n_distinct(!!rlang::ensym(effect), na.rm = T),.groups = "drop") %>%
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

    formula_string <- paste0(data_type, "~", paste(variable,collapse="*"))


    #Get factors
    fact.vars <- sapply(variable_testers, function(x) is.factor(df[[x]]))
    fact.vars <- names(fact.vars)[fact.vars]


    #Print verbose message
    if(verbose){
      if(!is.null(removed.sampleids) & length(removed.sampleids) >0){
        message("Samples removed due to missing variable ",
                paste(removed.sampleids,collapse=", "))
      }
      if(!is.null(converted.vars)){
        message(paste0("Variables converted from character to factors: ",
                       paste(converted.vars,collapse = ", ")))
      }
      if(!is.null(num.vars)){
        message(paste0("Variables treated as numeric: ",
                       paste(num.vars,collapse = ", ")))
      }
    }


    if (dependence){
      if(verbose){message(paste("Friedman model fit to each assay: "),formula_string)}
      formula_string <- paste0(formula_string,"|","group_id")
      p.val <- df %>%
        dplyr::filter(!(OlinkID %in% all_nas)) %>%
        dplyr::filter(!(OlinkID %in% nas_in_var)) %>%
        dplyr::group_by(Assay,OlinkID, UniProt, Panel,!!!rlang::syms(variable)) %>%
        dplyr::mutate(group_id = as.factor (1:dplyr::n()))%>%
        rstatix::convert_as_factor(!!!rlang::syms(variable)) %>%
        dplyr::ungroup(!!!rlang::syms(variable))%>%
        dplyr::group_by(Assay, OlinkID, UniProt, Panel) %>%
        dplyr::do(rstatix::friedman_test(as.formula(formula_string),
                                   data=.,na.action=na.omit)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(Adjusted_pval = p.adjust(p, method = "BH")) %>%
        dplyr::mutate(Threshold  = ifelse(Adjusted_pval<0.05, "Significant","Non-significant")) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(Adjusted_pval) %>%
        dplyr::select(-n,-`.y.`) %>%
        dplyr::mutate(term = variable) %>%
        dplyr::rename(p.value = p) %>%
        dplyr::select(Assay, OlinkID, UniProt, Panel, term, df, method, statistic, p.value, Adjusted_pval, Threshold)


 }else{
   if(verbose){message(paste("Kruskal model fit to each assay: "),formula_string)}
      p.val <- df %>%
        dplyr::filter(!(OlinkID %in% all_nas)) %>%
        dplyr::filter(!(OlinkID %in% nas_in_var)) %>%
        dplyr::group_by(Assay, OlinkID, UniProt, Panel) %>%
        dplyr::do(broom::tidy(kruskal.test(as.formula(formula_string),
                              data=.))) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(Adjusted_pval=p.adjust(p.value,method="BH")) %>%
        dplyr::mutate(Threshold  = ifelse(Adjusted_pval<0.05,"Significant","Non-significant")) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(Adjusted_pval)} %>%
        dplyr::mutate(term = variable) %>%
        dplyr::rename(df = parameter) %>%
        dplyr::select(Assay, OlinkID, UniProt, Panel, term, df, method, statistic, p.value, Adjusted_pval, Threshold)

  }, warning = function(w) {
    if (grepl(x = w, pattern = glob2rx("*contains implicit NA, consider using*")))
      invokeRestart("muffleWarning")
  })
}




#'Function which performs a Wilcox posthoc test per protein.
#'
#'Performs a posthoc test using rstatix::wilcox_test with Benjamini-Hochberg p-value adjustment per assay (by OlinkID) for each panel at confidence level 0.95.
#'See \code{olink_kruskal} for details of input notation. \cr\cr
#'The function handles both factor and numerical variables.
#'The posthoc test for a numerical variable compares the difference in medians of the outcome variable (default: NPX) for 1 standard deviation difference in the numerical variable, e.g.
#'median NPX at mean(numerical variable) versus median NPX at median(numerical variable) + 1*SD(numerical variable).
#'
#' @param df NPX data frame in long format with at least protein name (Assay), OlinkID, UniProt, Panel and a factor with at least 3 levels.
#' @param olinkid_list Character vector of OlinkID's on which to perform post hoc analysis. If not specified, all assays in df are used.
#' @param variable Single character value or character array.
#' @param outcome Character. The dependent variable. Default: NPX.
#' @param p_adjust_method Adjust P-value for Multiple Comparisons.
#' @param verbose Logical. Default: True. If information about removed samples, factor conversion and final model formula is to be printed to the console.
#' @return Tibble of posthoc tests for specified effect, arranged by ascending adjusted p-values.
#' @export
#' @examples \donttest{
#'
#' library(dplyr)
#'
#' npx_df <- npx_data1 %>% filter(!grepl('control',SampleID, ignore.case = TRUE))
#'
#' #Kruskal-Wallis Test
#' kruskal_wallis_results <- olink_one_non_parametric(npx_df, "Site")
#'
#' #Friedman Test
#' Friedman_results <- olink_one_non_parametric(npx_df, "Time", dependence = TRUE)
#'
#' #Posthoc test for the results from Friedman Test
#' #Filtering out significant and relevant results.
#' significant_assays <- Friedman_results %>%
#' filter(Threshold == 'Significant') %>%
#' dplyr::select(OlinkID) %>%
#' distinct() %>%
#' pull()
#'
#' #Posthoc
#' friedman_posthoc_results <- olink_one_non_parametric_posthoc(npx_df, variable = c("Time"),
#' olinkid_list = significant_assays)}
#'
#' @importFrom dplyr filter group_by ungroup pull do select arrange mutate
#' @importFrom stringr str_detect
#' @importFrom rstatix wilcox_test


olink_one_non_parametric_posthoc <- function(df,
                                             olinkid_list = NULL,
                                             variable,
                                             verbose=T
                                             ){



  if(missing(df) | missing(variable)){
    stop('The df and variable and effect arguments need to be specified.')
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

    # # Check whether it is NPX or QUANT

    if ('NPX' %in% colnames(df)) {
      data_type <- 'NPX'
    } else if ('Quantified_value' %in% colnames(df)) {
      data_type <- 'Quantified_value'
    } else {
      stop('The NPX or Quantified_value is not in the df.')}


    variable_testers <- intersect(c(variable), names(df))
    ##Remove rows where variables or covariate is NA (cant include in analysis anyway)
    removed.sampleids <- NULL

    #removed.sampleids <- unique(c(removed.sampleids,df$SampleID[is.na(df[[variable_testers]])]))
    #df <- df[!is.na(df[[variable_testers]]),]

    for(i in variable_testers){
      removed.sampleids <- unique(c(removed.sampleids,df$SampleID[is.na(df[[i]])]))
      df <- df[!is.na(df[[i]]),]
    }

    ##Convert character vars to factor
    converted.vars <- NULL
    num.vars <- NULL

    if(is.character(df[[variable_testers]])){
        df[[variable_testers]] <- factor(df[[variable_testers]])
        converted.vars <- c(converted.vars,variable_testers)
        } else if(is.numeric(df[[variable_testers]])){
        num.vars <- c(num.vars,variable_testers)
        }

    formula_string <- paste0(data_type, "~", paste(variable,collapse="*"))

    #Print verbose message
    if(verbose){
      if(!is.null(removed.sampleids) & length(removed.sampleids) >0){
        message("Samples removed due to missing variable: ",
                paste(removed.sampleids,collapse=", "))
      }
      if(!is.null(converted.vars)){
        message(paste0("Variables converted from character to factors: ",
                       paste(converted.vars,collapse = ", ")))
      }
      if(!is.null(num.vars)){
        message(paste0("Variables treated as numeric: ",
                       paste(num.vars,collapse = ", ")))
      }
      if(any(variable %in% num.vars)){
        message(paste0("Numeric variables post-hoc performed using Mean and Mean + 1SD: ",
                       paste(num.vars[num.vars%in%variable],collapse = ", ")))
      }
      message(paste("Means estimated for each assay from non-parametric model: ",formula_string))
    }



    p.hoc_val <- df %>%
      dplyr::filter(OlinkID %in% olinkid_list) %>%
      dplyr::mutate(OlinkID = factor(OlinkID, levels = olinkid_list)) %>%
      dplyr::group_by(Assay, OlinkID, UniProt, Panel) %>%
      dplyr::do(rstatix::wilcox_test(data =., as.formula(formula_string), p.adjust.method = "BH",detailed = TRUE, conf.level = 0.95)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate("variable" = variable) %>%
      dplyr::mutate(Threshold = if_else(p.adj < 0.05,'Significant','Non-significant')) %>%
      dplyr::rename(term = variable) %>%
      dplyr::mutate(contrast = paste(group1, group2, sep = " - ")) %>%
      dplyr::rename(Adjusted_pval = p.adj) %>%
      dplyr::select(all_of(c("Assay", "OlinkID", "UniProt", "Panel", "term", "contrast", "estimate","conf.low", "conf.high", "Adjusted_pval","Threshold")))


    return(p.hoc_val)

  }, warning = function(w) {
    if (grepl(x = w, pattern = glob2rx("*contains implicit NA, consider using*")))
      invokeRestart("muffleWarning")
  })
}




