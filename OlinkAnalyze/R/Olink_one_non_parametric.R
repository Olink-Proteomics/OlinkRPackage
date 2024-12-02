#'Function which performs a Kruskal-Wallis Test or Friedman Test per protein
#'
#'Performs an Kruskal-Wallis Test for each assay (by OlinkID) in every panel using stats::kruskal.test.
#'Performs an Friedman Test for each assay (by OlinkID) in every panel using rstatix::friedman_test.
#'The function handles factor variable. \cr\cr
#'Samples that have no variable information or missing factor levels are automatically removed from the analysis (specified in a message if verbose = TRUE).
#'Character columns in the input dataframe are automatically converted to factors (specified in a message if verbose = T).
#'Numerical variables are not converted to factors.
#'If a numerical variable is to be used as a factor, this conversion needs to be done on the dataframe before the function call. \cr\cr
#'Inference is specified in a message if verbose = TRUE. \cr
#'The formula notation of the final model is specified in a message if verbose = TRUE. \cr\cr
#'Adjusted p-values are calculated by stats::p.adjust according to the Benjamini & Hochberg (1995) method (“fdr”).
#'The threshold is determined by logic evaluation of Adjusted_pval < 0.05.
#'
#'
#' @param df NPX or Quantified_value data frame in long format with at least protein name (Assay), OlinkID, UniProt, Panel and a factor with at least 3 levels.
#' @param variable Single character value.
#' @param dependence Boolean. Default: FALSE. When the groups are independent, the kruskal-Wallis will run, when the groups are dependent, the Friedman test will run.
#' @param subject Group information for the repeated measurement. If (dependence = TRUE), this parameter need to be specified.
#' @param verbose Boolean. Default: True. If information about removed samples, factor conversion and final model formula is to be printed to the console.
#' @return A tibble containing the Kruskal-Wallis Test or Friedman Test results for every protein.
#'
#' Columns include:
#' \itemize{
#'  \item{Assay:} "character" Protein symbol
#'  \item{OlinkID:} "character" Olink specific ID
#'  \item{UniProt:} "character" UniProt ID
#'  \item{Panel:} "character" Name of Olink Panel
#'  \item{term:} "character" term in model
#'  \item{df:} "numeric" degrees of freedom
#'  \item{method:} "character" which method was used
#'  \item{statistic:} "named numeric" the value of the test statistic with a name describing it
#'  \item{p.value:} "numeric" p-value for the test
#'  \item{Adjusted_pval:} "numeric" adjusted p-value for the test (Benjamini&Hochberg)
#'  \item{Threshold:} "character" if adjusted p-value is significant or not (< 0.05)
#' }
#'
#' @export
#' @examples \donttest{
#'
#' library(dplyr)
#'
#' # One-way Kruskal-Wallis Test
#' try({ # May fail if dependencies are not installed
#' kruskal_results <- olink_one_non_parametric(df = npx_data1,
#'                                             variable = "Site")
#' })
#'
#' #Friedman Test
#' friedman_results <- olink_one_non_parametric(df = npx_data1,
#'                                              variable = "Time",
#'                                              subject = "Subject",
#'                                              dependence = TRUE)}
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
                                     subject = NULL,
                                     verbose=TRUE
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

    #Check data format
    npxCheck <- npxCheck(df)
    data_type <- npxCheck$data_type #Temporary fix to avoid issues with rlang::ensym downstream

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
        dplyr::filter(!(OlinkID %in% npxCheck$all_nas)) %>%
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
        dplyr::group_by(SampleID) %>%
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
      if (is.null(subject)){
        stop("Subject variable need to be specified!")
      }
      if(verbose){message(paste("Friedman model fit to each assay: "),formula_string)}

      formula_string <- paste0(formula_string,"|",subject)

      # add repeat measurement groups
      df_nas_remove <- df %>%
        dplyr::filter(!(OlinkID %in% npxCheck$all_nas)) %>%
        dplyr::filter(!(OlinkID %in% nas_in_var))
      # remove subject without complete data
      ## number of the items in the subject

      n_subject <- df_nas_remove %>%
        dplyr::select(!!!rlang::syms(variable)) %>%
        stats::na.omit() %>%
        unique() %>%
        pull() %>%
        length()

      subject_remove <- df_nas_remove %>%
        dplyr::filter(!is.na(NPX)) %>%
        dplyr::group_by(Assay, !!!rlang::syms(subject)) %>%
        dplyr::summarize(n = n(),.groups = "drop") %>%
        dplyr::filter(n == n_subject) %>%
        dplyr::mutate(Friedman_remove = "no") %>%
        dplyr::select(-n)

      if(length(subject_remove %>%
                dplyr::select(!!!rlang::syms(subject)) %>%
                dplyr::pull() %>%
                unique()) < length(df_nas_remove %>%
                                   dplyr::select(!!!rlang::syms(subject)) %>%
                                   dplyr::pull() %>%
                                   unique())){
        message(paste("Subjects removed due to incomplete data"))
      }

      p.val <- df_nas_remove %>%
        dplyr::left_join(subject_remove, by = c("Assay",subject)) %>%
        dplyr::filter(Friedman_remove == "no") %>%
        rstatix::convert_as_factor(!!!rlang::syms(variable)) %>%
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
        dplyr::filter(!(OlinkID %in% npxCheck$all_nas)) %>%
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




#'Function which performs posthoc test per protein for the results from Friedman or Kruskal-Wallis Test.
#'
#'Performs a posthoc test using rstatix::wilcox_test or FSA::dunnTest with Benjamini-Hochberg p-value adjustment per assay (by OlinkID) for each panel at confidence level 0.95.
#'See \code{olink_one_non_parametric} for details of input notation. \cr\cr
#'The function handles both factor and numerical variables.
#'
#' @param df NPX data frame in long format with at least protein name (Assay), OlinkID, UniProt, Panel and a factor with at least 3 levels.
#' @param olinkid_list Character vector of OlinkID's on which to perform post hoc analysis. If not specified, all assays in df are used.
#' @param variable Single character value or character array.
#' @param test Single character value indicates running the post hoc test for friedman or kruskal.
#' @param subject Single character value indicates running the post hoc test for friedman or kruskal.
#' @param verbose Boolean. Default: True. If information about removed samples, factor conversion and final model formula is to be printed to the console.
#' @return Tibble of posthoc tests for specified effect, arranged by ascending adjusted p-values.
#'
#' Columns include:
#' \itemize{
#'  \item{Assay:} "character" Protein symbol
#'  \item{OlinkID:} "character" Olink specific ID
#'  \item{UniProt:} "character" UniProt ID
#'  \item{Panel:} "character" Name of Olink Panel
#'  \item{term:} "character" term in model
#'  \item{contrast:} "character" the groups that were compared
#'  \item{estimate:} "numeric" the value of the test statistic with a name describing it
#'  \item{Adjusted_pval:} "numeric" adjusted p-value for the test
#'  \item{Threshold:} "character" if adjusted p-value is significant or not (< 0.05)
#' }
#'
#' @export
#' @examples \donttest{
#' library(dplyr)
#'
#'try({ # May fail if dependencies are not installed
#' # One-way Kruskal-Wallis Test
#' kruskal_results <- olink_one_non_parametric(df = npx_data1,
#'                                             variable = "Site")
#'})
#'
#' #Friedman Test
#' friedman_results <- olink_one_non_parametric(df = npx_data1,
#'                                             variable = "Time",
#'                                             subject = "Subject",
#'                                             dependence = TRUE)
#'
#' #Posthoc test for the results from Friedman Test
#'friedman_posthoc_results <- olink_one_non_parametric_posthoc(npx_data1,
#'                                                             variable = "Time",
#'                                                             test = "friedman",
#'                                                             olinkid_list = {friedman_results %>%
#'                                                               filter(Threshold == 'Significant') %>%
#'                                                               dplyr::select(OlinkID) %>%
#'                                                               distinct() %>%
#'                                                               pull()})}
#'
#' @importFrom dplyr filter group_by ungroup pull do select arrange mutate
#' @importFrom stringr str_detect
#' @importFrom rstatix wilcox_test



olink_one_non_parametric_posthoc <- function(df,
                                             olinkid_list = NULL,
                                             variable,
                                             test = "kruskal",
                                             subject = "Subject",
                                             verbose=TRUE
                                             ){
  if (test != "friedman"){
    if(!requireNamespace("FSA", quietly = TRUE)){
      stop("Kruskal test requires FSA package.
         Please install FSA before continuing.

         install.packages(\"FSA\")")
    }
  }
  if(missing(df) | missing(variable)){
    stop('The df and variable arguments need to be specified.')
  }
  if (!(test %in% c("friedman","kruskal"))){
    stop('The type of the test need to be specified as kruskal or friedman')
  }

  withCallingHandlers({

    # Filtering on valid OlinkID
    df <- df %>%
      dplyr::filter(stringr::str_detect(OlinkID, "OID[0-9]{5}"))

    if(is.null(olinkid_list)){
      olinkid_list <- df %>%
        dplyr::select(OlinkID) %>%
        dplyr::distinct() %>%
        dplyr::pull()
    }

    #Check data format
    npxCheck <- npxCheck(df)
    data_type <- npxCheck$data_type #Temporary fix to avoid issues with rlang::ensym downstream

    #Variables to check
    variable_testers <- intersect(c(variable), names(df))
    ##Remove rows where variables or covariate is NA (cant include in analysis anyway)
    removed.sampleids <- NULL

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
    }

    if(test == "friedman"){
      message("Pairwise comparisons for Friedman test using paired Wilcoxon signed-rank test were performed")

      #Not testing assays that have all NA:s in one level
      #Every sample needs to have a unique level of the factor

      nas_in_var <- character(0)

      single_fixed_effects <- variable

      for(effect in single_fixed_effects){

        current_nas <- df %>%
          dplyr::filter(!(OlinkID %in% npxCheck$all_nas)) %>%
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
          dplyr::group_by(SampleID) %>%
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

      # add repeat measurement groups
      df_nas_remove <- df %>%
        dplyr::filter(!(OlinkID %in% npxCheck$all_nas)) %>%
        dplyr::filter(!(OlinkID %in% nas_in_var))
      # remove subject without complete data
      ## number of the items in the subject

      n_subject <- df_nas_remove %>%
        dplyr::select(!!!rlang::syms(variable)) %>%
        stats::na.omit() %>%
        unique() %>%
        pull() %>%
        length()

      subject_remove <- df_nas_remove %>%
        dplyr::filter(!is.na(NPX)) %>%
        dplyr::group_by(Assay, !!!rlang::syms(subject)) %>%
        dplyr::summarize(n = n(),.groups = "drop") %>%
        dplyr::filter(n == n_subject) %>%
        dplyr::mutate(Friedman_remove = "no") %>%
        dplyr::select(-n)

      if(length(subject_remove %>%
                dplyr::select(!!!rlang::syms(subject)) %>%
                dplyr::pull() %>%
                unique()) < length(df_nas_remove %>%
                                   dplyr::select(!!!rlang::syms(subject)) %>%
                                   dplyr::pull() %>%
                                   unique())){
        message(paste("Subjects removed due to incomplete data"))
      }

      p.hoc_val <- df_nas_remove %>%
        dplyr::left_join(subject_remove, by = c("Assay",subject)) %>%
        dplyr::filter(Friedman_remove == "no") %>%
        dplyr::filter(OlinkID %in% olinkid_list) %>%
        dplyr::filter(!(OlinkID %in% npxCheck$all_nas)) %>% #Exclude assays where all samples have NPX=NA
        dplyr::mutate(OlinkID = factor(OlinkID, levels = olinkid_list)) %>%
        dplyr::arrange(Assay, OlinkID, UniProt, Panel,.data[[variable]],.data[[subject]]) %>%
        dplyr::group_by(Assay, OlinkID, UniProt, Panel) %>%
        dplyr::do(rstatix::wilcox_test(data =., as.formula(formula_string), p.adjust.method = "BH",detailed = TRUE, conf.level = 0.95, paired = T)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate("variable" = variable) %>%
        dplyr::mutate(Threshold = if_else(p.adj < 0.05,'Significant','Non-significant')) %>%
        dplyr::rename(term = variable) %>%
        dplyr::mutate(contrast = paste(group1, group2, sep = " - ")) %>%
        dplyr::arrange(p.adj) %>%
        dplyr::rename(Adjusted_pval = p.adj) %>%
        dplyr::select(all_of(c("Assay", "OlinkID", "UniProt", "Panel", "term", "contrast", "estimate", "Adjusted_pval", "Threshold")))
    } else {
      message("Pairwise comparisons for Kruskal-Wallis test using Dunn test were performed")
      p.hoc_val <- df %>%
        dplyr::filter(OlinkID %in% olinkid_list) %>%
        dplyr::filter(!(OlinkID %in% npxCheck$all_nas)) %>% #Exclude assays where all samples have NPX=NA
        dplyr::mutate(OlinkID = factor(OlinkID, levels = olinkid_list)) %>%
        dplyr::group_by(Assay, OlinkID, UniProt, Panel) %>%
        dplyr::do(FSA::dunnTest(data =., as.formula(formula_string), method = "bh")$res) %>%
        dplyr::ungroup() %>%
        dplyr::mutate("variable" = variable) %>%
        dplyr::mutate(Threshold = if_else(P.adj < 0.05,'Significant','Non-significant')) %>%
        dplyr::rename(term = variable) %>%
        dplyr::rename(contrast = Comparison) %>%
        dplyr::arrange(P.adj) %>%
        dplyr::rename(Adjusted_pval = P.adj) %>%
        dplyr::rename(estimate = Z) %>%
        dplyr::select(all_of(c("Assay", "OlinkID", "UniProt", "Panel", "term", "contrast", "estimate", "Adjusted_pval", "Threshold")))
    }

    return(p.hoc_val)

  }, warning = function(w) {
    if (grepl(x = w, pattern = glob2rx("*contains implicit NA, consider using*")))
      invokeRestart("muffleWarning")
  })
}




