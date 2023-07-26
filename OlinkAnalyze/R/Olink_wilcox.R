#'Function which performs a Mann-Whitney U Test per protein
#'
#'Performs a Welch 2-sample Mann-Whitney U Test at confidence level 0.95 for every protein (by OlinkID)
#'for a given grouping variable using stats::wilcox.test and corrects for multiple testing by
#'the Benjamini-Hochberg method (“fdr”) using stats::p.adjust.
#'Adjusted p-values are logically evaluated towards adjusted p-value<0.05.
#'The resulting Mann-Whitney U Test table is arranged by ascending p-values.
#'
#' @param df NPX or Quantified_value data frame in long format with at least protein name (Assay), OlinkID, UniProt and a factor with 2 levels.
#' @param variable Character value indicating which column should be used as the grouping variable. Needs to have exactly 2 levels.
#' @param pair_id Character value indicating which column indicates the paired sample identifier.
#' @param ... Options to be passed to wilcox.test. See \code{?wilcox_test} for more information.
#' @return A data frame containing the Mann-Whitney U Test results for every protein.
#'
#' Columns include:
#' \itemize{
#'  \item{Assay:} "character" Protein symbol
#'  \item{OlinkID:} "character" Olink specific ID
#'  \item{UniProt:} "character" UniProt ID
#'  \item{Panel:} "character" Name of Olink Panel
#'  \item{estimate:} "numeric" median of NPX differences between groups
#'  \item{statistic:} "named numeric" the value of the test statistic with a name describing it
#'  \item{p.value:} "numeric" p-value for the test
#'  \item{conf.low:} "numeric" confidence interval for the median of differences (lower end)
#'  \item{conf.high:} "numeric" confidence interval for the median of differences (upper end)
#'  \item{method:} "character" which wilcoxon method was used
#'  \item{alternative:} "character" describes the alternative hypothesis
#'  \item{Adjusted_pval:} "numeric" adjusted p-value for the test (Benjamini&Hochberg)
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
#' wilcox_results <- olink_wilcox(df = npx_df,
#'                                variable = 'Treatment',
#'                                alternative = 'two.sided')
#'
#' #Paired Mann-Whitney U Test
#' npx_df %>%
#'    filter(Time %in% c("Baseline","Week.6")) %>%
#'    olink_wilcox(variable = "Time", pair_id = "Subject")
#'}

#' @importFrom magrittr %>%
#' @importFrom dplyr n group_by summarise n_distinct ungroup filter as_tibble select mutate pull all_of rename arrange do
#' @importFrom stringr str_detect
#' @importFrom broom tidy
#' @importFrom rlang ensym
#' @importFrom tidyr pivot_wider
#' @importFrom stats wilcox.test
#' @importFrom tibble is_tibble


olink_wilcox <- function(df, variable, pair_id, ...){
  if (missing(df) | missing(variable)) {
    stop("The df and variable arguments need to be specified.")
    }
#Filtering on valid OlinkID
  df <- df %>%
    filter(stringr::str_detect(OlinkID,
                               "OID[0-9]{5}"))

  #Removing SampleID:s with no level for variable
  removed.sampleids <- NULL
  removed.sampleids <- unique(c(removed.sampleids,
                                df$SampleID[is.na(df[[variable]])]))
  df <- df[!is.na(df[[variable]]), ]

  if (!is.null(removed.sampleids) & length(removed.sampleids) > 0) {
    message("Samples removed due to missing variable levels: ",
            paste(removed.sampleids, collapse = ", "))
  }

  if(!missing(pair_id)){
    missing.pair <- NULL
    missing.pair <- df$SampleID[is.na(df[[pair_id]])]

    if (!is.null(missing.pair) & length(missing.pair) > 0) {
      message("Samples removed due to missing pair ID: ",
              paste(missing.pair, collapse = ", "))
    }
    df <- df[!is.na(df[[pair_id]]), ]
    removed.sampleids <- unique(c(removed.sampleids,missing.pair))
  }


  #Factor conversion
  if (is.character(df[[variable]])) {
    df[[variable]] <- factor(df[[variable]])
    message(paste0("Variable converted from character to factor: ", variable))
  }
  else if (!is.factor(df[[variable]])) {
    stop(paste0('The grouping variable ', variable, 'is neither factor nor character. Only character and factor variable types allowed.'))
  }

  var_levels <- levels(df[[variable]])
  number_of_levels <- length(var_levels)

  #Checking number of levels
  if(!(number_of_levels == 2)){

    stop(paste0('The number of levels in the factor needs to be 2. Your factor has ', number_of_levels,
                ' levels.'))

  }

  #Every sample needs to have a unique level of the factor
  number_of_samples_w_more_than_one_level <- df %>%
    dplyr::group_by(SampleID) %>%
    dplyr::summarise(n_levels = dplyr::n_distinct(!!rlang::ensym(variable), na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(n_levels > 1) %>%
    nrow(.)

  if (number_of_samples_w_more_than_one_level > 0) {
    stop(paste0("There are ", number_of_samples_w_more_than_one_level,
                " samples that do not have a unique level for your variable. Only one level per sample is allowed."))
  }

  #Check data format
  npxCheck <- npxCheck(df)
  data_type <- npxCheck$data_type #Temporary fix to avoid issues with rlang::ensym downstream

  nas_in_level <- df  %>%
    dplyr::filter(!(OlinkID %in% npxCheck$all_nas)) %>%
    dplyr::group_by(OlinkID, !!rlang::ensym(variable)) %>%
    dplyr::summarise(n = dplyr::n(), n_na = sum(is.na(!!rlang::ensym(data_type)))) %>%
    dplyr::ungroup() %>%
    dplyr::filter(n == n_na) %>%
    dplyr::pull(OlinkID)


  if(length(nas_in_level) > 0) {
    warning(paste0('The assays ',
                   paste(nas_in_level, collapse = ', '),
                   ' have only NA:s in one level of the factor. They will not be tested.'),
            call. = F)
  }


  if(!missing(pair_id)){

    if(!pair_id %in% colnames(df)) stop(paste0("Column ",pair_id," not found."))

    if(!tibble::is_tibble(df)){
      message("Converting data frame to tibble.")
      df <- dplyr::as_tibble(df)
    }

    #check that each "pair_id' has only 2 samples
    ct_pairs <- df %>%
      dplyr::filter(!(OlinkID %in% npxCheck$all_nas)) %>%
      dplyr::filter(!(OlinkID %in% nas_in_level)) %>%
      dplyr::filter(!is.na(!!rlang::ensym(variable))) %>%
      dplyr::group_by(OlinkID,!!rlang::ensym(pair_id)) %>%
      dplyr::summarize(n=dplyr::n())
    if(!all(ct_pairs$n <= 2)) stop(paste0("Each pair identifier must identify no more than 2 unique samples. Check pairs: ",
                                          paste(unique(ct_pairs[[pair_id]][ct_pairs$n>2]),collapse=", ")))

    message(paste0('Paired Mann-Whitney U Test is performed on ', var_levels[1], ' - ', var_levels[2], '.'))


    p.val <- df %>%
      dplyr::filter(!(OlinkID %in% npxCheck$all_nas)) %>%
      dplyr::filter(!(OlinkID %in% nas_in_level)) %>%
      dplyr::select(all_of(c("OlinkID", "UniProt", "Assay", "Panel", data_type,
                             variable, pair_id))) %>%
      tidyr::pivot_wider(names_from = all_of(variable),
                         values_from = all_of(data_type)) %>%
      dplyr::group_by(Assay, OlinkID, UniProt, Panel) %>%
      dplyr::do(tidy(stats::wilcox.test(x = .[[var_levels[1]]],
                                        y = .[[var_levels[2]]],
                                        paired = TRUE,
                                        conf.int = TRUE,
                                        ...))) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(Adjusted_pval = p.adjust(p.value, method = "fdr")) %>%
      dplyr::mutate(Threshold = ifelse(Adjusted_pval < 0.05,
                                       "Significant",
                                       "Non-significant")) %>%
      dplyr::arrange(p.value)

  } else {


    message(paste0('Mann-Whitney U Test is performed on ', var_levels[1], ' - ', var_levels[2], '.'))


    p.val <- df %>%
      dplyr::filter(!(OlinkID %in% npxCheck$all_nas)) %>%
      dplyr::filter(!(OlinkID %in% nas_in_level)) %>%
      dplyr::group_by(Assay, OlinkID, UniProt, Panel) %>%
      dplyr::do(broom::tidy(stats::wilcox.test(!!rlang::ensym(data_type) ~ !!rlang::ensym(variable), data = ., conf.int = TRUE, ...))) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(Adjusted_pval = p.adjust(p.value, method = "fdr")) %>%
      dplyr::mutate(Threshold = ifelse(Adjusted_pval < 0.05, "Significant", "Non-significant")) %>%
      dplyr::arrange(p.value)
  }

  return(p.val)

}
