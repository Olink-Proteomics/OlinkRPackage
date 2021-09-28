#' Function to plot the NPX distribution by panel
#'
#' Generates boxplots of NPX vs. protein (OlinkID) colored by QC_Warning and faceted by Panel using ggplot and ggplot2::geom_boxplot.
#'
#' @param df NPX data frame in long format. Must have columns SampleID, NPX and Panel
#' @param color_g Character value indicating which column to use as fill color (default QC_Warning)
#' @param ... Color option passed to specify color order.
#' @return An object of class "ggplot"
#' @keywords NPX
#' @export
#' @examples \donttest{olink_dist_plot(npx_data1, color_g = "QC_Warning")}
#' @importFrom dplyr filter mutate group_by ungroup if_else
#' @importFrom stats reorder
#' @importFrom ggplot2 ggplot aes scale_x_discrete geom_boxplot xlab facet_wrap
#' @importFrom stringr str_replace
#' @importFrom magrittr %>%
#' @importFrom rlang ensym

olink_dist_plot <- function(df, color_g = 'QC_Warning', ...) {

  #checking ellipsis
  if(length(list(...)) > 0){

    ellipsis_variables <- names(list(...))

    if(length(ellipsis_variables) == 1){

      if(!(ellipsis_variables == 'coloroption')){

        stop(paste0('The ... option only takes the coloroption argument. ... currently contains the variable ',
                    ellipsis_variables,
                    '.'))

      }

    }else{

      stop(paste0('The ... option only takes one argument. ... currently contains the variables ',
                  paste(ellipsis_variables, collapse = ', '),
                  '.'))
    }
  }



  #Filtering on valid OlinkID
  df_OlinkID <- df %>%
    dplyr::filter(stringr::str_detect(OlinkID,
                               "OID[0-9]{5}"))

  reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
    new_x <- paste(x, within, sep = sep)
    stats::reorder(new_x, by, FUN = fun)
  }

  scale_x_reordered <- function(..., sep = "___") {
    reg <- paste0(sep, ".+$")
    ggplot2::scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
  }

  #If not all are Pass, the QC_Warning is set as warning for plotting purposes
  df_OlinkID_fixed <- df_OlinkID %>%
    dplyr::mutate(Panel = Panel %>% stringr::str_replace("Olink ", "")) %>%
    dplyr::group_by(SampleID, Index, Panel) %>%
    dplyr::mutate(QC_Warning = dplyr::if_else(all(toupper(QC_Warning) == 'PASS'),
                                              'PASS',
                                              'WARNING')) %>%
    dplyr::ungroup()

  df_OlinkID_fixed %>%
    dplyr::filter(!(is.na(NPX))) %>%
    ggplot2::ggplot(., ggplot2::aes(x = reorder_within(factor(SampleID), NPX, Panel, median), y = NPX, fill=!!rlang::ensym(color_g)))+
    ggplot2::geom_boxplot()+
    scale_x_reordered()+
    ggplot2::xlab("Samples")+
    ggplot2::facet_wrap(~Panel,  scale="free")+
    OlinkAnalyze::set_plot_theme() +
    OlinkAnalyze::olink_fill_discrete(...)
}
