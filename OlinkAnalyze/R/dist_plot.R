#' Function to plot the NPX distribution by panel
#'
#' Generates boxplots of NPX vs. protein (OlinkID) colored by QC_Warning and faceted by Panel using ggplot and ggplot2::geom_boxplot. 
#' 
#' @param df NPX data frame in long format. Must have columns SampleID, NPX and Panel
#' @param color_g Character value indicating which column to use as fill color (default QC_Warning)
#' @return An object of class "ggplot"
#' @keywords NPX
#' @export
#' @examples \donttest{olink_dist_plot(df, color_g = "Group") }
#' @import dplyr stringr tidyr

olink_dist_plot <- function(df, color_g = 'QC_Warning') {
  
  #Filtering on valid OlinkID
  df <- df %>%
    filter(stringr::str_detect(OlinkID,
                               "OID[0-9]{5}"))
  
  reorder_within <- function(x, by, within, fun = mean, sep = "___", ...) {
    new_x <- paste(x, within, sep = sep)
    stats::reorder(new_x, by, FUN = fun)
  }
  
  scale_x_reordered <- function(..., sep = "___") {
    reg <- paste0(sep, ".+$")
    ggplot2::scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
  }
  
  df %>%
    filter(!(is.na(NPX))) %>% 
    ggplot(., aes(x = reorder_within(factor(SampleID), NPX, Panel, median), y = NPX, fill=!!rlang::ensym(color_g)))+
    geom_boxplot()+
    scale_x_reordered()+
    xlab("Samples")+
    facet_wrap(~Panel,  scale="free")+
    set_plot_theme() 
  
}