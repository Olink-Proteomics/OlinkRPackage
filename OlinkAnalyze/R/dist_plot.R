#' Function to plot the NPX distribution by panel
#'
#' Creates an array of plots of NPX distributions for all samples and assays, separated by Panel
#' @param df NPX data frame in long format. Must have columns SampleID, NPX and Panel
#' @param color_g Character value indicating which column to use as fill color (default QC_Warning)
#' @return An object of class "ggplot"
#' @keywords NPX
#' @export
#' @examples \donttest{olink_dist_plot(df, color_g = "Group") }
#' @import dplyr stringr tidyr

olink_dist_plot <- function(df, color_g = 'QC_Warning') {

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
    theme_bw()+  ##White background
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ ##Remove grid
    theme(panel.border = element_blank(), axis.line = element_line(size = 0.5))+ #Remove border, add x and y lin
    facet_wrap(~Panel,  scale="free")+
    theme(strip.background  = element_rect(fill="white"),
          strip.text = element_text(size=8),
          strip.text.x = element_text(size = 13), #Remove grey box from facet title
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

}
