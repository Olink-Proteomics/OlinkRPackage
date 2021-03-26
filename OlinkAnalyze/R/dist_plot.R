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
#' @import dplyr stringr tidyr

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

  # Remove "Olink", Target 96" and "Target 48" from the panel name(s)
  df_OlinkID_fixed <- df_OlinkID %>%
    mutate(Panel = Panel  %>% str_replace("Olink ", "") %>%
             str_replace("Target 96 ", "") %>%
             str_replace("Target 48 ", "")) %>%
    group_by(SampleID, Index, Panel) %>%
    mutate(QC_Warning = if_else(any(QC_Warning == "Warning"|QC_Warning == "WARN" ), "Warning", "Pass"))%>%
    ungroup()

  df_OlinkID_fixed %>%
    filter(!(is.na(NPX))) %>%
    ggplot(., aes(x = reorder_within(factor(SampleID), NPX, Panel, median), y = NPX, fill=!!rlang::ensym(color_g)))+
    geom_boxplot()+
    scale_x_reordered()+
    xlab("Samples")+
    facet_wrap(~Panel,  scale="free")+
    set_plot_theme() +
    olink_fill_discrete(...)

}
