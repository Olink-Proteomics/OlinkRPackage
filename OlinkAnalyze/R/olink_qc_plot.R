#' Function to plot an overview of a sample cohort per Panel
#'
#' Generates a facet plot per Panel using ggplot and ggplot2::geom_point and stats::IQR plotting IQR vs. median for all samples. 
#' Horizontal dashed lines indicate +/-3 standard deviations from the mean IQR. 
#' Vertical dashed lines indicate +/-3 standard deviations from the mean sample median. 
#' 
#' @param df NPX data frame in long format. Must have columns SampleID, Index, NPX and Panel
#' @param color_g Character value indicating which column to use as fill color (default QC_Warning)
#' @param plot_index Boolean. If FALSE (default), a point will be plotted for a sample. If TRUE, 
#' a sample's unique index number is displayed.
#' @param label_outliers Boolean. If TRUE, an outlier sample will be labelled with its SampleID.   
#' @return An object of class "ggplot"
#' @keywords NPX
#' @export
#' @examples \donttest{olink_qc_plot(df, color_g = "Group") }
#' @import dplyr stringr tidyr
#' 


olink_qc_plot <- function(npx_df, color_g = "QC_Warning", plot_index = F, label_outliers = T){
  
  #Filtering on valid OlinkID
  npx_df <- npx_df %>%
    filter(stringr::str_detect(OlinkID,
                               "OID[0-9]{5}"))
  
  npx_df_qr <- npx_df %>%
    group_by(Panel, SampleID, Index) %>%
    mutate(IQR = IQR(NPX, na.rm = T),
           sample_median = median(NPX, na.rm = T)) %>%
    ungroup() %>%
    select(SampleID, Index, Panel, IQR, sample_median, !!rlang::ensym(color_g)) %>%
    distinct() %>%
    group_by(Panel) %>%
    mutate(median_low = mean(sample_median, na.rm = T) - 3*sd(sample_median, na.rm = T),
           median_high = mean(sample_median, na.rm = T) + 3*sd(sample_median, na.rm = T),
           iqr_low = mean(IQR, na.rm = T) - 3*sd(IQR, na.rm = T),
           iqr_high = mean(IQR, na.rm = T) + 3*sd(IQR, na.rm = T)) %>%
    ungroup() %>%
    mutate(Outlier = if_else(sample_median < median_high &
                               sample_median > median_low &
                               IQR > iqr_low &
                               IQR < iqr_high,
                             0, 1))
  
  
  qc_plot <- npx_df_qr %>%
    ggplot(aes(x = sample_median, y = IQR)) +
    geom_hline(aes(yintercept=iqr_low),
               linetype = 'dashed', 
               color = 'grey') + 
    geom_hline(aes(yintercept=iqr_high),
               linetype = 'dashed',
               color = 'grey') +
    geom_vline(aes(xintercept = median_low),
               linetype = 'dashed',
               color = 'grey') +
    geom_vline(aes(xintercept = median_high),
               linetype = 'dashed',
               color = 'grey') +
    xlab('Sample Median') +
    facet_wrap(~Panel, scale = "free") +
    set_plot_theme() 
  
  if(plot_index){
    qc_plot <- qc_plot + geom_text(aes(color = !!rlang::ensym(color_g), label = Index), size = 3) 
  }else{
    qc_plot <- qc_plot + geom_point(aes(color = !!rlang::ensym(color_g)), size = 2.5) 
  }
  
  if(label_outliers){
    
    qc_plot <- qc_plot + 
      geom_label_repel(data = . %>% filter(Outlier == 1),
                       aes(label=SampleID),
                       box.padding = 0.5, 
                       min.segment.length = 0.1,
                       show.legend=F,
                       size = 3)
    
  }
  
  return(qc_plot)
  
} 
