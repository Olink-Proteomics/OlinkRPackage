#' Function to plot an overview of a sample cohort per Panel
#'
#' Generates a facet plot per Panel using ggplot2::ggplot and ggplot2::geom_point and stats::IQR plotting IQR vs. median for all samples.
#' Horizontal dashed lines indicate +/-IQR_outlierDef standard deviations from the mean IQR.
#' Vertical dashed lines indicate +/-median_outlierDef standard deviations from the mean sample median.
#'
#' @param df NPX data frame in long format. Must have columns SampleID, Index, NPX and Panel
#' @param color_g Character value indicating which column to use as fill color (default QC_Warning)
#' @param plot_index Boolean. If FALSE (default), a point will be plotted for a sample. If TRUE,
#' a sample's unique index number is displayed.
#' @param label_outliers Boolean. If TRUE, an outlier sample will be labelled with its SampleID.
#' @param IQR_outlierDef The number of standard deviations from the mean IQR that defines an outlier. Default is 3
#' @param median_outlierDef The number of standard deviations from the mean sample median that defines an outlier. Default is 3
#' @param facetNrow The number of rows that the panels are arranged on
#' @param facetNcol The number of columns that the panels are arranged on
#' @param ... coloroption passed to specify color order
#' @return An object of class "ggplot"
#' @keywords NPX
#' @export
#' @examples
#' \donttest{
#' olink_qc_plot(npx_data1, color_g = "QC_Warning")
#'
#' #Change the outlier threshold to +-4SD
#' olink_qc_plot(npx_data1, color_g = "QC_Warning", IQR_outlierDef = 4, median_outlierDef = 4)
#'
#' #Identify the outliers
#' qc <- olink_qc_plot(npx_data1, color_g = "QC_Warning", IQR_outlierDef = 4, median_outlierDef = 4)
#' outliers <- qc$data %>% dplyr::filter(Outlier == 1)
#' qc
#' }
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by mutate ungroup select distinct if_else filter case_when
#' @importFrom rlang ensym
#' @importFrom ggplot2 ggplot geom_hline geom_vline xlab facet_wrap geom_text geom_point
#' @importFrom ggrepel geom_label_repel
#' @importFrom stringr str_detect str_replace

olink_qc_plot <- function(df, color_g = "QC_Warning", plot_index = F, label_outliers = T, IQR_outlierDef = 3, median_outlierDef = 3, facetNrow = NULL, facetNcol = NULL, ...){

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
  npx_df <- df %>%
    dplyr::filter(stringr::str_detect(OlinkID,
                                      "OID[0-9]{5}"))

  npx_df_qr <- npx_df %>%
    dplyr::group_by(Panel, SampleID, Index) %>%
    dplyr::mutate(QC_Warning = dplyr::if_else(all(toupper(QC_Warning) == 'PASS'),
                                              'Pass',
                                              'Warning')) %>%
    dplyr::mutate(IQR = IQR(NPX, na.rm = T),
                  sample_median = median(NPX, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::select(SampleID, Index, Panel, IQR, sample_median, !!rlang::ensym(color_g)) %>%
    dplyr::distinct() %>%
    dplyr::group_by(Panel) %>%
    dplyr::mutate(median_low = mean(sample_median, na.rm = T) - median_outlierDef*sd(sample_median, na.rm = T),
                  median_high = mean(sample_median, na.rm = T) + median_outlierDef*sd(sample_median, na.rm = T),
                  iqr_low = mean(IQR, na.rm = T) - IQR_outlierDef*sd(IQR, na.rm = T),
                  iqr_high = mean(IQR, na.rm = T) + IQR_outlierDef*sd(IQR, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Outlier = dplyr::if_else(sample_median < median_high &
                                             sample_median > median_low &
                                             IQR > iqr_low &
                                             IQR < iqr_high,
                                           0, 1))


  qc_plot <- npx_df_qr %>%
    dplyr::mutate(Panel = Panel  %>% stringr::str_replace("Olink ", "")) %>%
    ggplot2::ggplot(ggplot2::aes(x = sample_median, y = IQR)) +
    ggplot2::geom_hline(ggplot2::aes(yintercept=iqr_low),
                        linetype = 'dashed',
                        color = 'grey') +
    ggplot2::geom_hline(ggplot2::aes(yintercept=iqr_high),
                        linetype = 'dashed',
                        color = 'grey') +
    ggplot2::geom_vline(ggplot2::aes(xintercept = median_low),
                        linetype = 'dashed',
                        color = 'grey') +
    ggplot2::geom_vline(ggplot2::aes(xintercept = median_high),
                        linetype = 'dashed',
                        color = 'grey') +
    ggplot2::xlab('Sample Median') +
    ggplot2::facet_wrap(~Panel, scale = "free", nrow = facetNrow, ncol = facetNcol) +
    OlinkAnalyze::set_plot_theme()+
    OlinkAnalyze::olink_color_discrete(...)

  if(plot_index){
    qc_plot <- qc_plot + ggplot2::geom_text(ggplot2::aes(color = !!rlang::ensym(color_g),
                                                         label = Index), size = 3)
  }else{
    qc_plot <- qc_plot + ggplot2::geom_point(ggplot2::aes(color = !!rlang::ensym(color_g)),
                                             size = 2.5)
  }

  if(label_outliers){

    qc_plot <- qc_plot +
      ggrepel::geom_label_repel(data = . %>% dplyr::mutate(SampleIDPlot = dplyr::case_when(Outlier == 1 ~ SampleID,
                                                                                           TRUE ~ "")),
                                ggplot2::aes(label=SampleIDPlot),
                                box.padding = 0.5,
                                min.segment.length = 0.1,
                                show.legend=FALSE,
                                size = 3)

  }

  return(qc_plot)

}
