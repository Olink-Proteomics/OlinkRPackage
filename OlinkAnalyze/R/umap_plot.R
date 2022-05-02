#' Function to plot a PCA of the data
#'
#' Generates a PCA projection of all samples from NPX data along two principal components (default PC2 vs. PC1) including the explained variance and dots colored by QC_Warning using stats::prcomp and ggplot2::ggplot.
#'
#' The values are by default scaled and centered in the PCA and proteins with missing NPX values are by default removed from the corresponding assay.
#' Unique sample names are required.
#' Imputation by the median is done for assays with missingness <10\% for multi-plate projects and <5\% for single plate projects.
#' The plot is printed, and a list of ggplot objects is returned. \cr\cr
#' If byPanel = TRUE, the data processing (imputation of missing values etc) and subsequent PCA is performed separately per panel. A faceted plot is printed, while the individual ggplot objects are returned. \cr\cr
#' The arguments outlierDefX and outlierDefY can be used to identify outliers in the PCA. Samples more than +/-outlierDef[X,Y] standard deviations from the mean of the plotted PC will be labelled. Both arguments have to be specified.
#'
#' @param df data frame in long format with Sample Id, NPX and column of choice for colors
#' @param color_g Character value indicating which column to use for colors (default QC_Warning)
#' @param x_val Integer indicating which principal component to plot along the x-axis (default 1)
#' @param y_val Integer indicating which principal component to plot along the y-axis (default 2)
#' @param label_samples Logical. If TRUE, points are replaced with SampleID (default FALSE)
#' @param config object of class umap.config, specifying the parameters for the UMAP algorithm.
#' @param drop_assays Logical. All assays with any missing values will be dropped. Takes precedence over sample drop.
#' @param drop_samples Logical. All samples with any missing values will be dropped.
#' @param n_loadings Integer. Will plot the top n_loadings based on size.
#' @param loadings_list Character vector indicating for which OlinkID's to plot as loadings. It is possible to use n_loadings and loadings_list simultaneously.
#' @param byPanel Perform the PCA per panel (default FALSE)
#' @param outlierDefX The number standard deviations along the PC plotted on the x-axis that defines an outlier. See also 'Details"
#' @param outlierDefY The number standard deviations along the PC plotted on the y-axis that defines an outlier. See also 'Details"
#' @param outlierLines Draw dashed lines at +/-outlierDef[X,Y] standard deviations from the mean of the plotted PCs (default FALSE)
#' @param quiet Logical. If TRUE, the resulting plot is not printed
#' @param verbose Logical. Whether warnings about the number of samples and/or assays dropped or imputed should be printed to the console.
#' @param ... coloroption passed to specify color order.
#' @return A list of objects of class "ggplot", each plot contains scatter plot of PCs
#' @keywords NPX PCA
#' @export
#' @examples
#' \donttest{
#' library(dplyr)
#' npx_data <- npx_data1 %>%
#'     mutate(SampleID = paste(SampleID, "_", Index, sep = ""))
#'
#' #PCA using all the data
#' olink_pca_plot(df=npx_data, color_g = "QC_Warning")
#'
#' #PCA per panel
#' g <- olink_pca_plot(df=npx_data, color_g = "QC_Warning", byPanel = TRUE)
#' g[[2]] #Plot only the second panel
#'
#' #Label outliers
#' olink_pca_plot(df=npx_data, color_g = "QC_Warning",
#'                outlierDefX = 2, outlierDefY = 4) #All data
#' olink_pca_plot(df=npx_data, color_g = "QC_Warning",
#'                outlierDefX = 2.5, outlierDefY = 4, byPanel = TRUE) #Per panel
#'
#' #Retrieve the outliers
#' g <- olink_pca_plot(df=npx_data, color_g = "QC_Warning",
#'                     outlierDefX = 2.5, outlierDefY = 4, byPanel = TRUE)
#' outliers <- lapply(g, function(x){x$data}) %>%
#'     bind_rows() %>%
#'     filter(Outlier == 1)
#' }
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select group_by ungroup mutate mutate_at if_else n_distinct summarise left_join arrange distinct
#' @importFrom stringr str_detect
#' @importFrom tidyr spread
#' @importFrom tidyselect all_of
#' @importFrom rlang ensym
#' @importFrom tibble column_to_rownames
#' @importFrom stats prcomp
#' @importFrom ggplot2 ggplot aes xlab ylab geom_text geom_point geom_segment  labs guides arrow
#' @importFrom ggrepel geom_label_repel
#' @importFrom utils head
#' @importFrom grid unit
#' @importFrom umap umap

olink_umap_plot <- function (df,
                             color_g = "QC_Warning",
                             x_val = 1,
                             y_val = 2,
                             config = umap.defaults,
                             label_samples = FALSE,
                             drop_assays = FALSE,
                             drop_samples = FALSE,
                             byPanel = FALSE,
                             outlierDefX = NA,
                             outlierDefY = NA,
                             outlierLines = FALSE,
                             quiet = FALSE,
                             verbose = TRUE,
                             ...){

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
  df <- df %>%
    dplyr::filter(stringr::str_detect(OlinkID,
                                      "OID[0-9]{5}"))

  #Check that the user didn't specify just one of outlierDefX and outlierDefY
  if(sum(c(is.numeric(outlierDefX), is.numeric(outlierDefY))) == 1){
    stop('To label outliers, both outlierDefX and outlierDefY have to be specified as numerical values')
  }

  #If outlierLines == TRUE, both outlierDefX and outlierDefY have to be specified
  if(outlierLines){
    if(!all(is.numeric(outlierDefX), is.numeric(outlierDefY))){
      stop('outlierLines requested but boundaries not specified. To draw lines, both outlierDefX and outlierDefY have to be specified as numerical values')
    }
  }

  if(byPanel){
    # Convert color_g variable to factor
    if(!is.factor(df[[paste(color_g)]])){
      df[[paste(color_g)]] <- as.factor(df[[paste(color_g)]])
    }
    df <- df %>%
      dplyr::mutate(Panel = Panel  %>% stringr::str_replace("Olink ", "")) #Strip "Olink" from the panel names

    plotList <- lapply(unique(df$Panel), function(x) {
      g <- df %>%
        dplyr::filter(Panel == x) %>%
        olink_umap_plot.internal(df = .,
                                 color_g = color_g,
                                 x_val = x_val,
                                 y_val = y_val,
                                 label_samples = label_samples,
                                 config = config,
                                 drop_assays = drop_assays,
                                 drop_samples = drop_samples,
                                 outlierDefX = outlierDefX,
                                 outlierDefY = outlierDefY,
                                 outlierLines = outlierLines,
                                 verbose = verbose,
                                 ...) +
        ggplot2::labs(title = x)

      #Add Panel info inside the ggplot object
      g$data <- g$data %>%
        dplyr::mutate(Panel = x)

      g
    })
    names(plotList) <- unique(df$Panel)
    if(!quiet) print(ggpubr::ggarrange(plotlist = plotList, common.legend = TRUE))

  } else{
    pca_plot <- olink_umap_plot.internal(df = df,
                                         color_g = color_g,
                                         x_val = x_val,
                                         y_val = y_val,
                                         label_samples = label_samples,
                                         config = config,
                                         drop_assays = drop_assays,
                                         drop_samples = drop_samples,
                                         outlierDefX = outlierDefX,
                                         outlierDefY = outlierDefY,
                                         outlierLines = outlierLines,
                                         verbose = verbose,
                                         ...)
    if(!quiet) print(pca_plot)
    plotList <- list(pca_plot) #For consistency, return a list even when there's just one plot
  }

  return(invisible(plotList))
}

olink_umap_plot.internal <- function (df,
                                     color_g = "QC_Warning",
                                     x_val = 1,
                                     y_val = 2,
                                     label_samples = FALSE,
                                     drop_assays = FALSE,
                                     drop_samples = FALSE,
                                     n_loadings = 0,
                                     loadings_list = NULL,
                                     outlierDefX,
                                     outlierDefY,
                                     outlierLines,
                                     verbose = TRUE,
                                     ...){

  ### Data pre-processing ###
  procData <- npxProcessing_forDimRed(df = df,
                                      color_g = color_g,
                                      drop_assays = drop_assays,
                                      drop_samples = drop_samples,
                                      verbose = verbose)

  #### UMAP ####
  #Determine number of UMAP components
  n_components <- config$n_components
  if(max(c(x_val, y_val)) > n_components){
    n_components <- max(c(x_val, y_val))
  }

  umap_fit <- umap(procData$df_wide_matrix, config = config, n_components = n_components)
  umapX <- umap_fit$layout[, x_val]
  umapY <- umap_fit$layout[, y_val]
  observation_names <- procData$df_wide$SampleID
  observation_colors <- procData$df_wide$pca_colors
  scores <- data.frame(umapX, umapY)

  #Identify outliers
  if(!is.na(outlierDefX) & !is.na(outlierDefY)){
    scores <- scores %>%
      tibble::rownames_to_column(var = 'SampleID') %>%
      dplyr::mutate( umapX_low = mean(umapX, na.rm = TRUE) - outlierDefX*sd(umapX, na.rm = TRUE),
                     umapX_high = mean(umapX, na.rm = TRUE) + outlierDefX*sd(umapX, na.rm = TRUE),
                     umapY_low = mean(umapY, na.rm = TRUE) - outlierDefY*sd(umapY, na.rm = TRUE),
                     umapY_high = mean(umapY, na.rm = TRUE) + outlierDefY*sd(umapY, na.rm = TRUE)) %>%
      dplyr::mutate(Outlier = dplyr::if_else(umapX < umapX_high &
                                               umapX > umapX_low &
                                               umapY > umapY_low &
                                               umapY < umapY_high,
                                             0, 1))
  }

  #### Plotting ####
  umap_plot <- ggplot2::ggplot(scores, ggplot2::aes(x = umapX, y = umapY)) +
    ggplot2::xlab(paste0('UMAP', x_val)) +
    ggplot2::ylab(paste0('UMAP', y_val))


  #Drawing scores

  if(label_samples){

    umap_plot <- umap_plot +
      ggplot2::geom_text(ggplot2::aes(label = observation_names, color = observation_colors), size = 3) +
      ggplot2::labs(color = color_g) +
      ggplot2::guides(size = "none")

  }else{

    umap_plot <- umap_plot +
      ggplot2::geom_point(ggplot2::aes(color = observation_colors), size = 2.5) +
      ggplot2::labs(color = color_g) +
      ggplot2::guides(size = "none")

  }


  #Label outliers in figure
  if(!is.na(outlierDefX) & !is.na(outlierDefY)){
    umap_plot <- umap_plot +
      ggrepel::geom_label_repel(data = . %>% dplyr::mutate(SampleIDPlot = dplyr::case_when(Outlier == 1 ~ SampleID,
                                                                                           TRUE ~ "")),
                                ggplot2::aes(label=SampleIDPlot),
                                box.padding = 0.5,
                                min.segment.length = 0.1,
                                show.legend=FALSE,
                                size = 3)
  }

  #Add outlier lines
  if(outlierLines){
    umap_plot <- umap_plot +
      ggplot2::geom_hline(ggplot2::aes(yintercept=PCY_low),
                          linetype = 'dashed',
                          color = 'grey') +
      ggplot2::geom_hline(ggplot2::aes(yintercept=PCY_high),
                          linetype = 'dashed',
                          color = 'grey') +
      ggplot2::geom_vline(ggplot2::aes(xintercept = PCX_low),
                          linetype = 'dashed',
                          color = 'grey') +
      ggplot2::geom_vline(ggplot2::aes(xintercept = PCX_high),
                          linetype = 'dashed',
                          color = 'grey')
  }



  umap_plot <- umap_plot +
    OlinkAnalyze::set_plot_theme() +
    OlinkAnalyze::olink_color_discrete(...,drop=FALSE)

  return(umap_plot)


}
