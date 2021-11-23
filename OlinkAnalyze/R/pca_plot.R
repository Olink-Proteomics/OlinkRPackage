#' Function to plot a PCA of the data
#'
#' Generates a PCA projection of all samples from NPX data along two principal components (default PC2 vs. PC1) including the explained variance and dots colored by QC_Warning using stats::prcomp and ggplot2::ggplot.
#' The values are by default scaled and centered in the PCA and proteins with missing NPX values are by default removed from the corresponding assay.
#' Unique sample names are required.
#' Imputation by the median is done for assays with missingness <10\% for multi-plate projects and <5\% for single plate projects.
#'
#' The plot is printed, and a list of ggplot objects is returned. \cr\cr
#' If byPanel = TRUE, the data processing (imputation of missing values etc) and subsequent PCA is performed separately per panel. A faceted plot is printed, while the individual ggplot objects are returned.
#'
#' @param df data frame in long format with Sample Id, NPX and column of choice for colors
#' @param color_g Character value indicating which column to use for colors (default QC_Warning)
#' @param x_val Integer indicating which principal component to plot along the x-axis (default 1)
#' @param y_val Integer indicating which principal component to plot along the y-axis (default 2)
#' @param label_samples Logical. If TRUE, points are replaced with SampleID (default FALSE)
#' @param drop_assays Logical. All assays with any missing values will be dropped. Takes precedence over sample drop.
#' @param drop_samples Logical. All samples with any missing values will be dropped.
#' @param n_loadings Integer. Will plot the top n_loadings based on size.
#' @param loadings_list Character vector indicating for which OlinkID's to plot as loadings. It is possible to use n_loadings and loadings_list simultaneously.
#' @param byPanel Perform the PCA per panel (default FALSE)
#' @param verbose Logical. Whether warnings about the number of samples and/or assays dropped or imputed should be printed to the console.
#' @param ... coloroption passed to specify color order.
#' @return A list of objects of class "ggplot"
#' @keywords NPX, PCA
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
#' g <- olink_pca_plot(df=npx_data, color_g = "QC_Warning", byPanel = T)
#' g[[2]] #Plot only the second panel
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

olink_pca_plot <- function (df,
                            color_g = "QC_Warning",
                            x_val = 1,
                            y_val = 2,
                            label_samples = FALSE,
                            drop_assays = FALSE,
                            drop_samples = FALSE,
                            n_loadings = 0,
                            loadings_list = NULL,
                            byPanel = F,
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

  if(byPanel){
    plotList <- lapply(unique(df$Panel), function(x) {
      df %>%
        dplyr::filter(Panel == x) %>%
        olink_pca_plot.internal(df = .,
                                color_g = color_g,
                                x_val = x_val,
                                y_val = y_val,
                                label_samples = label_samples,
                                drop_assays = drop_assays,
                                drop_samples = drop_samples,
                                n_loadings = n_loadings,
                                loadings_list = loadings_list,
                                verbose = verbose,
                                ...) +
        ggplot2::labs(title = x)

    })
    print(ggpubr::ggarrange(plotlist = plotList, common.legend = T))

  } else{
    pca_plot <- olink_pca_plot.internal(df = df,
                                        color_g = color_g,
                                        x_val = x_val,
                                        y_val = y_val,
                                        label_samples = label_samples,
                                        drop_assays = drop_assays,
                                        drop_samples = drop_samples,
                                        n_loadings = n_loadings,
                                        loadings_list = loadings_list,
                                        verbose = verbose,
                                        ...)
    print(pca_plot)
    plotList <- list(pca_plot) #For consistency, return a list even when there's just one plot
  }

  return(invisible(plotList))
}

olink_pca_plot.internal <- function (df,
                                     color_g = "QC_Warning",
                                     x_val = 1,
                                     y_val = 2,
                                     label_samples = FALSE,
                                     drop_assays = FALSE,
                                     drop_samples = FALSE,
                                     n_loadings = 0,
                                     loadings_list = NULL,
                                     byPanel = F,
                                     verbose = TRUE,
                                     ...){

  if (color_g == "QC_Warning"){

    df_temp <- df %>%
      dplyr::group_by(SampleID, Index) %>%
      dplyr::mutate(QC_Warning = dplyr::if_else(any(QC_Warning == "Warning"|QC_Warning == "WARN" ), "Warning", "Pass")) %>%
      dplyr::ungroup()

    colors_for_pca <- df_temp %>%
      dplyr::group_by(SampleID, Index) %>%
      dplyr::summarise(pca_colors = unique(!!rlang::ensym(color_g))) %>%
      dplyr::ungroup()


  } else {

    number_of_sample_w_more_than_one_color <- df %>%
      dplyr::group_by(SampleID, Index) %>%
      dplyr::summarise(n_colors = dplyr::n_distinct(!!rlang::ensym(color_g), na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(n_colors > 1) %>%
      nrow(.)

    if(number_of_sample_w_more_than_one_color > 0) {

      stop(paste0("There are ", number_of_sample_w_more_than_one_color, " samples that do not have a unique color. Only one color per sample is allowed."))

    }else{

      df_temp <- df

      colors_for_pca <- df_temp %>%
        dplyr::group_by(SampleID, Index) %>%
        dplyr::summarise(pca_colors = unique(!!rlang::ensym(color_g))) %>%
        dplyr::ungroup()

    }

  }

  #Checking if there are any proteins with 0 variance, they are filtered out

  df_temp <- df_temp %>%
    dplyr::group_by(OlinkID) %>%
    dplyr::mutate(assay_var = var(NPX, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!(assay_var == 0 | is.na(assay_var))) %>%
    dplyr::select(-assay_var)

  #wide format

  df_wide <- df_temp %>%
    dplyr::select(SampleID, Index, OlinkID, NPX) %>%
    dplyr::filter(!is.na(NPX)) %>%
    tidyr::spread(OlinkID, NPX)


  #Dropping any cols with NA
  #drop_assays take precedence
  if(drop_assays){

    dropped_assays <- colnames(df_wide[, -c(1:2)])[apply(df_wide[, -c(1:2)], 2, anyNA)]

    df_wide <- df_wide %>%
      dplyr::select(-tidyselect::all_of(dropped_assays))

    if(verbose){
      warning(paste0(length(dropped_assays)),
              " assay(s) contain NA and are dropped. ")
    }

    if(!is.null(loadings_list)){

      dropped_loadings <- intersect(dropped_assays,
                                    loadings_list)


      if(length(dropped_loadings) > 0){

        if(verbose){
          warning(paste0("The loading(s) ",
                         paste0(dropped_loadings, collapse=", "),
                         " from the loadings_list contain NA and are dropped . "))
        }

        loadings_list <- setdiff(loadings_list, dropped_loadings)

        if(length(loadings_list) == 0){

          loadings_list <- NULL

        }
      }

    }

    if(ncol(df_wide) < 4){
      stop('Too many assays removed. Set drop_assays = F for imputation.')
    }
  }


  if(drop_samples){

    dropped_samples <- apply(df_wide[, -c(1:2)], 1, anyNA)

    df_wide <- df_wide[!dropped_samples, ]

    if(verbose){
      warning(paste0(sum(dropped_samples)),
              " sample(s) contain NA and are dropped. ")
    }

    if(nrow(df_wide) < 2){

      stop('Too many samples removed. Set drop_samples = F for imputation.')
    }

  }



  percent_missingness <- colSums(is.na(df_wide[, -c(1:2)]))/nrow(df_wide)

  # assays with missingness > 10% are dropped from the PCA
  PERCENT_CUTOFF <- 0.1

  #If there are fewer samples than one plate (88), the PERCENT_CUTOFF is 0.05
  if(nrow(df_wide) <= 88){
    PERCENT_CUTOFF <- 0.05
  }

  if(any(percent_missingness > PERCENT_CUTOFF)){

    removed_assays_index <- which(percent_missingness > PERCENT_CUTOFF)
    percent_missingness <- percent_missingness[-removed_assays_index]

    removed_assays_index <- removed_assays_index + 2
    removed_assays <- colnames(df_wide)[removed_assays_index]

    df_wide <- df_wide[, -removed_assays_index]

    if(verbose){
      warning(paste0("There are ",
                     paste0(length(removed_assays)),
                     " assay(s) dropped due to high missingness (>",
                     round(PERCENT_CUTOFF*100),
                     "%)."))
    }

    if(!is.null(loadings_list)){

      dropped_loadings <- intersect(removed_assays,
                                    loadings_list)


      if(length(dropped_loadings) > 0){

        if(verbose){
          warning(paste0("The loading(s) ",
                         paste0(dropped_loadings, collapse=", "),
                         " from the loadings_list are dropped due to high missingness. "))
        }

        loadings_list <- setdiff(loadings_list, dropped_loadings)

        if(length(loadings_list) == 0){

          loadings_list <- NULL

        }
      }

    }

  }

  #<= PERCENT_CUTOFF assays imputed

  if(any(percent_missingness <= PERCENT_CUTOFF & percent_missingness > 0)){

    imputed_assays_index <- which(percent_missingness <= PERCENT_CUTOFF & percent_missingness > 0)
    percent_missingness <- percent_missingness[-imputed_assays_index]

    imputed_assays_index <- imputed_assays_index + 2
    imputed_assays <- colnames(df_wide)[imputed_assays_index]

    df_wide <- df_wide %>%
      dplyr::mutate_at(tidyselect::all_of(imputed_assays),
                       ~ifelse(is.na(.x), median(.x, na.rm = TRUE), .x))

    if(verbose){
      warning(paste0("There are ",
                     paste0(length(imputed_assays)),
                     " assay(s) that were imputed by their medians."))
    }
  }

  if(!all(colSums(is.na(df_wide[, -c(1:2)])) == 0)){
    stop('Missingness imputation failed.')
  }

  df_wide <- df_wide %>%
    dplyr::left_join(colors_for_pca,
                     by = c('SampleID',
                            'Index')) %>%
    dplyr::select(SampleID, Index, pca_colors, everything())

  df_wide_matrix <- df_wide %>%
    dplyr::select(-Index, -pca_colors) %>%
    tibble::column_to_rownames('SampleID') %>%
    as.matrix

  pca_fit <- stats::prcomp(df_wide_matrix, scale. = TRUE, center = TRUE)

  #Standardizing and selecting components

  scaling_factor_lambda <- pca_fit$sdev*sqrt(nrow(df_wide_matrix))

  PCX <- pca_fit$x[,x_val]/scaling_factor_lambda[x_val]
  PCY <- pca_fit$x[,y_val]/scaling_factor_lambda[y_val]
  PoV <- pca_fit$sdev^2/sum(pca_fit$sdev^2)
  LX <- pca_fit$rotation[, x_val]
  LY <- pca_fit$rotation[, y_val]

  observation_names <- df_wide$SampleID
  observation_colors <- df_wide$pca_colors

  scores <- data.frame(cbind(PCX, PCY))
  loadings <- data.frame(variables = rownames(pca_fit$rotation), LX, LY)

  range_PX <- c(-abs(min(PCX, na.rm = TRUE)), abs(max(PCX, na.rm = TRUE)))
  range_PY <- c(-abs(min(PCY, na.rm = TRUE)), abs(max(PCY, na.rm = TRUE)))
  range_LX <- c(-abs(min(LX, na.rm = TRUE)), abs(max(LX, na.rm = TRUE)))
  range_LY <- c(-abs(min(LY, na.rm = TRUE)), abs(max(LY, na.rm = TRUE)))

  loadings_scaling_factor <- 0.8/max(range_LX/range_PX, range_LY/range_PY)

  #Plotting

  pca_plot <- ggplot2::ggplot(scores, ggplot2::aes(x = PCX, y = PCY)) +
    ggplot2::xlab(paste0("PC", x_val,  " (", round(PoV[x_val]*100, digits = 2), "%)")) +
    ggplot2::ylab(paste0("PC", y_val, " (", round(PoV[y_val]*100, digits = 2), "%)"))


  #Drawing scores

  if(label_samples){

    pca_plot <- pca_plot +
      ggplot2::geom_text(ggplot2::aes(label = observation_names, color = observation_colors), size = 3) +
      ggplot2::labs(color = color_g) +
      ggplot2::guides(size = "none")

  }else{

    pca_plot <- pca_plot +
      ggplot2::geom_point(ggplot2::aes(color = observation_colors), size = 2.5) +
      ggplot2::labs(color = color_g) +
      ggplot2::guides(size = "none")

  }


  #Drawing loadings

  if(n_loadings > 0 | !is.null(loadings_list)) {

    N_loadings <- data.frame(matrix(vector(), 0, ncol(loadings)),
                             stringsAsFactors=FALSE)
    colnames(N_loadings) <- colnames(loadings)

    L_loadings <- N_loadings

    if(n_loadings > 0){

      #Largest loadings based on Pythagoras

      N_loadings <- loadings %>%
        dplyr::mutate(abs_loading = sqrt(LX^2 + LY^2)) %>%
        dplyr::arrange(desc(abs_loading)) %>%
        utils::head(n_loadings) %>%
        dplyr::select(-abs_loading)
    }

    if(!is.null(loadings_list)){

      #Selected loadings

      L_loadings <- loadings %>%
        dplyr::filter(variables %in% loadings_list)
    }

    loadings <- rbind(N_loadings,
                      L_loadings) %>%
      dplyr::distinct()

    pca_plot <- pca_plot +
      ggplot2::geom_segment(data = loadings,
                            ggplot2::aes(x = 0,
                                         y = 0,
                                         xend = LX*loadings_scaling_factor,
                                         yend = LY*loadings_scaling_factor),
                            arrow = ggplot2::arrow(length = grid::unit(1/2, "picas")),
                            color = "black") +
      ggrepel::geom_label_repel(data = loadings,
                                ggplot2::aes(x = LX*loadings_scaling_factor,
                                             y = LY*loadings_scaling_factor,
                                             label = variables),
                                box.padding = 1,
                                show.legend = FALSE,
                                segment.colour = 'gray')
  }


  pca_plot <- pca_plot +
    OlinkAnalyze::set_plot_theme() +
    OlinkAnalyze::olink_color_discrete(...)

  return(pca_plot)


}
