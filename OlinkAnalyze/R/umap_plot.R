#' Function to make a UMAP plot from the data
#'
#' NOTE: This is a beta version.
#' Computes a manifold approximation and projection using umap::umap and plots the two specified components. 
#' Unique sample names are required and imputation by the median is done for assays with missingness <10\% for multi-plate projects and <5\% for single plate projects.
#'
#' @param df data frame in long format with Sample Id, NPX and column of choice for colors
#' @param color_g Character value indicating which column to use for colors (default QC_Warning)
#' @param x_val Integer indicating which UMAP component to plot along the x-axis (default 1)
#' @param y_val Integer indicating which UMAP component to plot along the y-axis (default 2)
#' @param label_samples Logical. If TRUE, points are replaced with SampleID (default FALSE)
#' @param config object of class umap.config, specifying the parameters for the UMAP algorithm. 
#' @param drop_assays Logical. All assays with any missing values will be dropped. Takes precedence over sample drop.
#' @param drop_samples Logical. All samples with any missing values will be dropped.
#' @param returnEmbedding Logical. If true, the UMAP results will be returned.
#' @param verbose Logical. Whether warnings about the number of samples and/or assays dropped or imputed should be printed to the console.
#' @param ... coloroption passed to specify color order.
#' @return If returnEmbedding == FALSE (default): An object of class "ggplot". If returnEmbedding == TRUE: a list with 1. an object of class "ggplot", 2. an object of class umap
#' @keywords NPX, UMAP
#' @export
#' @examples
#' \donttest{
#' npx_data <- npx_data1 %>%
#'     mutate(SampleID = paste(SampleID, "_", Index, sep = ""))
#' olink_pca_plot(df=npx_data, color_g = "QC_Warning")}
#' @import dplyr stringr tidyr ggfortify ggrepel umap

olink_umap_plot <- function (df,
                             color_g = "QC_Warning",
                             x_val = 1,
                             y_val = 2,
                             label_samples = FALSE,
                             config = umap.defaults,
                             drop_assays = FALSE,
                             drop_samples = FALSE,
                             returnEmbedding = FALSE,
                             verbose = TRUE,
                             ...)
{
  if (length(list(...)) > 0) {
    ellipsis_variables <- names(list(...))
    if (length(ellipsis_variables) == 1) {
      if (!(ellipsis_variables == "coloroption")) {
        stop(paste0("The ... option only takes the coloroption argument. ... currently contains the variable ", ellipsis_variables, "."))
      }
    }
    else {
      stop(paste0("The ... option only takes one argument. ... currently contains the variables ", paste(ellipsis_variables, collapse = ", "), "."))
    }
  }
  df <- df %>% filter(stringr::str_detect(OlinkID, "OID[0-9]{5}"))
  if (color_g == "QC_Warning") {
    df_temp <- df %>% 
      group_by(SampleID, Index) %>% 
      mutate(QC_Warning = if_else(any(QC_Warning == "Warning" | QC_Warning == "WARN"), "Warning", "Pass")) %>% 
      ungroup()
    colors_for_pca <- df_temp %>% 
      group_by(SampleID, Index) %>% 
      summarise(pca_colors = unique(!!rlang::ensym(color_g))) %>% 
      ungroup()
  }
  else {
    number_of_sample_w_more_than_one_color <- df %>% 
      group_by(SampleID, Index) %>% 
      summarise(n_colors = n_distinct(!!rlang::ensym(color_g), na.rm = TRUE)) %>% 
      ungroup() %>% filter(n_colors > 1) %>% 
      nrow(.)
    if (number_of_sample_w_more_than_one_color > 0) {
      stop(paste0("There are ", number_of_sample_w_more_than_one_color, " samples that do not have a unique color. Only one color per sample is allowed."))
    }
    else {
      df_temp <- df
      colors_for_pca <- df_temp %>% 
        group_by(SampleID, Index) %>% 
        summarise(pca_colors = unique(!!rlang::ensym(color_g))) %>% 
        ungroup()
    }
  }
  df_temp <- df_temp %>% 
    group_by(OlinkID) %>% 
    mutate(assay_var = var(NPX, na.rm = TRUE)) %>% 
    ungroup() %>% 
    filter(!(assay_var == 0 | is.na(assay_var))) %>% 
    select(-assay_var)
  
  # return(df_temp)
  df_wide <- df_temp %>% 
    select(SampleID, Index, OlinkID, NPX) %>% 
    filter(!is.na(NPX)) %>% 
    spread(OlinkID, NPX)
  
  if (drop_assays) {
    dropped_assays <- colnames(df_wide[, -c(1:2)])[apply(df_wide[, -c(1:2)], 2, anyNA)]
    df_wide <- df_wide %>% 
      select(-tidyselect::all_of(dropped_assays))
    if (verbose) {
      warning(paste0(length(dropped_assays)), " assay(s) contain NA and are dropped. ")
    }
    if (ncol(df_wide) < 4) {
      stop("Too many assays removed. Set drop_assays = F for imputation.")
    }
  }
  if (drop_samples) {
    dropped_samples <- apply(df_wide[, -c(1:2)], 1, anyNA)
    df_wide <- df_wide[!dropped_samples, ]
    if (verbose) {
      warning(paste0(sum(dropped_samples)), " sample(s) contain NA and are dropped. ")
    }
    if (nrow(df_wide) < 2) {
      stop("Too many samples removed. Set drop_samples = F for imputation.")
    }
  }
  percent_missingness <- colSums(is.na(df_wide[, -c(1:2)]))/nrow(df_wide)
  PERCENT_CUTOFF <- 0.1
  if (nrow(df_wide) <= 88) {
    PERCENT_CUTOFF <- 0.05
  }
  if (any(percent_missingness > PERCENT_CUTOFF)) {
    removed_assays_index <- which(percent_missingness > PERCENT_CUTOFF)
    percent_missingness <- percent_missingness[-removed_assays_index]
    removed_assays_index <- removed_assays_index + 2
    removed_assays <- colnames(df_wide)[removed_assays_index]
    df_wide <- df_wide[, -removed_assays_index]
    if (verbose) {
      warning(paste0("There are ", paste0(length(removed_assays)), 
                     " assay(s) dropped due to high missingness (>", 
                     round(PERCENT_CUTOFF * 100), "%)."))
    }
  }
  if (any(percent_missingness <= PERCENT_CUTOFF & percent_missingness > 0)) {
    nrNAs <- df_wide[,-(1:2)] %>% 
      is.na() %>%
      sum()
    nObs <- df_wide[,-(1:2)] %>% 
      dim() %>% 
      prod()
    
    imputed_assays_index <- which(percent_missingness <= PERCENT_CUTOFF & percent_missingness > 0)
    percent_missingness <- percent_missingness[-imputed_assays_index]
    imputed_assays_index <- imputed_assays_index + 2
    imputed_assays <- colnames(df_wide)[imputed_assays_index]
    df_wide <- df_wide %>% 
      mutate_at(tidyselect::all_of(imputed_assays), ~ifelse(is.na(.x), median(.x, na.rm = TRUE), .x))
    if (verbose) {
      warning(paste0(nrNAs, ' missing NPX values, out of ', 
                     nObs, ' total observations (', signif(100*nrNAs/nObs, 3), '%), across ', 
                     length(imputed_assays), " assay(s) were imputed by their medians."))
    }
  }
  if (!all(colSums(is.na(df_wide[, -c(1:2)])) == 0)) {
    stop("Missingness imputation failed.")
  }
  df_wide <- df_wide %>% 
    left_join(colors_for_pca, by = c("SampleID", "Index")) %>% 
    select(SampleID, Index, pca_colors, everything())
  
  df_wide_matrix <- df_wide %>% 
    select(-Index, -pca_colors) %>% 
    column_to_rownames("SampleID") %>% 
    as.matrix
  
  #### Umapping ####
  #Determine number of UMAP components
  n_components <- config$n_components
  if(max(c(x_val, y_val)) > n_components){
    n_components <- max(c(x_val, y_val))
  }
  
  umap_fit <- umap(df_wide_matrix, config = config, n_components = n_components)
  umapX <- umap_fit$layout[, x_val]
  umapY <- umap_fit$layout[, y_val]
  observation_names <- df_wide$SampleID
  observation_colors <- df_wide$pca_colors
  scores <- cbind(umapX, umapY)
  
  umap_plot <- ggplot(scores, aes(x = umapX, y = umapY)) + 
    xlab(paste0('UMAP', x_val)) + ylab(paste0('UMAP', y_val))
  
  if (label_samples) {
    umap_plot <- umap_plot + 
      geom_text(aes(label = observation_names, color = observation_colors), size = 3) + 
      labs(color = color_g) + 
      guides(size = FALSE)
  }
  else {
    umap_plot <- umap_plot + 
      geom_point(aes(color = observation_colors), size = 2.5) + 
      labs(color = color_g) + 
      guides(size = FALSE)
  }
  
  umap_plot <- umap_plot + set_plot_theme() + olink_color_discrete(...)
  if(returnEmbedding){
    return(list(plot = umap_plot, 
                umap = umap_fit))
  }
  else{
    return(umap_plot)
  }
}
