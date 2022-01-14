#' Function to make an Autoencoder plot from the data
#'
#' NOTE: This is a beta version.
#' Trains a neural network using H2O and plots the learned features, referred to as codings, in a 2 dimensional figure.
#' Unique sample names are required and imputation by the median is done for assays with missingness <10\% for multi-plate projects and <5\% for single plate projects.
#'
#' @param df data frame in long format with Sample Id, NPX and column of choice for colors
#' @param color_g Character value indicating which column to use for colors (default QC_Warning)
#' @param label_samples Logical. If TRUE, points are replaced with SampleID (default FALSE)
#' @param activation Character string specifying the activation function in the neural network. Must be one of: "Tanh", "TanhWithDropout", "Rectifier", "RectifierWithDropout", "Maxout", "MaxoutWithDropout".
#' @param max_mem_size Character string specifying the maximum size, in bytes, of the memory allocation pool to H2O
#' @param layers Numeric vector specifying the sizes of the hidden layers. The number of layers must be odd, and the coding layer (assumed to be the middle one) must be 2. E.g. 2, c(x, 2, x), etc. (default 2, i.e. one hidden layer of size 2)
#' @param drop_assays Logical. All assays with any missing values will be dropped. Takes precedence over sample drop.
#' @param drop_samples Logical. All samples with any missing values will be dropped.
#' @param verbose Logical. Whether warnings about the number of samples and/or assays dropped or imputed should be printed to the console.
#' @param ... coloroption passed to specify color order.
#' @return An object of class "ggplot"
#' @keywords NPX, UMAP
#' @export
#' @examples
#' \donttest{
#' npx_data <- npx_data1 %>%
#'     mutate(SampleID = paste(SampleID, "_", Index, sep = ""))
#' olink_pca_plot(df=npx_data, color_g = "QC_Warning")}
#' @import dplyr stringr tidyr ggfortify ggrepel h2o

olink_autoencoder_plot <- function (df,
            color_g = "QC_Warning",
            label_samples = FALSE,
            activation = 'Tanh',
            max_mem_size = "5g",
            layers = 2,
            drop_assays = FALSE,
            drop_samples = FALSE,
            verbose = TRUE,
            ...)
  {
  if(!require(h2o)){
    stop('Could not load package h2o')
  }
  if(length(layers) %% 2 == 0){
    stop(paste(length(layers), 'hidden layers specified by the layers argument. This function expects a symmetric NN architecture with the coding layer in the middle. IE 1, 3, 5, ... hidden layers'))
  }
  else{
    codingLayer = ceiling(length(layers)/2)
  }
  if(verbose){
    message(paste(length(layers), 'hidden layer(s) specified. Codings will be extracted from layer', codingLayer))
  }
  if(layers[codingLayer] != 2){
    warning(paste0('The specified coding layer has the size ', layers[codingLayer], '. A size of 2 is expected.'))
  }

  if (length(list(...)) > 0) {
    ellipsis_variables <- names(list(...))
    if (length(ellipsis_variables) == 1) {
      if (!(ellipsis_variables == "coloroption")) {
        stop(paste0("The ... option only takes the coloroption argument. ... currently contains the variable ",
                    ellipsis_variables, "."))
      }
    }
    else {
      stop(paste0("The ... option only takes one argument. ... currently contains the variables ",
                  paste(ellipsis_variables, collapse = ", "), "."))
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

  #### Autoencoding ####
  h2o.no_progress()  # turn off progress bars
  h2o.init(max_mem_size = max_mem_size)  # initialize H2O instance
  features <- as.h2o(df_wide_matrix) # convert to an h2o input data set

  #Train the autoencoder
  ae1 <- h2o.deeplearning(
    x = seq_along(features),
    training_frame = features,
    autoencoder = T,
    hidden = layers,
    activation = activation
  )

  ae1_codings <- as.matrix(h2o.deepfeatures(ae1, features, layer = codingLayer)) # extract the deep features
  if(ncol(ae1_codings) != 2){
    warning('The extracted hidden layer has the size ', ncol(ae1_codings), '. Using the first two encodings')
  }
  h2o.shutdown() # shut down H2O instance

  autoX <- ae1_codings[,1]
  autoY <- ae1_codings[,2]
  scores <- data.frame(autoX, autoY)

  #I'm slightly uncomfortable with this. Is the row order always preserved from df_wide -> features -> ae1_codings? At least df_wide and features seem to agree
  scores$id <- df_wide$SampleID
  scores$color <- df_wide$pca_colors

  auto_plot <- ggplot(scores, aes(x = autoX, y = autoY)) +
    xlab('Encoding 1') + ylab('Encoding 2')

  if (label_samples) {
    auto_plot <- auto_plot +
      geom_text(aes(label = id, color = color), size = 3) +
      labs(color = color_g)
  }
  else {
    auto_plot <- auto_plot +
      geom_point(aes(color = color), size = 2.5) +
      labs(color = color_g)
  }

  auto_plot <- auto_plot + set_plot_theme() + olink_color_discrete(...)
  return(auto_plot)
}
