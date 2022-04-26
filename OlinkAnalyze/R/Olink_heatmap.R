#' Function to plot a heatmap of the NPX data
#'
#' Generates a heatmap using \code{pheatmap::pheatmap} of all samples from NPX data.
#'
#' The values are by default scaled across and centered in the heatmap. Columns
#' and rows are by default sorted by by dendrogram.
#' Unique sample names are required.
#'
#' @param df Data frame in long format with SampleID, NPX, OlinkID, Assay and columns of choice for annotations.
#' @param variable_list Columns in \code{df} to be annotated.
#' @param scale_data Logical. If data should be scaled and centered across assays (default \code{TRUE}).
#' @param cluster_rows Logical. Determining if rows should be clustered (default \code{TRUE}).
#' @param cluster_cols Logical. Determining if columns should be clustered (default \code{TRUE}).
#' @param show_rownames Logical. Determining if row names are shown (default \code{TRUE}).
#' @param show_colnames Logical. Determining if column names are shown (default \code{TRUE}).
#' @param annotation_legend Logical. Determining if legend for annotations should be shown (default \code{TRUE}).
#' @param fontsize Fontsize (default 10)
#' @param na_col Color of cells with \code{NA} (default black)
#' @param ... Additional arguments used in \code{pheatmap::pheatmap}
#' @keywords NPX Heatmap
#' @export
#' @examples
#' \donttest{
#' library(dplyr)
#' npx_data <- npx_data1 %>%
#'       filter(!str_detect(SampleID,'CONT'))
#'
#' #Heatmap
#' olink_pca_heatmap(df=npx_data)
#'
#' #Heatmap with annotation
#' olink_pca_heatmap(df=npx_data, variable_list = c('Time','Site'))
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter group_by summarise ungroup select mutate across
#' @importFrom stringr str_detect
#' @importFrom tidyr pivot_wider
#' @importFrom tibble column_to_rownames
#' @importFrom pheatmap pheatmap

olink_heatmap_plot <- function(df,
                               variable_list     = NULL,
                               scale_data        = TRUE,
                               cluster_rows      = TRUE,
                               cluster_cols      = TRUE,
                               show_rownames     = TRUE,
                               show_colnames     = TRUE,
                               annotation_legend = TRUE,
                               fontsize          = 10,
                               na_col            = 'black',
                               ...) {
  
  
  #Force data frame as tibble
  if (!tibble::is_tibble(df)) {
    df <- tibble::as_tibble(df)
  }
  
  #Filtering on valid OlinkID
  df_temp<-df %>%
    dplyr::filter(stringr::str_detect(OlinkID,
                                      "OID[0-9]{5}"))
  
  #Halt if muliple samplenames
  nr_dup<-df_temp %>%
    dplyr::group_by(OlinkID, SampleID) %>%
    dplyr::summarise(N = n(), .groups = 'keep') %>%
    dplyr::ungroup() %>%
    dplyr::select(SampleID, N) %>%
    dplyr::filter(N > 1) %>%
    unique()
  
  if (nr_dup %>% nrow == 1) {
    stop(paste0('The following sampleID is not unique: ',
                nr_dup$SampleID %>% toString(),
                '.'))
  }
  if(nr_dup %>% nrow > 1) {
    stop(paste0('The following sampleIDs are not unique: ',
                nr_dup$SampleID  %>% toString(),
                '.'))
  }
  
  #Remove assays with no variance
  df_temp2 <- df_temp %>%
    dplyr::group_by(OlinkID) %>%
    dplyr::mutate(assay_var = var(NPX, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!(assay_var == 0 | is.na(assay_var))) %>%
    dplyr::select(-assay_var)
  
  #Remove samples with only NA
  Sample_to_remove<-df_temp2 %>%
    dplyr::group_by(SampleID) %>%
    dplyr::mutate(NrNA = ifelse(is.na(NPX),0,1)) %>%
    dplyr::summarise(SumNA = sum(NrNA)) %>%
    dplyr::filter(SumNA == 0)
  
  if(Sample_to_remove %>% nrow > 0){
    if (Sample_to_remove %>% nrow == 1) {
      print(paste0('The following sampleID ',
                   Sample_to_remove$SampleID %>% toString(),
                   ' is removed due all NPX values being NA.'))
    }
    if (Sample_to_remove %>% nrow > 1) {
      print(paste0('The following sampleIDs ',
                   Sample_to_remove$SampleID %>% toString(),
                   ' are removed due all NPX values being NA.'))
    }
    
    df_temp2<- df_temp2 %>%
      filter(!(SampleID %in% Sample_to_remove$SampleID))
  }
  
  npxWide<-df_temp2 %>%
    mutate(Assay_OlinkID = paste0(Assay,'_',OlinkID)) %>%
    tidyr::pivot_wider(id_cols = SampleID,
                       names_from = Assay_OlinkID,
                       values_from = NPX) %>%
    tibble::column_to_rownames('SampleID')
  
  scale <- ifelse(scale_data, "column", "none")
  
  # Prepare argument list
  pheatmap_args <- list(
    mat               = npxWide,
    scale             = scale,
    cluster_rows      = cluster_rows,
    cluster_cols      = cluster_cols,
    na_col            = na_col,
    show_rownames     = show_rownames,
    show_colnames     = show_colnames,
    annotation_legend = annotation_legend,
    fontsize          = fontsize
  )
  
  # Add annotations to call, if requested
  if (!is.null(variable_list)){
    variable_df <- df %>%
      select(SampleID, variable_list) %>%
      unique() %>%
      column_to_rownames('SampleID')
    pheatmap_args[["annotation_row"]] <- variable_df
  }
  
  # Check ellipsis and add further arguments, if requested
  n_ellipsis <- length(list(...))
  if (n_ellipsis > 0L) {
    ellipsis_variables <- names(list(...))
    
    for (i in 1L:n_ellipsis) {
      if (ellipsis_variables[i] %in% c("mat", "scale", "annotation_row")) {
        # These arguments should not be over-written
        warning(paste0("Argument '", ellipsis_variables[i] ,"' to pheatmap cannot be manually set - ignoring"))
      }
      else {
        # Add to call
        pheatmap_args[[ ellipsis_variables[i] ]] <- list(...)[[i]]
      }
    }
  }
  
  # Run call
  tryCatch({ do.call(pheatmap::pheatmap, args=pheatmap_args) },
           error = function(e){
             if(grepl("NA/NaN/Inf", e$message, fixed = TRUE)){
               print("Error when clustering. Try setting cluster of rows or columns to FALSE.")
             }
             else{
               e$message
             }
           })
}
