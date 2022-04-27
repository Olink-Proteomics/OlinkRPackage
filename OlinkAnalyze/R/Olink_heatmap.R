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
#' @param center_scale Logical. If data should be centered and scaled across assays (default \code{TRUE}).
#' @param cluster_rows Logical. Determining if rows should be clustered (default \code{TRUE}).
#' @param cluster_cols Logical. Determining if columns should be clustered (default \code{TRUE}).
#' @param show_rownames Logical. Determining if row names are shown (default \code{TRUE}).
#' @param show_colnames Logical. Determining if column names are shown (default \code{TRUE}).
#' @param annotation_legend Logical. Determining if legend for annotations should be shown (default \code{TRUE}).
#' @param fontsize Fontsize (default 10)
#' @param na_col Color of cells with \code{NA} (default black)
#' @param ... Additional arguments used in \code{pheatmap::pheatmap}
#' @return An object of class \code{ggplot}, generated from the \code{gtable} returned by \code{pheatmap::pheatmap}.
#' @keywords NPX Heatmap
#' @export
#' @examples
#' \donttest{
#' library(dplyr)
#' npx_data <- npx_data1 %>%
#'       filter(!stringr::str_detect(SampleID,'CONT'))
#'
#' #Heatmap
#' olink_heatmap_plot(df=npx_data)
#'
#' #Heatmap with annotation
#' olink_heatmap_plot(df=npx_data, variable_list = c('Time','Site'))
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter group_by summarise ungroup select mutate across
#' @importFrom stringr str_detect
#' @importFrom tidyr pivot_wider
#' @importFrom tibble column_to_rownames
#' @importFrom pheatmap pheatmap
#' @importFrom ggplotify as.ggplot

olink_heatmap_plot <- function(df,
                               variable_list     = NULL,
                               center_scale      = TRUE,
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
      warning(paste0('The following sampleID ',
                     Sample_to_remove$SampleID %>% toString(),
                     ' is removed due all NPX values being NA.'))
    }
    if (Sample_to_remove %>% nrow > 1) {
      warning(paste0('The following sampleIDs ',
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
  
  scale <- ifelse(center_scale, "column", "none")
  
  # Prepare argument list
  pheatmap_args <- list(
    mat               = npxWide,
    scale             = scale,
    silent            = TRUE,
    cluster_rows      = cluster_rows,
    cluster_cols      = cluster_cols,
    na_col            = na_col,
    show_rownames     = show_rownames,
    show_colnames     = show_colnames,
    annotation_legend = annotation_legend,
    fontsize          = fontsize)
  
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
      if (ellipsis_variables[i] %in% c("mat", "silent", "scale", "annotation_row")) {
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
  tryCatch({ p <- do.call(pheatmap::pheatmap, args=pheatmap_args)$gtable },
           error = function(e){
             if(grepl("NA/NaN/Inf", e$message, fixed = TRUE)){
               stop("Error when clustering. Try setting cluster of rows or columns to FALSE.")
             }
             else{
               stop(e$message)
             }
           },
           finally = {
             # Apply retroactive theming for the existing parts of the plot
             p <- set_plot_theme_pheatmap(p, fontsize=fontsize)
             # Convert to ggplot object, and set theming (which applies to components added post-hoc)
             return( ggplotify::as.ggplot(p) +
                       OlinkAnalyze::set_plot_theme() +
                       ggplot2::theme(text       = ggplot2::element_text(size=fontsize),
                                      axis.line  = ggplot2::element_blank(),
                                      axis.ticks = ggplot2::element_blank(),
                                      axis.text  = ggplot2::element_blank()) +
                       ggplot2::xlab(NULL) +
                       ggplot2::ylab(NULL) )
           }
  )
}

# set_plot_theme_pheatmap() is a non-exported function
# It emulates some of the styling that OlinkAnalyze::set_plot_theme() would otherwise provide for a ggplot
set_plot_theme_pheatmap <- function(x, fontsize, col="#737373", font="Swedish Gothic") {
  xnames <- x$layout$name

  # Prepare font
  set_font <- FALSE
  if (getOption("OlinkAnalyze.allow.font.load", default = TRUE)) {
    if (requireNamespace("extrafont", quietly = TRUE)) {
      if (font %in% extrafont::fonts()){
        if (.Platform$OS.type == "windows"){
          extrafont::loadfonts(quiet = TRUE, device = "win")
        }
        extrafont::loadfonts(quiet = TRUE, device = "pdf")
        set_font <- TRUE
      }
    }
    else if (requireNamespace("systemfonts", quietly = TRUE)) {
      # NOTE: This is a special for OI, where the above fails but the font should already have been registered
      if (font %in% unique(systemfonts::registry_fonts()$family)) {
        set_font <- TRUE
      }
    }
  }
  
  # Dendogram styling
  col_tree_i <- which(x$layout$name == "col_tree")
  row_tree_i <- which(x$layout$name == "row_tree")
  if (length(col_tree_i) > 0L) {
    x$grobs[[col_tree_i]] <- grid::editGrob(x$grobs[[col_tree_i]], gp=grid::gpar(col=col, lwd=0.4))
  }
  if (length(row_tree_i) > 0L) {
    x$grobs[[row_tree_i]] <- grid::editGrob(x$grobs[[row_tree_i]], gp=grid::gpar(col=col, lwd=0.4))
  }
  
  # Main title
  main_i <- which(x$layout$name == "main")
  if (length(main_i) > 0L) {
    if (set_font) {
      x$grobs[[main_i]] <- grid::editGrob(x$grobs[[main_i]], gp=grid::gpar(col=col, fontface="bold", fontfamily=font))
    }
    else {
      x$grobs[[main_i]] <- grid::editGrob(x$grobs[[main_i]], gp=grid::gpar(col=col, fontface="bold"))
    }
  }
  
  # Row / column names
  col_names_i <- which(x$layout$name == "col_names")
  row_names_i <- which(x$layout$name == "row_names")
  if (length(col_names_i) > 0L) {
    if (set_font) {
      x$grobs[[col_names_i]] <- grid::editGrob(x$grobs[[col_names_i]], gp=grid::gpar(col="black", fontsize=fontsize, fontfamily=font))
    }
    else {
      x$grobs[[col_names_i]] <- grid::editGrob(x$grobs[[col_names_i]], gp=grid::gpar(col="black", fontsize=fontsize))
    }
  }
  if (length(row_names_i) > 0L) {
    if (set_font) {
      x$grobs[[row_names_i]] <- grid::editGrob(x$grobs[[row_names_i]], gp=grid::gpar(col=col, fontsize=fontsize, fontfamily=font))
    }
    else {
      x$grobs[[row_names_i]] <- grid::editGrob(x$grobs[[row_names_i]], gp=grid::gpar(col=col, fontsize=fontsize))
    }
  }
  
  # Row annotation
  row_annotation_names_i <- which(x$layout$name == "row_annotation_names")
  if (length(row_annotation_names_i) > 0L) {
    if (set_font) {
      x$grobs[[row_annotation_names_i]] <- grid::editGrob(x$grobs[[row_annotation_names_i]], gp=grid::gpar(col="black", fontsize=fontsize, fontface="bold", fontfamily=font))
    }
    else {
      x$grobs[[row_annotation_names_i]] <- grid::editGrob(x$grobs[[row_annotation_names_i]], gp=grid::gpar(col="black", fontsize=fontsize, fontface="bold"))
    }
  }
  
  # Annotation legend
  annotation_legend_i <- which(x$layout$name == "annotation_legend")
  if (length(annotation_legend_i) > 0L) {
    if (set_font) {
      x$grobs[[annotation_legend_i]] <- grid::editGrob(x$grobs[[annotation_legend_i]], gp=grid::gpar(col=col, fontsize=fontsize, fontfamily=font))
    }
    else {
      x$grobs[[annotation_legend_i]] <- grid::editGrob(x$grobs[[annotation_legend_i]], gp=grid::gpar(col=col, fontsize=fontsize))
    }
  }
  
  # Standard legend
  legend_i <- which(x$layout$name == "legend")
  if (length(legend_i) > 0L) {
    if (set_font) {
      x$grobs[[legend_i]] <- grid::editGrob(x$grobs[[legend_i]], gp=grid::gpar(col=col, fontsize=fontsize, fontfamily=font))
    }
    else {
      x$grobs[[legend_i]] <- grid::editGrob(x$grobs[[legend_i]], gp=grid::gpar(col=col, fontsize=fontsize))
    }
  }
  
  x
}