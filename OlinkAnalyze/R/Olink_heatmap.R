#' Function to plot a heatmap of the NPX data
#'
#' Generates a heatmap using \code{pheatmap::pheatmap} of all samples from NPX data.
#'
#' The values are by default scaled across and centered in the heatmap. Columns
#' and rows are by default sorted by by dendrogram.
#' Unique sample names are required.
#'
#' @param df Data frame in long format with SampleID, NPX, OlinkID, Assay and columns of choice for annotations.
#' @param variable_row_list Columns in \code{df} to be annotated for rows in the heatmap.
#' @param variable_col_list Columns in \code{df} to be annotated for columns in the heatmap.
#' @param center_scale Logical. If data should be centered and scaled across assays (default \code{TRUE}).
#' @param cluster_rows Logical. Determining if rows should be clustered (default \code{TRUE}).
#' @param cluster_cols Logical. Determining if columns should be clustered (default \code{TRUE}).
#' @param show_rownames Logical. Determining if row names are shown (default \code{TRUE}).
#' @param show_colnames Logical. Determining if column names are shown (default \code{TRUE}).
#' @param colnames Character. Determines how to label the columns. Must be 'assay', 'oid', or 'both' (default 'both').
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
#' try({ # This will fail if ggplotify is not installed
#' #Heatmap
#'   olink_heatmap_plot(df=npx_data)
#'
#' #Heatmap with annotation
#'   olink_heatmap_plot(df=npx_data, variable_row_list = c('Time','Site'))
#'
#' #Heatmap with calls from pheatmap
#'   olink_heatmap_plot(df=npx_data, cutree_rows = 3)
#' })
#'
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr filter group_by summarise ungroup select mutate
#' @importFrom stringr str_detect
#' @importFrom tidyr pivot_wider
#' @importFrom tibble column_to_rownames as_tibble
#' @importFrom grid editGrob gpar
#' @importFrom ggplot2 theme element_text element_blank xlab ylab

olink_heatmap_plot <- function(df,
                               variable_row_list = NULL,
                               variable_col_list = NULL,
                               center_scale      = TRUE,
                               cluster_rows      = TRUE,
                               cluster_cols      = TRUE,
                               show_rownames     = TRUE,
                               show_colnames     = TRUE,
                               colnames          = 'both',
                               annotation_legend = TRUE,
                               fontsize          = 10,
                               na_col            = 'black',
                               ...) {

  #Checking if packages are installed
  if(!requireNamespace("ggplotify", quietly = TRUE)){
    stop("Olink Heatmap function requires ggplotify package.
         Please install ggplotify before continuing.

         install.packages(\"ggplotify\")")
  }

  if(!requireNamespace("pheatmap", quietly = TRUE)){
    stop("Olink Heatmap function requires pheatmap package.
         Please install pheatmap before continuing.

         install.packages(\"pheatmap\")")
  }

  #Force data frame as tibble
  if (!tibble::is_tibble(df)) {
    df <- tibble::as_tibble(df)
  }

  #Save column classes
  col_classes <- sapply(df, "class")

  # Exclude OlinkIDs with missing NPX
  npx_check <- npxCheck(df)
  # Rename duplicate UniProts

  df <- df |>
    dplyr::mutate(UniProt = ifelse(OlinkID %in%
                                     npxCheck$uniprot_replace$OlinkID,
                                   npxCheck$uniprot_replace$new_UniProt,
                                   UniProt))

  #Filtering on valid OlinkID
  df_temp <- df %>%
    dplyr::filter(stringr::str_detect(OlinkID,
                                      "OID[0-9]{5}")) %>%
    dplyr::filter(!(OlinkID %in% npx_check$all_nas))


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

  # Label columns according to the colnames argument
  if(!colnames %in% c('assay', 'oid', 'both')){
    stop('colnames has to be \'assay\', \'oid\', or \'both\'')
  }
  if(colnames == 'assay'){
    assays <- sub(pattern = '(.*)_(OID.*)', replacement = '\\1', x = colnames(npxWide)) %>%
      as.data.frame() %>%
      dplyr::rename('Assay' = 1) %>%
      #If there are duplicate Assays (IL6 for instance), add a number to make the names unique
      dplyr::group_by(Assay) %>%
      dplyr::mutate(duplicateID = dplyr::row_number()) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(Assay = ifelse(duplicateID > 1, paste0(Assay, '_', duplicateID), Assay))

    colnames(npxWide) <- assays$Assay
  }
  if(colnames == 'oid'){
    colnames(npxWide) <- sub(pattern = '(.*)_(OID.*)', replacement = '\\2', x = colnames(npxWide))
  }

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

  # Check ellipsis and add further arguments, if requested
  annotation_colors_internal <- NULL
  n_ellipsis <- length(list(...))
  if (n_ellipsis > 0L) {
    ellipsis_variables <- names(list(...))

    for (i in 1L:n_ellipsis) {
      if (ellipsis_variables[i] == "annotation_colors") {
        # This is handled below
        annotation_colors_internal <- list(...)[[i]]
      }
      else {
        if (ellipsis_variables[i] %in% c("mat", "silent", "scale", "annotation_row", "annotation_col")) {
          # These arguments should not be over-written
          warning(paste0("Argument '", ellipsis_variables[i] ,"' to pheatmap cannot be manually set - ignoring"))
        }
        else {
          # Add to call
          pheatmap_args[[ ellipsis_variables[i] ]] <- list(...)[[i]]
        }
      }
    }
  }

  # Add row annotations to call, if requested
  if (!is.null(variable_row_list)){
    variable_df <- df %>%
      dplyr::select(SampleID, all_of(variable_row_list)) %>%
      dplyr::distinct() %>%
      tibble::column_to_rownames('SampleID')
    pheatmap_args[["annotation_row"]] <- variable_df
  }

  # Add column annotations to call, if requested
  if (!is.null(variable_col_list)){
    variable_df <- df %>%
      mutate(Assay_OlinkID = paste0(Assay,'_',OlinkID)) %>%
      dplyr::select(Assay_OlinkID, all_of(variable_col_list)) %>%
      dplyr::distinct() %>%
      tibble::column_to_rownames('Assay_OlinkID')
    pheatmap_args[["annotation_col"]] <- variable_df
  }

  # Set annotation colors for rows and columns
  if (!is.null(variable_row_list) || !is.null(variable_col_list)){
    row_variables <- character(0L)
    col_variables <- character(0L)
    if (!is.null(variable_row_list)) {
      row_variables <- rev(variable_row_list)
    }
    if (!is.null(variable_col_list)) {
      col_variables <- rev(variable_col_list)
    }
    # Use Olink color palette for characters and factors, unless user set specific colors
    vars_to_color <- c(col_variables, row_variables)
    vars_to_color <- vars_to_color[ col_classes[vars_to_color] %in% c("character", "factor") ]
    if (!is.null(annotation_colors_internal)) {
      vars_to_color <- vars_to_color[!vars_to_color %in% names(annotation_colors_internal)]
    }
    if (length(vars_to_color) > 0L) {
      # 'cols' contains all new colors for all variables to be annotated
      colors <- OlinkAnalyze::olink_pal()(
        sum(sapply(dplyr::select(df, all_of(vars_to_color)), function(x) length(unique(x))))
      )
      # 'var_colors' is a list that fits the format expected by pheatmap
      # Each entry is named by the variable to be annotated, and the content is a named vector of colors
      var_colors <- vector("list", length=length(vars_to_color))
      names(var_colors) <- vars_to_color
      color_index <- 1L
      for (var_to_color in vars_to_color) {
        n_colors_i <- length(unique(df[[var_to_color]]))
        colors_i   <- colors[color_index:(color_index + n_colors_i - 1L)]
        if (col_classes[var_to_color] == "character") {
          # Character: Sort unique values
          names(colors_i) <- sort(unique(df[[var_to_color]]))
        }
        else {
          # Factor: Take the levels
          names(colors_i) <- levels(df[[var_to_color]])
        }
        var_colors[[var_to_color]] <- colors_i
        color_index <- color_index + n_colors_i
      }
      if (is.null(annotation_colors_internal)) {
        # No colors specified by user -> our new list of color specs is the complete list
        annotation_colors_internal <- var_colors
      }
      else {
        # Add the new color specs to those provided by the user
        annotation_colors_internal <- append(annotation_colors_internal, var_colors)
      }
    }
    # Add annotation colors to function call
    if (is.null(annotation_colors_internal)) {
      # NA is the default no-specification expected by pheatmap
      pheatmap_args[["annotation_colors"]] <- NA
    }
    else {
      # Use the created list
      pheatmap_args[["annotation_colors"]] <- annotation_colors_internal
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
set_plot_theme_pheatmap <- function(x, fontsize, col="#737373", font1="Swedish Gothic Thin", font2="Swedish Gothic") {
  xnames <- x$layout$name

  # Prepare fonts
  set_font1 <- FALSE
  set_font2 <- FALSE
  if (getOption("OlinkAnalyze.allow.font.load", default = TRUE)) {
    if (requireNamespace("extrafont", quietly = TRUE)) {
      if (font1 %in% extrafont::fonts()){
        if (.Platform$OS.type == "windows"){
          extrafont::loadfonts(quiet = TRUE, device = "win")
        }
        extrafont::loadfonts(quiet = TRUE, device = "pdf")
        set_font1 <- TRUE
      }
      if (font2 %in% extrafont::fonts()){
        if (.Platform$OS.type == "windows"){
          extrafont::loadfonts(quiet = TRUE, device = "win")
        }
        extrafont::loadfonts(quiet = TRUE, device = "pdf")
        set_font2 <- TRUE
      }
    }
    else if (requireNamespace("systemfonts", quietly = TRUE)) {
      # NOTE: This is a special for OI, where the above fails but the fonts should already have been registered
      if (font1 %in% unique(systemfonts::registry_fonts()$family)) {
        set_font1 <- TRUE
      }
      if (font2 %in% unique(systemfonts::registry_fonts()$family)) {
        set_font2 <- TRUE
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

  # Main title (use Swedish Goth Thin if exists, otherwise Swedish Gothic)
  main_i <- which(x$layout$name == "main")
  if (length(main_i) > 0L) {
    if (set_font1) {
      x$grobs[[main_i]] <- grid::editGrob(x$grobs[[main_i]], gp=grid::gpar(col=col, fontface="bold", fontfamily=font1))
    }
    else if (set_font2) {
      x$grobs[[main_i]] <- grid::editGrob(x$grobs[[main_i]], gp=grid::gpar(col=col, fontface="bold", fontfamily=font2))
    }
    else {
      x$grobs[[main_i]] <- grid::editGrob(x$grobs[[main_i]], gp=grid::gpar(col=col, fontface="bold"))
    }
  }

  # Row / column names (use Swedish Goth Thin if exists, otherwise Swedish Gothic)
  col_names_i <- which(x$layout$name == "col_names")
  row_names_i <- which(x$layout$name == "row_names")
  if (length(col_names_i) > 0L) {
    if (set_font1) {
      x$grobs[[col_names_i]] <- grid::editGrob(x$grobs[[col_names_i]], gp=grid::gpar(col="black", fontsize=fontsize, fontfamily=font1))
    }
    else if (set_font2) {
      x$grobs[[col_names_i]] <- grid::editGrob(x$grobs[[col_names_i]], gp=grid::gpar(col="black", fontsize=fontsize, fontfamily=font2))
    }
    else {
      x$grobs[[col_names_i]] <- grid::editGrob(x$grobs[[col_names_i]], gp=grid::gpar(col="black", fontsize=fontsize))
    }
  }
  if (length(row_names_i) > 0L) {
    if (set_font1) {
      x$grobs[[row_names_i]] <- grid::editGrob(x$grobs[[row_names_i]], gp=grid::gpar(col=col, fontsize=fontsize, fontfamily=font1))
    }
    else if (set_font2) {
      x$grobs[[row_names_i]] <- grid::editGrob(x$grobs[[row_names_i]], gp=grid::gpar(col=col, fontsize=fontsize, fontfamily=font2))
    }
    else {
      x$grobs[[row_names_i]] <- grid::editGrob(x$grobs[[row_names_i]], gp=grid::gpar(col=col, fontsize=fontsize))
    }
  }

  # Row annotation (use Swedish Goth Thin if exists, otherwise Swedish Gothic)
  row_annotation_names_i <- which(x$layout$name == "row_annotation_names")
  if (length(row_annotation_names_i) > 0L) {
    if (set_font1) {
      x$grobs[[row_annotation_names_i]] <- grid::editGrob(x$grobs[[row_annotation_names_i]], gp=grid::gpar(col="black", fontsize=fontsize, fontface="bold", fontfamily=font1))
    }
    else if (set_font2) {
      x$grobs[[row_annotation_names_i]] <- grid::editGrob(x$grobs[[row_annotation_names_i]], gp=grid::gpar(col="black", fontsize=fontsize, fontface="bold", fontfamily=font2))
    }
    else {
      x$grobs[[row_annotation_names_i]] <- grid::editGrob(x$grobs[[row_annotation_names_i]], gp=grid::gpar(col="black", fontsize=fontsize, fontface="bold"))
    }
  }

  # Columns annotation (use Swedish Goth Thin if exists, otherwise Swedish Gothic)
  col_annotation_names_i <- which(x$layout$name == "col_annotation_names")
  if (length(col_annotation_names_i) > 0L) {
    if (set_font1) {
      x$grobs[[col_annotation_names_i]] <- grid::editGrob(x$grobs[[col_annotation_names_i]], gp=grid::gpar(col="black", fontsize=fontsize, fontface="bold", fontfamily=font1))
    }
    else if (set_font2) {
      x$grobs[[col_annotation_names_i]] <- grid::editGrob(x$grobs[[col_annotation_names_i]], gp=grid::gpar(col="black", fontsize=fontsize, fontface="bold", fontfamily=font2))
    }
    else {
      x$grobs[[col_annotation_names_i]] <- grid::editGrob(x$grobs[[col_annotation_names_i]], gp=grid::gpar(col="black", fontsize=fontsize, fontface="bold"))
    }
  }

  # Annotation legend (must use Swedish Gothic - otherwise result is poor)
  annotation_legend_i <- which(x$layout$name == "annotation_legend")
  if (length(annotation_legend_i) > 0L) {
    if (set_font2) {
      x$grobs[[annotation_legend_i]] <- grid::editGrob(x$grobs[[annotation_legend_i]], gp=grid::gpar(col=col, fontsize=fontsize, fontfamily=font2))
    }
    else {
      x$grobs[[annotation_legend_i]] <- grid::editGrob(x$grobs[[annotation_legend_i]], gp=grid::gpar(col=col, fontsize=fontsize))
    }
  }

  # Standard legend (use Swedish Goth Thin if exists, otherwise Swedish Gothic)
  legend_i <- which(x$layout$name == "legend")
  if (length(legend_i) > 0L) {
    if (set_font1) {
      x$grobs[[legend_i]] <- grid::editGrob(x$grobs[[legend_i]], gp=grid::gpar(col=col, fontsize=fontsize, fontfamily=font1))
    }
    if (set_font2) {
      x$grobs[[legend_i]] <- grid::editGrob(x$grobs[[legend_i]], gp=grid::gpar(col=col, fontsize=fontsize, fontfamily=font2))
    }
    else {
      x$grobs[[legend_i]] <- grid::editGrob(x$grobs[[legend_i]], gp=grid::gpar(col=col, fontsize=fontsize))
    }
  }

  x
}
