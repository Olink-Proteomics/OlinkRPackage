#' Function to plot a heatmap of the NPX data
#'
#' Generates a heatmap using \code{pheatmap::pheatmap} of all samples from NPX
#' data.
#'
#' The values are by default scaled across and centered in the heatmap. Columns
#' and rows are by default sorted by by dendrogram.
#' Unique sample names are required.
#'
#' @param df Data frame in long format with SampleID, NPX, OlinkID, Assay and
#' columns of choice for annotations.
#' @param check_log output from check_npx on \code{df}
#' @param variable_row_list Columns in \code{df} to be annotated for rows in
#' the heatmap.
#' @param variable_col_list Columns in \code{df} to be annotated for columns in
#' the heatmap.
#' @param center_scale Logical. If data should be centered and scaled across
#' assays (default \code{TRUE}).
#' @param cluster_rows Logical. Determining if rows should be clustered
#' (default \code{TRUE}).
#' @param cluster_cols Logical. Determining if columns should be clustered
#' (default \code{TRUE}).
#' @param show_rownames Logical. Determining if row names are shown
#' (default \code{TRUE}).
#' @param show_colnames Logical. Determining if column names are shown
#' (default \code{TRUE}).
#' @param colnames Character. Determines how to label the columns.
#' Must be 'assay', 'oid', or 'both' (default 'both').
#' @param annotation_legend Logical. Determining if legend for annotations
#' should be shown (default \code{TRUE}).
#' @param fontsize Fontsize (default 10)
#' @param na_col Color of cells with \code{NA} (default black)
#' @param ... Additional arguments used in \code{pheatmap::pheatmap}
#' @return An object of class \code{ggplot}, generated from the \code{gtable}
#' returned by \code{pheatmap::pheatmap}.
#' @keywords NPX Heatmap
#' @export
#' @examples
#' \donttest{
#' npx_data <- npx_data1 |>
#'   dplyr::filter(!stringr::str_detect(SampleID,'CONT'))
#'   check_log <- check_npx(npx_data) 
#' try({ # This will fail if ggplotify is not installed
#'   #Heatmap
#'   olink_heatmap_plot(df = npx_data,
#'                      check_log = check_log)
#'   #Heatmap with annotation
#'   olink_heatmap_plot(df = npx_data,
#'                      check_log = check_log,
#'                      variable_row_list = c('Time','Site'))
#'   #Heatmap with calls from pheatmap
#'   olink_heatmap_plot(df = npx_data,
#'                      check_log = check_log,
#'                      cutree_rows = 3)
#' })
#'
#' }
#'

olink_heatmap_plot <- function(df,
                               check_log = NULL,
                               variable_row_list = NULL,
                               variable_col_list = NULL,
                               center_scale = TRUE,
                               cluster_rows = TRUE,
                               cluster_cols = TRUE,
                               show_rownames = TRUE,
                               show_colnames = TRUE,
                               colnames = "both",
                               annotation_legend = TRUE,
                               fontsize = 10,
                               na_col = "black",
                               ...) {

  check_heatmap_inputs(colnames, ...)

  check_log <- run_check_npx(df = df,
                             check_log = check_log)

  df <- clean_heatmap_df(df = df,
                         check_log = check_log,
                         colnames = colnames)
  
  df_wide <- df_to_wide(df = df,
                        check_log = check_log,
                        colnames = colnames)

  # remove dup uniprot

  # Remove samples with only NA

  pheatmap_args <- create_pheatmap_args(df_wide = df_wide,
                                        df = df,
                                        check_log = check_log,
                                        center_scale = center_scale,
                                        cluster_rows = cluster_rows,
                                        cluster_cols = cluster_cols,
                                        na_col = na_col,
                                        show_rownames = show_rownames,
                                        show_colnames = show_colnames,
                                        annotation_legend = annotation_legend,
                                        fontsize = fontsize,
                                        variable_row_list = variable_row_list,
                                        variable_col_list = variable_col_list,
                                        colnames = colnames,
                                        ...)
  # set row and column colors for annotations
  # color palette
  p <- run_pheatmap(pheatmap_args = pheatmap_args)

  return(p)

}
