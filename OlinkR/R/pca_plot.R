#' Function to plot a PCA of the data
#'
#' Creates a scatterplot of all samples from an NPX data frame along two principal components. Unique sample names are required.
#' @param df data frame in long format with Sample Id, NPX and column of choice for colors
#' @param color_g Character value indicating which column to use for colors (default QC_Warning)
#' @param x_val Integer indicating which principal component to plot along the x-axis (default 1)
#' @param y_val Integer indicating which principal component to plot along the y-axis (default 2)
#' @param label_samples Logical. If TRUE, points are replaced with SampleID (default FALSE)
#' @return An object of class "ggplot"
#' @keywords NPX, PCA
#' @export
#' @examples \donttest{olink_pca_plot(df=npx.data, color_g = "QC_Warning")  }
#' @import dplyr stringr tidyr ggfortify

olink_pca_plot <- function (df, color_g = "QC_Warning", x_val = 1, y_val = 2, label_samples = F){

  if (color_g == "QC_Warning"){

    df_temp <- df %>%
      group_by(SampleID, Index) %>%
      mutate(QC_Warning = if_else(any(QC_Warning == "Warning"), "Warning", "Pass")) %>%
      ungroup()

    colors_for_pca <- df_temp %>%
      group_by(SampleID, Index) %>%
      summarise(pca_colors = unique(!!rlang::ensym(color_g))) %>%
      ungroup()


  } else {

    number_of_sample_w_more_than_one_color <- df %>%
      group_by(SampleID, Index) %>%
      summarise(n_colors = n_distinct(!!rlang::ensym(color_g), na.rm = T)) %>%
      ungroup() %>%
      filter(n_colors > 1) %>%
      nrow(.)

    if(number_of_sample_w_more_than_one_color > 0) {

      stop(paste0("There are ", number_of_sample_w_more_than_one_color, " samples that do not have a unique color. Only one color per sample is allowed."))

    }else{

      df_temp <- df

      colors_for_pca <- df_temp %>%
        group_by(SampleID, Index) %>%
        summarise(pca_colors = unique(!!rlang::ensym(color_g))) %>%
        ungroup()

    }

  }

  #Checking if there are any proteins with 0 variance, they are filtered out

  df_temp <- df_temp %>%
    group_by(OlinkID) %>%
    mutate(assay_var = var(NPX, na.rm = T)) %>%
    ungroup() %>%
    filter(!(assay_var == 0 | is.na(assay_var))) %>%
    select(-assay_var)


  df_wide <- df_temp %>%
    select(SampleID, Index, OlinkID, NPX) %>%
    filter(!is.na(NPX)) %>%
    spread(OlinkID, NPX) %>%
    na.omit() %>%
    left_join(colors_for_pca,
              by = c('SampleID',
                     'Index')) %>%
    select(SampleID, Index, pca_colors, everything())


  names(df_wide)[names(df_wide) == "pca_colors"] <- color_g


  first_col <- levels(factor(df$OlinkID)) %>% head(1)
  last_col <- levels(factor(df$OlinkID)) %>% tail(1)


  df_wide_matrix <- df_wide %>%
    column_to_rownames('SampleID') %>%
    select(first_col:last_col) %>%
    as.matrix



  pca_fit <- prcomp(df_wide_matrix, scale. = T, center = T)


  if(label_samples){

    pca_plot <- ggplot2::autoplot(pca_fit, x = x_val, y = y_val,
                                  data = df_wide, colour = color_g, shape = F, label.size = 3)

  }else{

    pca_plot <- ggplot2::autoplot(pca_fit, x = x_val, y = y_val,
                                  data = df_wide, colour = color_g, size = 2.5)

  }

  pca_plot <- pca_plot +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    theme(panel.border = element_blank(), axis.line = element_line(size = 0.5)) +
    theme(strip.background = element_rect(fill = "white"),
          strip.text = element_text(size = 8), strip.text.x = element_text(size = 13))

  return(pca_plot)


}
