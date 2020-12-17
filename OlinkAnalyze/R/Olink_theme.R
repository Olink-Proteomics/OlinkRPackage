#' Function to set plot theme
#'
#' This function sets a coherent plot theme for functions.
#' @export
#' @import dplyr 
#' 

set_plot_theme <- function() {
  
  olink_theme <- theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    theme(panel.border = element_blank(),
          axis.line = element_line(size = 0.5)) +
    theme(strip.background = element_rect(fill = "white"),
          strip.text = element_text(size = 8), strip.text.x = element_text(size = 13))
}
