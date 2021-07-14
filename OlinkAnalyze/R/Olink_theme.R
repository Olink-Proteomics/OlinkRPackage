#' Function to set plot theme
#'
#' This function sets a coherent plot theme for functions.
#' @export
#' @importFrom ggplot2 theme element_blank element_line element_rect element_text

set_plot_theme <- function() {

  olink_theme <- ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank()) +
    ggplot2::theme(panel.border = ggplot2::element_blank(),
                   axis.line = ggplot2::element_line(size = 0.5)) +
    ggplot2::theme(strip.background = ggplot2::element_rect(fill = "white"),
          strip.text = ggplot2::element_text(size = 8),
          strip.text.x = ggplot2::element_text(size = 13))
}
