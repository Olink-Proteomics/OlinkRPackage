#' Function to set plot theme
#'
#' This function sets a coherent plot theme for functions.
#'
#' @param font Font family to use for text elements. Depends on extrafont package.
#'
#' @export
#' @importFrom ggplot2 theme element_blank element_line element_rect element_text
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(mtcars, aes(x = wt, y = mpg, color = as.factor(cyl))) +
#'   geom_point(size = 4) +
#'   set_plot_theme()
#'
#' ggplot(mtcars, aes(x = wt, y = mpg, color = as.factor(cyl))) +
#'   geom_point(size = 4) +
#'   set_plot_theme(font = "")
#'
#'
set_plot_theme <- function(font = "Swedish Gothic Thin") {
  
  usefont <- ""
  
  if (getOption("OlinkAnalyze.allow.font.load", default = TRUE)) {
    if (requireNamespace("extrafont", quietly = TRUE)) {
      if(font %in% extrafont::fonts()){
        if(.Platform$OS.type == "windows"){
          extrafont::loadfonts(quiet = TRUE, device = "win")
        }
        extrafont::loadfonts(quiet = TRUE, device = "pdf")
        usefont <- font
      }
    }
  }
  
  olink_theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    ) +
    ggplot2::theme(
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(size = 0.5)
    ) +
    ggplot2::theme(
      strip.background = ggplot2::element_rect(fill = "white"),
      strip.text = ggplot2::element_text(size = 8),
      strip.text.x = ggplot2::element_text(size = 13)
    ) +
    ggplot2::theme(
      text = ggplot2::element_text(family = usefont, color = "#737373", size = 12)
    )
}
