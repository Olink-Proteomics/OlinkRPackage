#' Function to set plot theme
#'
#' This function sets a coherent plot theme for functions.
#'
#' @param font Font family to use for text elements.
#'
#' @return No return value, used as theme for ggplots
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
set_plot_theme <- function(font = "Arial") {

  if (!requireNamespace("showtext", quietly = TRUE)) {
    stop("Package 'showtext' is required for fonts_system(). Please install package \"showtext\" before continuing.

         install.packages(\"showtext\")")
  }

  usefont <- ""

  if (getOption("OlinkAnalyze.allow.font.load", default = TRUE)) {
    if (requireNamespace("showtext", quietly = TRUE)) {
      if (font %in% fonts_system()) {

        # If the font is already available in systemfonts, or showtext, add it
        # One approach: try to see if the font is installed in system; if not,
        # you may register it manually (via font_add or similar).
        usefont <- font

        # Turn on showtext automatic rendering
        showtext::showtext_auto()
      }
    }
  }

  olink_theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(fill = "white"),
      strip.text = ggplot2::element_text(size = 8L),
      strip.text.x = ggplot2::element_text(size = 13L),
      text = ggplot2::element_text(
        family = if(!is.null(usefont)) usefont else "",
        color = "#737373",
        size = 12L
      )
    )

  # Keep support for older ggplot2
  if (utils::packageVersion("ggplot2") >= package_version("3.4.0")) {
    olink_theme <- olink_theme +
      ggplot2::theme(
        axis.line = ggplot2::element_line(linewidth = 0.5)
      )
  } else {
    olink_theme <- olink_theme +
      ggplot2::theme(
        axis.line = ggplot2::element_line(size = 0.5)
      )
  }

}

# Replacement for extrafont::fonts()
fonts_system <- function() {
  if (!requireNamespace("systemfonts", quietly = TRUE)) {
    stop("Package 'systemfonts' is required for fonts_system(). Please install package \"systemfonts\" before continuing.

         install.packages(\"systemfonts\")")
  }

  fonts_df <- systemfonts::system_fonts()

  # Concatenate family + style, trimming whitespace
  font_names <- c(trimws(paste(fonts_df$family, fonts_df$style)),
                  fonts_df$family) |>
    unique() |>
    sort()

  # Remove duplicates and sort for consistency
  return(font_names)
}
