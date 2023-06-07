#' Olink color panel for plotting
#'
#' @param alpha transparency (optional)
#' @param coloroption string, one or more of the following:
#' c('red', 'orange', 'yellow', 'green', 'teal', 'turqoise', 'lightblue', 'darkblue', 'purple', 'pink')
#' @return A character vector of palette hex codes for colors
#' @keywords color palette Olink
#' @examples
#'
#' library(scales)
#'
#' # Color matrices
#' show_col(olink_pal()(10), labels = FALSE)
#' show_col(olink_pal(coloroption = c("lightblue", "green"))(2), labels = FALSE)
#'
#' # Contour plot
#' filled.contour(volcano, color.palette = olink_pal(), asp = 1)
#' filled.contour(volcano, color.palette = hue_pal(), asp = 1)
#'
#' @export
#' @importFrom grDevices col2rgb rgb colorRampPalette

olink_pal <- function(alpha = 1, coloroption = NULL) {
  if (alpha > 1 | alpha < 0) {
    stop("alpha in wrong range")
  }

  function(n) {
    alpha <- alpha * 255

    red <- grDevices::col2rgb("#FE1F04")
    orange <- grDevices::col2rgb("#FF8C22")
    yellow <- grDevices::col2rgb("#FFC700")
    green <- grDevices::col2rgb("#27AE55")
    teal <- grDevices::col2rgb("#077183")
    turqoise <- grDevices::col2rgb("#00C7E1")
    lightblue <- grDevices::col2rgb("#A2D9F5")
    darkblue <- grDevices::col2rgb("#00559E")
    purple <- grDevices::col2rgb("#6A27AE")
    pink <- grDevices::col2rgb("#FF51B8")


    red <- grDevices::rgb(red[1], red[2], red[3], alpha, maxColorValue = 255)
    orange <- grDevices::rgb(orange[1], orange[2], orange[3], alpha, maxColorValue = 255)
    yellow <- grDevices::rgb(yellow[1], yellow[2], yellow[3], alpha, maxColorValue = 255)
    green <- grDevices::rgb(green[1], green[2], green[3], alpha, maxColorValue = 255)
    teal <- grDevices::rgb(teal[1], teal[2], teal[3], alpha, maxColorValue = 255)
    turqoise <- grDevices::rgb(turqoise[1], turqoise[2], turqoise[3], alpha, maxColorValue = 255)
    lightblue <- grDevices::rgb(lightblue[1], lightblue[2], lightblue[3], alpha, maxColorValue = 255)
    darkblue <- grDevices::rgb(darkblue[1], darkblue[2], darkblue[3], alpha, maxColorValue = 255)
    purple <- grDevices::rgb(purple[1], purple[2], purple[3], alpha, maxColorValue = 255)
    pink <- grDevices::rgb(pink[1], pink[2], pink[3], alpha, maxColorValue = 255)



    hues_length <- NULL
    crispy_colors_hex <- NULL

    if (is.null(coloroption)) {
      hues_length <- n + 1
      crispy_colors_hex_ordered <- rbind(
        turqoise,
        red,
        darkblue,
        yellow,
        teal,
        pink,
        green,
        purple,
        orange,
        lightblue
      )

      crispy_colors_hex <- rbind(red, orange, yellow, green, teal, turqoise, lightblue, darkblue, purple, pink, red)
    } else {
      for (current_color in coloroption) {
        tryCatch(get(current_color),
          error = function(e) {
            print(paste("this color option not available:", current_color))
          }
        )

        crispy_colors_hex <- rbind(
          crispy_colors_hex,
          get(current_color)
        )
      }

      return(crispy_colors_hex)
    }

    length_crispy_colors <- dim(crispy_colors_hex)[1]

    if (n < length_crispy_colors) {
      hues <- seq(1, n)
      return(crispy_colors_hex_ordered[hues])
    } else {
      if (is.null(hues_length)) {
        hues_length <- n
      }

      olink_color_ramp_palette <- grDevices::colorRampPalette(crispy_colors_hex,
        space = "Lab",
        interpolate = "linear"
      )

      m <- 360
      hues <- seq(1, m, length = hues_length)[1:n]
      olink_color <- olink_color_ramp_palette(m)
      return(olink_color[hues])
    }
  }
}

#' Olink color scale for discrete ggplots
#'
#' @param alpha transparency
#' @param coloroption string, one or more of the following:
#' c('red', 'orange', 'yellow', 'green', 'teal', 'turqoise', 'lightblue', 'darkblue', 'purple', 'pink')
#' @param ... Optional. Additional arguments to pass to ggplot2::discrete_scale()
#'
#' @return No return value, called for side effects
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(mtcars, aes(x = wt, y = mpg, color = as.factor(cyl))) +
#'   geom_point(size = 4) +
#'   olink_color_discrete() +
#'   theme_bw()
#'
#' ggplot(mtcars, aes(x = wt, y = mpg, color = as.factor(cyl))) +
#'   geom_point(size = 4) +
#'   olink_color_discrete(coloroption = c("lightblue", "red", "green")) +
#'   theme_bw()
#' @importFrom ggplot2 discrete_scale


olink_color_discrete <- function(..., alpha = 1, coloroption = NULL) {
  ggplot2::discrete_scale(aesthetics = "colour", scale_name = "olink", palette = olink_pal(alpha, coloroption), ...)
}


#' Olink color scale for continuous ggplots
#'
#' @param alpha transparency (optional)
#' @param coloroption string, one or more of the following:
#' c('red', 'orange', 'yellow', 'green', 'teal', 'turqoise', 'lightblue', 'darkblue', 'purple', 'pink')
#' @param ... Optional. Additional arguments to pass to scale_color_gradientn()
#' @return No return value, called for side effects
#' @export
#'
#' @examples
#'
#' library(ggplot2)
#'
#' dsub <- subset(diamonds, x > 5 & x < 6 & y > 5 & y < 6)
#' dsub$diff <- with(dsub, sqrt(abs(x - y)) * sign(x - y))
#'
#' ggplot(dsub, aes(x, y, colour = diff)) +
#'   geom_point() +
#'   theme_bw() +
#'   olink_color_gradient()
#'
#' @importFrom ggplot2 scale_colour_gradientn


olink_color_gradient <- function(..., alpha = 1, coloroption = NULL) {
  ggplot2::scale_colour_gradientn(colors = rev(olink_pal(alpha, coloroption)(100)), ...)
}


#' Olink fill scale for discrete ggplots
#' @param alpha transparency (optional)
#' @param coloroption string, one or more of the following:
#' c('red', 'orange', 'yellow', 'green', 'teal', 'turqoise', 'lightblue', 'darkblue', 'purple', 'pink')
#' @param ... Optional. Additional arguments to pass to ggplot2::discrete_scale()
#'
#' @return No return value, called for side effects
#' @export
#'
#' @examples
#'
#' library(ggplot2)
#'
#' dsub <- subset(diamonds, x > 5 & x < 6 & y > 5 & y < 6)
#' dsub$diff <- with(dsub, sqrt(abs(x - y)) * sign(x - y))
#'
#' ggplot(dsub, aes(x, y, colour = diff)) +
#'   geom_point() +
#'   theme_bw() +
#'   olink_fill_discrete()
#'
#' @importFrom ggplot2 discrete_scale

olink_fill_discrete <- function(..., alpha = 1, coloroption = NULL) {
  ggplot2::discrete_scale(
    aesthetics = "fill", scale_name = "olink",
    palette = olink_pal(alpha, coloroption), ...
  )
}

#' Olink fill scale for continuous ggplots
#'
#' @param alpha transparency (optional)
#' @param coloroption string, one or more of the following:
#' c('red', 'orange', 'yellow', 'green', 'teal', 'turqoise', 'lightblue', 'darkblue', 'purple', 'pink')
#' @param ... Optional. Additional arguments to pass to ggplot2::scale_fill_gradientn()
#'
#' @return No return value, called for side effects
#' @export
#'
#' @examples
#'
#' library(ggplot2)
#'
#' dsub <- subset(diamonds, x > 5 & x < 6 & y > 5 & y < 6)
#' dsub$diff <- with(dsub, sqrt(abs(x - y)) * sign(x - y))
#' ggplot(dsub, aes(x, y, colour = diff)) +
#'   geom_point() +
#'   theme_bw() +
#'   olink_fill_gradient()
#'
#' @importFrom ggplot2 scale_fill_gradientn

olink_fill_gradient <- function(..., alpha = 1, coloroption = NULL) {
  ggplot2::scale_fill_gradientn(colors = rev(olink_pal(alpha, coloroption)(100)), ...)
}
