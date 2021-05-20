#' Olink color panel for plotting
#'
#' @param alpha transparency (optional)
#' @param coloroption string, one or more of the following:
#' c('red', 'orange', 'yellow', 'green', 'teal', 'turqoise', 'gray', 'darkblue', 'purple', 'pink')
#' @return Palette hex codes for colors
#' @keywords crispy, Olink
#' @examples
#'
#' #Color matrices
#' show_col(olink_pal()(10), labels = F)
#' show_col(olink_pal(coloroption = c('gray', 'green'))(2), labels = F)
#'
#' #Contour plot
#' filled.contour(volcano, color.palette = olink_pal(), asp = 1)
#' filled.contour(volcano, color.palette = hue_pal(), asp = 1)
#'
#'
#'
#' @export
#' @import RColorBrewer scales

olink_pal <- function(alpha = 1, coloroption = NULL) {

  if (alpha > 1 | alpha <  0) {
    stop('alpha in wrong range')
  }

  function(n) {

    alpha = alpha*255

    red <- col2rgb('#FE1F04')
    orange <- col2rgb('#FF8C22')
    yellow <- col2rgb('#FFC700')
    green <- col2rgb('#27AE55')
    teal <- col2rgb('#077183')
    turqoise <- col2rgb('#00C7E1')
    gray <- col2rgb('#AAB1B9')
    darkblue <- col2rgb('#00559E')
    purple <- col2rgb('#6A27AE')
    pink <- col2rgb('#FF51B8')


    red<- rgb(red[1], red[2], red[3], alpha, maxColorValue = 255)
    orange <- rgb(orange[1], orange[2], orange[3], alpha, maxColorValue = 255)
    yellow <- rgb(yellow[1], yellow[2], yellow[3], alpha, maxColorValue = 255)
    green <- rgb(green[1], green[2], green[3], alpha, maxColorValue = 255)
    teal <- rgb(teal[1], teal[2], teal[3], alpha, maxColorValue = 255)
    turqoise <- rgb(turqoise[1], turqoise[2], turqoise[3], alpha, maxColorValue = 255)
    gray <- rgb(gray[1], gray[2], gray[3], alpha, maxColorValue = 255)
    darkblue <- rgb(darkblue[1], darkblue[2], darkblue[3], alpha, maxColorValue = 255)
    purple <- rgb(purple[1], purple[2], purple[3], alpha, maxColorValue = 255)
    pink <- rgb(pink[1], pink[2], pink[3], alpha, maxColorValue = 255)


    hues_length <- NULL
    crispy_colors_hex <- NULL

    if(is.null(coloroption)) {

      hues_length <- n+1
      crispy_colors_hex_ordered <- rbind(turqoise,
                                         red,
                                         darkblue,
                                         yellow,
                                         teal,
                                         pink,
                                         green,
                                         purple,
                                         orange,
                                         gray)

      crispy_colors_hex <- rbind(red, orange, yellow, green, teal, turqoise, gray, darkblue, purple, pink, red)

    }else{

      for (current_color in coloroption) {

        tryCatch(get(current_color),
                 error=function(e)
                   print(paste('this color option not available:', current_color))
        )

        crispy_colors_hex <- rbind(crispy_colors_hex,
                                   get(current_color))
      }

      return(crispy_colors_hex)
    }

    length_crispy_colors <- dim(crispy_colors_hex)[1]

    if (n < length_crispy_colors) {

      hues <- seq(1, n)
      return(crispy_colors_hex_ordered[hues])

    }else {

      if(is.null(hues_length)){
        hues_length <- n
      }

      olink_color_ramp_palette <- colorRampPalette(crispy_colors_hex, space = "Lab", interpolate="linear")

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
#' c('red', 'orange', 'yellow', 'green', 'teal', 'turqoise', 'gray', 'darkblue', 'purple', 'pink')
#'
#' @return None
#' @export
#'
#' @examples
#'
#' ggplot(mtcars, aes(x=wt, y=mpg, color=as.factor(cyl))) +
#' geom_point(size = 4) +
#' olink_color_discrete() +
#' theme_bw()
#'
#' ggplot(mtcars, aes(x=wt, y=mpg, color=as.factor(cyl))) +
#' geom_point(size = 4) +
#' olink_color_discrete(coloroption = c('gray', 'red', 'green')) +
#' theme_bw()
#'
#'
#
olink_color_discrete <- function(..., alpha = 1, coloroption = NULL) {

  discrete_scale(aesthetics = "colour", scale_name = 'olink', palette = olink_pal(alpha, coloroption), ...)
}


#' Olink color scale for continuous ggplots
#'
#' @param alpha transparency (optional)
#' @param coloroption string, one or more of the following:
#' c('red', 'orange', 'yellow', 'green', 'teal', 'turqoise', 'gray', 'darkblue', 'purple', 'pink')
#'
#' @return None
#' @export
#'
#' @examples
#'
#'dsub <- subset(diamonds, x > 5 & x < 6 & y > 5 & y < 6)
#'dsub$diff <- with(dsub, sqrt(abs(x-y))* sign(x-y))
#'
#'ggplot(dsub, aes(x, y, colour=diff)) +
#'geom_point() +
#'  theme_bw() +
#'  olink_color_gradient()


olink_color_gradient <- function(..., alpha = 1, coloroption = NULL) {
  scale_colour_gradientn(colors = rev(olink_pal(alpha, coloroption)(100)), ...)
}


#' Olink fill scale for discrete ggplots
#' @param alpha transparency (optional)
#' @param coloroption string, one or more of the following:
#' c('red', 'orange', 'yellow', 'green', 'teal', 'turqoise', 'gray', 'darkblue', 'purple', 'pink')
#'
#' @return None
#' @export
#'
#' @examples
#'dsub <- subset(diamonds, x > 5 & x < 6 & y > 5 & y < 6)
#'dsub$diff <- with(dsub, sqrt(abs(x-y))* sign(x-y))
#'
#'ggplot(dsub, aes(x, y, colour=diff)) +
#'geom_point() +
#'  theme_bw() +
#'  olink_fill_discrete()

olink_fill_discrete <- function(..., alpha = 1, coloroption = NULL) {
  discrete_scale(aesthetics = "fill", scale_name = 'olink', palette = olink_pal(alpha, coloroption), ...)
}

#' Olink fill scale for continuous ggplots
#'
#' @param alpha transparency (optional)
#' @param coloroption string, one or more of the following:
#' c('red', 'orange', 'yellow', 'green', 'teal', 'turqoise', 'gray', 'darkblue', 'purple', 'pink')
#'
#' @return None
#' @export
#'
#' @examples
#'dsub <- subset(diamonds, x > 5 & x < 6 & y > 5 & y < 6) 
#'dsub$diff <- with(dsub, sqrt(abs(x-y))* sign(x-y)) 
#'ggplot(dsub, aes(x, y, colour=diff)) + 
#'geom_point() + 
#'  theme_bw() + 
#'  olink_fill_gradient()
olink_fill_gradient <- function(..., alpha = 1, coloroption = NULL) {
  scale_fill_gradientn(colors = rev(olink_pal(alpha, coloroption)(100)), ...)
}

