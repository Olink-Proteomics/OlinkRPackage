#' Olink color panel for plotting
#'
#' @param alpha transparency (optional)
#' @param coloroption string, one or more of the following:
#' c("red", "orange", "yellow", "green", "teal", "turqoise", "lightblue",
#' "darkblue", "purple", "pink")
#'
#' @return A character vector of palette hex codes for colors.
#'
#' @keywords color palette Olink
#'
#' @export
#'
#' @examples
#' \donttest{
#' if (rlang::is_installed(pkg = c("scales"))) {
#'   # Color matrices
#'   scales::show_col(
#'     colours = OlinkAnalyze::olink_pal()(10L),
#'     labels = FALSE
#'   )
#'   scales::show_col(
#'     colours = OlinkAnalyze::olink_pal(
#'       coloroption = c("lightblue", "green")
#'     )(2L),
#'     labels = FALSE
#'   )
#'
#'   # Contour plot
#'   filled.contour(
#'     x = datasets::volcano,
#'     color.palette = OlinkAnalyze::olink_pal(),
#'     asp = 1
#'   )
#'   filled.contour(
#'     x = datasets::volcano,
#'     color.palette = scales::hue_pal(),
#'     asp = 1
#'   )
#' }
#' }
#'
olink_pal <- function(alpha = 1,
                      coloroption = NULL) {

  check_is_scalar_numeric(x = alpha, error = TRUE)

  if (!dplyr::between(x = alpha, left = 0, right = 1)) {
    stop("alpha in wrong range")
  }

  function(n) { # nolint: return_linter

    alpha <- alpha * 255L

    get_color <- function(hex_color, alpha) {
      mod_color <- grDevices::col2rgb(hex_color) |>
        (\(hcol) {
          grDevices::rgb( # nolint: return_linter
            red = hcol[1],
            green = hcol[2],
            blue = hcol[3],
            alpha = alpha,
            maxColorValue = 255
          )
        })()
      return(mod_color)
    }

    red <- get_color(hex_color = "#FE1F04", alpha = alpha)
    orange <- get_color(hex_color = "#FF8C22", alpha = alpha)
    yellow <- get_color(hex_color = "#FFC700", alpha = alpha)
    green <- get_color(hex_color = "#27AE55", alpha = alpha)
    teal <- get_color(hex_color = "#077183", alpha = alpha)
    turqoise <- get_color(hex_color = "#00C7E1", alpha = alpha)
    lightblue <- get_color(hex_color = "#A2D9F5", alpha = alpha)
    darkblue <- get_color(hex_color = "#00559E", alpha = alpha)
    purple <- get_color(hex_color = "#6A27AE", alpha = alpha)
    pink <- get_color(hex_color = "#FF51B8", alpha = alpha)

    hues_length <- NULL
    crispy_colors_hex <- NULL

    if (is.null(coloroption)) {

      hues_length <- n + 1L
      crispy_colors_hex_ordered <- rbind(turqoise,
                                         red,
                                         darkblue,
                                         yellow,
                                         teal,
                                         pink,
                                         green,
                                         purple,
                                         orange,
                                         lightblue)

      crispy_colors_hex <- rbind(red, orange, yellow, green, teal, turqoise,
                                 lightblue, darkblue, purple, pink, red)

    } else {

      for (current_color in coloroption) {

        tryCatch(
          expr = get(current_color),
          error = function(e) {
            print(paste("this color option not available:", current_color)) # nolint: return_linter
          }
        )

        crispy_colors_hex <- rbind(crispy_colors_hex, get(current_color))
      }

      return(crispy_colors_hex)
    }

    length_crispy_colors <- dim(crispy_colors_hex)[1L]

    if (n < length_crispy_colors) {

      hues <- seq(1L, n)
      return(crispy_colors_hex_ordered[hues])

    } else {

      if (is.null(hues_length)) {
        hues_length <- n
      }

      olink_color_ramp_palette <- grDevices::colorRampPalette(
        colors = crispy_colors_hex,
        space = "Lab",
        interpolate = "linear"
      )

      m <- 360L
      hues <- seq(1L, m, length = hues_length)[1L:n]
      olink_color <- olink_color_ramp_palette(m)
      return(olink_color[hues])

    }
  }
}

#' Olink color scale for discrete ggplots
#'
#' @inheritParams olink_pal
#' @param ... Optional. Additional arguments to pass to
#' `ggplot2::discrete_scale()`
#'
#' @return No return value, called for side effects
#'
#' @export
#'
#' @examples
#' \donttest{
#' ggplot2::ggplot(
#'   data = datasets::mtcars,
#'   mapping = ggplot2::aes(
#'     x = .data[["wt"]],
#'     y = .data[["mpg"]],
#'     color = as.factor(x = .data[["cyl"]])
#'   )
#' ) +
#'   ggplot2::geom_point(
#'     size = 4L
#'   ) +
#'   OlinkAnalyze::olink_color_discrete() +
#'   ggplot2::theme_bw()
#'
#' ggplot2::ggplot(
#'   data = datasets::mtcars,
#'   mapping = ggplot2::aes(
#'     x = .data[["wt"]],
#'     y = .data[["mpg"]],
#'     color = as.factor(x = .data[["cyl"]])
#'   )
#' ) +
#'   ggplot2::geom_point(
#'     size = 4L
#'   ) +
#'   OlinkAnalyze::olink_color_discrete(
#'     coloroption = c("lightblue", "red", "green")
#'   ) +
#'   ggplot2::theme_bw()
#' }
#'
olink_color_discrete <- function(...,
                                 alpha = 1,
                                 coloroption = NULL) {
  # Add support for older and newer versions of ggplot
  if (utils::packageVersion("ggplot2") < "3.5.0") {
    ggplot2::discrete_scale( # nolint: return_linter
      aesthetics = "colour",
      scale_name = "olink",
      palette = olink_pal(
        alpha = alpha,
        coloroption = coloroption
      ),
      ...
    )
  } else {
    ggplot2::discrete_scale( # nolint: return_linter
      aesthetics = "colour",
      palette = olink_pal(
        alpha = alpha,
        coloroption = coloroption
      ),
      ...
    )
  }
}

#' Olink color scale for continuous ggplots
#'
#' @inheritParams olink_pal
#' @param ... Optional. Additional arguments to pass to
#' `ggplot2::scale_color_gradientn()`
#'
#' @return No return value, called for side effects
#'
#' @export
#'
#' @examples
#' \donttest{
#' ggplot2::diamonds |>
#'   dplyr::filter(
#'     .data[["x"]] > 5
#'     & .data[["x"]] < 6
#'     & .data[["y"]] > 5
#'     & .data[["y"]] < 6
#'   ) |>
#'   dplyr::mutate(
#'     diff = sqrt(
#'       x = abs(
#'         x = .data[["x"]] - .data[["y"]]
#'       )
#'     ) * sign(
#'       x = .data[["x"]] - .data[["y"]]
#'     )
#'   ) |>
#'   ggplot2::ggplot(
#'     mapping = ggplot2::aes(
#'       x = .data[["x"]],
#'       y = .data[["y"]],
#'       colour = .data[["diff"]]
#'     )
#'   ) +
#'   ggplot2::geom_point() +
#'   ggplot2::theme_bw() +
#'   OlinkAnalyze::olink_color_gradient()
#' }
#'
olink_color_gradient <- function(...,
                                 alpha = 1,
                                 coloroption = NULL) {
  ggplot2::scale_colour_gradientn( # nolint: return_linter
    colors = rev(
      x = olink_pal(
        alpha = alpha,
        coloroption = coloroption
      )(100L)
    ),
    ...
  )
}

#' Olink fill scale for discrete ggplots
#'
#' @inheritParams olink_pal
#' @param ... Optional. Additional arguments to pass to
#' `ggplot2::discrete_scale()`
#'
#' @return No return value, called for side effects
#'
#' @export
#'
#' @examples
#' \donttest{
#' ggplot2::diamonds |>
#'   dplyr::filter(
#'     .data[["x"]] > 5
#'     & .data[["x"]] < 6
#'     & .data[["y"]] > 5
#'     & .data[["y"]] < 6
#'   ) |>
#'   dplyr::mutate(
#'     diff = sqrt(
#'       x = abs(
#'         x = .data[["x"]] - .data[["y"]]
#'       )
#'     ) * sign(
#'       x = .data[["x"]] - .data[["y"]]
#'     )
#'   ) |>
#'   ggplot2::ggplot(
#'     mapping = ggplot2::aes(
#'       x = .data[["x"]],
#'       y = .data[["y"]],
#'       colour = .data[["diff"]]
#'     )
#'   ) +
#'   ggplot2::geom_point() +
#'   ggplot2::theme_bw() +
#'   OlinkAnalyze::olink_fill_discrete()
#' }
#'
olink_fill_discrete <- function(...,
                                alpha = 1,
                                coloroption = NULL) {
  if (utils::packageVersion("ggplot2") < "3.5.0") {
    ggplot2::discrete_scale( # nolint: return_linter
      aesthetics = "fill",
      scale_name =  "olink",
      palette = olink_pal(
        alpha = alpha,
        coloroption = coloroption
      ),
      ...
    )
  } else {
    ggplot2::discrete_scale( # nolint: return_linter
      aesthetics = "fill",
      palette = olink_pal(
        alpha = alpha,
        coloroption = coloroption
      ),
      ...
    )
  }
}

#' Olink fill scale for continuous ggplots
#'
#' @inheritParams olink_pal
#' @param ... Optional. Additional arguments to pass to
#' `ggplot2::scale_fill_gradientn()`
#'
#' @return No return value, called for side effects
#'
#' @export
#'
#' @examples
#' \donttest{
#' ggplot2::diamonds |>
#'   dplyr::filter(
#'     .data[["x"]] > 5
#'     & .data[["x"]] < 6
#'     & .data[["y"]] > 5
#'     & .data[["y"]] < 6
#'   ) |>
#'   dplyr::mutate(
#'     diff = sqrt(
#'       x = abs(
#'         x = .data[["x"]] - .data[["y"]]
#'       )
#'     ) * sign(
#'       x = .data[["x"]] - .data[["y"]]
#'     )
#'   ) |>
#'   ggplot2::ggplot(
#'     mapping = ggplot2::aes(
#'       x = .data[["x"]],
#'       y = .data[["y"]],
#'       colour = .data[["diff"]]
#'     )
#'   ) +
#'   ggplot2::geom_point() +
#'   ggplot2::theme_bw() +
#'   OlinkAnalyze::olink_fill_gradient()
#' }
#'
olink_fill_gradient <- function(...,
                                alpha = 1,
                                coloroption = NULL) {
  ggplot2::scale_fill_gradientn( # nolint: return_linter
    colors = rev(
      x = olink_pal(
        alpha = alpha,
        coloroption = coloroption
      )(100L)
    ),
    ...
  )
}
