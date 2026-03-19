
#' Title
#'
#' @param df data frame with OSI data present
#' @param check_log check log from check NPX
#' @param osi_score OSI column to graph, one of OSISummary,
#' OSITimeToCentrifugation, or OSIPreparationTemperature

#' @return distribution plot (histogram overlayed with density plot) of
#' osi values for corresponding osi_score column
#' @export
#'
#' @examples
#' # Creating fake OSI data from Site data
#' df1 <-npx_data1 |>
#'   dplyr::mutate(OSISummary = as.numeric(as.factor(Site))) |>
#'   dplyr::mutate(OSISummary = OSISummary - min(OSISummary, na.rm = TRUE)) |>
#'   dplyr::mutate(OSISummary = OSISummary/max(OSISummary, na.rm = TRUE))
#'
#' check_log <- check_npx(df1)
#' # Generate figure
#' olink_osi_dist_plot(df1,
#'                     check_log = check_log,
#'                     osi_score = "OSISummary")
olink_osi_dist_plot <- function(df,
                                check_log = NULL,
                                osi_score = NULL) {
  check_log <- run_check_npx(df, check_log = check_log)

  # Check specific to osi_dist_plot, OSI value must be continuous
  if (is.null(osi_score) || !(osi_score %in% c("OSITimeToCentrifugation",
                                               "OSIPreparationTemperature",
                                               "OSISummary"))) {

    cli::cli_abort(paste0("`osi_score` must be one of",
                          " OSISummary, OSITimeToCentrifugation, ",
                          "or OSIPreparationTemperature."))
  }

  # General OSI checks
  df <- check_osi(df, check_log, osi_score)

  # Filter for distinct sampleID and osi_score
  if (any(is.na(df[[osi_score]]))) {
    cli::cli_warn(paste("NA data detected in",
                        osi_score,
                        "Filtering out NA data."))
  }

  df1 <- df |>
    dplyr::filter(!is.na(.data[[osi_score]])) |>
    dplyr::select(dplyr::any_of(c(column_name_dict$col_names$sample_id,
                                  osi_score))) |>
    dplyr::distinct()

  # Check for duplicate sampleIDS
  if (any(duplicated(df1[[check_log$col_names$sample_id]]))) {
    cli::cli_abort("Multiple OSI values detected for same Sample ID.")
  }

  xlab <- dplyr::recode_values(osi_score,
                               "OSITimeToCentrifugation" ~
                                 "Time to Centrifugation",
                               "OSIPreparationTemperature" ~
                                 "Preparation Temperature",
                               "OSISummary" ~ "OSI Summary",
                               default = NA_character_)

  p <- ggplot2::ggplot(df1, ggplot2::aes(x = .data[[osi_score]])) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                            bins = 30L,
                            fill = "skyblue",
                            color = "grey80",
                            alpha = 0.5) +
    ggplot2::geom_density(
      color = "grey40",
      linewidth = 1L
    ) +
    ggplot2::scale_x_continuous(
      breaks = seq(from = 0L, to = 1L, by = 0.25),
      expand = c(0L, 0L)
    ) +
    ggplot2::scale_y_continuous(
      expand = c(0L, 0L)
    ) +
    ggplot2::labs(
      x = xlab,
      y = "Density"
    ) +
    OlinkAnalyze::olink_fill_discrete() +
    OlinkAnalyze::set_plot_theme() +
    ggplot2::theme(
      axis.title = ggplot2::element_text(size = 15L),
      axis.text = ggplot2::element_text(size = 13L),
      strip.text = ggplot2::element_text(size = 15L),
      strip.background = ggplot2::element_blank()
    )

  return(p)
}
