#' OSI distribution plot
#'
#' @description
#' Generates a density plot showing the distribution of the selected
#' OSI score among dataset samples using ggplot2. OSI score can be one of
#' "OSITimeToCentrifugation", "OSIPreparationTemperature", or "OSISummary".
#' Olink external controls are excluded from this visualization.
#'
#' @param df data frame with OSI data present
#' @param check_log check log from check NPX
#' @param osi_score OSI column to graph, one of OSISummary,
#' OSITimeToCentrifugation, or OSIPreparationTemperature

#' @return distribution plot (histogram overlayed with density plot) of
#' osi values for corresponding osi_score column
#'
#' @export
#'
#' @examples
#' \donttest{
#' # Creating fake OSI data from Site data
#' npx_df <- OlinkAnalyze::npx_data1 |>
#'   dplyr::filter(
#'     !grepl(pattern = "control",
#'     x = .data[["SampleID"]],
#'     ignore.case = TRUE)
#'   ) |>
#'   dplyr::mutate(
#'     OSISummary = as.numeric(as.factor(.data[["Site"]])),
#'     OSISummary = .data[["OSISummary"]] - min(.data[["OSISummary"]],
#'                                              na.rm = TRUE),
#'     OSISummary = .data[["OSISummary"]] / max(.data[["OSISummary"]],
#'                                              na.rm = TRUE)
#'   )
#'
#' check_log <- OlinkAnalyze::check_npx(
#'   df = npx_df
#' )
#'
#' # Generate figure
#' OlinkAnalyze::olink_osi_dist_plot(
#'   df = npx_df,
#'   check_log = check_log,
#'   osi_score = "OSISummary"
#' )
#'}
#'
olink_osi_dist_plot <- function(df,
                                check_log = NULL,
                                osi_score = NULL) {
  check_log <- run_check_npx(df, check_log = check_log)

  # General OSI checks
  df <- check_osi(
    df = df,
    check_log = check_log,
    osi_score = osi_score
  )

  # Check specific to osi_dist_plot, OSI value must be continuous
  if (osi_score == "OSICategory") {
    cli::cli_abort(
      c(
        "x" = "The argument {.arg osi_score} should be one of the continuous OSI
        scores, not {.val {osi_score}}.",
        "i" = "Expected one of {.or {.val {c(\"OSISummary\",
        \"OSITimeToCentrifugation\", \"OSIPreparationTemperature\")}}}."
      ),
      call = rlang::caller_env(),
      wrap = FALSE
    )
  }

  # Filter for distinct sampleID and osi_score
  if (any(is.na(df[[osi_score]]))) {
    cli::cli_warn(
      c(
        "!" = "NA values detected in column {.val {osi_score}} of {.arg df}.
        Filtering out NA values."
      ),
      call = rlang::caller_env(),
      wrap = TRUE
    )
  }

  df <- df |>
    dplyr::filter(!is.na(.data[[osi_score]])) |>
    dplyr::select(dplyr::all_of(c(check_log$col_names$sample_id,
                                  osi_score))) |>
    dplyr::distinct()

  # Check for duplicate sampleIDS
  if (!length(check_log$sample_id_dups) == 0L) {
    cli::cli_abort(
      c(
        "x" = "Multiple OSI values detected for the same sample identifier."
      ),
      call = rlang::caller_env(),
      wrap = TRUE
    )
  }

  xlab <- dplyr::recode_values(
    osi_score,
    "OSITimeToCentrifugation" ~ "Time to Centrifugation",
    "OSIPreparationTemperature" ~ "Preparation Temperature",
    "OSISummary" ~ "OSI Summary",
    default = NA_character_
  )

  p <- ggplot2::ggplot(
    data = df,
    mapping = ggplot2::aes(
      x = .data[[osi_score]]
    )
  ) +
    ggplot2::geom_histogram(
      mapping = ggplot2::aes(
        y = ggplot2::after_stat(density)
      ),
      bins = 30L,
      fill = "skyblue",
      color = "grey80",
      alpha = 0.5
    ) +
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
