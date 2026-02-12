#' Function which performs a point-range plot per protein on a linear mixed
#' model
#'
#' @description
#' Generates a point-range plot faceted by Assay using `ggplot` and
#' `ggplot2::geom_pointrange` based on a linear mixed effects model using
#' `lmerTest:lmer` and `emmeans::emmeans`. See `olink_lmer` for details of
#' input notation.
#'
#' @param df NPX data frame in long format with at least protein name (Assay),
#' OlinkID, UniProt, 1-2 variables with at least 2 levels.
#' @param check_log A named list returned by [`check_npx()`]. If `NULL`,
#' [`check_npx()`] will be run internally using `df`.
#' @param olinkid_list Character vector indicating which proteins (by OlinkID)
#' for which to create figures.
#' @param number_of_proteins_per_plot Number plots to include in the list of
#' point-range plots. Defaults to 6 plots per figure
#' @param variable Single character value or character array. Variable(s) to
#' test. If length > 1, the included variable names will be used in crossed
#' analyses. Also takes ':' or '*' notation.
#' @param outcome Character. The dependent variable. Default: NPX.
#' @param random Single character value or character array.
#' @param covariates Single character value or character array. Default: NULL.
#' Covariates to include. Takes ':' or '*' notation. Crossed analysis will not
#' be inferred from main effects.
#' @param x_axis_variable Character. Which main effect to use as x-axis in the
#' plot.
#' @param col_variable Character. If provided, the interaction effect
#' col_variable:x_axis_variable will be plotted with x_axis_variable on the
#' x-axis and col_variable as color.
#' @param verbose Boolean. Default: True. If information about removed samples,
#' factor conversion and final model formula is to be printed to the console.
#' @param ... coloroption for color ordering
#'
#' @return A list of objects of class "ggplot" showing point-range plot of NPX
#' (y-axis) over x_axis_variable for each assay (facet), colored by col_variable
#' if provided.
#'
#' @export
#'
#' @examples
#' \donttest{
#' if (rlang::is_installed(pkg = c("lme4", "lmerTest", "broom", "emmeans"))) {
#'   #data
#'   npx_df <- OlinkAnalyze::npx_data1 |>
#'     dplyr::filter(
#'       !grepl(
#'         pattern = "control|ctrl",
#'         x = .data[["SampleID"]],
#'         ignore.case = TRUE
#'       )
#'     )
#'
#'   # check data
#'   npx_df_check_log <- OlinkAnalyze::check_npx(
#'     df = npx_df
#'   )
#'
#'   # Results in model NPX ~ Time * Treatment + (1 | Subject) + (1 | Site)
#'   lmer_results <- OlinkAnalyze::olink_lmer(
#'     df = npx_df,
#'     check_log = npx_df_check_log,
#'     variable = c("Time", "Treatment"),
#'     random = c("Subject")
#'   )
#'
#'   # List of significant proteins for the interaction effect Time:Treatment
#'   assay_list <- lmer_results |>
#'     dplyr::filter(
#'     .data[["Threshold"]] == "Significant"
#'     & .data[["term"]] == "Time:Treatment"
#'   ) |>
#'     dplyr::distinct(.data[["OlinkID"]]) |>
#'     dplyr::pull()
#'
#'   lst_pointrange_plots <- OlinkAnalyze::olink_lmer_plot(
#'     df = npx_df,
#'     check_log = npx_df_check_log,
#'     variable = c("Time", "Treatment"),
#'     random = c("Subject"),
#'     x_axis_variable = "Time",
#'     col_variable = "Treatment",
#'     verbose = TRUE,
#'     olinkid_list = assay_list,
#'     number_of_proteins_per_plot = 10L
#'   )
#' }
#' }
#'
olink_lmer_plot <- function(df,
                            check_log = NULL,
                            variable,
                            outcome = "NPX",
                            random,
                            olinkid_list = NULL,
                            covariates = NULL,
                            x_axis_variable,
                            col_variable = NULL,
                            number_of_proteins_per_plot = 6L,
                            verbose = FALSE,
                            ...) {

  # Check if all required libraries for this function are installed
  rlang::check_installed(
    pkg = c("lme4", "lmerTest", "emmeans", "broom"),
    call = rlang::caller_env()
  )

  if (missing(df) || missing(variable)
      || missing(x_axis_variable) || missing(random)) {
    stop(paste("The df, variable, random and x_axis_variable arguments need to",
               "be specified."))
  }

  if (!all(x_axis_variable %in% unique(unlist(strsplit(variable, "[\\*:]"))))) {
    stop("The x axis variable must be included in the variable argument.")
  }

  if (!is.null(col_variable)) {
    if (!all(col_variable %in% unique(unlist(strsplit(variable, "[\\*:]"))))) {
      stop("The color variable must be included in the variable argument.")
    }
  }

  #checking ellipsis
  if (length(list(...)) > 0L) {
    ellipsis_variables <- names(list(...))
    if (length(ellipsis_variables) == 1L) {
      if (!(ellipsis_variables == "coloroption")) {
        stop(
          paste0("The '...' option only takes the coloroption argument. ",
                 "'...' currently contains the variable 'ellipsis_variables'.")
        )
      }
    } else {
      stop(
        paste0("The '...' option only takes one argument. '...' currently ",
               "contains the variables",
               paste(ellipsis_variables, collapse = ", "), ".")
      )
    }
  }

  #Filtering on valid OlinkID
  df <- df |>
    dplyr::filter(
      stringr::str_detect(
        string = .data[["OlinkID"]],
        pattern = "OID[0-9]{5}"
      )
    )

  if (is.null(olinkid_list) || length(olinkid_list) == 0L) {
    olinkid_list <- df |>
      dplyr::select(
        dplyr::all_of("OlinkID")
      ) |>
      dplyr::distinct() |>
      dplyr::pull()
  }

  # Setting up what needs to be plotted
  if (is.null(col_variable)) {
    current_fixed_effect <- x_axis_variable
    color_for_plot <- x_axis_variable
  } else {
    current_fixed_effect <- paste0(x_axis_variable, ":", col_variable)
    color_for_plot <- col_variable
  }

  lm.means <- olink_lmer_posthoc( # nolint: object_name_linter
    df = df,
    check_log = check_log,
    variable = variable,
    random = random,
    outcome = outcome,
    olinkid_list = olinkid_list,
    covariates = covariates,
    effect = current_fixed_effect,
    mean_return = TRUE,
    verbose = verbose
  ) |>
    dplyr::mutate(
      Name_Assay = paste0(.data[["Assay"]], "_", .data[["OlinkID"]])
    )

  #Keep olinkid_list input order
  assay_name_list <- lm.means |>
    dplyr::mutate(
      OlinkID = factor(x = .data[["OlinkID"]],
                       levels = .env[["olinkid_list"]])
    ) |>
    dplyr::arrange(
      .data[["OlinkID"]]
    ) |>
    dplyr::pull(
      .data[["Name_Assay"]]
    ) |>
    unique()

  lm.means <- lm.means |> # nolint: object_name_linter
    dplyr::mutate(
      Name_Assay = factor(x = .data[["Name_Assay"]],
                          levels = .env[["assay_name_list"]])
    )

  #Setup
  topX <- length(assay_name_list) # nolint: object_name_linter

  protein_index <- seq(from = 1L,
                       to = topX,
                       by = number_of_proteins_per_plot)

  list_of_plots <- list()
  COUNTER <- 1L # nolint: object_name_linter

  #loops
  for (i in c(1L:length(protein_index))) { # nolint: seq_linter

    from_protein <- protein_index[i]
    to_protein <- NULL

    if ((protein_index[i] + number_of_proteins_per_plot) > topX) {
      to_protein <- topX + 1L
    } else {
      to_protein <- protein_index[i + 1L]
    }

    assays_for_plotting <- assay_name_list[c(from_protein:(to_protein - 1L))]

    lmerplot <- lm.means |>
      dplyr::filter(
        .data[["Name_Assay"]] %in% .env[["assays_for_plotting"]]
      ) |>
      ggplot2::ggplot() +
      ggplot2::geom_pointrange(
        ggplot2::aes(
          x = as.factor(x = .data[[x_axis_variable]]),
          y = .data[["emmean"]],
          ymin = .data[["conf.low"]],
          ymax = .data[["conf.high"]],
          color = as.factor(x = .data[[color_for_plot]])
        ),
        position = ggplot2::position_dodge(
          width = 0.4
        ),
        size = 0.8
      ) +
      ggplot2::facet_wrap(
        . ~ .data[["Name_Assay"]],
        scales = "free_y"
      ) +
      ggplot2::labs(
        x = x_axis_variable,
        y = "NPX",
        color = color_for_plot
      ) +
      ggplot2::theme(
        axis.title.x = ggplot2::element_blank(),
        axis.text.x =  ggplot2::element_text(
          size = 10L
        )
      ) +
      OlinkAnalyze::olink_color_discrete(...) +
      OlinkAnalyze::set_plot_theme()

    list_of_plots[[COUNTER]] <- lmerplot
    COUNTER <- COUNTER + 1L # nolint: object_name_linter
  }

  return(invisible(list_of_plots))
}
