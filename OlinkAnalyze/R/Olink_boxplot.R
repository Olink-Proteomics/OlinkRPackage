#' Function which plots boxplots of selected variables
#'
#' Generates faceted boxplots of NPX vs. grouping variable(s) for a given list of proteins (OlinkIDs) using ggplot and ggplot2::geom_boxplot.
#'
#' @param df NPX data frame in long format with at least protein name (Assay), OlinkID (unique), UniProt and at least one grouping variable.
#' @param variable  A character vector or character value indicating which column to use as the x-axis and fill grouping variable.
#' The first or single value is used as x-axis, the second as fill. Further values in a vector are not plotted.
#' @param olinkid_list Character vector indicating which proteins (OlinkIDs) to plot.
#' @param posthoc_results Data frame from ANOVA posthoc analysis using olink_anova_posthoc() function.
#' @param ttest_results Data frame from ttest analysis using olink_ttest() function.
#' @param number_of_proteins_per_plot Number of boxplots to include in the facet plot (default 6).
#' @param verbose Boolean. If the plots are shown as well as returned in the list (default is false).
#' @param ... coloroption passed to specify color order
#'
#' @return A list of objects of class “ggplot” (the actual ggplot object is entry 1 in the list). Box and whisker plot of NPX (y-axis) by variable (x-axis) for each Assay
#' @importFrom magrittr %>%
#' @importFrom dplyr filter mutate select
#' @importFrom stringr str_detect
#' @importFrom tidyr unite
#' @importFrom ggplot2 ggplot aes geom_boxplot theme facet_wrap
#' @importFrom rlang ensym
#' @importFrom forcats as_factor
#' @importFrom methods show
#' @export
#' @examples
#' \donttest{
#'
#' library(dplyr)
#'
#' anova_results <- olink_anova(npx_data1, variable = "Site")
#' significant_assays <- anova_results %>%
#'   filter(Threshold == "Significant") %>%
#'   pull(OlinkID)
#' olink_boxplot(npx_data1,
#'   variable = "Site",
#'   olinkid_list = significant_assays,
#'   verbose = TRUE,
#'   number_of_proteins_per_plot = 3
#' )
#' }
#'
olink_boxplot <- function(df,
                          variable,
                          olinkid_list,
                          verbose = FALSE,
                          number_of_proteins_per_plot = 6,
                          posthoc_results = NULL,
                          ttest_results = NULL,
                          ...) {
  myRound <- function(x) {
    if (x >= .00009) {
      return(as.character(round(x, 4)))
    } else {
      out <- as.character(x)
      if (nchar(out) > 8) {
        out <- paste0(substring(out, 1, 4), substring(out, nchar(out) - 3, nchar(out)))
      }
      return(return(out))
    }
  }

  # checking ellipsis
  if (length(list(...)) > 0) {
    ellipsis_variables <- names(list(...))

    if (length(ellipsis_variables) == 1) {
      if (!(ellipsis_variables == "coloroption")) {
        stop(paste0(
          "The ... option only takes the coloroption argument. ... currently contains the variable ",
          ellipsis_variables,
          "."
        ))
      }
    } else {
      stop(paste0(
        "The ... option only takes one argument. ... currently contains the variables ",
        paste(ellipsis_variables, collapse = ", "),
        "."
      ))
    }
  }

  # Exclude OlinkIDs with missing NPX
  npx_check <- npxCheck(df)


  # Filtering on valid OlinkID
  df <- df %>%
    dplyr::filter(stringr::str_detect(
      OlinkID,
      "OID[0-9]{5}"
    )) %>%
    dplyr::filter(!(OlinkID %in% npx_check$all_nas))

  # Column setup
  columns_for_npx_data <- c("OlinkID", "UniProt", "Assay", "NPX", eval(variable))

  # Testing that needed columns are correct
  if (!(all(columns_for_npx_data %in% colnames(df)))) {
    stop(paste0(
      "Column(s) ",
      paste(
        setdiff(
          columns_for_npx_data,
          colnames(df)
        ),
        collapse = ", "
      ),
      " not found in NPX data frame!"
    ))
  }

  if (length(variable) > 2) {
    warning(paste0(
      "Variable(s) ",
      paste(setdiff(variable, variable[1:2]), collapse = ", "),
      " will not be used for plotting."
    ))
  }

  # Setup
  x_variable <- rlang::syms(variable[1])
  if (length(variable) > 1) {
    fill_variable <- rlang::syms(variable[2])
  } else {
    fill_variable <- x_variable
  }

  topX <- length(olinkid_list)

  protein_index <- seq(
    from = 1,
    to = topX,
    by = number_of_proteins_per_plot
  )

  list_of_plots <- list()
  COUNTER <- 1

  for (i in c(1:length(protein_index))) {
    # setting indeces

    from_protein <- protein_index[i]
    to_protein <- NULL

    if ((protein_index[i] + number_of_proteins_per_plot) > topX) {
      to_protein <- topX + 1
    } else {
      to_protein <- protein_index[i + 1]
    }

    assays_for_plotting <- olinkid_list[c(from_protein:(to_protein - 1))]


    npx_for_plotting <- df %>%
      dplyr::filter(OlinkID %in% assays_for_plotting) %>%
      dplyr::mutate(OlinkID = factor(OlinkID, levels = assays_for_plotting)) %>%
      dplyr::select(OlinkID, UniProt, Assay, NPX, eval(variable)) %>%
      with(., .[order(OlinkID), ]) %>%
      tidyr::unite(c(Assay, OlinkID), col = "Name_OID", sep = " ", remove = FALSE) %>%
      dplyr::mutate(Name_OID = forcats::as_factor(Name_OID))

    if (is.null(posthoc_results) && is.null(ttest_results)) {
      boxplot <- npx_for_plotting %>%
        ggplot2::ggplot(ggplot2::aes(
          y = NPX,
          !!x_variable[[1]]
        )) +
        ggplot2::geom_boxplot(ggplot2::aes(fill = !!fill_variable[[1]])) +
        OlinkAnalyze::set_plot_theme() +
        OlinkAnalyze::olink_fill_discrete(...) +
        ggplot2::theme(
          axis.ticks.x = element_blank(),
          legend.text = element_text(size = 13)
        ) +
        ggplot2::facet_wrap(~Name_OID, scales = "free")
    } else if (!is.null(posthoc_results) && is.null(ttest_results)) {
      star.info <- data.frame(
        x.vals = levels(npx_for_plotting %>%
          dplyr::pull(eval(variable)) %>%
          addNA()),
        id = 1:length(levels(npx_for_plotting %>%
          dplyr::pull(eval(variable)) %>%
          addNA()))
      ) %>%
        dplyr::mutate(x.vals = replace(x.vals, is.na(x.vals), "NA"))

      posthoc.results_temp <- posthoc_results %>%
        dplyr::filter(OlinkID %in% assays_for_plotting) %>%
        tidyr::unite(c(Assay, OlinkID), col = "Name_OID", sep = " ", remove = FALSE) %>%
        dplyr::mutate(Name_OID = forcats::as_factor(Name_OID))

      scale_inf <- npx_for_plotting %>%
        dplyr::group_by(Name_OID) %>%
        dplyr::summarise(maxNPX = max(NPX), rangeNPX = diff(range(NPX))) %>%
        dplyr::ungroup()

      line.data <- posthoc.results_temp %>%
        dplyr::left_join(scale_inf, by = "Name_OID") %>%
        dplyr::mutate(
          C1 = sapply(strsplit(as.character(contrast), " - "), function(x) x[1]),
          C2 = sapply(strsplit(as.character(contrast), " - "), function(x) x[2])
        ) %>%
        dplyr::group_by(Name_OID, contrast) %>%
        dplyr::mutate(c.sort = min(C1, C2)) %>%
        dplyr::mutate(p.value = paste0(myRound(Adjusted_pval), " Contrast: ", contrast)) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(Name_OID) %>%
        dplyr::arrange(c.sort) %>%
        dplyr::mutate(rowNum = n():1) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(y.anchor = maxNPX + rowNum * rangeNPX * (.5) / max(rowNum)) %>%
        dplyr::select(Name_OID, contrast, Adjusted_pval, C1, C2, p.value, Threshold, c.sort, y.anchor) %>%
        tidyr::pivot_longer(-c(Name_OID, Threshold, contrast, Adjusted_pval, p.value, c.sort, y.anchor), names_to = "tmp", values_to = "x.vals") %>%
        dplyr::mutate(Star = dplyr::case_when(
          Adjusted_pval < 0.05 & Adjusted_pval > 0.01 ~ "*",
          Adjusted_pval <= 0.01 & Adjusted_pval > 0.005 ~ "**",
          Adjusted_pval <= 0.005 ~ "***",
          Adjusted_pval >= 0.05 ~ NA_character_
        )) %>%
        dplyr::left_join(star.info, by = "x.vals") %>%
        dplyr::group_by(contrast, Name_OID) %>%
        dplyr::mutate(x.m = sum(id) / 2) %>%
        dplyr::ungroup() %>%
        dplyr::filter(Threshold == "Significant")

      boxplot <- npx_for_plotting %>%
        ggplot2::ggplot(ggplot2::aes(
          y = NPX,
          x = !!rlang::ensym(variable)
        )) +
        ggplot2::geom_boxplot(ggplot2::aes(fill = !!rlang::ensym(variable))) +
        ggplot2::geom_line(data = line.data, ggplot2::aes(x = x.vals, y = y.anchor, group = p.value)) +
        ggplot2::geom_text(data = line.data %>% dplyr::filter(tmp == "C1"), ggplot2::aes(group = p.value, x = x.m, y = y.anchor + 0.1, label = Star)) +
        OlinkAnalyze::set_plot_theme() +
        OlinkAnalyze::olink_fill_discrete(...) +
        ggplot2::theme(
          axis.ticks.x = element_blank(),
          legend.text = element_text(size = 13)
        ) +
        ggplot2::facet_wrap(~Name_OID, scales = "free")
    } else if (is.null(posthoc_results) && !is.null(ttest_results)) {
      star.info <- data.frame(
        x.vals = npx_for_plotting %>%
          dplyr::pull(eval(variable)) %>%
          unique(),
        id = 1:length(npx_for_plotting %>%
          dplyr::pull(eval(variable)) %>%
          unique())
      ) %>%
        dplyr::mutate(x.vals = replace(x.vals, is.na(x.vals), "NA"))

      ttest_results_temp <- ttest_results %>%
        dplyr::filter(OlinkID %in% assays_for_plotting) %>%
        tidyr::unite(c(Assay, OlinkID), col = "Name_OID", sep = " ", remove = FALSE) %>%
        dplyr::mutate(Name_OID = forcats::as_factor(Name_OID))

      scale_inf <- npx_for_plotting %>%
        dplyr::group_by(Name_OID) %>%
        dplyr::summarise(maxNPX = max(NPX), rangeNPX = diff(range(NPX))) %>%
        dplyr::ungroup()

      ttest_variable <- npx_for_plotting %>%
        dplyr::select(!!rlang::ensym(variable)) %>%
        unique() %>%
        na.omit() %>%
        dplyr::pull(!!rlang::ensym(variable))

      line.data <- ttest_results_temp %>%
        dplyr::select(Name_OID, Assay, OlinkID, UniProt, Panel, Adjusted_pval, Threshold) %>%
        dplyr::mutate(
          C1 = ttest_variable[1],
          C2 = ttest_variable[2]
        ) %>%
        dplyr::left_join(scale_inf, by = "Name_OID") %>%
        dplyr::group_by(Name_OID) %>%
        dplyr::mutate(c.sort = min(C1, C2)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(y.anchor = maxNPX + rangeNPX * (.2)) %>%
        dplyr::select(Name_OID, Adjusted_pval, C1, C2, Threshold, c.sort, y.anchor) %>%
        tidyr::pivot_longer(-c(Name_OID, Threshold, Adjusted_pval, c.sort, y.anchor), names_to = "tmp", values_to = "x.vals") %>%
        dplyr::mutate(Star = case_when(
          Adjusted_pval < 0.05 & Adjusted_pval > 0.01 ~ "*",
          Adjusted_pval <= 0.01 & Adjusted_pval > 0.005 ~ "**",
          Adjusted_pval <= 0.005 ~ "***",
          Adjusted_pval >= 0.05 ~ NA_character_
        )) %>%
        dplyr::left_join(star.info, by = "x.vals") %>%
        dplyr::group_by(Name_OID) %>%
        dplyr::mutate(x.m = sum(id) / 2) %>%
        dplyr::ungroup() %>%
        dplyr::filter(Threshold == "Significant")

      boxplot <- npx_for_plotting %>%
        ggplot2::ggplot(ggplot2::aes(
          y = NPX,
          x = !!rlang::ensym(variable)
        )) +
        ggplot2::geom_boxplot(aes(fill = !!rlang::ensym(variable))) +
        ggplot2::geom_line(data = line.data, ggplot2::aes(x = x.vals, y = y.anchor, group = Name_OID)) +
        ggplot2::geom_text(data = line.data %>% dplyr::filter(tmp == "C1"), aes(group = Name_OID, x = x.m, y = y.anchor + 0.1, label = Star)) +
        OlinkAnalyze::set_plot_theme() +
        OlinkAnalyze::olink_fill_discrete(...) +
        ggplot2::theme(
          axis.ticks.x = element_blank(),
          legend.text = element_text(size = 13)
        ) +
        ggplot2::facet_wrap(~Name_OID, scales = "free")
    }

    if (length(variable) == 1) {
      boxplot <- boxplot +
        ggplot2::theme(
          axis.text.x = element_blank(),
          legend.title = element_blank()
        )
    }

    if (verbose) {
      methods::show(boxplot)
    }

    list_of_plots[[COUNTER]] <- boxplot
    COUNTER <- COUNTER + 1
  }

  return(invisible(list_of_plots))
}
