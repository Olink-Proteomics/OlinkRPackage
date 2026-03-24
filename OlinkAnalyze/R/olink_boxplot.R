#'Function which plots boxplots of selected variables
#'
#'Generates faceted boxplots of NPX vs. grouping variable(s) for a given list of proteins (OlinkIDs) using ggplot and ggplot2::geom_boxplot.
#'
#' @param df NPX data frame in long format with at least protein name (Assay), OlinkID (unique), UniProt and at least one grouping variable.
#' @param variable  A character vector or character value indicating which column to use as the x-axis and fill grouping variable.
#' The first or single value is used as x-axis, the second as fill. Further values in a vector are not plotted.
#' @param olinkid_list Character vector indicating which proteins (OlinkIDs) to plot.
#' @param posthoc_results Data frame from ANOVA posthoc analysis using olink_anova_posthoc() function.
#' @param ttest_results Data frame from ttest analysis using olink_ttest() function.
#' @param number_of_proteins_per_plot Number of boxplots to include in the facet plot (default 6).
#' @param verbose Boolean. If the plots are shown as well as returned in the list (default is false).
#' @param check_log A named list returned by [`check_npx()`]. If `NULL`,
#' [`check_npx()`] will be run internally using `df`...
#' @param ... coloroption passed to specify color order
#'
#' @return A list of objects of class “ggplot” (the actual ggplot object is entry 1 in the list). Box and whisker plot of NPX (y-axis) by variable (x-axis) for each Assay
#' @importFrom dplyr filter mutate select
#' @importFrom stringr str_detect
#' @importFrom tidyr unite
#' @importFrom ggplot2 ggplot aes geom_boxplot theme facet_wrap
#' @importFrom forcats as_factor
#' @export
#' @examples
#' \donttest{
#'
#' library(dplyr)
#' npx_df <- npx_data1 |> filter(!grepl('control|ctrl',SampleID, ignore.case = TRUE))
#' anova_results <- olink_anova(npx_df, variable = "Site")
#' significant_assays <- anova_results |>
#'     filter(Threshold == 'Significant') |>
#'     pull(OlinkID)
#' olink_boxplot(npx_df,
#'               variable = "Site",
#'               olinkid_list = significant_assays,
#'               verbose = TRUE,
#'               number_of_proteins_per_plot = 3)}
#'
olink_boxplot <- function(df,
                          variable,
                          olinkid_list,
                          verbose = FALSE,
                          number_of_proteins_per_plot = 6,
                          posthoc_results = NULL,
                          ttest_results = NULL,
                          check_log = NULL,
                          ...) {
  # ---- Helper: rounding for significance labels --------------------------------
  myRound <- function(x) {
    if (x >= 0.00009) {
      return(as.character(round(x, 4)))
    }
    out <- as.character(x)
    if (nchar(out) > 8) {
      out <- paste0(
        substring(out, 1, 4),
        substring(out, nchar(out) - 3, nchar(out))
      )
    }
    out
  }
  
  # ---- Input validation ---------------------------------------------------------
  check_is_dataset(x = df, error = TRUE)
  check_is_character(variable, error = TRUE)
  check_is_character(olinkid_list, error = TRUE)
  check_is_scalar_boolean(verbose, error = TRUE)
  
  number_of_proteins_per_plot <- as.integer(number_of_proteins_per_plot)
  check_is_scalar_integer(number_of_proteins_per_plot, error = TRUE)
  
  # Validate ellipsis
  dots <- list(...)
  if (length(dots) > 0) {
    if (length(dots) != 1 || names(dots) != "coloroption") {
      stop(
        paste0(
          "The ... option only accepts 'coloroption'. Provided: ",
          paste(names(dots), collapse = ", ")
        )
      )
    }
  }
  
  # ---- QC & CLEANING -----------------------------------------------------------
  check_log <- run_check_npx(df = df, check_log = check_log)
  
  df <- clean_npx(
    df = df,
    check_log = check_log,
    remove_qc_warning = FALSE,
    remove_assay_warning = FALSE,
    verbose = FALSE
  ) |>
    suppressMessages() |>
    suppressWarnings()
  
  check_log_clean <- check_npx(df = df) |>
    suppressMessages() |>
    suppressWarnings()
  
  if (!("sample_type" %in% names(check_log_clean$col_names))) {
    cli::cli_inform(
      "No sample type column detected in input {.arg df}. Control samples may not be filtered out."
    )
  }
  
  # ---- Early column trimming (major speed-up) ----------------------------------
  required_cols <- c("OlinkID", "UniProt", "Assay", "NPX", variable)
  missing_cols <- setdiff(required_cols, colnames(df))
  if (length(missing_cols) > 0) {
    stop(
      paste0(
        "Missing required column(s): ",
        paste(missing_cols, collapse = ", ")
      )
    )
  }
  
  df <- df[, required_cols, drop = FALSE]
  
  # ---- Column names for tidy eval ----------------------------------------------
  x_var <- variable[1]
  fill_var <- if (length(variable) > 1) variable[2] else x_var
  
  # ---- Setup --------------------------------------------------------------------
  topX <- length(olinkid_list)
  protein_index <- seq(1, topX, by = number_of_proteins_per_plot)
  
  list_of_plots <- vector("list", length(protein_index))
  counter <- 1
  
  # ---- Precompute lookups -------------------------------------------------------
  if (!is.null(posthoc_results)) {
    posthoc_results <- posthoc_results |>
      dplyr::mutate(Name_OID = forcats::as_factor(paste(Assay, OlinkID)))
  }
  
  if (!is.null(ttest_results)) {
    ttest_results <- ttest_results |>
      dplyr::mutate(Name_OID = forcats::as_factor(paste(Assay, OlinkID)))
  }
  
  # ---- MAIN LOOP ----------------------------------------------------------------
  for (i in seq_along(protein_index)) {
    
    from <- protein_index[i]
    to <- min(from + number_of_proteins_per_plot - 1, topX)
    assays <- olinkid_list[from:to]
    
    # Fast filtering
    npx_plot <- df[df$OlinkID %in% assays, , drop = FALSE]
    npx_plot$OlinkID <- factor(npx_plot$OlinkID, levels = assays)
    npx_plot$Name_OID <- forcats::as_factor(paste(npx_plot$Assay, npx_plot$OlinkID))
    
    # ---- Basic boxplot ----------------------------------------------------------
    if (is.null(posthoc_results) && is.null(ttest_results)) {
      
      p <- ggplot2::ggplot(
        npx_plot,
        ggplot2::aes(
          x = .data[[x_var]],
          y = NPX
        )
      ) +
        ggplot2::geom_boxplot(ggplot2::aes(fill = .data[[fill_var]])) +
        OlinkAnalyze::set_plot_theme() +
        OlinkAnalyze::olink_fill_discrete(...) +
        ggplot2::theme(
          axis.ticks.x = ggplot2::element_blank(),
          legend.text = ggplot2::element_text(size = 13)
        ) +
        ggplot2::facet_wrap(~Name_OID, scales = "free")
      
      # ---- Posthoc annotation ------------------------------------------------------
    } else if (!is.null(posthoc_results)) {
      
      levs <- levels(addNA(npx_plot[[x_var]]))
      star.info <- data.frame(
        x.vals = ifelse(is.na(levs), "NA", levs),
        id = seq_along(levs)
      )
      
      posthoc.tmp <- posthoc_results[posthoc_results$OlinkID %in% assays, , drop = FALSE]
      
      scale_inf <- npx_plot |>
        dplyr::group_by(Name_OID) |>
        dplyr::summarise(
          maxNPX = max(NPX),
          rangeNPX = diff(range(NPX)),
          .groups = "drop"
        )
      
      line.data <- posthoc.tmp |>
        dplyr::left_join(scale_inf, by = "Name_OID") |>
        dplyr::mutate(
          C1 = gsub("[()]", "", sub(" .*", "", contrast)),
          C2 = gsub("[()]", "", sub(".* ", "", contrast)),
          rp = myRound(Adjusted_pval),
          p.value = paste0(rp, " Contrast: ", contrast)
        ) |>
        dplyr::group_by(Name_OID, contrast) |>
        dplyr::arrange(pmin(C1, C2)) |>
        dplyr::mutate(
          rowNum = dplyr::n():1,
          y.anchor = maxNPX + rowNum * rangeNPX * 0.5 / max(rowNum)
        ) |>
        dplyr::ungroup() |>
        tidyr::pivot_longer(
          c(C1, C2),
          names_to = "tmp",
          values_to = "x.vals"
        ) |>
        dplyr::mutate(
          Star = dplyr::case_when(
            Adjusted_pval < 0.05 & Adjusted_pval > 0.01 ~ "*",
            Adjusted_pval <= 0.01 & Adjusted_pval > 0.005 ~ "**",
            Adjusted_pval <= 0.005 ~ "***",
            TRUE ~ NA_character_
          )
        ) |>
        dplyr::left_join(star.info, by = "x.vals") |>
        dplyr::group_by(Name_OID, contrast) |>
        dplyr::mutate(x.m = mean(id)) |>
        dplyr::ungroup() |>
        dplyr::filter(Threshold == "Significant")
      
      p <- ggplot2::ggplot(
        npx_plot,
        ggplot2::aes(x = .data[[x_var]], y = NPX)
      ) +
        ggplot2::geom_boxplot(ggplot2::aes(fill = .data[[fill_var]])) +
        ggplot2::geom_line(
          data = line.data,
          ggplot2::aes(x = x.vals, y = y.anchor, group = p.value)
        ) +
        ggplot2::geom_text(
          data = line.data[line.data$tmp == "C1", ],
          ggplot2::aes(x = x.m, y = y.anchor + 0.1, label = Star)
        ) +
        OlinkAnalyze::set_plot_theme() +
        OlinkAnalyze::olink_fill_discrete(...) +
        ggplot2::theme(
          axis.ticks.x = ggplot2::element_blank(),
          legend.text = ggplot2::element_text(size = 13)
        ) +
        ggplot2::facet_wrap(~Name_OID, scales = "free")
      
      # ---- T-test annotation -------------------------------------------------------
    } else if (!is.null(ttest_results)) {
      
      uniq_vals <- unique(npx_plot[[x_var]])
      uniq_vals <- uniq_vals[!is.na(uniq_vals)]
      
      star.info <- data.frame(
        x.vals = uniq_vals,
        id = seq_along(uniq_vals)
      )
      
      ttest.tmp <- ttest_results[ttest_results$OlinkID %in% assays, , drop = FALSE]
      
      scale_inf <- npx_plot |>
        dplyr::group_by(Name_OID) |>
        dplyr::summarise(
          maxNPX = max(NPX),
          rangeNPX = diff(range(NPX)),
          .groups = "drop"
        )
      
      line.data <- ttest.tmp |>
        dplyr::left_join(scale_inf, by = "Name_OID") |>
        dplyr::mutate(
          C1 = uniq_vals[1],
          C2 = uniq_vals[2],
          y.anchor = maxNPX + rangeNPX * 0.2
        ) |>
        tidyr::pivot_longer(
          c(C1, C2),
          names_to = "tmp",
          values_to = "x.vals"
        ) |>
        dplyr::mutate(
          Star = dplyr::case_when(
            Adjusted_pval < 0.05 & Adjusted_pval > 0.01 ~ "*",
            Adjusted_pval <= 0.01 & Adjusted_pval > 0.005 ~ "**",
            Adjusted_pval <= 0.005 ~ "***",
            TRUE ~ NA_character_
          )
        ) |>
        dplyr::left_join(star.info, by = "x.vals") |>
        dplyr::group_by(Name_OID) |>
        dplyr::mutate(x.m = mean(id)) |>
        dplyr::ungroup() |>
        dplyr::filter(Threshold == "Significant")
      
      p <- ggplot2::ggplot(
        npx_plot,
        ggplot2::aes(x = .data[[x_var]], y = NPX)
      ) +
        ggplot2::geom_boxplot(ggplot2::aes(fill = .data[[fill_var]])) +
        ggplot2::geom_line(
          data = line.data,
          ggplot2::aes(x = x.vals, y = y.anchor, group = Name_OID)
        ) +
        ggplot2::geom_text(
          data = line.data[line.data$tmp == "C1", ],
          ggplot2::aes(x = x.m, y = y.anchor + 0.1, label = Star)
        ) +
        OlinkAnalyze::set_plot_theme() +
        OlinkAnalyze::olink_fill_discrete(...) +
        ggplot2::theme(
          axis.ticks.x = ggplot2::element_blank(),
          legend.text = ggplot2::element_text(size = 13)
        ) +
        ggplot2::facet_wrap(~Name_OID, scales = "free")
    }
    
    # ---- Clean up single-variable case -----------------------------------------
    if (length(variable) == 1) {
      p <- p +
        ggplot2::theme(
          axis.text.x = ggplot2::element_blank(),
          legend.title = ggplot2::element_blank()
        )
    }
    
    if (verbose) print(p)
    
    list_of_plots[[counter]] <- p
    counter <- counter + 1
  }
  
  invisible(list_of_plots)
}