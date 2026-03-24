#'Function which plots boxplots of selected variables
#'
#'Generates faceted boxplots of NPX vs. grouping variable(s) for a given list
#'of proteins (OlinkIDs) using ggplot and ggplot2::geom_boxplot.
#'
#' @param df NPX data frame in long format with at least protein name (Assay),
#'  OlinkID (unique), UniProt and at least one grouping variable.
#' @param variable  A character vector or character value indicating which
#'  column to use as the x-axis and fill grouping variable.
#' The first or single value is used as x-axis, the second as fill. Further
#'  values in a vector are not plotted.
#' @param olinkid_list Character vector indicating which proteins (OlinkIDs)
#' to plot.
#' @param posthoc_results Data frame from ANOVA posthoc analysis using
#' olink_anova_posthoc() function.
#' @param ttest_results Data frame from ttest analysis using
#' olink_ttest() function.
#' @param number_of_proteins_per_plot Number of boxplots to include in the
#' facet plot (default 6).
#' @param verbose Boolean. If the plots are shown as well as returned in the
#'  list (default is false).
#' @param check_log A named list returned by [`check_npx()`]. If `NULL`,
#' [`check_npx()`] will be run internally using `df`...
#' @param ... coloroption passed to specify color order
#'
#' @return A list of objects of class “ggplot” (the actual ggplot object is
#' entry 1 in the list). Box and whisker plot of NPX (y-axis) by variable
#'  (x-axis) for each Assay
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
#' npx_df <- npx_data1 |> filter(!grepl('control|ctrl',SampleID,
#' ignore.case = TRUE))
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
  # ---- Helper: rounding for significance labels -----------------------------
  my_round <- function(x) {
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
    return(out)
  }
  # ---- Input validation -----------------------------------------------------
  check_is_dataset(x = df, error = TRUE)
  check_is_character(variable, error = TRUE)
  check_is_character(olinkid_list, error = TRUE)
  check_is_scalar_boolean(verbose, error = TRUE)
  number_of_proteins_per_plot <- as.integer(number_of_proteins_per_plot)
  check_is_scalar_integer(number_of_proteins_per_plot, error = TRUE)
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
  # ---- QC & CLEANING --------------------------------------------------------
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
      paste("No sample type column detected in input {.arg df}. Control",
            "samples may not be filtered out.")
    )
  }
  # ---- Early column trimming ------------------------------------------------
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
  # ---- Tidy-eval variable setup ---------------------------------------------
  x_var <- variable[1]
  fill_var <- if (length(variable) > 1) variable[2] else x_var
  # ---- Setup ----------------------------------------------------------------
  top_x <- length(olinkid_list)
  protein_index <- seq(1, top_x, by = number_of_proteins_per_plot)
  list_of_plots <- vector("list", length(protein_index))
  counter <- 1
  # ---- Precompute lookups ---------------------------------------------------
  if (!is.null(posthoc_results)) {
    posthoc_results <- posthoc_results |>
      dplyr::mutate(
        Name_OID = forcats::as_factor(
          paste(.data[["Assay"]], .data[["OlinkID"]])
        )
      )
  }
  if (!is.null(ttest_results)) {
    dplyr::mutate(
      Name_OID = forcats::as_factor(
        paste(.data[["Assay"]], .data[["OlinkID"]])
      )
    )
  }
  # ---- MAIN LOOP ------------------------------------------------------------
  for (i in seq_along(protein_index)) {
    from <- protein_index[i]
    to <- min(from + number_of_proteins_per_plot - 1, top_x)
    assays <- olinkid_list[from:to]
    npx_plot <- df[df$OlinkID %in% assays, , drop = FALSE]
    npx_plot$OlinkID <- factor(npx_plot$OlinkID, levels = assays)
    npx_plot$Name_OID <- forcats::as_factor(paste(npx_plot$Assay,
                                                  npx_plot$OlinkID))
    # ---- CASE 1: BASIC BOXPLOT ----------------------------------------------
    if (is.null(posthoc_results) && is.null(ttest_results)) {
      p <- ggplot2::ggplot(
        npx_plot,
        ggplot2::aes(
          x = .data[[x_var]],
          y = .data[["NPX"]]
        )
      ) +
        ggplot2::geom_boxplot(
          ggplot2::aes(fill = .data[[fill_var]])
        ) +
        OlinkAnalyze::set_plot_theme() +
        OlinkAnalyze::olink_fill_discrete(...) +
        ggplot2::theme(
          axis.ticks.x = ggplot2::element_blank(),
          legend.text = ggplot2::element_text(size = 13)
        ) +
        ggplot2::facet_wrap(~Name_OID, scales = "free")
      # ---- CASE 2: POSTHOC ANNOTATION ---------------------------------------
    } else if (!is.null(posthoc_results)) {
      levs <- levels(addNA(npx_plot[[x_var]]))
      star_info <- data.frame(
        x_vals = ifelse(is.na(levs), "NA", levs),
        id = seq_along(levs)
      )
      posthoc_tmp <- posthoc_results[
        posthoc_results$OlinkID %in% assays,
        ,
        drop = FALSE
      ]
      scale_inf <- npx_plot |>
        dplyr::group_by(.data[["NPX"]]) |>
        dplyr::summarise(
          maxNPX = max(.data[["NPX"]]),
          rangeNPX = diff(range(.data[["NPX"]])),
          .groups = "drop"
        )
      line_data <- posthoc_tmp |>
        dplyr::left_join(scale_inf, by = "Name_OID") |>
        dplyr::mutate(
          C1 = gsub("[()]", "", sub(" .*", "", .data[["contrast"]])),
          C2 = gsub("[()]", "", sub(".* ", "", .data[["contrast"]])),
          rp = my_round(.data[["Adjusted_pval"]]),
          p_value = paste0(.data[["rp"]], " Contrast: ", .data[["contrast"]])
        ) |>
        dplyr::group_by(.data[["Name_OID"]], .data[["contrast"]]) |>
        dplyr::arrange(pmin(.data[["C1"]], .data[["C2"]])) |>
        dplyr::mutate(
          rowNum = rev(seq_len(dplyr::n())),
          y_anchor = .data[["maxNPX"]] + .data[["rowNum"]] *
            .data[["rangeNPX"]] * 0.5 / max(.data[["rowNum"]])
        ) |>
        dplyr::ungroup() |>
        tidyr::pivot_longer(
          c(.data[["C1"]], .data[["C2"]]),
          names_to = "tmp",
          values_to = "x_vals"
        ) |>
        dplyr::mutate(
          star = dplyr::case_when(
            .data[["Adjusted_pval"]] < 0.05 &
              .data[["Adjusted_pval"]] > 0.01 ~ "*",
            .data[["Adjusted_pval"]] <= 0.01 &
              .data[["Adjusted_pval"]] > 0.005 ~ "**",
            .data[["Adjusted_pval"]] <= 0.005 ~ "***",
            TRUE ~ NA_character_
          )
        ) |>
        dplyr::left_join(star_info, by = "x_vals") |>
        dplyr::group_by(.data[["Name_OID"]], .data[["contrast"]]) |>
        dplyr::mutate(x_m = mean(.data[["id"]])) |>
        dplyr::ungroup() |>
        dplyr::filter(.data[["Threshold"]] == "Significant")
      p <- ggplot2::ggplot(
        npx_plot,
        ggplot2::aes(x = .data[[x_var]], y = .data[["NPX"]])
      ) +
        ggplot2::geom_boxplot(
          ggplot2::aes(fill = .data[[fill_var]])
        ) +
        ggplot2::geom_line(
          data = line_data,
          ggplot2::aes(x = .data[["x_vals"]], y = .data[["y_anchor"]],
                       group = .data[["p_value"]])
        ) +
        ggplot2::geom_text(
          data = line_data[line_data$tmp == "C1", ],
          ggplot2::aes(x = .data[["x_m"]], y = .data[["y_anchor"]] + 0.1,
                       label = .data[["star"]])
        ) +
        OlinkAnalyze::set_plot_theme() +
        OlinkAnalyze::olink_fill_discrete(...) +
        ggplot2::theme(
          axis.ticks.x = ggplot2::element_blank(),
          legend.text = ggplot2::element_text(size = 13)
        ) +
        ggplot2::facet_wrap(~.data[["Name_OID"]], scales = "free")
      # ---- CASE 3: T-TEST ANNOTATION ----------------------------------------
    } else if (!is.null(ttest_results)) {
      uniq_vals <- unique(npx_plot[[x_var]])
      uniq_vals <- uniq_vals[!is.na(uniq_vals)]
      star_info <- data.frame(
        x_vals = uniq_vals,
        id = seq_along(uniq_vals)
      )
      ttest_tmp <- ttest_results[
        ttest_results$OlinkID %in% assays,
        ,
        drop = FALSE
      ]
      scale_inf <- npx_plot |>
        dplyr::group_by(.data[["Name_OID"]]) |>
        dplyr::summarise(
          maxNPX = max(.data[["NPX"]]),
          rangeNPX = diff(range(.data[["NPX"]])),
          .groups = "drop"
        )
      line_data <- ttest_tmp |>
        dplyr::left_join(scale_inf, by = "Name_OID") |>
        dplyr::mutate(
          C1 = uniq_vals[1],
          C2 = uniq_vals[2],
          y_anchor = .data[["maxNPX"]] + .data[["rangeNPX"]] * 0.2
        ) |>
        tidyr::pivot_longer(
          c(.data[["C1"]], .data[["C2"]]),
          names_to = "tmp",
          values_to = "x_vals"
        ) |>
        dplyr::mutate(
          star = dplyr::case_when(
            .data[["Adjusted_pval"]] < 0.05 &
              .data[["Adjusted_pval"]] > 0.01 ~ "*",
            .data[["Adjusted_pval"]] <= 0.01 &
              .data[["Adjusted_pval"]] > 0.005 ~ "**",
            .data[["Adjusted_pval"]] <= 0.005 ~ "***",
            TRUE ~ NA_character_
          )
        ) |>
        dplyr::left_join(star_info, by = "x_vals") |>
        dplyr::group_by(.data[["Name_OID"]]) |>
        dplyr::mutate(x_m = mean(.data[["id"]])) |>
        dplyr::ungroup() |>
        dplyr::filter(.data[["Threshold"]] == "Significant")
      p <- ggplot2::ggplot(
        npx_plot,
        ggplot2::aes(x = .data[[x_var]], y = .data[["NPX"]])
      ) +
        ggplot2::geom_boxplot(
          ggplot2::aes(fill = .data[[fill_var]])
        ) +
        ggplot2::geom_line(
          data = line_data,
          ggplot2::aes(x = .data[["x_vals"]], y = .data[["y_anchor"]],
                       group = .data[["Name_OID"]])
        ) +
        ggplot2::geom_text(
          data = line_data[line_data$tmp == "C1", ],
          ggplot2::aes(x = .data[["x_m"]], y = .data[["y_anchor"]] + 0.1,
                       label = .data[["star"]])
        ) +
        OlinkAnalyze::set_plot_theme() +
        OlinkAnalyze::olink_fill_discrete(...) +
        ggplot2::theme(
          axis.ticks.x = ggplot2::element_blank(),
          legend.text = ggplot2::element_text(size = 13)
        ) +
        ggplot2::facet_wrap(~.data[["Name_OID"]], scales = "free")
    }
    # ---- Clean up single-variable case --------------------------------------
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
  return(invisible(list_of_plots))
}
