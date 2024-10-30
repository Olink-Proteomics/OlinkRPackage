"
# Date : October 29th, 2024
# Aim : Plots for bridgeable
Author : Amrita Kar
"

rm(list = ls())

library(tidyverse)
library(OlinkAnalyze)
library(ggplot2)
library(ggpubr)
library(PupillometryR) # for iqr_range_plts (work around this else use as suggested package)
library(ggpmisc) # for r2_plts (work around this else use as suggested package)

# make each page an assay

bridgeable_plts <- function(data = npx_br_data , median_counts_threshold = 150){

  out_plts <- list()

  ids <- npx_br_data |>
    dplyr::select(OlinkID, BridgingRecommendation) |> #HT OlinkID
    dplyr::distinct() |>
    dplyr::pull(OlinkID)

  # Adjusting the platform
  npx_br_data <- npx_br_data |>
    dplyr::mutate(Panel = dplyr::case_when(!Panel == "Explore HT" |
                                             is.element("_II", Panel) ~
                                             "Explore 3072",
                                           !Panel == "Explore HT" ~
                                             "Explore 1536",
                                           TRUE ~ Panel))
  platforms <- unique(npx_br_data$Panel)

  npx_br_data_wider <- data.frame(
    Assay = unique(npx_br_data$Assay),
    OlinkID = unique(npx_br_data$OlinkID),
    NPX_1 <- npx_br_data |>
      dplyr::filter(Panel == platforms[1]) |>
      dplyr::pull(NPX),
    NPX_2 <- npx_br_data |>
      dplyr::filter(Panel == platforms[2]) |>
      dplyr::pull(NPX)
  )
  colnames(npx_br_data_wider)[3:4] <- c(paste0("NPX_",platforms[1]),
                                         paste0("NPX_",platforms[2]))
  # IQR/Range plot ----
  iqr_range_plt <- function(data = npx_br_data, id = id){
    iqr_range_plt <- npx_br_data |>
      dplyr::filter(OlinkID %in% id) |>
      ggplot2::ggplot(aes(x = paste(Assay, OlinkID, sep = "\n") , y = NPX,
                          fill = Panel)) +
      PupillometryR::geom_flat_violin(position = position_nudge(x = .2),
                                      alpha = .4) +
      geom_boxplot(width = .25, outlier.shape = NA, alpha = 0.4) +
      scale_fill_manual(values = c("#FF1F05", "#00C7E1")) +
      scale_color_manual(values = c("#FF1F05", "#00C7E1")) +
      labs(x = "Assay", y = "NPX Distribution", fill = "Platform: ") +
      guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
      OlinkAnalyze::set_plot_theme(font = "") +
      theme(axis.text.x = element_blank()) +
      facet_wrap(. ~ paste(Assay, OlinkID, sep = "\n"), scales = "free")
    return(iqr_range_plt)
  }

  # Correlation plot ----
  r2_plt <- function(data = npx_br_data_wider, id = id){
    r2_plt <- npx_br_data_wider |>
      dplyr::filter(OlinkID %in% id) |>
      ggplot2::ggplot() +
      geom_point(mapping = aes(x = `NPX_Explore HT`,
                               y = `NPX_Explore 3072`),
                 color = 'blue', alpha = 0.4) +
      ggpmisc::stat_poly_line(mapping = aes(x = `NPX_Explore HT`,
                                            y = `NPX_Explore 3072`),
                              color = 'black') +
      ggpmisc::stat_poly_eq(mapping = aes(x = `NPX_Explore HT`,
                                          y = `NPX_Explore 3072`,
                                          label = paste(after_stat(rr.label)))) +
      OlinkAnalyze::set_plot_theme(font = "") +
      OlinkAnalyze::olink_color_discrete() +
      facet_wrap(. ~ paste(Assay, OlinkID, sep = "\n"), scales = "free")
    return(r2_plt)
  }

  # Median counts plot ----
  counts_plt <- function(data = npx_br_data, median_counts_threshold,
                         id = id) {
    counts_plt <- npx_br_data |>
      dplyr::filter(OlinkID %in% id) |>
      dplyr::group_by(OlinkID, Panel) |>
      dplyr::mutate(median_count = median(Count[Count > 10], na.rm = T)) |>
      dplyr::ungroup() |>
      dplyr::mutate(Assay = factor(paste(Assay, OlinkID, sep = "\n")),
                    Panel = factor(Panel)) |>
      dplyr::group_by(Assay, Panel) |>
      dplyr::reframe(n = median_count) |>
      dplyr::ungroup() |>
      ggplot2::ggplot(aes(x = Assay, fill = Panel, y = n, label = n)) +
      geom_col(width = 0.5, position = 'dodge') +
      geom_text(position = position_dodge(0.5), vjust = -0.25) +
      labs(y = "Median Count", fill = "Platform:") +
      OlinkAnalyze::set_plot_theme(font = "") +
      theme(axis.text.x = element_blank()) +
      facet_wrap(. ~ Assay, scales = "free") +
      geom_hline(yintercept = median_counts_threshold, color = "#FF1F05",
                 linewidth = 0.7)
    return(counts_plt)
  }

  # KS plot ----
  ks_plt <- function(data = npx_br_data, id = id) {
    data1 <- npx_br_data |>
      dplyr::filter(Panel %in% platforms[1],
                    OlinkID %in% id)
    data2 <- npx_br_data |>
      dplyr::filter(Panel %in% platforms[2],
                    OlinkID %in% id)

    # Calculate empirical cumulative distribution function (ECDF) per platform
    ecdf_data1 <- ecdf(data1$NPX)
    ecdf_data2 <- ecdf(data2$NPX)

    # Find min and max statistics to draw line between points of greatest distance
    minMax <- seq(min(data1$NPX, data2$NPX),
                  max(data1$NPX, data2$NPX),
                  length.out = length(data1$NPX))
    x0 <- minMax[which(abs(ecdf_data1(minMax) - ecdf_data2(minMax)) ==
                         max(abs(ecdf_data1(minMax) - ecdf_data2(minMax))))]
    y0 <- ecdf_data1(x0)
    y1 <- ecdf_data2(x0)

    ks_results = ks.test(data1$NPX, data2$NPX)

    # Main KS plot construction
    ks_plt <- npx_br_data |>
      dplyr::filter(OlinkID %in% id) |>
      ggplot2::ggplot(aes(x = NPX, group = Panel, color = Panel)) +
      scale_color_manual(values = c("#FF1F05", "#00C7E1")) +
      stat_ecdf(linewidth = 1) +
      theme_bw(base_size = 10) +
      theme(legend.position = "top") +
      xlab("NPX") +
      ylab("ECDF") +
      geom_segment(aes(x = x0[1], y = y0[1], xend = x0[1], yend = y1[1]),
                   linetype = "dashed", color = "black") +
      geom_point(aes(x = x0[1], y = y0[1]), color = "black", size = 2) +
      geom_point(aes(x = x0[1], y = y1[1]), color = "black", size = 2) +
      facet_wrap(. ~ paste(Assay, OlinkID, sep = "\n"),
                 scales = "free", nrow = 4, ncol = 5) +
      annotate("text",
               x = Inf, y = 0.1, hjust = 1, cex = 2.5,
               label = paste0(
                 "D = ", signif(ks_results$statistic, 2),
                 "\nP-value = ", signif(ks_results$p.value, 2))) +
      theme(legend.title = element_blank()) +
      OlinkAnalyze::set_plot_theme() +
      labs(color = "Platform:")
    return(ks_plt)
  }

  # Bridegeable plot ----
  for (i in 1:length(ids)){
    final_plts <- list()

    message(paste0("Working on OlinkID : ", i ))
    final_plts[["iqr"]] <- iqr_range_plt(data = npx_br_data, id = ids[i])
    message("IQR plot done")
    final_plts[["r2"]] <- r2_plt(data = npx_br_data, id = ids[i])
    message("R2 plot done")
    final_plts[["counts"]] <- counts_plt(data = npx_br_data,
                                  median_counts_threshold = 150, id = ids[i])
    message("Counts plot done")
    final_plts[["ks"]] <- r2_plt(data = npx_br_data, id = ids[i])
    message("KS plot done")

    out <- ggpubr::ggarrange(plotlist = final_plts, widths = 1, heights = 1,
                             legend = "right", common.legend = T)

    out_plts[[i]] <- out
    rm(out, final_plts)
  }

  return(out_plts)
}

npx_ht <- OlinkAnalyze:::data_ht_small |>
  dplyr::filter(SampleType == "SAMPLE") |>
  dplyr::mutate(Project = "data1")
npx_3072 <- OlinkAnalyze:::data_3k_small |>
  dplyr::filter(SampleType == "SAMPLE") |>
  dplyr::mutate(Project = "data2")

overlapping_samples <- unique(
  intersect(npx_ht |>
              dplyr::distinct(SampleID) |>
              dplyr::pull(),
            npx_3072 |>
              dplyr::distinct(SampleID) |>
              dplyr::pull()))

npx_br_data <- olink_normalization(df1 = npx_ht,
                                   df2 = npx_3072,
                                   overlapping_samples_df1 =
                                     overlapping_samples,
                                   df1_project_nr = "Explore HT",
                                   df2_project_nr = "Explore 3072",
                                   reference_project = "Explore HT")

olinkids <- unique(paste0(npx_br_data$OlinkID,"_",npx_br_data$Assay))

results <- bridgeable_plts(data = npx_br_data, median_counts_threshold = 150)
names(results) <- olinkids

