# Tutorial Figures --------------------------------------------------------

# Outlier Figures ---------------------------------------------------------

# Figure 1
outlier_data <- npx_data1 |>
  dplyr::mutate(
    NPX = dplyr::case_match(
      .data[["SampleID"]],
      "A25" ~ .data[["NPX"]] + 4L,
      "A52" ~ .data[["NPX"]] - 4L,
      .default = .data[["NPX"]]
    )
  ) |>
  dplyr::filter(
    !stringr::str_detect(.data[["SampleID"]], "CONTROL")
  )

group_data <- npx_data1 |>
  dplyr::mutate(
    NPX = dplyr::if_else(.data[["Site"]] == "Site_D",
                         .data[["NPX"]] + 3L,
                         .data[["NPX"]])
  ) |>
  dplyr::filter(
    !stringr::str_detect(.data[["SampleID"]], "CONTROL")
  )

p1 <- outlier_data |>
  OlinkAnalyze::olink_pca_plot(
    label_samples = TRUE,
    quiet = TRUE
  )
p2 <- group_data |>
  OlinkAnalyze::olink_pca_plot(
    color_g = "Site",
    quiet = TRUE
  )

ggpubr::ggarrange(
  p1[[1]], p2[[1]],
  nrow = 2L,
  labels = "AUTO"
)
ggplot2::ggsave(
  filename = "man/figures/PCA_Outlier_Fig1.png",
  dpi = "screen"
)

rm(p1, p2)

# PCA Treatment -----------------------------------------------------------

OlinkAnalyze::npx_data1 |>
  dplyr::filter(
    !stringr::str_detect(.data[["SampleID"]], "CONTROL")
  ) |> # Filter duplicate SampleIDs
  olink_pca_plot(
    color_g = "Treatment"
  )
ggplot2::ggsave(
  filename = "man/figures/PCA_Treatment.png",
  dpi = "screen"
)

# PCA Panel ---------------------------------------------------------------

OlinkAnalyze::npx_data2 |>
  dplyr::filter(
    !stringr::str_detect(.data[["SampleID"]], "CONTROL")
  ) |> # Filter out control SampleIDs
  olink_pca_plot(
    byPanel = TRUE
  ) # Specify by panel
ggplot2::ggsave(
  filename = "man/figures/PCA_Panel.png",
  dpi = "screen"
)

# Outlier PCA -------------------------------------------------------------

outlier_data |>
  dplyr::filter(
    !stringr::str_detect(.data[["SampleID"]], "CONTROL")
  ) |> # Filter duplicate SampleIDs
  olink_pca_plot(
    byPanel = TRUE
  )
ggplot2::ggsave(
  filename = "man/figures/Outlier_PCA.png",
  dpi = "screen"
)

# Label samples -----------------------------------------------------------

outlier_data |>
  dplyr::filter(
    !stringr::str_detect(.data[["SampleID"]], "CONTROL")
  ) |> # Filter duplicate SampleIDs
  olink_pca_plot(
    label_samples = TRUE
  )
ggplot2::ggsave(
  filename = "man/figures/label_samples_pca.png",
  dpi = "screen"
)

# Outlier line PCA --------------------------------------------------------

outlier_data |>
  dplyr::filter(
    !stringr::str_detect(.data[["SampleID"]], "CONTROL")
  ) |> # Filter duplicate SampleIDs
  olink_pca_plot(
    outlierDefX = 3L,
    outlierDefY = 3L,
    outlierLines = TRUE,
    label_outliers = TRUE
  )
ggplot2::ggsave(
  filename = "man/figures/outlier_line_pca.png",
  dpi = "screen"
)

# Dist Box plot -----------------------------------------------------------

outlier_data |>
  dplyr::filter(
    .data[["SampleID"]] %in% c("A25", "A52", "A1", "A2", "A3", "A5", "A15",
                               "A16", "A18", "A19", "A20")
  )|>
  olink_dist_plot()
ggplot2::ggsave(
  filename = "man/figures/dist_boxplot.png",
  dpi = "screen"
)

# Site boxplot ------------------------------------------------------------

group_data |>
  dplyr::filter(
    .data[["Site"]] != "Site_E"
  ) |> # Site E filtered out so that all samples can be seen
  olink_dist_plot(
    color_g = "Site"
  )
ggplot2::ggsave(
  filename = "man/figures/site_boxplot.png",
  dpi = "screen"
)
