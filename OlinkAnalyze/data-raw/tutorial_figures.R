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
  p1[[1]], p2[[1]] ,
  nrow = 1L,
  labels = "AUTO",
  legend = "bottom"
)
ggplot2::ggsave(
  filename = "man/figures/PCA_Outlier_Fig1.png",
  width = 6L,
  height = 2L,
  units = "in",
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
  width = 3L,
  height = 2L,
  units = "in",
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
  width = 4L,
  height = 3L,
  units = "in",
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
  width = 4L,
  height = 3L,
  units = "in",
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
  width = 3L,
  height = 2L,
  units = "in",
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
  width = 6L,
  height = 2L,
  units = "in",
  dpi = "screen"
)

# Dist Box plot -----------------------------------------------------------

outlier_data |>
  dplyr::filter(
    .data[["SampleID"]] %in% c("A25", "A52", "A1", "A2", "A3", "A5", "A15",
                               "A16", "A18", "A19", "A20")
  ) |>
  olink_dist_plot()
ggplot2::ggsave(
  filename = "man/figures/dist_boxplot.png",
  width = 6L,
  height = 2L,
  units = "in",
  dpi = "screen"
)

# Site boxplot ------------------------------------------------------------

group_data |>
  dplyr::filter(
    .data[["Site"]] %in% c("Site_A", "Site_D")
  ) |> # Only look at 2 sites so that all samples can be seen
  olink_dist_plot(
    color_g = "Site"
  )
ggplot2::ggsave(
  filename = "man/figures/site_boxplot.png",
  width = 6L,
  height = 2L,
  units = "in",
  dpi = "screen"
)

# Sample Median Adjustment ------------------------------------------------

# Calculate SampleID Median NPX
median_NPX<-group_data |>
  dplyr::group_by(SampleID) |>
  dplyr::summarise(Median_NPX = median(NPX))

# Adjust by sample median ---------------------
adjusted_data <- group_data |>
  dplyr::inner_join(median_NPX, by = "SampleID")|>
  dplyr::mutate(NPX = NPX - Median_NPX)

adjusted_data|>
  dplyr::filter(Site %in% c("Site_A", "Site_D")) |> # Only visualizing 2 sites to see all samples
  olink_dist_plot(color_g = "Site")
ggplot2::ggsave(
  filename = "man/figures/sample_med_boxplot.png",
  width = 6L,
  height = 2L,
  units = "in",
  dpi = "screen")

# QC Plot -----------------------------------------------------------------

outlier_data |>
  olink_qc_plot()
ggplot2::ggsave(
  filename = "man/figures/qc_plot.png",
  width = 6L,
  height = 2L,
  units = "in",
  dpi = "screen")

# QC Site -----------------------------------------------------------------

group_data |>
  olink_qc_plot(color_g = "Site")
ggplot2::ggsave(
  filename = "man/figures/qc_site_plot.png",
  width = 6L,
  height = 2L,
  units = "in",
  dpi = "screen")

# QC Label ----------------------------------------------------------------

outlier_data |>
  olink_qc_plot(median_outlierDef = 2, IQR_outlierDef = 4,
                outlierLines = TRUE, label_outliers = TRUE)
ggplot2::ggsave(
  filename = "man/figures/qc_label_plot.png",
  width = 6L,
  height = 2L,
  units = "in",
  dpi = "screen")

# QC No lines -------------------------------------------------------------

outlier_data |>
  olink_qc_plot(median_outlierDef = 2, IQR_outlierDef = 4,
                outlierLines = FALSE, label_outliers = TRUE)

ggplot2::ggsave(
  filename = "man/figures/qc_nolines_plot.png",
  width = 6L,
  height = 2L,
  units = "in",
  dpi = "screen")

# 3k to HT bridging Figures -----------------------------------------------

# Read data
data_explore3072 <- readRDS(file = "tests/data/example_3k_data.rds")
data_exploreht <- readRDS(file = "tests/data/example_HT_data.rds")

data_explore3072_samples <- data_explore3072 |>
  dplyr::filter(
    .data[["SampleType"]] == "SAMPLE"
  ) |>
  dplyr::distinct(
    .data[["SampleID"]]
  ) |>
  dplyr::pull()

data_exploreht_samples <- data_exploreht |>
  dplyr::filter(
    .data[["SampleType"]] == "SAMPLE"
  ) |>
  dplyr::distinct(
    .data[["SampleID"]]
  ) |>
  dplyr::pull()

overlapping_samples <- intersect(
  x = data_explore3072_samples,
  y = data_exploreht_samples
) |>
unique()

# Overlapping samples table -----------------------------------------------

matrix(overlapping_samples, ncol = 4L) |>
  saveRDS("man/figures/overlapping_samples_table.rds")

# PCAs before bridging ----------------------------------------------------

data_explore3072_before_br <- data_explore3072 |>
  dplyr::filter(
    .data[["SampleType"]] == "SAMPLE"
  ) |>
  # Note that if `SampleType` is not is input data,
  # stringr::str_detect can be used to exclude control samples
  #  based on naming convention.
  dplyr::mutate(
    Type = dplyr::if_else(
      .data[["SampleID"]] %in% .env[["overlapping_samples"]],
      "Explore 3072 Bridge",
      "Explore 3072 Sample"
    )
  )

data_exploreht_before_br <- data_exploreht |>
  dplyr::filter(
    .data[["SampleType"]] == "SAMPLE"
  ) |>
  dplyr::mutate(
    Type = dplyr::if_else(
      .data[["SampleID"]] %in% .env[["overlapping_samples"]],
      "Explore HT Bridge",
      "Explore HT Sample"
    )
  )

pca_e3072 <- OlinkAnalyze::olink_pca_plot(
  df = data_explore3072_before_br,
  color_g = "Type",
  quiet = TRUE
)

pca_eht <- OlinkAnalyze::olink_pca_plot(
  df = data_exploreht_before_br,
  color_g = "Type",
  quiet = TRUE
)

ggpubr::ggarrange(
  pca_e3072[[1L]], pca_eht[[1L]],
  nrow = 1L,
  legend = "bottom"
)
ggplot2::ggsave(
  filename = "man/figures/PCA_btw_product_before.png",
  width = 6L,
  height = 2.5,
  units = "in",
  dpi = "screen"
)

rm(pca_e3072, pca_eht)

# Normalize products ------------------------------------------------------

# Find shared samples
npx_ht <- data_exploreht |>
  dplyr::mutate(Project = "data1")
npx_3072 <- data_explore3072 |>
  dplyr::mutate(Project = "data2")

npx_br_data <- OlinkAnalyze::olink_normalization(
  df1 = npx_ht,
  df2 = npx_3072,
  overlapping_samples_df1 = overlapping_samples,
  df1_project_nr = "Explore HT",
  df2_project_nr = "Explore 3072",
  reference_project = "Explore HT"
)

rm(npx_ht, npx_3072)

# Bridge table results ----------------------------------------------------

npx_br_data |>
  dplyr::filter(
    .data[["Project"]] == "Explore 3072"
  ) |>
  dplyr::slice_head(
    n = 5L
  ) |>
  saveRDS(
    file = "man/figures/bridging_results.rds"
  )

# SC pre bridging-------------------------------------------------------

npx_br_data |>
  dplyr::filter(
    .data[["SampleType"]] == "SAMPLE_CONTROL"
  ) |>
  dplyr::mutate(
    OlinkID = paste0(.data[["OlinkID"]], "_", .data[["OlinkID_E3072"]]),
    SampleID = paste0(.data[["Project"]], .data[["SampleID"]])
  ) |>
  OlinkAnalyze::olink_pca_plot(
    color_g = "Project"
  )

ggplot2::ggsave(
  filename = "man/figures/SCs_pre_bridging.png",
  width = 3L,
  height = 2L,
  units = "in",
  dpi = "screen"
)

# Bridge sample pre bridging ----------------------------------------------

npx_br_data |>
  dplyr::filter(
    .data[["SampleType"]] == "SAMPLE"
    & .data[["SampleID"]] %in% .env[["overlapping_samples"]]
  ) |>
  dplyr::mutate(
    OlinkID = paste0(.data[["OlinkID"]], "_", .data[["OlinkID_E3072"]]),
    SampleID = paste0(.data[["Project"]], .data[["SampleID"]])
  ) |>
  OlinkAnalyze::olink_pca_plot(
    color_g = "Project"
  )

ggplot2::ggsave(
  filename = "man/figures/bridges_pre_bridging.png",
  width = 3L,
  height = 2L,
  units = "in",
  dpi = "screen"
)

# Cleanup
rm(data_explore3072, data_explore3072_before_br, data_explore3072_samples,
   data_exploreht, data_exploreht_before_br, data_exploreht_samples,
   group_data, outlier_data)

# Recommended bridging data wrangling ------------------------

npx_after_br_reco <- npx_br_data |>
  dplyr::filter(
    .data[["BridgingRecommendation"]] != "Not Bridgeable"
    & .data[["AssayType"]] == "assay"
  ) |>
  dplyr::mutate(
    NPX = dplyr::case_match(
      .data[["BridgingRecommendation"]],
      "MedianCentering" ~ .data[["MedianCenteredNPX"]],
      "QuantileSmoothing" ~ .data[["QSNormalizedNPX"]],
      .default = .data[["NPX"]]
    ),
    OlinkID = paste0(.data[["OlinkID"]], "_", .data[["OlinkID_E3072"]])
  )

### Generate unique SampleIDs

npx_after_br_final <- npx_after_br_reco |>
  dplyr:::mutate(
    SampleID = paste0(.data[["Project"]], .data[["SampleID"]])
  )

# PCA plot of the data from SCs ------------------------------

npx_after_br_final |>
  dplyr::filter(
    .data[["SampleType"]] == "SAMPLE_CONTROL"
  ) |>
  OlinkAnalyze::olink_pca_plot(
    color_g = "Project"
  )

ggplot2::ggsave(
  filename = "man/figures/SCs_post_bridging.png",
  width = 3L,
  height = 2L,
  units = "in",
  dpi = "screen"
)

# PCA plot of the data from bridging samples ------------------

npx_after_br_reco |>
  dplyr::filter(
    .data[["SampleType"]] == "SAMPLE"
    & .data[["SampleID"]] %in% .env[["overlapping_samples"]]
  ) |>
  dplyr:::mutate(
    SampleID = paste0(.data[["Project"]], .data[["SampleID"]])
  ) |>
  OlinkAnalyze::olink_pca_plot(
    color_g = "Project"
  )

ggplot2::ggsave(
  filename = "man/figures/bridges_post_bridging.png",
  width = 3L,
  height = 2L,
  units = "in",
  dpi = "screen"
)
