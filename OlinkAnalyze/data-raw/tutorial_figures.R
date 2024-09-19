
# Tutorial Figures --------------------------------------------------------


# Outlier Figures ---------------------------------------------------------
# Figure 1
outlier_data <- npx_data1 |> 
  dplyr::mutate(NPX = ifelse(SampleID == "A25", NPX + 4, NPX)) |> 
  dplyr::mutate(NPX = ifelse(SampleID == "A52", NPX - 4, NPX)) |> 
  dplyr::filter(!stringr::str_detect(SampleID, "CONTROL"))

group_data <- npx_data1 |> 
  dplyr::mutate(NPX = ifelse(Site == "Site_D", NPX + 3, NPX)) |> 
  dplyr::filter(!stringr::str_detect(SampleID, "CONTROL"))

p1<- outlier_data |> olink_pca_plot(label_samples = T, quiet = T)
p2<- group_data |> olink_pca_plot(color_g = "Site", quiet = T)
ggpubr::ggarrange(p1[[1]], p2[[1]], nrow = 2, labels = "AUTO")
ggplot2::ggsave("man/figures/PCA_Outlier_Fig1.png", dpi = "screen")

rm(list = c("p1", "p2"))


# PCA Treatment -----------------------------------------------------------
OlinkAnalyze::npx_data1 |> 
  dplyr::filter(stringr::str_detect(SampleID, "CONTROL", negate = T)) |> # Filter duplicate SampleIDs
  olink_pca_plot(color_g = "Treatment")
ggplot2::ggsave("man/figures/PCA_Treatment.png", dpi = "screen")
