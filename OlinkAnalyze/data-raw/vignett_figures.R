
# Load OlinkAnalyze
library(OlinkAnalyze)

# Load other libraries used in Vignette
library(dplyr)
library(ggplot2)
library(stringr)

### Importing DFs necessary for figure generation

# importing npx_data1
load("~/OlinkRPackage/OlinkAnalyze/data/npx_data1.rda")

npx_data1_no_controls <- npx_data1 |>
  filter(!str_detect(SampleID, regex("control|ctrl", ignore_case = TRUE))) |>
  filter(!str_detect(Assay,    regex("control|ctrl", ignore_case = TRUE)))


# olink_umap_plot ---------------------------------------------------------

npx_data1 %>% 
  filter(!str_detect(SampleID, 'CONTROL_SAMPLE')) %>% 
  olink_umap_plot(df = .,
                  color_g = "QC_Warning", 
                  byPanel = TRUE)  

ggplot2::ggsave("OlinkAnalyze/man/figures/olink_umap_plot.png", dpi = "screen")


# olink_boxplot - Example 1-------------------------------------------------

plot <- npx_data1_no_controls %>%
  na.omit() %>% # removing missing values which exists for Site
  olink_boxplot(variable = "Site", 
                olinkid_list = c("OID00488", "OID01276"),
                number_of_proteins_per_plot  = 2)
plot[[1]]
rm(plot)

ggplot2::ggsave("OlinkAnalyze/man/figures/olink_boxplot.png", dpi = "screen")


# olink_boxplot - Example 2 -----------------------------------------------


anova_posthoc_results<- npx_data1_no_controls %>% 
  olink_anova_posthoc(olinkid_list = c("OID00488", "OID01276"),
                      variable = 'Site',
                      effect = 'Site')

plot2 <- npx_data1_no_controls %>%
  na.omit() %>% # removing missing values which exists for Site
  olink_boxplot(variable = "Site", 
                olinkid_list = c("OID00488", "OID01276"),
                number_of_proteins_per_plot  = 2,
                posthoc_results = anova_posthoc_results)

plot2[[1]]

rm(anova_posthoc_results)
rm(plot2)

ggplot2::ggsave("OlinkAnalyze/man/figures/olink_boxplot_anova_posthoc.png",
                        dpi = "screen")


# olink_lmer_plot ---------------------------------------------------------

plot <- olink_lmer_plot(df = npx_data1, 
                        olinkid_list = c("OID01216", "OID01217"), 
                        variable = c('Site', 'Treatment'), 
                        x_axis_variable =  'Site',
                        col_variable = 'Treatment',
                        random = 'Subject')
plot[[1]]
rm(plot)

ggplot2::ggsave("OlinkAnalyze/man/figures/olink_lmer_plot.png",
                        dpi = "screen")


# olink_pathway_heatmap - Generating Data ---------------------------------

# GSEA Heatmap from t-test results - truncated for readability of assays
npx_df <- npx_data1 %>% 
  filter(!grepl("control", SampleID, ignore.case = TRUE))

ttest_results <- olink_ttest(
  df = npx_df,
  variable = "Treatment",
  alternative = "two.sided")


gsea_results <- olink_pathway_enrichment(
  data = npx_data1, 
  test_results = ttest_results
  )

ora_results  <- olink_pathway_enrichment(
  data = npx_data1, 
  test_results = ttest_results,
  method = "ORA"
  )


# olink_pathway_heatmap - GSEA --------------------------------------------

# GSA Heatmap from t-test results, limited to top assays for readability

olink_pathway_heatmap(enrich_results = gsea_results, 
                      test_results = ttest_results |> head(12))

ggplot2::ggsave("OlinkAnalyze/man/figures/olink_pathway_heatmap_gsea.png", dpi = "screen")


# olink_pathway_heatmap - ORA ---------------------------------------------

# ORA Heatmap from t-test results with cell keyword

olink_pathway_heatmap(enrich_results = ora_results, 
                      test_results = ttest_results,
                      method = "ORA", keyword = "cell")

ggplot2::ggsave("OlinkAnalyze/man/figures/olink_pathway_heatmap_ora.png", dpi = "screen")

rm(ttest_results)
rm(gsea_results)
rm(ora_results)


# olink_pathway_visualization ---------------------------------------------

first10 <- npx_data1 %>%
  pull(OlinkID) %>% 
  unique() %>% 
  head(10)

first15samples <- npx_data1$SampleID %>% 
  unique() %>% 
  head(15)

npx_data_small <- npx_data1 %>% 
  filter(!str_detect(SampleID, 'CONT')) %>% 
  filter(OlinkID %in% first10) %>% 
  filter(SampleID %in% first15samples)


olink_heatmap_plot(npx_data_small, variable_row_list =  'Treatment')

ggplot2::ggsave("OlinkAnalyze/man/figures/olink_heatmap_plot.png", dpi = "screen")

rm(first10)
rm(first15samples)
rm(npx_data_small)

# olink_volcano_plot ------------------------------------------------------

# perform t-test
ttest_results <- olink_ttest(df = npx_data1,
                             variable = 'Treatment')

top_10_name <- ttest_results %>%
  slice_head(n = 10) %>%
  pull(OlinkID)

# volcano plot
olink_volcano_plot(p.val_tbl = ttest_results,
                   x_lab = 'Treatment',
                   olinkid_list = top_10_name)

ggplot2::ggsave("OlinkAnalyze/man/figures/olink_volcano_plot.png", dpi = "screen")

rm(ttest_results)
rm(top_10_name)


# set_plot_theme applied to boxplot ---------------------------------------

npx_data1 %>% 
  filter(OlinkID == 'OID01216') %>% 
  ggplot(aes(x = Treatment, y = NPX, fill = Treatment)) +
  geom_boxplot() +
  set_plot_theme()

ggplot2::ggsave("OlinkAnalyze/man/figures/set_plot_theme_boxplot.png", dpi = "screen")


# olink_fill_discreted applied to boxplot ---------------------------------

npx_data1 %>% 
  filter(OlinkID == 'OID01216') %>% 
  ggplot(aes(x = Treatment, y = NPX, fill = Treatment)) +
  geom_boxplot() +
  set_plot_theme() +
  olink_fill_discrete()

ggplot2::ggsave("OlinkAnalyze/man/figures/olink_fill_discrete_boxplot.png", dpi = "screen")
