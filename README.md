
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![R-CMD-check](https://github.com/Olink-Proteomics/OlinkRPackage/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Olink-Proteomics/OlinkRPackage/actions/workflows/R-CMD-check.yaml)
[![](https://img.shields.io/badge/DOI-10.32614/CRAN.package.OlinkAnalyze-1f57b6?style=flat&link=https://doi.org/10.32614/CRAN.package.OlinkAnalyze)](https://doi.org/10.32614/CRAN.package.OlinkAnalyze)

# Olink® Analyze

The goal of Olink® Analyze is to provide a versatile toolbox to enable
easy and smooth handling of Olink NPX data to speed up your proteomic
research. Olink® Analyze provides functions ranging from reading Olink
NPX data as exported by NPX Manager to various statistical tests and
modelling, via different QC plot functions. Thereby providing a
convenient pipeline for your Olink NPX data analysis.

## Installation

Olink® Analyze is now available on CRAN:
<https://cran.r-project.org/web/packages/OlinkAnalyze/index.html>

``` r
install.packages("OlinkAnalyze")
```

## Vignette

``` r
browseVignettes("OlinkAnalyze")
```

## Usage

### Reading Olink NPX data

``` r
# open package
library(OlinkAnalyze)

# reading Olink NPX data 
my_NPX_data <- read_NPX(filename = "path/to/my_NPX_data.xlsx")
```

### QC plot functions

There are several plot functions, below follows two examples using the
package provided npx_data1 dataset:

``` r
# visualize the NPX distribution per sample per panel, example for one panel
olink_dist_plot(npx_data1 %>% filter(Panel == 'Olink CARDIOMETABOLIC')) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_fill_manual(values = c('turquoise3', 'red'))
```

![dist_plot_example](figures/example_distplot.png)

``` r
# visualize potential outliers by IQR vs. sample median per panel, example for one panel
olink_qc_plot(npx_data1 %>% filter(Panel == 'Olink CARDIOMETABOLIC')) +
  scale_color_manual(values = c('turquoise3', 'red'))
```

![qc_plot_example](figures/example_qcplot.png)

### Normalization

Olink® Analyze provides several means of normalization when analyzing
multiple datasets. Below follows an example of reference sample (aka
bridge) normalization using the two package provided npx_data1 and
npx_data2 datasets:

``` r
# identify bridge samples
bridge_samples <- intersect(x = npx_data1$SampleID,
               y = npx_data2$SampleID)

# bridge normalize
bridge_normalized_data <- olink_normalization(df1 = npx_data1,
                        df2 = npx_data2,
                        overlapping_samples_df1 = bridge_samples,
                        df1_project_nr = "20200001",
                        df2_project_nr = "20200002",
                        reference_project = "20200001")
```

### Statistical tests and models

Olink® Analyze provides several statistical tests and model tools. Below
follows an example of how to perform a t-test and how to visualize the
t-test output in a volcano plot using the package provided npx_data1
dataset:

``` r
# t-test npx_data1
ttest_results_NPX1 <- olink_ttest(df = npx_data1,
                variable = "Treatment")

# select names of the top #10 most significant proteins
ttest_sign_NPX1 <- ttest_results_NPX1 %>%
    head(n=10) %>%
    pull(OlinkID)

# volcano plot with annotated top #10 most significant proteins
olink_volcano_plot(p.val_tbl = ttest_results_NPX1,
                olinkid_list = ttest_sign_NPX1) +
  scale_color_manual(values = c('turquoise3', 'red'))
```

![volcano_plot_example](figures/example_volcanoplot.png)

## Learn more

Please see the function specific help pages. Moreover, Olink® Analyze
includes two simulated NPX datasets for your convenience to help you
explore the package and its functions.

## Issues

Please report any issues (good or bad) to \<biostattools\[a\]olink.com\>
or use the github [issue
function](https://github.com/Olink-Proteomics/OlinkRPackage/issues).

## Alternative install methods

To install directly from the github repository:

``` r
# install.packages("remotes")
remotes::install_github(repo ='Olink-Proteomics/OlinkRPackage/OlinkAnalyze', ref = "main", build_vignettes = TRUE)
```

To install Olink Analyze into a new
[conda](https://docs.conda.io/en/latest/) environment:

``` bash
conda create -n OlinkAnalyze -c conda-forge r-olinkanalyze
```

## Credits

Olink® Analyze is developed and maintained by the Olink Proteomics Data
Science Team.
