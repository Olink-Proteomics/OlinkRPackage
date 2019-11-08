
<!-- README.md is generated from README.Rmd. Please edit that file -->

# OlinkAnalyze

<!-- badges: start -->

<!-- badges: end -->

The goal of OlinkAnalyze is to make working with Olink NPX data easy and
easily accessible.

## Installation

You can install the released version of OlinkAnalyze from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Olink-Proteomics/OlinkRPackage")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(OlinkAnalyze)

data <- read_NPX(NPXdata.xlsx)

olink_boxplot(data)

olink_pca_plot(data)
```
