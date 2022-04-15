# ScreenR <img src="man/figures/logo.png" align="right" height="137"/>

[![codecov](https://codecov.io/gh/EmanuelSoda/ScreenR/branch/master/graph/badge.svg?token=NX5YVRP4L0)](https://codecov.io/gh/EmanuelSoda/ScreenR) [![R-CMD-check](https://github.com/EmanuelSoda/ScreenR/workflows/R-CMD-check/badge.svg)](https://github.com/EmanuelSoda/ScreenR/actions)

The aim of ScreenR is to help in the analysis of High Throughput Biological Screening using pooled shRNAs. Those type of screening uses RNA-seq expression data to find candidate hits. ScreenR tries to combine together the power of software like edgeR with the semplicity of package like the metapackage Tydiverse. Using ScreenR a pipeline able to find candidate hits can be applied on RNA-seq data moreover it integrates a wide range of visualization in order to shows the results obtained.

## Installation

``` r
# Development version from GitHub:
# install.packages("devtools")
devtools::install_github("EmanuelSoda/ScreenR")
```

## Getting started

``` r
library(ScreenR)
```
