---
title: "ScreenR Example Analysis"
output: BiocStyle::html_document
author:
- name: "Emanuel Michele Soda "
  affiliation: Istituto Europeo Oncologia (IEO),
               Milano, Italy
  email: emanuelmichele@ieo.it or emanuelsoda@gmail.com
package: ScreenR  
vignette: >
  %\VignetteIndexEntry{ScreenR Example Analysis}
  %\VignetteEncoding{UTF-8}
  %\VignetteEngine{knitr::knitr}
editor_options: 
  chunk_output_type: console
---

# Importing Pacakge

```r
library(ggplot2)
library(dplyr)
library(tidyr)
knitr::opts_chunk$set(fig.width=10, fig.height=7) 
```

# Introduction 
The R package **ScreenR** has been developed to perform the analysis of data 
coming from RNA-seq data generated using Genetic Screening. It is based 
on the same idea of edgeR but it also integrate the idea at the base of 
Tidyverse. 

# Installation {#installation}

## Bioconductor

**ScreenR** requires several CRAN and Bioconductor R packages to be
installed. Dependencies are usually handled automatically, when installing the
package using the following commands:


```r
install.packages("BiocManager")
BiocManager::install("ScreenR")
```

## Manual installation

In the unlikely case that a manual installation is required, e.g., if you do
not install **ScreenR** via Bioconductor (which is highly recommended),
you can install CRAN pacakges using: 


```r
install.packages("<package_name>")
```

While Bioconductor packages can be installed from R using the following command:


```r
BiocManager::install("<package_name>")
```

Sometimes, it may also be useful to update Bioconductor:


```r
BiocManager::install()
```

Finally, the manual installation of **ScreenR** can, for example, be
done from the command line ...


```r
R CMD INSTALL ScreenR_<version>.tar.gz
```







































