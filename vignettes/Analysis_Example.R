## ----packages, message=FALSE, warning=TRUE------------------------------------
library(ggplot2)
library(dplyr)
library(tidyr)
knitr::opts_chunk$set(fig.width=10, fig.height=7) 

## ----ScreenR install Bioc, eval=FALSE, message=FALSE, warning=TRUE------------
#  install.packages("BiocManager")
#  BiocManager::install("ScreenR")

## ----pkg install, eval=FALSE, message=FALSE, warning=TRUE---------------------
#  install.packages("<package_name>")

## ----pkg bioc install, eval=FALSE, message=FALSE, warning=TRUE----------------
#  BiocManager::install("<package_name>")

## ----bioc update, eval=FALSE, message=FALSE, warning=TRUE---------------------
#  BiocManager::install()

## ----manual install, eval=FALSE, message=FALSE, warning=TRUE------------------
#  R CMD INSTALL ScreenR_<version>.tar.gz

