
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ScreenR

<!-- badges: start -->

[![codecov](https://codecov.io/gh/EmanuelSoda/ScreenR/branch/master/graph/badge.svg?token=NX5YVRP4L0)](https://codecov.io/gh/EmanuelSoda/ScreenR)
[![GitHub
issues](https://img.shields.io/github/issues/EmanuelSoda/ScreenR)](https://github.com/EmanuelSoda/ScreenR/issues)
[![GitHub
pulls](https://img.shields.io/github/issues-pr/EmanuelSoda/ScreenR)](https://github.com/EmanuelSoda/ScreenR/pulls)
[![check-bioc](https://github.com/EmanuelSoda/ScreenR/workflows/check-bioc/badge.svg)](https://github.com/EmanuelSoda/ScreenR/actions)
<!-- badges: end -->

The goal of `ScreenR` is to …

## Installation instructions

Get the latest stable `R` release from
[CRAN](http://cran.r-project.org/). Then install `ScreenR` from
[Bioconductor](http://bioconductor.org/) using the following code:

``` r
if (!requireNamespace("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager")
}

BiocManager::install("ScreenR")
```

And the development version from
[GitHub](https://github.com/EmanuelSoda/ScreenR) with:

``` r
BiocManager::install("EmanuelSoda/ScreenR")
```

## ScreenR overall workflow

<img src="man/figures/Pipeline.png" align="top" height="200"/> \##
Citation

Below is the citation output from using `citation('ScreenR')` in R.
Please run this yourself to check for any updates on how to cite
**ScreenR**.

``` r
print(citation('ScreenR'), bibtex = TRUE)
#> Warning in citation("ScreenR"): no date field in DESCRIPTION file of package
#> 'ScreenR'
#> Warning in citation("ScreenR"): could not determine year for 'ScreenR' from
#> package DESCRIPTION file
#> 
#> To cite package 'ScreenR' in publications use:
#> 
#>   Soda E (????). _ScreenR: Package to Perform High Throughput
#>   Biological Screening_. R package version 0.99.41,
#>   <https://emanuelsoda.github.io/ScreenR/>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {ScreenR: Package to Perform High Throughput Biological Screening},
#>     author = {Emanuel Michele Soda},
#>     note = {R package version 0.99.41},
#>     url = {https://emanuelsoda.github.io/ScreenR/},
#>   }
```

Please note that the `ScreenR` was only made possible thanks to many
other R and bioinformatics software authors, which are cited either in
the vignettes and/or the paper(s) describing this package.

## Code of Conduct

Please note that the `ScreenR` project is released with a [Contributor
Code of Conduct](http://bioconductor.org/about/code-of-conduct/). By
contributing to this project, you agree to abide by its terms.

## Development tools

-   Continuous code testing is possible thanks to [GitHub
    actions](https://www.tidyverse.org/blog/2020/04/usethis-1-6-0/)
    through *[usethis](https://CRAN.R-project.org/package=usethis)*,
    *[remotes](https://CRAN.R-project.org/package=remotes)*, and
    *[rcmdcheck](https://CRAN.R-project.org/package=rcmdcheck)*
    customized to use [Bioconductor’s docker
    containers](https://www.bioconductor.org/help/docker/) and
    *[BiocCheck](https://bioconductor.org/packages/3.15/BiocCheck)*.
-   Code coverage assessment is possible thanks to
    [codecov](https://codecov.io/gh) and
    *[covr](https://CRAN.R-project.org/package=covr)*.
-   The [documentation website](http://EmanuelSoda.github.io/ScreenR) is
    automatically updated thanks to
    *[pkgdown](https://CRAN.R-project.org/package=pkgdown)*.
-   The code is styled automatically thanks to
    *[styler](https://CRAN.R-project.org/package=styler)*.
-   The documentation is formatted thanks to
    *[devtools](https://CRAN.R-project.org/package=devtools)* and
    *[roxygen2](https://CRAN.R-project.org/package=roxygen2)*.

For more details, check the `dev` directory.

This package was developed using
*[biocthis](https://bioconductor.org/packages/3.15/biocthis)*.
