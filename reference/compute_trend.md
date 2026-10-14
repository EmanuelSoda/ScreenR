# Compute trend

This is an internal function used to computes the trend of a gene

## Usage

``` r
compute_trend(screenR_Object, genes, group_var)
```

## Arguments

- screenR_Object:

  object created with
  [`estimateDisp`](https://rdrr.io/pkg/edgeR/man/estimateDisp.html)

- genes:

  a list of genes

- group_var:

  the variable that as to be used as grouping variable

## Value

A table with the trend of the genes passed as input
