# Compute the trend of genes

Internal function that computes, for each gene and sample, the mean
normalized count of the barcodes of the gene.

## Usage

``` r
compute_trend(screenR_Object, genes, group_var)
```

## Arguments

- screenR_Object:

  The ScreenR object obtained using
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md).

- genes:

  The names of the genes.

- group_var:

  The values of the Treatment column of the samples to use.

## Value

A tibble with the columns Sample, Gene and Frequency.
