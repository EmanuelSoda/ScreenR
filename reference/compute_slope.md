# Compute the slope of the trend of genes

Computes, for each gene, the slope of a regression line of the
normalized counts (Frequency column of the data table) of its barcodes
over the time points. The time points (Day column of the data table) are
numbered 1, 2, ... in the order of the samples, so they are treated as
equally spaced, and replicates of the same time point share the same
number. Only the samples whose Treatment is in group_var are used. It is
used by
[`filter_by_slope`](https://emanuelsoda.github.io/ScreenR/reference/filter_by_slope.md).

## Usage

``` r
compute_slope(screenR_Object, genes, group_var)
```

## Arguments

- screenR_Object:

  The ScreenR object obtained using
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md).

- genes:

  The genes for which the slope is computed.

- group_var:

  The values of the Treatment column of the data table (see
  [`get_sample_table`](https://emanuelsoda.github.io/ScreenR/reference/get_sample_table.md))
  of the samples to use.

## Value

A row-wise tibble with the columns Gene and Slope, with one row for each
gene.

## See also

[`filter_by_slope`](https://emanuelsoda.github.io/ScreenR/reference/filter_by_slope.md),
[`plot_trend`](https://emanuelsoda.github.io/ScreenR/reference/plot_trend.md)

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))

compute_slope(object,
    genes = c("Gene_42", "Gene_24"),
    group_var = c("T1", "T2", "TRT")
)
#> # A tibble: 2 × 2
#> # Rowwise:  Gene
#>   Gene     Slope
#>   <chr>    <dbl>
#> 1 Gene_24 -0.597
#> 2 Gene_42 -1.33 
```
