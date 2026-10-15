# Compute the Gini index of each sample

Computes the Gini index of the barcode counts of each sample, a measure
of how evenly the reads are distributed over the barcodes of the
library. It is 0 when all the barcodes have the same number of reads and
close to 1 when a few barcodes take most of the reads. A Gini index that
increases strongly over time or differs between replicates can indicate
a bottleneck or a technical problem. Use
[`plot_gini`](https://emanuelsoda.github.io/ScreenR/reference/plot_gini.md)
to plot it.

## Usage

``` r
compute_gini(screenR_Object)
```

## Arguments

- screenR_Object:

  The ScreenR object obtained using
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md).

## Value

A tibble with the columns Sample and Gini.

## See also

[`plot_gini`](https://emanuelsoda.github.io/ScreenR/reference/plot_gini.md),
[`barcode_lost`](https://emanuelsoda.github.io/ScreenR/reference/barcode_lost.md)

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))
compute_gini(object)
#> # A tibble: 14 × 2
#>    Sample       Gini
#>    <fct>       <dbl>
#>  1 T1          0.420
#>  2 T2          0.434
#>  3 Time3_TRT_A 0.445
#>  4 Time3_TRT_B 0.445
#>  5 Time3_TRT_C 0.443
#>  6 Time3_A     0.438
#>  7 Time3_B     0.441
#>  8 Time3_C     0.444
#>  9 Time4_TRT_A 0.477
#> 10 Time4_TRT_B 0.482
#> 11 Time4_TRT_C 0.483
#> 12 Time4_A     0.470
#> 13 Time4_B     0.467
#> 14 Time4_c     0.471
```
