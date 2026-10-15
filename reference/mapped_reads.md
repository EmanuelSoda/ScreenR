# Compute the number of mapped reads of each sample

Computes the total number of reads mapped to the barcodes in each sample
(the library size). Samples with far fewer mapped reads than the others
may have a lower quality. Use
[`plot_mapped_reads`](https://emanuelsoda.github.io/ScreenR/reference/plot_mapped_reads.md)
to plot the result.

## Usage

``` r
mapped_reads(screenR_Object)
```

## Arguments

- screenR_Object:

  The ScreenR object obtained using
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md).

## Value

A tibble with one row for each sample and the columns Sample and Mapped
(the total number of mapped reads).

## See also

[`count_mapped_reads`](https://emanuelsoda.github.io/ScreenR/reference/count_mapped_reads.md),
[`plot_mapped_reads`](https://emanuelsoda.github.io/ScreenR/reference/plot_mapped_reads.md),
[`barcode_lost`](https://emanuelsoda.github.io/ScreenR/reference/barcode_lost.md)

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))
mapped_reads(object)
#> # A tibble: 14 × 2
#>    Sample        Mapped
#>    <fct>          <int>
#>  1 T1          10671757
#>  2 T2           9799724
#>  3 Time3_TRT_A  6824877
#>  4 Time3_TRT_B  9107354
#>  5 Time3_TRT_C  9717868
#>  6 Time3_A      9485195
#>  7 Time3_B      9016812
#>  8 Time3_C      9568624
#>  9 Time4_TRT_A  8933915
#> 10 Time4_TRT_B  9883209
#> 11 Time4_TRT_C 12325124
#> 12 Time4_A     10351136
#> 13 Time4_B      9180192
#> 14 Time4_c      9315365
```
