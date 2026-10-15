# Compute the number of barcodes lost in each sample

Counts, for each sample, the barcodes lost during the screen or the
sequencing, that is the barcodes with zero mapped reads. Many lost
barcodes may indicate a sample of low quality or a bottleneck in the
screen. Use
[`plot_barcode_lost`](https://emanuelsoda.github.io/ScreenR/reference/plot_barcode_lost.md)
to plot the result.

## Usage

``` r
barcode_lost(screenR_Object)
```

## Arguments

- screenR_Object:

  The ScreenR object obtained using
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md).

## Value

A tibble with the columns Sample and LostBarcode (the number of barcodes
with zero reads). Samples without lost barcodes are not reported.

## See also

[`plot_barcode_lost`](https://emanuelsoda.github.io/ScreenR/reference/plot_barcode_lost.md),
[`plot_barcode_lost_for_gene`](https://emanuelsoda.github.io/ScreenR/reference/plot_barcode_lost_for_gene.md),
[`count_mapped_reads`](https://emanuelsoda.github.io/ScreenR/reference/count_mapped_reads.md)

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))
head(barcode_lost(object))
#> # A tibble: 6 × 2
#>   Sample      LostBarcode
#>   <fct>             <int>
#> 1 T1                    5
#> 2 T2                   21
#> 3 Time3_TRT_A          27
#> 4 Time3_TRT_B          28
#> 5 Time3_TRT_C          27
#> 6 Time3_A               9
```
