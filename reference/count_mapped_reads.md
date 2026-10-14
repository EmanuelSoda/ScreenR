# Count the number of mapped read

This function counts the number of reads for each barcode in each
sample. It is a quality control function (QC) to see if the biological
protocol went as planned. If a sample has very low mapped compared to
the other means that is has a lower quality.

## Usage

``` r
count_mapped_reads(screenR_Object)
```

## Arguments

- screenR_Object:

  The ScreenR object obtained using the
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md)

## Value

Return a tibble containing the number of mapped read for sample

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))
head(count_mapped_reads(object))
#> # A tibble: 6 × 3
#>   Barcode Sample Mapped
#>   <fct>   <fct>   <int>
#> 1 10208   T1       3520
#> 2 10502   T1        280
#> 3 10796   T1       1507
#> 4 11089   T1       3797
#> 5 11382   T1        717
#> 6 11675   T1       7330
```
