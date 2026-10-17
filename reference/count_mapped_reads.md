# Get the mapped reads of each barcode in each sample

Returns the count table in long format, with the number of reads mapped
to each barcode in each sample. It is the starting point of the quality
control functions, such as
[`mapped_reads`](https://emanuelsoda.github.io/ScreenR/reference/mapped_reads.md),
[`barcode_lost`](https://emanuelsoda.github.io/ScreenR/reference/barcode_lost.md)
and
[`plot_mapped_reads_distribution`](https://emanuelsoda.github.io/ScreenR/reference/plot_mapped_reads_distribution.md).

## Usage

``` r
count_mapped_reads(screenR_Object)
```

## Arguments

- screenR_Object:

  The ScreenR object obtained using
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md).

## Value

A tibble with one row for each barcode and sample and the columns
Barcode, Sample (a factor in the order of the samples of the count
table) and Mapped (the number of reads).

## See also

[`mapped_reads`](https://emanuelsoda.github.io/ScreenR/reference/mapped_reads.md),
[`barcode_lost`](https://emanuelsoda.github.io/ScreenR/reference/barcode_lost.md),
[`plot_mapped_reads_distribution`](https://emanuelsoda.github.io/ScreenR/reference/plot_mapped_reads_distribution.md)

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))
head(count_mapped_reads(object))
#> # A tibble: 6 × 3
#>   Barcode Sample Mapped
#>   <chr>   <fct>   <int>
#> 1 10208   T1       3520
#> 2 10502   T1        280
#> 3 10796   T1       1507
#> 4 11089   T1       3797
#> 5 11382   T1        717
#> 6 11675   T1       7330
```
