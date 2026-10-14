# Mapped Reads

This function returns the number of mapped reads inside the ScreenR
object

## Usage

``` r
mapped_reads(screenR_Object)
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
