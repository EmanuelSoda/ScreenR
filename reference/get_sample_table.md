# Get the sample table

Returns the Day (time point) and the Treatment of each sample, as used
by the analysis. They come from the sample_table given to
[`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md)
or, when it was not given, from the sample names
(Day_Treatment_Replicate). Use this function to check that the samples
are interpreted correctly, and to find the values to give to the day,
treatment and control arguments of functions such as
[`compute_metrics`](https://emanuelsoda.github.io/ScreenR/reference/compute_metrics.md)
and
[`find_time_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_time_hit.md).

## Usage

``` r
get_sample_table(screenR_Object)
```

## Arguments

- screenR_Object:

  The ScreenR object obtained using
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md).

## Value

A tibble with the columns Sample, Day and Treatment (and any other
column of the sample_table).

## See also

[`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md),
[`get_column_names`](https://emanuelsoda.github.io/ScreenR/reference/get_column_names.md)

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))
get_sample_table(object)
#> # A tibble: 14 × 3
#>    Sample      Day   Treatment
#>    <chr>       <chr> <chr>    
#>  1 T1          T1    T1       
#>  2 T2          T2    T2       
#>  3 Time3_TRT_A Time3 TRT      
#>  4 Time3_TRT_B Time3 TRT      
#>  5 Time3_TRT_C Time3 TRT      
#>  6 Time3_A     Time3 Time3    
#>  7 Time3_B     Time3 Time3    
#>  8 Time3_C     Time3 Time3    
#>  9 Time4_TRT_A Time4 TRT      
#> 10 Time4_TRT_B Time4 TRT      
#> 11 Time4_TRT_C Time4 TRT      
#> 12 Time4_A     Time4 Time4    
#> 13 Time4_B     Time4 Time4    
#> 14 Time4_c     Time4 Time4    
```
