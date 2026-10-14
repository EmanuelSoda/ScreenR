# Get ScreenR normalized_count_table

Get function for the normalized_count_table of the ScreenR object

## Usage

``` r
get_normalized_count_table(object)

# S4 method for class 'screenr_object'
get_normalized_count_table(object)
```

## Arguments

- object:

  The ScreenR object obtained using the
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md)

## Value

The normalized_count_table of the ScreenR object

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))
normalized_count_table <- get_normalized_count_table(object)
#> ScreenR normalized count table containing:
#>  5320 rows
#>  15 columns
normalized_count_table
#> # A tibble: 5,320 × 15
#>    Barcode    T1    T2 Time3_TRT_A Time3_TRT_B Time3_TRT_C Time3_A Time3_B
#>    <fct>   <dbl> <dbl>       <dbl>       <dbl>       <dbl>   <dbl>   <dbl>
#>  1 10208   330.  437.        365.        439.        311.    358.    425. 
#>  2 10502    26.2  34.6        20.5        59.5        16.7    49.3    17.4
#>  3 10796   141.  156.        164.        124.        132.    120.    121. 
#>  4 11089   356.  424.        336.        424.        375.    388.    346. 
#>  5 11382    67.2  65.7        82.6        36.9        26.3    58.4    65.4
#>  6 11675   687.  775.        722.        682.        669.    820.    797. 
#>  7 11969   455.  548.        623.        540.        558.    488.    438. 
#>  8 12263   144.  119.        112.        138.         95.0   158.    128. 
#>  9 12555   406.  413.        459.        518.        483.    408.    425. 
#> 10 12847    43.7  42.6        30.2        34.9        29.6    34.6    20.5
#> # ℹ 5,310 more rows
#> # ℹ 7 more variables: Time3_C <dbl>, Time4_TRT_A <dbl>, Time4_TRT_B <dbl>,
#> #   Time4_TRT_C <dbl>, Time4_A <dbl>, Time4_B <dbl>, Time4_c <dbl>
```
