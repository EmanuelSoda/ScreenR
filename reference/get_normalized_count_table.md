# Get the normalized count table

Returns the normalized count table of a ScreenR object. A short summary
of the size of the table is printed. The normalized counts are not
stored: they are computed from the raw counts and the effective library
sizes set by
[`normalize_data`](https://emanuelsoda.github.io/ScreenR/reference/normalize_data.md)
(raw count / effective library size \* 1e6), so they always follow the
counts and the normalization.

## Usage

``` r
get_normalized_count_table(object)

# S4 method for class 'screenr_object'
get_normalized_count_table(object)
```

## Arguments

- object:

  The ScreenR object obtained using
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md).

## Value

A tibble with the Barcode column and one column of normalized counts for
each sample (an empty tibble if the data have not been normalized).

## See also

[`normalize_data`](https://emanuelsoda.github.io/ScreenR/reference/normalize_data.md),
[`set_normalized_count_table`](https://emanuelsoda.github.io/ScreenR/reference/set_normalized_count_table.md),
[`get_count_table`](https://emanuelsoda.github.io/ScreenR/reference/get_count_table.md)

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))
normalized_count_table <- get_normalized_count_table(object)
#> ScreenR normalized count table: 5320 rows, 15 columns
normalized_count_table
#> # A tibble: 5,320 × 15
#>    Barcode    T1    T2 Time3_TRT_A Time3_TRT_B Time3_TRT_C Time3_A Time3_B
#>    <chr>   <dbl> <dbl>       <dbl>       <dbl>       <dbl>   <dbl>   <dbl>
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
