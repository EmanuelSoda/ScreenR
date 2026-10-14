# Get ScreenR data_table

Get function for the data_table of the ScreenR object

## Usage

``` r
get_data_table(object)

# S4 method for class 'screenr_object'
get_data_table(object)
```

## Arguments

- object:

  The ScreenR object obtained using the
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md)

## Value

The data_table of the ScreenR object

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))
data_table <- get_data_table(object)
#> ScreenR normalized data table containing:
#>  74480 rows
#>  9 columns
data_table
#> # A tibble: 74,480 × 9
#>    Barcode Gene     Sample    Frequency Sequence Library Gene_ID Day   Treatment
#>    <fct>   <chr>    <fct>         <dbl> <lgl>    <lgl>   <lgl>   <chr> <chr>    
#>  1 10208   Gene_380 T1             330. NA       NA      NA      T1    T1       
#>  2 10208   Gene_380 T2             437. NA       NA      NA      T2    T2       
#>  3 10208   Gene_380 Time3_TR…      365. NA       NA      NA      Time3 TRT      
#>  4 10208   Gene_380 Time3_TR…      439. NA       NA      NA      Time3 TRT      
#>  5 10208   Gene_380 Time3_TR…      311. NA       NA      NA      Time3 TRT      
#>  6 10208   Gene_380 Time3_A        358. NA       NA      NA      Time3 Time3    
#>  7 10208   Gene_380 Time3_B        425. NA       NA      NA      Time3 Time3    
#>  8 10208   Gene_380 Time3_C        363. NA       NA      NA      Time3 Time3    
#>  9 10208   Gene_380 Time4_TR…      323. NA       NA      NA      Time4 TRT      
#> 10 10208   Gene_380 Time4_TR…      382. NA       NA      NA      Time4 TRT      
#> # ℹ 74,470 more rows
```
