# Compute data Table

This function computes the data table that will be used for the
analysis. The data_table is a tidy and normalized version of the
original count_table and will be used throughout the analysis.

## Usage

``` r
compute_data_table(screenR_Object)
```

## Arguments

- screenR_Object:

  The ScreenR object obtained using the
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md)

## Value

ScreenR_Object with the data_table filed containing the table.

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))
object <- compute_data_table(object)
head(slot(object, "data_table"))
#> # A tibble: 6 × 9
#>   Barcode Gene     Sample     Frequency Sequence Library Gene_ID Day   Treatment
#>   <fct>   <chr>    <fct>          <dbl> <lgl>    <lgl>   <lgl>   <chr> <chr>    
#> 1 10208   Gene_380 T1              330. NA       NA      NA      T1    T1       
#> 2 10208   Gene_380 T2              437. NA       NA      NA      T2    T2       
#> 3 10208   Gene_380 Time3_TRT…      365. NA       NA      NA      Time3 TRT      
#> 4 10208   Gene_380 Time3_TRT…      439. NA       NA      NA      Time3 TRT      
#> 5 10208   Gene_380 Time3_TRT…      311. NA       NA      NA      Time3 TRT      
#> 6 10208   Gene_380 Time3_A         358. NA       NA      NA      Time3 Time3    
```
