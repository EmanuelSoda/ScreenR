# Set ScreenR annotation table

Set function for the annotation table of the ScreenR object

## Usage

``` r
set_annotation_table(object, annotation_table)

# S4 method for class 'screenr_object'
set_annotation_table(object, annotation_table)
```

## Arguments

- object:

  The ScreenR object obtained using the
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md)

- annotation_table:

  a table containing the annotation for each shRNA

## Value

The ScreenR object with the annotation table

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))
annotation <- get_annotation_table(object)
#> ScreenR annotation table containing:
#>  5320 rows
#>  5 columns
set_annotation_table(object, annotation)
#> An object of class "screenr_object"
#> Slot "count_table":
#> # A tibble: 5,320 × 15
#>    Barcode    T1    T2 Time3_TRT_A Time3_TRT_B Time3_TRT_C Time3_A Time3_B
#>    <fct>   <int> <int>       <int>       <int>       <int>   <int>   <int>
#>  1 10208    3520  4283        2493        3998        3018    3393    3829
#>  2 10502     280   339         140         542         162     468     157
#>  3 10796    1507  1533        1120        1125        1286    1136    1095
#>  4 11089    3797  4151        2296        3857        3645    3677    3124
#>  5 11382     717   644         564         336         256     554     590
#>  6 11675    7330  7593        4927        6211        6504    7781    7183
#>  7 11969    4854  5371        4251        4921        5424    4627    3952
#>  8 12263    1539  1171         764        1258         923    1503    1150
#>  9 12555    4328  4051        3130        4720        4697    3870    3835
#> 10 12847     466   417         206         318         288     328     185
#> # ℹ 5,310 more rows
#> # ℹ 7 more variables: Time3_C <int>, Time4_TRT_A <int>, Time4_TRT_B <int>,
#> #   Time4_TRT_C <int>, Time4_A <int>, Time4_B <int>, Time4_c <int>
#> 
#> Slot "annotation_table":
#> # A tibble: 5,320 × 5
#>    Gene     Barcode Gene_ID Sequence Library
#>    <chr>    <fct>   <lgl>   <lgl>    <lgl>  
#>  1 Gene_380 10208   NA      NA       NA     
#>  2 Gene_380 10502   NA      NA       NA     
#>  3 Gene_380 10796   NA      NA       NA     
#>  4 Gene_380 11089   NA      NA       NA     
#>  5 Gene_380 11382   NA      NA       NA     
#>  6 Gene_380 11675   NA      NA       NA     
#>  7 Gene_380 11969   NA      NA       NA     
#>  8 Gene_380 12263   NA      NA       NA     
#>  9 Gene_380 12555   NA      NA       NA     
#> 10 Gene_380 12847   NA      NA       NA     
#> # ℹ 5,310 more rows
#> 
#> Slot "groups":
#>  [1] T1/T2   T1/T2   Treated Treated Treated Control Control Control Treated
#> [10] Treated Treated Control Control Control
#> Levels: Control T1/T2 Treated
#> 
#> Slot "replicates":
#> [1] ""
#> 
#> Slot "normalized_count_table":
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
#> 
#> Slot "data_table":
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
#> 
```
