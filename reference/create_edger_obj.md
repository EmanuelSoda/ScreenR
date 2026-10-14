# Create edgeR Object

Utility function that using the screenr-class object create the
corresponding edgeR object. This function and other utility function
enables the user to not worry abut the implementation and just focus on
the analysis. The ScreenR package will take care of the rest.

## Usage

``` r
create_edger_obj(screenR_Object)
```

## Arguments

- screenR_Object:

  The ScreenR object obtained using the
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md)

## Value

The edgeR object will all the needed information for the analysis.

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))
create_edger_obj(object)
#> An object of class "DGEList"
#> $counts
#>          T1        T2 Time3_TRT_A Time3_TRT_B Time3_TRT_C   Time3_A   Time3_B
#> 1 329.84259 437.05313   365.28131   438.98590   310.56195 357.71537 424.65120
#> 2  26.23748  34.59281    20.51319    59.51235    16.67032  49.34005  17.41192
#> 3 141.21386 156.43298   164.10552   123.52655   132.33355 119.76559 121.43982
#> 4 355.79896 423.58336   336.41632   423.50391   375.08227 387.65676 346.46392
#> 5  67.18669  65.71614    82.63885    36.89326    26.34323  58.40681  65.43333
#>     Time3_C Time4_TRT_A Time4_TRT_B Time4_TRT_C   Time4_A   Time4_B   Time4_c
#> 1 363.06161   323.48640   382.06214   456.79054 338.12714 358.16244 390.75227
#> 2  16.93033    32.46057    15.37962    33.91447  63.18147  29.41115  20.28906
#> 3 126.87300    14.21549    39.96678    50.70943  43.66670  41.17561  90.38830
#> 4 361.38947   259.79652   325.90629   280.80853 359.96049 338.11929 274.92213
#> 5  76.81355    35.70663    10.72526    32.53517  12.26918  43.89886  71.49478
#> 5315 more rows ...
#> 
#> $samples
#>               group lib.size norm.factors
#> T1            T1/T2    1e+06            1
#> T2            T1/T2    1e+06            1
#> Time3_TRT_A Treated    1e+06            1
#> Time3_TRT_B Treated    1e+06            1
#> Time3_TRT_C Treated    1e+06            1
#> 9 more rows ...
#> 
#> $genes
#>       Gene Barcode Gene_ID Sequence Library
#> 1 Gene_380   10208      NA       NA      NA
#> 2 Gene_380   10502      NA       NA      NA
#> 3 Gene_380   10796      NA       NA      NA
#> 4 Gene_380   11089      NA       NA      NA
#> 5 Gene_380   11382      NA       NA      NA
#> 5315 more rows ...
#> 
```
