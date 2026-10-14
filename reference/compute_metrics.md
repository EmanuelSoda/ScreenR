# Compute Metrics

This function computes the metrics that will be then used to compute the
z-score using the function
[`find_zscore_hit`](https://emanuelsoda.github.io/ScreenR/reference/find_zscore_hit.md)
starting from the screenr object for a given treatment in a given day.
More information about the z-score and other metrics used in genetic
screening can be found at this paper
[z-score](https://pubmed.ncbi.nlm.nih.gov/21515799/)

## Usage

``` r
compute_metrics(screenR_Object, control, treatment, day)
```

## Arguments

- screenR_Object:

  The ScreenR object obtained using the
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md)

- control:

  A string specifying the sample that as to be used as control in the
  analysis. This string has to be equal to the interested sample in the
  Treatment column of the data_table slot

- treatment:

  A string specifying the sample that as to be used as treatment in the
  analysis. This string has to be equal to the interested sample in the
  Treatment column of the data_table slot.

- day:

  A string containing the day (time point) to consider in the metrics
  computation. This string has to be equal to the interested sample in
  the Day column of the data_table slot.

## Value

Return a tibble with all the measure computed.

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))
metrics <- compute_metrics(object,
    control = "TRT",
    treatment = "Time3", day = "Time3"
)
head(metrics)
#> # A tibble: 6 × 9
#>   Barcode Gene     Treated Control  Log2FC  Zscore ZscoreRobust Day   Treatment
#>   <fct>   <chr>      <dbl>   <dbl>   <dbl>   <dbl>        <dbl> <chr> <chr>    
#> 1 1       Gene_386   326.    296.   0.140   0.291         1.23  Time3 Time3    
#> 2 10      Gene_519   110.    115.  -0.0621 -0.136        -0.574 Time3 Time3    
#> 3 100     Gene_84     16.9    30.8 -0.868  -1.84         -7.77  Time3 Time3    
#> 4 1000    Gene_350   127.    130.  -0.0344 -0.0773       -0.327 Time3 Time3    
#> 5 10001   Gene_379   492.    463.   0.0895  0.184         0.779 Time3 Time3    
#> 6 10002   Gene_379   147.    169.  -0.201  -0.430        -1.82  Time3 Time3    
```
