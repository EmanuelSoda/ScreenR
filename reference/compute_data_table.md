# Compute the data table

Computes the data table, a tidy (long) version of the normalized count
table used by many functions of the analysis, such as
[`compute_metrics`](https://emanuelsoda.github.io/ScreenR/reference/compute_metrics.md),
[`plot_boxplot`](https://emanuelsoda.github.io/ScreenR/reference/plot_boxplot.md),
[`plot_trend`](https://emanuelsoda.github.io/ScreenR/reference/plot_trend.md)
and
[`filter_by_slope`](https://emanuelsoda.github.io/ScreenR/reference/filter_by_slope.md).
Run it after
[`normalize_data`](https://emanuelsoda.github.io/ScreenR/reference/normalize_data.md).
The Day and Treatment of each sample come from
[`get_sample_table`](https://emanuelsoda.github.io/ScreenR/reference/get_sample_table.md).

## Usage

``` r
compute_data_table(screenR_Object)
```

## Arguments

- screenR_Object:

  The ScreenR object obtained using
  [`create_screenr_object`](https://emanuelsoda.github.io/ScreenR/reference/create_screenr_object.md).

## Value

The ScreenR object with the data table filled. The data table has one
row for each barcode and sample and the columns Barcode, Gene, Sample,
Frequency (the normalized count), Day and Treatment, plus Sequence,
Library and Gene_ID when the annotation table has them. It can be read
with
[`get_data_table`](https://emanuelsoda.github.io/ScreenR/reference/get_data_table.md).

## See also

[`normalize_data`](https://emanuelsoda.github.io/ScreenR/reference/normalize_data.md),
[`get_data_table`](https://emanuelsoda.github.io/ScreenR/reference/get_data_table.md)

## Examples

``` r
object <- get0("object", envir = asNamespace("ScreenR"))
object <- normalize_data(object)
object <- compute_data_table(object)
head(get_data_table(object))
#> ScreenR normalized data table: 74480 rows, 9 columns
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
